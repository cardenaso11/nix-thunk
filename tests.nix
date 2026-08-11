{ command # The nix-thunk command to test
, packedThunkNixpkgs # The nixpkgs that nix-thunk uses
}:
let
  # Get a version of nixpkgs corresponding to release-22.05, which
  # contains the python based tests and recursive nix.
  pkgs = import (builtins.fetchTarball https://github.com/nixos/nixpkgs/archive/478f3cbc8448b5852539d785fbfe9a53304133be.tar.gz) {};
  sshKeys   = import (pkgs.path + /nixos/tests/ssh-keys.nix) pkgs;
  make-test = import (pkgs.path + /nixos/tests/make-test-python.nix);
  snakeOilPrivateKey = sshKeys.snakeOilPrivateKey.text;
  snakeOilPublicKey = sshKeys.snakeOilPublicKey;

  privateKeyFile = pkgs.writeText "id_rsa" ''${snakeOilPrivateKey}'';

  thunkableSample = pkgs.writeText "default.nix" ''
    let pkgs = import <nixpkgs> {}; in pkgs.git
  '';

  # An upstream flake with a relative path input. This flake has no external
  # inputs on purpose, so a lock of it needs no network.
  #
  # `subAlias` is a second root-level name for the same lock node, and a root
  # `follows` produces such a name. The thunk must keep both names.
  #
  # Nix accepts the name `sub-1.10` here, and Nix rejects that name in a
  # `follows`. haskell.nix's `hls-1.10` has the same problem. The thunk must
  # expose the node under a name that a `follows` accepts.
  flakeSample = pkgs.writeText "flake.nix" ''
    {
      inputs.sub.url = "path:./sub";
      inputs.subAlias.follows = "sub";
      inputs."sub-1.10".url = "path:./sub";
      outputs = { self, sub, subAlias, ... }: {
        value = "upstream";
        subValue = sub.value;
      };
    }
  '';

  flakeSubSample = pkgs.writeText "flake.nix" ''
    { outputs = { self }: { value = "sub"; }; }
  '';

  # The override test substitutes this flake for the thunk's `sub` input.
  flakeOverrideSample = pkgs.writeText "flake.nix" ''
    { outputs = { self }: { value = "overridden"; }; }
  '';

  # This flake reads through the thunk. The thunk forwards its outputs, and a
  # `follows` reaches the inputs of the repository that the thunk points at.
  consumerReadSample = pkgs.writeText "flake.nix" ''
    {
      inputs.mythunk.url = "path:/root/code/myflake";
      inputs.readsSub.follows = "mythunk/sub";
      inputs.readsAlias.follows = "mythunk/subAlias";
      outputs = { self, mythunk, readsSub, readsAlias }: {
        forwarded = mythunk.value;
        viaThunk = mythunk.subValue;
        read = readsSub.value;
        readAlias = readsAlias.value;
      };
    }
  '';

  # This flake overrides an input of the thunk, and that override must change
  # the value of the thunk itself.
  consumerOverrideSample = pkgs.writeText "flake.nix" ''
    {
      inputs.mysub.url = "path:/root/code/mysub";
      inputs.mythunk.url = "path:/root/code/myflake";
      inputs.mythunk.inputs.sub.follows = "mysub";
      outputs = { self, mythunk, mysub }: {
        viaThunk = mythunk.subValue;
      };
    }
  '';

  sshConfigFile = pkgs.writeText "ssh_config" ''
    Host *
      StrictHostKeyChecking no
      UserKnownHostsFile=/dev/null
      ConnectionAttempts=1
      ConnectTimeout=1
      IdentityFile=~/.ssh/id_rsa
      User=root
  '';
in
  make-test ({...}: {
    name  = "nix-thunk";
    nodes = {
      githost = {
        networking.firewall.allowedTCPPorts = [ 22 80 443 ];
        services.openssh = {
          enable = true;
        };
        environment.systemPackages = [ pkgs.git ];
        users.users.root.openssh.authorizedKeys.keys = [
          snakeOilPublicKey
        ];
      };

      client = {
        imports = [ (pkgs.path + /nixos/modules/installer/cd-dvd/channel.nix) ];
        nix.useSandbox = false;
        nix.binaryCaches = [];
        # The tests that consume a packed thunk as a flake input need this
        # setting. `nix-thunk` itself does not need it, because `nix-thunk`
        # passes `--extra-experimental-features` on every command that it runs.
        nix.extraOptions = ''
          experimental-features = nix-command flakes
        '';
        environment.systemPackages = [
          pkgs.nix-prefetch-git
          pkgs.git
          pkgs.rsync

          command

          # This is the version of nixpkgs that we use in thunks. It needs to be
          # included in the VM so that builtin.fetchgit succeeds without a
          # network connection.
          packedThunkNixpkgs
        ];
      };

      # This machine is used for testing that thunks can be built if
      # your nix-thunk is weird, wacky, dead, or not present at all. The
      # GCD of those failure modes is "not present at all", thus:
      noNixThunk = {
        imports = [ (pkgs.path + /nixos/modules/installer/cd-dvd/channel.nix) ];
        nix.useSandbox = false;
        nix.binaryCaches = [];
        environment.systemPackages = [
          pkgs.git
          pkgs.rsync

          # This is the version of nixpkgs that we use in thunks. It needs to be
          # included in the VM so that builtin.fetchgit succeeds without a
          # network connection.
          packedThunkNixpkgs
        ];
      };
    };

    testScript =
      let
      in ''
      start_all()

      with subtest("nix-thunk is installed and git can be configured"):
        client.succeed("""
          nix-thunk --help;
          test "$(nix-thunk --version)" = "nix-thunk ${command.version}";
          git config --global user.email "you@example.com";
          git config --global user.name "Your Name";
        """)

      githost.wait_for_open_port("22")

      with subtest("the clients can access the server via ssh"):
        for machine in [client, noNixThunk]:
          machine.succeed("""
            mkdir -p ~/.ssh/;
            cp ${privateKeyFile} ~/.ssh/id_rsa;
            chmod 600 ~/.ssh/id_rsa;
          """)
          machine.wait_until_succeeds(
            "ssh -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no -i ~/.ssh/id_rsa githost true"
          )
          machine.succeed("cp ${sshConfigFile} ~/.ssh/config")
          machine.wait_until_succeeds("ssh githost true")

      with subtest("a remote bare repo can be started"):
        githost.succeed("""
          mkdir -p ~/myorg/myapp.git;
          cd ~/myorg/myapp.git && git init --bare
        """)

      with subtest("a git project can be configured with a remote using ssh"):
        client.succeed("""
          mkdir -p ~/code/myapp;
          cd ~/code/myapp;
          git init;
          cp ${thunkableSample} default.nix;
          git add .;
          git commit -m 'Initial';
          git remote add origin root@githost:/root/myorg/myapp.git;
        """)

      with subtest("pushing code to the remote"):
        client.succeed("""
          cd ~/code/myapp;
          git push -u origin master;
          git status;
        """)

      with subtest("nix-thunk can pack and unpack"):
        client.succeed("""
          nix-thunk pack ~/code/myapp;
          grep -qF 'git.json' ~/code/myapp/thunk.nix;
          grep -qF 'myorg' ~/code/myapp/git.json;
          nix-thunk unpack ~/code/myapp;
        """)

      with subtest("thunkSource works on unpacked thunk"):
        client.succeed("""
          cp ${./default.nix} ~/code/default.nix;

          # Actual `gitignore.nix` is hard to use without internet.
          # `builtins.fetchgit` will do as a filter-er.
          nix-instantiate --eval --expr --strict '(import ~/code/default.nix { pkgs = import <nixpkgs> {}; gitignoreSource = builtins.fetchGit; }).thunkSource ~/code/myapp';
        """)

      with subtest("thunkSource works on packed thunk"):
        client.succeed("""
          nix-thunk pack ~/code/myapp;

          # Actual `gitignore.nix` is hard to use without internet. But
          # we don't need it in this case.
          nix-instantiate --eval --expr --strict '(import ~/code/default.nix { pkgs = import <nixpkgs> {}; gitignoreSource = _: throw "unused"; }).thunkSource ~/code/myapp';

          nix-thunk unpack ~/code/myapp;
        """)

      with subtest("nix-thunk can create from ssh remote"):
        client.succeed("""
          nix-thunk pack ~/code/myapp;
          nix-thunk create -b master root@githost:/root/myorg/myapp.git ~/code/myapp-remote;
          diff -u ~/code/myapp/git.json ~/code/myapp-remote/git.json;
          cmp ~/code/myapp/git.json ~/code/myapp-remote/git.json;
          nix-thunk unpack ~/code/myapp;
          nix-thunk unpack ~/code/myapp-remote;
        """)

      with subtest("nix-thunk can create from local directory"):
        client.succeed("""
          nix-thunk create ~/code/myapp ~/code/myapp-local
          nix-thunk unpack ~/code/myapp-local
        """)

      with subtest("unpacked thunks can be built"):
        client.succeed("""
          nix-build ~/code/myapp;
          nix-build ~/code/myapp-remote;
          nix-build ~/code/myapp-local;
        """)

      with subtest("packed thunks can be built"):
        client.succeed("""
          nix-thunk -v pack ~/code/myapp-remote;
          nix-thunk -v pack ~/code/myapp-local;
          nix-build ~/code/myapp-remote;
          nix-build ~/code/myapp-local;
          nix-thunk unpack ~/code/myapp-remote;
        """)

      with subtest("a thunk of a repo that is not a flake exposes its source"):
        client.succeed("""
          nix-thunk pack ~/code/myapp;
          test -f ~/code/myapp/flake.nix;
          test -f ~/code/myapp/flake.lock;
          grep -qF 'inherit src' ~/code/myapp/flake.nix;
          grep -qF 'flake = false' ~/code/myapp/flake.nix;

          # `src` is a fetched tree, and not a bare path. It stays a fetched
          # tree after a pack and after an unpack.
          nix eval --raw "path:$HOME/code/myapp#src.outPath" >/dev/null;
          nix eval --raw "path:$HOME/code/myapp#src.narHash" >/dev/null;
        """)

      with subtest("unpacking keeps the flake interface of a non-flake thunk"):
        client.succeed("""
          nix-thunk unpack ~/code/myapp;
          test -f ~/code/myapp/flake.nix;
          nix eval --raw "path:$HOME/code/myapp#src.outPath" >/dev/null;
          nix eval --raw "path:$HOME/code/myapp#src.narHash" >/dev/null;

          # Git must not report the generated files as unsaved work, or a pack
          # would refuse to continue.
          test -z "$(git -C ~/code/myapp status --porcelain)";
        """)

      with subtest("an unpacked thunk's source leaves out the generated flake files"):
        # This test uses `nix eval`, because `nix-instantiate --eval` is
        # read-only and never copies the filtered source into the store.
        client.succeed("""
          nix eval --impure --raw --expr '
            let
              pkgs = import <nixpkgs> {};
              nix-thunk = import ~/code/default.nix {
                inherit pkgs;
                gitignoreSource = pkgs.lib.cleanSource;
              };
              src = (nix-thunk.thunkSource ~/code/myapp).outPath;
            in
              assert builtins.pathExists (src + "/default.nix");
              assert !builtins.pathExists (src + "/flake.nix");
              assert !builtins.pathExists (src + "/flake.lock");
              src
          ' >/dev/null;
        """)

      with subtest("a refused pack leaves the flake interface in place"):
        client.succeed("touch ~/code/myapp/uncommitted;")
        client.fail("nix-thunk pack ~/code/myapp;")
        client.succeed("""
          test -f ~/code/myapp/flake.nix;
          test -f ~/code/myapp/flake.lock;
          rm ~/code/myapp/uncommitted;
        """)

      with subtest("packing discards the generated files and restores the fetching flake"):
        client.succeed("""
          nix-thunk pack ~/code/myapp;
          grep -qF 'inherit src' ~/code/myapp/flake.nix;
          grep -qF 'flake = false' ~/code/myapp/flake.nix;

          # nix-thunk writes the lock itself. Nix does not change the file, and
          # that proves the file is complete and current. Nix would rewrite the
          # file for any input that Nix had to resolve itself.
          cp ~/code/myapp/flake.lock /tmp/myapp.lock.packed;
          (cd ~/code/myapp && nix flake lock);
          cmp ~/code/myapp/flake.lock /tmp/myapp.lock.packed;

          nix-thunk unpack ~/code/myapp;
        """)

      with subtest("--no-flake writes a thunk with no flake interface"):
        client.succeed("""
          nix-thunk create --no-flake -b master root@githost:/root/myorg/myapp.git ~/code/myapp-noflake;
          test -f ~/code/myapp-noflake/thunk.nix;
          test ! -e ~/code/myapp-noflake/flake.nix;
          test ! -e ~/code/myapp-noflake/flake.lock;
          nix-build ~/code/myapp-noflake;

          # An unpack keeps the format that the thunk has. This thunk has no
          # flake interface, so this code preserves nothing and writes nothing.
          nix-thunk unpack ~/code/myapp-noflake;
          test ! -e ~/code/myapp-noflake/flake.nix;
          test -z "$(git -C ~/code/myapp-noflake status --porcelain)";

          # A pack without the flag moves the thunk to the newest format.
          nix-thunk pack ~/code/myapp-noflake;
          test -f ~/code/myapp-noflake/flake.nix;
        """)

      with subtest("a flake repo can be pushed to the remote"):
        githost.succeed("""
          mkdir -p ~/myorg/myflake.git;
          cd ~/myorg/myflake.git && git init --bare
        """)
        client.succeed("""
          mkdir -p ~/code/myflake-src/sub;
          cd ~/code/myflake-src;
          git init;
          cp ${flakeSample} flake.nix;
          cp ${flakeSubSample} sub/flake.nix;
          git add .;
          nix flake lock;
          git add .;
          git commit -m 'Initial';
          git remote add origin root@githost:/root/myorg/myflake.git;
          git push -u origin master;
        """)

      with subtest("a thunk of a flake repo is itself a flake"):
        client.succeed("""
          nix-thunk create -b master root@githost:/root/myorg/myflake.git ~/code/myflake;
          test -f ~/code/myflake/flake.nix;
          test -f ~/code/myflake/flake.lock;

          # The flake pins the same revision as the thunk pointer.
          rev=$(sed -n 's/.*"rev": "\\([0-9a-f]*\\)".*/\\1/p' ~/code/myflake/git.json | head -1);
          test -n "$rev";
          grep -qF "$rev" ~/code/myflake/flake.lock;

          # The thunk exposes upstream's own inputs under upstream's own names.
          # This includes both names that resolve to the same node.
          grep -qF '"sub"' ~/code/myflake/flake.nix;
          grep -qF '"subAlias"' ~/code/myflake/flake.nix;

          # Nix does not accept some names in a `follows`. The thunk binds such
          # a node under a name that Nix accepts. The thunk never writes the
          # original name into a `follows`. The override keeps the original
          # name, because Nix reads an override as an attribute name.
          grep -qF '"sub-1_10"' ~/code/myflake/flake.nix;
          grep -qF '"sub-1.10" = { follows = "sub-1_10"; };' ~/code/myflake/flake.nix;
          test -z "$(grep -F 'follows = "sub-1.10"' ~/code/myflake/flake.nix)";

          # Nix must accept the lock without a change, whatever code wrote it.
          cp ~/code/myflake/flake.lock /tmp/myflake.lock.packed;
          (cd ~/code/myflake && nix flake lock);
          cmp ~/code/myflake/flake.lock /tmp/myflake.lock.packed;
        """)

      with subtest("the packed thunk forwards outputs and follows can read its inputs"):
        client.succeed("""
          mkdir -p ~/code/consumer;
          cp ${consumerReadSample} ~/code/consumer/flake.nix;
          cd ~/code/consumer;
          nix flake lock;
          test "$(nix eval --raw .#forwarded)" = "upstream";
          test "$(nix eval --raw .#viaThunk)" = "sub";
          test "$(nix eval --raw .#read)" = "sub";
          test "$(nix eval --raw .#readAlias)" = "sub";
        """)

      with subtest("the thunk's inputs can be overridden"):
        client.succeed("""
          mkdir -p ~/code/mysub ~/code/consumer-override;
          cp ${flakeOverrideSample} ~/code/mysub/flake.nix;
          cp ${consumerOverrideSample} ~/code/consumer-override/flake.nix;
          cd ~/code/consumer-override;
          nix flake lock;
          test "$(nix eval --raw .#viaThunk)" = "overridden";
        """)

      with subtest("packing a flake thunk is stable across a round trip"):
        client.succeed("""
          cp -r ~/code/myflake ~/code/myflake-before;
          nix-thunk unpack ~/code/myflake;
          nix-thunk pack ~/code/myflake;
          diff -u ~/code/myflake-before/flake.nix ~/code/myflake/flake.nix;
          diff -u ~/code/myflake-before/flake.lock ~/code/myflake/flake.lock;
          diff -u ~/code/myflake-before/git.json ~/code/myflake/git.json;
        """)

      with subtest("nix-thunk can update from ssh remote"):
        client.succeed("""
          cd ~/code/myapp;
          touch test-file;
          git add test-file;
          git commit test-file -m "add test file";
          git push;

          nix-thunk pack ~/code/myapp-remote;
          nix-thunk update ~/code/myapp-remote;
          nix-thunk unpack ~/code/myapp-remote;
          test -f ~/code/myapp-remote/test-file;
        """)

      with subtest("nix-thunk can update from local directory"):
        client.succeed("""
          nix-thunk update ~/code/myapp-local;
          nix-thunk unpack ~/code/myapp-local;
          test -f ~/code/myapp-local/test-file;
        """)

      with subtest("nix-thunk pack will not destroy changes"):
        client.succeed("""
          cd ~/code/myapp-local;
          echo "# Some change" >> default.nix;
          nix-build
        """);
        client.fail("nix-thunk pack ~/code/myapp-local;")

      with subtest("packed thunks can be built without nix-thunk"):
        client.succeed("""
          nix-thunk pack ~/code/myapp-remote;
          rsync -avx ~/code/myapp-remote githost:
        """)
        noNixThunk.succeed("""
          rsync -avx githost:myapp-remote .;
          nix-build myapp-remote
        """)

      with subtest("nix-thunk informs the user about parse errors"):
        client.fail("""
          touch ~/code/myapp-remote/extra-file;
          nix-thunk unpack ~/code/myapp-remote 2>parse-error
        """)
        client.succeed("grep 'extra-file' parse-error")

      with subtest("nix-thunk can create from ssh remote, with branch.master.merge set"):
        client.succeed("""
          git config --global branch.master.merge master;
          nix-thunk pack ~/code/myapp;
          nix-thunk create -b master root@githost:/root/myorg/myapp.git ~/code/myapp-remote-merge-master;
          diff -u ~/code/myapp/git.json ~/code/myapp-remote-merge-master/git.json;
          cmp ~/code/myapp/git.json ~/code/myapp-remote-merge-master/git.json;
          nix-thunk unpack ~/code/myapp
        """)

      with subtest("can create worktree using existing repo, doing detached HEAD when no branch is specified in thunk"):
        client.succeed("""
          nix-thunk create root@githost:/root/myorg/myapp.git ~/code/myapp-2;
          git clone root@githost:/root/myorg/myapp.git ~/code/myapp-mainrepo;
          nix-thunk worktree ~/code/myapp-2 ~/code/myapp-mainrepo;
          branch=$(git -C ~/code/myapp-2 branch --show-current);
          if [ ! -z $branch ]; then
             exit 1
          fi

          # A worktree replaces the thunk, so the worktree carries the same
          # flake interface as an unpacked checkout. Git reports the worktree as
          # clean.
          test -f ~/code/myapp-2/flake.nix;
          test -z "$(git -C ~/code/myapp-2 status --porcelain)";
        """);

      with subtest("gives error when packing worktree on detached HEAD"):
        client.fail("""
          nix-thunk pack ~/code/myapp-2;
        """)

      with subtest("can pack worktree with branch specified, and removes the local branch after packing"):
        client.succeed("""
          git -C ~/code/myapp-mainrepo checkout -b temp-branch;
          git -C ~/code/myapp-2 checkout master;
          nix-thunk pack ~/code/myapp-2;
        """);
        client.fail("""
          git -C ~/code/myapp-mainrepo rev-parse --verify master;
        """)

      with subtest("can create worktree, and checkout the default branch"):
        client.succeed("""
          nix-thunk worktree ~/code/myapp-2 ~/code/myapp-mainrepo;
          git -C ~/code/myapp-mainrepo rev-parse --verify master;
        """);

      with subtest("fails if the branch is already checked out"):
        client.succeed("""
          git -C ~/code/myapp-2 branch --set-upstream-to origin/master;
          nix-thunk pack ~/code/myapp-2;
          git -C ~/code/myapp-mainrepo checkout -b master;
        """);
        client.fail("""
          nix-thunk worktree ~/code/myapp-2 ~/code/myapp-mainrepo;
        """);

      with subtest("can create worktree, when a new branch is specified"):
        client.succeed("""
          nix-thunk worktree ~/code/myapp-2 ~/code/myapp-mainrepo -b somebranch-2;
          git -C ~/code/myapp-mainrepo rev-parse --verify somebranch-2;
        """);

      with subtest("fails when packing worktree with unpushed branch"):
        client.fail("""
          nix-thunk pack ~/code/myapp-2; # has somebranch-2 checked out
        """)

      with subtest("can pack worktree having unpushed branches"):
        client.succeed("""
          git -C ~/code/myapp-mainrepo checkout temp-branch;
          git -C ~/code/myapp-2 checkout master; # repo still contains somebranch-2, having no remote
          git -C ~/code/myapp-2 branch --set-upstream-to origin/master;
          nix-thunk pack ~/code/myapp-2;
        """)

      with subtest("fails to pack worktree containing modifications"):
        client.succeed("""
          nix-thunk worktree ~/code/myapp-2 ~/code/myapp-mainrepo;
          touch ~/code/myapp-2/extra-file;
        """)
        client.fail("""
          nix-thunk pack ~/code/myapp-2;
        """)

      with subtest("can pack worktree with stashed changes"):
        client.succeed("""
          git -C ~/code/myapp-2 add extra-file;
          git -C ~/code/myapp-2 stash;
          git -C ~/code/myapp-2 branch --set-upstream-to origin/master;
          nix-thunk pack ~/code/myapp-2;
        """)
      '';
  }) {}
