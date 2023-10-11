let
  fetchsources = nixpkgs: with nixpkgs; {
    # https://github.com/awakesecurity/proto3-wire/pull/88
    proto3-suite = fetchFromGitHub {
      owner = "awakesecurity";
      repo = "proto3-suite";
      rev = "9f7daef66ba6dfc9574039b1d206c5df126d4b39";
      sha256 = "sha256-1a1ZHlvvtE1urvDL7n984OQ5gbro26RnKAjvDCH/2fs=";
    };
  };

  config = {
    packageOverrides = pkgs: let sources = fetchsources pkgs; in {
      haskellPackages = pkgs.haskellPackages.extend (self: super: with pkgs.haskell.lib; {
        blackbird = self.callCabal2nix "blackbird" ./compiler {};
        data-diverse = dontCheck (unmarkBroken super.data-diverse); # need https://github.com/louispan/data-diverse/pull/13
        ghcid = dontCheck super.ghcid; # some time-related tests are failing

        # both disallow newer versions of primitive
        # will be fixed by https://github.com/well-typed/large-records/pull/151
        large-generics = unmarkBroken (doJailbreak super.large-generics);
        large-records = unmarkBroken (doJailbreak super.large-records);

        proto3-suite = doJailbreak # lagging upper bounds
          (dontCheck # autogen modules not bundled with repo; upstream uses heavy custom scripts to setup protoc tests
            (self.callCabal2nix "proto3-suite" sources.proto3-suite {})); # hackage version is somehow stuck on dhall version even with jailbreak

        proto3-wire = doJailbreak super.proto3-wire;
      });
    };
  };

  # merge into master of https://github.com/NixOS/nixpkgs/pull/248027
  nixpkgsSrc = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/8ed712ec90468c931713e5ec60bd9e6678daf8a8.tar.gz";
    sha256 = "sha256:1mk3zl8rv9fgnq8l62vz7dq0sag0cvcyf0qkbkp9y2vff2yv0x7r";
  };

  nixpkgs = import nixpkgsSrc { inherit config; };

  nixpkgsWith = args: import nixpkgsSrc (args // { inherit config; });

  nixpkgsArch = nixpkgsWith { system = "aarch64-linux"; };

  dockerImage = nixpkgsArch.dockerTools.buildImage {
    name = "blackbird-demo";
    tag = "latest";
    copyToRoot = nixpkgs.buildEnv {
      name = "image-root";
      paths = with nixpkgsArch.haskellPackages; [ blackbird ];
      pathsToLink = [ "/bin" ];
    };
    config.Cmd = [ "/bin/blackbird-demo" ];
  };

  shell = with nixpkgs; haskellPackages.shellFor {
    strictDeps = true;
    packages = p: [ p.blackbird ];
    withHoogle = true;
    nativeBuildInputs =
      let
        hask = with haskellPackages; [
          cabal-install
          ghcid
          (haskell-language-server.overrideAttrs(finalAttrs: previousAttrs: { propagatedBuildInputs = []; buildInputs = previousAttrs.propagatedBuildInputs; }))
        ];
        gopher = with nixpkgs; [
          air
          delve
          go_1_21
          go-outline
          gopls
          go-tools
          protoc-gen-go
          (wgo.overrideAttrs (_: _: {
            # https://github.com/bokwoon95/wgo/blob/720204590891aedbf4aa9a1ff7e703c3d8c8b108/wgo_cmd.go#L355
            # https://github.com/bokwoon95/wgo/blob/720204590891aedbf4aa9a1ff7e703c3d8c8b108/wgo_cmd.go#L473
            patchPhase = ''
              sed -i 's@500 * time.Millisecond@/0@g' wgo_cmd.go
            '';})
          )
        ];
      in hask ++ gopher ++ [
        protobuf
        zlib
      ];
  };
in {
  inherit dockerImage nixpkgs nixpkgsWith shell;
  inherit (nixpkgs.haskellPackages) blackbird;
  }
