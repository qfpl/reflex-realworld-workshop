# NOTE: This doesn't work!!! It's just here so I can try again
# later (maybe). :)
let
  loadThunk = path: (import <nixpkgs> {}).fetchFromGitHub (
    let json = builtins.fromJSON (builtins.readFile path);
    in {
      inherit (json) owner repo rev sha256;
      private = json.private or false;
    });

  obelisk-src = loadThunk ../.obelisk/impl/github.json;
  reflex-platform-src = loadThunk "${obelisk-src}/dep/reflex-platform/github.json";
  nixpkgs-src = builtins.fetchTarball (
    let json = builtins.fromJSON (builtins.readFile "${reflex-platform-src}/nixpkgs/github.json");
    in {
      inherit (json) sha256;
      url = "https://github.com/${json.owner}/${json.repo}/archive/${json.rev}.tar.gz";
    });

  project = import ../. {};

in with project.reflex.nixpkgs.pkgs; let
  entrypoint = writeScript "entrypoint" ''
    #!${bash}/bin/bash
    set -e
    if [ $(id -u) != 0 ]; then
      echo "You must run this docker container as root (i.e without the --user flag)" >&2;
      exit 1;
    fi;
    if [ ! -d /workshop ]; then
      echo "Please run this with the workshop code as a volume at /workshop. Eg. docker run -ti -p 8000:8000 -v $(pwd):/workshop -w /workshop benkolera/reflex-realworld-workshop:latest $@" >&2
      exit 1;
    fi;
    source /etc/profile;
    exec $@
  '';
  # Borrowed from https://github.com/NixOS/nix/blob/master/scripts/nix-profile.sh.in
  etc-profile = writeText "etc-profile" ''
  if [ -n "$HOME" ] && [ -n "$USER" ]; then
    __savedpath="$PATH"
    export PATH=${coreutils}/bin

    # Set up the per-user profile.
    # This part should be kept in sync with nixpkgs:nixos/modules/programs/shell.nix

    NIX_LINK=$HOME/.nix-profile

    NIX_USER_PROFILE_DIR=/nix/var/nix/profiles/per-user/$USER

    mkdir -m 0755 -p "$NIX_USER_PROFILE_DIR"

    if [ "$(stat --printf '%u' "$NIX_USER_PROFILE_DIR")" != "$(id -u)" ]; then
        echo "Nix: WARNING: bad ownership on "$NIX_USER_PROFILE_DIR", should be $(id -u)" >&2
    fi

    if [ -w "$HOME" ]; then
        if ! [ -L "$NIX_LINK" ]; then
            echo "Nix: creating $NIX_LINK" >&2
            if [ "$USER" != root ]; then
                if ! ln -s "$NIX_USER_PROFILE_DIR"/profile "$NIX_LINK"; then
                    echo "Nix: WARNING: could not create $NIX_LINK -> $NIX_USER_PROFILE_DIR/profile" >&2
                fi
            else
                # Root installs in the system-wide profile by default.
                ln -s /nix/var/nix/profiles/default "$NIX_LINK"
            fi
        fi

        # Subscribe the user to the unstable Nixpkgs channel by default.
        if [ ! -e "$HOME/.nix-channels" ]; then
            echo "https://nixos.org/channels/nixpkgs-unstable nixpkgs" > "$HOME/.nix-channels"
        fi

        # Create the per-user garbage collector roots directory.
        __user_gcroots=/nix/var/nix/gcroots/per-user/"$USER"
        mkdir -m 0755 -p "$__user_gcroots"
        if [ "$(stat --printf '%u' "$__user_gcroots")" != "$(id -u)" ]; then
            echo "Nix: WARNING: bad ownership on $__user_gcroots, should be $(id -u)" >&2
        fi
        unset __user_gcroots

        # Set up a default Nix expression from which to install stuff.
        __nix_defexpr="$HOME"/.nix-defexpr
        [ -L "$__nix_defexpr" ] && rm -f "$__nix_defexpr"
        mkdir -m 0755 -p "$__nix_defexpr"
        if [ "$USER" != root ] && [ ! -L "$__nix_defexpr"/channels_root ]; then
            ln -s /nix/var/nix/profiles/per-user/root/channels "$__nix_defexpr"/channels_root
        fi
        unset __nix_defexpr
    fi

    # Append ~/.nix-defexpr/channels/nixpkgs to $NIX_PATH so that
    # <nixpkgs> paths work when the user has fetched the Nixpkgs
    # channel.
    export NIX_PATH="''${NIX_PATH:+$NIX_PATH:}nixpkgs=$HOME/.nix-defexpr/channels/nixpkgs"

    # Set up environment.
    # This part should be kept in sync with nixpkgs:nixos/modules/programs/environment.nix
    NIX_PROFILES="/nix/var/nix/profiles/default $NIX_USER_PROFILE_DIR"

    # Set $NIX_SSL_CERT_FILE so that Nixpkgs applications like curl work.
    if [ -e /etc/ssl/certs/ca-certificates.crt ]; then # NixOS, Ubuntu, Debian, Gentoo, Arch
        export NIX_SSL_CERT_FILE=/etc/ssl/certs/ca-certificates.crt
    elif [ -e /etc/ssl/ca-bundle.pem ]; then # openSUSE Tumbleweed
        export NIX_SSL_CERT_FILE=/etc/ssl/ca-bundle.pem
    elif [ -e /etc/ssl/certs/ca-bundle.crt ]; then # Old NixOS
        export NIX_SSL_CERT_FILE=/etc/ssl/certs/ca-bundle.crt
    elif [ -e /etc/pki/tls/certs/ca-bundle.crt ]; then # Fedora, CentOS
        export NIX_SSL_CERT_FILE=/etc/pki/tls/certs/ca-bundle.crt
    elif [ -e "$NIX_LINK/etc/ssl/certs/ca-bundle.crt" ]; then # fall back to cacert in Nix profile
        export NIX_SSL_CERT_FILE="$NIX_LINK/etc/ssl/certs/ca-bundle.crt"
    elif [ -e "$NIX_LINK/etc/ca-bundle.crt" ]; then # old cacert in Nix profile
        export NIX_SSL_CERT_FILE="$NIX_LINK/etc/ca-bundle.crt"
    fi

    if [ -n "''${MANPATH-}" ]; then
        export MANPATH="$NIX_LINK/share/man:$MANPATH"
    fi

    export PATH="$NIX_LINK/bin:$__savedpath"
    unset __savedpath NIX_LINK NIX_USER_PROFILE_DIR NIX_PROFILES
fi
  '';
  dev-ghc = project.ghc.ghcWithPackages (hpkgs: [
    hpkgs.frontend
    hpkgs.backend
    hpkgs.common
    hpkgs.obelisk-generated-static
    hpkgs.obelisk-run
  ]);
  workOnMulti' = { env, packageNames, tools ? _: [], shellToolOverrides ? _: _: {} }:
    let inherit (builtins) listToAttrs filter attrValues all concatLists;
        inherit (project.reflex) overrideCabal generalDevToolsAttrs;
        combinableAttrs = [
          "benchmarkDepends"
          "benchmarkFrameworkDepends"
          "benchmarkHaskellDepends"
          "benchmarkPkgconfigDepends"
          "benchmarkSystemDepends"
          "benchmarkToolDepends"
          "buildDepends"
          "buildTools"
          "executableFrameworkDepends"
          "executableHaskellDepends"
          "executablePkgconfigDepends"
          "executableSystemDepends"
          "executableToolDepends"
          "extraLibraries"
          "libraryFrameworkDepends"
          "libraryHaskellDepends"
          "libraryPkgconfigDepends"
          "librarySystemDepends"
          "libraryToolDepends"
          "pkgconfigDepends"
          "setupHaskellDepends"
          "testDepends"
          "testFrameworkDepends"
          "testHaskellDepends"
          "testPkgconfigDepends"
          "testSystemDepends"
          "testToolDepends"
        ];
        concatCombinableAttrs = haskellConfigs: lib.filterAttrs (n: v: v != []) (lib.listToAttrs (map (name: { inherit name; value = concatLists (map (haskellConfig: haskellConfig.${name} or []) haskellConfigs); }) combinableAttrs));
        getHaskellConfig = p: (overrideCabal p (args: {
          passthru = (args.passthru or {}) // {
            out = args;
          };
        })).out;
        notInTargetPackageSet = p: all (pname: (p.pname or "") != pname) packageNames;
        baseTools = generalDevToolsAttrs env;
        overriddenTools = attrValues (baseTools // shellToolOverrides env baseTools);
        depAttrs = lib.mapAttrs (_: v: filter notInTargetPackageSet v) (concatCombinableAttrs (concatLists [
          (map getHaskellConfig (lib.attrVals packageNames env))
          [{
            buildTools = overriddenTools ++ tools env;
          }]
        ]));

    in builtins.map builtins.head (lib.attrValues (lib.groupBy (x: x.name) (lib.remove null (lib.flatten (lib.attrValues depAttrs)))));
in dockerTools.buildImageWithNixDb {
  name = "benkolera/reflex-realworld-workshop";
  tag = "latest";
  extraCommands = ''
    mkdir -pv -m 0755 /nix /nix/var /nix/var/log /nix/var/log/nix /nix/var/log/nix/drvs /nix/var/nix{,/db,/gcroots,/profiles,/temproots,/userpool}
    mkdir -pv -m 1777 /nix/var/nix/{gcroots,profiles}/per-user
    mkdir -pv -m 1775 /nix/store
    mkdir -pv -m 0755 /nix/var/nix/profiles/per-user/root/channels
    ln -s ${nixpkgs-src} /nix/var/nix/profiles/per-user/root/channels/nixpkgs
  '';
  runAsRoot = ''
    #!${stdenv.shell}
    ${dockerTools.shadowSetup}
    groupadd -r nixbld -g 30000
    for n in $(seq 1 10); do
      useradd -c "Nix build user $n" -d /var/empty -g nixbld -G nixbld -M -N -r -s "${shadow}/bin/nologin" nixbld$n;
    done;
    mkdir -pv -m 777 /tmp
    mkdir -pv -m 655 /home
    mkdir -pv -m 0555 /etc/nix
    cat <<EOF > /etc/nix/nix.conf
sandbox = false
substituters = https://cache.nixos.org/ https://nixcache.reflex-frp.org http://hydra.qfpl.io
trusted-public-keys = ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI= qfpl.io:xME0cdnyFcOlMD1nwmn6VrkkGgDNLLpMXoMYl58bz5g= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=
EOF
    ln -s ${cacert}/etc/ssl/certs/ca-certificates.crt /etc/ssl/certs/ca-certificates.crt
    ln -s ${etc-profile} /etc/profile
    mkdir -pv -m 0700 /root/.nix-defexpr/channels/
    echo "https://nixos.org/channels/nixpkgs-unstable nixpkgs" > /root/.nix-channels
    ln -s ${nixpkgs-src} /root/.nix-defexpr/channels/nixpkgs
  #'';
  contents = [
    cacert
    coreutils
    gnutar
    gzip
    nix
    bashInteractive
    project.obelisk.command
    project.obelisk.skeleton
    project.obelisk.shell
    obelisk-src
    reflex-platform-src
    dev-ghc
  ] ++ (workOnMulti' {
    env = project.ghc;
    packageNames = ["backend" "common" "frontend"];
  });
  diskSize = 20480;
  config = {
    EntryPoint = entrypoint;
    Env = [
      "NIX_PAGER=cat"
      "USER=root"
      "HOME=/root"
    ];
    ExposedPorts = {
      "8000/tcp" = {};
    };
  };
}
