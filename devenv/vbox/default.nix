{ oldNixpkgs ? <nixpkgs>}:
let
  loadThunk = path: (import oldNixpkgs {}).fetchFromGitHub (
    let json = builtins.fromJSON (builtins.readFile path);
    in {
      inherit (json) owner repo rev sha256;
      private = json.private or false;
    });

  obelisk-src = loadThunk ../../.obelisk/impl/github.json;
  reflex-platform-src = loadThunk "${obelisk-src}/dep/reflex-platform/github.json";
  nixpkgs-src = builtins.fetchTarball (
    let json = builtins.fromJSON (builtins.readFile "${reflex-platform-src}/nixpkgs/github.json");
    in {
      inherit (json) sha256;
      url = "https://github.com/${json.owner}/${json.repo}/archive/${json.rev}.tar.gz";
    });
  project = import ../../. {};
  nixpkgs = project.reflex.nixpkgs;
  pkgs = nixpkgs.pkgs;

  reflex-realworld-workshop = pkgs.stdenv.mkDerivation {
    name = "reflex-realworld-workshop-source";
    src = ../../.;
    phases = ["installPhase"];
    installPhase = ''
      mkdir $out
      cp -r $src $out/
    '';
  };
  vsCodeCustom = pkgs.vscode-with-extensions.override {
    vscodeExtensions = pkgs.vscode-utils.extensionsFromVscodeMarketplace [
      {
            name = "language-haskell";
            publisher = "justusadam";
            version = "2.6.0";
            sha256 = "1891pg4x5qkh151pylvn93c4plqw6vgasa4g40jbma5xzq8pygr4";
      }
    ];
  };
  emacsCustom = pkgs.emacsWithPackages (epkgs: with epkgs.melpaPackages;
    [haskell-mode]
  );
  projectGhc = project.ghc.ghcWithPackages (hpkgs: [
      hpkgs.backend
      hpkgs.frontend
      hpkgs.common
  ]);
  tools = with pkgs; [
    utillinux
    coreutils
    binutils
    pkgconfig
    gnutar
    gzip
    project.obelisk.command

    wmctrl

    gitAndTools.gitFull
    chromium
    ag

    emacsCustom
    vim
    gnome3.gedit
    vsCodeCustom

    reflex-realworld-workshop
    obelisk-src
    reflex-platform-src
    nixpkgs-src
    projectGhc
  ];

  spacemacs-config = ./spacemacs;

  workshop-vm-config = {
      imports = [ "${oldNixpkgs}/nixos/modules/virtualisation/virtualbox-image.nix" ];

      virtualbox = {
        baseImageSize = 20 * 1024;
        memorySize = 3 * 1024;
        vmName = "Reflex Realworld workshop (NixOS)";
        vmDerivationName = "nixos-ova-reflex-realworld-workshop";
        vmFileName = "reflex-realworld-workshop.ova";
      };
      nix = {
        binaryCaches = [
          "https://cache.nixos.org/"
          "https://qfpl.cachix.org"
          "https://nixcache.reflex-frp.org"
          "http://hydra.qfpl.io"
        ];
        binaryCachePublicKeys = [
          "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI="
          "qfpl.io:xME0cdnyFcOlMD1nwmn6VrkkGgDNLLpMXoMYl58bz5g="
          "qfpl.cachix.org-1:JTTxGW07zLPAGglHlMbMC3kQpCd6eFRgbtnuorCogYw="
        ];
      };

      services.xserver = {
        enable = true;
        displayManager.sddm = {
          enable = true;
          autoLogin = {
            enable = true;
            relogin = true;
            user = "workshop";
          };
        };
        desktopManager.xfce.enable = true;
        libinput.enable = true; # for touchpad support on many laptops
      };

      services.postgresql = {
        enable = true;
        initialScript = pkgs.writeText "conduit-dbdbump" (builtins.concatStringsSep "\n"
          [ (builtins.readFile ./setup.sql)
            (builtins.readFile ../common/dbdump.sql)
          ]);
      };

      users.extraUsers.workshop = {
        isNormalUser = true;
        description = "Workshop user";
        extraGroups = [ "wheel" ];
        password = "workshop";
        uid = 1000;
      };

      environment.systemPackages = tools;

      systemd.services.workshop-setup = {
        description = "Setup workshop";
        serviceConfig.Type = "oneshot";
        serviceConfig.RemainAfterExit = true;
        wantedBy = [ "multi-user.target" ];
        script = ''
          if [ ! -e /home/workshop/.emacs.d ]; then
            git clone https://github.com/syl20bnr/spacemacs /home/workshop/.emacs.d
            cp ${spacemacs-config} /home/workshop/.spacemacs
            chown workshop /home/workshop/.spacemacs 
            chown -R workshop /home/workshop/.emacs.d
            chmod -R u+w /home/workshop/.emacs.d
          fi
          if [ ! -e /home/workshop/reflex-realworld-workshop ]; then
            cp -r "${reflex-realworld-workshop}"/* /home/workshop/reflex-realworld-workshop
            chown -R workshop /home/workshop/reflex-realworld-workshop
            chmod -R u+w /home/workshop/reflex-realworld-workshop
          fi
        '';
      };
  };
in (import "${oldNixpkgs}/nixos/lib/eval-config.nix" {
  modules = [ workshop-vm-config ];
}).config.system.build.virtualBoxOVA
