{ system ? builtins.currentSystem # TODO: Get rid of this system cruft
, iosSdkVersion ? "10.2"
}:
with import ./.obelisk/impl { inherit system iosSdkVersion; };
project ./. ({ pkgs, ... }: {
  android.applicationId = "io.qfpl.obelisk.reflex.realworld.workshop";
  android.displayName = "Reflex Realworld Workshop";
  ios.bundleIdentifier = "io.qfpl.obelisk.reflex.realworld.workshop";
  ios.bundleName = "Reflex Realworld Workshop";
  withHoogle = true;
  overrides = with pkgs.haskell.lib; (self: super: {
    entropy = self.callHackage "entropy" "0.4.1.3" {};
    scrypt = dontCheck super.scrypt;
    reflex-dom-storage = (import ./dep/reflex-dom-storage) self super;
    servant-reflex = (import ./dep/servant-reflex) self super;
    servant-snap = (import ./dep/servant-snap) self super;
    servant-auth-snap = (import ./dep/servant-auth {}) self super;
    mmark = overrideCabal
     ((import ./dep/mmark) self super)
     (drv: {
       doHaddock = false;
       doCheck   = false;
     });
    megaparsec = pkgs.haskell.lib.dontCheck ((import ./dep/megaparsec) self super);
    modern-uri = pkgs.haskell.lib.dontCheck ((import ./dep/modern-uri) self super);
    neat-interpolation = pkgs.haskell.lib.dontCheck ((import ./dep/neat-interpolation) self super);
    email-validate = pkgs.haskell.lib.dontCheck super.email-validate;
    validation = doJailbreak super.validation; # newer hedgehog in nixpkgs.
    servant = pkgs.haskell.lib.dontCheck super.servant;
  });
})
