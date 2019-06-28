with import <nixpkgs> {}; {
  IrisEnv = stdenv.mkDerivation {
    name = "IrisEnv";
    buildInputs = [ ghc
                    cabal-install
                    stack
                    haskellPackages.ghcid
                  ];

  };
}
