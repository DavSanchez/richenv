{ mkDerivation, base, hashable, hspec, hspec-discover, lib
, QuickCheck, text, unordered-containers
}:
mkDerivation {
  pname = "richenv";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base hashable text unordered-containers
  ];
  testHaskellDepends = [
    base hspec QuickCheck unordered-containers
  ];
  testToolDepends = [ hspec-discover ];
  doHaddock = false;
  homepage = "https://github.com/DavSanchez/richenv";
  description = "Rich environment variable setup for Haskell";
  license = lib.licenses.mit;
}
