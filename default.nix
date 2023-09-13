{ mkDerivation, aeson, base, bytestring, hashable, hspec
, hspec-discover, lib, QuickCheck, unordered-containers, vector
, yaml
}:
mkDerivation {
  pname = "richenv";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base hashable unordered-containers vector
  ];
  testHaskellDepends = [
    aeson base bytestring hashable hspec QuickCheck
    unordered-containers yaml
  ];
  testToolDepends = [ hspec-discover ];
  doHaddock = false;
  homepage = "https://github.com/DavSanchez/richenv";
  description = "Rich environment variable setup for Haskell";
  license = lib.licenses.mit;
}
