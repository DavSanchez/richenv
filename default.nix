{ mkDerivation, aeson, base, bytestring, hspec, hspec-discover, lib
, QuickCheck, quickcheck-instances, text, unordered-containers
, yaml
}:
mkDerivation {
  pname = "richenv";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ aeson base text unordered-containers ];
  testHaskellDepends = [
    aeson base bytestring hspec QuickCheck quickcheck-instances text
    unordered-containers yaml
  ];
  testToolDepends = [ hspec-discover ];
  doHaddock = false;
  homepage = "https://github.com/DavSanchez/richenv";
  description = "Rich environment variable setup for Haskell";
  license = lib.licenses.mit;
}
