{ mkDerivation, array, base, containers, criterion, deepseq
, exceptions, lib, primitive, QuickCheck, quickcheck-instances
, random, sandwich, sandwich-quickcheck, string-interpolate, text
, text-rope, vector, weigh
}:
mkDerivation {
  pname = "myers-diff";
  version = "0.2.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base containers exceptions primitive text vector
  ];
  testHaskellDepends = [
    base containers deepseq exceptions primitive QuickCheck
    quickcheck-instances sandwich sandwich-quickcheck
    string-interpolate text text-rope vector
  ];
  benchmarkHaskellDepends = [
    array base containers criterion deepseq exceptions primitive
    QuickCheck quickcheck-instances random string-interpolate text
    text-rope vector weigh
  ];
  homepage = "https://github.com/codedownio/myers-diff#readme";
  license = lib.licenses.bsd3;
}
