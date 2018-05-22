{ mkDerivation, base, bytes, stdenv, tasty, tasty-quickcheck
, vector
}:
mkDerivation {
  pname = "vector-bytes-instances";
  version = "0.1.1";
  src = ./.;
  libraryHaskellDepends = [ base bytes vector ];
  testHaskellDepends = [ base bytes tasty tasty-quickcheck vector ];
  homepage = "https://github.com/k0001/vector-bytes-instances";
  description = "Serial (from the bytes package) for Vector (from the vector package)";
  license = stdenv.lib.licenses.bsd3;
}
