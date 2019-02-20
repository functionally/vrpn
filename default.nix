{
  mkDerivation, stdenv
, base
, libvrpn
}:

mkDerivation {
  pname = "vrpn";
  version = "0.3.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base
  ];
  executableHaskellDepends = [
  ];
  librarySystemDepends = [
    libvrpn
  ];
  homepage = "https://bitbucket.org/functionally/vrpn";
  description = "Bindings to VRPN";
  license = stdenv.lib.licenses.mit;
}
