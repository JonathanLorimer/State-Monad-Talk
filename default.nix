{ mkDerivation, base, bytestring, containers, stdenv, wreq }:
mkDerivation {
  pname = "state-monad-talk";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base bytestring containers wreq ];
  executableHaskellDepends = [ base bytestring containers wreq ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
