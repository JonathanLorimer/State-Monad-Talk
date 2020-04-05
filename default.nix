{ mkDerivation, base, bytestring, containers, stdenv, wreq }:
mkDerivation {
  pname = "state-monad-talk";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base bytestring containers wreq ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
