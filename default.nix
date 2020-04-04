{ mkDerivation, base, containers, stdenv }:
mkDerivation {
  pname = "state-monad-talk";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base containers ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
