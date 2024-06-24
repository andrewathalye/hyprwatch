{ stdenv

# Native
, gnat
, gprbuild

# Deps
, dbus-ada
, gnatcoll-core
, gtkada
}:

stdenv.mkDerivation {
   pname = "hyprwatch";
   version = "1.1";
   src = ./.;
   
   nativeBuildInputs = [
      gprbuild
      gnat
   ];

   buildInputs = [
      dbus-ada
      gnatcoll-core
      gtkada
   ];

   buildPhase = ''
      runHook preBuild
      gprbuild -j0
      runHook postBuild
   '';

   installPhase = ''
      gprinstall -m -p -Phyprwatch --prefix=$out --mode=usage --no-project --no-manifest
   '';
}
