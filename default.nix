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
   version = "1.2";
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
      gprbuild -j0 -XBUILD_MODE=prod
      runHook postBuild
   '';

   installPhase = ''
      gprinstall -m -p -Phyprwatch -XBUILD_MODE=prod --prefix=$out --mode=usage --no-project --no-manifest
   '';
}
