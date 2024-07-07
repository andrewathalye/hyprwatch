{ stdenv

# Native
, gnat
, gprbuild
, which

# Deps
, dbus-ada
, gnatcoll-core
, gtkada
}:

stdenv.mkDerivation {
   pname = "hyprwatch";
   version = "1.3";
   src = ./.;
   
   nativeBuildInputs = [
      gprbuild
      gnat
      which
   ];

   buildInputs = [
      dbus-ada
      gnatcoll-core
      gtkada
   ];

   buildPhase = ''
      runHook preBuild
      gprbuild -j0 -XBUILD_MODE=prod -XLIBRARY_TYPE=relocatable
      runHook postBuild
   '';

   installPhase = ''
      gprinstall -m -p -Phyprwatch -XBUILD_MODE=prod -XLIBRARY_TYPE=relocatable --prefix=$out --mode=usage --no-project --no-manifest
   '';
}
