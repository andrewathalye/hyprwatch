{ stdenv

# Native
, gnat
, gprbuild

# Deps
, dbus-ada
}:

stdenv.mkDerivation {
   pname = "hyprwatch";
   version = "1.0";
   src = ./.;
   
   nativeBuildInputs = [
      gprbuild
      gnat
   ];

   buildInputs = [
      dbus-ada
   ];

   buildPhase = ''
      runHook preBuild
      gprbuild -j0
      runHook postBuild
   '';

   installPhase = ''
      gprinstall -p --prefix=$out
   '';
}
