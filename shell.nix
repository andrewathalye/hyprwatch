{ nix-ada ? import ../nix-ada/default.nix {}
}:

nix-ada.pkgs.mkShell {
   nativeBuildInputs = [
      nix-ada.gprbuild  
      nix-ada.gnat
      nix-ada.libadalang-tools
      nix-ada.ada-language-server
      nix-ada.pkgs.nodejs
      nix-ada.pkgs.gdb
      nix-ada.pkgs.which
   ];
      
   buildInputs = [
      nix-ada.dbus-ada
      nix-ada.gtkada
      nix-ada.pkgs.gnatcoll-core
   ];

   shellHook = ''
      export LIBRARY_TYPE=relocatable
   '';
}
