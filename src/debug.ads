package Debug is
   --  Various debug facilities

   procedure Put_Debug (Item : String);
   --  Print a message to Standard_Error iff
   --  debugging is enabled. Debugging can
   --  be enabled by setting the environment
   --  variable DEBUG=1
end Debug;
