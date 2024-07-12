with Ada.Environment_Variables;
with Ada.Text_IO;

package body Debug is

   Debug : Boolean := False;

   procedure Put_Debug (Item : String) is

      use Ada.Text_IO;

   begin
      if Debug then
         Put_Line (Standard_Error, Item);
      end if;
   end Put_Debug;

begin
   if Ada.Environment_Variables.Exists ("DEBUG") then
      if Ada.Environment_Variables.Value ("DEBUG") = "1" then
         Debug := True;
      end if;
   end if;
end Debug;
