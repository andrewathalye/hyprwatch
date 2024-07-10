with Ada.Text_IO;
with Ada.Characters.Handling;

with Interfaces;
use type Interfaces.Unsigned_64;
use type Interfaces.Integer_32;
with String_Utils;

package body Hyprland.State_Impl is

   package Unsigned_64_IO is new Ada.Text_IO.Modular_IO
     (Interfaces.Unsigned_64);

   function "<"
     (L : Hyprland_Window_Id; R : Hyprland_Window_Id) return Boolean is
     (Interfaces.Unsigned_64 (L) < Interfaces.Unsigned_64 (R));
   function "<"
     (L : Hyprland_Workspace_Id; R : Hyprland_Workspace_Id) return Boolean is
     (Interfaces.Integer_32 (L) < Interfaces.Integer_32 (R));

   function Value (Item : String) return Hyprland_Window_Id is
     (Hyprland_Window_Id'Value (Item));
   function Value (Item : String) return Hyprland_Workspace_Id is
     (Hyprland_Workspace_Id'Value (Item));

   function Image (Item : Hyprland_Window_Id) return String is (Item'Image);
   function Image (Item : Hyprland_Workspace_Id) return String is (Item'Image);

   function To_Selector (Item : Hyprland_Window_Id) return String is

      use Unsigned_64_IO;

      Prefix : constant String  := "address:0x";
      Buf    : String (1 .. 20) := "16#0000000000000000#";
      --  An Unsigned_64 may take 20 bytes to represent
      --  in Ada hex string format.
   begin
      --  Print hexadecimal representation of Item
      Put (Buf, Interfaces.Unsigned_64 (Item), 16);

      --  Strip 16#...# prefix and suffix
      --  Also make lowercase for Hyprland
      declare
         Result : constant String := String_Utils.Strip_Spaces (Buf);
      begin
         return
           Prefix &
           Ada.Characters.Handling.To_Lower
             (Result (Result'First + 3 .. Result'Last - 1));
      end;
   end To_Selector;

   function Is_Special (Item : Hyprland_Workspace_Id) return Boolean is
     (Item < 0);

end Hyprland.State_Impl;
