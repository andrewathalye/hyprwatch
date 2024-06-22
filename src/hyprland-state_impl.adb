with Ada.Text_IO;
with Ada.Characters.Handling;

with Interfaces;
use type Interfaces.Unsigned_32;
use type Interfaces.Integer_32;

package body Hyprland.State_Impl is
   package Unsigned_32_IO is new Ada.Text_IO.Modular_IO
     (Interfaces.Unsigned_32);

   function "<"
     (L : Hyprland_Window_Id; R : Hyprland_Window_Id) return Boolean is
     (Interfaces.Unsigned_32 (L) < Interfaces.Unsigned_32 (R));
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
      use Unsigned_32_IO;

      Prefix : constant String  := "address:0x";
      Result : String (1 .. 11) := "16#0000000#";
      --  TODO Check whether it can actually take the full 12 bytes
   begin
      --  Print hexadecimal representation of Item
      Put (Result, Interfaces.Unsigned_32 (Item), 16);

      --  Strip 16#...# prefix and suffix
      --  Also make lowercase for Hyprland
      return
        Prefix &
        Ada.Characters.Handling.To_Lower
          (Result (Result'First + 3 .. Result'Last - 1));
   end To_Selector;
end Hyprland.State_Impl;
