with Ada.Text_IO;
with Ada.Characters.Handling;

with Interfaces;
use type Interfaces.Integer_32;

with System.Storage_Elements;
use type System.Storage_Elements.Integer_Address;

with String_Utils;

package body Hyprland.State_Impl is
   -----------------
   -- To_Selector --
   -----------------
   generic
      type T is mod <>;
   function To_Selector_G (Item : T) return String;
   function To_Selector_G (Item : T) return String is
      package T_IO is new Ada.Text_IO.Modular_IO (T);
      use T_IO;

      Prefix : constant String := "address:0x";
      Buf    : String (1 .. (4 + 2 * (T'Size / 8)));
      --  Dynamically ascertain the necessary size to store "16#...#"
      --  where "..." is the hexadecimal representation of type `T`
   begin
      --  Print hexadecimal representation of Item
      Put (Buf, Item, 16);

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
   end To_Selector_G;

   ---------------------------
   -- Convenience Renamings --
   ---------------------------
   subtype SSEIA is System.Storage_Elements.Integer_Address;

   -------------
   -- Monitor --
   -------------
   function "<" (L, R : Hyprland_Monitor_Id) return Boolean is
     (Interfaces.Integer_32 (L) < Interfaces.Integer_32 (R));

   function Value (Item : String) return Hyprland_Monitor_Id is
     (Hyprland_Monitor_Id'Value (Item));

   function Image (Item : Hyprland_Monitor_Id) return String is (Item'Image);

   ---------------
   -- Workspace --
   ---------------
   function "<" (L, R : Hyprland_Workspace_Id) return Boolean is
     (Interfaces.Integer_32 (L) < Interfaces.Integer_32 (R));

   function Value (Item : String) return Hyprland_Workspace_Id is
     (Hyprland_Workspace_Id'Value (Item));

   function Image (Item : Hyprland_Workspace_Id) return String is (Item'Image);

   function Is_Special (Item : Hyprland_Workspace_Id) return Boolean is
     (Item < 0);

   ------------
   -- Window --
   ------------
   function "<" (L, R : Hyprland_Window_Id) return Boolean is
     (SSEIA (L) < SSEIA (R));

   function Value (Item : String) return Hyprland_Window_Id is
     (Hyprland_Window_Id'Value (Item));

   function Image (Item : Hyprland_Window_Id) return String is (Item'Image);

   function To_Selector_HWI is new To_Selector_G (Hyprland_Window_Id);
   function To_Selector (Item : Hyprland_Window_Id) return String is
     (To_Selector_HWI (Item));
end Hyprland.State_Impl;
