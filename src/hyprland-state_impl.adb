with Interfaces;
   use type Interfaces.Unsigned_32;
   use type Interfaces.Integer_32;

package body Hyprland.State_Impl is
   function "<" (L : Hyprland_Window_Id; R : Hyprland_Window_Id) return Boolean is (Interfaces.Unsigned_32 (L) < Interfaces.Unsigned_32 (R));
   function "<" (L : Hyprland_Workspace_Id; R : Hyprland_Workspace_Id) return Boolean is (Interfaces.Integer_32 (L) < Interfaces.Integer_32 (R));

   function Value (Item : String) return Hyprland_Window_Id is (Hyprland_Window_Id'Value (Item));
   function Value (Item : String) return Hyprland_Workspace_Id is (Hyprland_Workspace_Id'Value (Item));


   function Image (Item : Hyprland_Window_Id) return String is (Item'Image);
   function Image (Item : Hyprland_Workspace_Id) return String is (Item'Image);
end Hyprland.State_Impl;

