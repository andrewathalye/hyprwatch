with Interfaces;

package Hyprland.State_Impl is
   --  Internal implementation package for `Hyprland.State`
   --  Not intended for use.

   pragma Pure (Hyprland.State_Impl);

   type Hyprland_Window_Id is private;
   function "<" (L : Hyprland_Window_Id; R : Hyprland_Window_Id) return Boolean;
   function Value (Item : String) return Hyprland_Window_Id;
   function Image (Item : Hyprland_Window_Id) return String;
   No_Window : constant Hyprland_Window_Id;

   type Hyprland_Workspace_Id is private;
   function "<" (L : Hyprland_Workspace_Id; R : Hyprland_Workspace_Id) return Boolean;
   function Value (Item : String) return Hyprland_Workspace_Id;
   function Image (Item : Hyprland_Workspace_Id) return String;
   No_Workspace : constant Hyprland_Workspace_Id;
private
   type Hyprland_Window_Id is new Interfaces.Unsigned_32;
   type Hyprland_Workspace_Id is new Interfaces.Integer_32;

   No_Window : constant Hyprland_Window_Id := 0;
   No_Workspace : constant Hyprland_Workspace_Id := Hyprland_Workspace_Id'First;
end Hyprland.State_Impl;
