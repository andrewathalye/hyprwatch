with Interfaces;

package Hyprland.State_Impl is
   --  Internal implementation package for `Hyprland.State`
   --  Not intended for use. All useful subprograms should be
   --  re-exported in `Hyprland.State`

   type Hyprland_Window_Id is private;
   function "<"
     (L : Hyprland_Window_Id; R : Hyprland_Window_Id) return Boolean;

   function Value (Item : String) return Hyprland_Window_Id;
   function Image (Item : Hyprland_Window_Id) return String;
   function To_Selector (Item : Hyprland_Window_Id) return String;
   --  Produces a Hyprland selector in the format address:0xAABBCCDD
   --  Note: Any leading zeroes must be stripped

   No_Window : constant Hyprland_Window_Id;

   type Hyprland_Workspace_Id is private;
   function "<"
     (L : Hyprland_Workspace_Id; R : Hyprland_Workspace_Id) return Boolean;

   function Value (Item : String) return Hyprland_Workspace_Id;
   function Image (Item : Hyprland_Workspace_Id) return String;
   function Is_Special (Item : Hyprland_Workspace_Id) return Boolean;

   No_Workspace : constant Hyprland_Workspace_Id;

private
   type Hyprland_Window_Id is new Interfaces.Unsigned_64;
   --  Note: Type unconfirmed but 64-bit values have
   --  been observed.

   type Hyprland_Workspace_Id is new Interfaces.Integer_32;
   --  Note: Type confirmed for Hyprland v0.41.1

   No_Window    : constant Hyprland_Window_Id    := 0; --  Invalid window id
   No_Workspace : constant Hyprland_Workspace_Id := 0; --  Invalid workspace id
end Hyprland.State_Impl;
