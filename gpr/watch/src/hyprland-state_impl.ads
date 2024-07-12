with Interfaces;
with System.Storage_Elements;

package Hyprland.State_Impl is
   --  Internal implementation package for `Hyprland.State`
   --  Not intended for use. All useful subprograms should be
   --  re-exported in `Hyprland.State`

   type Hyprland_Monitor_Id is private;
   function "<"
     (L : Hyprland_Monitor_Id; R : Hyprland_Monitor_Id) return Boolean;

   function Value (Item : String) return Hyprland_Monitor_Id;
   function Image (Item : Hyprland_Monitor_Id) return String;

   No_Monitor : constant Hyprland_Monitor_Id;

   type Hyprland_Workspace_Id is private;
   function "<"
     (L : Hyprland_Workspace_Id; R : Hyprland_Workspace_Id) return Boolean;

   function Value (Item : String) return Hyprland_Workspace_Id;
   function Image (Item : Hyprland_Workspace_Id) return String;
   function Is_Special (Item : Hyprland_Workspace_Id) return Boolean;

   No_Workspace : constant Hyprland_Workspace_Id;

   type Hyprland_Window_Id is private;
   function "<"
     (L : Hyprland_Window_Id; R : Hyprland_Window_Id) return Boolean;

   function Value (Item : String) return Hyprland_Window_Id;
   function Image (Item : Hyprland_Window_Id) return String;
   function To_Selector (Item : Hyprland_Window_Id) return String;
   --  Produces a Hyprland selector in the format address:0xAABBCCDD
   --  Note: Any leading zeroes must be stripped

   No_Window : constant Hyprland_Window_Id;

private
   --  TODO nothing actually wrong, but this relies on Hyprland implementation
   --  details and should be the first thing to check if anything breaks

   type Hyprland_Monitor_Id is range -1 .. Interfaces.Integer_32'Last
   with
      Size => 32;
   --  Note: Type is undocumented but assumed to be int32_t
   --  As of v0.41.1

   type Hyprland_Workspace_Id is new Interfaces.Integer_32;
   --  Note: Type confirmed via documentation as (1 .. 2 147 483 647)
   --  Negative values are an implementation detail of special workspaces
   --  (we shouldn’t generate them but must store them)
   --  As of v0.41.1

   type Hyprland_Window_Id is new System.Storage_Elements.Integer_Address;
   --  Note: Type is undocumented but defined as uintptr_t
   --  As of v0.41.1

   No_Monitor   : constant Hyprland_Monitor_Id   := -1; --  Invalid monitor id
   No_Workspace : constant Hyprland_Workspace_Id := 0; --  Invalid workspace id
   No_Window    : constant Hyprland_Window_Id    := 0; --  Invalid window id
end Hyprland.State_Impl;
