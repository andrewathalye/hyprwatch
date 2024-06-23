with Hyprland.State;

package Glib_Helpers is
   --  Helper functions for Glib integration into Hyprwatch

   function Hypr_Source
     (State : Hyprland.State.Hyprland_State_Access) return Boolean;
   --  Implements `Glib.Main.G_Source_Func` for the instance in `Hyprwatch`
   --  A Glib Source_Func that polls Hyprland for updates and returns True.
   --
   --  A function which returns False is automatically removed from the list
   --  of sources.
end Glib_Helpers;
