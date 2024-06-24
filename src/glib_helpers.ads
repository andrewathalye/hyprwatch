with Glib.IOChannel;

with Hyprland.State;

package Glib_Helpers is
   --  Helper functions for Glib integration into Hyprwatch

   function Hypr_Source_Callback
     (Source    : Glib.IOChannel.Giochannel;
      Condition : Glib.IOChannel.GIOCondition;
      Data : access Hyprland.State.Hyprland_State) return Glib.Gboolean with
     Convention => C;
   --  Implements `GIO_Source_Func` for the instance of
   --  `Glib.IOChannel.Generic_Add_Watch` in `Hyprwatch`.
   --
   --  A function which returns False is automatically removed from the list
   --  of sources.
end Glib_Helpers;
