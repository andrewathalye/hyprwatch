with Glib.IOChannel;
with Glib.Main;

with Hyprland.State;

package Glib_Helpers is
   --  Helper functions for Glib integration into Hyprwatch

   function Hypr_Source_Callback
     (Source : Glib.IOChannel.Giochannel;
      Condition : Glib.IOChannel.GIOCondition;
      Data : in out Hyprland.State.Hyprland_State) return Boolean;
   --  Implements `GIO_Source_Func` for the instance in `Hyprwatch`
   --  A GIO_Source_Func that polls Hyprland for updates and returns True.
   --
   --  A function which returns False is automatically removed from the list
   --  of sources.

   generic
      type Data_Type (<>) is private;
   package GIO_Sources is
      type Data_Access is access all Data_Type;

      type GIO_Source_Func is access function
        (Source : Glib.IOChannel.Giochannel;
         Condition : Glib.IOChannel.GIOCondition;
         Data : in out Data_Type) return Boolean;

      type Destroy_Notify is access procedure (Data : in out Data_Type);

      procedure Set_Callback (
         Source : Glib.Main.G_Source;
         Func : GIO_Source_Func;
         Data : Data_Access;
         Notify : Destroy_Notify := null);
   end GIO_Sources;
end Glib_Helpers;
