pragma Ada_2012;

with Ada.Text_IO;

with Hypr_Helpers;

package body Glib_Helpers is
   --------------------------
   -- Hypr_Source_Callback --
   --------------------------

   function Hypr_Source_Callback
     (Source    : Glib.IOChannel.Giochannel;
      Condition : Glib.IOChannel.GIOCondition;
      Data      : access Hyprland.State.Hyprland_State) return Glib.Gboolean
   is

      pragma Unreferenced (Source);
      pragma Unreferenced (Condition);

      use Ada.Text_IO;

      Has_Updates : Boolean;

   begin
      Has_Updates := Data.Update;

      if Has_Updates then
         Put_Line (Hypr_Helpers.Generate_Status_JSON (Data.all).Write);
      end if;

      return Glib.Gboolean (1);
   end Hypr_Source_Callback;

end Glib_Helpers;
