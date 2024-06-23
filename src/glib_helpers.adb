pragma Ada_2012;

package body Glib_Helpers is

   -----------------
   -- Hypr_Source --
   -----------------
   --
   --TODO rewrite to use fd polling instead of function
   --polling (drop cpu usage)

   function Hypr_Source
     (State : Hyprland.State.Hyprland_State_Access) return Boolean
   is
      Discard : Boolean;
   begin
      Discard := State.Update;
      delay 0.01;

      return True;
   end Hypr_Source;

end Glib_Helpers;
