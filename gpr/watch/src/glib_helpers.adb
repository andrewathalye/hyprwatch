pragma Ada_2012;

with D_Bus.Arguments;
with D_Bus.Arguments.Basic;
with D_Bus.Connection;
with D_Bus.Types;
with D_Bus_Helpers;
with Hypr_Helpers;

with Debug; use Debug;

package body Glib_Helpers is
   --------------------------
   -- Hypr_Source_Callback --
   --------------------------

   function Hypr_Source_Callback
     (Source    : Glib.IOChannel.Giochannel;
      Condition : Glib.IOChannel.GIOCondition;
      Data      : access Hyprland.State.Hyprland_State) return Glib.Gboolean
   is
      use type D_Bus.Types.Obj_Path;
      use type D_Bus.Arguments.Basic.String_Type;

      pragma Unreferenced (Source);
      pragma Unreferenced (Condition);

      Has_Updates : Boolean;

   begin
      Has_Updates := Data.Update;

      if Has_Updates then
         --  Send out a signal over the D_Bus
         declare
            Message : constant String :=
              Hypr_Helpers.Generate_Status_JSON (Data.all).Write;
            Args    : D_Bus.Arguments.Argument_List_Type;
         begin
            Put_Debug ("Sending signal: " & Message);

            Args.Append (+Message);
            D_Bus.Connection.Send_Signal
              (Connection  =>
                 D_Bus_Helpers.Connection (D_Bus_Helpers.Global_Service.all),
               Object_Name => +"/tk/zenithseeker/hyprwatch",
               Iface => "tk.zenithseeker.hyprwatch", Name => "HyprUpdate",
               Args        => Args);
         end;
      end if;

      return Glib.Gboolean (1);
   end Hypr_Source_Callback;

end Glib_Helpers;
