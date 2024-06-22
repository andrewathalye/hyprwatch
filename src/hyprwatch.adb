pragma Ada_2022;

with Ada.Text_IO; use Ada.Text_IO;

with GNAT.Exception_Traces;
use all type GNAT.Exception_Traces.Trace_Kind;
with GNAT.Traceback.Symbolic;

with D_Bus.Connection;
with D_Bus.Connection.G_Main;
with D_Bus.Types;
use type D_Bus.Types.Obj_Path;

with D_Bus_Methods; use D_Bus_Methods;
with Hyprland.State;

with Helpers;

procedure hyprwatch is
   Bus : D_Bus.Connection.Connection_Type;

   Hypr_State   : Hyprland.State.Hyprland_State;
   Hypr_Service : Hypr_Service_Type;
begin
   GNAT.Exception_Traces.Trace_On (Unhandled_Raise);
   GNAT.Exception_Traces.Set_Trace_Decorator
     (GNAT.Traceback.Symbolic.Symbolic_Traceback'Access);

   Put_Line (Standard_Error, "Starting hyprwatch");

   --  D_Bus
   Bus := D_Bus.Connection.Connect;
   D_Bus.Connection.Request_Name (Bus, "tk.zenithseeker.hyprwatch");
   D_Bus.Connection.G_Main.Register_Object
     (Connection => Bus, Path => +"/tk/zenithseeker/hyprwatch",
      Object     => Hypr_Service);

   --  Hyprland
   Hypr_State.Connect;

   Put_Line (Helpers.Generate_Status_JSON (Hypr_State).Write);

   Main_Loop :
   loop
      if Hypr_State.Update then
         Put_Line (Helpers.Generate_Status_JSON (Hypr_State).Write);
      end if;
   end loop Main_Loop;

   pragma Warnings (Off, "unreachable");
   Hypr_State.Disconnect;
end hyprwatch;
