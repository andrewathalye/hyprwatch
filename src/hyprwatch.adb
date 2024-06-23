pragma Ada_2022;

with Ada.Text_IO; use Ada.Text_IO;

with GNAT.Exception_Traces;
use all type GNAT.Exception_Traces.Trace_Kind;
with GNAT.Traceback.Symbolic;

with D_Bus.G_Main;

with Glib.Main;

with Hyprland.State;

with D_Bus_Helpers;
with Glib_Helpers;
with Hypr_Helpers;

procedure hyprwatch is
   --  Hyprland
   Hypr_State   : aliased Hyprland.State.Hyprland_State;

   --  Glib
   package Hypr_Sources is new Glib.Main.Generic_Sources
      (Hyprland.State.Hyprland_State_Access);
   Discard : Glib.Main.G_Source_Id;
begin
   --  Enable printing detailed stack traces
   GNAT.Exception_Traces.Trace_On (Unhandled_Raise);
   GNAT.Exception_Traces.Set_Trace_Decorator
     (GNAT.Traceback.Symbolic.Symbolic_Traceback'Access);

   Put_Line (Standard_Error, "Starting hyprwatch");

   --  Hyprland
   Hypr_State.Connect;

   --  Print initial status update
   Put_Line (Hypr_Helpers.Generate_Status_JSON (Hypr_State).Write);

   --  D_Bus
   D_Bus_Helpers.Global_Service := new D_Bus_Helpers.Hypr_Service_Type;
   D_Bus_Helpers.Connect
     (D_Bus_Helpers.Global_Service.all,
      Hypr_State'Unchecked_Access);

   --  Glib
   Discard := Hypr_Sources.Idle_Add (
      Glib_Helpers.Hypr_Source'Access,
      Hypr_State'Unchecked_Access);

   --  Main Loop
   D_Bus.G_Main.Start;

   pragma Warnings (Off, "unreachable");
   Hypr_State.Disconnect;
end hyprwatch;
