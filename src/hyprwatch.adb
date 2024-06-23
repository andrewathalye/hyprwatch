pragma Ada_2022;

with Ada.Text_IO; use Ada.Text_IO;

with GNAT.Exception_Traces;
use all type GNAT.Exception_Traces.Trace_Kind;
with GNAT.Traceback.Symbolic;

with D_Bus.G_Main;

with Glib.Main;
with Glib.IOChannel;

with Hyprland.Protocol;
with Hyprland.State;

with D_Bus_Helpers;
with Glib_Helpers;
with Hypr_Helpers;

procedure hyprwatch is
   --  Hyprland
   Hypr_State   : aliased Hyprland.State.Hyprland_State;

   --  Glib
   package Hypr_Sources is new Glib_Helpers.GIO_Sources
      (Hyprland.State.Hyprland_State);
   Hypr_Channel : Glib.IOChannel.Giochannel;
   Hypr_Watch : Glib.Main.G_Source;
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
   Hypr_Channel := Glib.IOChannel.Giochannel_Unix_New (
      Glib.Gint (Hyprland.Protocol.File_Descriptor
            (Hypr_State.Connection.all)));

   Hypr_Watch := Glib.IOChannel.Io_Create_Watch
     (Channel => Hypr_Channel,
      Condition => Glib.IOChannel.G_Io_In);

   Hypr_Sources.Set_Callback
     (Source => Hypr_Watch,
      Func   => Glib_Helpers.Hypr_Source_Callback'Access,
      Data   => Hypr_State'Unchecked_Access);

   Discard := Glib.Main.Attach (Hypr_Watch);

   --  Main Loop
   D_Bus.G_Main.Start;

   pragma Warnings (Off, "unreachable");
   Hypr_State.Disconnect;
end hyprwatch;
