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
   function Hypr_Add_Watch is new Glib.IOChannel.Generic_Add_Watch
     (Hyprland.State.Hyprland_State);

   Hypr_Channel : Glib.IOChannel.Giochannel;
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

   --  Note: Had we used Glib.IOChannel.Create_Watch, the resulting
   --  G_Source would have required a non-existent GIO_Source_Func type
   --  as its callback. Using Add_Watch is less convenient but avoids
   --  a full reimplementation of Glib.Main.Generic_Sources for a new
   --  callback type since Ada doesn’t have generic function prototypes.
   Discard := Hypr_Add_Watch
     (Channel => Hypr_Channel,
      Condition => Glib.IOChannel.G_Io_In,
      Callback => Glib_Helpers.Hypr_Source_Callback'Access,
      Data => Hypr_State'Unchecked_Access);

   --  Main Loop
   D_Bus.G_Main.Start;

   pragma Warnings (Off, "unreachable");
   Hypr_State.Disconnect;
end hyprwatch;
