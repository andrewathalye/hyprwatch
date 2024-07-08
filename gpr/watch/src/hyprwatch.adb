pragma Ada_2022;

with D_Bus.G_Main;

with Glib.Main;
with Glib.IOChannel;

with Hyprland.Protocol;
with Hyprland.State;

with D_Bus_Helpers;
with Glib_Helpers;

with Debug; use Debug;

procedure hyprwatch is
   --  Hyprland

   Hypr_State : aliased Hyprland.State.Hyprland_State;

   --  Glib
   function Hypr_Add_Watch is new Glib.IOChannel.Generic_Add_Watch
     (Hyprland.State.Hyprland_State);

   Hypr_Channel : Glib.IOChannel.Giochannel;
   Discard      : Glib.Main.G_Source_Id;
begin
   Put_Debug ("Starting hyprwatch");

   --  Hyprland
   Hypr_State.Connect;

   --  D_Bus
   --  Note: intentionally not freed as it persists throughout the program
   D_Bus_Helpers.Global_Service := new D_Bus_Helpers.Hypr_Service_Type;

   D_Bus_Helpers.Connect
     (D_Bus_Helpers.Global_Service.all, Hypr_State'Unchecked_Access);

   --  Glib
   Hypr_Channel :=
     Glib.IOChannel.Giochannel_Unix_New
       (Glib.Gint
          (Hyprland.Protocol.File_Descriptor (Hypr_State.Connection.all)));

   --  Note: Had we used Glib.IOChannel.Create_Watch, the resulting
   --  G_Source would have required a non-existent GIO_Source_Func type
   --  as its callback. Using Add_Watch is less convenient but avoids
   --  a full reimplementation of Glib.Main.Generic_Sources for a new
   --  callback type since Ada doesn’t have generic function prototypes.
   Discard :=
     Hypr_Add_Watch
       (Channel  => Hypr_Channel, Condition => Glib.IOChannel.G_Io_In,
        Callback => Glib_Helpers.Hypr_Source_Callback'Access,
        Data     => Hypr_State'Unchecked_Access);

   --  Main Loop
   D_Bus.G_Main.Start;
end hyprwatch;
