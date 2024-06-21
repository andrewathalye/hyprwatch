pragma Ada_2022;

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Interfaces.C;

with GNAT.OS_Lib;
with GNAT.Exception_Traces;
use all type GNAT.Exception_Traces.Trace_Kind;
with GNAT.Traceback.Symbolic;

with GNATCOLL.JSON;

with D_Bus.Connection;
with D_Bus.Connection.G_Main;
with D_Bus.Service;
with D_Bus.Messages;
with D_Bus.Types;
use type D_Bus.Types.Obj_Path;

with D_Bus_Methods; use D_Bus_Methods;
with Hyprland.State;
with Hyprland.State_Impl;

procedure hyprwatch is
   Bus : D_Bus.Connection.Connection_Type;

   Hypr_State : Hyprland.State.Hyprland_State;
   Hypr_Service    : Hypr_Service_Type;
begin
   GNAT.Exception_Traces.Trace_On (Unhandled_Raise);
   GNAT.Exception_Traces.Set_Trace_Decorator
     (GNAT.Traceback.Symbolic.Symbolic_Traceback'Access);

   Put_Line ("Starting hyprwatch");
   Put_Line ("Connecting to Hyprland");
   Hypr_State.Connect;

   Put_Line ("Registering dbus object");
   Bus := D_Bus.Connection.Connect;

   D_Bus.Connection.Request_Name (Bus, "tk.zenithseeker.hyprwatch");

   D_Bus.Connection.G_Main.Register_Object
     (Connection => Bus, Path => +"/tk/zenithseeker/hyprwatch",
      Object     => Hypr_Service);

   Put_Line ("Main loop");

   Main_Loop :
   loop
      if Hypr_State.Update then
         declare
            use GNATCOLL.JSON;
            use Ada.Strings.Unbounded;
            use Interfaces;

            Result : JSON_Value := GNATCOLL.JSON.Create_Object;

            Active_Window : constant Hyprland.State.Hyprland_Window := Hypr_State.Windows.Element (Hypr_State.Active_Window);
            Active_Workspace : constant Hyprland.State.Hyprland_Workspace := Hypr_State.Workspaces.Element (Hypr_State.Active_Workspace);

            function Transform (Number : Integer) return String is
            begin
               if Number < 0 then
                  return "";
               else
                  declare
                     Base : Unsigned_32 := Unsigned_32 (Number);
                     X : Unsigned_32;
                     Y : Unsigned_32;
                  begin
                     X := (Base and 16#FF#);
                     Y := Shift_Right (Base , 8);

                     return "(" & (X'Image) & "," & (Y'Image) & ")";
                  end;
               end if;
            end;

            function Generate_Workspaces (List : Hyprland.State.Hyprland_Workspace_List) return Unbounded_String is
               Result : Unbounded_String;
            begin
               for W of List loop
                  Append (Result, Transform (Integer'Value (Hyprland.State_Impl.Image (W.Id))));
               end loop;

               return Result;
            end Generate_Workspaces;
         begin
            Result.Set_Field ("class", Active_Window.Class);
            Result.Set_Field ("title", Active_Window.Title);
            Result.Set_Field ("workspace", Transform (Natural'Value (Hyprland.State_Impl.Image (Active_Workspace.Id))));
            Result.Set_Field ("workspaces", Generate_Workspaces (Hypr_State.Workspaces));
            Put_Line (Result.Write);
         end;
      end if;
   end loop Main_Loop;

--   Hypr_State.Disconnect
end hyprwatch;
