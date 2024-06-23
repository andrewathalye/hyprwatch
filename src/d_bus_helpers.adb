with Ada.Strings.Unbounded;
with Ada.Text_IO;

with D_Bus.Messages;
with D_Bus.Connection.G_Main;
with D_Bus.Arguments.Containers;
with D_Bus.Arguments.Basic;
with D_Bus.Types;
use type D_Bus.Types.Obj_Path;

with Hypr_Helpers;

package body D_Bus_Helpers is
   function To_Struct (W2D : Hypr_Helpers.Workspace_2D)
      return D_Bus.Arguments.Containers.Struct_Type;
   function To_Struct (W2D : Hypr_Helpers.Workspace_2D)
      return D_Bus.Arguments.Containers.Struct_Type
   is
      use type D_Bus.Arguments.Basic.Byte_Type;

      Struct : D_Bus.Arguments.Containers.Struct_Type;
   begin
      Struct.Append (+D_Bus.Byte (W2D.X));
      Struct.Append (+D_Bus.Byte (W2D.Y));

      return Struct;
   end To_Struct;

   ---------------------------------
   --  D_Bus Method Declarations  --
   ---------------------------------
   procedure Get_Workspaces
     (Request :     D_Bus.Messages.Message_Type;
      Reply   : out D_Bus.Messages.Message_Type);

   ------------------------------------
   --  D_Bus Method Implementations  --
   ------------------------------------
   procedure Get_Workspaces
     (Request :     D_Bus.Messages.Message_Type;
      Reply   : out D_Bus.Messages.Message_Type)
   is
      use Ada.Text_IO;

      List : D_Bus.Arguments.Containers.Array_Type;
   begin
      Put_Line (Standard_Error, "D_Bus: Get_Workspaces");

      for W of Global_Service.State.Workspaces loop
         if Ada.Strings.Unbounded.To_String (W.Name) /= "special:special" then
            declare
               W2D : constant Hypr_Helpers.Workspace_2D :=
                  Hypr_Helpers.Transform (W.Id);
            begin
               List.Append (To_Struct (W2D));
            end;
         end if;
      end loop;

      Reply := D_Bus.Messages.New_Method_Return (Request);
      D_Bus.Messages.Add_Arguments (
         Msg => Reply,
         Args => D_Bus.Arguments.Argument_List_Type (List));
   end Get_Workspaces;

   -------------------------
   --  Hypr_Service_Type  --
   -------------------------
   overriding procedure Initialize (Obj : in out Hypr_Service_Type) is
   begin
      Obj.Register ("get_workspaces", Get_Workspaces'Access);
   end Initialize;

   procedure Connect
      (Service : in out Hypr_Service_Type;
       State : Hyprland.State.Hyprland_State_Access) is
   begin
      if Service.Valid then
         raise Program_Error with "Service has already been connected";
      end if;

      Service.Bus := D_Bus.Connection.Connect;
      Service.State := State;


      D_Bus.Connection.Request_Name (Service.Bus, "tk.zenithseeker.hyprwatch");
      D_Bus.Connection.G_Main.Register_Object
        (Connection => Service.Bus,
         Path       => +"/tk/zenithseeker/hyprwatch",
         Object     => Service);

      D_Bus.Connection.G_Main.Setup_With_G_Main (Service.Bus);

      Service.Valid := True;
   end Connect;
end D_Bus_Helpers;
