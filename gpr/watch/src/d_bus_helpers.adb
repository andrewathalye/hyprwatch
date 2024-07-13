with D_Bus.Connection.G_Main;
with D_Bus.Types;
use type D_Bus.Types.Obj_Path;

--  Methods
with D_Bus_Helpers.Introspect;
with D_Bus_Helpers.Get_Workspaces;
with D_Bus_Helpers.Activate_Workspace;
with D_Bus_Helpers.Activate_Workspace_Rel;
with D_Bus_Helpers.Move_Window;
with D_Bus_Helpers.Move_Window_Rel;
with D_Bus_Helpers.Update_Keyboard_Layout;
with D_Bus_Helpers.Dump_Data;

package body D_Bus_Helpers is
   -------------------------
   --  Hypr_Service_Type  --
   -------------------------

   overriding procedure Initialize (Obj : in out Hypr_Service_Type) is
   begin
      --  org.freedesktop.DBus.Introspectable
      Obj.Register ("Introspect", D_Bus_Helpers.Introspect'Access);

      --  tk.zenithseeker.hyprwatch
      Obj.Register ("GetWorkspaces", D_Bus_Helpers.Get_Workspaces'Access);
      Obj.Register
        ("ActivateWorkspace", D_Bus_Helpers.Activate_Workspace'Access);
      Obj.Register
        ("ActivateWorkspaceRel", D_Bus_Helpers.Activate_Workspace_Rel'Access);
      Obj.Register ("MoveWindow", D_Bus_Helpers.Move_Window'Access);
      Obj.Register ("MoveWindowRel", D_Bus_Helpers.Move_Window_Rel'Access);
      Obj.Register
        ("UpdateKeyboardLayout", D_Bus_Helpers.Update_Keyboard_Layout'Access);
      Obj.Register ("DumpData", D_Bus_Helpers.Dump_Data'Access);
   end Initialize;

   function Connection
     (Service : Hypr_Service_Type) return D_Bus.Connection.Connection_Type
   is
   begin
      if not Service.Valid then
         raise Program_Error with "Service not connected!";
      end if;

      return Service.Bus;
   end Connection;

   procedure Connect
     (Service : in out Hypr_Service_Type;
      State   :        Hyprland.State.Hyprland_State_Access)
   is
   begin
      if Service.Valid then
         raise Program_Error with "Service has already been connected";
      end if;

      Service.Bus   := D_Bus.Connection.Connect;
      Service.State := State;

      D_Bus.Connection.Request_Name (Service.Bus, "tk.zenithseeker.hyprwatch");
      D_Bus.Connection.G_Main.Register_Object
        (Connection => Service.Bus, Path => +"/tk/zenithseeker/hyprwatch",
         Object     => Service);

      D_Bus.Connection.G_Main.Setup_With_G_Main (Service.Bus);

      Service.Valid := True;
   end Connect;

end D_Bus_Helpers;
