with D_Bus.Service;
with D_Bus.Connection;
private with D_Bus.Messages;

with Hyprland.State;

package D_Bus_Helpers is
   --  Implementation for the D_Bus side of Hyprwatch

   type Hypr_Service_Type is new D_Bus.Service.Object with private;
   overriding procedure Initialize (Obj : in out Hypr_Service_Type);
   --  Automatically called as needed by D_Bus_Ada

   Global_Service : access Hypr_Service_Type;
   --  Global instance of a Hypr_Service_Type to be used by
   --  D_Bus method dispatchers.
   --
   --  NOT THREAD SAFE

   procedure Connect
     (Service : in out Hypr_Service_Type;
      State   :        Hyprland.State.Hyprland_State_Access);
   --  Connect to D_Bus and set up the Service

   function Connection
     (Service : Hypr_Service_Type) return D_Bus.Connection.Connection_Type;
   --  Return the Service’s underlying D_Bus connection.

   procedure Disconnect (Service : in out Hypr_Service_Type) is null;
   --  TODO (not needed at the moment)

private
   type Hypr_Service_Type is new D_Bus.Service.Object with record
      Valid : Boolean := False;
      Bus   : D_Bus.Connection.Connection_Type;
      State : Hyprland.State.Hyprland_State_Access;
   end record;

   ---------------
   -- Constants --
   ---------------

   Signature_Error : constant String :=
     "org.freedesktop.DBus.Error.InvalidSignature";
   Arguments_Error : constant String :=
     "org.freedesktop.DBus.Error.InvalidArgs";
   Hyprland_Error  : constant String :=
     "tk.zenithseeker.hyprwatch.Error.RequestFailed";

   ----------------------------------
   -- Exported to Private Children --
   ----------------------------------
   function Get_Signature
     (Message : D_Bus.Messages.Message_Type) return String;

   -----------------------
   -- Method Signatures --
   -----------------------
   Introspect_In  : constant String := "";
   Introspect_Out : constant String := "";

   Get_Workspaces_In  : constant String := "";
   Get_Workspaces_Out : constant String := "a(qq)";

   Activate_Workspace_In  : constant String := "(qq)";
   Activate_Workspace_Out : constant String := "";

   Update_Keyboard_Layout_In  : constant String := "s(ss)";
   Update_Keyboard_Layout_Out : constant String := "";
end D_Bus_Helpers;
