with D_Bus.Service;
with D_Bus.Connection;

with Hyprland.State;

package D_Bus_Helpers is
   --  D_Bus service implementation for Hyprwatch

   type Hypr_Service_Type is new D_Bus.Service.Object with private;

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
   --  Return the `Service`’s underlying D_Bus connection.

   procedure Disconnect (Service : in out Hypr_Service_Type) is null;
   --  Disconnects `Service`. This is currently a no-op.

private
   type Hypr_Service_Type is new D_Bus.Service.Object with record
      Valid : Boolean := False;
      Bus   : D_Bus.Connection.Connection_Type;
      State : Hyprland.State.Hyprland_State_Access;
   end record;

   overriding procedure Initialize (Obj : in out Hypr_Service_Type);
end D_Bus_Helpers;
