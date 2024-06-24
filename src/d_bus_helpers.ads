with D_Bus.Service;
with D_Bus.Connection;

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

   procedure Disconnect (Service : in out Hypr_Service_Type) is null;
   --  TODO (not needed at the moment)
private
   type Hypr_Service_Type is new D_Bus.Service.Object with record
      Valid : Boolean := False;
      Bus   : D_Bus.Connection.Connection_Type;
      State : Hyprland.State.Hyprland_State_Access;
   end record;
end D_Bus_Helpers;
