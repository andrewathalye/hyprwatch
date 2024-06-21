with D_Bus.Messages;
with D_Bus.Service;

package D_Bus_Methods is
   type Hypr_Service_Type is new D_Bus.Service.Object with private;

   overriding procedure Initialize (Obj : in out Hypr_Service_Type);
private
   type Hypr_Service_Type is new D_Bus.Service.Object with null record;
end D_Bus_Methods;
