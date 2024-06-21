with D_Bus.Messages;

package body D_Bus_Methods is
   procedure Get_Workspaces
     (Request :     D_Bus.Messages.Message_Type;
      Reply   : out D_Bus.Messages.Message_Type)
   is
   begin
      raise Program_Error with "Get_Workspaces called: noop";
   end Get_Workspaces;

   overriding procedure Initialize (Obj : in out Hypr_Service_Type) is
   begin
      Obj.Register ("get_workspaces", Get_Workspaces'Access);
   end Initialize;
end D_Bus_Methods;
