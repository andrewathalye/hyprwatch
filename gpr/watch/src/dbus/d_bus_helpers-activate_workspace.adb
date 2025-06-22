with D_Bus.Arguments.Basic;
with D_Bus.Arguments.Containers;

with D_Bus_Helpers.Impl; use D_Bus_Helpers.Impl;
with Hypr_Helpers;

with Hyprland.State;
use type Hyprland.State.Hyprland_Workspace_Id;

with Debug; use Debug;

procedure D_Bus_Helpers.Activate_Workspace
  (Request :     D_Bus.Messages.Message_Type;
   Reply   : out D_Bus.Messages.Message_Type)
is
   use Hypr_Helpers;

   Request_Signature : constant String := Get_Signature (Request);

   Workspace_D_Bus : D_Bus.Arguments.Containers.Struct_Type;
   Workspace       : Hypr_Helpers.Workspace_2D;
begin
   Put_Debug ("Activate_Workspace: " & Request_Signature);

   --  Check signature
   if Request_Signature /= Activate_Workspace_In then
      Raise_Signature_Error
        (Request, Reply, Request_Signature, Activate_Workspace_In);
      return;
   end if;

   Workspace_D_Bus :=
     D_Bus.Arguments.Containers.Struct_Type
       (D_Bus.Messages.Get_Arguments (Request).First_Element);

   begin
      Workspace :=
        (X =>
           Workspace_2D_Axis
             (D_Bus.Arguments.Basic.To_Ada
                (D_Bus.Arguments.Basic.Byte_Type
                   (Workspace_D_Bus.First_Element))),
         Y =>
           Workspace_2D_Axis
             (D_Bus.Arguments.Basic.To_Ada
                (D_Bus.Arguments.Basic.Byte_Type
                   (Workspace_D_Bus.Last_Element))));
   exception
      when Constraint_Error =>
         Reply :=
           D_Bus.Messages.New_Error
             (Reply_To      => Request, Error_Name => Arguments_Error,
              Error_Message =>
                "The arguments could not be parsed as a 2D workspace.");
         return;
   end;

   Global_Service.State.Activate_Workspace (Hypr_Helpers.Convert (Workspace));

   Reply := D_Bus.Messages.New_Method_Return (Request);
end D_Bus_Helpers.Activate_Workspace;
