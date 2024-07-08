with D_Bus.Arguments.Basic;
with D_Bus.Arguments.Containers;

with Hypr_Helpers;

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
      Reply :=
        D_Bus.Messages.New_Error
          (Reply_To      => Request, Error_Name => Signature_Error,
           Error_Message =>
             ASCII.Quotation & Request_Signature & ASCII.Quotation & " != " &
             ASCII.Quotation & Activate_Workspace_In & ASCII.Quotation);
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
                (D_Bus.Arguments.Basic.U_Int16_Type
                   (Workspace_D_Bus.First_Element))),
         Y =>
           Workspace_2D_Axis
             (D_Bus.Arguments.Basic.To_Ada
                (D_Bus.Arguments.Basic.U_Int16_Type
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

   Put_Debug (Image (Workspace));

   Global_Service.State.Activate_Workspace (Hypr_Helpers.Convert (Workspace));

   Reply := D_Bus.Messages.New_Method_Return (Request);
end D_Bus_Helpers.Activate_Workspace;
