with D_Bus.Arguments.Basic;

with D_Bus_Helpers.Impl; use D_Bus_Helpers.Impl;
with Hypr_Helpers;

with Debug; use Debug;

procedure D_Bus_Helpers.Activate_Workspace_Rel
  (Request :     D_Bus.Messages.Message_Type;
   Reply   : out D_Bus.Messages.Message_Type)
is

   use all type Hypr_Helpers.Hyprland_Direction;

   Request_Signature : constant String := Get_Signature (Request);
   Arguments         : constant D_Bus.Arguments.Argument_List_Type :=
     D_Bus.Messages.Get_Arguments (Request);

   Direction : D_Bus.Arguments.Basic.String_Type;

   Ada_Direction : Hypr_Helpers.Hyprland_Direction;

begin
   Put_Debug ("Activate_Workspace_Rel: " & Request_Signature);

   --  Check signature
   if Request_Signature /= Activate_Workspace_Rel_In then
      Raise_Signature_Error
        (Request  => Request, Reply => Reply, Actual => Request_Signature,
         Expected => Activate_Workspace_In);
      return;
   end if;

   Direction := D_Bus.Arguments.Basic.String_Type (Arguments.First_Element);

   Ada_Direction := To_Direction (Direction);

   if Ada_Direction = Unknown then
      Reply :=
        D_Bus.Messages.New_Error
          (Reply_To      => Request, Error_Name => Arguments_Error,
           Error_Message =>
             "Value for parameter " & ASCII.Quotation & "direction" &
             ASCII.Quotation & " was not understood.");
      return;
   end if;

   --  Activate selected workspace
   Global_Service.State.Activate_Workspace
     (Hypr_Helpers.Convert
        (Hypr_Helpers.Translate
           (W2D       =>
              Hypr_Helpers.Convert (Global_Service.State.Active_Workspace),
            Direction => Ada_Direction)));

   Reply := D_Bus.Messages.New_Method_Return (Request);
end D_Bus_Helpers.Activate_Workspace_Rel;
