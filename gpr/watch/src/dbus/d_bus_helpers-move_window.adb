with D_Bus.Arguments.Basic;
with D_Bus.Arguments.Containers;

with Hyprland.State_Impl;

with D_Bus_Helpers.Impl; use D_Bus_Helpers.Impl;
with Hypr_Helpers;

with Debug; use Debug;

procedure D_Bus_Helpers.Move_Window
  (Request :     D_Bus.Messages.Message_Type;
   Reply   : out D_Bus.Messages.Message_Type)
is
   Request_Signature : constant String := Get_Signature (Request);
begin
   Put_Debug ("Move_Window " & Request_Signature);

   --  Check signature
   if Request_Signature /= Move_Window_In then
      Raise_Signature_Error
        (Request, Reply, Request_Signature, Move_Window_In);
      return;
   end if;

   declare
      use type Hyprland.State.Hyprland_Window_Id;

      Parameters : constant D_Bus.Arguments.Argument_List_Type     :=
        D_Bus.Messages.Get_Arguments (Request);
      W_D_Bus    : constant D_Bus.Arguments.Basic.U_Int64_Type     :=
        D_Bus.Arguments.Basic.U_Int64_Type (Parameters.First_Element);
      WS_D_Bus   : constant D_Bus.Arguments.Containers.Struct_Type :=
        D_Bus.Arguments.Containers.Struct_Type (Parameters.Last_Element);

      W  : Hyprland.State.Hyprland_Window_Id :=
        Hyprland.State_Impl.Value (W_D_Bus.To_String);
      WS : Hyprland.State.Hyprland_Workspace_Id;
   begin
      --  If no window specified, use active
      if W = Hyprland.State.No_Window then
         W := Global_Service.State.Active_Window;
      end if;

      Set_WS :
      declare
         X : Hypr_Helpers.Workspace_2D_Axis;
         Y : Hypr_Helpers.Workspace_2D_Axis;
      begin
         X  :=
           Hypr_Helpers.Workspace_2D_Axis
             (D_Bus.Arguments.Basic.To_Ada
                (D_Bus.Arguments.Basic.Byte_Type (WS_D_Bus.First_Element)));
         Y  :=
           Hypr_Helpers.Workspace_2D_Axis
             (D_Bus.Arguments.Basic.To_Ada
                (D_Bus.Arguments.Basic.Byte_Type (WS_D_Bus.Last_Element)));
         WS := Hypr_Helpers.Convert ((X, Y));
      exception
         when Constraint_Error =>
            Reply :=
              D_Bus.Messages.New_Error
                (Reply_To      => Request, Error_Name => Arguments_Error,
                 Error_Message =>
                   "The provided workspace coordinates were invalid.");
            return;
      end Set_WS;
      Global_Service.State.Move_Window (W, WS);
   end;

   Reply := D_Bus.Messages.New_Method_Return (Request);
end D_Bus_Helpers.Move_Window;
