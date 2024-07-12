with D_Bus.Arguments.Basic;

with Hyprland.State_Impl;

with D_Bus_Helpers.Impl; use D_Bus_Helpers.Impl;
with Hypr_Helpers;

with Debug; use Debug;

procedure D_Bus_Helpers.Move_Window_Rel
  (Request :     D_Bus.Messages.Message_Type;
   Reply   : out D_Bus.Messages.Message_Type)
is

   use type Hyprland.State.Hyprland_Window_Id;
   use all type Hypr_Helpers.Hyprland_Direction;

   Request_Signature : constant String := Get_Signature (Request);
   Arguments         : constant D_Bus.Arguments.Argument_List_Type :=
     D_Bus.Messages.Get_Arguments (Request);

   Window    : D_Bus.Arguments.Basic.U_Int64_Type;
   Direction : D_Bus.Arguments.Basic.String_Type;

   Ada_Window    : Hyprland.State.Hyprland_Window_Id;
   Ada_Direction : Hypr_Helpers.Hyprland_Direction;

begin
   Put_Debug ("Move_Window_Rel: " & Request_Signature);

   --  Check signature

   if Request_Signature /= Move_Window_Rel_In then
      Raise_Signature_Error
        (Request, Reply, Request_Signature, Move_Window_Rel_In);
      return;
   end if;

   Window    := D_Bus.Arguments.Basic.U_Int64_Type (Arguments.First_Element);
   Direction := D_Bus.Arguments.Basic.String_Type (Arguments.Last_Element);

   Ada_Window := Hyprland.State_Impl.Value (Window.To_String);

   if Ada_Window = Hyprland.State.No_Window then
      Ada_Window := Global_Service.State.Active_Window;

      --  If there was no active window either

      if Ada_Window = Hyprland.State.No_Window then
         Reply :=
           D_Bus.Messages.New_Error
             (Reply_To      => Request, Error_Name => Arguments_Error,
              Error_Message =>
                "A null Window Address was provided and no window" &
                " is selected.");
         return;
      end if;
   end if;

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

   --  Move selected window to workspace computed by moving current one
   --  on 2D grid in the correct direction
   Global_Service.State.Move_Window
     (Window      => Ada_Window,
      Destination =>
        Hypr_Helpers.Convert
          (Hypr_Helpers.Translate
             (W2D       =>
                Hypr_Helpers.Convert (Global_Service.State.Active_Workspace),
              Direction => Ada_Direction)));

   Reply := D_Bus.Messages.New_Method_Return (Request);
end D_Bus_Helpers.Move_Window_Rel;
