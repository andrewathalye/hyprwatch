with D_Bus.Arguments.Basic;
with D_Bus.Arguments.Containers;

with Hyprland.State;
with D_Bus_Helpers.Impl; use D_Bus_Helpers.Impl;

with Debug; use Debug;

procedure D_Bus_Helpers.Update_Keyboard_Layout
  (Request :     D_Bus.Messages.Message_Type;
   Reply   : out D_Bus.Messages.Message_Type)
is
   use D_Bus.Arguments.Basic;
   use D_Bus.Arguments.Containers;

   Request_Signature : constant String := Get_Signature (Request);

   --  Parameters
   Parameters : D_Bus.Arguments.Argument_List_Type;
begin
   Put_Debug ("Update_Keyboard_Layout " & Request_Signature);

   --  Check signature
   if Request_Signature /= Update_Keyboard_Layout_In then
      Reply :=
        D_Bus.Messages.New_Error
          (Reply_To      => Request, Error_Name => Signature_Error,
           Error_Message =>
             ASCII.Quotation & Request_Signature & ASCII.Quotation & " != " &
             ASCII.Quotation & Update_Keyboard_Layout_In & ASCII.Quotation);
      return;
   end if;

   --  Read parameters
   Parameters := D_Bus.Messages.Get_Arguments (Request);
   declare
      Keyboard       : constant String :=
        To_String (String_Type (Parameters.First_Element));
      Layout_Main    : constant String :=
        To_String
          (String_Type
             (Struct_Type (Parameters.Get_Element (2)).First_Element));
      Layout_Variant : constant String :=
        To_String
          (String_Type
             (Struct_Type (Parameters.Get_Element (2)).Last_Element));
   begin
      Global_Service.State.Set_Layout (Keyboard, Layout_Main, Layout_Variant);
   exception
      when Hyprland.State.Request_Failed =>
         Reply :=
           D_Bus.Messages.New_Error
             (Reply_To      => Request, Error_Name => Hyprland_Error,
              Error_Message =>
                "Unable to set keyboard layout to " & Layout_Main & "-" &
                Layout_Variant);
         return;
   end;

   Reply := D_Bus.Messages.New_Method_Return (Request);
end D_Bus_Helpers.Update_Keyboard_Layout;
