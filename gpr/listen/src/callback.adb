with Ada.Text_IO;

with D_Bus.Arguments.Basic;

procedure Callback (Message : D_Bus.Messages.Message_Type) is
begin
   if D_Bus.Messages.Get_Interface (Message) /= "tk.zenithseeker.hyprwatch"
   then
      return;
   end if;

   --  Hopefully redundant checks
   if D_Bus.Messages.Get_Arguments (Message).Is_Empty then
      raise Program_Error with "HyprUpdate message was empty.";
   end if;

   if D_Bus.Messages.Get_Arguments (Message).First_Element.Get_Signature /= "s"
   then
      raise Program_Error with "HyprUpdate signature was incorrect.";
   end if;

   --  Print the message
   Ada.Text_IO.Put_Line
     (D_Bus.Arguments.Basic.String_Type
        (D_Bus.Messages.Get_Arguments (Message).First_Element)
        .To_String);
end Callback;
