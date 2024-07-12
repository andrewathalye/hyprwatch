with Ada.Strings.Unbounded;

package body D_Bus_Helpers.Impl is
   function Get_Signature (Message : D_Bus.Messages.Message_Type) return String
   is

      use Ada.Strings.Unbounded;

      Arguments : constant D_Bus.Arguments.Argument_List_Type :=
        D_Bus.Messages.Get_Arguments (Message);

      Buf : Unbounded_String;

   begin
      for Index in 1 .. Arguments.Get_Count loop
         Append
           (Buf,
            D_Bus.Arguments.Get_Signature (Arguments.Get_Element (Index)));
      end loop;

      return To_String (Buf);
   end Get_Signature;

   function To_Direction
     (Direction : D_Bus.Arguments.Basic.String_Type)
      return Hypr_Helpers.Hyprland_Direction
   is

      use all type Hypr_Helpers.Hyprland_Direction;

      Direction_Str : constant String :=
        D_Bus.Arguments.Basic.To_String (Direction);

   begin
      return
        (case Direction_Str (1) is when 'l' => Left, when 'r' => Right,
           when 'u' => Up, when 'd' => Down, when others => Unknown);
   end To_Direction;

   procedure Raise_Signature_Error
     (Request :     D_Bus.Messages.Message_Type;
      Reply   : out D_Bus.Messages.Message_Type; Actual, Expected : String)
   is
   begin
      Reply :=
        D_Bus.Messages.New_Error
          (Request, Signature_Error,
           ASCII.Quotation & Actual & ASCII.Quotation & " != " &
           ASCII.Quotation & Expected & ASCII.Quotation);
   end Raise_Signature_Error;
end D_Bus_Helpers.Impl;
