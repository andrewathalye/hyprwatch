pragma Ada_2022;

with D_Bus.Arguments.Basic;

with D_Bus_Helpers.Impl; use D_Bus_Helpers.Impl;
with Debug;              use Debug;

procedure D_Bus_Helpers.Dump_Data
  (Request :     D_Bus.Messages.Message_Type;
   Reply   : out D_Bus.Messages.Message_Type)
is
   use type D_Bus.Arguments.Basic.String_Type;

   Request_Signature : constant String := Get_Signature (Request);
   Result            : D_Bus.Arguments.Argument_List_Type :=
     D_Bus.Arguments.Empty_Argument_List;
begin
   Put_Debug ("Dump_Data: " & Request_Signature);

   if Request_Signature /= Dump_Data_In then
      Raise_Signature_Error
        (Request  => Request, Reply => Reply, Actual => Request_Signature,
         Expected => Dump_Data_In);
      return;
   end if;

   Reply := D_Bus.Messages.New_Method_Return (Request);
   Result.Append (+Global_Service.State.all'Image);

   D_Bus.Messages.Add_Arguments (Reply, Result);
end D_Bus_Helpers.Dump_Data;
