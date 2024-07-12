with D_Bus.Arguments.Basic;
with D_Bus.Arguments.Containers;

with D_Bus_Helpers.Impl; use D_Bus_Helpers.Impl;
with Hypr_Helpers;

with Debug; use Debug;

procedure D_Bus_Helpers.Get_Workspaces
  (Request :     D_Bus.Messages.Message_Type;
   Reply   : out D_Bus.Messages.Message_Type)
is
   Request_Signature : constant String := Get_Signature (Request);

   List : D_Bus.Arguments.Containers.Array_Type;
begin
   Put_Debug ("Get_Workspaces " & Request_Signature);

   --  Check Signature
   if Request_Signature /= Get_Workspaces_In then
      Reply :=
        D_Bus.Messages.New_Error
          (Reply_To      => Request, Error_Name => Signature_Error,
           Error_Message =>
             ASCII.Quotation & Request_Signature & ASCII.Quotation & " != " &
             ASCII.Quotation & Get_Workspaces_In & ASCII.Quotation);
      return;
   end if;

   for W of Global_Service.State.Workspaces loop
      if not Hyprland.State.Is_Special (W.Id) then
         declare

            W2D : constant Hypr_Helpers.Workspace_2D :=
              Hypr_Helpers.Convert (W.Id);

         begin
            declare
               use type D_Bus.Arguments.Basic.Byte_Type;

               S : D_Bus.Arguments.Containers.Struct_Type;
            begin
               S.Append (+D_Bus.Byte (W2D.X));
               S.Append (+D_Bus.Byte (W2D.Y));

               List.Append (S);
            end;
         end;
      end if;
   end loop;

   Reply := D_Bus.Messages.New_Method_Return (Request);
   D_Bus.Messages.Add_Arguments
     (Msg => Reply, Args => D_Bus.Arguments.Argument_List_Type (List));
end D_Bus_Helpers.Get_Workspaces;
