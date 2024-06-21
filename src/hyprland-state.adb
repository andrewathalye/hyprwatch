pragma Ada_2022;

with Ada.Unchecked_Deallocation;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded;

with GNATCOLL.JSON;

package body Hyprland.State is
   --  Intentionally blank Hyprland_State
   Blank_State : constant Hyprland_State := (others => <>);

   procedure Deallocate is new Ada.Unchecked_Deallocation (Object => Protocol.Hyprland_Connection, Name => HCA);

   ---------------
   -- Accessors --
   ---------------

   function Workspaces (State : Hyprland_State) return Hyprland_Workspace_List
   is (State.Workspaces);

   function Windows (State : Hyprland_State) return Hyprland_Window_List
   is (State.Windows);

   function Active_Window (State : Hyprland_State) return Hyprland_Window_Id
   is (State.Active_Window);

   function Active_Workspace
     (State : Hyprland_State) return Hyprland_Workspace_Id
   is (State.Active_Workspace);

   function Fullscreen (State : Hyprland_State) return Boolean
   is (State.Fullscreen);


   ----------------
   --  Is_Valid  --
   ----------------
   function Is_Valid (State : Hyprland_State) return Boolean
   is (State.Valid);

   -------------
   -- Connect --
   -------------

   procedure Connect (State : in out Hyprland_State) is
   begin
      if State.Valid then
         raise Invalid_State with "Hyprland State already Connected.";
      end if;

      State.Connection := new Protocol.Hyprland_Connection;
      Protocol.Connect (State.Connection.all);

      ---------------------
      --  Initial State  --
      ---------------------
      --  We try to dynamically grab info from Hyprland’s Hypr2 socket where possible,
      --  but when first starting there is no choice but to use Hypr1.
      Read_Workspaces : declare
         use GNATCOLL.JSON;

         W : constant String := Protocol.Send_Message
           (Hypr => State.Connection.all,
            Id => Workspaces);
         W_JSON : constant JSON_Value := Read (W);
      begin
         for Object of JSON_Array'(W_JSON.Get) loop
            declare
               use Hyprland.State_Impl;

               Workspace : Hyprland_Workspace;
               Id : Hyprland_Workspace_Id := Value (Integer'(Object.Get ("id"))'Image);
            begin
               Workspace.Id := Id;
               Workspace.Name := Object.Get ("name");

               State.Workspaces.Insert (Id, Workspace);
            end;
         end loop;
      end Read_Workspaces;

      Read_Active_Workspace : declare
         use GNATCOLL.JSON;
         use Hyprland.State_Impl;

         W : constant String := Protocol.Send_Message
           (Hypr => State.Connection.all,
            Id => Activeworkspace);
         W_JSON : constant JSON_Value := Read (W);
      begin
         State.Active_Workspace := Value (Integer'(W_JSON.Get ("id"))'Image);
      end Read_Active_Workspace;

      Read_Windows : declare
         use GNATCOLL.JSON;
         use Hyprland.State_Impl;

         W : constant String := Protocol.Send_Message
           (Hypr => State.Connection.all,
            Id => Clients);
         W_JSON : constant JSON_Value := Read (W);
      begin
         for Object of JSON_Array'(W_JSON.Get) loop
            declare
               Window : Hyprland_Window;
               Raw_Address : constant String := Object.Get ("address"); --  In the format 0x1234567 instead of 1234567
               Id : constant Hyprland_Window_Id := Value ("16#" & Raw_Address (Raw_Address'First + 2 .. Raw_Address'Last) & "#");
            begin
               Window.Id := Id;
               Window.Workspace := Value (Integer'(Object.Get ("workspace").Get ("id"))'Image);
               Window.Class := Object.Get ("class");
               Window.Title := Object.Get ("title");

               State.Windows.Insert (Id, Window);
               State.Workspaces (Window.Workspace).Windows.Append (Id);
            end;
         end loop;
      end Read_Windows;

      Read_Active_Window : declare
         use GNATCOLL.JSON;
         use Hyprland.State_Impl;

         W : constant String := Protocol.Send_Message
           (Hypr => State.Connection.all,
            Id => Activewindow);
         W_JSON : constant JSON_Value := Read (W);

         Raw_Address : constant String := W_JSON.Get ("address"); --  See above for format issues
      begin
         State.Active_Window := Value ("16#" & Raw_Address (Raw_Address'First + 2 .. Raw_Address'Last) & "#");
      end Read_Active_Window;

      State.Valid := True;
   end Connect;

   ----------------
   -- Disconnect --
   ----------------

   procedure Disconnect (State : in out Hyprland_State) is
   begin
      Protocol.Disconnect (State.Connection.all);
      Deallocate (State.Connection);

      --  Intentionally wipe state
      State := Blank_State;
   end Disconnect;

   ------------
   -- Update --
   ------------
   function Update (State : in out Hyprland_State) return Boolean is
      use Ada.Strings.Unbounded;
      use Hyprland.State_Impl;

      function "+" (Item : Unbounded_String) return String renames To_String;
      type Unbounded_String_Array is array (Natural range <>) of Unbounded_String;

      function Parse_Args (Msg : Unbounded_String; Length : Positive) return Unbounded_String_Array
      is
         Result : Unbounded_String_Array (1 .. Length);

         First : Positive := 1;
         Last : Positive;

         Index : Positive := 1;

         Count : Positive := 1;
      begin
         for C of To_String (Msg) loop
            if C = ',' then
               Last := Index - 1;
               Result (Count) := Ada.Strings.Unbounded.Unbounded_Slice (Msg, First, Last);

               First := Index + 1;
               Count := @ + 1;
            end if;
            Index := @ + 1;
         end loop;

         return Result;
      end Parse_Args;

      Msg : Protocol.Hypr2_Message;

      Active_Class : Unbounded_String;
      Active_Title : Unbounded_String;

      Updated : Boolean := False;
   begin
      while Protocol.Has_Messages (State.Connection.all) loop
         Msg := Protocol.Receive_Message (State.Connection.all);

         Put_Line (Msg.Msg_Id'Image & ": " & (+Msg.Msg_Body));

         Updated := True;
         case Msg.Msg_Id is
            when Workspacev2 =>
               --  Workspace_Id, Workspace_Name
               declare
                  Args : constant Unbounded_String_Array := Parse_Args (Msg.Msg_Body, 2);
                  Workspace_Id : constant Hyprland_Workspace_Id := Value (+(Args (1)));
               begin
                  State.Active_Workspace := Workspace_Id;
               end;
            when Activewindow =>
               --  Class, Title
               --  Note: But not associated with a window id and activewindowv2 comes after,
               --  so our own record-keeping would assign these to the wrong window
               declare
                  Args : constant Unbounded_String_Array := Parse_Args (Msg.Msg_Body, 2);
               begin
                  Active_Class := Args (1);
                  Active_Title := Args (2);
               end;
            when Activewindowv2 =>
               --  Window_Address
               declare
                  Window_Id : constant Hyprland_Window_Id := Value ("16#" & (+Msg.Msg_Body) & "#");
               begin
                  State.Active_Window := Window_Id;

                  --  Update the class and title if the window was marked stale
                  --  Skip this if the window hasn’t been OPENed yet (a new window)
                  if State.Windows.Contains (Window_Id)
                     and then State.Windows (State.Active_Window).Title_Stale
                  then
                     State.Windows (State.Active_Window).Class := Active_Class;
                     State.Windows (State.Active_Window).Title := Active_Title;

                     State.Windows (State.Active_Window).Title_Stale := False;
                  end if;
               end;
            when Fullscreen =>
               --  0 / 1
               State.Fullscreen := (if (+Msg.Msg_Body) = "1" then True else False);
            when Createworkspacev2 =>
               --  Workspace_Id, Workspace_Name
               declare
                  Args : constant Unbounded_String_Array := Parse_Args (Msg.Msg_Body, 2);
                  Workspace_Id : constant Hyprland_Workspace_Id := Value (+Args (1));
                  Workspace_Name : constant Unbounded_String := Args (2);

                  Workspace : constant Hyprland_Workspace := (Workspace_Id, Workspace_Name, others => <>);
               begin
                  State.Workspaces.Insert (Workspace_Id, Workspace);
               end;
            when Destroyworkspacev2 =>
               --  Workspace_Id, Workspace_Name
               declare
                  Args : constant Unbounded_String_Array := Parse_Args (Msg.Msg_Body, 2);
                  Workspace_Id : constant Hyprland_Workspace_Id := Value (+(Args (1)));
               begin
                  State.Workspaces.Delete (Workspace_Id);
               end;
            when Renameworkspace =>
               --  Workspace_Id, New_Name
               declare
                  Args : constant Unbounded_String_Array := Parse_Args (Msg.Msg_Body, 2);
                  Workspace_Id : constant Hyprland_Workspace_Id := Value (+(Args (1)));
                  New_Name : constant Unbounded_String := Args (2);
               begin
                  State.Workspaces (Workspace_Id).Name := New_Name;
               end;
            when Openwindow =>
               --  Window_Address, Workspace_Name, Window_Class, Window_Title
               declare
                  Args : constant Unbounded_String_Array := Parse_Args (Msg.Msg_Body, 4);
                  Window_Id : constant Hyprland_Window_Id := Value ("16#" & (+(Args (1))) & "#");
                  Window_Class : constant Unbounded_String := Args (3);
                  Window_Title : constant Unbounded_String := Args (4);

                  --  TODO: Need a source for the actual workspace id
                  Window : constant Hyprland_Window :=
                   (Id          => Window_Id,
                     Workspace   => State.Active_Workspace,
                     Class       => Window_Class,
                     Title       => Window_Title,
                     others => <>);
               begin
                  State.Windows.Insert (Window_Id, Window);
                  State.Workspaces (State.Active_Workspace).Windows.Append (Window_Id);
               end;
            when Closewindow =>
               --  Window_Address
               declare
                  Window_Id : constant Hyprland_Window_Id := Value ("16#" & (+Msg.Msg_Body) & "#");

                  Relevant_Workspace : Hyprland_Workspace renames State.Workspaces (State.Windows (Window_Id).Workspace);
               begin
                  Relevant_Workspace.Windows.Delete (Relevant_Workspace.Windows.Find_Index (Window_Id));
                  State.Windows.Delete (Window_Id);
               end;
            when Movewindowv2 =>
               --  Window_Address, Workspace_Id, Workspace_Name
               declare
                  Args : constant Unbounded_String_Array := Parse_Args (Msg.Msg_Body, 3);
                  Window_Id : constant Hyprland_Window_Id := Value ("16#" & (+(Args (1))) & "#");
                  Workspace_Id : constant Hyprland_Workspace_Id := Value (+(Args (2)));

                  Relevant_Workspace : Hyprland_Workspace renames State.Workspaces (State.Windows (Window_Id).Workspace);
               begin
                  Relevant_Workspace.Windows.Delete (Relevant_Workspace.Windows.Find_Index (Window_Id));
                  State.Workspaces (Workspace_Id).Windows.Append (Window_Id);
               end;
            when Windowtitle =>
               --  Window_Address
               declare
                  Window_Id : constant Hyprland_Window_Id := Value ("16#" & (+Msg.Msg_Body) & "#");
               begin
                  --  This is also sent when a window has just been created, in which case
                  --  there is no need to parse it
                  if State.Windows.Contains (Window_Id) then
                     --  The title will be refreshed when it is once again the active window
                     State.Windows (Window_Id).Title_Stale := True;
                  end if;
               end;
            when others =>
               --  TODO unhandled command?
               Updated := False;
         end case;
      end loop;

      return Updated;
   end Update;

   -----------------
   -- Move_Window --
   -----------------

   procedure Move_Window
     (State : in out Hyprland_State;
      Window : Hyprland_Window_Id;
      Destination : Hyprland_Workspace_Id)
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "Move_Window unimplemented");
      raise Program_Error with "Unimplemented procedure Move_Window";
   end Move_Window;

end Hyprland.State;
