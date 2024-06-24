pragma Ada_2022;

with Ada.Unchecked_Deallocation;

with GNATCOLL.JSON;
with Hyprland.State;

with Debug; use Debug;

package body Hyprland.State is
   --  Intentionally blank Hyprland_State
   Blank_State : constant Hyprland_State := (others => <>);

   procedure Deallocate is new Ada.Unchecked_Deallocation
     (Object => Protocol.Hyprland_Connection, Name => HCA);

   function "+"
     (Item : Ada.Strings.Unbounded.Unbounded_String) return String renames
     Ada.Strings.Unbounded.To_String;

   type Unbounded_String_Array is
     array (Natural range <>) of Ada.Strings.Unbounded.Unbounded_String;

   ---------------
   --  Private  --
   ---------------

   --  Parse Hypr2 arguments into an array
   function Parse_Args
     (Msg : Ada.Strings.Unbounded.Unbounded_String; Length : Positive)
      return Unbounded_String_Array;
   function Parse_Args
     (Msg : Ada.Strings.Unbounded.Unbounded_String; Length : Positive)
      return Unbounded_String_Array
   is
      use Ada.Strings.Unbounded;
      Result : Unbounded_String_Array (1 .. Length);

      Count : Positive := 1;
   begin
      for C of To_String (Msg) loop
         if C = ',' and Count /= Length then
            Count := @ + 1;
         else
            Append (Result (Count), C);
         end if;
      end loop;

      return Result;
   end Parse_Args;

   --  Strip a leading space if present
   function Strip_Space (Item : String) return String;
   function Strip_Space (Item : String) return String is
   begin
      if Item (Item'First) = ' ' then
         if Item'Length = 1 then
            return "";
         else
            return Item (Item'First + 1 .. Item'Last);
         end if;
      else
         return Item;
      end if;
   end Strip_Space;

   ---------------
   -- Accessors --
   ---------------

   function Workspaces
     (State : Hyprland_State) return Hyprland_Workspace_List is
     (State.Workspaces);

   function Windows (State : Hyprland_State) return Hyprland_Window_List is
     (State.Windows);

   function Active_Window (State : Hyprland_State) return Hyprland_Window_Id is
     (State.Active_Window);

   function Active_Workspace
     (State : Hyprland_State) return Hyprland_Workspace_Id is
     (State.Active_Workspace);

   function Fullscreen (State : Hyprland_State) return Boolean is
     (State.Fullscreen);

   ----------------
   --  Is_Valid  --
   ----------------
   function Is_Valid (State : Hyprland_State) return Boolean is (State.Valid);

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
      --  We try to dynamically grab info from Hyprland’s Hypr2 socket where
      --  possible, but when first starting there is no choice but to
      --  use Hypr1.
      Read_Workspaces :
      declare
         use GNATCOLL.JSON;

         W      : constant String     :=
           Protocol.Send_Message
             (Hypr => State.Connection.all, Id => Workspaces);
         W_JSON : constant JSON_Value := Read (W);
      begin
         for Object of JSON_Array'(W_JSON.Get) loop
            declare
               use Hyprland.State_Impl;

               Workspace : Hyprland_Workspace;
               Id        : constant Hyprland_Workspace_Id :=
                 Value (Integer'(Object.Get ("id"))'Image);
            begin
               Workspace.Id   := Id;
               Workspace.Name := Object.Get ("name");

               State.Workspaces.Insert (Id, Workspace);
            end;
         end loop;
      end Read_Workspaces;

      Read_Active_Workspace :
      declare
         use GNATCOLL.JSON;
         use Hyprland.State_Impl;

         W      : constant String     :=
           Protocol.Send_Message
             (Hypr => State.Connection.all, Id => Activeworkspace);
         W_JSON : constant JSON_Value := Read (W);
      begin
         State.Active_Workspace := Value (Integer'(W_JSON.Get ("id"))'Image);
      end Read_Active_Workspace;

      Read_Windows :
      declare
         use GNATCOLL.JSON;
         use Hyprland.State_Impl;

         W      : constant String     :=
           Protocol.Send_Message (Hypr => State.Connection.all, Id => Clients);
         W_JSON : constant JSON_Value := Read (W);
      begin
         for Object of JSON_Array'(W_JSON.Get) loop
            declare
               Window      : Hyprland_Window;
               Raw_Address : constant String             :=
                 Object.Get
                   ("address"); --  In the format 0x1234567 instead of 1234567
               Id          : constant Hyprland_Window_Id :=
                 Value
                   ("16#" &
                    Raw_Address (Raw_Address'First + 2 .. Raw_Address'Last) &
                    "#");
            begin
               Window.Id        := Id;
               Window.Workspace :=
                 Value (Integer'(Object.Get ("workspace").Get ("id"))'Image);
               Window.Class     := Object.Get ("class");
               Window.Title     := Object.Get ("title");

               State.Windows.Insert (Id, Window);
               State.Workspaces (Window.Workspace).Windows.Append (Id);
            end;
         end loop;
      end Read_Windows;

      Read_Active_Window :
      declare
         use GNATCOLL.JSON;
         use Hyprland.State_Impl;

         W      : constant String     :=
           Protocol.Send_Message
             (Hypr => State.Connection.all, Id => Activewindow);
         W_JSON : constant JSON_Value := Read (W);
      begin
         --  If no window is active at startup
         if W_JSON.Is_Empty then
            State.Active_Window := No_Window;
         else
            --  Otherwise
            declare
               Raw_Address : constant String :=
                 W_JSON.Get ("address"); --  See above for format issues
            begin
               State.Active_Window :=
                 Value
                   ("16#" &
                    Raw_Address (Raw_Address'First + 2 .. Raw_Address'Last) &
                    "#");
            end;
         end if;
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

   ----------------
   -- Connection --
   ----------------
   function Connection
     (State : in out Hyprland_State)
      return access Hyprland.Protocol.Hyprland_Connection is
     (State.Connection);

   ------------
   -- Update --
   ------------
   function Update (State : in out Hyprland_State) return Boolean is
      use Ada.Strings.Unbounded;
      use Hyprland.State_Impl;

      Msg : Protocol.Hypr2_Message;

      Active_Class : Unbounded_String;
      Active_Title : Unbounded_String;

      Updated : Boolean := False;
   begin
      while Protocol.Has_Messages (State.Connection.all) loop
         Msg := Protocol.Receive_Message (State.Connection.all);

         Put_Debug (Msg.Msg_Id'Image & ": " & (+Msg.Msg_Body));

         Updated := True;
         case Msg.Msg_Id is
            when Workspacev2 =>
               --  Workspace_Id, Workspace_Name
               declare
                  Args         : constant Unbounded_String_Array :=
                    Parse_Args (Msg.Msg_Body, 2);
                  Workspace_Id : constant Hyprland_Workspace_Id  :=
                    Value (+(Args (1)));
               begin
                  State.Active_Workspace := Workspace_Id;
               end;
            when Activewindow =>
               --  Class, Title
               --  Note: But not associated with a window id and
               --  activewindowv2 comes after, so our own record-keeping
               --  would assign these to the wrong window
               declare
                  Args : constant Unbounded_String_Array :=
                    Parse_Args (Msg.Msg_Body, 2);
               begin
                  Active_Class := Args (1);
                  Active_Title := Args (2);
               end;
            when Activewindowv2 =>
               --  Window_Address
               declare
                  Window_Id : Hyprland_Window_Id;
               begin
                  begin
                     Window_Id := Value ("16#" & (+Msg.Msg_Body) & "#");
                  exception
                     when Constraint_Error =>
                        Window_Id := Hyprland.State.No_Window;
                  end;

                  State.Active_Window := Window_Id;

                  --  Update the class and title if the window was marked stale
                  --  Skip this if the Openwindow event has not yet been fired.
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
               State.Fullscreen :=
                 (if (+Msg.Msg_Body) = "1" then True else False);
            when Createworkspacev2 =>
               --  Workspace_Id, Workspace_Name
               declare
                  Args           : constant Unbounded_String_Array :=
                    Parse_Args (Msg.Msg_Body, 2);
                  Workspace_Id   : constant Hyprland_Workspace_Id  :=
                    Value (+Args (1));
                  Workspace_Name : constant Unbounded_String       := Args (2);

                  Workspace : constant Hyprland_Workspace :=
                    (Workspace_Id, Workspace_Name, others => <>);
               begin
                  State.Workspaces.Insert (Workspace_Id, Workspace);
               end;
            when Destroyworkspacev2 =>
               --  Workspace_Id, Workspace_Name
               declare
                  Args         : constant Unbounded_String_Array :=
                    Parse_Args (Msg.Msg_Body, 2);
                  Workspace_Id : constant Hyprland_Workspace_Id  :=
                    Value (+(Args (1)));
               begin
                  State.Workspaces.Delete (Workspace_Id);
               end;
            when Renameworkspace =>
               --  Workspace_Id, New_Name
               declare
                  Args         : constant Unbounded_String_Array :=
                    Parse_Args (Msg.Msg_Body, 2);
                  Workspace_Id : constant Hyprland_Workspace_Id  :=
                    Value (+(Args (1)));
                  New_Name     : constant Unbounded_String       := Args (2);
               begin
                  State.Workspaces (Workspace_Id).Name := New_Name;
               end;
            when Activespecial =>
               --  Workspace_Name, Monitor_Name
               null;
               --  Note: We intentionally don’t do anything with this
            when Openwindow =>
               --  Window_Address, Workspace_Name, Window_Class, Window_Title
               declare
                  Args : constant Unbounded_String_Array :=
                    Parse_Args (Msg.Msg_Body, 4);

                  Window_Id      : constant Hyprland_Window_Id :=
                    Value ("16#" & (+(Args (1))) & "#");
                  Workspace_Name : constant Unbounded_String   := Args (2);
                  Window_Class   : constant Unbounded_String   := Args (3);
                  Window_Title   : constant Unbounded_String   := Args (4);

                  Window : Hyprland_Window;
               begin
                  Window.Id := Window_Id;

                  --  Workspace_Id is not passed by Hypr2, so find
                  --  the workspace by name
                  for W of State.Workspaces loop
                     if W.Name = Workspace_Name then
                        Window.Workspace := W.Id;
                        goto Found_Workspace;
                     end if;
                  end loop;
                  raise Program_Error
                    with "Could not find workspace with ID " &
                    (+Workspace_Name);
                  <<Found_Workspace>>

                  Window.Class := Window_Class;
                  Window.Title := Window_Title;

                  State.Windows.Insert (Window_Id, Window);
                  State.Workspaces (Window.Workspace).Windows.Append
                    (Window_Id);
               end;
            when Closewindow =>
               --  Window_Address
               declare
                  Window_Id : constant Hyprland_Window_Id :=
                    Value ("16#" & (+Msg.Msg_Body) & "#");

                  Relevant_Workspace :
                    Hyprland_Workspace renames
                    State.Workspaces (State.Windows (Window_Id).Workspace);
               begin
                  Relevant_Workspace.Windows.Delete
                    (Relevant_Workspace.Windows.Find_Index (Window_Id));
                  State.Windows.Delete (Window_Id);

                  State.Active_Window := No_Window;
               end;
            when Movewindowv2 =>
               --  Window_Address, Workspace_Id, Workspace_Name
               declare
                  Args         : constant Unbounded_String_Array :=
                    Parse_Args (Msg.Msg_Body, 3);
                  Window_Id    : constant Hyprland_Window_Id     :=
                    Value ("16#" & (+(Args (1))) & "#");
                  Workspace_Id : constant Hyprland_Workspace_Id  :=
                    Value (+(Args (2)));

                  Relevant_Workspace :
                    Hyprland_Workspace renames
                    State.Workspaces (State.Windows (Window_Id).Workspace);
               begin

                  Relevant_Workspace.Windows.Delete
                    (Relevant_Workspace.Windows.Find_Index (Window_Id));

                  State.Windows (Window_Id).Workspace := Workspace_Id;
                  State.Workspaces (Workspace_Id).Windows.Append (Window_Id);
               end;
            when Windowtitle =>
               --  Window_Address
               declare
                  Window_Id : constant Hyprland_Window_Id :=
                    Value ("16#" & (+Msg.Msg_Body) & "#");
               begin
                  --  This is also sent when a window has just been created,
                  --  in which case there is no need to parse it
                  if State.Windows.Contains (Window_Id) then
                     --  The title will be refreshed when it is once again
                     --  the active window
                     State.Windows (Window_Id).Title_Stale := True;
                  end if;
               end;
            when others =>
               --  A command that we don’t need to handle for whatever reason
               Updated := False;
         end case;
      end loop;

      return Updated;
   end Update;

   ------------------------
   -- Activate_Workspace --
   ------------------------
   procedure Activate_Workspace
     (State : in out Hyprland_State; Workspace : Hyprland_Workspace_Id)
   is
      Result : constant String :=
        Hyprland.Protocol.Send_Message
          (Hypr      => State.Connection.all, Id => Dispatch,
           Arguments =>
             "workspace " &
             Strip_Space (Hyprland.State_Impl.Image (Workspace)));
   begin
      if Result /= "ok" then
         raise Request_Failed with Result;
      end if;
   end Activate_Workspace;

   -----------------
   -- Move_Window --
   -----------------

   procedure Move_Window
     (State       : in out Hyprland_State; Window : Hyprland_Window_Id;
      Destination :        Hyprland_Workspace_Id)
   is
      Result : constant String :=
        Hyprland.Protocol.Send_Message
          (Hypr      => State.Connection.all, Id => Dispatch,
           Arguments =>
             "movetoworkspace " &
             Strip_Space (Hyprland.State_Impl.Image (Destination)) & "," &
             Hyprland.State_Impl.To_Selector (Window));
   begin
      if Result /= "ok" then
         raise Request_Failed with Result;
      end if;
   end Move_Window;

end Hyprland.State;
