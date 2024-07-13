pragma Ada_2022;

with Ada.Exceptions;

with GNATCOLL.JSON;
with Hyprland.State;

with String_Utils;
with Debug; use Debug;

package body Hyprland.State is
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

   Id_Search_Failure : exception;

   function Find_Monitor_Id
     (State : Hyprland_State; Name : Ada.Strings.Unbounded.Unbounded_String)
      return Hyprland_Monitor_Id;
   --  Find a monitor by name
   --  Raises `Id_Search_Failure`

   function Find_Monitor_Id
     (State : Hyprland_State; Name : Ada.Strings.Unbounded.Unbounded_String)
      return Hyprland_Monitor_Id
   is
      use Ada.Strings.Unbounded;
   begin
      for M of State.Monitors loop
         if M.Name = Name then
            return M.Id;
         end if;
      end loop;

      raise Id_Search_Failure
        with "Could not find monitor with name " & (+Name);
   end Find_Monitor_Id;

   function Find_Workspace_Id
     (State : Hyprland_State; Name : Ada.Strings.Unbounded.Unbounded_String)
      return Hyprland_Workspace_Id;
   --  Find a workspace by name
   --  Raises `Id_Search_Failure`

   function Find_Workspace_Id
     (State : Hyprland_State; Name : Ada.Strings.Unbounded.Unbounded_String)
      return Hyprland_Workspace_Id
   is
      use Ada.Strings.Unbounded;
   begin
      for WS of State.Workspaces loop
         if WS.Name = Name then
            return WS.Id;
         end if;
      end loop;

      raise Id_Search_Failure
        with "Could not find workspace with name " & (+Name);
   end Find_Workspace_Id;

   procedure Remove_Workspace_From_Monitor
     (State : in out Hyprland_State; Workspace : Hyprland_Workspace_Id);

   procedure Remove_Workspace_From_Monitor
     (State : in out Hyprland_State; Workspace : Hyprland_Workspace_Id)
   is
   begin
      pragma Assert (Workspace /= No_Workspace);
      pragma Assert (State.Workspaces (Workspace).Monitor /= No_Monitor);

      State.Monitors (State.Workspaces (Workspace).Monitor).Workspaces.Delete
        (State.Monitors (State.Workspaces (Workspace).Monitor).Workspaces
           .Find_Index
           (Workspace));

      --  Orphan the monitor
      State.Workspaces (Workspace).Monitor := No_Monitor;
   end Remove_Workspace_From_Monitor;

   procedure Add_Workspace_To_Monitor
     (State   : in out Hyprland_State; Workspace : Hyprland_Workspace_Id;
      Monitor :        Hyprland_Monitor_Id);

   procedure Add_Workspace_To_Monitor
     (State   : in out Hyprland_State; Workspace : Hyprland_Workspace_Id;
      Monitor :        Hyprland_Monitor_Id)
   is
   begin
      pragma Assert (Workspace /= No_Workspace);
      pragma Assert (Monitor /= No_Monitor);
      pragma Assert (State.Workspaces (Workspace).Monitor = No_Monitor);

      State.Workspaces (Workspace).Monitor := Monitor;
      State.Monitors (Monitor).Workspaces.Append (Workspace);
   end Add_Workspace_To_Monitor;

   procedure Remove_Window_From_Workspace
     (State : in out Hyprland_State; Window : Hyprland_Window_Id);

   procedure Remove_Window_From_Workspace
     (State : in out Hyprland_State; Window : Hyprland_Window_Id)
   is
   begin
      pragma Assert (Window /= No_Window);
      pragma Assert (State.Windows (Window).Workspace /= No_Workspace);

      State.Workspaces (State.Windows (Window).Workspace).Windows.Delete
        (State.Workspaces (State.Windows (Window).Workspace).Windows.Find_Index
           (Window));

      --  Orphan the window
      State.Windows (Window).Workspace := No_Workspace;
   end Remove_Window_From_Workspace;

   procedure Add_Window_To_Workspace
     (State     : in out Hyprland_State; Window : Hyprland_Window_Id;
      Workspace :        Hyprland_Workspace_Id);

   procedure Add_Window_To_Workspace
     (State     : in out Hyprland_State; Window : Hyprland_Window_Id;
      Workspace :        Hyprland_Workspace_Id)
   is
   begin
      pragma Assert (Window /= No_Window);
      pragma Assert (Workspace /= No_Workspace);
      pragma Assert (State.Windows (Window).Workspace = No_Workspace);

      State.Windows (Window).Workspace := Workspace;
      State.Workspaces (Workspace).Windows.Append (Window);
   end Add_Window_To_Workspace;

   function Hypr1_Active_Monitor
     (State : in out Hyprland_State) return Hyprland_Monitor_Id;
   function Hypr1_Active_Monitor
     (State : in out Hyprland_State) return Hyprland_Monitor_Id
   is
      use GNATCOLL.JSON;
      use Hyprland.State_Impl;

      AW      : constant String     := State.Send_Message (Activeworkspace);
      AW_JSON : constant JSON_Value := Read (AW);
   begin
      return Value (Integer'(AW_JSON.Get ("monitorID"))'Image);
   end Hypr1_Active_Monitor;

   function Hypr1_Active_Workspace
     (State : in out Hyprland_State) return Hyprland_Workspace_Id;
   function Hypr1_Active_Workspace
     (State : in out Hyprland_State) return Hyprland_Workspace_Id
   is
      use GNATCOLL.JSON;
      use Hyprland.State_Impl;

      AW      : constant String     := State.Send_Message (Activeworkspace);
      AW_JSON : constant JSON_Value := Read (AW);
   begin
      return Value (Integer'(AW_JSON.Get ("id"))'Image);
   end Hypr1_Active_Workspace;

   function Hypr1_Find_Monitor_Name
     (State : in out Hyprland_State; Monitor : Hyprland_Monitor_Id)
      return Ada.Strings.Unbounded.Unbounded_String;

   function Hypr1_Find_Monitor_Name
     (State : in out Hyprland_State; Monitor : Hyprland_Monitor_Id)
      return Ada.Strings.Unbounded.Unbounded_String
   is
      use GNATCOLL.JSON;
      use Hyprland.State_Impl;

      M      : constant String     := State.Send_Message (Monitors);
      M_JSON : constant JSON_Value := Read (M);
   begin
      pragma Assert (Monitor /= No_Monitor);

      for Object of JSON_Array'(M_JSON.Get) loop
         if Value (Integer'(Object.Get ("id"))'Image) = Monitor then
            return Object.Get ("name");
         end if;
      end loop;
      raise Id_Search_Failure with "Couldn’t find monitor name.";
   end Hypr1_Find_Monitor_Name;

   ---------------
   -- Accessors --
   ---------------
   function Monitors (State : Hyprland_State) return Hyprland_Monitor_List is
     (State.Monitors);

   function Workspaces
     (State : Hyprland_State) return Hyprland_Workspace_List is
     (State.Workspaces);

   function Windows (State : Hyprland_State) return Hyprland_Window_List is
     (State.Windows);

   function Active_Monitor
     (State : Hyprland_State) return Hyprland_Monitor_Id is
     (State.Active_Monitor);

   function Active_Workspace
     (State : Hyprland_State) return Hyprland_Workspace_Id is
     (State.Active_Workspace);

   function Active_Window (State : Hyprland_State) return Hyprland_Window_Id is
     (State.Active_Window);

   function Keyboard_Layout
     (State : Hyprland_State) return Ada.Strings.Unbounded.Unbounded_String is
     (State.Keyboard_Layout);

   function Keyboard_Variant
     (State : Hyprland_State) return Ada.Strings.Unbounded.Unbounded_String is
     (State.Keyboard_Variant);

   -------------
   -- Connect --
   -------------

   procedure Connect (State : in out Hyprland_State) is
   begin
      if State.Valid then
         raise Invalid_State with "Hyprland State already Connected.";
      end if;

      Protocol.Hyprland_Connection (State).Connect;

      ---------------------
      --  Initial State  --
      ---------------------
      --  We try to dynamically grab info from Hyprland’s Hypr2 socket where
      --  possible, but when first starting there is no choice but to
      --  use Hypr1.

      Read_Monitors :
      declare
         use GNATCOLL.JSON;

         M      : constant String     := State.Send_Message (Monitors);
         M_JSON : constant JSON_Value := Read (M);
      begin
         for Object of JSON_Array'(M_JSON.Get) loop
            declare
               use Hyprland.State_Impl;

               Monitor : Hyprland_Monitor;
            begin
               Monitor.Id   := Value (Integer'(Object.Get ("id"))'Image);
               Monitor.Name := Object.Get ("name");

               State.Monitors.Insert (Monitor.Id, Monitor);
            end;
         end loop;
      end Read_Monitors;

      Read_Workspaces :
      declare
         use GNATCOLL.JSON;

         W      : constant String     := State.Send_Message (Workspaces);
         W_JSON : constant JSON_Value := Read (W);

      begin
         for Object of JSON_Array'(W_JSON.Get) loop
            declare
               use Hyprland.State_Impl;

               Workspace_Id : constant Hyprland_Workspace_Id :=
                 Value (Integer'(Object.Get ("id"))'Image);
               Monitor_Id   : constant Hyprland_Monitor_Id   :=
                 Value (Integer'(Object.Get ("monitorID"))'Image);

               Workspace : constant Hyprland_Workspace :=
                 (Id   => Workspace_Id, Monitor => No_Monitor,
                  Name => Object.Get ("name"), Windows => []);
            begin
               State.Workspaces.Insert (Workspace_Id, Workspace);
               Add_Workspace_To_Monitor (State, Workspace_Id, Monitor_Id);
            end;
         end loop;
      end Read_Workspaces;

      Read_Active_Workspace :
      declare

         use GNATCOLL.JSON;
         use Hyprland.State_Impl;

         W      : constant String     := State.Send_Message (Activeworkspace);
         W_JSON : constant JSON_Value := Read (W);

      begin
         State.Active_Workspace := Value (Integer'(W_JSON.Get ("id"))'Image);

         --  Note: there is no separate 'activemonitor' call, so we use this
         --  instead.
         State.Active_Monitor :=
           State.Workspaces (State.Active_Workspace).Monitor;
      end Read_Active_Workspace;

      Read_Windows :
      declare

         use GNATCOLL.JSON;
         use Hyprland.State_Impl;

         W      : constant String     := State.Send_Message (Clients);
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
                 Value (Integer'Image (Object.Get ("workspace").Get ("id")));
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

         W      : constant String     := State.Send_Message (Activewindow);
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
      if not State.Valid then
         raise Invalid_State;
      end if;

      Protocol.Hyprland_Connection (State).Disconnect;

      --  Intentionally wipe state
      --  TODO fix
      State.Valid := False;
   end Disconnect;

   function File_Descriptor (State : Hyprland_State) return Integer is
     (Protocol.Hyprland_Connection (State).File_Descriptor);

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
      if not State.Valid then
         raise Invalid_State;
      end if;

      while State.Has_Messages loop
         Msg := State.Receive_Message;

         Put_Debug (Msg.Msg_Id'Image & ": " & (+Msg.Msg_Body));

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
                  Updated                := True;
               end;

            when Focusedmon =>
               --  Monitor_Name, Workspace_Name
               --  Note: Hyprland currently does not return info
               --  about selected special workspaces. It is not
               --  practical to retrieve this without a worst-case
               --  two Hypr1 round trips, so this is currentlyç
               --  unimplemented.
               --
               --  TODO
               declare
                  Args           : constant Unbounded_String_Array :=
                    Parse_Args (Msg.Msg_Body, 2);
                  Monitor_Name   : constant Unbounded_String       := Args (1);
                  Workspace_Name : constant Unbounded_String       := Args (2);

                  Monitor_Id   : constant Hyprland_Monitor_Id   :=
                    Find_Monitor_Id (State, Monitor_Name);
                  Workspace_Id : constant Hyprland_Workspace_Id :=
                    Find_Workspace_Id (State, Workspace_Name);
               begin
                  --  Repair orphaned workspaces after `Monitorremoved`
                  if State.Workspaces (Workspace_Id).Monitor = No_Monitor then
                     Add_Workspace_To_Monitor
                       (State, Workspace_Id, Monitor_Id);
                  end if;

                  State.Active_Monitor   := Monitor_Id;
                  State.Active_Workspace := Workspace_Id;

                  Updated := True;
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
                        --  This occurs when no window is active
                        Window_Id := Hyprland.State.No_Window;
                  end;

                  --  Update the class and title if the window was marked stale
                  --  Skip this if the Openwindow event has not yet been fired.

                  if State.Windows.Contains (Window_Id) then
                     --  Update the workspace
                     State.Active_Workspace :=
                       State.Windows (Window_Id).Workspace;

                     --  Update stale titles
                     if State.Windows (Window_Id).Title_Stale then
                        State.Windows (Window_Id).Class := Active_Class;
                        State.Windows (Window_Id).Title := Active_Title;

                        State.Windows (Window_Id).Title_Stale := False;
                     end if;
                  end if;

                  State.Active_Window := Window_Id;
                  Updated             := True;
               end;

            when Monitorremoved =>
               --  Monitor_Name
               declare
                  Args         : constant Unbounded_String_Array :=
                    Parse_Args (Msg.Msg_Body, 1);
                  Monitor_Name : constant Unbounded_String       := Args (1);
                  Monitor_Id   : constant Hyprland_Monitor_Id    :=
                    Find_Monitor_Id (State, Monitor_Name);
               begin
                  --  Clear data for workspaces which were on that monitor
                  --  These will be updated after a `Focusedmon`
                  for WS of State.Workspaces loop
                     if WS.Monitor = Monitor_Id then
                        WS.Monitor := No_Monitor;
                     end if;
                  end loop;

                  State.Monitors.Delete (Monitor_Id);
                  Updated := True;
               end;

            when Monitoraddedv2 =>
               --  Monitor_Id, Monitor_Name, Monitor_Description
               --  TODO https://github.com/hyprwm/Hyprland/issues/6848
               --  (Monitor_Name is currently incorrect, so we need to
               --  fetch it via Hypr1)
               declare
                  Args         : constant Unbounded_String_Array :=
                    Parse_Args (Msg.Msg_Body, 3);
                  Monitor_Id   : constant Hyprland_Monitor_Id    :=
                    Value (+Args (1));
                  Monitor_Name : constant Unbounded_String       :=
                    Hypr1_Find_Monitor_Name (State, Monitor_Id);
                  --  Monitor_Description isn’t currently used
               begin
                  State.Monitors.Insert
                    (Monitor_Id,
                     Hyprland_Monitor'
                       (Id         => Monitor_Id, Name => Monitor_Name,
                        Workspaces => []));
                  Updated := True;
               end;

            when Createworkspacev2 =>
               --  Workspace_Id, Workspace_Name
               --  Note Hyprland doesn’t currently provide
               --  the monitor associated with the workspace,
               --  so it must be fetched synchronously
               declare

                  Args           : constant Unbounded_String_Array :=
                    Parse_Args (Msg.Msg_Body, 2);
                  Workspace_Id   : constant Hyprland_Workspace_Id  :=
                    Value (+Args (1));
                  Workspace_Name : constant Unbounded_String       := Args (2);

                  Workspace : constant Hyprland_Workspace :=
                    (Id   => Workspace_Id, Monitor => No_Monitor,
                     Name => Workspace_Name, Windows => []);

               begin
                  State.Workspaces.Insert (Workspace_Id, Workspace);
                  Add_Workspace_To_Monitor
                    (State, Workspace_Id, Hypr1_Active_Monitor (State));
                  Updated := True;
               end;

            when Destroyworkspacev2 =>
               --  Workspace_Id, Workspace_Name
               declare

                  Args         : constant Unbounded_String_Array :=
                    Parse_Args (Msg.Msg_Body, 2);
                  Workspace_Id : constant Hyprland_Workspace_Id  :=
                    Value (+(Args (1)));

               begin
                  Remove_Workspace_From_Monitor (State, Workspace_Id);

                  --  Disassociate all windows from this workspace
                  for W of State.Windows loop
                     if W.Workspace = Workspace_Id then
                        W.Workspace := No_Workspace;
                     end if;
                  end loop;

                  --  Note: After resuming from hibernation or DPMS off
                  --  it’s common for the current workspace (a temp
                  --  workspace) to be removed.
                  --
                  --  The real active workspace isn’t provided, however,
                  --  so we have to fetch it synchronously.

                  if State.Active_Workspace = Workspace_Id then
                     State.Active_Workspace := Hypr1_Active_Workspace (State);
                  end if;

                  --  Delete the actual workspace object
                  State.Workspaces.Delete (Workspace_Id);
                  Updated := True;
               end;

            when Moveworkspacev2 =>
               --  Workspace_Id, Workspace_Name, Monitor_Name
               declare
                  Args : constant Unbounded_String_Array :=
                    Parse_Args (Msg.Msg_Body, 3);

                  Workspace_Id : constant Hyprland_Workspace_Id :=
                    Value (+(Args (1)));
                  --  Workspace_Name not needed
                  Monitor_Name : constant Unbounded_String      := Args (3);
                  Monitor_Id   : constant Hyprland_Monitor_Id   :=
                    Find_Monitor_Id (State, Monitor_Name);
               begin
                  Remove_Workspace_From_Monitor (State, Workspace_Id);
                  Add_Workspace_To_Monitor (State, Workspace_Id, Monitor_Id);
                  Updated := True;
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
                  Updated                              := True;
               end;

            when Activespecial =>
               --  Workspace_Name, Monitor_Name
               declare
                  Args : constant Unbounded_String_Array :=
                    Parse_Args (Msg.Msg_Body, 2);

                  Workspace_Name : constant Unbounded_String := Args (1);
                  Monitor_Name   : constant Unbounded_String := Args (2);

                  Monitor_Id : constant Hyprland_Monitor_Id :=
                    Find_Monitor_Id (State, Monitor_Name);
               begin
                  --  If enabling the special workspace
                  if Workspace_Name /= Null_Unbounded_String then
                     declare
                        Workspace_Id : constant Hyprland_Workspace_Id :=
                          Find_Workspace_Id (State, Workspace_Name);
                     begin
                        --  If the special workspace is attached to
                        --  an old monitor, remove it.

                        if State.Workspaces (Workspace_Id).Monitor /=
                          No_Monitor
                        then
                           Remove_Workspace_From_Monitor (State, Workspace_Id);
                        end if;

                        Add_Workspace_To_Monitor
                          (State, Workspace_Id, Monitor_Id);

                        State.Active_Workspace := Workspace_Id;
                     end;
                  else --  If returning to a normal workspace
                     --  Note: This code path is inefficient
                     --  Hyprland doesn’t tell us what workspace is being
                     --  activated when the special workspace is closed.
                     --  Thus we must fetch that via Hypr1 (synchronous)
                     State.Active_Workspace := Hypr1_Active_Workspace (State);
                  end if;
                  Updated := True;
               end;

            when Activelayout =>
               --  Keyboard_Name, Layout_Name
               Updated := True;
               --  Note This is bugged in Hyprland right now, so
               --  we can’t actually use any of these messages.
               --  It’s still cause for a status update however.

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

                  Workspace_Id : constant Hyprland_Workspace_Id :=
                    Find_Workspace_Id (State, Workspace_Name);

                  Window : constant Hyprland_Window :=
                    (Id          => Window_Id, Workspace => No_Workspace,
                     Class       => Window_Class, Title => Window_Title,
                     Title_Stale => False);

               begin
                  State.Windows.Insert (Window_Id, Window);
                  Add_Window_To_Workspace (State, Window_Id, Workspace_Id);
                  Updated := True;
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
                  Updated             := True;
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
               begin
                  Remove_Window_From_Workspace (State, Window_Id);
                  Add_Window_To_Workspace (State, Window_Id, Workspace_Id);
                  Updated := True;
               end;

            when Windowtitle =>
               --  Window_Address
               declare

                  Window_Id : constant Hyprland_Window_Id :=
                    Value ("16#" & (+Msg.Msg_Body) & "#");

               begin
                  --  This event provides no actual data, just metadata
                  --  about an upcoming `Activewindow` event

                  --  If the window already exists, set a flag so that the next
                  --  `Activewindow` and `Activewindowv2` call chain will cause
                  --  the title to be updated.
                  if State.Windows.Contains (Window_Id) then
                     State.Windows (Window_Id).Title_Stale := True;
                  end if;
               end;

            when others =>
               null;
         end case;
      end loop;

      return Updated;
   exception
      when X : others =>
         Put_Debug (State'Image);
         Ada.Exceptions.Reraise_Occurrence (X);
   end Update;

   ------------------------
   -- Activate_Workspace --
   ------------------------

   procedure Activate_Workspace
     (State : in out Hyprland_State; Workspace : Hyprland_Workspace_Id)
   is

      Result : constant String :=
        State.Send_Message
          (Id        => Dispatch,
           Arguments =>
             "workspace " &
             String_Utils.Strip_Spaces
               (Hyprland.State_Impl.Image (Workspace)));

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
        State.Send_Message
          (Id        => Dispatch,
           Arguments =>
             "movetoworkspace " &
             String_Utils.Strip_Spaces
               (Hyprland.State_Impl.Image (Destination)) &
             "," & Hyprland.State_Impl.To_Selector (Window));

   begin
      if Result /= "ok" then
         raise Request_Failed with Result;
      end if;
   end Move_Window;

   ----------------
   -- Set_Layout --
   ----------------
   procedure Set_Layout
     (State   : in out Hyprland_State; Keyboard : String; Layout : String;
      Variant :        String)
   is
      Result : constant String :=
        State.Send_Unchecked
          ("[[BATCH]]" & "j/keyword input:kb_variant  , ;" &
           "j/keyword input:kb_layout us," & Layout & ";" &
           "j/keyword input:kb_variant  ," & Variant & ";" &
           "j/switchxkblayout " & Keyboard & " 1");
   begin
      --  The result should be four 'ok's separated by three LFs each
      --!pp off
      if Result /=
         "ok" & ASCII.LF & ASCII.LF & ASCII.LF
       & "ok" & ASCII.LF & ASCII.LF & ASCII.LF
       & "ok" & ASCII.LF & ASCII.LF & ASCII.LF
       & "ok"
      then
         Put_Debug (Result);
         raise Request_Failed with Result;
      end if;
      --!pp on

      State.Keyboard_Layout  :=
        Ada.Strings.Unbounded.To_Unbounded_String (Layout);
      State.Keyboard_Variant :=
        Ada.Strings.Unbounded.To_Unbounded_String (Variant);
   end Set_Layout;
end Hyprland.State;
