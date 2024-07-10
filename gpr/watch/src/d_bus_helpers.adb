pragma Ada_2022;

with Ada.Strings.Unbounded;

with D_Bus.Arguments.Containers;
with D_Bus.Connection.G_Main;
with D_Bus.Arguments.Basic;
with D_Bus.Types;
use type D_Bus.Types.Obj_Path;

with Hypr_Helpers;
with Hyprland.State_Impl;
with Debug; use Debug;

--  Methods
with D_Bus_Helpers.Introspect;
with D_Bus_Helpers.Get_Workspaces;
with D_Bus_Helpers.Activate_Workspace;
with D_Bus_Helpers.Update_Keyboard_Layout;

package body D_Bus_Helpers is
   --------------
   -- Internal --
   --------------

   function To_Direction
     (Direction : D_Bus.Arguments.Basic.String_Type)
      return Hypr_Helpers.Hyprland_Direction;

   function To_Direction
     (Direction : D_Bus.Arguments.Basic.String_Type)
      return Hypr_Helpers.Hyprland_Direction
   is

      use all type Hypr_Helpers.Hyprland_Direction;

      Direction_Str : constant String :=
        D_Bus.Arguments.Basic.To_String (Direction);

   begin
      return
        (if Direction_Str = "l" then Left elsif Direction_Str = "r" then Right
         elsif Direction_Str = "u" then Up elsif Direction_Str = "d" then Down
         else Unknown);
   end To_Direction;

   -------------
   -- Private --
   -------------
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

   ---------------------------------
   --  D_Bus Method Declarations  --
   ---------------------------------
   Activate_Workspace_Rel_In : constant String := "s";
   procedure Activate_Workspace_Rel
     (Request :     D_Bus.Messages.Message_Type;
      Reply   : out D_Bus.Messages.Message_Type);

   Move_Window_In : constant String := "t(qq)";
   procedure Move_Window
     (Request :     D_Bus.Messages.Message_Type;
      Reply   : out D_Bus.Messages.Message_Type);

   Move_Window_Rel_In : constant String := "ts";
   procedure Move_Window_Rel
     (Request :     D_Bus.Messages.Message_Type;
      Reply   : out D_Bus.Messages.Message_Type);

   ------------------------------------
   --  D_Bus Method Implementations  --
   ------------------------------------
   procedure Activate_Workspace_Rel
     (Request :     D_Bus.Messages.Message_Type;
      Reply   : out D_Bus.Messages.Message_Type)
   is

      use all type Hypr_Helpers.Hyprland_Direction;

      Request_Signature : constant String := Get_Signature (Request);
      Arguments         : constant D_Bus.Arguments.Argument_List_Type :=
        D_Bus.Messages.Get_Arguments (Request);

      Direction : D_Bus.Arguments.Basic.String_Type;

      Ada_Direction : Hypr_Helpers.Hyprland_Direction;

   begin
      Put_Debug ("Activate_Workspace_Rel: " & Request_Signature);

      --  Check signature
      if Request_Signature /= Activate_Workspace_Rel_In then
         Reply :=
           D_Bus.Messages.New_Error
             (Reply_To      => Request, Error_Name => Signature_Error,
              Error_Message =>
                ASCII.Quotation & Request_Signature & ASCII.Quotation &
                " != " & ASCII.Quotation & Activate_Workspace_Rel_In &
                ASCII.Quotation);
         return;
      end if;

      Direction := D_Bus.Arguments.Basic.String_Type (Arguments.First_Element);

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

      --  Activate selected workspace
      Global_Service.State.Activate_Workspace
        (Hypr_Helpers.Convert
           (Hypr_Helpers.Translate
              (W2D       =>
                 Hypr_Helpers.Convert (Global_Service.State.Active_Workspace),
               Direction => Ada_Direction)));

      Reply := D_Bus.Messages.New_Method_Return (Request);
   end Activate_Workspace_Rel;

   procedure Move_Window
     (Request :     D_Bus.Messages.Message_Type;
      Reply   : out D_Bus.Messages.Message_Type)
   is
      Request_Signature : constant String := Get_Signature (Request);
   begin
      --  Check signature
      if Request_Signature /= Move_Window_In then
         Reply :=
           D_Bus.Messages.New_Error
             (Reply_To      => Request, Error_Name => Signature_Error,
              Error_Message =>
                ASCII.Quotation & Request_Signature & ASCII.Quotation &
                " != " & ASCII.Quotation & Move_Window_In & ASCII.Quotation);
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
                   (D_Bus.Arguments.Basic.U_Int16_Type
                      (WS_D_Bus.First_Element)));
            Y  :=
              Hypr_Helpers.Workspace_2D_Axis
                (D_Bus.Arguments.Basic.To_Ada
                   (D_Bus.Arguments.Basic.U_Int16_Type
                      (WS_D_Bus.Last_Element)));
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
   end Move_Window;

   procedure Move_Window_Rel
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
         Reply :=
           D_Bus.Messages.New_Error
             (Reply_To      => Request, Error_Name => Signature_Error,
              Error_Message =>
                ASCII.Quotation & Request_Signature & ASCII.Quotation &
                " != " & ASCII.Quotation & Move_Window_Rel_In &
                ASCII.Quotation);
         return;
      end if;

      Window := D_Bus.Arguments.Basic.U_Int64_Type (Arguments.First_Element);
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
                   Hypr_Helpers.Convert
                     (Global_Service.State.Active_Workspace),
                 Direction => Ada_Direction)));

      Reply := D_Bus.Messages.New_Method_Return (Request);
   end Move_Window_Rel;

   -------------------------
   --  Hypr_Service_Type  --
   -------------------------

   overriding procedure Initialize (Obj : in out Hypr_Service_Type) is
   begin
      --  org.freedesktop.DBus.Introspectable
      Obj.Register ("Introspect", D_Bus_Helpers.Introspect'Access);

      --  tk.zenithseeker.hyprwatch
      Obj.Register ("GetWorkspaces", D_Bus_Helpers.Get_Workspaces'Access);
      Obj.Register
        ("ActivateWorkspace", D_Bus_Helpers.Activate_Workspace'Access);
      Obj.Register ("ActivateWorkspaceRel", Activate_Workspace_Rel'Access);
      Obj.Register ("MoveWindow", Move_Window'Access);
      Obj.Register ("MoveWindowRel", Move_Window_Rel'Access);
      Obj.Register
        ("UpdateKeyboardLayout", D_Bus_Helpers.Update_Keyboard_Layout'Access);
   end Initialize;

   function Connection
     (Service : Hypr_Service_Type) return D_Bus.Connection.Connection_Type
   is
   begin
      if not Service.Valid then
         raise Program_Error with "Service not connected!";
      end if;

      return Service.Bus;
   end Connection;

   procedure Connect
     (Service : in out Hypr_Service_Type;
      State   :        Hyprland.State.Hyprland_State_Access)
   is
   begin
      if Service.Valid then
         raise Program_Error with "Service has already been connected";
      end if;

      Service.Bus   := D_Bus.Connection.Connect;
      Service.State := State;

      D_Bus.Connection.Request_Name (Service.Bus, "tk.zenithseeker.hyprwatch");
      D_Bus.Connection.G_Main.Register_Object
        (Connection => Service.Bus, Path => +"/tk/zenithseeker/hyprwatch",
         Object     => Service);

      D_Bus.Connection.G_Main.Setup_With_G_Main (Service.Bus);

      Service.Valid := True;
   end Connect;

end D_Bus_Helpers;
