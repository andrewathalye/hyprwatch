pragma Ada_2022;

with Ada.Strings.Unbounded;
with Ada.Unchecked_Conversion;
with System;

with D_Bus.Messages;
with D_Bus.Connection.G_Main;
with D_Bus.Arguments.Containers;
with D_Bus.Arguments.Basic;
with D_Bus.Types;
use type D_Bus.Types.Obj_Path;

with Hypr_Helpers;
with Hyprland.State_Impl;
with Debug; use Debug;

package body D_Bus_Helpers is
   ---------------
   -- Constants --
   ---------------

   Signature_Error : constant String :=
     "org.freedesktop.DBus.Error.InvalidSignature";
   Arguments_Error : constant String :=
     "org.freedesktop.DBus.Error.InvalidArgs";

   ------------------------
   -- Introspection Data --
   ------------------------
   function Read_Introspect_String return String;

   function Read_Introspect_String return String is

      type Symbol is null record with
        Convention => C;
      --  Note: DO NOT ATTEMPT TO ACCESS

      function To_Natural (S : aliased Symbol) return Natural;
      --  Architecture-indepedent function to transform a Symbol into
      --  a Natural. This is equivalent to reading the address of an
      --  ELF symbol and interpreting it as a number.

      function To_Natural (S : aliased Symbol) return Natural is

         type Address_Mod is mod System.Memory_Size;

         function Convert is new Ada.Unchecked_Conversion
           (System.Address, Address_Mod);

      begin
         return Natural (Convert (S'Address));
      end To_Natural;

      Introspect_XML_Size : aliased Symbol with
        Import        => True, Convention => C,
        External_Name => "_binary_introspect_xml_size";

      Introspect_XML_Start :
        aliased String (1 .. To_Natural (Introspect_XML_Size)) with
        Import        => True, Convention => C,
        External_Name => "_binary_introspect_xml_start";

   begin
      return Introspect_XML_Start;
   end Read_Introspect_String;

   Introspect_String : constant String := Read_Introspect_String;
   --  Note: The reason this is implemented via a call to
   --  `Read_Introspect_String` is that directly importing
   --  a variable with dynamic bounds appeared to cause memory issues.

   --------------
   -- Internal --
   --------------
   function To_Struct
     (W2D : Hypr_Helpers.Workspace_2D)
      return D_Bus.Arguments.Containers.Struct_Type;

   function To_Struct
     (W2D : Hypr_Helpers.Workspace_2D)
      return D_Bus.Arguments.Containers.Struct_Type
   is

      use type D_Bus.Arguments.Basic.Byte_Type;

      Struct : D_Bus.Arguments.Containers.Struct_Type;

   begin
      Struct.Append (+D_Bus.Byte (W2D.X));
      Struct.Append (+D_Bus.Byte (W2D.Y));

      return Struct;
   end To_Struct;

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

   function Get_Signature
     (Message : D_Bus.Messages.Message_Type) return String;

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
   Introspect_In : constant String := "";
   procedure Introspect
     (Request :     D_Bus.Messages.Message_Type;
      Reply   : out D_Bus.Messages.Message_Type);

   Get_Workspaces_In : constant String := "";
   procedure Get_Workspaces
     (Request :     D_Bus.Messages.Message_Type;
      Reply   : out D_Bus.Messages.Message_Type);

   Activate_Workspace_In : constant String := "(qq)";
   procedure Activate_Workspace
    (Request : D_Bus.Messages.Message_Type;
     Reply : out D_Bus.Messages.Message_Type);

   Activate_Workspace_Rel_In : constant String := "s";
   procedure Activate_Workspace_Rel
     (Request :     D_Bus.Messages.Message_Type;
      Reply   : out D_Bus.Messages.Message_Type);

   Move_Window_Rel_In : constant String := "us";
   procedure Move_Window_Rel
     (Request :     D_Bus.Messages.Message_Type;
      Reply   : out D_Bus.Messages.Message_Type);

   Update_Keyboard_Layout_In : constant String := "s(ss)s";
   pragma Unreferenced (Update_Keyboard_Layout_In);
   procedure Update_Keyboard_Layout
     (Request : D_Bus.Messages.Message_Type;
      Reply : out D_Bus.Messages.Message_Type) is null;
   --  TODO needs implemented

   ------------------------------------
   --  D_Bus Method Implementations  --
   ------------------------------------
   procedure Introspect
     (Request :     D_Bus.Messages.Message_Type;
      Reply   : out D_Bus.Messages.Message_Type)
   is

      use D_Bus.Arguments.Basic;

      Request_Signature : constant String := Get_Signature (Request);

   begin
      Put_Debug ("Introspect " & Request_Signature);

      --  Check signature
      if Request_Signature /= Introspect_In then
         Reply :=
           D_Bus.Messages.New_Error
             (Reply_To      => Request, Error_Name => Signature_Error,
              Error_Message =>
                ASCII.Quotation & Request_Signature & ASCII.Quotation &
                " != " & ASCII.Quotation & Get_Workspaces_In &
                ASCII.Quotation);
         return;
      end if;

      Reply := D_Bus.Messages.New_Method_Return (Request);
      D_Bus.Messages.Add_Arguments (Reply, +Introspect_String);
   end Introspect;

   procedure Get_Workspaces
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
                ASCII.Quotation & Request_Signature & ASCII.Quotation &
                " != " & ASCII.Quotation & Get_Workspaces_In &
                ASCII.Quotation);
         return;
      end if;

      for W of Global_Service.State.Workspaces loop
         if not Hyprland.State.Is_Special (W.Id) then
            declare

               W2D : constant Hypr_Helpers.Workspace_2D :=
                 Hypr_Helpers.Convert (W.Id);

            begin
               List.Append (To_Struct (W2D));
            end;
         end if;
      end loop;

      Reply := D_Bus.Messages.New_Method_Return (Request);
      D_Bus.Messages.Add_Arguments
        (Msg => Reply, Args => D_Bus.Arguments.Argument_List_Type (List));
   end Get_Workspaces;

   procedure Activate_Workspace
    (Request : D_Bus.Messages.Message_Type;
     Reply : out D_Bus.Messages.Message_Type)
   is
      use Hypr_Helpers;

      Request_Signature : constant String := Get_Signature (Request);

      Workspace_D_Bus : D_Bus.Arguments.Containers.Struct_Type;
      Workspace : Hypr_Helpers.Workspace_2D;
   begin
      Put_Debug ("Activate_Workspace: " & Request_Signature);

      --  Check signature
      if Request_Signature /= Activate_Workspace_In then
         Reply := D_Bus.Messages.New_Error
           (Reply_To => Request,
            Error_Name => Signature_Error,
            Error_Message =>
               ASCII.Quotation & Request_Signature & ASCII.Quotation & " != " &
               ASCII.Quotation & Activate_Workspace_In & ASCII.Quotation);
         return;
      end if;

      Workspace_D_Bus := D_Bus.Arguments.Containers.Struct_Type
        (D_Bus.Messages.Get_Arguments (Request).First_Element);

      begin
         Workspace :=
           (X => Workspace_2D_Axis
              (D_Bus.Arguments.Basic.To_Ada
                 (D_Bus.Arguments.Basic.U_Int16_Type
                    (Workspace_D_Bus.First_Element))),
            Y => Workspace_2D_Axis
              (D_Bus.Arguments.Basic.To_Ada
                 (D_Bus.Arguments.Basic.U_Int16_Type
                  (Workspace_D_Bus.Last_Element))));
      exception
         when Constraint_Error =>
            Reply := D_Bus.Messages.New_Error
              (Reply_To => Request,
               Error_Name => Arguments_Error,
               Error_Message =>
                  "The arguments could not be parsed as a 2D workspace.");
            return;
      end;

      Put_Debug (Image (Workspace));

      Global_Service.State.Activate_Workspace
        (Hypr_Helpers.Convert (Workspace));

      Reply := D_Bus.Messages.New_Method_Return (Request);
   end Activate_Workspace;

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

   procedure Move_Window_Rel
     (Request :     D_Bus.Messages.Message_Type;
      Reply   : out D_Bus.Messages.Message_Type)
   is

      use type Hyprland.State.Hyprland_Window_Id;
      use all type Hypr_Helpers.Hyprland_Direction;

      Request_Signature : constant String := Get_Signature (Request);
      Arguments         : constant D_Bus.Arguments.Argument_List_Type :=
        D_Bus.Messages.Get_Arguments (Request);

      Window    : D_Bus.Arguments.Basic.U_Int32_Type;
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

      Window := D_Bus.Arguments.Basic.U_Int32_Type (Arguments.First_Element);
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
      Obj.Register ("Introspect", Introspect'Access);

      --  tk.zenithseeker.hyprwatch
      Obj.Register ("GetWorkspaces", Get_Workspaces'Access);
      Obj.Register ("ActivateWorkspace", Activate_Workspace'Access);
      Obj.Register ("ActivateWorkspaceRel", Activate_Workspace_Rel'Access);
      Obj.Register ("MoveWindowRel", Move_Window_Rel'Access);
      Obj.Register ("UpdateKeyboardLayout", Update_Keyboard_Layout'Access);
   end Initialize;

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
