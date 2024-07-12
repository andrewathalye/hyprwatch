with D_Bus.Messages;
with D_Bus.Arguments.Basic;

with Hypr_Helpers;

package D_Bus_Helpers.Impl is
   --  Private implementation details for D_Bus_Helpers.

   ---------------
   -- Constants --
   ---------------
   Signature_Error : constant String :=
     "org.freedesktop.DBus.Error.InvalidSignature";
   Arguments_Error : constant String :=
     "org.freedesktop.DBus.Error.InvalidArgs";
   Hyprland_Error  : constant String :=
     "tk.zenithseeker.hyprwatch.Error.RequestFailed";

   ----------------------------------
   -- Exported to Private Children --
   ----------------------------------
   function Get_Signature
     (Message : D_Bus.Messages.Message_Type) return String;
   --  Returns the D_Bus wire signature of a message

   function To_Direction
     (Direction : D_Bus.Arguments.Basic.String_Type)
      return Hypr_Helpers.Hyprland_Direction;
   --  Convert a D_Bus `String_Type` with 'l', 'r', 'u', 'd')
   --  To a `Hyprland_Direction` with Left, Right, Up, Down, Unknown

   procedure Raise_Signature_Error
     (Request :     D_Bus.Messages.Message_Type;
      Reply   : out D_Bus.Messages.Message_Type; Actual, Expected : String);
   --  Raises a D_Bus error if the actual and expected
   --  signatures provided to a method do not match.

   -----------------------
   -- Method Signatures --
   -----------------------
   Introspect_In  : constant String := "";
   Introspect_Out : constant String := "";

   Get_Workspaces_In  : constant String := "";
   Get_Workspaces_Out : constant String := "a(yy)";

   Activate_Workspace_In  : constant String := "(yy)";
   Activate_Workspace_Out : constant String := "";

   Activate_Workspace_Rel_In  : constant String := "s";
   Activate_Workspace_Rel_Out : constant String := "";

   Move_Window_In  : constant String := "t(yy)";
   Move_Window_Out : constant String := "";

   Move_Window_Rel_In  : constant String := "ts";
   Move_Window_Rel_Out : constant String := "";

   Update_Keyboard_Layout_In  : constant String := "s(ss)";
   Update_Keyboard_Layout_Out : constant String := "";

   Dump_Data_In  : constant String := "";
   Dump_Data_Out : constant String := "s";
end D_Bus_Helpers.Impl;
