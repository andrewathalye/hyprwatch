pragma Ada_2012;

with Ada.Strings.Unbounded;
with Interfaces;

with Hyprland.State_Impl;

package body Hypr_Helpers is
   --  Simple bounded arithmetic
   function Bounded_Decrement
     (Val : Workspace_2D_Axis) return Workspace_2D_Axis is
     (if Val = Workspace_2D_Axis'First then Val else Val - 1);

   function Bounded_Increment
     (Val : Workspace_2D_Axis) return Workspace_2D_Axis is
     (if Val = Workspace_2D_Axis'Last then Val else Val + 1);

   -----------
   -- Image --
   -----------

   function Image (Item : Workspace_2D) return String is
      X_Image : constant String := Item.X'Image;
      Y_Image : constant String := Item.Y'Image;
   begin
      return
        "(" & X_Image (X_Image'First + 1 .. X_Image'Last) & "," &
        Y_Image (Y_Image'First + 1 .. Y_Image'Last) & ")";
   end Image;

   ---------------
   -- Transform --
   ---------------

   function Convert
     (W2D : Workspace_2D) return Hyprland.State.Hyprland_Workspace_Id
   is
      use Interfaces;

      X   : constant Unsigned_32 := Unsigned_32 (W2D.X);
      Y   : constant Unsigned_32 := Unsigned_32 (W2D.Y);
      Raw : Unsigned_32;
   begin
      Raw := Shift_Left (Y - 1, 8);
      Raw := Raw or X;

      return Hyprland.State_Impl.Value (Raw'Image);
   end Convert;

   function Convert
     (Workspace : Hyprland.State.Hyprland_Workspace_Id) return Workspace_2D
   is
      use Interfaces;
      Raw : constant Unsigned_32 :=
        Unsigned_32'Value (Hyprland.State_Impl.Image (Workspace));
      X   : Unsigned_32;
      Y   : Unsigned_32;
   begin
      X := Raw and 16#FF#;
      Y := Shift_Right (Raw, 8) + 1;
      return (Workspace_2D_Axis (X), Workspace_2D_Axis (Y));
   end Convert;

   ---------------
   -- Translate --
   ---------------
   function Translate
     (W2D : Workspace_2D; Direction : Hyprland_Direction) return Workspace_2D
   is
      Result : Workspace_2D := W2D;
   begin
      case Direction is
         when Left =>
            Result.X := Bounded_Decrement (Result.X);
         when Right =>
            Result.X := Bounded_Increment (Result.X);
         when Up =>
            Result.Y := Bounded_Decrement (Result.Y);
         when Down =>
            Result.Y := Bounded_Increment (Result.Y);
         when Unknown =>
            raise Program_Error with "Direction unknown";
      end case;

      return Result;
   end Translate;

   -------------------------------
   -- Generate_Workspace_String --
   -------------------------------
   function Generate_Workspace_String
     (State : Hyprland.State.Hyprland_State) return String;
   function Generate_Workspace_String
     (State : Hyprland.State.Hyprland_State) return String
   is
      use Ada.Strings.Unbounded;
      use type Hyprland.State.Hyprland_Workspace_Id;

      Active_W : constant Hyprland.State.Hyprland_Workspace_Id :=
        State.Active_Workspace;

      Buf : Unbounded_String;
   begin
      for W of State.Workspaces loop
         --  Skip the active workspace and any special workspaces
         if W.Id /= Active_W and not Hyprland.State.Is_Special (W.Id) then
            Append (Buf, Image (Convert (W.Id)));
            Append (Buf, " ");
         end if;
      end loop;

      return To_String (Buf);
   end Generate_Workspace_String;

   --------------------------
   -- Generate_Status_JSON --
   --------------------------
   function Transform
     (Workspace : Hyprland.State.Hyprland_Workspace_Id) return String;
   function Transform
     (Workspace : Hyprland.State.Hyprland_Workspace_Id) return String
   is
   begin
      if Hyprland.State.Is_Special (Workspace) then
         return Hyprland.State_Impl.Image (Workspace);
      else
         return Image (Convert (Workspace));
      end if;
   end Transform;

   function Truncate
     (U : Ada.Strings.Unbounded.Unbounded_String)
      return Ada.Strings.Unbounded.Unbounded_String
   is
      use Ada.Strings.Unbounded;
   begin
      if Length (U) > 100 then
         return Unbounded_Slice (U, 1, 100);
      else
         return U;
      end if;
   end Truncate;

   function Generate_Status_JSON
     (State : Hyprland.State.Hyprland_State) return GNATCOLL.JSON.JSON_Value
   is
      use GNATCOLL.JSON;
      use type Hyprland.State.Hyprland_Window_Id;

      Result : constant JSON_Value := GNATCOLL.JSON.Create_Object;

      Active_Window : Hyprland.State.Hyprland_Window;
   begin
      --  If no window is selected then everything should be blank
      --  Alternatively, if a window was marked as selected but hasn’t
      --  officially been opened yet, idem
      if State.Active_Window = Hyprland.State.No_Window or
        not State.Windows.Contains (State.Active_Window)
      then
         Result.Set_Field ("class", "");
         Result.Set_Field ("title", "");

         --  Take 'workspace' from the generic focused workspace
         Result.Set_Field
           ("workspace", Hypr_Helpers.Transform (State.Active_Workspace));
      else
         Active_Window := State.Windows.Element (State.Active_Window);
         Result.Set_Field ("class", Active_Window.Class);
         Result.Set_Field ("title", Truncate (Active_Window.Title));

         --  Take 'workspace' from the currently-focused window
         --  (more accurate)
         Result.Set_Field
           ("workspace", Hypr_Helpers.Transform (Active_Window.Workspace));
      end if;

      Result.Set_Field
        ("workspaces", Hypr_Helpers.Generate_Workspace_String (State));

      return Result;
   end Generate_Status_JSON;
end Hypr_Helpers;
