pragma Ada_2012;

with Ada.Strings.Unbounded;
with Interfaces;

with Hyprland.State_Impl;

package body Helpers is
   function Not_Negative
     (Workspace : Hyprland.State.Hyprland_Workspace_Id) return Boolean is
     (Integer'Value (Hyprland.State_Impl.Image (Workspace)) >= 0);

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

   function Transform
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
   end Transform;

   function Transform
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
   end Transform;

   function Transform
     (Workspace : Hyprland.State.Hyprland_Workspace_Id) return String is
     (if Not_Negative (Workspace) then Image (Transform (Workspace))
      else Hyprland.State_Impl.Image (Workspace));

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
         if W.Id /= Active_W and Not_Negative (W.Id) then
            Append (Buf, Image (Transform (W.Id)));
            Append (Buf, " ");
         end if;
      end loop;

      return To_String (Buf);
   end Generate_Workspace_String;

   --------------------------
   -- Generate_Status_JSON --
   --------------------------
   function Generate_Status_JSON
     (State : Hyprland.State.Hyprland_State) return GNATCOLL.JSON.JSON_Value
   is
      use GNATCOLL.JSON;
      use type Hyprland.State.Hyprland_Window_Id;

      Result : constant JSON_Value := GNATCOLL.JSON.Create_Object;

      Active_Window : Hyprland.State.Hyprland_Window;
   begin
      --  If no window is selected then everything should be blank
      if State.Active_Window = Hyprland.State.No_Window then
         Result.Set_Field ("class", "");
         Result.Set_Field ("title", "");

         --  Take 'workspace' from the generic focused workspace
         Result.Set_Field
           ("workspace", Helpers.Transform (State.Active_Workspace));
      else
         Active_Window := State.Windows.Element (State.Active_Window);
         Result.Set_Field ("class", Active_Window.Class);
         Result.Set_Field ("title", Active_Window.Title);

         --  Take 'workspace' from the currently-focused window
         --  (more accurate)
         Result.Set_Field
           ("workspace", Helpers.Transform (Active_Window.Workspace));
      end if;

      Result.Set_Field
        ("workspaces", Helpers.Generate_Workspace_String (State));

      return Result;
   end Generate_Status_JSON;
end Helpers;
