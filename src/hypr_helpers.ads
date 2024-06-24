with GNATCOLL.JSON;
with Hyprland.State;

package Hypr_Helpers is
   --  Various helpers for Hyprland-related tasks used in `Hyprwatch`

   subtype Workspace_2D_Axis is Positive range 1 .. 255;
   type Workspace_2D is record
      X : Workspace_2D_Axis;
      Y : Workspace_2D_Axis;
   end record;
   --  A 2D representation of a Hyprland workspace
   --  The mapping to 1D workspaces is an implementation detail.

   type Hyprland_Direction is (Unknown, Left, Right, Up, Down);
   --  Directions that a Hyprland window or workspace can be moved

   function Image (Item : Workspace_2D) return String;
   --  Return a String representation of a 2D workspace object

   function Convert
     (W2D : Workspace_2D) return Hyprland.State.Hyprland_Workspace_Id;
   --  Return a `Hyprland_Workspace_Id` object corresponding to a given
   --  2D Workspace
   --
   --  This does not verify whether the target Workspace currently exists!

   function Convert
     (Workspace : Hyprland.State.Hyprland_Workspace_Id) return Workspace_2D;
   --  Return a 2D representation of the provided Workspace
   --
   --  Note that this only works with normal, non-special namespaces

   function Translate
     (W2D : Workspace_2D; Direction : Hyprland_Direction) return Workspace_2D;
   --  Translate the given workspace in the requested direction

   function Generate_Status_JSON
     (State : Hyprland.State.Hyprland_State) return GNATCOLL.JSON.JSON_Value;
   --  Return a JSON document in the following format:
   --  {
   --   "class" : ""
   --   "title" : ""
   --   "workspace" : ""
   --   "workspaces" : ""
   --  }
end Hypr_Helpers;
