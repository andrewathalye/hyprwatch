with GNATCOLL.JSON;
with Hyprland.State;

package Helpers is
   subtype Workspace_2D_Axis is Positive range 1 .. 255;
   type Workspace_2D is record
      X : Workspace_2D_Axis;
      Y : Workspace_2D_Axis;
   end record;

   function Image (Item : Workspace_2D) return String;
   --  Return a String representation of a 2D workspace object

   function Transform
     (W2D : Workspace_2D) return Hyprland.State.Hyprland_Workspace_Id;
   --  Return a `Hyprland_Workspace_Id` object corresponding to a given
   --  2D Workspace
   --
   --  This does not verify whether the target Workspace currently exists!

   function Transform
     (Workspace : Hyprland.State.Hyprland_Workspace_Id) return Workspace_2D;
   --  Return a 2D representation of the provided Workspace
   --  1 => (1,0) 2 => (2,0)
   --  256 => (1,1) etc.
   --
   --  Note that this only works with normal, non-special namespaces

   function Transform
     (Workspace : Hyprland.State.Hyprland_Workspace_Id) return String;
   --  See above, but return a String representation of the provided
   --  2D Workspace and a backup representation for special Workspaces

   function Generate_Status_JSON
     (State : Hyprland.State.Hyprland_State) return GNATCOLL.JSON.JSON_Value;
   --  Return a JSON document in the following format:
   --  {
   --   "class" : ""
   --   "title" : ""
   --   "workspace" : ""
   --   "workspaces" : ""
   --  }
end Helpers;
