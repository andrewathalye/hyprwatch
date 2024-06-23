with Ada.Containers.Vectors;
with Ada.Containers.Ordered_Maps;
with Ada.Strings.Unbounded;

with Hyprland.Protocol;
with Hyprland.State_Impl;
use type Hyprland.State_Impl.Hyprland_Window_Id;
use type Hyprland.State_Impl.Hyprland_Workspace_Id;

package Hyprland.State is
   --  A high-level interface to the Hyprland compositor's IPC.
   --  This is preferred to `Hyprland.Protocol`, which allows direct
   --  control over the relevant protocol details.

   ------------------
   --  Exceptions  --
   ------------------
   Request_Failed : exception;
   Invalid_State  : exception;

   -------------
   --  Types  --
   -------------
   subtype Hyprland_Window_Id is Hyprland.State_Impl.Hyprland_Window_Id;
   --  Private type for a window ID

   No_Window : Hyprland_Window_Id renames Hyprland.State_Impl.No_Window;
   --  The ID of a non-existent window

   subtype Hyprland_Workspace_Id is Hyprland.State_Impl.Hyprland_Workspace_Id;
   --  Private type for a workspace ID

   No_Workspace :
     Hyprland_Workspace_Id renames Hyprland.State_Impl.No_Workspace;
   --  The ID of a non-existent workspace

   type Hyprland_Window is record
      Id          : Hyprland_Window_Id    := No_Window;
      Workspace   : Hyprland_Workspace_Id := No_Workspace;
      Class       : Ada.Strings.Unbounded.Unbounded_String;
      Title       : Ada.Strings.Unbounded.Unbounded_String;
      Title_Stale : Boolean               := False;
   end record;

   package Hyprland_Window_Lists is new Ada.Containers.Ordered_Maps
     (Hyprland_Window_Id, Hyprland_Window);
   subtype Hyprland_Window_List is Hyprland_Window_Lists.Map;

   package Hyprland_Window_Id_Lists is new Ada.Containers.Vectors
     (Natural, Hyprland_Window_Id);
   subtype Hyprland_Window_Id_List is Hyprland_Window_Id_Lists.Vector;

   type Hyprland_Workspace is record
      Id      : Hyprland_Workspace_Id := No_Workspace;
      Name    : Ada.Strings.Unbounded.Unbounded_String;
      Windows : Hyprland_Window_Id_List;
   end record;

   package Hyprland_Workspace_Lists is new Ada.Containers.Ordered_Maps
     (Hyprland_Workspace_Id, Hyprland_Workspace);
   subtype Hyprland_Workspace_List is Hyprland_Workspace_Lists.Map;

   type Hyprland_State is tagged private;
   type Hyprland_State_Access is access all Hyprland_State;

   -------------------------
   --  Validity Checking  --
   -------------------------
   function Is_Valid (State : Hyprland_State) return Boolean;
   --  Checks whether `State` is valid

   subtype Valid_Hyprland_State is Hyprland_State with
       Dynamic_Predicate => Is_Valid (Valid_Hyprland_State);
   --  A `Hyprland_State` object which semantically cannot be
   --  invalid.

   -----------------
   --  Accessors  --
   -----------------
   function Workspaces (State : Hyprland_State) return Hyprland_Workspace_List;
   function Windows (State : Hyprland_State) return Hyprland_Window_List;
   function Active_Window (State : Hyprland_State) return Hyprland_Window_Id;
   function Active_Workspace
     (State : Hyprland_State) return Hyprland_Workspace_Id;
   function Fullscreen (State : Hyprland_State) return Boolean;

   -------------------
   --  Subprograms  --
   -------------------
   procedure Connect (State : in out Hyprland_State);
   --  Connect to a running Hyprland instance and load basic state information.

   procedure Disconnect (State : in out Hyprland_State);
   --  Disconnect from a Hyprland instance and destroy state information.

   function Update (State : in out Hyprland_State) return Boolean;
   --  Update a `Hyprland_State` object using fresh data from the compositor.
   --  The return value indicates whether any fields were updated.

   procedure Move_Window
     (State       : in out Hyprland_State; Window : Hyprland_Window_Id;
      Destination :        Hyprland_Workspace_Id);
   --  Move a window to another workspace.
private
   type HCA is access Protocol.Hyprland_Connection;

   type Hyprland_State is tagged record
      Valid      : Boolean := False;
      Connection : HCA;

      Workspaces       : Hyprland_Workspace_List;
      Windows          : Hyprland_Window_List;
      Active_Window    : Hyprland_Window_Id    := No_Window;
      Active_Workspace : Hyprland_Workspace_Id := No_Workspace;
      Fullscreen       : Boolean               := False;
   end record;
end Hyprland.State;
