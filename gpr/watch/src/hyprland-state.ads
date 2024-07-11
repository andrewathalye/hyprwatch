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

   function Is_Special (Item : Hyprland_Workspace_Id) return Boolean renames
     Hyprland.State_Impl.Is_Special;
   --  Returns `True` if `Item` is a "special" workspace according
   --  to Hyprland. In this case it may be sorted differently, etc.

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

   -----------------
   --  Accessors  --
   -----------------
   function Workspaces (State : Hyprland_State) return Hyprland_Workspace_List;
   function Windows (State : Hyprland_State) return Hyprland_Window_List;
   function Active_Window (State : Hyprland_State) return Hyprland_Window_Id;
   function Active_Workspace
     (State : Hyprland_State) return Hyprland_Workspace_Id;
   function Keyboard_Layout
     (State : Hyprland_State) return Ada.Strings.Unbounded.Unbounded_String;
   function Keyboard_Variant
     (State : Hyprland_State) return Ada.Strings.Unbounded.Unbounded_String;

   -------------------
   --  Subprograms  --
   -------------------
   procedure Connect (State : in out Hyprland_State);
   --  Connect to a running Hyprland instance and load basic state information.

   procedure Disconnect (State : in out Hyprland_State);
   --  Disconnect from a Hyprland instance and destroy state information.

   function Connection
     (State : in out Hyprland_State)
      return access Hyprland.Protocol.Hyprland_Connection;
   --  Return the underlying connection.
   --  Note that any modification of this connection might result
   --  in undefined behaviour should `Hyprland.State` continue
   --  to be used.

   function Update (State : in out Hyprland_State) return Boolean;
   --  Update a `Hyprland_State` object using fresh data from the compositor.
   --  The return value indicates whether any fields were updated.

   procedure Activate_Workspace
     (State : in out Hyprland_State; Workspace : Hyprland_Workspace_Id);
   --  Activate the selected workspace

   procedure Move_Window
     (State       : in out Hyprland_State; Window : Hyprland_Window_Id;
      Destination :        Hyprland_Workspace_Id);
   --  Move a window to another workspace.

   procedure Set_Layout
     (State   : in out Hyprland_State; Keyboard : String; Layout : String;
      Variant :        String);
   --  Set the current keyboard layout to (Layout, Variant)
   --  TODO this is currently global across all keyboards

private
   type HCA is access Protocol.Hyprland_Connection;

   type Hyprland_State is tagged record
      Valid      : Boolean := False;
      Connection : HCA;

      Workspaces       : Hyprland_Workspace_List;
      Windows          : Hyprland_Window_List;
      Active_Window    : Hyprland_Window_Id    := No_Window;
      Active_Workspace : Hyprland_Workspace_Id := No_Workspace;

      --  TODO add keyboard name
      --  TODO this is also global across keyboards
      Keyboard_Layout  : Ada.Strings.Unbounded.Unbounded_String;
      Keyboard_Variant : Ada.Strings.Unbounded.Unbounded_String;
   end record;
end Hyprland.State;
