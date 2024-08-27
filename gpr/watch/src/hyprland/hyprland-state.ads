with Ada.Containers.Vectors;
with Ada.Containers.Ordered_Maps;
with Ada.Strings.Unbounded;

with Hyprland.Protocol;
with Hyprland.State_Impl;

use type Hyprland.State_Impl.Hyprland_Monitor_Id;
use type Hyprland.State_Impl.Hyprland_Workspace_Id;
use type Hyprland.State_Impl.Hyprland_Window_Id;

package Hyprland.State is
   --  A high-level interface to the Hyprland compositor's IPC.
   --  This is preferred to `Hyprland.Protocol`, which allows direct
   --  control over the relevant protocol details.

   ------------------
   --  Exceptions  --
   ------------------
   Request_Failed : exception;
   Invalid_State  : exception;

   -------------------
   --  Basic Types  --
   -------------------
   subtype Hyprland_Monitor_Id is Hyprland.State_Impl.Hyprland_Monitor_Id;
   --  Private type for a monitor ID

   No_Monitor : Hyprland_Monitor_Id renames Hyprland.State_Impl.No_Monitor;
   --  The ID of a non-existent monitor
   --
   subtype Hyprland_Workspace_Id is Hyprland.State_Impl.Hyprland_Workspace_Id;
   --  Private type for a workspace ID

   function Is_Special (Item : Hyprland_Workspace_Id) return Boolean renames
     Hyprland.State_Impl.Is_Special;
   --  Returns `True` if `Item` is a "special" workspace according
   --  to Hyprland. In this case it may be sorted differently, etc.

   No_Workspace :
     Hyprland_Workspace_Id renames Hyprland.State_Impl.No_Workspace;
   --  The ID of a non-existent workspace

   subtype Hyprland_Window_Id is Hyprland.State_Impl.Hyprland_Window_Id;
   --  Private type for a window ID

   No_Window : Hyprland_Window_Id renames Hyprland.State_Impl.No_Window;
   --  The ID of a non-existent window

   --------------------
   -- Implementation --
   --------------------
   package Hyprland_Workspace_Id_Lists is new Ada.Containers.Vectors
     (Natural, Hyprland_Workspace_Id);
   subtype Hyprland_Workspace_Id_List is Hyprland_Workspace_Id_Lists.Vector;

   package Hyprland_Window_Id_Lists is new Ada.Containers.Vectors
     (Natural, Hyprland_Window_Id);
   subtype Hyprland_Window_Id_List is Hyprland_Window_Id_Lists.Vector;

   ----------------
   -- State Type --
   ----------------
   type Hyprland_State is tagged limited private;

   type Hyprland_State_Access is access all Hyprland_State;

   -----------------
   -- Field Types --
   -----------------
   type Hyprland_Monitor is record
      Id         : Hyprland_Monitor_Id := No_Monitor;
      Name       : Ada.Strings.Unbounded.Unbounded_String;
      Workspaces : Hyprland_Workspace_Id_List;
   end record;

   package Hyprland_Monitor_Lists is new Ada.Containers.Ordered_Maps
     (Hyprland_Monitor_Id, Hyprland_Monitor);
   subtype Hyprland_Monitor_List is Hyprland_Monitor_Lists.Map;

   type Hyprland_Workspace is record
      Id      : Hyprland_Workspace_Id := No_Workspace;
      Monitor : Hyprland_Monitor_Id   := No_Monitor;
      Name    : Ada.Strings.Unbounded.Unbounded_String;
      Windows : Hyprland_Window_Id_List;
   end record;

   package Hyprland_Workspace_Lists is new Ada.Containers.Ordered_Maps
     (Hyprland_Workspace_Id, Hyprland_Workspace);
   subtype Hyprland_Workspace_List is Hyprland_Workspace_Lists.Map;

   type Hyprland_Window is record
      Id          : Hyprland_Window_Id    := No_Window;
      Workspace   : Hyprland_Workspace_Id := No_Workspace;
      Class       : Ada.Strings.Unbounded.Unbounded_String;
      Title       : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   package Hyprland_Window_Lists is new Ada.Containers.Ordered_Maps
     (Hyprland_Window_Id, Hyprland_Window);
   subtype Hyprland_Window_List is Hyprland_Window_Lists.Map;

   -----------------
   --  Accessors  --
   -----------------
   function Monitors (State : Hyprland_State) return Hyprland_Monitor_List;
   function Workspaces (State : Hyprland_State) return Hyprland_Workspace_List;
   function Windows (State : Hyprland_State) return Hyprland_Window_List;

   function Active_Monitor (State : Hyprland_State) return Hyprland_Monitor_Id;
   function Active_Workspace
     (State : Hyprland_State) return Hyprland_Workspace_Id;
   function Active_Window (State : Hyprland_State) return Hyprland_Window_Id;

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

   function File_Descriptor (State : Hyprland_State) return Integer;
   --  Return a file descriptor associated with `State` to be polled
   --  for Hyprland updates. Reading or writing from the resulting
   --  descriptor is undefined behaviour.

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
   type Hyprland_State is new Protocol.Hyprland_Connection with record
      Valid : Boolean := False;

      Monitors   : Hyprland_Monitor_List;
      Workspaces : Hyprland_Workspace_List;
      Windows    : Hyprland_Window_List;

      Active_Monitor   : Hyprland_Monitor_Id   := No_Monitor;
      Active_Workspace : Hyprland_Workspace_Id := No_Workspace;
      Active_Window    : Hyprland_Window_Id    := No_Window;

      --  TODO add keyboard name
      --  TODO this is also global across keyboards
      Keyboard_Layout  : Ada.Strings.Unbounded.Unbounded_String;
      Keyboard_Variant : Ada.Strings.Unbounded.Unbounded_String;
   end record;
end Hyprland.State;
