with Ada.Strings.Unbounded;

private with GNAT.Sockets;

package Hyprland.Protocol is
   --  This package is a low-level interface to the Hyprland compositor’s
   --  IPC components. See `Hyprland.State` for a high-level interface that
   --  allows a client program to disregard low-level protocol details.
   --
   --  Basic protocol overview:
   --  `Connect`
   --  `Has_Messages` -> `Receive_Message` -> `Send_Message`
   --  `Disconnect`

   ------------------
   --  Exceptions  --
   ------------------
   Socket_Not_Found   : exception;
   Invalid_Connection : exception;

   -------------
   --  Types  --
   -------------
   type Hyprland_Connection is limited private;
   --  The base Hyprland object. `Connect` to the compositor to use it and
   --  `Disconnect` when done.

   type Hypr2_Message is record
      Msg_Id   : Hypr2_Message_Id;
      Msg_Body : Ada.Strings.Unbounded.Unbounded_String;
   end record;
   --  A control message to be read from Hypr2 via `Receive_Message`

   -------------------
   --  Subprograms  --
   -------------------
   procedure Connect (Hypr : in out Hyprland_Connection);
   --  Connect to a Hyprland compositor instance. This will
   --  connect to both Hyprland control sockets and allow
   --  the `Hyprland_Connection` instance to be used with the
   --  remaining subprograms.

   procedure Disconnect (Hypr : in out Hyprland_Connection);
   --  This disconnects from a compositor instance and cleans
   --  up all relevant data. The connection is invalid after
   --  this point.

   function Has_Messages (Hypr : in out Hyprland_Connection) return Boolean;
   --  Blocks for 100 msec and checks whether any messages are available.

   function Receive_Message
     (Hypr : in out Hyprland_Connection) return Hypr2_Message;
   --  Reads a message from the Hyprland connection (blocking).

   function Send_Message
     (Hypr      : in out Hyprland_Connection; Id : Hypr1_Message_Id;
      Arguments :        String := "") return String;
   --  Sends a message to Hyprland (synchronous, blocking)
   --  Returns Hypr1's response
private
   type Hyprland_Connection is limited record
      Valid : Boolean := False;

      --  Socket1 needs to be opened every time it is used
      --  No need to store here

      --  Socket2
      Socket2 : GNAT.Sockets.Socket_Type;
      Stream2 : GNAT.Sockets.Stream_Access;

      --  Semi-Asynchronous I/O
      Selector : GNAT.Sockets.Selector_Type;
   end record;
end Hyprland.Protocol;
