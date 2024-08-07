with Ada.Directories;
with Ada.Environment_Variables;
with Ada.Characters.Handling;
with Ada.Streams.Stream_IO;

with Debug; use Debug;

package body Hyprland.Protocol is

   function "+"
     (Source : Ada.Strings.Unbounded.Unbounded_String) return String renames
     Ada.Strings.Unbounded.To_String;

   ---------------------------------
   --  Get_Hypr_Socket_Base_Path  --
   ---------------------------------
   --  Checks that the correct environment variables are set and that
   --  the /hypr/ directory exists.
   function Get_Hypr_Socket_Base_Path return String;

   function Get_Hypr_Socket_Base_Path return String is
   begin
      if not Ada.Environment_Variables.Exists ("HYPRLAND_INSTANCE_SIGNATURE")
      then
         raise Socket_Not_Found with "HYPRLAND_INSTANCE_SIGNATURE not set.";

      elsif not Ada.Environment_Variables.Exists ("XDG_RUNTIME_DIR") then
         raise Socket_Not_Found with "XDG_RUNTIME_DIR not set.";
      end if;

      declare

         Proposed_Path : constant String :=
           Ada.Environment_Variables.Value ("XDG_RUNTIME_DIR") & "/hypr/" &
           Ada.Environment_Variables.Value ("HYPRLAND_INSTANCE_SIGNATURE") &
           "/";

      begin
         if not Ada.Directories.Exists (Proposed_Path) then
            raise Socket_Not_Found
              with "Directory " & Proposed_Path & " does not exist.";
         end if;

         return Proposed_Path;
      end;
   end Get_Hypr_Socket_Base_Path;

   -------------
   -- Connect --
   -------------

   procedure Connect (Hypr : in out Hyprland_Connection) is

      Hypr_Socket_Base_Path : constant String := Get_Hypr_Socket_Base_Path;

   begin
      if Hypr.Valid then
         raise Invalid_Connection;
      end if;

      GNAT.Sockets.Create_Socket (Hypr.Socket2, GNAT.Sockets.Family_Unix);

      GNAT.Sockets.Connect_Socket
        (Hypr.Socket2,
         GNAT.Sockets.Unix_Socket_Address
           (Hypr_Socket_Base_Path & "/.socket2.sock"));

      Hypr.Stream2 := GNAT.Sockets.Stream (Hypr.Socket2);

      GNAT.Sockets.Create_Selector (Hypr.Selector);

      Hypr.Valid := True;
   end Connect;

   ----------------
   -- Disconnect --
   ----------------

   procedure Disconnect (Hypr : in out Hyprland_Connection) is
   begin
      if not Hypr.Valid then
         raise Invalid_Connection;
      end if;

      Hypr.Valid := False;
      GNAT.Sockets.Close_Socket (Hypr.Socket2);
      GNAT.Sockets.Close_Selector (Hypr.Selector);
   end Disconnect;

   ---------------------
   -- File_Descriptor --
   ---------------------

   function File_Descriptor (Hypr : Hyprland_Connection) return Integer is
   begin
      if not Hypr.Valid then
         raise Invalid_Connection;
      end if;

      return GNAT.Sockets.To_C (Hypr.Socket2);
   end File_Descriptor;

   ------------------
   -- Has_Messages --
   ------------------

   function Has_Messages (Hypr : in out Hyprland_Connection) return Boolean is

      use all type GNAT.Sockets.Selector_Status;

      Read_Socket_Set  : GNAT.Sockets.Socket_Set_Type;
      Write_Socket_Set : GNAT.Sockets.Socket_Set_Type;

      Status : GNAT.Sockets.Selector_Status;

   begin
      if not Hypr.Valid then
         raise Invalid_Connection;
      end if;

      --  Note: This has to be done here instead of globally for some reason
      GNAT.Sockets.Set (Read_Socket_Set, Hypr.Socket2);

      GNAT.Sockets.Check_Selector
        (Selector     => Hypr.Selector, R_Socket_Set => Read_Socket_Set,
         W_Socket_Set => Write_Socket_Set, Status => Status,
         Timeout      => GNAT.Sockets.Immediate);

      return Status = Completed;
   end Has_Messages;

   ---------------------
   -- Receive_Message --
   ---------------------

   function Receive_Message
     (Hypr : in out Hyprland_Connection) return Hypr2_Message
   is

      Msg_Id      : Ada.Strings.Unbounded.Unbounded_String;
      Msg_Id_Done : Boolean := False;

      Msg_Body : Ada.Strings.Unbounded.Unbounded_String;

      Buf : Character;

   begin
      if not Hypr.Valid then
         raise Invalid_Connection;
      end if;

      Character'Read (Hypr.Stream2, Buf);

      while Buf /= ASCII.LF loop
         if Msg_Id_Done then
            Ada.Strings.Unbounded.Append (Msg_Body, Buf);

         else
            if Buf = '>' then
               Character'Read (Hypr.Stream2, Buf); --  '>'
               Msg_Id_Done := True;

            else
               Ada.Strings.Unbounded.Append (Msg_Id, Buf);
            end if;
         end if;

         Character'Read (Hypr.Stream2, Buf);
      end loop;

      return
        Hypr2_Message'
          (Msg_Id => Hypr2_Message_Id'Value (+Msg_Id), Msg_Body => Msg_Body);
   end Receive_Message;

   --------------------
   -- Send_Unchecked --
   --------------------
   function Send_Unchecked
     (Hypr : in out Hyprland_Connection; Command : String) return String
   is
      use GNAT.Sockets;

      Buf    : Ada.Strings.Unbounded.Unbounded_String;
      Socket : GNAT.Sockets.Socket_Type;
      Stream : GNAT.Sockets.Stream_Access;

   begin
      --  For the sake of semantics
      if not Hypr.Valid then
         raise Invalid_Connection;
      end if;

      --  Open Hypr1 for reading / writing
      --  Note: This socket automatically closes after a command is issued.
      Create_Socket (Socket, Family_Unix);
      Connect_Socket
        (Socket,
         Unix_Socket_Address (Get_Hypr_Socket_Base_Path & ".socket.sock"));

      Stream := GNAT.Sockets.Stream (Socket);

      Put_Debug (Command);

      --  Send command
      String'Write (Stream, Command);

      --  Read response
      declare

         C : Character;

      begin
         loop
            Character'Read (Stream, C);
            Ada.Strings.Unbounded.Append (Buf, C);
         end loop;
      exception
         when Ada.Streams.Stream_IO.End_Error =>
            null;
         --  This is expected as Hyprland doesn’t give us an EOF indicator
      end;

      Close_Socket (Socket);

      return +Buf;
   end Send_Unchecked;

   ------------------
   -- Send_Message --
   ------------------

   function Send_Message
     (Hypr      : in out Hyprland_Connection; Id : Hypr1_Message_Id;
      Arguments :        String := "") return String
   is
      Buf : Ada.Strings.Unbounded.Unbounded_String;
   begin
      --  Request JSON format
      Ada.Strings.Unbounded.Append (Buf, "j/");
      Ada.Strings.Unbounded.Append
        (Buf, Ada.Characters.Handling.To_Lower (Id'Image));

      if Arguments'Length > 0 then
         Ada.Strings.Unbounded.Append (Buf, " " & Arguments);
      end if;

      return Send_Unchecked (Hypr, +Buf);
   end Send_Message;

end Hyprland.Protocol;
