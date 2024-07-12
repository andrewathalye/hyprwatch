with System.Storage_Elements;

with D_Bus.Arguments.Basic;

with D_Bus_Helpers.Impl; use D_Bus_Helpers.Impl;

with Debug; use Debug;

procedure D_Bus_Helpers.Introspect
  (Request :     D_Bus.Messages.Message_Type;
   Reply   : out D_Bus.Messages.Message_Type)
is
   ------------------------
   -- Introspection Data --
   ------------------------
   type Symbol is null record with
     Convention => C;
   --  Note: DO NOT ATTEMPT TO ACCESS

   function To_Natural (S : aliased Symbol) return Natural;
   --  Architecture-indepedent function to transform a Symbol into
   --  a Natural. This is equivalent to reading the address of an
   --  ELF symbol and interpreting it as a number.

   function To_Natural (S : aliased Symbol) return Natural is
   begin
      return Natural (System.Storage_Elements.To_Integer (S'Address));
   end To_Natural;

   Introspect_XML_Size : aliased Symbol with
     Import        => True, Convention => C,
     External_Name => "_binary_introspect_xml_size";

   Introspect_String :
     aliased String (1 .. To_Natural (Introspect_XML_Size)) with
     Import        => True, Convention => C,
     External_Name => "_binary_introspect_xml_start";

     -------------------------
     -- Normal D_Bus Method --
     -------------------------
   use D_Bus.Arguments.Basic;

   Request_Signature : constant String := Get_Signature (Request);
begin
   Put_Debug ("Introspect " & Request_Signature);

   --  Check signature
   if Request_Signature /= Introspect_In then
      Raise_Signature_Error (Request, Reply, Request_Signature, Introspect_In);
      return;
   end if;

   Reply := D_Bus.Messages.New_Method_Return (Request);
   D_Bus.Messages.Add_Arguments (Reply, +Introspect_String);
end D_Bus_Helpers.Introspect;
