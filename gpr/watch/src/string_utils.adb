package body String_Utils is
   function Strip_Spaces (Item : String) return String is
      Index : Natural := Item'First;
   begin
      while Item (Index) = ' ' loop
         Index := Index + 1;
      end loop;

      if Item'Length = 1 then
         return "";
      else
         return Item (Index .. Item'Last);
      end if;
   end Strip_Spaces;
end String_Utils;
