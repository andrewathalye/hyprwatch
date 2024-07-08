package body String_Utils is
   function Strip_Space (Item : String) return String is
   begin
      if Item (Item'First) = ' ' then
         if Item'Length = 1 then
            return "";

         else
            return Item (Item'First + 1 .. Item'Last);
         end if;

      else
         return Item;
      end if;
   end Strip_Space;
end String_Utils;
