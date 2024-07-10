package String_Utils is
   function Strip_Spaces (Item : String) return String;
   --  Strip leading spaces from `Item`, if present.
   --  Otherwise return the original String.
end String_Utils;
