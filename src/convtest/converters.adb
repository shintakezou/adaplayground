package body Converters is
   
   function To_String (V : A_Type) return String is
     (Float'Image (Float (V)));
   
   function Concat (L : String; R : A_Type) return String is
     (L & String_Conv (R));
   
end Converters;
