--  @summary
--  Convert to string and concatenate float-derived types.
--
--  @description
--  See its usage to get the idea. However, I don't think this
--  should be a "top" package. I would like it could be seen
--  only in Measure_Units, because Measure_Units uses it to
--  provide some functions to its clients.
-- 
--  Could this be a good idea as a general purpose utility?
--
package Converters is
   
   generic
      type A_Type is new Float;
   function To_String (V : A_Type) return String;
   
   generic
      type A_Type is new Float;
      with function String_Conv (C : A_Type) return String;
   function Concat (L : String; R : A_Type) return String;
   
end Converters;
