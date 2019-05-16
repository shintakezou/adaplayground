pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with stdint_h;

package pngfunc_c is
   
   function create_image
     (filename : Interfaces.C.Strings.chars_ptr;
      w : int;
      h : int;
      data : access stdint_h.uint8_t) return int;  -- pngfunc.c:12
   pragma Import (C, create_image, "create_image");

end pngfunc_c;
