pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Extensions;

package stdint_h is
   subtype uint8_t is unsigned_char;  -- /usr/include/stdint.h:48
end stdint_h;
