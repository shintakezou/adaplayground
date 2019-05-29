with Ada.Finalization;
with Interfaces; use Interfaces;
package My_Class is
   
   type Object is 
     new Ada.Finalization.Limited_Controlled with private;
   
   overriding
   procedure Initialize (O : in out Object);
   
   overriding
   procedure Finalize   (O : in out Object);
   
   procedure Write (O : in Object);
   --  write the object?
   
private
   
   type Buffer is array (Positive range <>) of Unsigned_32;
   type Buffer_Ptr is access Buffer;

   type Object is 
     new Ada.Finalization.Limited_Controlled
      with record
         A : Buffer_Ptr;
      end record;
   
end My_Class;
