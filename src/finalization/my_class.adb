with Ada.Unchecked_Deallocation;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Sequential_IO;
with Interfaces; use Interfaces;

package body My_Class is
      
   overriding
   procedure Initialize (O : in out Object) is
   begin
      Put_Line ("Allocating");
      O.A := new Buffer (1..256);
   end Initialize;
   
   overriding
   procedure Finalize   (O : in out Object) is
      procedure Free is 
         new Ada.Unchecked_Deallocation (Buffer, Buffer_Ptr);
   begin
      Put_Line ("Freeing");
      Free (O.A);
   end Finalize;
   
   
   procedure Write (O : in Object) is
      package Buf_IO is new Ada.Sequential_IO (Buffer);
      
      H : Buf_IO.File_Type;
      
   begin
      for E of O.A.all loop
         --Put_Line (Integer'Image (E));
         E := 16#55AABBCC#;
      end loop;
      O.A (1) := Unsigned_32'Last;
      O.A (2) := Unsigned_32'First;
      Buf_IO.Create (H, Buf_IO.Out_File, "prova.bin");
      Buf_IO.Write (H, O.A.all);
      --  clearly endianness dependent; and there's a header, sort of,
      --      00 04 00 00 ff ff ff ff  00 00 00 00 cc bb aa 55
      --      ??????????? ---Last----  ---First--- -55AABBCC-- 
      --  I doubt this is the way to produce a portable binary file
      --  which can be read by a similar program on every other machine
      Buf_IO.Close (H);
   end Write;   
   
end My_Class;
