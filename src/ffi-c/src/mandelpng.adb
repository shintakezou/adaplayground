--
-- for the Mandelbrot part, see
--   https://github.com/shintakezou/adaplayground/blob/master/src/mandel_utf.adb
--
-- Usage example:
--   ./mandelpng -2 -2 2 2
--
-- To build, gprbuild. It works on my machine! You need libpng lib and
-- headers.
--
-- Messy withs and uses; no idea if there's a better more idiomatic
-- way.
--
with 
  Ada.Numerics.Elementary_Functions,
  Ada.Numerics.Generic_Complex_Types,
  Ada.Unchecked_Deallocation;

use 
  Ada.Numerics.Elementary_Functions;

with 
  Interfaces.C,
  Interfaces.C.Strings,
  Interfaces.C.Pointers;

with PNGFunc_C;
with stdint_h; use stdint_h;

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;


procedure MandelPNG is
   package C renames Interfaces.C;
   
   
   package Complex_Types is new Ada.Numerics.Generic_Complex_Types (Float);
   use Complex_Types;
   
   -- Configuration constants.
   Width  : constant := 800;
   Height : constant := 600;   
   
   Max_Iterations : constant := 32;
   Num_Of_Threads : constant := 4;
   
   -- Returns the intensity of a single point in the Mandelbrot set.
   function Render_Pixel (C : Complex) return Float is
      Z : Complex := Complex'(0.0, 0.0);
   begin
      for N in Integer range 0 .. Max_Iterations loop
         Z := Z*Z + C;
         if (abs Z > 2.0) then
            return Float (N) / Float (Max_Iterations);
         end if;
      end loop;
      return 0.0;
   end;


   type Bitmap is array(Integer range <>, Integer range <>) of Float;
   type Bitmap_Ref is access Bitmap;
   
   procedure Free_Bitmap is new Ada.Unchecked_Deallocation
     (Object => Bitmap, Name => Bitmap_Ref);
   
   
   procedure Mandelbrot
     (Data           : Bitmap_Ref;
      R1, I1, R2, I2 : Float) 
   is
      Width  : Integer := Data'Length (1);
      Height : Integer := Data'Length (2);
      Xdelta : Float   := (R2-R1) / Float (Width);
      Ydelta : Float   := (I2-I1) / Float (Height);

      I : Float;
      C : Complex;
   begin
      Put_Line ("Width: " & Integer'Image (Width));
      Put_Line ("Height: " & Integer'Image (Height));
      Put_Line ("Xdelta: " & Float'Image (Xdelta));
      Put_Line ("Ydelta: " & Float'Image (Ydelta));
      
      for Y in Data'Range (2) loop
         I := I1 + Float (Y) * Ydelta;
         for X in Data'Range (1) loop
            C := Complex'(R1 + Float (X) * Xdelta, I);
            Data (X, Y) := Render_Pixel (C);
         end loop;
      end loop;
   end;

   
   procedure Dump_Bitmap(Data : Bitmap_Ref) is
      subtype Buffer_Index is Integer range 0 .. 3 * Width * Height - 1;
      
      type Buffer is array (Buffer_Index range <>) of aliased uint8_t
      with
        Component_Size => 8,
        Convention => C;
      
      type Buffer_Access is access all Buffer;
      
      type RGB is record
         Red, Green, Blue : uint8_t;
      end record;
      
      procedure To_RGB (V : in Float; D : out RGB) is
         RR, GG, BB : Float;
      begin
         RR := 255.0 * V; GG := 255.0 * V; BB := 255.0 * V;
         D.Red := uint8_t (RR);
         D.Green := uint8_t (GG);
         D.Blue := uint8_t (BB);
      end To_RGB;
      
      package Im is new C.Pointers (Index => Buffer_Index,
                                    Element => uint8_t,
                                    Element_Array => Buffer,
                                    Default_Terminator => uint8_t'First);
      
      R : C.int;
      Image : aliased Buffer (Buffer_Index'Range);
      The_Image : Im.Pointer := Image (0)'Access;
      
      
      Vals : RGB;
   begin
      for Y in Data'Range (2) loop
         for X in Data'Range (1) loop
            To_RGB (Data (X, Y), Vals);
            Image (3 * (Y * Width + X) + 0) := Vals.Red;
            Image (3 * (Y * Width + X) + 1) := Vals.Green;
            Image (3 * (Y * Width + X) + 2) := Vals.Blue;
         end loop;
      end loop;
      
      R := PNGFunc_C.create_image (C.Strings.New_String ("out.png"),
                                   Width,
                                   Height,
                                   The_Image);
      if Integer (R) < 0 then
         Put_Line (Standard_Error, "error writing image to file");
      else
         Put_Line ("file written");
      end if;
   end;
   
   Image  : Bitmap_Ref;
   use Ada.Command_Line;
begin
   if Argument_Count < 4 then
      Put_Line (Standard_Error, "Usage: " & 
                  Command_Name & " R1 I1 R2 I2");
   else
      declare
         R1 : Float := Float'Value (Argument (1));
         R2 : Float := Float'Value (Argument (3));
         I1 : Float := Float'Value (Argument (2));
         I2 : Float := Float'Value (Argument (4));
      begin
         Image := new Bitmap (0 .. Width - 1, 0 .. Height - 1);
         Mandelbrot (Image, R1, I1, R2, I2);
         Dump_Bitmap (Image);
         Free_Bitmap (Image);
      end;
   end if;
end;
