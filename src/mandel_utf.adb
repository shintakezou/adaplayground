-- this is taken from https://ideone.com/a1ky4l, which comes from
-- here http://cowlark.com/2014-04-27-ada/index.html (David Given)
-- 
-- I've polished it, adapted in style, removed some comments,
-- changed other things, … but mostly it's his code.
--
with 
  Ada.Numerics.Elementary_Functions,
  Ada.Numerics.Generic_Complex_Types,
  Ada.Wide_Text_IO,
  Ada.Unchecked_Deallocation;

use 
  Ada.Numerics.Elementary_Functions,
  Ada.Wide_Text_IO;

pragma Wide_Character_Encoding (UTF8); -- -gnatW8?

procedure Mandel_UTF is
   package Complex_Types is new Ada.Numerics.Generic_Complex_Types (Float);
   use Complex_Types;

   -- Configuration constants.
   Bitmap_Size    : constant := 128; -- pixels
   Max_Iterations : constant := 32;
   Num_Of_Threads : constant := 4;
   
   ------------------------------------------------------
   -- This part replaces Given's glyphs table
   subtype Brush_Levels is Integer range 0 .. 255;
   type Brushes is array (Brush_Levels) of Wide_Character;
   
   function Generate_Glyphs return Brushes is
      First_Code_Point : constant := 10240;
   begin
      return E : Brushes do
	 for I in Brush_Levels'Range loop
	    E (I) := Wide_Character'Val (First_Code_Point + I);
	 end loop;
      end return;
   end Generate_Glyphs;
   
   Glyphs : constant Brushes := Generate_Glyphs;
   ------------------------------------------------------
      
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

   -- Encapsulates the multithreaded render: creates a bunch of workers
   -- and a scheduler, which hands out work units to the renderers.
   procedure Mandelbrot
     (Data           : Bitmap_Ref;
      R1, I1, R2, I2 : Float) 
   is
      Width  : Integer := Data'Length (1);
      Height : Integer := Data'Length (2);
      Xdelta : Float   := (R2-R1) / Float (Width);
      Ydelta : Float   := (I2-I1) / Float (Height);

      task Scheduler is
	 -- Each worker calls this to find out what it needs to do.
	 entry Request_Work_Unit (Y : out Integer; I : out Float);
      end;

      task body Scheduler is
      begin
	 -- Hand out each scanline in turn to tasks that want things to
	 -- do, then exit.
	 for Yy in Data'Range (2) loop
            accept Request_Work_Unit (Y : out Integer; 
				      I : out Float) 
	    do
               Y := Yy;
               I := I1 + Float (Yy) * Ydelta;
            end Request_Work_Unit;
	 end loop;
      end;

      -- Actually does the rendering. Each of these is self contained and will
      -- keep working until there's nothing left to do, at which point it
      -- exits.
      task type Worker;
      task body Worker is
	 Y : Integer;
	 I : Float;
	 C : Complex;
      begin
	 loop
            Scheduler.Request_Work_Unit (Y, I);
            for X in Data'Range (1) loop
               C := Complex'(R1 + Float (X) * Xdelta, I);
               Data (X, Y) := Render_Pixel (C);
            end loop;
	 end loop;
      end;

      -- Create some work threads (which will automatically start).
      Scanlines : array (Integer range 1 .. Num_Of_Threads) of Worker;
   begin
      null;
   end;

   
   -- Writes the bitmap to stdout, using funky Unicode hackery to make it
   -- look pretty. Sort of.
   procedure Dump_Bitmap(Data : Bitmap_Ref) is
      function Is_Set(X, Y : Integer) return Boolean is (Data (X, Y) > 0.0)
	with Inline;

      type Byte is mod 2**8;
      
      X, Y : Integer;
      B    : Byte;
   begin
      Y := 0;
      while (Y <= Data'Last (2)) loop
	 X := 0;
	 while (X < Data'Last (1)) loop
      	    B := (if Is_Set (X+0, Y+0) then 1 else 0);
	    -- this looks clean but I'd like to write it in a different way;
	    -- maybe later... (sort of TODO)
     	    if Is_Set (X+0, Y+1) then B := B or 2; end if;
     	    if Is_Set (X+0, Y+2) then B := B or 4; end if;
     	    if Is_Set (X+0, Y+3) then B := B or 64; end if;
     	    if Is_Set (X+1, Y+0) then B := B or 8; end if;
     	    if Is_Set (X+1, Y+1) then B := B or 16; end if;
     	    if Is_Set (X+1, Y+2) then B := B or 32; end if;
     	    if Is_Set (X+1, Y+3) then B := B or 128; end if;
     	    Put (Glyphs (Byte'Pos (B)));
      	    X := X + 2;
	 end loop;
	 New_Line;
	 Y := Y + 4;
      end loop;
   end;
   
   Width  : constant := Bitmap_Size;
   Height : constant := Width;
   
   Image       : Bitmap_Ref;
begin
   -- Render, print, then ~~leak~~ free a bitmap.
   Image := new Bitmap (0 .. (Width - 1), 0 .. (Height - 1));
   Mandelbrot (Image, -2.0, -2.0, +2.0, +2.0);
   Dump_Bitmap (Image);
   Free_Bitmap (Image);
end;
