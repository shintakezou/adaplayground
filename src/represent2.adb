with Ada.Text_IO;      use Ada.Text_IO; 
with Ada.Wide_Text_IO; use Ada.Wide_Text_IO;
with Interfaces;       use Interfaces;
with System;
with Ada.Unchecked_Conversion;

use Ada;

-- Play with UTF8
pragma Wide_Character_Encoding (UTF8);

procedure represent2 is
   
   type Action_Type is (Load, 
			Store, 
			Copy, 
			Add, 
			Clear,
			Jump,
			Ret)
     with Size => 4;
   
   for Action_Type use 
     (Load  => 2#1000#,
      Store => 2#1001#,
      Copy  => 2#1010#,
      Add   => 2#1011#,
      Clear => 2#1100#,
      Jump  => 2#1101#,
      Ret   => 2#1110#);
   
   type Register_Type is mod 2**4 with Size => 4;
   type Ref_Register_Type is mod 2**2
     with Size => 2;
   
      
   type Mode_Type is (Direct, Register, Indirect)
     with Size => 2;
   
   for Mode_Type use
     (Direct   => 2#01#,
      Register => 2#10#,
      Indirect => 2#11#);
   
  
   type Mult_Factor_Type is mod 2**2 
     with Size => 2;
   
   type Long_Reg (Mode : Mode_Type := Direct) is record
      Action : Action_Type;                          -- 4
      case Mode is
	 when Direct =>
	    Bit_Fill : Mult_Factor_Type;             -- 2
	    Address  : Unsigned_8;                   -- 8
	 when Register =>
	    Mult_Factor : Mult_Factor_Type;          -- 2
	    Reg1 : Register_Type;                    -- 4
	    Reg2 : Register_Type;                    -- 4
	 when Indirect =>
	    Ref_Reg : Ref_Register_Type;             -- 2
	    Offset  : Unsigned_8;                    -- 8
      end case;
   end record with
     Size => 16, -- Pack, 
     Bit_Order => System.Low_Order_First;
   
   -- This works.
   for Long_Reg use record
      Action      at 0 range 12 .. 15;
      Mode        at 0 range 10 .. 11;
      
      Bit_Fill    at 0 range  8 ..  9;
      Address     at 0 range  0 ..  7;
      
      Mult_Factor at 0 range  8 ..  9;
      Reg1        at 0 range  4 ..  7;
      Reg2        at 0 range  0 ..  3;
      Ref_Reg     at 0 range  8 ..  9;
      Offset      at 0 range  0 ..  7;
   end record;
   
   type Word is new Interfaces.Unsigned_16;
   
   function To_Word is
      new Unchecked_Conversion (Long_Reg, Word);
   
   procedure Put_Word (S : Long_Reg) is
   begin
      Put_Line (Word'Image (To_Word (S)));
   end;
   
   
   E : Long_Reg;
      
begin
   Put_Line (Integer'Image (Long_Reg'Size));
   Put (Wide_Character'Val (16#03BC#));
   Text_IO.New_Line;
   
   E := (Action => Load,
	 Mode => Direct,
	 Bit_Fill => 0,
	 Address => 0);
   Put_Word (E);   
   
   E := (Action => Load,
	 Mode => Register,
	 Mult_Factor => 3,
	 Reg1 => 15,
	 Reg2 => 0);
   Put_Word (E);
end;
