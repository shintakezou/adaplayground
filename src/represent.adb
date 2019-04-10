with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;

with Ada.Unchecked_Conversion;
with System;

procedure Represent is
   use Ada;
   
   type Byte is mod 2**8 with Size => 8;
   
   type T_Instruction is (Load, Store, Clear, Move)
     with Size => 2;
   
   for T_Instruction use (Load => 2#00#,
			  Store => 2#01#,
			  Clear => 2#10#,
			  Move  => 2#11#);
   
   type T_Register is mod 2**3 with Size => 3;
   
   type Command is record
      Instruction : T_Instruction;
      Source      : T_Register;
      Dest        : T_Register;
   end record 
   with 
     Size => 8,
     Bit_Order => System.Low_Order_First;
   
   for Command use record
      Instruction at 0 range 6 .. 7;
      Source      at 0 range 3 .. 5;
      Dest        at 0 range 0 .. 2;
   end record;
   
   function To_Byte is new Unchecked_Conversion
     (Source => Command,
      Target => Byte);
   
   procedure Put_Instruction
     (Instruction : T_Instruction;
      Src, Dest   : T_Register) 
   is
      Instr : Command := (Instruction, Src, Dest);
   begin
      Put_Line (Byte'Image (To_Byte (Instr)));
   end;
   
begin
   Put_Instruction(Load, 0, 0);
   Put_Instruction(Store, 0, 0);
   Put_Instruction(Clear, 0, 0);
   Put_Instruction(Move, 0, 0);
end;
