--
-- See protect.adb
--
-- This test proves that multiple readers on a protected
-- object are possible when using function: in the example
-- protect.adb the problem was the delay.
--
-- (Hence the name One_A_Time must refer to the writers)
--
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Calendar; use Ada.Calendar;

procedure Protect2 is
   
   procedure Put_Time_Diff (D : in Time) is
      T : Duration := Clock - D;
   begin
      Put (Duration'Image (T));
   end Put_Time_Diff;
   
   
   protected One_A_Time is
      function Read (Id : String) return Integer;
      procedure Write (X : Integer);
   private
      V : Integer := 2;
      T : Time := Clock;
   end One_A_Time;
   
   protected body One_A_Time is
      
      function Read (Id : String) return Integer is
      begin
	 Put_Time_Diff (T); Put_Line (" OAT reads for " & Id);
	 return V;
      end Read;
      
      procedure Write (X : Integer) is
      begin
	 Put_Time_Diff (T); Put_Line (" OAT starts writing...");	 
	 delay 5.0;
	 V := X;
	 Put_Time_Diff (T); Put_Line (" OAT ended writing...");
      end Write;
      
   end One_A_Time;
   
   task Reader1;
   task Reader2;
   
   task body Reader1 is
      I : Integer := One_A_Time.Read ("R1");
   begin
      loop
	 exit when I = 0;
	 I := One_A_Time.Read ("R1");
	 delay 0.5;
      end loop;
   end Reader1;
   
   task body Reader2 is
      I : Integer := One_A_Time.Read ("R2");
   begin
      loop
	 exit when I = 0;
	 I := One_A_Time.Read ("R2");
	 delay 0.5;
      end loop;      
   end Reader2;
   
   T : Time := Clock;
begin
   -- The main writes
   Put_Time_Diff (T); Put_Line (" ET writes 1...");
   One_A_Time.Write (1);
   Put_Time_Diff (T); Put_Line (" ET has written 1");
   delay 5.0;
   Put_Time_Diff (T); Put_Line (" ET writes 0...");
   One_A_Time.Write (0);
   Put_Time_Diff (T); Put_Line (" ET has written 0");   
end;
