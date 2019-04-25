--
-- One writer, one reader at a time, using
-- protected object.
--
-- Output can be intermixed (can another
-- protected object be used to streamline output?)
--
-- Each task has its reference start time; shouldn't it be
-- the same for all? (Another protected object, again;
-- anyway the differences should be small)
--
-------------------
-- OUTPUT EXAMPLE
--
--  0.000003000 R1 reading...
--  0.000371000 OAT starts reading...
--  0.000123000 ET writes 1...
--  0.000031000 R2 works for 2s
--  2.000471000 R2 reading...
--  3.000517000 OAT ended read
--  3.000430000 3.000578000 OAT starts writing...
--  ...R1 read 0
--  8.000836000 OAT ended writing...
--  8.000850000 ET has written 1
--  8.000916000 OAT starts reading...
--  11.001029000 OAT ended read
--  11.000936000 ...R2 read 1
--
--  R1 reads at 0, end reading at 3
--  ET starts writing at 0, but OAT is already reading,
--     so it really begins writing at 3, hence ending at 8
--  R2 starts reading at 2, and ends at 11
--
--        ..........1.
--        0....5....0.
--  R1    RRR  
--  R2    --rrrrrrRRR
--  ET    wwwWWWWW____
--
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Calendar; use Ada.Calendar;

procedure Protect is
   
   procedure Put_Time_Diff (D : in Time) is
      T : Duration := Clock - D;
   begin
      Put (Duration'Image (T));
   end Put_Time_Diff;
   
   
   protected One_A_Time is
      function Read return Integer;
      --  Reading takes 3s
      
      procedure Write (X : Integer);
      --  Writing takes 5s   
   private
      V : Integer := 0;
      T : Time := Clock;
   end One_A_Time;
   
   
   
   protected body One_A_Time is
      
      function Read return Integer is
      begin
	 Put_Time_Diff (T); Put_Line (" OAT starts reading...");
	 delay 3.0;
	 Put_Time_Diff (T); Put_Line (" OAT ended read");
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
      I : Integer;
      T : Time := Clock;
   begin
      Put_Time_Diff (T);
      Put_Line (" R1 reading...");
      I := One_A_Time.Read;
      Put_Time_Diff (T);
      Put_Line (" ...R1 read" & Integer'Image (I));
   end Reader1;
   
   task body Reader2 is
      I : Integer;
      T : Time := Clock;
   begin
      Put_Time_Diff (T);
      Put_Line (" R2 works for 2s");
      delay 2.0;
      Put_Time_Diff (T);
      Put_Line (" R2 reading...");
      I := One_A_Time.Read;
      Put_Time_Diff (T);
      Put_Line (" ...R2 read" & Integer'Image (I));
   end Reader2;
   
   T : Time := Clock;
begin
   -- The main writes
   Put_Time_Diff (T);
   Put_Line (" ET writes 1...");
   One_A_Time.Write (1);
   Put_Time_Diff (T);
   Put_Line (" ET has written 1");
end;
