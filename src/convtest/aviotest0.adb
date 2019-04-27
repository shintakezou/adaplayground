with Measure_Units; use Measure_Units;
with Ada.Text_IO; use Ada.Text_IO;

procedure Aviotest0 is
   Avg_Speed   : Kn := 350.0;
   Travel_Time : Duration := 2.0 * 3600.0; -- two hours
   CR          : Climb_Rate := 1500.0;
   Climb_Time  : Duration := 60.0 * 20; -- 2 minutes
   Alt0        : Ft := 50_000.0; -- from here
   Alt1        : Ft := 20_000.0; -- to here
   Sink_Time   : Duration := 60.0 * 10; -- in 10 minutes
begin
   Put_Line ("avg speed (kt): " & Avg_Speed);
   Put_Line ("avg speed (m/s): " & To_Mps (Avg_Speed));
   Put_Line ("flight duration (s): " & Duration'Image (Travel_Time));
   Put_Line ("distance (NM): " & (Avg_Speed * Travel_Time));
   Put_Line ("distance (m): " & To_Meters (Avg_Speed * Travel_Time));
   Put_Line ("climb rate (ft/min): " & CR);
   Put_Line ("alt (ft) after " 
	       & Duration'Image (Climb_Time)
	       & " s: " 
	       & (CR * Climb_Time));
   Put_Line ("change alt rate (ft/m): " & ((Alt1 - Alt0) / Sink_Time));
end Aviotest0;
