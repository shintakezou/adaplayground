with Measure_Units; use Measure_Units;
with Ada.Text_IO; use Ada.Text_IO;

procedure Aviotest0 is
   Avg_Speed   : Kn := 350.0;
   Travel_Time : Duration := 2.0 * 3600.0; -- two hours
begin
   Put_Line ("avg speed (kt): " & Avg_Speed);
   Put_Line ("avg speed (m/s): " & To_Mps (Avg_Speed));
   Put_Line ("flight duration (s): " & Duration'Image (Travel_Time));
   Put_Line ("distance (NM): " & (Avg_Speed * Travel_Time));
   Put_Line ("distance (m): " & To_Meters (Avg_Speed * Travel_Time));
end Aviotest0;
