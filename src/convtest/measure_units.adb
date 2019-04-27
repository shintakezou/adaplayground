package body Measure_Units is
   
   function To_Mps (V : Kn) return Mps is
     (Mps (Float (V) * Mps_per_Knot));
   
   function To_Meters (V : NM) return Meter is
     (Meter (Float (V) * Meters_per_Nautical_Mile));
   
   function To_Meters (V : Ft) return Meter is
     (Meter (Float (V) * Meters_per_Foot));
   
   function "*"
     (Speed : Kn; 
      T     : Duration) return NM is
     (NM (Float (Speed) * Float (T) / 3600.0));
   
   function "*"
     (CR : Climb_Rate;
      T  : Duration) return Ft is
     (Ft (Float (CR) * Float (T) / 60.0));
   
   function "/"
     (D : Ft;
      T : Duration) return Climb_Rate is
     (Climb_Rate (60.0 * Float (D) / Float (T)));
      
end Measure_Units;
