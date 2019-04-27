with Converters;
--  @summary 
--  Units used in aviation, operations among them and conversion
--  factors.
--
--  @description 
--  Measure_Units provides all you can need to play with
--  units used in aviation, except that this is a lie because
--  this is a test, so you won't find anything really useful
--  here.
--
package Measure_Units is
   
   Meters_per_Nautical_Mile : constant := 1852.0;
   Meters_per_Foot          : constant := 0.3048;
   Mps_per_Knot             : constant := Meters_per_Nautical_Mile/3600.0;
   
   type Kn is new Float;
   --  Knot (NM/h)
   
   subtype Kt is Kn;
   --  kt is used in aviation in place of kn
   
   type NM is new Float;
   --  Nautical Mile (1832 m by definition; it was 1/60 of a degree of
   --  latitude)
   
   type Ft is new Float;
   --  Foot (0.3048 m)
   
   type Climb_Rate is new Float;
   --  Climb rate is misured in ft/min; negative values are for sink
   --  rate.
   
   type Meter is new Float;
   --  Standard meter
   
   type Mps is new Float;
   --  m/s
   
   function To_Mps (V : Kn) return Mps;
   --  Convert kn to m/s.
   
   function To_Meters (V : NM) return Meter;
   --  Convert NM to meters.
   
   function To_Meters (V : Ft) return Meter;
   --  Convert ft to meters.
   
   
   function "*"
     (Speed : Kn; 
      T     : Duration) return NM;
   --  Speed in kn * t gives a distance.
   --  @param Speed  Speed expressed in knots.
   --  @param T      Time in seconds.
   --  @return NM done in the given time.
   
   
   function "*"
     (CR : Climb_Rate;
      T  : Duration) return Ft;
   --  Climb rate (ft/min) * t gives an altitude (distance).
   --  @param CR  Climb rate.
   --  @param T   Time in seconds.
   
   
   function To_String is new Converters.To_String (Kn);
   function To_String is new Converters.To_String (Ft);
   function To_String is new Converters.To_String (NM);
   function To_String is new Converters.To_String (Mps);   
   function To_String is new Converters.To_String (Meter);      
   
   function "&" is new Converters.Concat (Kn, To_String);
   function "&" is new Converters.Concat (Ft, To_String);
   function "&" is new Converters.Concat (NM, To_String);
   function "&" is new Converters.Concat (Mps, To_String);
   function "&" is new Converters.Concat (Meter, To_String);   
   
end Measure_Units;
