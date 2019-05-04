with Ada.Text_IO;

package body Fibonacci is
   
   function Has_Element (Pos : Fibo_Cur) return Boolean is
     (Pos.Cur <= Natural'Last - Pos.Prev and Pos.Prev > 0);
   
   overriding
   function First (O : Fibo_Forward) return Fibo_Cur is
     ((Cur => 1, Prev => 1));
   
   overriding
   function Next (O   : Fibo_Forward;
		  Pos : Fibo_Cur)
		 return Fibo_Cur
     is ((Cur => Pos.Cur + Pos.Prev, 
	  Prev => Pos.Cur));
   
   overriding
   function First (O : Fibo_Reversible) return Fibo_Cur is
     ((Cur => 1, Prev => 1));
   
   overriding
   function Next (O   : Fibo_Reversible;
		  Pos : Fibo_Cur)
		 return Fibo_Cur
     is ((Cur => Pos.Cur + Pos.Prev, 
	  Prev => Pos.Cur));
   
   
   overriding
   function Last (O : Fibo_Reversible) return Fibo_Cur is
     ((Cur => 1134903170, Prev => 701408733));
   
   overriding
   function Previous (O   : Fibo_Reversible;
		      Pos : Fibo_Cur) 
		     return Fibo_Cur
     is ((Cur => Pos.Prev,
	  Prev => Pos.Cur - Pos.Prev));
   
   
   
   function Element (C : Fibo_Cur) return Natural is (C.Cur);   
   function Element (V : Fibo_Type'Class) return Natural is (V.Value);
   
   
   function Element_Value
     (C : Fibo_List; P : Fibo_Cur) return Fibo_Type'Class 
     is (Fibo_Type'(Value => P.Cur,
		    Cursor => P));
   
   function Iterate 
     (C : Fibo_List) return Fibo.Forward_Iterator'Class
     is (Fibo_Forward'(Value => 1,
		       Cursor => (Cur => 1,
				  Prev => 1)));
   
   
   procedure Put_Line
     (O : Fibo_Type'Class) is
   begin
     Ada.Text_IO.Put_Line (Natural'Image (Element (O)));
   end;
   
end Fibonacci;
