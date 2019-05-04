with Ada.Iterator_Interfaces;

package Fibonacci is
   
   type Fibo_Cur is private;
   
   function Has_Element (Pos : Fibo_Cur) return Boolean;
   
   package Fibo is new Ada.Iterator_Interfaces (Fibo_Cur, Has_Element);
   
   type Fibo_Type is tagged private;
   
   type Fibo_List is tagged record
      --X : Integer;
      null;
   end record 
   with 
     Default_Iterator => Iterate,
     Iterator_Element => Fibo_Type'Class,
     Constant_Indexing => Element_Value;
   
   type Fibo_Forward 
     is new Fibo_Type and Fibo.Forward_Iterator with private;    -- new Fibo_Type and Fibo.Forward_Iterator with null record;
   
   type Fibo_Reversible 
     is new Fibo_Type and Fibo.Reversible_Iterator with private; -- new Fibo_Type and Fibo.Reversible_Iterator with null record;
   
   
   function Element (C : Fibo_Cur) return Natural;
   function Element (V : Fibo_Type'Class) return Natural;
   
   function Element_Value
     (C : Fibo_List; P : Fibo_Cur) return Fibo_Type'Class;
   
   function Iterate 
     (C : Fibo_List) return Fibo.Forward_Iterator'Class;
   
   overriding
   function First (O : Fibo_Forward) return Fibo_Cur;
   
   overriding
   function Next (O   : Fibo_Forward;
   Pos : Fibo_Cur)
		 return Fibo_Cur;
   
   overriding
   function First (O : Fibo_Reversible) return Fibo_Cur;
   
   overriding
   function Next (O   : Fibo_Reversible;
		  Pos : Fibo_Cur)
		 return Fibo_Cur;
   
   
   overriding
   function Last (O : Fibo_Reversible) return Fibo_Cur;
   
   overriding
   function Previous (O   : Fibo_Reversible;
		      Pos : Fibo_Cur) 
		     return Fibo_Cur;
   
   
   procedure Put_Line (O : Fibo_Type'Class);
   
private

   type Fibo_Cur is
      record
	 Cur  : Natural := 1;
	 Prev : Natural := 1;
      end record;
   
   type Fibo_Type is tagged record
      Value  : Natural;
      Cursor : Fibo_Cur; 
   end record;
   
   type Fibo_Forward is new Fibo_Type and Fibo.Forward_Iterator with null record;
   type Fibo_Reversible is new Fibo_Type and Fibo.Reversible_Iterator with null record;

end Fibonacci;
