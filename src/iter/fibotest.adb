with Ada.Text_IO; use Ada.Text_IO;
with Fibonacci; use Fibonacci;

procedure Fibotest is   
   C : Fibo_Cur;
   -- T : Fibo_Forward;
   -- R : Fibo_Reversible;
   F : Fibo_List;
begin
   --  C := First (T);
   --  while Has_Element (C) loop
   --     Put_Line (Natural'Image (Element (C)));
   --     C := Next (T, C);
   --  end loop;
   
   --  C := Last (R);
   --  while Has_Element (C) loop
   --     Put_Line (Natural'Image (Element (C)));
   --     C := Previous (R, C);
   --  end loop;
   
   --  for I in F.Iterate loop
   --     Put_Line (Natural'Image (Element (I)));
   --  end loop;
   
   for I of F loop
      Put_Line (I); -- not a Put_Line from Text_IO, though.
   end loop;
   
   -- missing parts...
   --  for I of reverse F loop
   --     Put_Line (I);      
   --  end loop;
   
end Fibotest;
