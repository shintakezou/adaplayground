--
-- In this example two different tasks exchange values
-- through a third task. Delays are used to assure the
-- get isn't executed by the same task that did the put.
--
with Ada.Text_IO; use Ada.Text_IO;
procedure Task_Queue is
   
   -- cf. Barnes, J., "Programming in Ada 2012" (Chapter 20)
   task Buffer is
      entry Put (X : in Integer);
      entry Get (X : out Integer);
   end;
   
   task body Buffer is
      V: Integer;
   begin
      loop
	 accept Put (X : in Integer) do
	    Put_Line ("Requested put: " & Integer'Image (X));
	    V := X;
	 end Put;
	 if V = 0 then
	    exit;
	 end if;
	 delay 5.0; -- give time to other tasks
	 accept Get (X : out Integer) do
	    X := V;
	    Put_Line ("Requested get: " & Integer'Image (X));	    
	 end Get;
      end loop;
   end;
   
   task E1;
   task E2;
   
   task body E1 is
      P : Integer;
   begin
      Put_Line ("E1 puts 11...");
      Buffer.Put (11);
      delay 10.0;
      Put_Line ("E1 getting...");
      Buffer.Get (P);
      Put_Line ("...E1 got " & Integer'Image (P));
   end;
   
   task body E2 is
      P : Integer;
   begin
      delay 3.0;
      -- a Put here would freeze because Buffer is executing the delay
      -- and then expecting a Get. Hence, just get!
      Buffer.Get (P);
      Put_Line ("E2 gets " & Integer'Image (P));
      Put_Line ("E2 puts 12...");
      Buffer.Put (12);
      Put_Line ("...E2 done");
   end;
   
begin
   Put_Line ("All set");
   delay 16.0;
   Buffer.Put (0);
   Put_Line ("Stuck?");   
end;
