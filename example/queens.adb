with Ada.Text_IO; use Ada.Text_IO;
package body Queens is

   Board : array (1 .. 20) of Integer;

   procedure Put (Value : Integer) is
      Image : constant String := Integer'Image (Value);
   begin
      if Value < 0
      then
         Put (Value);
      else
         Put (Image
              (Image'First + 1 .. Image'Last));
      end if;
   end Put;

   -- Print the current solution (keeping track of the number of solutions
   -- we've printed)
   Solution : Natural := 0;
   procedure Print (Board_Size : Integer) is
   begin
      Solution := Solution + 1;
      Put_Line ("Solution number" & Integer'Image (Solution));
      -- Column titles
      Put ("  ");
      for I in 1 .. Board_Size
      loop
         Put (I);
      end loop;
      New_Line;
      -- Print each row
      for I in 1 .. Board_Size
      loop
         Put (I);
         Put (" ");
         -- Print each column
         for J in 1 .. Board_Size
         loop
            -- If the board says we have a queen a this column/row
            if Board (I) = J
            then
               -- print queen indication
               Put ("Q");
            else
               -- print empty column indication
               Put ("-");
            end if;
         end loop;
         New_Line;

      end loop;
   end Print;

   -- If this row/column is already covered by a previously placed queen,
   -- return false; otherwise return true
   function Place
     (Row    : Integer;
      Column : Integer)
      return Boolean is
   begin
      for I in 1 .. Row - 1
      loop
         -- look for a column conflict
         if Board (I) = Column
         then
            return False;
            -- look for a row conflict
         elsif abs (Board (I) - Column) = abs (I - Row)
         then
            return False;
         end if;
      end loop;
      -- no conflicts
      return True;
   end Place;

   -- place queen for Row in an NxN boar
   procedure Queen
     (Row        : Integer;
      Board_Size : Integer) is
   begin
      for Column in 1 .. Board_Size
      loop
         if Place (Row, Column)
         then

            Board (Row) := Column;
            if Row = Board_Size
            then
               Print (Board_Size);
            else
               Queen (Row + 1, Board_Size);
            end if;
         end if;
      end loop;
   end Queen;

end Queens;
