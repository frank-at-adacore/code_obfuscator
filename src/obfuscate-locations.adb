with Ada.Containers;

package body Obfuscate.Locations is

   use type Langkit_Support.Slocs.Line_Number;
   use type Langkit_Support.Slocs.Column_Number;
   use type Location_Map.Cursor;

   procedure Add_Reference
     (Node           : Lal.Ada_Node'Class;
      Qualified_Name : Wide_Wide_String) is
      To_Add : Key_T;
      Cursor : Location_Map.Cursor;
   begin
      To_Add.Filename   := To_Unbounded_String (Node.Unit.Get_Filename);
      To_Add.Sloc_Range := Node.Sloc_Range;
      Cursor            := Location_Map.Find (Map, To_Add);
      if Cursor = Location_Map.No_Element then
         Location_Map.Insert
           (Container => Map,
            Key       => To_Add,
            New_Item  => To_Unbounded_Wide_Wide_String (Qualified_Name));
      end if;
   end Add_Reference;

   function Value
     (Filename   : String;
      Sloc_Range : Langkit_Support.Slocs.Source_Location_Range)
      return Wide_Wide_String is
      To_Find : constant Key_T :=
        (Filename   => To_Unbounded_String (Filename),
         Sloc_Range => Sloc_Range);
      Cursor  : Location_Map.Cursor;
   begin
      Cursor := Location_Map.Find (Map, To_Find);
      if Cursor /= Location_Map.No_Element then
         declare
            Element : constant Unbounded_Wide_Wide_String :=
              Location_Map.Element (Cursor);
         begin
            if Length (Element) <= Max_Qualified_Name_Length then
               return To_Wide_Wide_String (Location_Map.Element (Cursor));
            end if;
         end;
      end if;
      return "";
   end Value;

   function "<"
     (Left  : Key_T;
      Right : Key_T)
      return Boolean is
   begin
      if Left.Filename < Right.Filename then
         return True;
      elsif Left.Filename > Right.Filename then
         return False;
      elsif Left.Sloc_Range.Start_Line < Right.Sloc_Range.Start_Line then
         return True;
      elsif Left.Sloc_Range.Start_Line > Right.Sloc_Range.Start_Line then
         return False;
      elsif Left.Sloc_Range.Start_Column < Right.Sloc_Range.Start_Column then
         return True;
      elsif Left.Sloc_Range.Start_Column > Right.Sloc_Range.Start_Column then
         return False;

      elsif Left.Sloc_Range.End_Line < Right.Sloc_Range.End_Line then
         return True;
      elsif Left.Sloc_Range.End_Line > Right.Sloc_Range.End_Line then
         return False;
      elsif Left.Sloc_Range.End_Column < Right.Sloc_Range.End_Column then
         return True;
      elsif Left.Sloc_Range.End_Column > Right.Sloc_Range.End_Column then
         return False;
      else
         return False;

      end if;
   end "<";

   function Map_Size return Natural is (Natural (Location_Map.Length (Map)));

end Obfuscate.Locations;
