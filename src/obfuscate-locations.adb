with Ada.Containers;
with Ada.Containers.Formal_Ordered_Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Wide_Wide_Unbounded; use Wide_Wide_Unbounded;
with Ada.Text_IO;
with Ada.Wide_Wide_Text_IO;

with Langkit_Support.Slocs;

use type Ada.Containers.Count_Type;
use type Langkit_Support.Slocs.Line_Number;
use type Langkit_Support.Slocs.Column_Number;

with Debug;

package body Obfuscate.Locations with
   SPARK_Mode
is

   type Key_T is record
      Filename   : Unbounded_String;
      Sloc_Range : Langkit_Support.Slocs.Source_Location_Range;
   end record;
   function "<"
     (Left  : Key_T;
      Right : Key_T)
      return Boolean;

   Max_Size : constant := 10_000;

   package Location_Map is new Ada.Containers.Formal_Ordered_Maps
     (Key_Type => Key_T, Element_Type => Unbounded_Wide_Wide_String);
   use type Location_Map.Cursor;

   Map : Location_Map.Map (Max_Size);

   procedure Add_Reference
     (Node           : Lal.Ada_Node'Class;
      Qualified_Name : Wide_Wide_String) is
      To_Add : Key_T;
      Cursor : Location_Map.Cursor;
   begin
      To_Add.Filename   := To_Unbounded_String (Node.Unit.Get_Filename);
      To_Add.Sloc_Range := Node.Sloc_Range;
      Cursor            := Location_Map.Find (Map, To_Add);
      if Cursor = Location_Map.No_Element
        and then Location_Map.Length (Map) < Max_Size
      then
         Location_Map.Insert
           (Container => Map,
            Key       => To_Add,
            New_Item  => To_Unbounded_Wide_Wide_String (Qualified_Name));
         Debug.Print ("Add_reference " & Qualified_Name, False);
         Debug.Print (" for ", Node);
      end if;
   end Add_Reference;

   function Value
     (Filename   : String;
      Sloc_Range : Langkit_Support.Slocs.Source_Location_Range)
      return Wide_Wide_String is
      To_Find : Key_T :=
        (Filename => To_Unbounded_String (Filename), Sloc_Range => Sloc_Range);
      Cursor : Location_Map.Cursor;
   begin
      Cursor := Location_Map.Find (Map, To_Find);
      if Cursor /= Location_Map.No_Element
      then
         declare
            Element : Unbounded_Wide_Wide_String :=
              Location_Map.Element (Map, Cursor);
         begin
            if Length (Element) <= Max_Qualified_Name_Length
            then
               return To_Wide_Wide_String (Location_Map.Element (Map, Cursor));
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
      if Left.Filename < Right.Filename
      then
         return True;
      elsif Left.Filename > Right.Filename
      then
         return False;
      elsif Left.Sloc_Range.Start_Line < Right.Sloc_Range.Start_Line
      then
         return True;
      elsif Left.Sloc_Range.Start_Line > Right.Sloc_Range.Start_Line
      then
         return False;
      elsif Left.Sloc_Range.Start_Column < Right.Sloc_Range.Start_Column
      then
         return True;
      elsif Left.Sloc_Range.Start_Column > Right.Sloc_Range.Start_Column
      then
         return False;

      elsif Left.Sloc_Range.End_Line < Right.Sloc_Range.End_Line
      then
         return True;
      elsif Left.Sloc_Range.End_Line > Right.Sloc_Range.End_Line
      then
         return False;
      elsif Left.Sloc_Range.End_Column < Right.Sloc_Range.End_Column
      then
         return True;
      elsif Left.Sloc_Range.End_Column > Right.Sloc_Range.End_Column
      then
         return False;
      else
         return False;

      end if;
   end "<";

   function Map_Size return Natural is (Natural (Location_Map.Length (Map)));

   -- Debug procedure - turn SPARK off
   procedure Dump with
      SPARK_Mode => Off
   is
      Cursor  : Location_Map.Cursor;
      Key     : Key_T;
      Element : Unbounded_Wide_Wide_String;
   begin
      Ada.Text_IO.Put_Line ("=== Locations ===");
      Cursor := Location_Map.First (Map);
      while Cursor /= Location_Map.No_Element
      loop
         Key     := Location_Map.Key (Map, Cursor);
         Element := Location_Map.Element
             (Container => Map,
              Position  => Cursor);
         Ada.Text_IO.Put (To_String (Key.Filename) & " ");
         Ada.Wide_Wide_Text_IO.Put (Debug.Image (Key.Sloc_Range) & " => ");
         Ada.Wide_Wide_Text_IO.Put_Line (To_Wide_Wide_String (Element));
         Cursor := Location_Map.Next (Map, Cursor);
      end loop;

   end Dump;

end Obfuscate.Locations;
