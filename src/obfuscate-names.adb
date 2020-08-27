with Wide_Wide_Unbounded; use Wide_Wide_Unbounded;
with Ada.Containers;
with Ada.Containers.Formal_Ordered_Maps;
with Ada.Text_IO;
with Ada.Wide_Wide_Text_IO;

with Command_Line;
with Obfuscate.Names.Random;

with Debug;

use type Ada.Containers.Count_Type;

package body Obfuscate.Names with
   SPARK_Mode
is

   Max_Size : constant := 10_000;

   package Name_Map is new Ada.Containers.Formal_Ordered_Maps
     (Key_Type     => Unbounded_Wide_Wide_String,
      Element_Type => Unbounded_Wide_Wide_String);
   use type Name_Map.Cursor;

   Map : Name_Map.Map (Max_Size);

   function Last_Dot
     (Str : Wide_Wide_String)
      return Integer is
   begin
      if Str'Length > 0
      then
         for I in reverse Str'Range
         loop
            if Str (I) = '.'
            then
               return I;
            end if;
            pragma Loop_Invariant (for all C of Str
                 (I .. Str'Last) => C /= '.');
         end loop;
      end if;
      return Str'First - 1;
   end Last_Dot;

   function Name_Part
     (Str : Wide_Wide_String)
      return Wide_Wide_String is
      Dot : Integer := Last_Dot (Str);
   begin
      if Dot in Str'Range
      then
         return Str
             (Dot + 1 .. Str'Last);
      else
         return Str;
      end if;
   end Name_Part;

   subtype Base_26_T is Natural range 0 .. 25;
   First_Valid_Character : constant Natural :=
     Wide_Wide_Character'Pos (Wide_Wide_Character'('A'));

   function Base_26_To_Char
     (Counter : Base_26_T)
      return Wide_Wide_Character is
     (Wide_Wide_Character'Val (First_Valid_Character + Counter));

   -- If RIGHT is too long, we will just use the maximum number of characters
   -- Otherwise, we will convert LEFT to a character and return in prepended
   -- to RIGHT
   function Combine
     (Left  : Base_26_T;
      Right : Wide_Wide_String)
      return Wide_Wide_String with
      Post => Combine'Result'Length <= Max_Qualified_Name_Length;
   function Combine
     (Left  : Base_26_T;
      Right : Wide_Wide_String)
      return Wide_Wide_String is
   begin
      if Right'Length >= Max_Qualified_Name_Length
      then
         return Right
             (Right'First .. Right'First - 1 + Max_Qualified_Name_Length);
      else
         return Base_26_To_Char (Left) & Right;
      end if;
   end Combine;

   function Combine
     (Left  : Base_26_T;
      Right : Unbounded_Wide_Wide_String)
      return Unbounded_Wide_Wide_String with
      Pre  => Length (Right) <= Max_Qualified_Name_Length,
      Post => Length (Combine'Result) <= Max_Qualified_Name_Length;

   function Combine
     (Left  : Base_26_T;
      Right : Unbounded_Wide_Wide_String)
      return Unbounded_Wide_Wide_String is
      Ret_Val : constant Wide_Wide_String :=
        Combine (Left, To_Wide_Wide_String (Right));
   begin
      return To_Unbounded_Wide_Wide_String (Ret_Val);
   end Combine;

   function Str_To_Base_26
     (Counter : Natural)
      return Unbounded_Wide_Wide_String with
      Post => Length (Str_To_Base_26'Result) <= Max_Qualified_Name_Length;
   function Str_To_Base_26
     (Counter : Natural)
      return Unbounded_Wide_Wide_String is
      Number     : Natural := Counter;
      New_Number : Natural;
      Ret_Val    : Unbounded_Wide_Wide_String;
   begin
      Ret_Val := To_Unbounded_Wide_Wide_String ("");
      while Number >= 26
      loop
         New_Number := Number / 26;
         pragma Loop_Invariant (Number - (New_Number * 26) in Base_26_T);
         pragma Loop_Invariant (Length (Ret_Val) <= Max_Qualified_Name_Length);
         Ret_Val := Combine (Number - (New_Number * 26), Ret_Val);
         Number  := New_Number;
      end loop;
      Ret_Val := Combine (Number, Ret_Val);
      return Ret_Val;
   end Str_To_Base_26;

   function Pad_Length
     (Actual_Length : Natural)
      return Natural is
     (if Command_Line.Option (Command_Line.Constant_Length) > 0 then
        Command_Line.Option (Command_Line.Constant_Length)
      else Actual_Length);

   function Random_Pad
     (Str : Unbounded_Wide_Wide_String;
      Len : Natural)
      return Unbounded_Wide_Wide_String with
      Pre  => Length (Str) <= Max_Qualified_Name_Length,
      Post => Length (Random_Pad'Result) <= Max_Qualified_Name_Length;
   function Random_Pad
     (Str : Unbounded_Wide_Wide_String;
      Len : Natural)
      return Unbounded_Wide_Wide_String is
      Ret_Val : Unbounded_Wide_Wide_String := Str;
   begin
      for I in 1 .. Pad_Length (Len) - Length (Str)
      loop
         Ret_Val := Combine (Random.Random_Character, Ret_Val);
         pragma Loop_Invariant (Length (Ret_Val) <= Max_Qualified_Name_Length);
      end loop;
      return Ret_Val;
   end Random_Pad;

   procedure Obfuscated_Name
     (Input_Name  :     Wide_Wide_String;
      Output_Name : out Unbounded_Wide_Wide_String) with
      Pre => Map_Size < Natural'Last and
      Input_Name'Length <= Max_Qualified_Name_Length and
      Input_Name'Last < Integer'Last,
      Post => Length (Output_Name) <= Max_Qualified_Name_Length;
   procedure Obfuscated_Name
     (Input_Name  :     Wide_Wide_String;
      Output_Name : out Unbounded_Wide_Wide_String) is
   begin
      Output_Name :=
        Random_Pad (Str_To_Base_26 (Map_Size), Name_Part (Input_Name)'Length);
   end Obfuscated_Name;

   procedure Add_Name (Qualified_Name : Wide_Wide_String) is
      To_Add : constant Unbounded_Wide_Wide_String :=
        To_Unbounded_Wide_Wide_String (Qualified_Name);
      New_Name : Unbounded_Wide_Wide_String;
   begin
      if not Name_Map.Contains (Map, To_Add)
        and then Name_Map.Length (Map) < Max_Size
      then
         Obfuscated_Name (Qualified_Name, New_Name);
         Debug.Print
           ("Add Name: " & Qualified_Name & " as " &
            To_Wide_Wide_String (New_Name));
         Name_Map.Insert
           (Container => Map,
            Key       => To_Add,
            New_Item  => New_Name);
      end if;
   end Add_Name;

   function Get_Name
     (Qualified_Name : Wide_Wide_String)
      return Wide_Wide_String is
      To_Find : constant Unbounded_Wide_Wide_String :=
        To_Unbounded_Wide_Wide_String (Qualified_Name);
      Cursor : Name_Map.Cursor;
   begin
      Cursor := Name_Map.Find (Map, To_Find);
      if Cursor = Name_Map.No_Element
      then
         Debug.Print ("Not found: " & Qualified_Name);
         return "";
      else
         return To_Wide_Wide_String (Name_Map.Element (Map, Cursor));
      end if;
   end Get_Name;

   function Map_Size return Natural is (Natural (Name_Map.Length (Map)));

   function Obfuscated_Text
     (Text : Wide_Wide_String)
      return Wide_Wide_String is
      Ret_Val : Wide_Wide_String (1 .. Text'Length) := Text;
   begin
      for C of Ret_Val
      loop
         if C >= '0' and C <= 'z'
         then
            C := Base_26_To_Char (Random.Random_Character);
         end if;
      end loop;
      return Ret_Val;
   end Obfuscated_Text;

   -- Debug procedure - turn SPARK off
   procedure Dump with
      SPARK_Mode => Off
   is
      Cursor  : Name_Map.Cursor;
      Element : Unbounded_Wide_Wide_String;
   begin
      Ada.Text_IO.Put_Line ("=== Names ===");

      Cursor := Name_Map.First (Map);
      while Cursor /= Name_Map.No_Element
      loop
         Element := Name_Map.Element
             (Container => Map,
              Position  => Cursor);
         Ada.Wide_Wide_Text_IO.Put_Line
           (To_Wide_Wide_String (Name_Map.Key (Map, Cursor)) & ": " &
            To_Wide_Wide_String (Element));
         Cursor := Name_Map.Next (Map, Cursor);
      end loop;
   end Dump;

end Obfuscate.Names;
