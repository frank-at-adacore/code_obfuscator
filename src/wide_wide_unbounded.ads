-- SPARK-compliant version of Ada.Strings.Wide_Wide_Unbounded
with Ada.Strings.Wide_Wide_Unbounded;
package Wide_Wide_Unbounded with
   SPARK_Mode
is

   type Unbounded_Wide_Wide_String is private;

   Null_Unbounded_Wide_Wide_String : constant Unbounded_Wide_Wide_String;

   function To_Unbounded_Wide_Wide_String
     (Source : Wide_Wide_String)
      return Unbounded_Wide_Wide_String with
      Global => null,
      Post   => Length (To_Unbounded_Wide_Wide_String'Result) = Source'Length;

   function To_Wide_Wide_String
     (Source : Unbounded_Wide_Wide_String)
      return Wide_Wide_String with
      Global => null,
      Post   => To_Wide_Wide_String'Result'Length = Length (Source);

   function Length
     (Source : Unbounded_Wide_Wide_String)
      return Natural with
      Post   => Length'Result = To_Wide_Wide_String (Source)'Length,
      Global => null;

   function "<"
     (Left  : Unbounded_Wide_Wide_String;
      Right : Unbounded_Wide_Wide_String)
      return Boolean with
      Global => null;

   procedure Append
     (Source   : in out Unbounded_Wide_Wide_String;
      New_Item :        Unbounded_Wide_Wide_String) with
      Pre    => Length (Source) <= Natural'Last - Length (New_Item),
      Post   => Length (Source) = Length (Source'Old) + Length (New_Item),
      Global => null;

   procedure Append
     (Source   : in out Unbounded_Wide_Wide_String;
      New_Item :        Wide_Wide_Character) with
      Pre    => Length (Source) < Natural'Last,
      Post   => Length (Source) = Length (Source'Old) + 1,
      Global => null;

private

   type Unbounded_Wide_Wide_String is record
      Uwws : Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String;
   end record;

   Null_Unbounded_Wide_Wide_String : constant Unbounded_Wide_Wide_String :=
     (Uwws => Ada.Strings.Wide_Wide_Unbounded.Null_Unbounded_Wide_Wide_String);

end Wide_Wide_Unbounded;
