-- SPARK-compliant version of Ada.Strings.Wide_Wide_Unbounded
with Ada.Strings.Wide_Wide_Unbounded;
package Wide_Wide_Unbounded is

   type Unbounded_Wide_Wide_String is private;

   Null_Unbounded_Wide_Wide_String : constant Unbounded_Wide_Wide_String;

   function To_Unbounded_Wide_Wide_String
     (Source : Wide_Wide_String)
      return Unbounded_Wide_Wide_String;

   function To_Wide_Wide_String
     (Source : Unbounded_Wide_Wide_String)
      return Wide_Wide_String;

   function Length
     (Source : Unbounded_Wide_Wide_String)
      return Natural;

   function "<"
     (Left  : Unbounded_Wide_Wide_String;
      Right : Unbounded_Wide_Wide_String)
      return Boolean;

   procedure Append
     (Source   : in out Unbounded_Wide_Wide_String;
      New_Item :        Unbounded_Wide_Wide_String);

   procedure Append
     (Source   : in out Unbounded_Wide_Wide_String;
      New_Item :        Wide_Wide_Character);

   function From_String
     (Source : String)
      return Unbounded_Wide_Wide_String;

private

   type Unbounded_Wide_Wide_String is record
      Uwws : Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String;
   end record;

   Null_Unbounded_Wide_Wide_String : constant Unbounded_Wide_Wide_String :=
     (Uwws => Ada.Strings.Wide_Wide_Unbounded.Null_Unbounded_Wide_Wide_String);

end Wide_Wide_Unbounded;
