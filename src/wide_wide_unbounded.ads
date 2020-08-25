-- SPARK-compliant version of Ada.Strings.Wide_Wide_Unbounded
with Ada.Strings.Wide_Wide_Unbounded;
package Wide_Wide_Unbounded with spark_mode is

   type Unbounded_Wide_Wide_String is private;

   Null_Unbounded_Wide_Wide_String : constant Unbounded_Wide_Wide_String;

   function To_Unbounded_Wide_Wide_String
     (Source : Wide_Wide_String)
      return Unbounded_Wide_Wide_String with global => null,
   post => length(To_Unbounded_Wide_Wide_String'result) = source'length;

   function To_Wide_Wide_String
     (Source : Unbounded_Wide_Wide_String)
      return Wide_Wide_String with global => null, post => to_wide_wide_string'result'length = length ( source );

   function Length
     (Source : Unbounded_Wide_Wide_String)
      return Natural with post => length'result = to_wide_wide_String(source)'length, global => null;

   function "<"
     (Left  : Unbounded_Wide_Wide_String;
      Right : Unbounded_Wide_Wide_String)
      return Boolean with global => null;

   procedure Append
     (Source   : in out Unbounded_Wide_Wide_String;
      New_Item :        Unbounded_Wide_Wide_String) with pre => length(source) <= natural'last - length(new_item),
     post => length(source) = length(source'old) + length(new_item), global => null;

   procedure Append
     (Source   : in out Unbounded_Wide_Wide_String;
      New_Item :        Wide_Wide_Character) with pre => length(source) < natural'last,
     post => length(source) = length(source'old) + 1, global => null;

private

   type Unbounded_Wide_Wide_String is record
      Uwws : Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String;
   end record;

   Null_Unbounded_Wide_Wide_String : constant Unbounded_Wide_Wide_String :=
     (Uwws => Ada.Strings.Wide_Wide_Unbounded.Null_Unbounded_Wide_Wide_String);

end Wide_Wide_Unbounded;
