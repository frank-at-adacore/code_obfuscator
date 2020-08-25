with Ada.Strings.Wide_Wide_Unbounded;
package body Wide_Wide_Unbounded with
   SPARK_Mode
is

   package Aswwu renames Ada.Strings.Wide_Wide_Unbounded;

   function To_Unbounded_Wide_Wide_String
     (Source : Wide_Wide_String)
      return Unbounded_Wide_Wide_String is
     ((Uwws => Aswwu.To_Unbounded_Wide_Wide_String (Source)));

   function To_Wide_Wide_String
     (Source : Unbounded_Wide_Wide_String)
      return Wide_Wide_String is
      ret_val : constant wide_wide_string := Aswwu.To_Wide_Wide_String (Source.Uwws);
   begin
      pragma assume ( ret_val'length = length(source) );
      return ret_val;
   end To_Wide_Wide_String;

   function Length
     (Source : Unbounded_Wide_Wide_String)
      return Natural is (To_Wide_Wide_String (Source)'Length);

   function "<"
     (Left  : Unbounded_Wide_Wide_String;
      Right : Unbounded_Wide_Wide_String)
      return Boolean is (Aswwu."<" (Left.Uwws, Right.Uwws));

   procedure Append
     (Source   : in out Unbounded_Wide_Wide_String;
      New_Item :        Unbounded_Wide_Wide_String) is
      Original_Length : Natural := Length (Source) with
         Ghost;
   begin
      Aswwu.Append (Source.Uwws, New_Item.Uwws);
      pragma Assume (Length (Source) = Original_Length + Length (New_Item));
   end Append;

   procedure Append
     (Source   : in out Unbounded_Wide_Wide_String;
      New_Item :        Wide_Wide_Character) is
      Original_Length : Natural := Length (Source) with
         Ghost;
   begin
      Aswwu.Append (Source.Uwws, New_Item);
      pragma Assume (Length (Source) = Original_Length + 1);
   end Append;

end Wide_Wide_Unbounded;
