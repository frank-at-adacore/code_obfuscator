with Ada.Containers.Ordered_Maps;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;

package Obfuscate.Names is

   function Last_Dot
     (Str : Wide_Wide_String)
      return Integer;

   function Name_Part
     (Str : Wide_Wide_String)
      return Wide_Wide_String;

   procedure Add_Name (Qualified_Name : Wide_Wide_String);

   function Get_Name
     (Qualified_Name : Wide_Wide_String)
      return Wide_Wide_String;

   -- Used to obfuscate strings and comments
   function Obfuscated_Text
     (Text : Wide_Wide_String)
      return Wide_Wide_String;

   function Map_Size return Natural;

private

   package Name_Map is new Ada.Containers.Ordered_Maps
     (Key_Type     => Unbounded_Wide_Wide_String,
      Element_Type => Unbounded_Wide_Wide_String);
   use type Name_Map.Cursor;

   Map : Name_Map.Map;

end Obfuscate.Names;
