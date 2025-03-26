with Ada.Containers.Ordered_Maps;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;

package Obfuscate.Names is

   procedure Add_Name (Qualified_Name : Wide_Wide_String);
   --  Add obfuscated version of Qualified_Name to the map

   function Get_Name
     (Qualified_Name : Wide_Wide_String)
      return Wide_Wide_String;
   --  Get obfuscated name for Qualified_Name

   function Obfuscated_Text
     (Text : Wide_Wide_String)
      return Wide_Wide_String;
   -- Return obfuscated version of Text

   function Map_Size return Natural;
   --  Size of name map

private

   package Name_Map is new Ada.Containers.Ordered_Maps
     (Key_Type     => Unbounded_Wide_Wide_String,
      Element_Type => Unbounded_Wide_Wide_String);
   use type Name_Map.Cursor;

   Map : Name_Map.Map;

end Obfuscate.Names;
