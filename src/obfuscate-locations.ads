with Ada.Containers.Ordered_Maps;
with Ada.Strings.Unbounded;
with Ada.Strings.Wide_Wide_Unbounded;
with Langkit_Support.Slocs;
with Libadalang.Analysis;

package Obfuscate.Locations is
   package Lal renames Libadalang.Analysis;

   use Ada.Strings.Unbounded;
   use Ada.Strings.Wide_Wide_Unbounded;

   procedure Add_Reference
     (Node           : Lal.Ada_Node'Class;
      Qualified_Name : Wide_Wide_String);
   --  Add Qualified_Name to the map for key Node

   function Value
     (Filename   : String;
      Sloc_Range : Langkit_Support.Slocs.Source_Location_Range)
      return Wide_Wide_String;
      --  Return the content from Sloc_Range in Filename.
      --  If not found, return an empty string

private

   type Key_T is record
      Filename   : Unbounded_String;
      Sloc_Range : Langkit_Support.Slocs.Source_Location_Range;
   end record;

   function "<"
     (Left  : Key_T;
      Right : Key_T)
      return Boolean;
      --  Compare keys for map ordering

   package Location_Map is new Ada.Containers.Ordered_Maps
     (Key_Type     => Key_T,
      Element_Type => Unbounded_Wide_Wide_String);

   Map : Location_Map.Map;

end Obfuscate.Locations;
