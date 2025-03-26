with Ada.Containers.Ordered_Maps;
with Ada.Strings.Unbounded;
with Langkit_Support.Slocs;
with Libadalang.Analysis;
with Wide_Wide_Unbounded;

package Obfuscate.Locations is
   package Lal renames Libadalang.Analysis;

   procedure Add_Reference
     (Node           : Lal.Ada_Node'Class;
      Qualified_Name : Wide_Wide_String);

   -- null string means key was not found
   function Value
     (Filename   : String;
      Sloc_Range : Langkit_Support.Slocs.Source_Location_Range)
      return Wide_Wide_String;

   function Map_Size return Natural;

private

   use type Wide_Wide_Unbounded.Unbounded_Wide_Wide_String;

   type Key_T is record
      Filename   : Ada.Strings.Unbounded.Unbounded_String;
      Sloc_Range : Langkit_Support.Slocs.Source_Location_Range;
   end record;

   function "<"
     (Left  : Key_T;
      Right : Key_T)
      return Boolean;
      --  Compare keys for map ordering

   package Location_Map is new Ada.Containers.Ordered_Maps
     (Key_Type     => Key_T,
      Element_Type => Wide_Wide_Unbounded.Unbounded_Wide_Wide_String);

   Map : Location_Map.Map;

end Obfuscate.Locations;
