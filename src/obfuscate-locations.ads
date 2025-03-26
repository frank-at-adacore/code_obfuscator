with Libadalang.Analysis;

with Langkit_Support.Slocs;

private package Obfuscate.Locations is
   package Lal renames Libadalang.Analysis;

   procedure Add_Reference
     (Node           : Lal.Ada_Node'Class;
      Qualified_Name : Wide_Wide_String) with
     Pre => Qualified_Name'Length <= Max_Qualified_Name_Length;

   -- null string means key was not found
   function Value
     (Filename   : String;
      Sloc_Range : Langkit_Support.Slocs.Source_Location_Range)
      return Wide_Wide_String with
     Post => Value'Result'Length <= Max_Qualified_Name_Length;

   function Map_Size return Natural;

   procedure Dump;

end Obfuscate.Locations;
