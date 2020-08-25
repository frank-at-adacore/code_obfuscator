with Libadalang.Analysis;

with Langkit_Support.Slocs;

private package Obfuscate.Locations with
   SPARK_Mode
is
   package Lal renames Libadalang.Analysis;

   procedure Add_Reference
     (Node           : Lal.Ada_Node'Class;
      Qualified_Name : Wide_Wide_String) with pre => Qualified_Name'length <= max_qualified_name_length;

      -- null string means key was not found
   function Value
     (Filename   : String;
      Sloc_Range : Langkit_Support.Slocs.Source_Location_Range)
      return Wide_Wide_String with post => value'result'length <= max_qualified_name_length;

      function Map_Size return Natural;

   procedure Dump;

end Obfuscate.Locations;
