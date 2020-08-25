private package Obfuscate.Names with
   SPARK_Mode
is

   procedure Add_Name (Qualified_Name : Wide_Wide_String) with
      Pre => Qualified_Name'Length <= max_qualified_name_length and Map_Size < Natural'Last;
   function Get_Name
     (Qualified_Name : Wide_Wide_String)
      return Wide_Wide_String with pre => Qualified_Name'length <= max_qualified_name_length;

   function Map_Size return Natural;

   procedure Dump;

end Obfuscate.Names;
