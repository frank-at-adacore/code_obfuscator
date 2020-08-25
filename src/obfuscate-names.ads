private package Obfuscate.Names with
   SPARK_Mode
is

   procedure Add_Name (Qualified_Name : Wide_Wide_String) with
      Pre => Qualified_Name'Length <= Max_Qualified_Name_Length and
      Map_Size < Natural'Last;
   function Get_Name
     (Qualified_Name : Wide_Wide_String)
      return Wide_Wide_String with
      Pre => Qualified_Name'Length <= Max_Qualified_Name_Length;

      -- Used to obfuscate strings and comments
   function Obfuscated_Text
     (Text : Wide_Wide_String)
      return Wide_Wide_String;

   function Map_Size return Natural;

   procedure Dump;

end Obfuscate.Names;
