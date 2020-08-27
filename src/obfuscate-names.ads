private package Obfuscate.Names with
   SPARK_Mode
is

   function Last_Dot
     (Str : Wide_Wide_String)
      return Integer with
      Pre  => Str'First > Integer'First and Str'Last < Integer'Last,
      Post =>
      ((Last_Dot'Result in Str'Range and then Str (Last_Dot'Result) = '.'
        and then (for all C of Str
           (Last_Dot'Result + 1 .. Str'Last) => C /= '.'))
       or else Last_Dot'Result not in Str'Range);

   function Name_Part
     (Str : Wide_Wide_String)
      return Wide_Wide_String with
      Pre => Str'Length <= Max_Qualified_Name_Length and
      Str'First > Integer'First and Str'Last < Integer'Last,
      Post => Name_Part'Result'Length <= Max_Qualified_Name_Length and
      (if Last_Dot (Str) in Str'Range then Name_Part'Result = Str
           (Last_Dot (Str) + 1 .. Str'Last) else Name_Part'Result = Str);

   procedure Add_Name (Qualified_Name : Wide_Wide_String) with
      Pre => Qualified_Name'Length <= Max_Qualified_Name_Length and
      Qualified_Name'Last < Integer'Last and Map_Size < Natural'Last;

   function Get_Name
     (Qualified_Name : Wide_Wide_String)
      return Wide_Wide_String with
      Pre => Qualified_Name'Length <= Max_Qualified_Name_Length;

      -- Used to obfuscate strings and comments
   function Obfuscated_Text
     (Text : Wide_Wide_String)
      return Wide_Wide_String with
      Post => Obfuscated_Text'Result'Length = Text'Length;

   function Map_Size return Natural;

   procedure Dump;

end Obfuscate.Names;
