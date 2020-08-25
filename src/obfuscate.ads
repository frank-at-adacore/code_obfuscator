with Libadalang.Analysis;

package Obfuscate with
   SPARK_Mode
is

   package Lal renames Libadalang.Analysis;

   procedure Parse (Filename : String);
   procedure Write (Filename : String);

   procedure Parse (Unit : Lal.Analysis_Unit);
   procedure Write
     (Unit         : Lal.Analysis_Unit;
      New_Filename : String);

private
   max_qualified_name_length : constant := 1024;

end Obfuscate;
