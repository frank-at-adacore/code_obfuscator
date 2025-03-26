with Libadalang.Analysis;

package Obfuscate is

   package Lal renames Libadalang.Analysis;

   procedure Parse (Filename : String);
   procedure Write (Filename : String);

   procedure Parse (Unit : Lal.Analysis_Unit);
   procedure Write
     (Unit         : Lal.Analysis_Unit;
      New_Filename : String);

private
   Max_Qualified_Name_Length : constant := 1_024;

end Obfuscate;
