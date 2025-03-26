with Libadalang.Analysis;

package Obfuscate is

   package Lal renames Libadalang.Analysis;

   procedure Parse (Filename : String);
   --  Parse the file

   procedure Write (Filename : String);
   --  Write the file

   procedure Parse (Unit : Lal.Analysis_Unit);
   --  Parse the LAL unit

   procedure Write
     (Unit         : Lal.Analysis_Unit;
      New_Filename : String);
   --  Write the updated LAL unit to New_Filename

end Obfuscate;
