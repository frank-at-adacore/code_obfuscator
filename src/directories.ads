-- SPARK-compliant version of Ada.Directories
package Directories with
   SPARK_Mode
is

   function Exists
     (Name : String)
      return Boolean with
      Global => null;
   procedure Set_Directory (Directory : String) with
      Global => null;
   function Containing_Directory
     (Name : String)
      return String with
      Global => null;

end Directories;
