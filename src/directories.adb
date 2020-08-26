with Ada.Directories;
package body Directories is

   function Exists
     (Name : String)
      return Boolean renames Ada.Directories.Exists;
   procedure Set_Directory (Directory : String) renames
     Ada.Directories.Set_Directory;
   function Containing_Directory
     (Name : String)
      return String renames Ada.Directories.Containing_Directory;

end Directories;
