with Ada.Directories;
with Ada.Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.Directory_Operations;
with GNATCOLL.Projects;
with GNATCOLL.Vfs;

with Libadalang.Analysis;
with Libadalang.Project_Provider;

with Ada.Text_IO;

with Cli;
with Obfuscate;

package body Collection is

   package Gcp renames GNATCOLL.Projects;
   package Lal renames Libadalang.Analysis;
   package Lalpp renames Libadalang.Project_Provider;
   package Vfs renames GNATCOLL.Vfs;

   Project_Tree   : aliased Gcp.Project_Tree;
   Excluded_Paths : Cli.String_Set_T;
   Skipped_Units  : Cli.String_Set_T;

   function Begins_With
     (Look_For  : Unbounded_String;
      In_What   : String;
      Separator : Character)
      return Boolean;
   --  If In_What either matches Look_For, or starts with Look_For followed
   --  by Separator, then return True.

   function Is_Excluded
     (Filename : String)
      return Boolean;
   --  Return True if Filename is found as the first (or only) node in any
   --  of the excluded files.

   function Is_Skipped
     (Unit_Name : String)
      return Boolean;
   --  Return True if Unit_Name is found as the first (or only) node in any
   --  of the skipped units.

   procedure Parse_One_File
     (Context : Lal.Analysis_Context;
      File    : Vfs.Virtual_File);
   --  Read and parse the file

   procedure Write_One_File
     (Context : Lal.Analysis_Context;
      File    : Vfs.Virtual_File);
   --  Write the obfuscated file

   -----------------
   -- Begins_With --
   -----------------

   function Begins_With
     (Look_For  : Unbounded_String;
      In_What   : String;
      Separator : Character)
      return Boolean is
   begin
      if Look_For = In_What then
         return True;
      elsif In_What'Length > Length (Look_For)
        and then In_What (In_What'First .. In_What'First + Length (Look_For)) =
          Look_For & Separator
      then
         return True;
      else
         return False;
      end if;
   end Begins_With;

   ----------------
   -- Is_Skipped --
   ----------------

   function Is_Skipped
     (Unit_Name : String)
      return Boolean is
   begin
      for Skipped of Skipped_Units loop
         if Begins_With (Skipped, Unit_Name, '.') then
            return True;
         end if;
      end loop;
      return False;
   end Is_Skipped;

   -----------------
   -- Is_Excluded --
   -----------------

   function Is_Excluded
     (Filename : String)
      return Boolean is
   begin
      for Excluded of Excluded_Paths loop
         if Begins_With
             (Excluded, Filename, GNAT.Directory_Operations.Dir_Separator)
         then
            return True;
         end if;
      end loop;
      return False;
   end Is_Excluded;

   --------------------
   -- Parse_One_File --
   --------------------

   procedure Parse_One_File
     (Context : Lal.Analysis_Context;
      File    : Vfs.Virtual_File) is
      Info_Set  : constant Gcp.File_Info_Set :=
        Project_Tree.Info_Set (File => File);
      File_Info : Gcp.File_Info;
      Unit      : Lal.Analysis_Unit;

   begin
      for Info of Info_Set loop
         File_Info := Gcp.File_Info (Info);
         if Is_Skipped (File_Info.Unit_Name)
           or else Is_Excluded (File_Info.File.Display_Full_Name)
         then
            null;
         else
            Unit := Context.Get_From_File (File_Info.File.Display_Full_Name);
            Obfuscate.Parse (Unit);
         end if;
      end loop;
   end Parse_One_File;

   --------------------
   -- Write_One_File --
   --------------------

   procedure Write_One_File
     (Context : Lal.Analysis_Context;
      File    : Vfs.Virtual_File) is
      Info_Set  : constant Gcp.File_Info_Set :=
        Project_Tree.Info_Set (File => File);
      File_Info : Gcp.File_Info;
      Unit      : Lal.Analysis_Unit;

   begin
      for Info of Info_Set loop
         File_Info := Gcp.File_Info (Info);
         if Is_Skipped (File_Info.Unit_Name)
           or else Is_Excluded (File_Info.File.Display_Full_Name)
         then
            null;
         else
            Unit := Context.Get_From_File (File_Info.File.Display_Full_Name);
            Obfuscate.Write
              (Unit,
               Cli.Destination &
               Ada.Directories.Simple_Name (File_Info.File.Display_Full_Name));
         end if;
      end loop;
   end Write_One_File;

   ----------------------
   -- Process_Gpr_File --
   ----------------------

   procedure Process_Gpr_File (Filename : String) is
      Root_Project_Path : constant Vfs.Virtual_File :=
        Vfs.Create (Full_Filename => Vfs.Filesystem_String (Filename));
      Context           : Lal.Analysis_Context;
      Files             : Vfs.File_Array_Access;

   begin

      Excluded_Paths := Cli.Excluded_Paths;
      Skipped_Units  := Cli.Skipped_Units;

      Gcp.Load
        (Self              => Project_Tree,
         Root_Project_Path => Root_Project_Path);

      Context :=
        Lal.Create_Context (Unit_Provider => Lalpp.Create_Project_Unit_Provider
               (Tree             => Project_Tree'Access,
                Env              => null,
                Is_Project_Owner => False));

      Files := Project_Tree.Root_Project.Source_Files
          (Recursive                => Cli.Recursive,
           Include_Externally_Built => Cli.Externally_Built);
      for F in Files'Range loop
         declare
            Fn : constant Vfs.Filesystem_String := Files (F).Full_Name;
         begin
            Parse_One_File (Context, Files (F));
         exception
            when The_Err : others =>
               Ada.Text_IO.Put_Line
                 (String (Fn) & " => " &
                  Ada.Exceptions.Exception_Name (The_Err));
         end;
      end loop;

      for F in Files'Range loop
         Write_One_File (Context, Files (F));
      end loop;

      Project_Tree.Unload;

   end Process_Gpr_File;

end Collection;
