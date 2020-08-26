with Ada.Containers.Formal_Vectors;
with Ada.Directories;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.Directory_Operations;
with Gnatcoll.Projects;
with Gnatcoll.Vfs;

with Libadalang.Analysis;
with Libadalang.Project_Provider;

with Ada.Text_IO;

with Command_Line;
with Obfuscate;

-- SPARK off until GNATCOLL and LAL are SPARK-compliant
package body Collection with
   SPARK_Mode => Off
is

   package Gcp renames Gnatcoll.Projects;
   package Lal renames Libadalang.Analysis;
   package Lalpp renames Libadalang.Project_Provider;
   package Vfs renames Gnatcoll.Vfs;

   Max_Size : constant := 10_000;

   subtype Index_T is Integer range 1 .. Max_Size;
   package String_Vectors is new Ada.Containers.Formal_Vectors
     (Index_T, Unbounded_String);
   subtype Vector_T is String_Vectors.Vector (Max_Size);

   function Convert_To_String_Vectors
     (Str : String)
      return Vector_T is
      Ret_Val : Vector_T;
      First   : Integer := Str'First;
      procedure Add_One (Piece : String) is
         To_Add : Unbounded_String := To_Unbounded_String (Piece);
      begin
         Trim (To_Add, Ada.Strings.Both);
         String_Vectors.Append (Ret_Val, To_Add);
      end Add_One;
   begin
      for I in Str'First .. Str'Last
      loop
         if Str (I) = ','
         then
            Add_One (Str
                 (First .. I - 1));
            First := I + 1;
         end if;
      end loop;
      if First < Str'Last
      then
         Add_One (Str
              (First .. Str'Last));
      end if;
      return Ret_Val;
   end Convert_To_String_Vectors;

   Project_Tree   : aliased Gcp.Project_Tree;
   Excluded_Paths : Vector_T;
   Skipped_Units  : Vector_T;

   function Begins_With
     (Look_For  : Unbounded_String;
      In_What   : String;
      Separator : Character)
      return Boolean is
   begin
      if Look_For = In_What
      then
         return True;
      elsif In_What'Length > Length (Look_For) and then In_What
            (In_What'First .. In_What'First + Length (Look_For)) =
          Look_For & Separator
      then
         return True;
      else
         return False;
      end if;
   end Begins_With;

   function Is_Skipped
     (Unit_Name : String)
      return Boolean is
   begin
      for Skipped of Skipped_Units
      loop
         if Begins_With (Skipped, Unit_Name, '.')
         then
            return True;
         end if;
      end loop;
      return False;
   end Is_Skipped;

   function Is_Excluded
     (Filename : String)
      return Boolean is
   begin
      for Excluded of Excluded_Paths
      loop
         if Begins_With
             (Excluded, Filename, GNAT.Directory_Operations.Dir_Separator)
         then
            return True;
         end if;
      end loop;
      return False;
   end Is_Excluded;

   procedure Parse_One_File
     (Context : Lal.Analysis_Context;
      File    : Vfs.Virtual_File) with
      SPARK_Mode => Off
   is
      Info_Set  : Gcp.File_Info_Set := Project_Tree.Info_Set (File => File);
      File_Info : Gcp.File_Info;
      Unit      : Lal.Analysis_Unit;

   begin
      for Info of Info_Set
      loop
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

   procedure Write_One_File
     (Context : Lal.Analysis_Context;
      File    : Vfs.Virtual_File) with
      SPARK_Mode => Off
   is
      Info_Set  : Gcp.File_Info_Set := Project_Tree.Info_Set (File => File);
      File_Info : Gcp.File_Info;
      Unit      : Lal.Analysis_Unit;

   begin
      for Info of Info_Set
      loop
         File_Info := Gcp.File_Info (Info);
         if Is_Skipped (File_Info.Unit_Name)
           or else Is_Excluded (File_Info.File.Display_Full_Name)
         then
            null;
         else
            Unit := Context.Get_From_File (File_Info.File.Display_Full_Name);
            Obfuscate.Write
              (Unit,
               Command_Line.Option (Command_Line.Destination) &
               Ada.Directories.Simple_Name (File_Info.File.Display_Full_Name));
         end if;
      end loop;
   end Write_One_File;

   procedure Process_Gpr_File (Filename : String) with
      SPARK_Mode => Off
   is
      Root_Project_Path : Vfs.Virtual_File :=
        Vfs.Create (Full_Filename => Vfs.Filesystem_String (Filename));
      Context : Lal.Analysis_Context;
      Files   : Vfs.File_Array_Access;

   begin

      Excluded_Paths :=
        Convert_To_String_Vectors
          (Command_Line.Option (Command_Line.Excluded_Paths));
      Skipped_Units :=
        Convert_To_String_Vectors
          (Command_Line.Option (Command_Line.Skipped_Units));

      Gcp.Load
        (Self              => Project_Tree,
         Root_Project_Path => Root_Project_Path);

      Context :=
        Lal.Create_Context (Unit_Provider => Lalpp.Create_Project_Unit_Provider
               (Tree => Project_Tree'Access,
                Env  => null));

      Files := Project_Tree.Root_Project.Source_Files (Recursive => True);
      for F in Files'Range
      loop
         Parse_One_File (Context, Files (F));
      end loop;

      for F in Files'Range
      loop
         Write_One_File (Context, Files (F));
      end loop;

      Project_Tree.Unload;

   end Process_Gpr_File;

end Collection;
