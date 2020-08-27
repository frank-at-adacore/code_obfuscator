with Ada.Characters.Handling; use Ada.Characters.Handling;
-- SPARK-compliant version of Ada.Directories
with Directories; use Directories;
with Ada.Text_IO; use Ada.Text_IO;

with Collection;
with Command_Line;
with Obfuscate;

procedure Obfuscator with
   SPARK_Mode
is

   Success : Boolean;

   function Is_Gpr_File
     (Filename : String)
      return Boolean is
      Lc_Filename : constant String  := To_Lower (Filename);
      Last        : constant Integer := Lc_Filename'Last;
   begin
      return Lc_Filename'Length > 4 and then Lc_Filename
            (Last - 3 .. Last) = ".gpr";
   end Is_Gpr_File;

begin
   --  Insert code here.
   Command_Line.Initialize (Success);

   if Success
   then
      declare
         Argument : constant String := Command_Line.Argument;
      begin

         if Argument'Length = 0
         then
            Command_Line.Help;

         elsif not Exists (Argument)
         then
            Put (Argument);
            Put_Line (" does not exist");

         elsif Is_Gpr_File (Argument)
         then
            Collection.Process_Gpr_File (Argument);

         else
            Set_Directory (Containing_Directory (Argument));
            Obfuscate.Parse (Argument);
            Obfuscate.Write (Argument);
         end if;

      end;
   end if;

end Obfuscator;
