with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Directories;         use Ada.Directories;
with Ada.Text_IO;             use Ada.Text_IO;
with Cli;
with Collection;
with Obfuscate;

procedure Obfuscator is

   function Is_Gpr_File
     (Filename : String)
      return Boolean;
   --  Return True if Filename ends with ".gpr"

   -----------------
   -- Is_Gpr_File --
   -----------------

   function Is_Gpr_File
     (Filename : String)
      return Boolean is (Extension (To_Lower (Filename)) = "gpr");

   Was_Help_Request : Boolean;

begin

   Cli.Initialize;
   Cli.Parse (Was_Help_Request);

   if Was_Help_Request then
      Cli.Help;

   else
      declare
         Argument : constant String := Cli.Argument;
      begin

         if Argument'Length = 0 then
            Cli.Help;

         elsif not Exists (Argument) then
            Put (Argument);
            Put_Line (" does not exist");

         elsif Is_Gpr_File (Argument) then
            Collection.Process_Gpr_File (Argument);

         else
            Set_Directory (Containing_Directory (Argument));
            Obfuscate.Parse (Argument);
            Obfuscate.Write (Argument);
         end if;

      end;
   end if;

end Obfuscator;
