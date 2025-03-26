with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Ordered_Sets;

package Cli is

   package String_Sets_Pkg is new Ada.Containers.Ordered_Sets
     (Unbounded_String);
   subtype String_Set_T is String_Sets_Pkg.Set;

   procedure Initialize;
   --  Set up global data.

   procedure Parse (Was_Help_Request : out Boolean);
   --  Parse command line. Set Was_Help_Request to True if we help
   --  was displayed.

   function Argument return String;
   --  Return command-line argument

   function Clear_Strings return Boolean;
   --  Do not obfuscate strings if True

   function Destination return String;
   --  Directory to write obfuscated files

   function Excluded_Paths return String_Set_T;
   --  Comma-separated list of file path prefixes to skip

   function Externally_Built return Boolean;
   --  Externally built projects will be traversed if True

   function Min_Length return Positive;
   --  Minimum length of name to obfuscate

   function Recursive return Boolean;
   --  Imported projects will be traversed if True

   function Skipped_Units return String_Set_T;
   --  Comma-separated list of units (and children) to exclude

   function Constant_Length return Natural;
   --  Make all names the specified length if True

   procedure Help;
   --  Display help message

end Cli;
