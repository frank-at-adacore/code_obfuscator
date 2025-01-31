with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with GNAT.Command_Line;       use GNAT.Command_Line;
with GNAT.Strings;            use GNAT.Strings;

package body Command_Line with
   SPARK_Mode,
   Refined_State => (Command_Line_State => Options.Options_State)

is

   package Options with
      Abstract_State => Options_State,
      Initializes    => Options_State
   is
      procedure Initialize (Success : out Boolean) with
         Global => (Output => Options_State);
      function Value
        (Which : String_Options_T)
         return String with
         Global => (Input => Options_State);
      function Value
        (Which : Boolean_Options_T)
         return Boolean with
         Global => (Input => Options_State);
      function Value
        (Which : Integer_Options_T)
         return Integer with
         Global => (Input => Options_State);
      function Argument return String;
      procedure Help with
         Global => (Input => Options_State);
   end Options;

   procedure Initialize (Success : out Boolean) renames Options.Initialize;

   function Argument return String renames Options.Argument;

   procedure Help renames Options.Help;

   function Option
     (Which : String_Options_T)
      return String is (Options.Value (Which));
   function Option
     (Which : Boolean_Options_T)
      return Boolean is (Options.Value (Which));
   function Option
     (Which : Integer_Options_T)
      return Integer is (Options.Value (Which));

   package body Options with
      SPARK_Mode    => Off,
      Refined_State => (Options_State => (Option_Information, Configuration))
   is

      type Option_Information_T is record
         Switch        : Character;
         Long_Switch   : Unbounded_String;
         Help          : Unbounded_String;
         String_Value  : aliased GNAT.Strings.String_Access := null;
         Boolean_Value : aliased Boolean                    := True;
         Integer_Value : aliased Integer                    := 0;
      end record;
      Option_Information : array (Options_T) of aliased Option_Information_T :=
        (Destination =>
           (Switch => 'd', Long_Switch => To_Unbounded_String ("destination"),
            Help   =>
              To_Unbounded_String ("Directory to write obfuscated files"),
            others => <>),
         Strings =>
           (Switch => 's', Long_Switch => To_Unbounded_String ("strings"),
            Help   =>
              To_Unbounded_String
                ("If specified, string literals will also be obfuscated"),
            Boolean_Value => False, others => <>),
         Recursive =>
           (Switch => 'r', Long_Switch => To_Unbounded_String ("recursive"),
            Help   =>
              To_Unbounded_String
                ("If specified, imported projects will be traversed"),
            Boolean_Value => False, others => <>),
         Externally_Built =>
           (Switch => 'b', Long_Switch => To_Unbounded_String ("external"),
            Help   =>
              To_Unbounded_String
                ("If specified, externally built projects will be traversed"),
            Boolean_Value => False, others => <>),
         Help =>
           (Switch => 'h', Long_Switch => To_Unbounded_String ("help"),
            Help   => To_Unbounded_String ("This help message"), others => <>),
         Min_Length =>
           (Switch => 'm', Long_Switch => To_Unbounded_String ("min_length"),
            Help   =>
              To_Unbounded_String ("Minimum length of name to obfuscate"),
            Integer_Value => 2, others => <>),
         Constant_Length =>
           (Switch      => 'c',
            Long_Switch => To_Unbounded_String ("constant_length"),
            Help        =>
              To_Unbounded_String ("Make all names the specified length"),
            others => <>),
         Skipped_Units =>
           (Switch      => 'u',
            Long_Switch => To_Unbounded_String ("skipped_units"),
            Help        =>
              To_Unbounded_String
                ("Comma-separated list of units (and children) to exclude"),
            others => <>),
         Excluded_Paths =>
           (Switch      => 'p',
            Long_Switch => To_Unbounded_String ("excluded_paths"),
            Help        =>
              To_Unbounded_String
                ("Comma-separated list of file path prefixes to skip"),
            others => <>)) with
         Part_Of => Options_State;

      Configuration : Command_Line_Configuration with
         Part_Of => Options_State;

      procedure Initialize (Success : out Boolean) is
      begin
         for Option in String_Options_T
         loop
            Define_Switch
              (Config      => Configuration,
               Output      => Option_Information (Option).String_Value'Access,
               Switch      => "-" & Option_Information (Option).Switch & ":",
               Long_Switch =>
                 "--" & To_String (Option_Information (Option).Long_Switch) &
                 ":",
               Help => To_String (Option_Information (Option).Help));
         end loop;
         for Option in Integer_Options_T
         loop
            Define_Switch
              (Config      => Configuration,
               Output      => Option_Information (Option).Integer_Value'Access,
               Switch      => "-" & Option_Information (Option).Switch & ":",
               Initial     => Option_Information (Option).Integer_Value,
               Default     => Option_Information (Option).Integer_Value,
               Long_Switch =>
                 "--" & To_String (Option_Information (Option).Long_Switch) &
                 ":",
               Help => To_String (Option_Information (Option).Help));
         end loop;
         for Option in Boolean_Options_T
         loop
            Define_Switch
              (Config => Configuration,
               Output => Option_Information (Option).Boolean_Value'Access,
            --Value       => Option_Information (Option).Boolean_Value,
               Switch      => "-" & Option_Information (Option).Switch,
               Long_Switch =>
                 "--" & To_String (Option_Information (Option).Long_Switch),
               Help => To_String (Option_Information (Option).Help));
         end loop;
         Set_Usage
           (Config => Configuration,
            Usage  => "[switches] GPR-file | Ada-file");

         Getopt (Configuration);
         Success := True;
      exception
         when others =>
            Success := False;

      end Initialize;

      function Value
        (Which : String_Options_T)
         return String is (Option_Information (Which).String_Value.all);
      function Value
        (Which : Boolean_Options_T)
         return Boolean is (Option_Information (Which).Boolean_Value);
      function Value
        (Which : Integer_Options_T)
         return Integer is (Option_Information (Which).Integer_Value);

      procedure Help is
      begin
         Display_Help (Configuration);
      end Help;

      function Argument return String is
      begin
         return Get_Argument;
      end Argument;

   end Options;

end Command_Line;
