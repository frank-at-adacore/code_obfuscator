with GNAT.Command_Line;       use GNAT.Command_Line;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with GNAT.Strings;            use GNAT.Strings;
package body Command_Line with
   SPARK_Mode,
   Refined_State => (Command_Line_State => Options.Options_State)

is

   package Options with
      Abstract_State => Options_State,
      Initializes    => Options_State
   is
      procedure Initialize with
         Global => (Output => Options_State);
      function Value
        (Which : String_Options_T)
         return String with
         Global => (Input => Options_State);
      function Value
        (Which : Boolean_Options_T)
         return Boolean with
         Global => (Input => Options_State);
      procedure Help with
         Global => (Input => Options_State);
      function Argument return String;
   end Options;

   function Short_Specifier
     (Option : Options_T)
      return String is
      Str : constant String := Option'Image;
   begin
      pragma Assert (Str'Length > 0);
      pragma Annotate (Gnatprove, Intentional, "assertion might fail",
         "'image always returns non-empty string");
      return "-" & To_Lower (Str (Str'First));
   end Short_Specifier;

   function Long_Specifier
     (Option : Options_T)
      return String is
      Str : constant String := Option'Image;
   begin
      pragma Assert (Str'Length > 0);
      pragma Annotate (Gnatprove, Intentional, "assertion might fail",
         "'image always returns non-empty string");
      return "--" & To_Lower (Str);
   end Long_Specifier;

   function Help
     (Option : Options_T)
      return String is
   begin
      case Option is
                  when Destination =>
            return "Directory to write obfuscated files";
         when Min_Length =>
            return "Minimum length of name to obfuscate";
         when Constant_Length =>
            return "Make all names the specified length";
         when Skipped_Units =>
            return "Comma-separated list of units (and children) to exclude";
         when Excluded_Paths =>
            return "Comma-separated list of file path prefixes to skip";
         when Help =>
            return "This help message";
      end case;
   end Help;

   procedure Initialize renames Options.Initialize;

   procedure Help renames Options.Help;

   function Argument return String renames Options.Argument;

   function Option
     (Which : String_Options_T)
      return String is (Options.Value (Which));
   function Option
     (Which : Boolean_Options_T)
      return Boolean is (Options.Value (Which));

   package body Options with
      SPARK_Mode    => Off,
      Refined_State =>
      (Options_State =>
         (String_Option_Information, Boolean_Option_Information,
          Configuration))
   is
      String_Option_Information : array
        (String_Options_T) of aliased String_Access with
         Part_Of => Options_State;
      Boolean_Option_Information : array
        (Boolean_Options_T) of aliased Boolean with
         Part_Of => Options_State;

      Configuration : Command_Line_Configuration with
         Part_Of => Options_State;
      procedure Initialize is
      begin
         for Option in String_Options_T
         loop

            Define_Switch
              (Config      => Configuration,
               Output      => String_Option_Information (Option)'Access,
               Switch      => Short_Specifier (Option) & ":",
               Long_Switch => Long_Specifier (Option) & ":",
               Help        => Help (Option));
         end loop;
         for Option in Boolean_Options_T
         loop

            Define_Switch
              (Config      => Configuration,
               Output      => Boolean_Option_Information (Option)'Access,
               Switch      => Short_Specifier (Option),
               Long_Switch => Long_Specifier (Option),
               Help        => Help (Option));
         end loop;
         Set_Usage
           (Config => Configuration,
            Usage  => "[switches] GPR-file | Ada-file");

         Getopt (Configuration);
      end Initialize;
      function Value
        (Which : String_Options_T)
         return String is (String_Option_Information (Which).all);
      function Value
        (Which : Boolean_Options_T)
         return Boolean is (Boolean_Option_Information (Which));

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
