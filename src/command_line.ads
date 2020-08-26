package Command_Line with
   SPARK_Mode,
   Abstract_State => Command_Line_State,
   Initializes    => Command_Line_State

is

   Command_Line_Exception : exception;

   -- all possible command-line options
   type Options_T is
     (Destination, Strings, Min_Length, Constant_Length, Skipped_Units,
      Excluded_Paths, Help);
   subtype String_Options_T is Options_T with
        Static_Predicate => String_Options_T in Destination | Skipped_Units |
            Excluded_Paths;
   subtype Boolean_Options_T is Options_T with
        Static_Predicate => Boolean_Options_T in Strings | Help;
   subtype Integer_Options_T is Options_T with
        Static_Predicate => Integer_Options_T in Min_Length | Constant_Length;

   procedure Initialize with
      Global => (Output => Command_Line_State);

   procedure Help with
      Global => (Input => Command_Line_State);

   function Argument return String;
   function Option
     (Which : String_Options_T)
      return String;
   function Option
     (Which : Boolean_Options_T)
      return Boolean;
   function Option
     (Which : Integer_Options_T)
      return Integer;

end Command_Line;
