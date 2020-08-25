package Command_Line with
   SPARK_Mode,
   Abstract_State => Command_Line_State,
   Initializes    => Command_Line_State

is

   -- all possible command-line options
   type Options_T is
     (Destination, Min_Length, Constant_Length, Skipped_Units, Excluded_Paths, Help);
   subtype String_Options_T is
     Options_T range Options_T'First .. Options_T'Pred (Help);
   subtype Boolean_Options_T is Options_T range Help .. Help;

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

end Command_Line;
