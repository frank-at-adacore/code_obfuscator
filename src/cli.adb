with Ada.Directories;           use Ada.Directories;
with Ada.Containers.Ordered_Maps;
with GNAT.Command_Line;         use GNAT.Command_Line;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;

package body Cli is
   Config : Command_Line_Configuration;

   Default_Min_Length : constant := 2;

   Global_Argument         : Unbounded_String := Null_Unbounded_String;
   Global_Constant_Length  : Natural          := 0;
   Global_Destination      : Unbounded_String := Null_Unbounded_String;
   Global_Excluded_Paths   : String_Set_T     := String_Sets_Pkg.Empty_Set;
   Global_Externally_Built : Boolean          := False;
   Global_Min_Length       : Positive         := Default_Min_Length;
   Global_Clear_Strings    : Boolean          := False;
   Global_Recursive        : Boolean          := False;
   Global_Skipped_Units    : String_Set_T     := String_Sets_Pkg.Empty_Set;

   function From_String
     (Source : String)
      return Unbounded_String renames To_Unbounded_String;

   type Switch_Action_T is access procedure (Param : String := "");
   type Switch_T is record
      Switch      : Unbounded_String;
      Long_Switch : Unbounded_String;
      With_Param  : Boolean;
      Help        : Unbounded_String;
      Action      : Switch_Action_T;
   end record;
   type Switches_T is array (Positive range <>) of Switch_T;

   procedure Action_Clear_Strings (Unused : String := "");
   --  Callback for Clear_Strings command line switch

   procedure Action_Destination (Param : String := "");
   --  Callback for Destination command line switch

   procedure Action_Excluded_Paths (Param : String := "");
   --  Callback for Excluded_Paths command line switch

   procedure Action_Externally_Built (Unused : String := "");
   --  Callback for Externally_Built command line switch

   procedure Action_Min_Length (Param : String := "");
   --  Callback for Min_Length command line switch

   procedure Action_Recursive (Unused : String := "");
   --  Callback for Recursive command line switch

   procedure Action_Skipped_Units (Param : String := "");
   --  Callback for Skipped_Units command line switch

   procedure Action_Constant_Length (Param : String := "");
   --  Callback for Constant_Length command line switch

   function Convert
     (Source : String)
      return String_Set_T;
   --  Convert comma-separated list of strings into a set of strings

   -----------------
   -- Action_Help --
   -----------------

   procedure Action_Help (Unused : String := "") is
   begin
      Display_Help (Config);
   end Action_Help;

   --------------------------
   -- Action_Clear_Strings --
   --------------------------

   procedure Action_Clear_Strings (Unused : String := "") is
   begin
      Global_Clear_Strings := True;
   end Action_Clear_Strings;

   ------------------------
   -- Action_Destination --
   ------------------------

   procedure Action_Destination (Param : String := "") is
   begin
      Global_Destination := To_Unbounded_String (Param);
      if Param'Length > 0 and then Param (Param'Last) /= Dir_Separator then
         Global_Destination := Global_Destination & Dir_Separator;
      end if;
      if not Exists (To_String (Global_Destination)) then
         Create_Path (To_String (Global_Destination));
      end if;
   end Action_Destination;

   ---------------------------
   -- Action_Excluded_Paths --
   ---------------------------

   procedure Action_Excluded_Paths (Param : String := "") is
   begin
      Global_Excluded_Paths := Convert (Param);
   end Action_Excluded_Paths;

   -----------------------------
   -- Action_Externally_Built --
   -----------------------------

   procedure Action_Externally_Built (Unused : String := "") is
   begin
      Global_Externally_Built := True;
   end Action_Externally_Built;

   -----------------------
   -- Action_Min_Length --
   -----------------------

   procedure Action_Min_Length (Param : String := "") is
   begin
      Global_Min_Length := Positive'Value (Param);
   exception
      when others =>
         Global_Min_Length := Default_Min_Length;
   end Action_Min_Length;

   ----------------------
   -- Action_Recursive --
   ----------------------

   procedure Action_Recursive (Unused : String := "") is
   begin
      Global_Recursive := True;
   end Action_Recursive;

   --------------------------
   -- Action_Skipped_Units --
   --------------------------

   procedure Action_Skipped_Units (Param : String := "") is
   begin
      Global_Skipped_Units := Convert (Param);
   end Action_Skipped_Units;

   ----------------------------
   -- Action_Constant_Length --
   ----------------------------

   procedure Action_Constant_Length (Param : String := "") is
   begin
      Global_Constant_Length := Positive'Value (Param);
   exception
      when others =>
         Global_Constant_Length := 0;
   end Action_Constant_Length;

   --------------
   -- Argument --
   --------------

   function Argument return String is (To_String (Global_Argument));

   -------------------
   -- Clear_Strings --
   -------------------

   function Clear_Strings return Boolean is (Global_Clear_Strings);

   -------------------
   -- Destination   --
   -------------------

   function Destination return String is (To_String (Global_Destination));

   ----------------- --
   -- Excluded_Paths --
   ----------------- --

   function Excluded_Paths return String_Set_T is (Global_Excluded_Paths);

   ----------------------
   -- Externally_Built --
   ----------------------

   function Externally_Built return Boolean is (Global_Externally_Built);

   -------------------
   -- Min_Length    --
   -------------------

   function Min_Length return Positive is (Global_Min_Length);

   -------------------
   -- Recursive     --
   -------------------

   function Recursive return Boolean is (Global_Recursive);

   -------------------
   -- Skipped_Units --
   -------------------

   function Skipped_Units return String_Set_T is (Global_Skipped_Units);

   ---------------------
   -- Constant_Length --
   ---------------------

   function Constant_Length return Natural is (Global_Constant_Length);

   Global_Switch_Definitions : constant Switches_T :=
     [1 =>
       (Switch      => From_String ("h"),
        Long_Switch => From_String ("help"),
        With_Param  => False,
        Action      => Action_Help'Access,
        Help        => From_String ("Display help")),
     2  =>
       (Switch      => From_String ("d"),
        Long_Switch => From_String ("destination"),
        With_Param  => True,
        Action      => Action_Destination'Access,
        Help        => From_String ("Directory to write obfuscated files")),
     3  =>
       (Switch      => From_String ("u"),
        Long_Switch => From_String ("skipped_units"),
        With_Param  => True,
        Action      => Action_Skipped_Units'Access,
        Help        =>
          From_String
            ("Comma-separated list of units (and children) to exclude")),
     4  =>
       (Switch      => From_String ("p"),
        Long_Switch => From_String ("excluded_paths"),
        With_Param  => True,
        Action      => Action_Excluded_Paths'Access,
        Help        =>
          From_String ("Comma-separated list of file path prefixes to skip")),
     5  =>
       (Switch      => From_String ("m"),
        Long_Switch => From_String ("min_length"),
        With_Param  => True,
        Action      => Action_Min_Length'Access,
        Help        => From_String ("Minimum length of name to obfuscate")),
     6  =>
       (Switch      => From_String ("c"),
        Long_Switch => From_String ("constant_length"),
        With_Param  => True,
        Action      => Action_Constant_Length'Access,
        Help        => From_String ("Make all names the specified length")),
     7  =>
       (Switch      => From_String ("k"),
        Long_Switch => From_String ("keep_strings"),
        With_Param  => False,
        Action      => Action_Clear_Strings'Access,
        Help        =>
          From_String
            ("If specified, string literals will not be obfuscated")),
     8  =>
       (Switch      => From_String ("r"),
        Long_Switch => From_String ("recursive"),
        With_Param  => False,
        Action      => Action_Recursive'Access,
        Help        =>
          From_String ("If specified, imported projects will be traversed")),
     9  =>
       (Switch      => From_String ("b"),
        Long_Switch => From_String ("external"),
        With_Param  => False,
        Action      => Action_Externally_Built'Access,
        Help        =>
          From_String
            ("If specified, externally built projects will be traversed"))];

   package Switch_Map is new Ada.Containers.Ordered_Maps
     (Key_Type     => Unbounded_String,
      Element_Type => Switch_T);
   Global_Switches : Switch_Map.Map;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Set_Usage (Config, "[switches] ");
      for Switch of Global_Switch_Definitions loop
         declare
            Colon : constant String := (if Switch.With_Param then ":" else "");
            Short_Switch : constant String :=
              (if Length (Switch.Switch) = 0 then ""
               else "-" & To_String (Switch.Switch));
            Long_Switch  : constant String :=
              (if Length (Switch.Long_Switch) = 0 then ""
               else "--" & To_String (Switch.Long_Switch));
         begin
            Define_Switch
              (Config,
               Switch      => Short_Switch & Colon,
               Long_Switch => Long_Switch & Colon,
               Help        => To_String (Switch.Help));
            if Short_Switch'Length /= 0 then
               Global_Switches.Insert (From_String (Short_Switch), Switch);
            end if;
            if Long_Switch'Length /= 0 then
               Global_Switches.Insert (From_String (Long_Switch), Switch);
            end if;
         end;
      end loop;
   end Initialize;

   ---------------------
   -- Config_Callback --
   ---------------------

   procedure Config_Callback (Switch, Param, Unused : String) is
      use type Switch_Map.Cursor;
      Element : Switch_Map.Cursor;
   begin
      Element := Global_Switches.Find (From_String (Switch));
      if Element /= Switch_Map.No_Element then
         Switch_Map.Element (Element).Action (Param);
      end if;
   end Config_Callback;

   -----------
   -- Parse --
   -----------

   procedure Parse (Was_Help_Request : out Boolean) is
   begin
      Getopt (Config, Config_Callback'Access);
      declare
         End_Of_Arguments : Boolean;
         Argument         : constant String :=
           Get_Argument (End_Of_Arguments => End_Of_Arguments);
      begin
         if End_Of_Arguments or else Argument'Length = 0 then
            Was_Help_Request := True;
         else
            Global_Argument  := To_Unbounded_String (Argument);
            Was_Help_Request := False;
         end if;
      end;
   exception
      when Exit_From_Command_Line =>
         Was_Help_Request := True;
   end Parse;

   ----------
   -- Help --
   ----------

   procedure Help is
   begin
      Display_Help (Config);
   end Help;

   -------------
   -- Convert --
   -------------

   function Convert
     (Source : String)
      return String_Set_T is
      To_Insert : Unbounded_String := Null_Unbounded_String;
      Retval    : String_Set_T;
   begin
      for One of Source loop
         if One = ',' then
            if Length (To_Insert) > 0 then
               Retval.Insert (To_Insert);
            end if;
            To_Insert := Null_Unbounded_String;
         else
            To_Insert := To_Insert & One;
         end if;
      end loop;
      if Length (To_Insert) > 0 then
         Retval.Insert (To_Insert);
      end if;
      return Retval;
   end Convert;

end Cli;
