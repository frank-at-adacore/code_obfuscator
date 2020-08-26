with Ada.Wide_Wide_Text_IO;

with Libadalang.Common;
with Langkit_Support.Text; use Langkit_Support.Text;

with Command_Line;
with Obfuscate.Locations;
with Obfuscate.Names;

with Debug;

package body Obfuscate with
   SPARK_Mode
is

   package Lalco renames Libadalang.Common;

   package Wwio renames Ada.Wide_Wide_Text_IO;

   use type Lalco.Ada_Node_Kind_Type;

   -- declarations are here for subprograms that need to be implemented outside
   -- of SPARK
   procedure Find_Reference (Node : Lal.Ada_Node'Class);
   function Visit_For_Identifiers
     (Node : Lal.Ada_Node'Class)
      return Lalco.Visit_Status;

   -- TBD: Turn SPARK back on when we figure a good way to remove exception
   -- handler
   procedure Find_Reference (Node : Lal.Ada_Node'Class) with
      SPARK_Mode => Off
   is
      Referenced_Decl : Lal.Basic_Decl;
   begin
      Referenced_Decl := Node.As_Name.P_Referenced_Decl;
      if not Referenced_Decl.Is_Null
      then
         Locations.Add_Reference
           (Node, Referenced_Decl.As_Basic_Decl.P_Fully_Qualified_Name);
      else
         Debug.Print ("referenced_decl is null", Node);
      end if;
   exception
      when The_Err : others =>
         Debug.Print ("Parent " & Node.Parent.Kind'Image, Node);
   end Find_Reference;

   function Valid_Length
     (Text : Wide_Wide_String)
      return Boolean is
     (Text'Length <= Max_Qualified_Name_Length
      and then Names.Name_Part (Text)'Length >=
        Command_Line.Option (Command_Line.Min_Length));

   procedure Find_Defining_Name (Node : Lal.Ada_Node'Class) is
      Parent : Lal.Ada_Node := Node.Parent;
   begin
      while not Parent.Is_Null
      loop
         exit when Names.Map_Size = Natural'Last;
         exit when Locations.Map_Size = Natural'Last;
         if Parent.Kind = Lalco.Ada_Defining_Name
         then
            declare
               Qualified_Name : Wide_Wide_String renames
                 Parent.As_Defining_Name.P_Basic_Decl.P_Fully_Qualified_Name;
            begin
               if Valid_Length (Qualified_Name)
               then
                  Names.Add_Name (Qualified_Name);
                  Locations.Add_Reference (Node, Qualified_Name);
               end if;
            end;
            exit;
         end if;
         Parent := Parent.Parent;
      end loop;
   end Find_Defining_Name;

   -- We could traverse the tree by hand if we wanted to stay in SPARK mode.
   function Visit_For_Identifiers
     (Node : Lal.Ada_Node'Class)
      return Lalco.Visit_Status with
      SPARK_Mode => Off
   is
   begin
      if Node.Kind = Lalco.Ada_Identifier
      then
         if Node.As_Name.P_Is_Defining
         then
            Find_Defining_Name (Node);
         elsif Node.Parent.Kind = Lalco.Ada_End_Name
         then
            Locations.Add_Reference
              (Node,
               Node.Parent.As_End_Name.P_Basic_Decl.P_Fully_Qualified_Name);
         else
            Find_Reference (Node);
         end if;
      end if;
      return Lalco.Into;
   end Visit_For_Identifiers;

   -- TBD: Turn SPARK back on when we get support for subprogram accesses
   procedure Parse (Unit : Lal.Analysis_Unit) with
      SPARK_Mode => Off
   is
   begin
      if not Unit.Root.Is_Null
      then
         Lal.Traverse
           (Node  => Unit.Root,
            Visit => Visit_For_Identifiers'Access);
      else
         Debug.Print (String'("Null unit"));
      end if;
   end Parse;

   -- TBD: Turn SPARK back on when LAL is SPARK-compliant
   procedure Parse (Filename : String) with
      SPARK_Mode => Off
   is
      Context : Lal.Analysis_Context := Lal.Create_Context;
      Unit    : Lal.Analysis_Unit    := Lal.Get_From_File (Context, Filename);

   begin
      Parse (Unit);
   end Parse;

   function Convert_Comment
     (Text : Wide_Wide_String)
      return Wide_Wide_String is
     (if Text'Length > 2 then "--" & Names.Obfuscated_Text (Text
             (Text'First + 2 .. Text'Last)) else Text);
   function Convert_String
     (Text : Wide_Wide_String)
      return Wide_Wide_String is
     (if Command_Line.Option (Command_Line.Strings) then
        Names.Obfuscated_Text (Text)
      else Text);

   procedure Write
     (Unit         : Lal.Analysis_Unit;
      New_Filename : String) is
      Full_Filename : constant String := Unit.Root.Unit.Get_Filename;
      File          : Wwio.File_Type;

      function Qualified_Name
        (Token : Lalco.Token_Reference)
         return Wide_Wide_String is
         Object_Name : constant Wide_Wide_String :=
           Locations.Value
             (Full_Filename, Lalco.Sloc_Range (Lalco.Data (Token)));
      begin
         if Object_Name'Length > 0
         then
            return Names.Get_Name (Object_Name);
         end if;
         return "";
      end Qualified_Name;

   begin
      Wwio.Create
        (File => File,
         Mode => Wwio.Out_File,
         Name => New_Filename);
      for Token of Unit.Root.Token_Range
      loop
         declare
            New_Value : constant Wide_Wide_String := Qualified_Name (Token);
         begin
            if New_Value'Length > 0
            then
               Wwio.Put (File, New_Value);
            else
               declare
                  Text : constant Wide_Wide_String := Lalco.Text (Token);
               begin
                  case Lalco.Kind (Lalco.Data (Token)) is
                     when Lalco.Ada_Comment =>
                        Wwio.Put (File, Convert_Comment (Text));
                     when Lalco.Ada_String =>
                        Wwio.Put (File, Convert_String (Text));
                     when others =>

                        Wwio.Put (File, Text);
                  end case;
               end;

            end if;
         end;
      end loop;
      Wwio.Close (File);

   end Write;

   -- TBD: Turn SPARK back on when LAL is SPARK-compliant
   procedure Write (Filename : String) with
      SPARK_Mode => Off
   is
      Context     : Lal.Analysis_Context := Lal.Create_Context;
      Unit        : Lal.Analysis_Unit := Lal.Get_From_File (Context, Filename);
      Destination : constant String      :=
        Command_Line.Option (Command_Line.Destination);
   begin
      if Destination'Length > 0
      then
         Write
           (Unit, Command_Line.Option (Command_Line.Destination) & Filename);
      else
         Write (Unit, Filename & ".new");
      end if;
   end Write;

end Obfuscate;
