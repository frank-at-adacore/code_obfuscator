with Ada.Text_IO;
with Ada.Wide_Wide_Text_IO;

package body Debug is

   package Tio renames Ada.Text_IO;
   package Wio renames Ada.Wide_Wide_Text_IO;

   Debugging : Boolean := False;

   function Image
     (Sloc_Range : Langkit_Support.Slocs.Source_Location_Range)
      return Wide_Wide_String is
      Start_Line   : constant Wide_Wide_String :=
        Sloc_Range.Start_Line'Wide_Wide_Image;
      Start_Column : constant Wide_Wide_String :=
        Sloc_Range.Start_Column'Wide_Wide_Image;
      End_Line     : constant Wide_Wide_String :=
        Sloc_Range.End_Line'Wide_Wide_Image;
      End_Column   : constant Wide_Wide_String :=
        Sloc_Range.End_Column'Wide_Wide_Image;
   begin
      return
        Start_Line (2 .. Start_Line'Last) & ":" &
        Start_Column (2 .. Start_Column'Last) & "-" &
        End_Line (2 .. End_Line'Last) & ":" &
        End_Column (2 .. End_Column'Last);
   end Image;

   procedure Print
     (Prompt : String;
      Node   : Lal.Ada_Node'Class) is
      Str : constant Wide_Wide_String := Node.Text;
      function Text return Wide_Wide_String is
        (if Str'Length < 15 then Str
         else Node.Text (Str'First .. Str'First + 10) & "....");
   begin
      Print (Prompt & "> ", False);
      Print ("(" & Node.Kind'Image & ") ", False);
      Print (Text, False);
      Print (" / " & Image (Node.Sloc_Range));
   end Print;

   procedure Print
     (S  : Wide_Wide_String;
      LF : Boolean := True) is
   begin
      if Debugging then
         Wio.Put (S);
         if LF then
            Tio.New_Line;
         end if;
      end if;
   end Print;

   procedure Print
     (S  : String;
      LF : Boolean := True) is
   begin
      if Debugging then
         Tio.Put (S);
         if LF then
            Tio.New_Line;
         end if;
      end if;
   end Print;
end Debug;
