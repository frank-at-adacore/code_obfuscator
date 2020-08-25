with Ada.Text_IO;
with Ada.Wide_Wide_Text_IO;

package body Debug with
   SPARK_Mode
is

   package Tio renames Ada.Text_IO;
   package Wio renames Ada.Wide_Wide_Text_IO;

   Debugging : Boolean := False;

   function Image
     (Sloc_Range : Langkit_Support.Slocs.Source_Location_Range)
      return wide_wide_String is
      Start_Line   : constant wide_wide_String := Sloc_Range.Start_Line'wide_wide_Image;
      Start_Column : constant wide_wide_String := Sloc_Range.Start_Column'wide_wide_Image;
      End_Line     : constant wide_wide_String := Sloc_Range.End_Line'wide_wide_Image;
      End_Column   : constant wide_wide_String := Sloc_Range.End_Column'wide_wide_Image;
   begin
      return Start_Line
          (2 .. Start_Line'Last) & ":" & Start_Column
          (2 .. Start_Column'Last) & "-" & End_Line
          (2 .. End_Line'Last) & ":" & End_Column
          (2 .. End_Column'Last);
   end Image;

   procedure Print
     (Prompt : String;
      Node   : Lal.Ada_Node'Class) with
      SPARK_Mode => Off
   is
      Str : constant Wide_Wide_String := Node.Text;
      function Text return Wide_Wide_String is
        (if Str'Length < 15 then Str else Node.Text
             (Str'First .. Str'First + 10) & "....");
   begin
      Print (Prompt & "> ", False);
      Print ("(" & Node.Kind'Image & ") ", False);
      Print (Text, False);
      Print (" / " & Image (Node.Sloc_Range));
   end Print;

   procedure Print
     (S  : Wide_Wide_String;
      Lf : Boolean := True) with
      SPARK_Mode => Off
   is
   begin
      if Debugging
      then
         Wio.Put (S);
         if Lf
         then
            Tio.New_Line;
         end if;
      end if;
   end Print;

   procedure Print
     (S  : String;
      Lf : Boolean := True) with
      SPARK_Mode => Off
   is
   begin
      if Debugging
      then
         Tio.Put (S);
         if Lf
         then
            Tio.New_Line;
         end if;
      end if;
   end Print;
end Debug;
