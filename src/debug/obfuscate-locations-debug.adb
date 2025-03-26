with Ada.Text_IO;
with Ada.Wide_Wide_Text_IO;

package body Obfuscate.Locations.Debug is

   use Ada.Strings.Unbounded;
   use Wide_Wide_Unbounded;

   use type Location_Map.Cursor;

   function Image
     (Sloc_Range : Langkit_Support.Slocs.Source_Location_Range)
      return String;
   --  convert Sloc_Range into a string

   -----------
   -- Image --
   -----------

   function Image
     (Sloc_Range : Langkit_Support.Slocs.Source_Location_Range)
      return String is
      Start_Line   : constant String := Sloc_Range.Start_Line'Image;
      Start_Column : constant String := Sloc_Range.Start_Column'Image;
      End_Line     : constant String := Sloc_Range.End_Line'Image;
      End_Column   : constant String := Sloc_Range.End_Column'Image;
   begin
      return
        Start_Line (2 .. Start_Line'Last) & ":" &
        Start_Column (2 .. Start_Column'Last) & "-" &
        End_Line (2 .. End_Line'Last) & ":" &
        End_Column (2 .. End_Column'Last);
   end Image;

   -----------
   -- Print --
   -----------

   procedure Print is
      Cursor  : Location_Map.Cursor;
      Key     : Key_T;
      Element : Unbounded_Wide_Wide_String;
   begin
      Ada.Text_IO.Put_Line ("=== Locations ===");
      Cursor := Location_Map.First (Map);
      while Cursor /= Location_Map.No_Element loop
         Key     := Location_Map.Key (Cursor);
         Element := Location_Map.Element (Position => Cursor);
         Ada.Text_IO.Put (To_String (Key.Filename) & " ");
         Ada.Text_IO.Put (Image (Key.Sloc_Range) & " => ");
         Ada.Wide_Wide_Text_IO.Put_Line (To_Wide_Wide_String (Element));
         Cursor := Location_Map.Next (Cursor);
      end loop;

   end Print;

end Obfuscate.Locations.Debug;
