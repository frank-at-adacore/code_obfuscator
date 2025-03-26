with Ada.Text_IO;
with Ada.Wide_Wide_Text_IO;

package body Obfuscate.Names.Debug is

   procedure Print is
      Cursor  : Name_Map.Cursor;
      Element : Unbounded_Wide_Wide_String;
   begin
      Ada.Text_IO.Put_Line ("=== Names ===");

      Cursor := Name_Map.First (Map);
      while Cursor /= Name_Map.No_Element loop
         Element := Name_Map.Element (Position => Cursor);
         Ada.Wide_Wide_Text_IO.Put_Line
           (To_Wide_Wide_String (Name_Map.Key (Cursor)) & ": " &
            To_Wide_Wide_String (Element));
         Cursor := Name_Map.Next (Cursor);
      end loop;
   end Print;

end Obfuscate.Names.Debug;
