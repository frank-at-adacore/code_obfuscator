with Libadalang.Analysis;
with Langkit_Support.Slocs;
package Debug is

   package Lal renames Libadalang.Analysis;

   procedure Print
     (S  : Wide_Wide_String;
      LF : Boolean := True);
   procedure Print
     (S  : String;
      LF : Boolean := True);
   procedure Print
     (Prompt : String;
      Node   : Lal.Ada_Node'Class);

   function Image
     (Sloc_Range : Langkit_Support.Slocs.Source_Location_Range)
      return Wide_Wide_String;

end Debug;
