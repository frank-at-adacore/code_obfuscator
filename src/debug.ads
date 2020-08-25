with Libadalang.Analysis;
with Langkit_Support.Slocs;
package Debug with
   SPARK_Mode
is

   package Lal renames Libadalang.Analysis;

   procedure Print
     (S  : Wide_Wide_String;
      Lf : Boolean := True);
   procedure Print
     (S  : String;
      Lf : Boolean := True);
   procedure Print
     (Prompt : String;
      Node   : Lal.Ada_Node'Class);

   function Image
     (Sloc_Range : Langkit_Support.Slocs.Source_Location_Range)
      return wide_wide_String;

end Debug;
