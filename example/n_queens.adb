with Ada.Text_IO; use Ada.Text_IO;
with Queens;
procedure N_Queens is
   Count : Positive;
begin
   Put ("Enter number of queens: ");
   Count := Positive'Value (Get_Line);
   -- Start solving N-Queens problem
   Queens.Queen (1, Count);
end N_Queens;
