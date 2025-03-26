package Obfuscate.Names.Random is

   subtype Base_26_T is Natural range 0 .. 25;

   function Random_Character return Base_26_T;
   --  return a random number between 0 and 25

end Obfuscate.Names.Random;
