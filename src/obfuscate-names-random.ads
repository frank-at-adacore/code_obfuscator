package Obfuscate.Names.Random is

   subtype Base_26_T is Natural range 0 .. 25;
   function Random_Character return Base_26_T;

end Obfuscate.Names.Random;
