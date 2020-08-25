with Ada.Numerics.Discrete_Random;
package body Obfuscate.Names.Random is

   package Generate_Random_Character is new Ada.Numerics.Discrete_Random
     (Base_26_T);

   Random_Generator : Generate_Random_Character.Generator;

   function Random_Character return Base_26_T is
     (Generate_Random_Character.Random (Random_Generator));

begin
   Generate_Random_Character.Reset (Random_Generator);

end Obfuscate.Names.Random;
