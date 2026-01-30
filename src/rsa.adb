with Ada.Text_IO;               use Ada.Text_IO;
with Ada.Numerics.Discrete_Random;
with Interfaces;               use Interfaces;

procedure Rsa is

   subtype U32 is Interfaces.Unsigned_32;

   function Rand (Low, High : U32) return U32 is
      subtype Rand_Range is U32 range Low .. High;
      package Rand_U32 is new Ada.Numerics.Discrete_Random (Rand_Range);
      Gen : Rand_U32.Generator;
   begin
      Rand_U32.Reset (Gen);
      return Rand_U32.Random (Gen);
   end Rand;
   
   function Is_Prime (N : U32) return Boolean is
      I : U32 := 2;
   begin
      if N < 2 then
         return False;
      end if;

      -- Trial division up to floor(sqrt(N))
      while I * I <= N loop
         if N mod I = 0 then
            return False;
         end if;
         I := I + 1;
      end loop;

      return True;
   end Is_Prime;

   Candidate : U32;

begin
   loop
      Candidate := Rand (2 ** 31, 2 ** 32 - 1);

      if Is_Prime (Candidate) then
         Put_Line
           ("Prime found: " & U32'Image (Candidate));
         exit;
      end if;
   end loop;
end Rsa;
