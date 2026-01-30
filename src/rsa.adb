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

      --  Trial division up to floor(sqrt(N))
      while I * I <= N loop
         if N mod I = 0 then
            return False;
         end if;
         I := I + 1;
      end loop;

      return True;
   end Is_Prime;
   function GCD (A, B : U32) return U32 is
      Temp : U32;
      X : U32 := A;
      Y : U32 := B;
   begin
      while Y /= 0 loop
         Temp := Y;
         Y := X mod Y;
         X := Temp;
      end loop;
      return X;
   end GCD;

   p_candidate : U32;
   q_candidate : U32;
   p : U32;
   q : U32;
   n : U32;
   lambda_n : U32;
begin
   loop
      q_candidate := Rand (2 ** 31, 2 ** 32 - 1);
      p_candidate := Rand (2 ** 31, 2 ** 32 - 1);
      if Is_Prime (q_candidate and p_candidate) then
         q := q_candidate;
         p := p_candidate;
         Put_Line ("Found primes:");
         Put_Line ("p = " & U32'Image (p));
         Put_Line ("q = " & U32'Image (q));
         n := p * q;
         Put_Line ("n = p * q = " & U32'Image (n));
         lambda_n := (p - 1) * (q - 1) / GCD (p - 1, q - 1);
         Put_Line ("lambda(n) = " & U32'Image (lambda_n));
         exit;
      end if;
   end loop;
end Rsa;
