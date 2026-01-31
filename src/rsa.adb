with Ada.Text_IO;                      use Ada.Text_IO;
with Ada.Streams;                      use Ada.Streams;
with Ada.Streams.Stream_IO;            use Ada.Streams.Stream_IO;
with Ada.Numerics.Big_Numbers.Big_Integers; use Ada.Numerics.Big_Numbers.Big_Integers;
with Interfaces;                        use Interfaces;

procedure Rsa is

   type Byte_Array is array (Positive range <>) of Unsigned_8;

   function Read_Random_Bytes (N : Positive) return Byte_Array is
      File  : Ada.Streams.Stream_IO.File_Type;
      SArr  : Ada.Streams.Stream_Element_Array (Ada.Streams.Stream_Element_Offset (0) .. Ada.Streams.Stream_Element_Offset (N - 1));
      Bytes : Byte_Array (1 .. N);
   begin
      Ada.Streams.Stream_IO.Open (File => File,
                               Mode => Ada.Streams.Stream_IO.In_File,
                               Name => "/dev/urandom");

      Ada.Streams.Stream_Element_Array'Read (Ada.Streams.Stream_IO.Stream (File), SArr);

      Ada.Streams.Stream_IO.Close (File);

      for I in 1 .. N loop
         Bytes (I) := Unsigned_8 (SArr (Ada.Streams.Stream_Element_Offset (I - 1)));
      end loop;

      return Bytes;
   end Read_Random_Bytes;

   --  Convert bytes to Big_Integer
   function Bytes_To_BI (Arr : Byte_Array) return Big_Integer is
      Result : Big_Integer := 0;
   begin
      for I in Arr'Range loop
         Result := Result * 256 + To_Big_Integer (Integer (Arr (I)));
      end loop;
      return Result;
   end Bytes_To_BI;

   --  Trial division primality test (32-bit only, for simplicity)
   function Is_Prime (N : Big_Integer) return Boolean is
      I : Big_Integer := 2;
   begin
      if N < 2 then
         return False;
      end if;
      while I * I <= N loop
         if N mod I = 0 then
            return False;
         end if;
         I := I + 1;
      end loop;
      return True;
   end Is_Prime;

   --  Extended Euclidean Algorithm: returns (gcd, x, y) for a*x + b*y = gcd
   procedure Extended_GCD (A, B : Big_Integer;
                           G, X, Y : out Big_Integer) is
      Old_R : Big_Integer := A;
      R     : Big_Integer := B;

      Old_S : Big_Integer := 1;
      S     : Big_Integer := 0;

      Old_T : Big_Integer := 0;
      T     : Big_Integer := 1;

      Q, Temp  : Big_Integer;
   begin
      while R /= 0 loop
         Q := Old_R / R;

         Temp := R; R := Old_R - Q * R; Old_R := Temp;
         Temp := S; S := Old_S - Q * S; Old_S := Temp;
         Temp := T; T := Old_T - Q * T; Old_T := Temp;
      end loop;
      G := Old_R;
      X := Old_S;
      Y := Old_T;
   end Extended_GCD;

   --  Modular inverse: e^-1 mod m
   function Mod_Inv (E, M : Big_Integer) return Big_Integer is
      G, X, Y : Big_Integer;
   begin
      Extended_GCD (E, M, G, X, Y);
      if G /= 1 then
         raise Constraint_Error with "No modular inverse exists";
      end if;
      if X < 0 then
         X := X + M;
      end if;
      return X;
   end Mod_Inv;
      --  Modular exponentiation: Base^Exponent mod Modulus
   function Mod_Exp (Base, Exponent, Modulus : Big_Integer) return Big_Integer is
      Result : Big_Integer := 1;
      B      : Big_Integer := Base mod Modulus;
      E2     : Big_Integer := Exponent;
   begin
      while E2 > 0 loop
         if (E2 mod 2) = 1 then
            Result := (Result * B) mod Modulus;
         end if;
         E2 := E2 / 2;
         if E2 > 0 then
            B := (B * B) mod Modulus;
         end if;
      end loop;
      return Result;
   end Mod_Exp;
   function Encrypt (Message : Big_Integer; E, N    : Big_Integer) return Big_Integer is
      begin
         if Message <= 0 or else Message >= N then
            raise Constraint_Error with "Message out of range";
         end if;
      return Mod_Exp (Message, E, N);
   end Encrypt;

   function Decrypt
     (Ciphertext : Big_Integer;
      D, N       : Big_Integer) return Big_Integer
   is
   begin
      return Mod_Exp (Ciphertext, D, N);
   end Decrypt;

   --  Generate a random 32-bit prime
   function Rand_Prime return Big_Integer is
      Candidate : Big_Integer;
      Bytes : Byte_Array (1 .. 4);
   begin
      loop
         Bytes := Read_Random_Bytes (4);      -- 4 bytes = 32-bit
         Candidate := Bytes_To_BI (Bytes);
         --  ensure it's >= 2^31
         if Candidate < 2**31 then
            Candidate := Candidate + 2**31;
         end if;
         if Is_Prime (Candidate) then
            return Candidate;
         end if;
      end loop;
   end Rand_Prime;

   --  RSA variables
   P, Q, N, Lambda_N, E, D : Big_Integer;
   function GCD (A, B : Big_Integer) return Big_Integer is
      X, Y, Temp : Big_Integer;
   begin
      X := A;
      Y := B;
      while Y /= 0 loop
         Temp := Y;
         Y := X mod Y;
         X := Temp;
      end loop;
      return X;
   end GCD;
begin
   Put_Line ("Generating 32-bit primes...");

   P := Rand_Prime;
   loop
      Q := Rand_Prime;
      exit when Q /= P;
   end loop;

   Put_Line ("Found primes:");
   Put_Line ("P = " & Big_Integer'Image (P));
   Put_Line ("Q = " & Big_Integer'Image (Q));

   N := P * Q;
   Put_Line ("N = P * Q = " & Big_Integer'Image (N));

   --  λ(n) = lcm(P-1, Q-1)
   Lambda_N := ((P - 1) * (Q - 1)) / GCD (P - 1, Q - 1);
   Put_Line ("lambda(N) = " & Big_Integer'Image (Lambda_N));

   E := 65537;
   D := Mod_Inv (E, Lambda_N);

   Put_Line ("Public exponent E = " & Big_Integer'Image (E));
   Put_Line ("Private exponent D = " & Big_Integer'Image (D));
   declare
   M : Big_Integer := 123456789;
   C : Big_Integer;
   R : Big_Integer;
   begin
      Put_Line ("Plaintext M = " & Big_Integer'Image (M));

      C := Encrypt (M, E, N);
      Put_Line ("Ciphertext C = " & Big_Integer'Image (C));

      R := Decrypt (C, D, N);
      Put_Line ("Recovered M = " & Big_Integer'Image (R));
   end;

end Rsa;
