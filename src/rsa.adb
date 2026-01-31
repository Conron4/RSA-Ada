with Ada.Text_IO;                      use Ada.Text_IO;
with Ada.Streams;                      use Ada.Streams;
with Ada.Streams.Stream_IO;            use Ada.Streams.Stream_IO;
with Ada.Numerics.Big_Numbers.Big_Integers; use Ada.Numerics.Big_Numbers.Big_Integers;
with Interfaces;                        use Interfaces;

procedure Rsa is

   type Byte_Array is array (Positive range <>) of Unsigned_8;
   --  Read N random bytes from /dev/urandom
   --  N: number of bytes to read
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
   --  Arr: byte array in big-endian order
   function Bytes_To_BI (Arr : Byte_Array) return Big_Integer is
      Result : Big_Integer := 0;
   begin
      for I in Arr'Range loop
         Result := Result * 256 + To_Big_Integer (Integer (Arr (I)));
      end loop;
      return Result;
   end Bytes_To_BI;

   --  Trial division primality test (32-bit only, for simplicity)
   --  N: candidate integer to test for primality
   function Is_Prime (N : Big_Integer) return Boolean is
      Divisor : Big_Integer := 2;
   begin
      if N < 2 then
         return False;
      end if;
      while Divisor * Divisor <= N loop
         if N mod Divisor = 0 then
            return False;
         end if;
         Divisor := Divisor + 1;
      end loop;
      return True;
   end Is_Prime;

   --  Extended Euclidean Algorithm: returns (gcd, x, y) for a*x + b*y = gcd
   --  A, B: input integers
   --  G: gcd(A,B)  X, Y: coefficients such that A*X + B*Y = G
   procedure Extended_GCD (A, B : Big_Integer;
                           G, X, Y : out Big_Integer) is
      Prev_Remainder : Big_Integer := A;
      Remainder      : Big_Integer := B;

      Prev_X : Big_Integer := 1;
      Curr_X : Big_Integer := 0;

      Prev_Y : Big_Integer := 0;
      Curr_Y : Big_Integer := 1;

      Quotient, Temp  : Big_Integer;
   begin
      while Remainder /= 0 loop
         Quotient := Prev_Remainder / Remainder;

         Temp := Remainder; Remainder := Prev_Remainder - Quotient * Remainder; Prev_Remainder := Temp;
         Temp := Curr_X;    Curr_X    := Prev_X - Quotient * Curr_X;           Prev_X         := Temp;
         Temp := Curr_Y;    Curr_Y    := Prev_Y - Quotient * Curr_Y;           Prev_Y         := Temp;
      end loop;
      G := Prev_Remainder;
      X := Prev_X;
      Y := Prev_Y;
   end Extended_GCD;

   --  Modular inverse: e^-1 mod m
   --  E: value to invert  M: modulus
   function Mod_Inv (E, M : Big_Integer) return Big_Integer is
      Gcd_Value, X, Y : Big_Integer;
   begin
      Extended_GCD (E, M, Gcd_Value, X, Y);
      if Gcd_Value /= 1 then
         raise Constraint_Error with "No modular inverse exists";
      end if;
      if X < 0 then
         X := X + M;
      end if;
      return X;
   end Mod_Inv;
      --  Modular exponentiation: Base^Exponent mod Modulus
      --  Base: base value  Exponent: exponent value  Modulus: modulus value
   function Mod_Exp (Base, Exponent, Modulus : Big_Integer) return Big_Integer is
      Result          : Big_Integer := 1;
      Base_Modulus    : Big_Integer := Base mod Modulus;
      Exponent_Work   : Big_Integer := Exponent;
   begin
      while Exponent_Work > 0 loop
         if (Exponent_Work mod 2) = 1 then
            Result := (Result * Base_Modulus) mod Modulus;
         end if;
         Exponent_Work := Exponent_Work / 2;
         if Exponent_Work > 0 then
            Base_Modulus := (Base_Modulus * Base_Modulus) mod Modulus;
         end if;
      end loop;
      return Result;
   end Mod_Exp;
   --  RSA encryption
   --  Message: plaintext integer  E: public exponent  N: modulus
   function Encrypt (Message : Big_Integer; E, N : Big_Integer) return Big_Integer is
   begin
      if Message <= 0 or else Message >= N then
         raise Constraint_Error with "Message out of range";
      end if;
      return Mod_Exp (Message, E, N);
   end Encrypt;

   --  RSA decryption
   --  Ciphertext: ciphertext integer  D: private exponent  N: modulus
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
   --  P, Q: random primes  N: modulus  Lambda_N: lcm(P-1, Q-1)
   --  E: public exponent  D: private exponent
   P, Q, N, Lambda_N, E, D : Big_Integer;
   --  Greatest common divisor
   --  A, B: input integers
   function GCD (A, B : Big_Integer) return Big_Integer is
      A_Work, B_Work, Temp : Big_Integer;
   begin
      A_Work := A;
      B_Work := B;
      while B_Work /= 0 loop
         Temp := B_Work;
         B_Work := A_Work mod B_Work;
         A_Work := Temp;
      end loop;
      return A_Work;
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
      Plaintext  : Big_Integer := 123456789;
      Ciphertext : Big_Integer;
      Recovered  : Big_Integer;
   begin
      Put_Line ("Plaintext M = " & Big_Integer'Image (Plaintext));

      Ciphertext := Encrypt (Plaintext, E, N);
      Put_Line ("Ciphertext C = " & Big_Integer'Image (Ciphertext));

      Recovered := Decrypt (Ciphertext, D, N);
      Put_Line ("Recovered M = " & Big_Integer'Image (Recovered));
   end;

end Rsa;
