\ Bounded x86-64 byte emission into caller-owned writable memory.
require src/targets/x86-64/isa/asm.f

package X64EMIT
public

DEFTYPE CAPACITY
DEFTYPE OFFSET
DEFTYPE TARGET-ADDRESS

E-X64EMIT-BOUNDS constant E-BOUNDS
E-X64EMIT-RELOC constant E-RELOC

private

: ROOM ( capacity offset n -- ) {: capacity:capacity offset:offset size:n :}
   capacity CAPACITY>N {: cap:n :} offset OFFSET>N {: pos:n :}
   cap 0 < pos 0 < or size 0 < or if E-BOUNDS throw then
   pos cap > if E-BOUNDS throw then
   size cap pos - > if E-BOUNDS throw then ;

: WRITE ( ptr u8 offset X64ASM:instruction -- )
   {: data offset:offset insn:X64ASM:instruction :}
   insn X64ASM:SIZE X64ASM:INSN-SIZE>N 0 ?do
      insn i X64ASM:>BYTE-INDEX X64ASM:BYTE
      data offset OFFSET>N + i + c!
   loop ;

: LE32! ( n ptr u8 -- ) {: value:n data :}
   value $FF and data c! value 8 rshift $FF and data 1 + c!
   value 16 rshift $FF and data 2 + c!
   value 24 rshift $FF and data 3 + c! ;

public

: EMIT ( ptr u8 capacity offset X64ASM:instruction -- offset )
   {: data capacity:capacity offset:offset insn:X64ASM:instruction :}
   insn X64ASM:VALIDATE
   insn X64ASM:SIZE X64ASM:INSN-SIZE>N {: size:n :}
   capacity offset size ROOM
   data offset insn WRITE offset OFFSET>N size + >OFFSET ;

\ Whole pair is checked before either instruction touches the output.
: EMIT2 ( ptr u8 capacity offset X64ASM:instruction X64ASM:instruction -- offset )
   {: data capacity:capacity offset:offset first:X64ASM:instruction second:X64ASM:instruction :}
   first X64ASM:VALIDATE second X64ASM:VALIDATE
   first X64ASM:SIZE X64ASM:INSN-SIZE>N {: one:n :}
   second X64ASM:SIZE X64ASM:INSN-SIZE>N {: two:n :}
   capacity offset one two + ROOM
   data offset first WRITE
   data offset OFFSET>N one + >OFFSET second WRITE
   offset OFFSET>N one + two + >OFFSET ;

\ Unsigned address comparison occurs before subtraction; no signed wrap test.
: DELTA32 ( target-address target-address -- X64ASM:displacement )
   {: target:target-address pc:target-address :}
   target TARGET-ADDRESS>N {: dest:n :} pc TARGET-ADDRESS>N {: origin:n :}
   dest origin u< if
      origin dest - dup $80000000 u> if E-RELOC throw then negate
   else
      dest origin - dup $7FFFFFFF u> if E-RELOC throw then
   then X64ASM:>DISPLACEMENT ;

: ADDRESS+ ( target-address offset -- target-address )
   {: base:target-address offset:offset :}
   offset OFFSET>N dup 0 < if E-BOUNDS throw then {: size:n :}
   base TARGET-ADDRESS>N {: origin:n :}
   origin size + dup origin u< if E-RELOC throw then >TARGET-ADDRESS ;

\ The two offsets are coordinates, not byte counts and not host pointers.
\ end is the actual instruction end, including any trailing immediate.
: ELF-ADDEND ( offset offset -- n ) {: field:offset end:offset :}
   field OFFSET>N {: patch:n :} end OFFSET>N {: next:n :}
   patch 0 < next 0 < or patch next > or if E-RELOC throw then
   next patch - 4 < if E-RELOC throw then patch next - ;

\ Low-level patch primitive. A future owned-emission validates recipe ownership,
\ opcode/field identity and generation before calling this bounded writer.
: PATCH-REL32 ( ptr u8 capacity offset offset target-address target-address -- )
   {: data capacity:capacity field:offset end:offset base:target-address target:target-address :}
   capacity field 4 ROOM capacity end 0 ROOM
   field end ELF-ADDEND drop
   target base end ADDRESS+ DELTA32 X64ASM:DISPLACEMENT>N {: delta:n :}
   delta data field OFFSET>N + LE32! ;

: PATCH-ABS64 ( ptr u8 capacity offset target-address -- )
   {: data capacity:capacity field:offset target:target-address :}
   capacity field 8 ROOM
   target TARGET-ADDRESS>N {: value:n :}
   value data field OFFSET>N + LE32!
   value 32 rshift data field OFFSET>N + 4 + LE32! ;

;package
