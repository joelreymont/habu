\ arm64-kernel-head.f - LINKER's ARM64:KERNEL writes the arm64 kernel at the
\ head of the code buffer, and ARM64:KERNEL-ENTRY maps the running hb's kernel
\ primitive entries to the same primitives' entries there.
require src/lib/test.f
require src/lib/le.f
require src/lib/ffi-abi.f
require src/habu/layout.f
require src/habu/xref.f
require src/arch/arm64/icode.f
require src/compiler/native/dict.f
require src/arch/arm64/link-kernel.f

package LINKER
private
package KERNEL-HEAD-TEST

CAST: N>U8 ( n -- ptr u8 )
CAST: XT>N ( xt -- n )

variable IN-KERNEL
variable MAPPED
variable COMPARED
variable SAME
variable PCREL


\ b, bl, b.cond, cbz/cbnz, tbz/tbnz, adr/adrp and ldr (literal) encode a
\ pc-relative offset, so the same instruction can differ between the kernels.
: PC-RELATIVE? ( n -- bool )
   {: w:n :}
   w $7C000000 and $14000000 =
   w $FF000010 and $54000000 = or
   w $7E000000 and $34000000 = or
   w $7E000000 and $36000000 = or
   w $1F000000 and $10000000 = or
   w $3B000000 and $18000000 = or ;


: HOST-WORD ( n -- n ) N>U8 LE:U32@ ;
: PRODUCT-WORD ( n -- n ) A64ICODE:CODE swap + LE:U32@ ;


: COMPARE ( n -- )
   {: host:n :}
   host ARM64:KERNEL-ENTRY {: off:n :}
   off 0 < if exit then
   1 MAPPED +!
   host HOST-WORD {: w:n :}
   w PC-RELATIVE? if 1 PCREL +! exit then
   1 COMPARED +!
   off PRODUCT-WORD w = if 1 SAME +! then ;


\ The running hb's kernel primitives are the records that start in its own
\ code, [rbase, CODE-END-CELL).
: VISIT ( ptr n -- )
   {: rec:ptr :}
   rec XREF-START {: start:n :}
   start rbase < if exit then
   start data-base CODE-END-CELL + @ >= if exit then
   1 IN-KERNEL +!
   start COMPARE ;


: ENTRIES-CASE ( -- )
   s" every host kernel primitive maps to a product entry with its first word" T-LABEL
   0 IN-KERNEL !  0 MAPPED !  0 COMPARED !  0 SAME !  0 PCREL !
   ndict@ 0 ?do i XREF-REC VISIT loop
   IN-KERNEL @ 0 > TTRUE
   MAPPED @ IN-KERNEL @ T=
   SAME @ COMPARED @ T=
   s" kernel-head: primitives " type IN-KERNEL @ .
   s" compared " type COMPARED @ .
   s" skipped pc-relative " type PCREL @ . cr ;


: KERNEL-ENTRY-MAPS ( n -- ) ARM64:KERNEL-ENTRY 0 >= TTRUE ;
: MAPS ( xt -- ) XT>N KERNEL-ENTRY-MAPS ;


: NAMED-CASE ( -- )
   s" the primitives T0 compiles in by address map" T-LABEL
   ['] type MAPS
   ['] xt! MAPS
   ['] execute MAPS
   ['] die MAPS
   NDICT:DOES-PATCH-ENTRY KERNEL-ENTRY-MAPS ;


: THUNK-CASE ( -- )
   s" the callback thunk T0 bakes maps" T-LABEL
   -1 FFI:CALLBACK-ENTRY KERNEL-ENTRY-MAPS ;


: INTERIOR-CASE ( -- )
   s" an address inside a primitive maps to nothing" T-LABEL
   ['] dup XT>N 4 + ARM64:KERNEL-ENTRY -1 T= ;

public

: RUN ( -- )
   ARM64:KERNEL
   ENTRIES-CASE
   NAMED-CASE
   THUNK-CASE
   INTERIOR-CASE
   T-REPORT ;

;package

KERNEL-HEAD-TEST:RUN
;package
