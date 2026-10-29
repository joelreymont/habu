\ t0-arm64-rows.f - the rows the T0 arm64 emitter (src/habu/t0-arm64.f) files
\ for an install, through the seam src/habu/t0-emit.f: the does> patch slot of
\ a unit SLOT-CLOSE ends, a data address row's origin, the callback stub an
\ armed NCBSTUB appends, and the refusals that keep the patch slot last and an
\ operation x86-64 does not provide from running. Bodies are compiled by
\ T0:COMPILE-TEXT or driven through the seam, sealed into NEMIT, and checked by
\ decoding the words NEMIT answers against the assembler's encodings.

require src/lib/test.f
require src/arch/arm64/asm.f
require src/compiler/target.f
require src/compiler/native/emission.f
require src/compiler/native/callback-stub.f
require src/compiler/native/string.f
require src/habu/t0-emit.f
require src/habu/t0-arm64.f
require src/habu/t0-x64.f
require src/habu/t0.f

package T0-ROWS-TEST
private

\ Where the placed seals put the emission. No body here calls, so no branch
\ has to reach anything from it.
$40000 constant PLACE

\ The stub's slot and thunk, every half distinct, so a lane out of place shows.
$0001000200030004 constant CB-SLOT
$0000123456789ABC constant CB-THUNK

\ A DATA cell an ADDR body names, and the record ordinal its row files.
variable CELLV
7 constant ORDINAL

: V0 ( -- T0-EMIT:vreg ) 0 T0-EMIT:>VREG ;

\ The instruction word at byte at of the sealed emission.
: WORD@ ( n -- n ) {: at:n :}
   NEMIT:BYTES at + {: p:ptr :}
   0 4 0 ?do p i + c@ i 8 * lshift or loop ;

: HALF ( n n -- n ) 16 * rshift $FFFF and ;

\ The four-lane carrier of v into r starts at byte at.
: CHAIN4? ( n n n -- bool ) {: at:n r:n v:n :}
   at WORD@ r v 0 HALF 0 A64ASM:MOVZHW =
   4 1 ?do at i 4 * + WORD@ r v i HALF i A64ASM:MOVKHW = and loop ;

: ARCH-A64 ( -- ) CTARGET-ARCH:AARCH64 T0-EMIT:SELECT ;

\ An ADDR body, as the front end emits a fixed one: the DATA address, pushed,
\ and the return that is the does> patch slot.
: ADDR-BODY ( -- )
   T0-EMIT:OPEN
   CELLV NULL-PTR - ORDINAL V0 T0-EMIT:ADDR-DATA
   V0 T0-EMIT:PUSH
   T0-EMIT:SLOT-CLOSE ;

: PATCH-SLOT-CASE ( -- )
   ARCH-A64
   ADDR-BODY
   T0-EMIT:SEAL-UNPLACED
   s" the patch slot is the emission's last word" T-LABEL
   NEMIT:PATCH-SLOT NEMIT:SIZE 4 - T=
   s" the patch slot is a ret" T-LABEL
   NEMIT:PATCH-SLOT WORD@ A64ASM:ENC-RET T=
   s" the ADDR body is a leaf: its address chain is its first word" T-LABEL
   0 NEMIT:ADDR-SITE@ 0 T=
   s" the data address row files the ordinal ADDR-DATA took" T-LABEL
   0 NEMIT:ADDR-SITE-ORIGIN@ ORDINAL T=
   NEMIT:CLEAR ;

\ The body of source text t has one string literal: under source capture its
\ address row files the captured row's origin and names that row's address.
: STRING-ROW ( ptr u8 n -- ) {: t:ptr u:n :}
   t u T0:COMPILE-TEXT
   T0-EMIT:SEAL-UNPLACED
   s" a string body files one address row" T-LABEL
   NEMIT:ADDR-SITES 1 T=
   NSTR:SOURCE-COUNT 1- {: row:n :}
   s" under source capture the row files -(row+2)" T-LABEL
   0 NEMIT:ADDR-SITE-ORIGIN@ row 2 + negate T=
   0 T0-EMIT:ADDR-ROW {: at:n kind:n to:n :}
   s" the captured source row starts at the address the row names" T-LABEL
   row NSTR:SOURCE@ drop NULL-PTR - to T=
   NEMIT:CLEAR ;

\ A plain literal interns from the text, a counted one from STR-BUF.
: ORIGIN-CASE ( -- )
   ARCH-A64
   NSTR:SOURCE-BEGIN
   S\" ORG ( -- ptr u8 n ) s\" abc\" " STRING-ROW
   S\" CORG ( -- ptr u8 ) c\" abc\" " STRING-ROW
   NSTR:SOURCE-END ;

\ The defer an `is` body binds. Its address row names the defer's cell and files
\ the defer's record ordinal, which the unit object resolves the cell through.
\ Public: the body runs after the package closes and names it qualified.
public
defer ROW-HOOK ( -- n )
s" ROW-HOOK" XREF-FIND-CURRENT-INDEX XREF-REQUIRE-INDEX constant HOOK-IX
private

: IS-ORIGIN-CASE ( -- )
   ARCH-A64
   s" ISO ( n -- ) is T0-ROWS-TEST:ROW-HOOK " T0:COMPILE-TEXT
   T0-EMIT:SEAL-UNPLACED
   s" an `is` body files one address row" T-LABEL
   NEMIT:ADDR-SITES 1 T=
   0 T0-EMIT:ADDR-ROW {: at:n kind:n to:n :}
   s" the row names the defer's cell" T-LABEL
   s" T0-ROWS-TEST:ROW-HOOK" NDICT:SPELL-DEFER-CELL to T=
   s" the row files the defer's record ordinal" T-LABEL
   0 NEMIT:ADDR-SITE-ORIGIN@ HOOK-IX T=
   NEMIT:CLEAR ;

\ A fixed VAL body sealed placed while NCBSTUB is armed.
: STUB-CASE ( -- )
   ARCH-A64
   CB-SLOT CB-THUNK NCBSTUB:SET
   s" VAL ( -- n ) 42 " T0:COMPILE-TEXT
   PLACE T0-EMIT:SEAL
   s" the stub is one more function, after the unit" T-LABEL
   NEMIT:FUNCTIONS 2 T=
   1 NEMIT:FUNCTION-OFFSET@ {: stub:n :}
   s" the stub is the emission's last nine words" T-LABEL
   stub NEMIT:SIZE 36 - T=
   s" the VAL unit ends in ret just before the stub" T-LABEL
   stub 4 - WORD@ A64ASM:ENC-RET T=
   s" the stub loads x9 with the slot, every lane" T-LABEL
   stub 9 CB-SLOT CHAIN4? TTRUE
   s" then x16 with the thunk, every lane" T-LABEL
   stub 16 + 16 CB-THUNK CHAIN4? TTRUE
   s" and branches through x16" T-LABEL
   stub 32 + WORD@ 16 A64ASM:ENC-BR T=
   s" the thunk carrier is the one address row" T-LABEL
   NEMIT:ADDR-SITES 1 T=
   s" at the thunk carrier" T-LABEL
   0 NEMIT:ADDR-SITE@ stub 16 + T=
   s" a code address" T-LABEL
   0 NEMIT:ADDR-SITE-KIND@ NEMIT:ADDR-CODE T=
   s" with no origin" T-LABEL
   0 NEMIT:ADDR-SITE-ORIGIN@ -1 T=
   s" the placed seal records the stub's address as the entry" T-LABEL
   NCBSTUB:ENTRY@ PLACE stub + T=
   NEMIT:CLEAR
   NCBSTUB:CLEAR ;

: SLOT-REFUSED-CASE ( -- )
   ARCH-A64
   s" a unit begun after the patch slot is refused" T-LABEL
   ADDR-BODY
   [: 1 T0-EMIT:QUOT-OPEN ;] T0-EMIT:E-T0-SLOT TTHROWSQ
   s" a callback stub after the patch slot is refused" T-LABEL
   ADDR-BODY
   CB-SLOT CB-THUNK NCBSTUB:SET
   [: T0-EMIT:SEAL-UNPLACED ;] T0-EMIT:E-T0-SLOT TTHROWSQ
   NCBSTUB:CLEAR ;

\ x86-64 provides neither operation yet: each is refused, not run.
: X86-UNFILLED-CASE ( -- )
   CTARGET-ARCH:X86-64 T0-EMIT:SELECT
   s" x86-64 SLOT-CLOSE is refused as unfilled" T-LABEL
   T0-EMIT:OPEN
   [: T0-EMIT:SLOT-CLOSE ;] T0-EMIT:E-T0-UNFILLED TTHROWSQ
   s" an armed x86-64 seal is refused as unfilled" T-LABEL
   s" XVAL ( -- n ) 42 " T0:COMPILE-TEXT
   CB-SLOT CB-THUNK NCBSTUB:SET
   [: T0-EMIT:SEAL-UNPLACED ;] T0-EMIT:E-T0-UNFILLED TTHROWSQ
   NCBSTUB:CLEAR
   ARCH-A64 ;

public

: RUN ( -- )
   T-RESET
   PATCH-SLOT-CASE
   ORIGIN-CASE
   IS-ORIGIN-CASE
   STUB-CASE
   SLOT-REFUSED-CASE
   X86-UNFILLED-CASE
   T-REPORT ;

;package

T0-ROWS-TEST:RUN
