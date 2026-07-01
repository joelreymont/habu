\ render.f - a byte-exact output buffer shared by the analyzer renderers. The Zig
\ renderers print into an allocating writer; here we append into a fixed buffer and
\ hand back its bytes for a byte-for-byte snapshot comparison. Separate from the
\ lib/string.f SB builder (1 KiB) because a full multi-camera JSON report is larger.
\ Integer text is built with the same digit recursion as lib/fmt.f SB-U.

require lib/errors.f
require lib/string.f
require lib/float.f

$4000 constant RB-CAP                        \ 16 KiB: ample for multi-camera JSON
create RB-BUF RB-CAP allot
variable RB-N  variable RB-CP
-6210 constant E-RB-FULL

: RB-RESET ( -- ) 0 RB-N ! ;
: RB-C ( n -- )                              \ append one byte
   RB-N @ RB-CAP >= if E-RB-FULL throw then  \ guard: never silently truncate output
   RB-BUF RB-N @ + c!  RB-N @ 1+ RB-N ! ;
: RB+ ( ptr u8 n -- ) {: a:ptr u :}          \ append a string
   0 RB-CP ! begin RB-CP @ u < while a RB-CP @ + c@ RB-C  RB-CP @ 1+ RB-CP ! repeat ;
: RB-U ( n -- )                              \ append unsigned decimal
   dup 10 < if 48 + RB-C exit then
   dup 10 / RECURSE  10 mod 48 + RB-C ;
: RB# ( n -- )                               \ append signed decimal
   dup 0 < if 45 RB-C negate then RB-U ;
: RB-NL ( -- ) 10 RB-C ;                     \ newline
: RB$ ( -- ptr u8 n ) RB-BUF RB-N @ ;

\ --- shared CSV/JSON formatting toolkit (used by every analyzer renderer) ---
: QT ( -- ) 34 RB-C ;                         \ "
: CM ( -- ) 44 RB-C ;                         \ ,
: QSTR ( ptr u8 n -- ) QT RB+ QT ;            \ "string"
: QK ( ptr u8 n -- ) QSTR 58 RB-C 32 RB-C ;   \ "key": (colon space)
variable RB-SP
: SPACES ( n -- ) RB-SP ! begin RB-SP @ 0 > while 32 RB-C RB-SP @ 1- RB-SP ! repeat ;
: RB-BOOL ( bool -- ) if s" true" RB+ else s" false" RB+ then ;
\ indented "key": value emitters (no trailing comma/newline)
: KVN ( i64 i64 ptr u8 n -- ) {: v:i64 ind:i64 ka:ptr ku:n :}
   ind SPACES
   ka ku QK
   v RB# ;

: KVS ( ptr u8 n i64 ptr u8 n -- ) {: va:ptr vu:n ind:i64 ka:ptr ku:n :}
   ind SPACES
   ka ku QK
   va vu QSTR ;

: KVB ( bool i64 ptr u8 n -- ) {: b:bool ind:i64 ka:ptr ku:n :}
   ind SPACES
   ka ku QK
   b RB-BOOL ;

\ --- fixed-3-decimal formatters (writeMilli3 / writeFixed3 in fps_sweep.zig) ---
: RB-3 ( n -- ) {: x :}                       \ x in 0..999 as exactly 3 zero-padded digits
   x 100 / 48 + RB-C  x 100 mod 10 / 48 + RB-C  x 10 mod 48 + RB-C ;
: RB-MILLI3 ( n -- )                          \ milli value -> "<int>.<3frac>"
   dup 1000 / RB#  46 RB-C  1000 mod RB-3 ;
: RB-FIXED3 ( n n -- ) {: num:n den:n :}
   den 0= if s" 0.000" RB+
   else num 1000 * den /  dup 1000 / RB#  46 RB-C  1000 mod RB-3
   then ;

: RB-4 ( n -- ) {: x :}                       \ x in 0..9999 as 4 zero-padded digits
   x 1000 / 48 + RB-C  x 1000 mod 100 / 48 + RB-C  x 100 mod 10 / 48 + RB-C  x 10 mod 48 + RB-C ;
: RB-FFIX3 ( r -- )
   1000.0 f* 0.5 f+ f>s {: scaled:n :}
   scaled 1000 / RB#  46 RB-C  scaled 1000 mod RB-3 ;

: RB-RATIO4 ( n n -- ) {: num:n den:n :}
   den 0= if s" 0.0000" RB+
   else num 10000 *  den 2 /  +  den /  dup 10000 / RB#  46 RB-C  10000 mod RB-4
   then ;

\ --- markdown table cell bars ---
: LBAR ( -- ) s" | " RB+ ;                    \ leading "| "
: BAR  ( -- ) s"  | " RB+ ;                   \ middle " | "
: RBAR ( -- ) s"  |" RB+ ;                    \ trailing " |"

\ --- vertical key/value DSL: markdown bullets ("- k: v") and CSV "k,v" rows ---
: MD-S ( ptr u8 n ptr u8 n -- ) {: la:ptr ln:n va:ptr vn:n :}
   s" - " RB+ la ln RB+ s" : " RB+
   va vn RB+
   RB-NL ;

: MD-N ( ptr u8 n n -- ) {: la ln v :} s" - " RB+ la ln RB+ s" : " RB+ v RB# RB-NL ;

: MD-R ( ptr u8 n n n -- ) {: la:ptr ln:n num:n den:n :}
   s" - " RB+ la ln RB+ s" : " RB+
   num den RB-RATIO4
   RB-NL ;

: CVN ( ptr u8 n n -- ) {: la ln v :} la ln RB+ CM v RB# RB-NL ;
: CVR ( ptr u8 n n n -- ) {: la ln num den :} la ln RB+ CM num den RB-RATIO4 RB-NL ;
