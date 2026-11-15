\ deftype.f - `DEFTYPE NAME` mints a value-nominal integer type in the active
\ package with converters `>NAME ( n -- NAME )` and `NAME>N ( NAME -- n )`, the
\ only way across; `DEFTYPE NAME base` mints one over the base, with `>NAME` and
\ `NAME>BASE` (test/deftype-suite.f). The same NAME in two packages is two types.
\ STORAGE CLASS. PROCESS-WIDE: TAIL is a load-time mapping; DEFTYPE runs in
\ one loading task at a time.

require src/lib/string.f
require src/core/dynamic-storage.f

package VNOM

private

DYNAMIC-BUFFER TAIL u8

: MANGLE ( ^u8 n -- ^u8 n ) {: a:^ u:n :}
   u TAIL-RESERVE
   u 0 ?do  a i + c@ ASCII-LOWER i TAIL c!  loop
   0 TAIL u ;

public

\ Registers the arity-0 family and its converter pair in the active package, or
\ the global scope at top level. A TFAM tail is lowercase (E-TFAM-CASE): the
\ surface NAME folds to it (FRAME-INDEX -> frame-index; TF-CANON? keeps hyphens);
\ the converters take the surface spelling (`>SERIAL`), a signature names the
\ tail (`( serial -- n )`). CHECKER-DEFTYPE rejects an empty or over-cap name,
\ a reserved tail (a CT-role, or `ptr`, which an effect never reads as a
\ family) and one the scope already declares (E-TFAM-DUP); a package may still
\ shadow a global or another package's family.
: MINT ( ^u8 n -- ) {: sa:^ su:n :}
   sa su MANGLE {: ta:^ tu:n :}
   sa su ta tu CHECKER-DEFTYPE ;

\ Mints NAME over BASE, one name: a family as wide as the base, with
\ >NAME ( base -- name ) and NAME>BASE ( name -- base ).
: MINT-OVER ( ^u8 n ^u8 n -- ) {: sa:^ su:n ba:^ bu:n :}
   sa su ba bu STRUCTURE-DECL:SD-ONE-NAME 0= IF EXIT THEN
   sa su MANGLE {: ta:^ tu:n :}
   ta tu ba bu STRUCTURE-DECL:SD-WRAP {: fam:n :}
   sa su ba bu fam TYPE-DECL:TDECL-CONVERTERS ;

;package

\ The one global word, a declarer beside ENUM, SUMTYPE and NEWTYPE, so it reads
\ bare from any package while the family lands in that package. Top-level
\ interpret only: it parses the next name, and a base on the name's line, and
\ mutates the type registry through VNOM:MINT or VNOM:MINT-OVER, effects its
\ ( -- ) row does not model. A `\` on the name's line is no base.
: DEFTYPE ( -- )
   parse-name {: na:^ nu:n :}
   nu 0= IF na nu VNOM:MINT EXIT THEN
   STORAGE-LINE-TOKEN {: ba:^ bu:n :}
   bu 0= IF na nu VNOM:MINT EXIT THEN
   bu 1 = IF ba c@ 92 = IF ba STORAGE-UNREAD  na nu VNOM:MINT EXIT THEN THEN
   na nu ba bu VNOM:MINT-OVER ;
