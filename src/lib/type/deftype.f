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
EXPORT TYPE-DECL:SD-WRAP
EXPORT TYPE-DECL:SD-ONE-NAME
EXPORT TYPE-DECL:TDECL-CONVERTERS
EXPORT TYPE-DECL:CHECKER-DEFTYPE
EXPORT CHECK:INTRINSIC
EXPORT CHECK:INTRINSIC-DECLARATION

DYNAMIC-BUFFER TAIL u8

: MANGLE ( str -- str ) {: s:str :}
   s STR:LEN TAIL-RESERVE
   s STR:LEN 0 ?do  s STR:DATA i + c@ STR:ASCII-LOWER i TAIL c!  loop
   0 TAIL s STR:LEN >str ;

public

\ Registers the arity-0 family and its converter pair in the active package, or
\ the global scope at top level. A TFAM tail is lowercase (E-TFAM-CASE): the
\ surface NAME folds to it (FRAME-INDEX -> frame-index; TF-CANON? keeps hyphens);
\ the converters take the surface spelling (`>SERIAL`), a signature names the
\ tail (`( serial -- n )`). CHECKER-DEFTYPE rejects an empty name,
\ a reserved tail (a CT-role, or `ptr`, which an effect never reads as a
\ family) and one the scope already declares (E-TFAM-DUP); a package may still
\ shadow a global or another package's family.
: MINT ( str -- ) {: ss:str :}
   ss MANGLE {: ts:str :}
   ss ts CHECKER-DEFTYPE ;

\ Mints NAME over BASE, one name: a family as wide as the base, with
\ >NAME ( base -- name ) and NAME>BASE ( name -- base ).
: MINT-OVER ( str str -- ) {: ss:str bs:str :}
   ss bs SD-ONE-NAME 0= IF EXIT THEN
   ss MANGLE {: ts:str :}
   ts STR:DATA ts STR:LEN  ss STR:DATA ss STR:LEN  bs STR:DATA bs STR:LEN
   s" deftype" dup STR:DATA swap STR:LEN SD-WRAP {: fam:n :}
   ss bs fam TDECL-CONVERTERS ;

global

\ The one global word, a declarer beside ENUM, SUMTYPE and NEWTYPE, so it reads
\ bare from any package while the family lands in that package. Top-level
\ interpret only: it parses the next name, and a base on the name's line, and
\ mutates the type registry through VNOM:MINT or VNOM:MINT-OVER, effects its
\ ( -- ) row does not model. A `\` on the name's line is no base.
: DEFTYPE ( -- )
   SRCLOC:TOKEN$ >str {: kw:str :}
   parse-name {: na:str :}
   na STR:LEN 0= if
      kw kw s" no name follows" REJECTION:MISSING-NAME
      EXIT-STATUS:REJECTED REJECTION:STOP
   then
   kw na REJECTION:NOT-A-NAME
   LBUF:STORAGE-LINE-TOKEN {: bs:str :}
   bs STR:LEN 0= IF na VNOM:MINT EXIT THEN
   bs STR:LEN 1 = IF bs STR:DATA c@ 92 = IF bs STR:DATA LBUF:STORAGE-UNREAD  na VNOM:MINT EXIT THEN THEN
   na bs VNOM:MINT-OVER ;
INTRINSIC-DECLARATION INTRINSIC
;package
