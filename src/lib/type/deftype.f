\ deftype.f - `DEFTYPE NAME` mints a value-nominal integer type in the active
\ package with converters `>NAME ( n -- NAME )` and `NAME>N ( NAME -- n )`, the
\ only way across: the checker never lets n stand in for a nominal or back
\ (test/deftype-suite.f). The same NAME in two packages is two types.
\ STORAGE CLASS. PROCESS-WIDE: TAIL is a load-time mapping; DEFTYPE runs in
\ one loading task at a time.

require src/lib/string.f
require src/core/dynamic-storage.f

package VNOM

private

DYNAMIC-BUFFER TAIL u8

: MANGLE ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   u TAIL-RESERVE
   u 0 ?do  a i + c@ ASCII-LOWER i TAIL c!  loop
   0 TAIL u ;

public

\ Registers the arity-0 family and its converter pair in the active package, or
\ the global scope at top level. A TFAM tail is lowercase (E-TFAM-CASE): the
\ surface NAME folds to it (FRAME-INDEX -> frame-index; TF-CANON? keeps hyphens);
\ the converters take the surface spelling (`>SERIAL`), a signature names the
\ tail (`( serial -- n )`). CHECKER-DEFTYPE rejects an empty or over-cap name,
\ a reserved tail (a CT-role, or `ptr`, the pointer constructor) and one the
\ scope already declares (E-TFAM-DUP); a package may still shadow a global or
\ another package's family.
: MINT ( ptr u8 n -- ) {: sa:ptr su:n :}
   sa su MANGLE {: ta:ptr tu:n :}
   sa su ta tu CHECKER-DEFTYPE ;

;package

\ The one global word, a declarer beside ENUM, SUMTYPE and NEWTYPE, so it reads
\ bare from any package while the family lands in that package. Top-level
\ interpret only: it parses the next name and mutates the type registry through
\ VNOM:MINT, effects its ( -- ) row does not model.
: DEFTYPE ( -- )
   SRCLOC:TOKEN$ {: kw:ptr ku:n :}
   parse-name {: na:ptr nu:n :}
   nu 0= if
      kw ku kw ku s" no name follows" REJECTION:MISSING-NAME
      CHECKER-REJECT-RC REJECTION:STOP
   then
   kw ku na nu REJECTION:NOT-A-NAME
   na nu VNOM:MINT ;
