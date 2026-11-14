\ deftype.f - `DEFTYPE NAME` mints a value-nominal integer type in the active
\ package with converters `>NAME ( n -- NAME )` and `NAME>N ( NAME -- n )`, the
\ only way across: the checker never lets n stand in for a nominal or back
\ (test/deftype-suite.f). The same NAME in two packages is two types.
\ STORAGE CLASS. PROCESS-WIDE: NM-BUFFER is a static load-time codegen buffer;
\ DEFTYPE runs in one loading task at a time.

require src/lib/string.f
require src/lib/codegen.f

package VNOM

private

-6001 constant E-VNOM-NAME     \ DEFTYPE given an empty name
-6002 constant E-VNOM-CAP      \ mangle buffer capacity exceeded

\ Holds the longest family name (TF-NAME-MAX), so a longer surface name is
\ refused with E-VNOM-CAP.
TFAM:TF-NAME-MAX constant NM-CAP
NM-CAP E-VNOM-CAP E-VNOM-CAP CODEGEN:BUFFER-E NM-BUFFER
: MANGLE ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   NM-BUFFER CODEGEN:RESET
   u 0 ?do  a i + c@ ASCII-LOWER NM-BUFFER CODEGEN:APPEND-BYTE  loop
   NM-BUFFER CODEGEN:CONTENTS ;

public

\ Registers the arity-0 family and its converter pair in the active package, or
\ the global scope at top level. A TFAM tail is lowercase (E-TFAM-CASE): the
\ surface NAME folds to it (FRAME-INDEX -> frame-index; TF-CANON? keeps hyphens);
\ the converters take the surface spelling (`>SERIAL`), a signature names the
\ tail (`( serial -- n )`). An empty name is E-VNOM-NAME. CHECKER-DEFTYPE
\ refuses a reserved tail (a CT-role, or `ptr`, the pointer constructor) and one
\ the scope already declares (E-TFAM-DUP); a package may still shadow a global or
\ another package's family.
: MINT ( ptr u8 n -- ) {: sa:ptr su:n :}
   su 0= if E-VNOM-NAME throw then
   sa su MANGLE {: ta:ptr tu:n :}
   sa su ta tu CHECKER-DEFTYPE ;

;package

\ The one global word, a declarer beside ENUM, SUMTYPE and NEWTYPE, so it reads
\ bare from any package while the family lands in that package. Top-level
\ interpret only: it parses the next name and mutates the type registry through
\ VNOM:MINT, effects its ( -- ) row does not model.
: DEFTYPE ( -- )
   parse-name VNOM:MINT ;
