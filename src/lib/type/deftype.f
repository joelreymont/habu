\ deftype.f - `DEFTYPE NAME` mints a value-nominal integer type in the active
\ package with converters `>NAME ( n -- NAME )` and `NAME>N ( NAME -- n )`, the
\ only way across: the checker never lets n stand in for a nominal or back
\ (test/deftype-suite.f). The same NAME in two packages is two types.
\ STORAGE CLASS. PROCESS-WIDE: NG-BUFFER and NM-BUFFER are static load-time
\ codegen buffers; DEFTYPE runs in one loading task at a time.

require src/lib/string.f
require src/lib/codegen.f

package VNOM

private

CODEGEN:BUFFER NG-BUFFER
: NG-RESET ( -- )  NG-BUFFER CODEGEN:RESET ;
: NG+ ( ptr u8 n -- )  NG-BUFFER CODEGEN:APPEND-STRING ;
: NG$ ( -- ptr u8 n )  NG-BUFFER CODEGEN:CONTENTS ;

\ Evaluate declaration text with the checker active, as a closed program: a text
\ that left or took cells would be rejected with a record and 70. The generated
\ converters use CAST:, which checks representation, ownership and linearity;
\ evaluation does not exempt them.
: NG-EVAL ( -- )  NG$ evaluate-closed ;

\ Kept apart from NG-BUFFER because MINT reads the mangled tail here while
\ EMIT-IN/EMIT-OUT build the converter text there.
CODEGEN:BUFFER NM-BUFFER
: MANGLE ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   NM-BUFFER CODEGEN:RESET
   u 0 ?do  a i + c@ ASCII-LOWER NM-BUFFER CODEGEN:APPEND-BYTE  loop
   NM-BUFFER CODEGEN:CONTENTS ;

\ Converter declarations have no body; CAST: validates the retype.
: EMIT-IN ( ptr u8 n ptr u8 n -- ) {: sa:ptr su:n ta:ptr tu:n :}
   NG-RESET
   s" CAST: >" NG+  sa su NG+  s"  ( n -- " NG+  ta tu NG+  s"  ) " NG+
   NG-EVAL ;
: EMIT-OUT ( ptr u8 n ptr u8 n -- ) {: sa:ptr su:n ta:ptr tu:n :}
   NG-RESET
   s" CAST: " NG+  sa su NG+  s" >N ( " NG+  ta tu NG+  s"  -- n ) " NG+
   NG-EVAL ;

public

\ Registers the arity-0 family and its converter pair in the active package, or
\ the global scope at top level. A TFAM tail is lowercase (E-TFAM-CASE): the
\ surface NAME folds to it (FRAME-INDEX -> frame-index; TF-CANON? keeps hyphens);
\ the converters take the surface spelling (`>SERIAL`), a signature names the
\ tail (`( serial -- n )`). CHECKER-DEFFAMILY refuses a reserved tail (a
\ CT-role, or `ptr`, the pointer constructor) and one the scope already declares
\ (E-TFAM-DUP); a package may still shadow a global or another package's family.
\ It also rejects a name longer than TFAM:TF-NAME-MAX before anything is
\ registered.
: MINT ( ptr u8 n -- ) {: sa:ptr su:n :}
   sa su MANGLE {: ta:ptr tu:n :}
   ta tu sa su CHECKER-DEFFAMILY
   sa su ta tu EMIT-IN
   sa su ta tu EMIT-OUT ;

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
