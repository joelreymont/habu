\ tail-parity.f - direct/subject result parity shared by the tail gates.

require src/lib/test.f
require src/lib/string.f
require src/lib/adt/option.f

package TAIL-PARITY

$800 constant CAP

create SAVED-OUT CAP allot
create SAVED-ERR CAP allot

variable SAVED-OUT-U
variable SAVED-ERR-U
variable SAVED-RC

: SAVE-BYTES ( ptr u8 n ptr u8 ptr n -- )
   {: src:ptr u:n dst:ptr used:ptr :}
   u CAP > if E-STR-CAPACITY throw then
   src dst u BYTE-COPY
   u used ! ;

\ Past a leading absolute `<path>:<line>: ` or `<path>: `; prose has no
\ leading `/`, so text without one is returned whole.
: PAST-PATH ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   a u s" /" STARTS-WITH? if
      a u s" : " FIND-SUB MATCH option
        none OF a u ENDOF
        some OF IDX>N 2 + {: at:n :} a at + u at - ENDOF
      ;MATCH
   else a u then ;

\ A refusal split at ` habu: `: the class before it and the text after its
\ location; with no ` habu: ` the whole is the class.
: UNLOCATED ( ptr u8 n -- ptr u8 n ptr u8 n ) {: a:ptr u:n :}
   a u s"  habu: " FIND-SUB MATCH option
     none OF a u a 0 ENDOF
     some OF IDX>N {: at:n :}
        a at  a at + 7 +  u at - 7 -  PAST-PATH ENDOF
   ;MATCH ;

public

: SNAPSHOT ( ptr u8 n ptr u8 n n -- )
   {: out:ptr outu:n err:ptr erru:n rc:n :}
   out outu SAVED-OUT SAVED-OUT-U SAVE-BYTES
   err erru SAVED-ERR SAVED-ERR-U SAVE-BYTES
   rc SAVED-RC ! ;

: SAME ( ptr u8 n ptr u8 n n -- )
   {: out:ptr outu:n err:ptr erru:n rc:n :}
   rc SAVED-RC @ T=
   out outu SAVED-OUT SAVED-OUT-U @ T$=
   err erru SAVED-ERR SAVED-ERR-U @ T$= ;

\ As SAME, but stderr's location may differ: a run names the file it reads
\ and line, the file with no line, or nothing.
: SAME-UNLOCATED ( ptr u8 n ptr u8 n n -- )
   {: out:ptr outu:n err:ptr erru:n rc:n :}
   rc SAVED-RC @ T=
   out outu SAVED-OUT SAVED-OUT-U @ T$=
   err erru UNLOCATED {: cls:ptr cu:n tail:ptr tu:n :}
   SAVED-ERR SAVED-ERR-U @ UNLOCATED
   {: was:ptr wu:n wtail:ptr wtu:n :}
   cls cu was wu T$=
   tail tu wtail wtu T$= ;

;package
