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

: SAVE-BYTES ( ^u8 n ^u8 ^n -- )
   {: src:^ u:n dst:^ used:^ :}
   u CAP > if E-STR-CAPACITY throw then
   src dst u BYTE-COPY
   u used ! ;

\ Past a leading absolute `<path>:<line>: ` or `<path>: `; prose has no
\ leading `/`, so text without one is returned whole.
: PAST-PATH ( ^u8 n -- ^u8 n ) {: a:^ u:n :}
   a u s" /" STARTS-WITH? if
      a u s" : " FIND-SUB MATCH option
        none OF a u ENDOF
        some OF IDX>N 2 + {: at:n :} a at + u at - ENDOF
      ;MATCH
   else a u then ;

\ A rejection split at ` habu: `: the class before it and the text after its
\ location. One that starts `habu: ` has no class; with no `habu: ` the
\ whole is the class.
: UNLOCATED ( ^u8 n -- ^u8 n ^u8 n ) {: a:^ u:n :}
   a u s" habu: " STARTS-WITH? if
      a 0  a 6 +  u 6 -  PAST-PATH exit
   then
   a u s"  habu: " FIND-SUB MATCH option
     none OF a u a 0 ENDOF
     some OF IDX>N {: at:n :}
        a at  a at + 7 +  u at - 7 -  PAST-PATH ENDOF
   ;MATCH ;

\ Short of a trailing ` lr <16 hex>` and newline: a kernel exit line ends in
\ its trapping code's return address, which ASLR moves between runs.
21 constant LR-TAIL
: PAST-LR ( ^u8 n -- ^u8 n ) {: a:^ u:n :}
   u LR-TAIL < if a u exit then
   a u + LR-TAIL - 4 s"  lr " STR= if a u LR-TAIL - else a u then ;

public

: SNAPSHOT ( ^u8 n ^u8 n n -- )
   {: out:^ outu:n err:^ erru:n rc:n :}
   out outu SAVED-OUT SAVED-OUT-U SAVE-BYTES
   err erru SAVED-ERR SAVED-ERR-U SAVE-BYTES
   rc SAVED-RC ! ;

: SAME ( ^u8 n ^u8 n n -- )
   {: out:^ outu:n err:^ erru:n rc:n :}
   rc SAVED-RC @ T=
   out outu SAVED-OUT SAVED-OUT-U @ T$=
   err erru PAST-LR SAVED-ERR SAVED-ERR-U @ PAST-LR T$= ;

\ As SAME, but stderr's location may differ: a run names the file it reads
\ and line, the file with no line, or nothing.
: SAME-UNLOCATED ( ^u8 n ^u8 n n -- )
   {: out:^ outu:n err:^ erru:n rc:n :}
   rc SAVED-RC @ T=
   out outu SAVED-OUT SAVED-OUT-U @ T$=
   err erru UNLOCATED {: cls:^ cu:n tail:^ tu:n :}
   SAVED-ERR SAVED-ERR-U @ UNLOCATED
   {: was:^ wu:n wtail:^ wtu:n :}
   cls cu was wu T$=
   tail tu wtail wtu T$= ;

;package
