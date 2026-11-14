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

\ A record split at its place line ` --> file:L:C`: the text before it and
\ the text after it; with no place line the whole is the head.
: UNLOCATED ( ptr u8 n -- ptr u8 n ptr u8 n ) {: a:ptr u:n :}
   a u s"  --> " FIND-SUB MATCH option
     none OF a u a 0 ENDOF
     some OF IDX>N {: at:n :}
        a at + u at - 10 INDEX-OF MATCH option
          none OF a at a 0 ENDOF
          some OF IDX>N 1 + at + {: end:n :} a at  a end +  u end - ENDOF
        ;MATCH ENDOF
   ;MATCH ;

\ Short of a trailing ` lr <16 hex>` and newline: a kernel exit line ends in
\ its trapping code's return address, which ASLR moves between runs.
21 constant LR-TAIL
: PAST-LR ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   u LR-TAIL < if a u exit then
   a u + LR-TAIL - 4 s"  lr " STR= if a u LR-TAIL - else a u then ;

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
   err erru PAST-LR SAVED-ERR SAVED-ERR-U @ PAST-LR T$= ;

\ As SAME, but stderr's place line may differ: a run names the file it reads
\ or `<input>`.
: SAME-UNLOCATED ( ptr u8 n ptr u8 n n -- )
   {: out:ptr outu:n err:ptr erru:n rc:n :}
   rc SAVED-RC @ T=
   out outu SAVED-OUT SAVED-OUT-U @ T$=
   err erru UNLOCATED {: head:ptr hu:n tail:ptr tu:n :}
   SAVED-ERR SAVED-ERR-U @ UNLOCATED
   {: whead:ptr whu:n wtail:ptr wtu:n :}
   head hu whead whu T$=
   tail tu wtail wtu T$= ;

;package
