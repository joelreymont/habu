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

create GOT-KEPT CAP allot
create SAVED-KEPT CAP allot

variable GOT-KEPT-U
variable SAVED-KEPT-U

: PAST-CARETS ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   0 begin
      dup u < if dup a + c@ 94 = else false then
   while 1+ repeat
   {: i:n :} a i + u i - ;

\ A line, its gutter indent dropped, that tells where a record is: the place
\ line, a source line, an `= evaluated:` note and the note's caret line.
: PLACE? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" -->" STARTS-WITH?
   a u s" = evaluated:" STARTS-WITH? or
   a u s" ^" STARTS-WITH? or
   u 0 > if a c@ STR-DIGIT? else false then or ;

: KEEP+ ( ptr u8 n ptr u8 ptr len -- ) {: a:ptr u:n dst:ptr lenp:ptr :}
   a u dst CAP lenp BUF-APPEND
   10 dst CAP lenp BUF-APPEND-C ;

\ A gutter line keeps only the label past its carets, whose run is the
\ placed token's length.
: LINE+ ( ptr u8 n ptr u8 ptr len -- ) {: a:ptr u:n dst:ptr lenp:ptr :}
   a u LTRIM {: t:ptr tu:n :}
   t tu PLACE? if exit then
   t tu s" |" STARTS-WITH? if
      t tu 94 INDEX-OF MATCH option
        none OF ENDOF
        some OF IDX>N {: at:n :}
           t at + tu at - PAST-CARETS dst lenp KEEP+ ENDOF
      ;MATCH exit
   then
   t tu dst lenp KEEP+ ;

\ The record's lines short of where it is placed, into DST.
: KEPT ( ptr u8 n ptr u8 ptr len -- ) {: a:ptr u:n dst:ptr lenp:ptr :}
   lenp BUF-RESET
   0 begin
      >r a u 10 r> SPLIT-NEXT
   while
      >r dst lenp LINE+ r>
   repeat drop 2drop ;

\ Short of a trailing ` lr 0x<16 hex>` and newline: a kernel exit line ends in
\ its trapping code's return address, which ASLR moves between runs.
23 constant LR-TAIL
: PAST-LR ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   u LR-TAIL < if a u exit then
   a u + LR-TAIL - 6 s"  lr 0x" STR= if a u LR-TAIL - else a u then ;

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

\ As SAME, but stderr's record may be placed elsewhere: a direct run places
\ it in the file it reads, a subject run at the test file's word that ran the
\ text, with an `= evaluated:` note. Its code, message, label and other notes
\ agree.
: SAME-UNLOCATED ( ptr u8 n ptr u8 n n -- )
   {: out:ptr outu:n err:ptr erru:n rc:n :}
   rc SAVED-RC @ T=
   out outu SAVED-OUT SAVED-OUT-U @ T$=
   err erru GOT-KEPT GOT-KEPT-U KEPT
   SAVED-ERR SAVED-ERR-U @ SAVED-KEPT SAVED-KEPT-U KEPT
   GOT-KEPT GOT-KEPT-U @ SAVED-KEPT SAVED-KEPT-U @ T$= ;

;package
