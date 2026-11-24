split-str
\ tail-parity.f - direct/subject result parity shared by the tail gates.

require src/lib/test.f
require src/lib/string.f
require src/lib/string-option.f
require src/lib/adt/option.f

package TAIL-PARITY

$800 constant CAP

create SAVED-OUT CAP allot
create SAVED-ERR CAP allot

variable SAVED-OUT-U
variable SAVED-ERR-U
variable SAVED-RC

: SAVE-BYTES ( str ^u8 ^n -- )
   {: src:^ u:n dst:^ used:^ :}
   u CAP > if E-STR-CAPACITY throw then
   src dst u BYTE-COPY
   u used ! ;

create GOT-KEPT CAP allot
create SAVED-KEPT CAP allot

variable GOT-KEPT-U
variable SAVED-KEPT-U

: PAST-CARETS ( str -- str ) {: a:^ u:n :}
   0 begin
      dup u < if dup a + c@ 94 = else false then
   while 1+ repeat
   {: i:n :} a i + u i - ;

\ A line, its gutter indent dropped, that tells where a record is: the place
\ line, a source line or an `= evaluated:` note.
: PLACE? ( str -- bool ) {: a:^ u:n :}
   a u s" -->" STR:STARTS-WITH?
   a u s" = evaluated:" STR:STARTS-WITH? or
   u 0 > if a c@ STR:DIGIT? else false then or ;

: KEEP+ ( str ^u8 ^len -- ) {: a:^ u:n dst:^ lenp:^ :}
   a u dst CAP lenp BUF-APPEND
   10 dst CAP lenp BUF-APPEND-C ;

\ A gutter line, or the caret line of an `= evaluated:` note, keeps only the
\ label past its carets; the carets, and an evaluating call's label
\ `evaluates the text below`, only tell where the record is.
: LINE+ ( str ^u8 ^len -- ) {: a:^ u:n dst:^ lenp:^ :}
   a u STR:LTRIM {: t:^ tu:n :}
   t tu PLACE? if exit then
   t tu s" |" STR:STARTS-WITH?  t tu s" ^" STR:STARTS-WITH? or if
      t tu 94 STR:INDEX-OF MATCH option
        none OF ENDOF
        some OF IDX>N {: at:n :}
           t at + tu at - PAST-CARETS {: l:^ lu:n :}
           l lu s"  evaluates the text below" STR:= 0= if l lu dst lenp KEEP+ then ENDOF
      ;MATCH exit
   then
   t tu dst lenp KEEP+ ;

\ The record's lines short of where it is placed, into DST.
: KEPT ( ^u8 n ^u8 ^len -- ) {: a:^ u:n dst:^ lenp:^ :}
   lenp BUF-RESET
   0 begin
      >r a u 10 r> STR:SPLIT-NEXT
   while
      >r dst lenp LINE+ r>
   repeat drop 2drop ;

\ Short of a trailing ` lr 0x<16 hex>` and newline: a kernel exit line ends in
\ its trapping code's return address, which ASLR moves between runs.
23 constant LR-TAIL
: PAST-LR ( str -- str ) {: a:^ u:n :}
   u LR-TAIL < if a u exit then
   a u + LR-TAIL - 6 s"  lr 0x" STR:= if a u LR-TAIL - else a u then ;

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

\ As SAME, but stderr's record may be placed elsewhere: a direct run places
\ it in the file it reads, a subject run at the test file's word that ran the
\ text, with an `= evaluated:` note. Its code, message, label and other notes
\ agree.
: SAME-UNLOCATED ( str str n -- )
   {: out:^ outu:n err:^ erru:n rc:n :}
   rc SAVED-RC @ T=
   out outu SAVED-OUT SAVED-OUT-U @ T$=
   err erru GOT-KEPT GOT-KEPT-U KEPT
   SAVED-ERR SAVED-ERR-U @ SAVED-KEPT SAVED-KEPT-U KEPT
   GOT-KEPT GOT-KEPT-U @ SAVED-KEPT SAVED-KEPT-U @ T$= ;

;package
