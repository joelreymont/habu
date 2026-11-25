\ tail-parity.f - direct/subject result parity shared by the tail gates.

require src/lib/test/assert.f
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
   {: src:str dst:^ used:^ :}
   src STR:LEN CAP > if E-STR-CAPACITY throw then
   src STR:DATA dst src STR:LEN BYTE-COPY
   src STR:LEN used ! ;

create GOT-KEPT CAP allot
create SAVED-KEPT CAP allot

variable GOT-KEPT-U
variable SAVED-KEPT-U

: PAST-CARETS ( str -- str ) {: s:str :}
   0 begin
      dup s STR:LEN < if dup s STR:DATA + c@ 94 = else false then
   while 1+ repeat
   {: i:n :} s STR:DATA i + s STR:LEN i - >str ;

\ A line, its gutter indent dropped, that tells where a record is: the place
\ line, a source line or an `= evaluated:` note.
: PLACE? ( str -- bool ) {: s:str :}
   s s" -->" STR:STARTS-WITH?
   s s" = evaluated:" STR:STARTS-WITH? or
   s STR:LEN 0 > if s STR:DATA c@ STR:DIGIT? else false then or ;

: KEEP+ ( str ^u8 ^len -- ) {: s:str dst:^ lenp:^ :}
   s dst CAP lenp BUF-APPEND
   10 dst CAP lenp BUF-APPEND-C ;

\ A gutter line, or the caret line of an `= evaluated:` note, keeps only the
\ label past its carets; the carets, and an evaluating call's label
\ `evaluates the text below`, only tell where the record is.
: LINE+ ( str ^u8 ^len -- ) {: s:str dst:^ lenp:^ :}
   s STR:LTRIM {: t:str :}
   t PLACE? if exit then
   t s" |" STR:STARTS-WITH?  t s" ^" STR:STARTS-WITH? or if
      t 94 STR:INDEX-OF MATCH option
        none OF ENDOF
        some OF IDX>N {: at:n :}
           t STR:DATA at + t STR:LEN at - >str PAST-CARETS {: l:str :}
           l s"  evaluates the text below" STR:= 0= if l dst lenp KEEP+ then ENDOF
      ;MATCH exit
   then
   t dst lenp KEEP+ ;

\ The record's lines short of where it is placed, into DST.
: KEPT ( str ^u8 ^len -- ) {: s:str dst:^ lenp:^ :}
   lenp BUF-RESET
   0 begin
      >r s 10 r> STR:SPLIT-NEXT
   while
      >r dst lenp LINE+ r>
   repeat drop drop ;

\ Short of a trailing ` lr 0x<16 hex>` and newline: a kernel exit line ends in
\ its trapping code's return address, which ASLR moves between runs.
23 constant LR-TAIL
: PAST-LR ( str -- str ) {: s:str :}
   s STR:LEN LR-TAIL < if s exit then
   s STR:DATA s STR:LEN + LR-TAIL - 6 >str s"  lr 0x" STR:= if s STR:DATA s STR:LEN LR-TAIL - >str else s then ;

public

: SNAPSHOT ( str str n -- )
   {: out:str err:str rc:n :}
   out SAVED-OUT SAVED-OUT-U SAVE-BYTES
   err SAVED-ERR SAVED-ERR-U SAVE-BYTES
   rc SAVED-RC ! ;

: SAME ( str str n -- )
   {: out:str err:str rc:n :}
   rc SAVED-RC @ T=
   out SAVED-OUT SAVED-OUT-U @ >str T$=
   err PAST-LR SAVED-ERR SAVED-ERR-U @ >str PAST-LR T$= ;

\ As SAME, but stderr's record may be placed elsewhere: a direct run places
\ it in the file it reads, a subject run at the test file's word that ran the
\ text, with an `= evaluated:` note. Its code, message, label and other notes
\ agree.
: SAME-UNLOCATED ( str str n -- )
   {: out:str err:str rc:n :}
   rc SAVED-RC @ T=
   out SAVED-OUT SAVED-OUT-U @ >str T$=
   err GOT-KEPT GOT-KEPT-U KEPT
   SAVED-ERR SAVED-ERR-U @ >str SAVED-KEPT SAVED-KEPT-U KEPT
   GOT-KEPT GOT-KEPT-U @ >str SAVED-KEPT SAVED-KEPT-U @ >str T$= ;

;package
