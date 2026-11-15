\ Shared declaration-rejection capture for the declaration suites: a suite reads
\ the declaration's record from REJECTION's capture, the channel the check tool
\ reads, as prose or as JSON. It pins the channel, not a check-tool run. A
\ capture also silences the records a reject suite provokes.

require src/lib/string.f

package DECL-DIAG

\ CAPTURE-ON captures declaration diagnostics, as JSON when its flag is true;
\ CAPTURE-OFF restores prose mode; CAPTURED$ returns the production capture.
: CAPTURE-ON ( bool -- )
   REJECTION:JSON !
   REJECTION:CAPTURE ;
: CAPTURE-OFF ( -- )
   REJECTION:BUFFER-OFF
   false REJECTION:JSON ! ;
: CAPTURED$ ( -- ptr u8 n ) REJECTION:BUFFER$ ;

variable SI

: AT? ( ptr u8 n ptr u8 n -- bool )     \ haystack tail, needle: needle starts here?
   {: ha:ptr hu:n na:ptr nu:n :}
   nu hu > IF 0 0= 0= EXIT THEN
   0 BEGIN dup nu < WHILE
      dup na + c@  over ha + c@ <> IF drop 0 0= 0= EXIT THEN
      1 +
   REPEAT drop 0 0= ;

\ The offset of the first newline at or after from, else u.
: EOL ( ptr u8 n n -- n )
   {: a:ptr u:n from:n :}
   from begin dup u < while
      dup a + c@ 10 = if exit then
      1+
   repeat ;

\ The offset just past the newline that ends the line before at, else 0.
: BOL ( ptr u8 n n -- n )
   {: a:ptr u:n at:n :}
   at begin dup 0 > while
      dup 1- a + c@ 10 = if exit then
      1-
   repeat ;

\ How many bytes from at on equal c.
: RUN-OF ( ptr u8 n n n -- n )
   {: a:ptr u:n at:n c:n :}
   at begin dup u < while
      dup a + c@ c <> if at - exit then
      1+
   repeat at - ;

\ The bytes at i read `^ LABEL` and end their line.
: LABEL-AT? ( n ptr u8 n ptr u8 n -- bool )
   {: i:n ha:ptr hu:n la:ptr lu:n :}
   i lu + 3 + hu > if false exit then
   ha i + c@ 94 =  ha i 1+ + c@ 32 = and
   ha i 2 + + lu la lu AT? and
   ha i 2 + lu + + c@ 10 = and ;

\ A caret line of the record ends in `^ LABEL`.
: LABEL? ( ptr u8 n -- bool )
   {: la:ptr lu:n :}
   CAPTURED$ {: ha:ptr hu:n :}
   0 begin dup hu < while
      dup ha hu la lu LABEL-AT? if drop true exit then
      1+
   repeat drop false ;

\ The `= evaluated:` note's offset, else -1.
: NOTE ( -- n )
   CAPTURED$ s" = evaluated: " FIND-SUB MATCH option
     none OF -1 ENDOF
     some OF IDX>N ENDOF
   ;MATCH ;

\ The caret line under the note at e marks ta tu in the note's text; an
\ empty token is one caret just past the text.
: MARKS? ( n ptr u8 n -- bool )
   {: e:n ta:ptr tu:n :}
   CAPTURED$ {: ha:ptr hu:n :}
   e 13 + {: t:n :}
   ha hu t EOL {: nl:n :}
   ha hu nl 1+ 32 RUN-OF {: k:n :}
   ha hu nl 1+ k + 94 RUN-OF {: c:n :}
   k t ha hu e BOL - - {: col:n :}
   tu 0= if c 1 = col nl t - = and exit then
   c tu <> col tu + nl t - > or if false exit then
   ha t + col + tu ta tu AT? ;

public

\ Start (or restart) a capture. Each call empties the capture, so a suite asserts
\ about exactly one declaration at a time.
: PROSE ( -- ) false CAPTURE-ON ;
: JSON ( -- ) true CAPTURE-ON ;
: OFF ( -- ) CAPTURE-OFF ;

: TEXT$ ( -- ptr u8 n ) CAPTURED$ ;
: LEN ( -- n ) CAPTURED$ nip ;
: SILENT? ( -- bool ) LEN 0= ;

\ Byte equality of two spans, for asserting a packet field against a literal
\ when the field is read back through DECL-REJECT's reflection rather than out
\ of a rendered line.
: SAME? ( ptr u8 n ptr u8 n -- bool ) {: aa:ptr au:n ba:ptr bu:n :}
   au bu <> IF 0 0= 0= EXIT THEN
   aa au ba bu AT? ;

: HAS? ( ptr u8 n -- bool ) {: na:ptr nu:n :}
   CAPTURED$ {: ha:ptr hu:n :}
   0 SI !
   BEGIN SI @ nu + hu <= WHILE
      ha SI @ +  hu SI @ -  na nu AT? IF 0 0= EXIT THEN
      SI @ 1 + SI !
   REPEAT 0 0= 0= ;

\ CAUSE? is true when the record's `= evaluated:` note's caret marks the
\ token ta tu and a caret line of the record carries the label la lu.
: CAUSE? ( ptr u8 n ptr u8 n -- bool )
   {: ta:ptr tu:n la:ptr lu:n :}
   NOTE {: e:n :}
   e 0< if false exit then
   e ta tu MARKS?  la lu LABEL? and ;

;package
