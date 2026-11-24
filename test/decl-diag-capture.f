\ Shared declaration-rejection capture for the declaration suites: a suite reads
\ the declaration's record from REJECTION's capture, the channel the check tool
\ reads, as prose or as JSON. It pins the channel, not a check-tool run. A
\ capture also silences the records a reject suite provokes.

require src/lib/string.f
require src/lib/string-option.f

package DECL-DIAG

\ CAPTURE-ON captures declaration diagnostics, as JSON when its flag is true;
\ CAPTURE-OFF restores prose mode; CAPTURED$ returns the production capture.
: CAPTURE-ON ( bool -- )
   REJECTION:JSON !
   REJECTION:CAPTURE ;
: CAPTURE-OFF ( -- )
   REJECTION:BUFFER-OFF
   false REJECTION:JSON ! ;
: CAPTURED$ ( -- str ) REJECTION:BUFFER$ ;

\ Haystack tail, needle: does the needle start here?
: AT? ( str str -- bool )
   {: hs:str ns:str :}
   ns STR:LEN hs STR:LEN > IF 0 0= 0= EXIT THEN
   0 BEGIN dup ns STR:LEN < WHILE
      dup ns STR:DATA + c@  over hs STR:DATA + c@ <>
      IF drop 0 0= 0= EXIT THEN
      1 +
   REPEAT drop 0 0= ;

\ The offset of the first newline at or after from, else the length.
: EOL ( str n -- n )
   {: s:str from:n :}
   from begin dup s STR:LEN < while
      dup s STR:DATA + c@ 10 = if exit then
      1+
   repeat ;

\ The offset just past the newline that ends the line before at, else 0.
: BOL ( str n -- n )
   {: s:str at:n :}
   at begin dup 0 > while
      dup 1- s STR:DATA + c@ 10 = if exit then
      1-
   repeat ;

\ How many bytes from at on equal c.
: RUN-OF ( str n n -- n )
   {: s:str at:n c:n :}
   at begin dup s STR:LEN < while
      dup s STR:DATA + c@ c <> if at - exit then
      1+
   repeat at - ;

\ The bytes at i read `^ LABEL` and end their line.
: LABEL-AT? ( n str str -- bool )
   {: i:n hs:str ls:str :}
   hs STR:DATA {: ha:^ :}
   ls STR:LEN {: lu:n :}
   i lu + 3 + hs STR:LEN > if false exit then
   ha i + c@ 94 =  ha i 1+ + c@ 32 = and
   ha i 2 + + lu >str ls AT? and
   ha i 2 + lu + + c@ 10 = and ;

\ A caret line of the record ends in `^ LABEL`.
: LABEL? ( str -- bool )
   {: ls:str :}
   CAPTURED$ {: hs:str :}
   0 begin dup hs STR:LEN < while
      dup hs ls LABEL-AT? if drop true exit then
      1+
   repeat drop false ;

\ The `= evaluated:` note's offset, else -1.
: NOTE ( -- n )
   CAPTURED$ s" = evaluated: " STR:FIND-SUB MATCH option
     none OF -1 ENDOF
     some OF IDX>N ENDOF
   ;MATCH ;

\ The caret line under the note at e marks the token ts in the note's text;
\ an empty token is one caret just past the text.
: MARKS? ( n str -- bool )
   {: e:n ts:str :}
   CAPTURED$ {: hs:str :}
   ts STR:LEN {: tu:n :}
   e 13 + {: t:n :}
   hs t EOL {: nl:n :}
   hs nl 1+ 32 RUN-OF {: k:n :}
   hs nl 1+ k + 94 RUN-OF {: c:n :}
   k t hs e BOL - - {: col:n :}
   tu 0= if c 1 = col nl t - = and exit then
   c tu <> col tu + nl t - > or if false exit then
   hs STR:DATA t + col + tu >str ts AT? ;

public

\ Start (or restart) a capture. Each call empties the capture, so a suite asserts
\ about exactly one declaration at a time.
: PROSE ( -- ) false CAPTURE-ON ;
: JSON ( -- ) true CAPTURE-ON ;
: OFF ( -- ) CAPTURE-OFF ;

: TEXT$ ( -- str ) CAPTURED$ ;
: LEN ( -- n ) CAPTURED$ STR:LEN ;
: SILENT? ( -- bool ) LEN 0= ;

\ Byte equality of two spans, for asserting a packet field against a literal
\ when the field is read back through DECL-REJECT's reflection rather than out
\ of a rendered line.
: SAME? ( str str -- bool ) {: xs:str ys:str :}
   xs STR:LEN ys STR:LEN <> IF 0 0= 0= EXIT THEN
   xs ys AT? ;

: HAS? ( str -- bool )
   CAPTURED$ swap STR:CONTAINS? ;

\ CAUSE? is true when the record's `= evaluated:` note's caret marks the
\ token ts and a caret line of the record carries the label ls.
: CAUSE? ( str str -- bool )
   {: ts:str ls:str :}
   NOTE {: e:n :}
   e 0< if false exit then
   e ts MARKS?  ls LABEL? and ;

;package
