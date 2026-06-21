\ forth-candidate-test.f - focused tests for Forth candidate scanning.

4096 constant FCT-CAP
4 constant FCT-SMALL-CAP

create FCT-BUF FCT-CAP allot
create FCT-SMALL FCT-SMALL-CAP allot

variable FCT-U
variable FCT-OK

: FCT-CAND$ ( -- ptr u8 n )
   FCT-BUF FCT-U @ ;

: FCT-EXTRACT! ( ptr u8 n -- )
   FCT-BUF FCT-CAP FC-EXTRACT-CANDIDATE FCT-OK ! FCT-U !
   FCT-OK @ TTRUE ;

: FCT-RAW$ ( -- ptr u8 n )
   s" The answer:
  ```forth
: SQUARE ( i64 -- i64 )
 dup *
;
  ```
tail text
" ;

: FCT-EXPECTED$ ( -- ptr u8 n )
   s" : SQUARE ( i64 -- i64 )
 dup *
;
" ;

: FCT-NO-CODE$ ( -- ptr u8 n )
   s" prose only
  ```forth
  ```
" ;

: FCT-INCOMPLETE$ ( -- ptr u8 n )
   s" before
: BROKEN ( -- i64 )
 1
" ;

: FCT-RECURSIVE$ ( -- ptr u8 n )
   s" : FACT ( i64 -- i64 ) dup 1 > if dup 1- FACT * then ;" ;

: FCT-SUBSTRINGS$ ( -- ptr u8 n )
   s" : OK ( -- ) ENTRUSTED-VALUE SET-CHECKED trustworthy ;" ;

: FCT-SET-CHECK$ ( -- ptr u8 n )
   s" : BAD ( -- ) 0 set-check ;" ;

: FCT-TRUSTED$ ( -- ptr u8 n )
   s" TRUSTED: BAD ( -- ) ;" ;

: FCT-TRUST$ ( -- ptr u8 n )
   s" : BAD ( -- ) trust ;" ;

: FCT-COMMENT-STRING$ ( -- ptr u8 n )
   SB-RESET
   s" : OK ( trust set-check TRUSTED: ) 1 " SB-APPEND
   FC-S SB-APPEND-C
   FC-DQ SB-APPEND-C
   STR-SPACE SB-APPEND-C
   s" TRUSTED: set-check trust" SB-APPEND
   FC-DQ SB-APPEND-C
   s"  drop " SB-APPEND
   FC-BACKSLASH SB-APPEND-C
   s"  trust set-check TRUSTED:" SB-APPEND
   FC-LF SB-APPEND-C
   s" ;" SB-APPEND
   SB$ ;

: FCT-EXPECT-EXTRACT ( -- )
   FCT-RAW$ FCT-EXTRACT!
   FCT-CAND$ FCT-EXPECTED$ T$=
   FCT-CAND$ FC-COMPLETE? TTRUE ;

: FCT-EXPECT-NAME-SIG ( -- )
   FCT-CAND$ FC-FIRST-NAME$ FCT-OK ! s" SQUARE" T$=
   FCT-OK @ TTRUE
   FCT-CAND$ FC-FIRST-SIG$ FCT-OK ! s" i64 -- i64" T$=
   FCT-OK @ TTRUE ;

: FCT-EXPECT-NO-CODE ( -- )
   FCT-NO-CODE$ FCT-BUF FCT-CAP FC-EXTRACT-CANDIDATE FCT-OK ! FCT-U !
   FCT-U @ 0 T=
   FCT-OK @ TFALSE ;

: FCT-EXPECT-INCOMPLETE ( -- )
   FCT-INCOMPLETE$ FCT-EXTRACT!
   FCT-CAND$ FC-COMPLETE? TFALSE ;

: FCT-EXPECT-RECURSIVE ( -- )
   FCT-RECURSIVE$ FCT-EXTRACT!
   FCT-CAND$ FC-FIRST-NAME$ FCT-OK ! s" FACT" T$=
   FCT-OK @ TTRUE
   FCT-CAND$ FC-COMPLETE? TTRUE ;

: FCT-EXPECT-FORBIDDEN ( -- )
   FCT-SUBSTRINGS$ FC-FORBIDDEN? TFALSE
   FCT-COMMENT-STRING$ FC-FORBIDDEN? TFALSE
   FCT-SET-CHECK$ FC-FORBIDDEN? TTRUE
   FCT-TRUSTED$ FC-FORBIDDEN? TTRUE
   FCT-TRUST$ FC-FORBIDDEN? TTRUE ;

: FCT-EXPECT-SMALL-CAP ( -- )
   FCT-RAW$ FCT-SMALL FCT-SMALL-CAP FC-EXTRACT-CANDIDATE 2drop ;

: FCT-MAIN ( -- )
   T-RESET
   FCT-EXPECT-EXTRACT
   FCT-EXPECT-NAME-SIG
   FCT-EXPECT-NO-CODE
   FCT-EXPECT-INCOMPLETE
   FCT-EXPECT-RECURSIVE
   FCT-EXPECT-FORBIDDEN
   ['] FCT-EXPECT-SMALL-CAP E-FC-CAPACITY TTHROWS
   T-REPORT
   s" forth-candidate-test: ok" type cr ;

FCT-MAIN
