\ run-worker.f - phase-owned resident worker dispatcher.
\
\ Loaded inside a forked worker after test/run-resident.f stores the resident phase id.


package TEST
private
variable TRW-LOAD-START-NS
variable TRW-PATH-A
variable TRW-PATH-U

: TRW-PATH-A-FIELD ( -- ptr ptr u8 )
   TRW-PATH-A 0 ptr-field ;

: TRW-PATH$ ( -- ptr u8 n )
   TRW-PATH-A-FIELD @ TRW-PATH-U @ ;

: TRW-LOAD-MS ( -- n )
   mono-ns TRW-LOAD-START-NS @ - PROC-NS-PER-MS / ;

public
: TRW-LOAD-DONE ( -- )
   TRW-PATH$ TRW-LOAD-MS GS-SPAN-LOAD ;

public
: TRW-CHILD-TEST ( ptr u8 n idx -- ) {: label:ptr labelu:n idx:idx :}
   label labelu
   idx PHASE-SUBJECT
   idx PHASE-RUNNER-KIND
   idx PHASE-BOUNDARY
   idx PHASE-SHA
   GS-TEST ;

: TRW-LOAD ( ptr u8 n -- )
   {: path:ptr pathu:n :}
   path TRW-PATH-A-FIELD !
   pathu TRW-PATH-U !
   mono-ns TRW-LOAD-START-NS !
   path pathu included ;

: TRW-RUN ( -- )
   RESIDENT case
      2 of s" test/run-worker-stdlib.f" TRW-LOAD endof
      3 of s" test/run-worker-stdlib.f" TRW-LOAD endof
      5 of s" test/run-worker-engine.f" TRW-LOAD endof
      6 of s" test/run-worker-debug.f" TRW-LOAD endof
      7 of s" test/run-worker-aot.f" TRW-LOAD endof
      8 of s" test/run-worker-aot-neg.f" TRW-LOAD endof
      9 of s" test/run-worker-engine.f" TRW-LOAD endof
      10 of s" test/run-worker-diag.f" TRW-LOAD endof
      11 of s" test/run-worker-diag.f" TRW-LOAD endof
      12 of s" test/run-worker-diag-all-strict.f" TRW-LOAD endof
      13 of s" test/run-worker-diag.f" TRW-LOAD endof
      14 of s" test/run-worker-dict.f" TRW-LOAD endof
      16 of s" test/run-worker-engine.f" TRW-LOAD endof
      17 of s" test/run-worker-stdlib.f" TRW-LOAD endof
      18 of s" test/run-worker-stdlib.f" TRW-LOAD endof
      21 of s" test/run-worker-engine.f" TRW-LOAD endof
      22 of s" test/run-worker-stdlib.f" TRW-LOAD endof
      23 of s" test/run-worker-stdlib.f" TRW-LOAD endof
      24 of s" test/run-worker-stdlib.f" TRW-LOAD endof
      25 of s" test/run-worker-stdlib.f" TRW-LOAD endof
      26 of s" test/run-worker-stdlib.f" TRW-LOAD endof
      27 of s" test/run-worker-stdlib.f" TRW-LOAD endof
      28 of s" test/run-worker-stdlib.f" TRW-LOAD endof
      29 of s" test/run-worker-stdlib.f" TRW-LOAD endof
      31 of s" test/run-worker-stdlib.f" TRW-LOAD endof
      32 of s" test/run-worker-stdlib.f" TRW-LOAD endof
      33 of s" test/run-worker-stdlib.f" TRW-LOAD endof
      34 of s" test/run-worker-stdlib.f" TRW-LOAD endof
      35 of s" test/run-worker-stdlib.f" TRW-LOAD endof
      36 of s" test/run-worker-stdlib.f" TRW-LOAD endof
      37 of s" test/run-worker-stdlib.f" TRW-LOAD endof
      38 of s" test/run-worker-stdlib.f" TRW-LOAD endof
      39 of s" test/run-worker-stdlib.f" TRW-LOAD endof
      40 of s" test/run-worker-stdlib.f" TRW-LOAD endof
      E-TBL-BOUNDS throw
   endcase ;

\ The phase body is included at load time, so it must run at top-level
\ scope: a package is still open here and an include cannot open another.
' TRW-RUN
;package
execute
