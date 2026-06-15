\ aot-call-report-test.f — focused tests for tools/aot-call-report.f.
\ Run: sed '$d' tools/aot-call-report.f | cat - tools/aot-call-report-test.f | bin/hb
0 set-check

variable TEST-N

: ASSERT ( f -- )
   0= if s" aot-call-report-test: assertion " type TEST-N @ . cr 1 die then
   TEST-N @ 1+ TEST-N ! ;

: ASSERT= ( got want -- )
   = ASSERT ;

$4020 constant T-CAP
create TBUF T-CAP allot
create TPATH 64 allot
variable TFD

: T-CLEAR {: u :} ( u -- )
   0 begin dup u < while
      0 over TBUF + c!
      1+
   repeat drop ;

: T-W32! {: w off :} ( w off -- )
   w TBUF off + c!
   w 8 rshift TBUF off 1+ + c!
   w 16 rshift TBUF off 2 + + c!
   w 24 rshift TBUF off 3 + + c! ;

: T-PATH! {: a u :} ( a u -- )
   a TPATH u COPY-BYTES
   0 TPATH u + c! ;

: T-WRITE {: a u n :} ( a u n -- )
   a u T-PATH!
   TPATH 1537 493 open TFD !
   TFD @ 0 < if s" aot-call-report-test: open failed" 1 die then
   TFD @ TBUF n write n ASSERT=
   TFD @ close ;

: T-COUNT {: a u :} ( a u -- )
   a u REPORT-FILE!
   REPORT-COUNT ;

: TEST-SMALL ( -- )
   43 T-CLEAR
   NOP-INSTR 0 T-W32!
   NOP-INSTR 4 T-W32!
   NOP-INSTR 8 T-W32!
   $94000005 12 T-W32!
   $97FFFFFE 20 T-W32!
   NOP-INSTR 24 T-W32!
   NOP-INSTR 28 T-W32!
   NOP-INSTR 32 T-W32!
   $94000000 36 T-W32!
   s" /tmp/habu-aot-report-small.bin" 43 T-WRITE
   s" /tmp/habu-aot-report-small.bin" T-COUNT
   REPORT-BYTES @ 43 ASSERT=
   REPORT-STENCILS @ 2 ASSERT=
   REPORT-STENCILS @ STENCIL-PADDING-BYTES * 24 ASSERT=
   REPORT-STENCILS @ WORD-BYTES * 8 ASSERT=
   REPORT-BLS @ 3 ASSERT= ;

: TEST-BOUNDARY ( -- )
   $4010 T-CLEAR
   $94000001 8 T-W32!
   NOP-INSTR $3FF4 T-W32!
   NOP-INSTR $3FF8 T-W32!
   NOP-INSTR $3FFC T-W32!
   $94000002 $4000 T-W32!
   s" /tmp/habu-aot-report-boundary.bin" $4010 T-WRITE
   s" /tmp/habu-aot-report-boundary.bin" T-COUNT
   REPORT-BYTES @ $4010 ASSERT=
   REPORT-STENCILS @ 1 ASSERT=
   REPORT-BLS @ 2 ASSERT= ;

: AOT-CALL-REPORT-TEST ( -- )
   1 TEST-N !
   TEST-SMALL
   TEST-BOUNDARY
   s" aot-call-report-test: ok (" type TEST-N @ 1- . s"  assertions)" type cr ;

AOT-CALL-REPORT-TEST
