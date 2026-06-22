\ aot-call-report-test.f - focused checked tests for tools/aot-call-report.f.
\ Run: bin/hb --load tools/aot-call-report.f tools/aot-call-report-test.f

$D503201F constant ACRT-NOP-INSTR
12 constant ACRT-STENCIL-PADDING-BYTES
4 constant ACRT-WORD-BYTES
$94000005 constant ACRT-BL-PLUS-5
$97FFFFFE constant ACRT-BL-MINUS-2
$94000000 constant ACRT-BL-ZERO
$94000001 constant ACRT-BL-PLUS-1
$94000002 constant ACRT-BL-PLUS-2
$4020 constant ACRT-BUF-CAP

create ACRT-BUF ACRT-BUF-CAP allot
create ACRT-PATH 128 allot
variable ACRT-FD
variable ACRT-N

: ACRT-COUNT-FILE ( ptr u8 n -- n n n )
   REPORT-FILE!
   REPORT-COUNT
   REPORT-BYTES @
   REPORT-STENCILS @
   REPORT-BLS @ ;

: ACRT-ASSERT ( bool -- )
   0= if
      s" aot-call-report-test: assertion failed" 1 die
   then
   ACRT-N @ 1+ ACRT-N ! ;

: ACRT= ( n n -- )
   = ACRT-ASSERT ;

: ACRT-CLEAR ( n -- ) {: u :}
   0 begin dup u < while
      0 over ACRT-BUF + c!
      1+
   repeat drop ;

: ACRT-COPY ( ptr u8 ptr u8 n -- ) {: a:ptr dst:ptr u :}
   0 begin dup u < while
      dup a + c@ over dst + c!
      1+
   repeat drop ;

: ACRT-PATH! ( ptr u8 n -- ) {: a:ptr u :}
   a ACRT-PATH u ACRT-COPY
   0 ACRT-PATH u + c! ;

: ACRT-W32! ( n n -- ) {: w off :}
   w ACRT-BUF off + c!
   w 8 rshift ACRT-BUF off 1+ + c!
   w 16 rshift ACRT-BUF off 2 + + c!
   w 24 rshift ACRT-BUF off 3 + + c! ;

: ACRT-WRITE ( ptr u8 n n -- ) {: a:ptr u n :}
   a u ACRT-PATH!
   ACRT-PATH 1537 493 open ACRT-FD !
   ACRT-FD @ 0 < if s" aot-call-report-test: open failed" 1 die then
   ACRT-FD @ ACRT-BUF n write n ACRT=
   ACRT-FD @ close ;

: ACRT-TEST-SMALL ( -- )
   43 ACRT-CLEAR
   ACRT-NOP-INSTR 0 ACRT-W32!
   ACRT-NOP-INSTR 4 ACRT-W32!
   ACRT-NOP-INSTR 8 ACRT-W32!
   ACRT-BL-PLUS-5 12 ACRT-W32!
   ACRT-BL-MINUS-2 20 ACRT-W32!
   ACRT-NOP-INSTR 24 ACRT-W32!
   ACRT-NOP-INSTR 28 ACRT-W32!
   ACRT-NOP-INSTR 32 ACRT-W32!
   ACRT-BL-ZERO 36 ACRT-W32!
   s" /tmp/habu-aot-report-small.bin" 43 ACRT-WRITE
   s" /tmp/habu-aot-report-small.bin" ACRT-COUNT-FILE
   3 ACRT=
   2 ACRT=
   43 ACRT= ;

: ACRT-TEST-BOUNDARY ( -- )
   $4010 ACRT-CLEAR
   ACRT-BL-PLUS-1 8 ACRT-W32!
   ACRT-NOP-INSTR $3FF4 ACRT-W32!
   ACRT-NOP-INSTR $3FF8 ACRT-W32!
   ACRT-NOP-INSTR $3FFC ACRT-W32!
   ACRT-BL-PLUS-2 $4000 ACRT-W32!
   s" /tmp/habu-aot-report-boundary.bin" $4010 ACRT-WRITE
   s" /tmp/habu-aot-report-boundary.bin" ACRT-COUNT-FILE
   2 ACRT=
   1 ACRT=
   $4010 ACRT= ;

: ACRT-MAIN ( -- )
   1 ACRT-N !
   ACRT-TEST-SMALL
   ACRT-TEST-BOUNDARY
   s" aot-call-report-test: ok (" type ACRT-N @ 1- . s"  assertions)" type cr ;

ACRT-MAIN
