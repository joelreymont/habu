\ aot-call-report-test.f - focused checked tests for tools/aot-call-report.f.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/memory.f lib/fs.f lib/process.f lib/process-argv.f tools/aot-call-report.f tools/aot-call-report-test.f

$D503201F constant ACRT-NOP-INSTR
12 constant ACRT-STENCIL-PADDING-BYTES
4 constant ACRT-WORD-BYTES
$94000005 constant ACRT-BL-PLUS-5
$97FFFFFE constant ACRT-BL-MINUS-2
$94000000 constant ACRT-BL-ZERO
$94000001 constant ACRT-BL-PLUS-1
$94000002 constant ACRT-BL-PLUS-2
$4020 constant ACRT-BUF-CAP
$8000 constant ACRT-JSON-CAP
$1000 constant ACRT-ERR-CAP
10000 constant ACRT-TIMEOUT-MS

create ACRT-BUF ACRT-BUF-CAP allot
create ACRT-PATH 128 allot
variable ACRT-FD
variable ACRT-N
variable ACRT-JSON-A
variable ACRT-ERR-A

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

: ACRT-JSON-A-FIELD ( -- ptr ptr u8 )
   ACRT-JSON-A 0 ptr-field ;

: ACRT-ERR-A-FIELD ( -- ptr ptr u8 )
   ACRT-ERR-A 0 ptr-field ;

: ACRT-JSON-A@ ( -- ptr u8 )
   ACRT-JSON-A-FIELD @ ;

: ACRT-ERR-A@ ( -- ptr u8 )
   ACRT-ERR-A-FIELD @ ;

: ACRT-JSON-A! ( ptr u8 -- )
   ACRT-JSON-A-FIELD ! ;

: ACRT-ERR-A! ( ptr u8 -- )
   ACRT-ERR-A-FIELD ! ;

: ACRT-JSON ( -- ptr u8 )
   ACRT-JSON-A@ 0= if
      ACRT-JSON-CAP MEM-ALLOC-BYTES drop ACRT-JSON-A!
   then
   ACRT-JSON-A@ ;

: ACRT-ERR ( -- ptr u8 )
   ACRT-ERR-A@ 0= if
      ACRT-ERR-CAP MEM-ALLOC-BYTES drop ACRT-ERR-A!
   then
   ACRT-ERR-A@ ;

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

: ACRT-CLI-ARGV ( ptr u8 n -- ) {: path:ptr pathu:n :}
   PROC-ARGV-RESET
   s" tools/aot-call-report.f" >LEN PROC-ARGV+
   path pathu >LEN PROC-ARGV+ ;

: ACRT-CAPTURE>N ( len len rc -- n n n ) {: outu:len erru:len rc:rc :}
   outu LEN>N erru LEN>N rc RC>N ;

: ACRT-CLI-RUN ( ptr u8 n -- n n n ) {: path:ptr pathu:n :}
   path pathu ACRT-CLI-ARGV
   s" bin/hb" >LEN ACRT-JSON ACRT-JSON-CAP >LEN ACRT-ERR ACRT-ERR-CAP >LEN
   ACRT-TIMEOUT-MS >MS RUN-ARGV-CAPTURE ACRT-CAPTURE>N ;

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

: ACRT-TEST-BUFFER ( -- )
   s" /tmp/habu-aot-report-small.bin" ACRT-JSON ACRT-JSON-CAP REPORT-JSON-BUFFER
   {: out:ptr outu:n :}
   out ACRT-JSON = ACRT-ASSERT
   outu 0 > ACRT-ASSERT
   REPORT-BYTES @ 43 ACRT=
   REPORT-STENCILS @ 2 ACRT=
   REPORT-BLS @ 3 ACRT=
   out outu 1- + c@ ACR-C-LF ACRT= ;

: ACRT-TEST-CLI ( -- )
   s" /tmp/habu-aot-report-small.bin" ACRT-CLI-RUN
   {: outu:n erru:n rc:n :}
   rc 0 ACRT=
   erru 0 ACRT=
   outu 0 > ACRT-ASSERT
   ACRT-JSON outu 1- + c@ ACR-C-LF ACRT= ;

: ACRT-MAIN ( -- )
   1 ACRT-N !
   ACRT-TEST-SMALL
   ACRT-TEST-BOUNDARY
   ACRT-TEST-BUFFER
   ACRT-TEST-CLI
   s" aot-call-report-test: ok (" type ACRT-N @ 1- . s"  assertions)" type cr ;

ACRT-MAIN
