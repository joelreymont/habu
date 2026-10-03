\ aot-call-report-test.f - focused checked tests for tools/aot-call-report.f.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/memory.f lib/fs.f lib/process.f lib/process-argv.f tools/aot-call-report-lib.f tools/aot-call-report-test.f

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require tools/aot-call-report-lib.f
require lib/fmt.f                        \ FMT:.INT - one-line number text

using AOT-CALL-REPORT

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
30000 constant ACRT-TIMEOUT-MS

create ACRT-BUF ACRT-BUF-CAP allot
create ACRT-PATH FS-PATH-CAP allot
create ACRT-SMALL-PATH FS-PATH-CAP allot
create ACRT-BOUNDARY-PATH FS-PATH-CAP allot
create ACRT-MISSING-PATH FS-PATH-CAP allot
variable ACRT-FD
variable ACRT-N
variable ACRT-SMALL-U
variable ACRT-BOUNDARY-U
variable ACRT-MISSING-U
TYPED-VARIABLE ACRT-JSON-A ptr u8
TYPED-VARIABLE ACRT-ERR-A ptr u8

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

: ACRT-JSON-A@ ( -- ptr u8 )
   ACRT-JSON-A @ ;

: ACRT-ERR-A@ ( -- ptr u8 )
   ACRT-ERR-A @ ;

: ACRT-JSON-A! ( ptr u8 -- )
   ACRT-JSON-A ! ;

: ACRT-ERR-A! ( ptr u8 -- )
   ACRT-ERR-A ! ;

: ACRT-JSON ( -- ptr u8 )
   ACRT-JSON-A@ 0= if
      ACRT-JSON-CAP MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop ACRT-JSON-A!
   then
   ACRT-JSON-A@ ;

: ACRT-ERR ( -- ptr u8 )
   ACRT-ERR-A@ 0= if
      ACRT-ERR-CAP MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop ACRT-ERR-A!
   then
   ACRT-ERR-A@ ;

: ACRT-PATH! ( ptr u8 n -- ) {: a:ptr u :}
   a ACRT-PATH u ACRT-COPY
   0 ACRT-PATH u + c! ;

: ACRT-SMALL$ ( -- ptr u8 n )
   ACRT-SMALL-PATH ACRT-SMALL-U @ ;

: ACRT-BOUNDARY$ ( -- ptr u8 n )
   ACRT-BOUNDARY-PATH ACRT-BOUNDARY-U @ ;

: ACRT-MISSING$ ( -- ptr u8 n )
   ACRT-MISSING-PATH ACRT-MISSING-U @ ;

: ACRT-PREPARE ( -- )
   CLEANUP-RESET
   s" habu-aot-report" HB-TMP-MKDIR 2dup CLEANUP-TREE+
   {: root:ptr rootu:n :}
   root rootu s" small.bin" ACRT-SMALL-PATH JOIN-PATH ACRT-SMALL-U !
   root rootu s" boundary.bin" ACRT-BOUNDARY-PATH JOIN-PATH ACRT-BOUNDARY-U !
   root rootu s" missing.bin" ACRT-MISSING-PATH JOIN-PATH ACRT-MISSING-U ! ;

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

: ACRT-CAPTURE>N ( result<pcap:captured,pcap:failed> -- n n n )   \ outn errn code (0 on clean exit)
   MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE {: o:len e:len :} o LEN>N e LEN>N 0 ENDOF
     err OF PCAP-FAILED:UNMAKE  {: o:len e:len c:rc :} o LEN>N e LEN>N c RC>N ENDOF
   ;MATCH ;

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
   ACRT-SMALL$ 43 ACRT-WRITE
   ACRT-SMALL$ ACRT-COUNT-FILE
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
   ACRT-BOUNDARY$ $4010 ACRT-WRITE
   ACRT-BOUNDARY$ ACRT-COUNT-FILE
   2 ACRT=
   1 ACRT=
   $4010 ACRT= ;

\ The report buffer is the caller's and cannot grow: a report that outgrows it
\ is refused by name, and the input the scan had open is closed, so the lowest
\ free descriptor is the same before and after. The short buffer ends four bytes
\ before the whole report, inside the stencil-site list the last scan writes,
\ and keeps the report up to the cut.
variable ACRT-SHORT-CAP

: ACRT-SHORT-REPORT ( -- )
   ACRT-SMALL$ ACRT-ERR ACRT-SHORT-CAP @ REPORT-JSON-BUFFER 2drop ;

: ACRT-NEXT-FD ( -- n )
   ACRT-SMALL$ ACRT-PATH!
   ACRT-PATH 0 0 open dup close ;

: ACRT-TEST-FULL ( -- )
   ACRT-SMALL$ ACRT-JSON ACRT-JSON-CAP REPORT-JSON-BUFFER nip
   4 - ACRT-SHORT-CAP !
   ACRT-NEXT-FD {: fd:n :}
   [: ACRT-SHORT-REPORT ;] catch E-JW-CAPACITY ACRT=
   REPORT-OUT$ ACRT-JSON ACRT-SHORT-CAP @ STR= ACRT-ASSERT
   ACRT-NEXT-FD fd ACRT= ;

\ Misuse is refused by a named throw the caller reports, never by ending the
\ process. E-FS-PATH: the shortest length the path buffer cannot hold with its
\ NUL, the largest cell (it wraps when one is added) and a negative length.
\ E-JW-OUTPUT: a null output buffer or a negative capacity. E-FMT-DOMAIN: a
\ negative number. E-FS-OPEN: an input that does not open. E-FS-IO: one that
\ opens but does not read, a directory, its descriptor closed after.
: ACRT-TEST-MISUSE ( -- )
   [: ACRT-BUF REPORT-PATH-CAP REPORT-FILE! ;] catch E-FS-PATH ACRT=
   [: ACRT-BUF -1 REPORT-FILE! ;] catch E-FS-PATH ACRT=
   [: ACRT-BUF STR-MAX-I64 REPORT-FILE! ;] catch E-FS-PATH ACRT=
   [: NULL$ REPORT-BUFFER! ;] catch E-JW-OUTPUT ACRT=
   [: ACRT-ERR -1 REPORT-BUFFER! ;] catch E-JW-OUTPUT ACRT=
   [: -1 JSON-NUM ;] catch E-FMT-DOMAIN ACRT=
   [: ACRT-MISSING$ REPORT-FILE! REPORT-COUNT ;] catch E-FS-OPEN ACRT=
   ACRT-NEXT-FD {: fd:n :}
   [: s" ." REPORT-FILE! REPORT-COUNT ;] catch E-FS-IO ACRT=
   ACRT-NEXT-FD fd ACRT= ;

\ The CLI ends a refused report with exit 74 and the refusal in words.
: ACRT-CLI-REFUSED ( ptr u8 n ptr u8 n -- )
   {: path:ptr pathu:n msg:ptr msgu:n :}
   path pathu ACRT-CLI-RUN
   {: outu:n erru:n rc:n :}
   rc 74 ACRT=
   outu 0 ACRT=
   ACRT-ERR erru 1- msg msgu STR= ACRT-ASSERT ;

: ACRT-TEST-CLI-REFUSED ( -- )
   ACRT-MISSING$ s" aot-call-report: cannot open input" ACRT-CLI-REFUSED
   s" ." s" aot-call-report: read failed" ACRT-CLI-REFUSED ;

: ACRT-TEST-CLI ( -- )
   ACRT-SMALL$ ACRT-CLI-RUN
   {: outu:n erru:n rc:n :}
   rc 0 ACRT=
   erru 0 ACRT=
   outu 0 > ACRT-ASSERT
   ACRT-JSON outu 1- + c@ STR-LF ACRT= ;

: ACRT-MAIN ( -- )
   1 ACRT-N !
   ACRT-PREPARE
   ACRT-TEST-SMALL
   ACRT-TEST-BOUNDARY
   ACRT-TEST-FULL
   ACRT-TEST-MISUSE
   ACRT-TEST-CLI
   ACRT-TEST-CLI-REFUSED
   CLEANUP-RUN
   s" aot-call-report-test: ok (" type ACRT-N @ 1- FMT:.INT s"  assertions)" type cr ;

;using

ACRT-MAIN
