\ aot-call-report.f — report patched AOT call stencils in a binary.
\ Run: bin/hb tools/aot-call-report.f binary
0 set-check

$D503201F constant NOP-INSTR
$FC000000 constant BL-MASK
$94000000 constant BL-OP

4 constant WORD-BYTES
12 constant STENCIL-PADDING-BYTES
16 constant STENCIL-BYTES
15 constant SCAN-CARRY-CAP
$4000 constant SCAN-READ-CAP
SCAN-READ-CAP SCAN-CARRY-CAP + constant SCAN-BUF-CAP
1024 constant REPORT-PATH-CAP
32 constant JSON-NUM-CAP

9 constant C-TAB
10 constant C-LF
13 constant C-CR
32 constant C-SP
34 constant C-DQ
44 constant C-COMMA
48 constant C-ZERO
58 constant C-COLON
91 constant C-LBRACK
92 constant C-BACKSLASH
93 constant C-RBRACK
102 constant C-F
110 constant C-N
114 constant C-R
116 constant C-T
117 constant C-U
123 constant C-LBRACE
125 constant C-RBRACE

0 constant SCAN-COUNT
1 constant SCAN-DIRECT-SITES
2 constant SCAN-STENCIL-SITES

create REPORT-PATH REPORT-PATH-CAP allot
create SCAN-BUF SCAN-BUF-CAP allot
create OUT-BYTE 1 allot
create JSON-NUM-BUF JSON-NUM-CAP allot

variable FILE-A
variable FILE-U
variable SCAN-FD
variable SCAN-GOT
variable SCAN-FILE-OFF
variable SCAN-BASE
variable SCAN-LEN
variable SCAN-CARRY
variable SCAN-NEXT-BL
variable SCAN-NEXT-STENCIL
variable SCAN-MODE
variable REPORT-BYTES
variable REPORT-STENCILS
variable REPORT-BLS
variable ARRAY-FIRST
variable JSON-NUM-V
variable JSON-NUM-N

: COPY-BYTES {: a dst u :} ( a dst u -- )
   0 begin dup u < while
      dup a + c@  over dst + c!
      1+
   repeat drop ;

: REPORT-FILE! {: a u :} ( a u -- )
   a FILE-A !
   u FILE-U ! ;

: PATHZ {: a u :} ( a u -- z )
   u 1+ REPORT-PATH-CAP > if s" aot-call-report: path too long" 74 die then
   a REPORT-PATH u COPY-BYTES
   0 REPORT-PATH u + c!
   REPORT-PATH ;

: OUT-C {: c :} ( c -- )
   c OUT-BYTE c!
   OUT-BYTE 1 type ;

: JSON-NIBBLE {: n :} ( n -- c )
   n 10 < if n C-ZERO + else n 55 + then ;

: JSON-U00 {: c :} ( c -- )
   C-BACKSLASH OUT-C
   C-U OUT-C
   C-ZERO OUT-C
   C-ZERO OUT-C
   c 4 rshift JSON-NIBBLE OUT-C
   c $F and JSON-NIBBLE OUT-C ;

: JSON-ESC-C {: c :} ( c -- )
   c C-DQ = if C-BACKSLASH OUT-C C-DQ OUT-C exit then
   c C-BACKSLASH = if C-BACKSLASH OUT-C C-BACKSLASH OUT-C exit then
   c C-LF = if C-BACKSLASH OUT-C C-N OUT-C exit then
   c C-CR = if C-BACKSLASH OUT-C C-R OUT-C exit then
   c C-TAB = if C-BACKSLASH OUT-C C-T OUT-C exit then
   c C-SP < if c JSON-U00 exit then
   c OUT-C ;

: JSON-STRING {: a u :} ( a u -- )
   C-DQ OUT-C
   0 begin dup u < while
      dup a + c@ JSON-ESC-C
      1+
   repeat drop
   C-DQ OUT-C ;

: JSON-KEY ( a u -- )
   JSON-STRING
   C-COLON OUT-C ;

: JSON-NUM {: n :} ( n -- )
   n 0 < if s" aot-call-report: negative json number" 74 die then
   n JSON-NUM-V !
   0 JSON-NUM-N !
   JSON-NUM-V @ 0= if C-ZERO OUT-C exit then
   begin JSON-NUM-V @ 0 > while
      JSON-NUM-V @ 10 mod C-ZERO +  JSON-NUM-BUF JSON-NUM-N @ + c!
      JSON-NUM-N @ 1+ JSON-NUM-N !
      JSON-NUM-N @ JSON-NUM-CAP > if s" aot-call-report: number buffer full" 74 die then
      JSON-NUM-V @ 10 / JSON-NUM-V !
   repeat
   begin JSON-NUM-N @ 0 > while
      JSON-NUM-N @ 1- JSON-NUM-N !
      JSON-NUM-BUF JSON-NUM-N @ + c@ OUT-C
   repeat ;

: ARRAY-NUM {: n :} ( n -- )
   ARRAY-FIRST @ if
      0 ARRAY-FIRST !
   else
      C-COMMA OUT-C
   then
   n JSON-NUM ;

: LE32@ {: p :} ( p -- u )
   p c@  p 1+ c@ 8 lshift or
   p 2 + c@ 16 lshift or
   p 3 + c@ 24 lshift or ;

: BL? ( u -- f )
   BL-MASK and BL-OP = ;

: STENCIL? {: p :} ( p -- f )
   p LE32@ NOP-INSTR =
   p WORD-BYTES + LE32@ NOP-INSTR = and
   p WORD-BYTES 2 * + LE32@ NOP-INSTR = and
   p WORD-BYTES 3 * + LE32@ BL? and ;

: SCAN-END ( -- n )
   SCAN-BASE @ SCAN-LEN @ + ;

: SCAN-ADDR {: off :} ( off -- a )
   SCAN-BUF off SCAN-BASE @ - + ;

: MATCH-BL {: off :} ( off -- )
   SCAN-MODE @ SCAN-COUNT = if
      REPORT-BLS @ 1+ REPORT-BLS !
      exit
   then
   SCAN-MODE @ SCAN-DIRECT-SITES = if off ARRAY-NUM then ;

: MATCH-STENCIL {: off :} ( off -- )
   SCAN-MODE @ SCAN-COUNT = if
      REPORT-STENCILS @ 1+ REPORT-STENCILS !
      exit
   then
   SCAN-MODE @ SCAN-STENCIL-SITES = if off ARRAY-NUM then ;

: PROCESS-BLS ( -- )
   begin SCAN-NEXT-BL @ SCAN-BASE @ < while
      SCAN-NEXT-BL @ WORD-BYTES + SCAN-NEXT-BL !
   repeat
   begin SCAN-NEXT-BL @ WORD-BYTES + SCAN-END <= while
      SCAN-NEXT-BL @ SCAN-ADDR LE32@ BL? if SCAN-NEXT-BL @ MATCH-BL then
      SCAN-NEXT-BL @ WORD-BYTES + SCAN-NEXT-BL !
   repeat ;

: PROCESS-STENCILS ( -- )
   begin SCAN-NEXT-STENCIL @ SCAN-BASE @ < while
      SCAN-NEXT-STENCIL @ WORD-BYTES + SCAN-NEXT-STENCIL !
   repeat
   begin SCAN-NEXT-STENCIL @ STENCIL-BYTES + SCAN-END <= while
      SCAN-NEXT-STENCIL @ SCAN-ADDR STENCIL? if SCAN-NEXT-STENCIL @ MATCH-STENCIL then
      SCAN-NEXT-STENCIL @ WORD-BYTES + SCAN-NEXT-STENCIL !
   repeat ;

: PROCESS-CHUNK ( -- )
   PROCESS-BLS
   PROCESS-STENCILS ;

: SAVE-CARRY ( -- )
   SCAN-LEN @ SCAN-CARRY-CAP < if SCAN-LEN @ else SCAN-CARRY-CAP then SCAN-CARRY !
   SCAN-CARRY @ 0 > if
      SCAN-BUF SCAN-LEN @ SCAN-CARRY @ - +  SCAN-BUF  SCAN-CARRY @ COPY-BYTES
   then ;

: OPEN-INPUT ( -- fd )
   FILE-A @ FILE-U @ PATHZ 0 0 open
   dup 0 < if s" aot-call-report: cannot open input" 74 die then ;

: SCAN-RESET ( -- )
   0 SCAN-FILE-OFF !
   0 SCAN-CARRY !
   0 SCAN-NEXT-BL !
   0 SCAN-NEXT-STENCIL ! ;

: SCAN-ONE-READ ( -- more )
   SCAN-FD @  SCAN-BUF SCAN-CARRY @ +  SCAN-READ-CAP read SCAN-GOT !
   SCAN-GOT @ 0 < if s" aot-call-report: read failed" 74 die then
   SCAN-GOT @ 0= if 0 exit then
   SCAN-FILE-OFF @ SCAN-CARRY @ - SCAN-BASE !
   SCAN-CARRY @ SCAN-GOT @ + SCAN-LEN !
   SCAN-MODE @ SCAN-COUNT = if
      REPORT-BYTES @ SCAN-GOT @ + REPORT-BYTES !
   then
   PROCESS-CHUNK
   SCAN-FILE-OFF @ SCAN-GOT @ + SCAN-FILE-OFF !
   SAVE-CARRY
   -1 ;

: SCAN-FILE {: mode :} ( mode -- )
   mode SCAN-MODE !
   SCAN-RESET
   OPEN-INPUT SCAN-FD !
   begin SCAN-ONE-READ while repeat
   SCAN-FD @ close ;

: REPORT-COUNT ( -- )
   0 REPORT-BYTES !
   0 REPORT-STENCILS !
   0 REPORT-BLS !
   SCAN-COUNT SCAN-FILE ;

: EMIT-DIRECT-SITES ( -- )
   -1 ARRAY-FIRST !
   C-LBRACK OUT-C
   SCAN-DIRECT-SITES SCAN-FILE
   C-RBRACK OUT-C ;

: EMIT-STENCIL-SITES ( -- )
   -1 ARRAY-FIRST !
   C-LBRACK OUT-C
   SCAN-STENCIL-SITES SCAN-FILE
   C-RBRACK OUT-C ;

: JSON-FIELD-COMMA ( -- )
   C-COMMA OUT-C ;

: REPORT-JSON ( -- )
   REPORT-COUNT
   C-LBRACE OUT-C
   s" schema_version" JSON-KEY 1 JSON-NUM JSON-FIELD-COMMA
   s" file" JSON-KEY FILE-A @ FILE-U @ JSON-STRING JSON-FIELD-COMMA
   s" file_bytes" JSON-KEY REPORT-BYTES @ JSON-NUM JSON-FIELD-COMMA
   s" patched_call_stencils" JSON-KEY REPORT-STENCILS @ JSON-NUM JSON-FIELD-COMMA
   s" padding_bytes" JSON-KEY REPORT-STENCILS @ STENCIL-PADDING-BYTES * JSON-NUM JSON-FIELD-COMMA
   s" compact_call_bytes" JSON-KEY REPORT-STENCILS @ WORD-BYTES * JSON-NUM JSON-FIELD-COMMA
   s" direct_bl_instructions" JSON-KEY REPORT-BLS @ JSON-NUM JSON-FIELD-COMMA
   s" direct_bl_sites" JSON-KEY EMIT-DIRECT-SITES JSON-FIELD-COMMA
   s" sites" JSON-KEY EMIT-STENCIL-SITES
   C-RBRACE OUT-C
   C-LF OUT-C ;

: USAGE ( -- )
   s" usage: tools/aot-call-report.f binary" 64 die ;

: REPORT-MAIN ( -- )
   SCRIPT-ARGC 1 <> if USAGE then
   0 SCRIPT-ARGV$ REPORT-FILE!
   REPORT-JSON ;

: MAYBE-REPORT-MAIN ( -- )
   ARGC 1 > if REPORT-MAIN then ;

MAYBE-REPORT-MAIN
