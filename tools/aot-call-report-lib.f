\ aot-call-report-lib.f - report patched AOT call stencils in a binary.
\ Load before tools/aot-call-report.f or tools/aot-call-report-test.f.
\ The gates load it in process, so it refuses misuse by a named throw its
\ caller reports; only REPORT-MAIN, the CLI boundary, ends the process.

require lib/errors.f

package AOT-CALL-REPORT

$D503201F constant NOP-INSTR
$FC000000 constant BL-MASK
$94000000 constant BL-OP

4 constant WORD-BYTES
12 constant STENCIL-PADDING-BYTES
16 constant STENCIL-BYTES
15 constant ACR-CARRY-CAP
$4000 constant ACR-READ-CAP
ACR-READ-CAP ACR-CARRY-CAP + constant ACR-BUF-CAP
PATH-CAP 1 + constant REPORT-PATH-CAP
\ A cell has at most 19 decimal digits, so JSON-NUM fills JSON-NUM-BUF unchecked.
32 constant JSON-NUM-CAP

9 constant ACR-C-TAB
10 constant ACR-C-LF
13 constant ACR-C-CR
32 constant ACR-C-SP
34 constant ACR-C-DQ
44 constant ACR-C-COMMA
48 constant ACR-C-ZERO
58 constant ACR-C-COLON
91 constant ACR-C-LBRACK
92 constant ACR-C-BACKSLASH
93 constant ACR-C-RBRACK
102 constant ACR-C-F
110 constant ACR-C-N
114 constant ACR-C-R
116 constant ACR-C-T
117 constant ACR-C-U
123 constant ACR-C-LBRACE
125 constant ACR-C-RBRACE

0 constant ACR-MODE-COUNT
1 constant ACR-MODE-DIRECT
2 constant ACR-MODE-STENCIL

create REPORT-PATH REPORT-PATH-CAP allot
create ACR-BUF ACR-BUF-CAP allot
create OUT-BYTE 1 allot
create JSON-NUM-BUF JSON-NUM-CAP allot

variable REPORT-PATH-U
TYPED-VARIABLE REPORT-OUT-A ptr u8
variable REPORT-OUT-CAP
variable REPORT-OUT-U
variable ACR-FD
variable ACR-GOT
variable ACR-FILE-OFF
variable ACR-BASE
variable ACR-LEN
variable ACR-CARRY
variable ACR-NEXT-BL
variable ACR-NEXT-STENCIL
variable ACR-MODE
variable REPORT-BYTES
variable REPORT-STENCILS
variable REPORT-BLS
variable ARRAY-FIRST
variable JSON-NUM-V
variable JSON-NUM-N

: REPORT-TRUE ( -- bool )
   0 0= ;

: REPORT-FALSE ( -- bool )
   0 0= 0= ;

: REPORT-OUT-A@ ( -- ptr u8 )
   REPORT-OUT-A @ ;

: REPORT-OUT-A! ( ptr u8 -- )
   REPORT-OUT-A ! ;

: REPORT-COPY-BYTES ( ptr u8 ptr u8 n -- ) {: a:ptr dst:ptr u:n :}
   0 begin dup u < while
      dup a + c@  over dst + c!
      1+
   repeat drop ;

\ A negative length, or one the path buffer cannot hold with its NUL, is a bad
\ path, refused as lib/fs.f refuses one, by E-FS-PATH. The length is compared
\ with the capacity, never added to, so no length wraps into range.
: REPORT-FILE! ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0 < if E-FS-PATH throw then
   u REPORT-PATH-CAP >= if E-FS-PATH throw then
   a REPORT-PATH u REPORT-COPY-BYTES
   0 REPORT-PATH u + c!
   u REPORT-PATH-U ! ;

: REPORT-FILE$ ( -- ptr u8 n )
   REPORT-PATH REPORT-PATH-U @ ;

: REPORT-PATH0 ( -- ptr u8 )
   REPORT-PATH ;

\ A null output buffer or a negative capacity is refused as the JSON writer
\ refuses one, by E-JW-OUTPUT.
: REPORT-BUFFER! ( ptr u8 n -- ) {: a:ptr cap:n :}
   a 0= if E-JW-OUTPUT throw then
   cap 0 < if E-JW-OUTPUT throw then
   a REPORT-OUT-A!
   cap REPORT-OUT-CAP !
   0 REPORT-OUT-U ! ;

: REPORT-BUFFER-OFF ( -- )
   NULL$ drop REPORT-OUT-A!
   0 REPORT-OUT-CAP !
   0 REPORT-OUT-U ! ;

: REPORT-OUT$ ( -- ptr u8 n )
   REPORT-OUT-A@ REPORT-OUT-U @ ;

: REPORT-BUFFERED? ( -- bool )
   REPORT-OUT-A@ 0= 0= ;

\ The report buffer is the caller's and cannot grow. A byte past its capacity
\ is refused as the JSON writer refuses fixed output that cannot hold a value,
\ by E-JW-CAPACITY, and the buffer keeps the report up to it.
: REPORT-BUF-C ( n -- )
   {: c:n :}
   REPORT-OUT-U @ REPORT-OUT-CAP @ >= if E-JW-CAPACITY throw then
   c REPORT-OUT-A@ REPORT-OUT-U @ + c!
   REPORT-OUT-U @ 1+ REPORT-OUT-U ! ;

: OUT-C ( n -- ) {: c:n :}
   REPORT-BUFFERED? if c REPORT-BUF-C exit then
   c OUT-BYTE c!
   OUT-BYTE 1 type ;

: JSON-NIBBLE ( n -- n ) {: n:n :}
   n 10 < if n ACR-C-ZERO + else n 55 + then ;

: JSON-U00 ( n -- ) {: c:n :}
   ACR-C-BACKSLASH OUT-C
   ACR-C-U OUT-C
   ACR-C-ZERO OUT-C
   ACR-C-ZERO OUT-C
   c 4 rshift JSON-NIBBLE OUT-C
   c $F and JSON-NIBBLE OUT-C ;

: JSON-ESC-C ( n -- ) {: c:n :}
   c ACR-C-DQ = if ACR-C-BACKSLASH OUT-C ACR-C-DQ OUT-C exit then
   c ACR-C-BACKSLASH = if ACR-C-BACKSLASH OUT-C ACR-C-BACKSLASH OUT-C exit then
   c ACR-C-LF = if ACR-C-BACKSLASH OUT-C ACR-C-N OUT-C exit then
   c ACR-C-CR = if ACR-C-BACKSLASH OUT-C ACR-C-R OUT-C exit then
   c ACR-C-TAB = if ACR-C-BACKSLASH OUT-C ACR-C-T OUT-C exit then
   c ACR-C-SP < if c JSON-U00 exit then
   c OUT-C ;

: JSON-STRING ( ptr u8 n -- ) {: a:ptr u:n :}
   ACR-C-DQ OUT-C
   0 begin dup u < while
      dup a + c@ JSON-ESC-C
      1+
   repeat drop
   ACR-C-DQ OUT-C ;

: JSON-KEY ( ptr u8 n -- )
   JSON-STRING
   ACR-C-COLON OUT-C ;

\ Every number in the report is a count or an offset: a negative one is refused
\ as the unsigned formatters refuse it, by E-FMT-DOMAIN.
: JSON-NUM ( n -- ) {: n:n :}
   n 0 < if E-FMT-DOMAIN throw then
   n JSON-NUM-V !
   0 JSON-NUM-N !
   JSON-NUM-V @ 0= if ACR-C-ZERO OUT-C exit then
   begin JSON-NUM-V @ 0 > while
      JSON-NUM-V @ 10 mod ACR-C-ZERO +  JSON-NUM-BUF JSON-NUM-N @ + c!
      JSON-NUM-N @ 1+ JSON-NUM-N !
      JSON-NUM-V @ 10 / JSON-NUM-V !
   repeat
   begin JSON-NUM-N @ 0 > while
      JSON-NUM-N @ 1- JSON-NUM-N !
      JSON-NUM-BUF JSON-NUM-N @ + c@ OUT-C
   repeat ;

: ARRAY-NUM ( n -- ) {: n:n :}
   ARRAY-FIRST @ if
      0 ARRAY-FIRST !
   else
      ACR-C-COMMA OUT-C
   then
   n JSON-NUM ;

: LE32@ ( ptr u8 -- n ) {: p:ptr :}
   p c@  p 1+ c@ 8 lshift or
   p 2 + c@ 16 lshift or
   p 3 + c@ 24 lshift or ;

: BL? ( n -- bool )
   BL-MASK and BL-OP = ;

: STENCIL? ( ptr u8 -- bool ) {: p:ptr :}
   p LE32@ NOP-INSTR =
   p WORD-BYTES + LE32@ NOP-INSTR = and
   p WORD-BYTES 2 * + LE32@ NOP-INSTR = and
   p WORD-BYTES 3 * + LE32@ BL? and ;

: ACR-SCAN-END ( -- n )
   ACR-BASE @ ACR-LEN @ + ;

: ACR-SCAN-ADDR ( n -- ptr u8 ) {: off:n :}
   ACR-BUF off ACR-BASE @ - + ;

: MATCH-BL ( n -- ) {: off:n :}
   ACR-MODE @ ACR-MODE-COUNT = if
      REPORT-BLS @ 1+ REPORT-BLS !
      exit
   then
   ACR-MODE @ ACR-MODE-DIRECT = if off ARRAY-NUM then ;

: MATCH-STENCIL ( n -- ) {: off:n :}
   ACR-MODE @ ACR-MODE-COUNT = if
      REPORT-STENCILS @ 1+ REPORT-STENCILS !
      exit
   then
   ACR-MODE @ ACR-MODE-STENCIL = if off ARRAY-NUM then ;

: PROCESS-BLS ( -- )
   begin ACR-NEXT-BL @ ACR-BASE @ < while
      ACR-NEXT-BL @ WORD-BYTES + ACR-NEXT-BL !
   repeat
   begin ACR-NEXT-BL @ WORD-BYTES + ACR-SCAN-END <= while
      ACR-NEXT-BL @ ACR-SCAN-ADDR LE32@ BL? if ACR-NEXT-BL @ MATCH-BL then
      ACR-NEXT-BL @ WORD-BYTES + ACR-NEXT-BL !
   repeat ;

: PROCESS-STENCILS ( -- )
   begin ACR-NEXT-STENCIL @ ACR-BASE @ < while
      ACR-NEXT-STENCIL @ WORD-BYTES + ACR-NEXT-STENCIL !
   repeat
   begin ACR-NEXT-STENCIL @ STENCIL-BYTES + ACR-SCAN-END <= while
      ACR-NEXT-STENCIL @ ACR-SCAN-ADDR STENCIL? if ACR-NEXT-STENCIL @ MATCH-STENCIL then
      ACR-NEXT-STENCIL @ WORD-BYTES + ACR-NEXT-STENCIL !
   repeat ;

: PROCESS-CHUNK ( -- )
   PROCESS-BLS
   PROCESS-STENCILS ;

: SAVE-CARRY ( -- )
   ACR-LEN @ ACR-CARRY-CAP < if ACR-LEN @ else ACR-CARRY-CAP then ACR-CARRY !
   ACR-CARRY @ 0 > if
      ACR-BUF ACR-LEN @ ACR-CARRY @ - +  ACR-BUF  ACR-CARRY @ REPORT-COPY-BYTES
   then ;

: OPEN-INPUT ( -- n )
   REPORT-PATH0 0 0 open
   dup 0 < if E-FS-OPEN throw then ;

: ACR-SCAN-RESET ( -- )
   0 ACR-FILE-OFF !
   0 ACR-CARRY !
   0 ACR-NEXT-BL !
   0 ACR-NEXT-STENCIL ! ;

: ACR-SCAN-ONE-READ ( -- bool )
   ACR-FD @  ACR-BUF ACR-CARRY @ +  ACR-READ-CAP read ACR-GOT !
   ACR-GOT @ 0 < if E-FS-IO throw then
   ACR-GOT @ 0= if REPORT-FALSE exit then
   ACR-FILE-OFF @ ACR-CARRY @ - ACR-BASE !
   ACR-CARRY @ ACR-GOT @ + ACR-LEN !
   ACR-MODE @ ACR-MODE-COUNT = if
      REPORT-BYTES @ ACR-GOT @ + REPORT-BYTES !
   then
   PROCESS-CHUNK
   ACR-FILE-OFF @ ACR-GOT @ + ACR-FILE-OFF !
   SAVE-CARRY
   REPORT-TRUE ;

: ACR-SCAN-READS ( -- )
   begin ACR-SCAN-ONE-READ while repeat ;

: ACR-SCAN-CLOSE ( -- )
   ACR-FD @ close ;

\ A report refused mid-scan still closes the input.
: ACR-SCAN-FILE ( n -- )
   {: mode:n :}
   mode ACR-MODE !
   ACR-SCAN-RESET
   OPEN-INPUT ACR-FD !
   [: ACR-SCAN-READS ;] [: ACR-SCAN-CLOSE ;] finally ;

: REPORT-COUNT ( -- )
   0 REPORT-BYTES !
   0 REPORT-STENCILS !
   0 REPORT-BLS !
   ACR-MODE-COUNT ACR-SCAN-FILE ;

: EMIT-DIRECT-SITES ( -- )
   -1 ARRAY-FIRST !
   ACR-C-LBRACK OUT-C
   ACR-MODE-DIRECT ACR-SCAN-FILE
   ACR-C-RBRACK OUT-C ;

: EMIT-STENCIL-SITES ( -- )
   -1 ARRAY-FIRST !
   ACR-C-LBRACK OUT-C
   ACR-MODE-STENCIL ACR-SCAN-FILE
   ACR-C-RBRACK OUT-C ;

: JSON-FIELD-COMMA ( -- )
   ACR-C-COMMA OUT-C ;

: REPORT-JSON ( -- )
   REPORT-COUNT
   ACR-C-LBRACE OUT-C
   s" schema_version" JSON-KEY 1 JSON-NUM JSON-FIELD-COMMA
   s" file" JSON-KEY REPORT-FILE$ JSON-STRING JSON-FIELD-COMMA
   s" file_bytes" JSON-KEY REPORT-BYTES @ JSON-NUM JSON-FIELD-COMMA
   s" patched_call_stencils" JSON-KEY REPORT-STENCILS @ JSON-NUM JSON-FIELD-COMMA
   s" padding_bytes" JSON-KEY REPORT-STENCILS @ STENCIL-PADDING-BYTES * JSON-NUM JSON-FIELD-COMMA
   s" compact_call_bytes" JSON-KEY REPORT-STENCILS @ WORD-BYTES * JSON-NUM JSON-FIELD-COMMA
   s" direct_bl_instructions" JSON-KEY REPORT-BLS @ JSON-NUM JSON-FIELD-COMMA
   s" direct_bl_sites" JSON-KEY EMIT-DIRECT-SITES JSON-FIELD-COMMA
   s" sites" JSON-KEY EMIT-STENCIL-SITES
   ACR-C-RBRACE OUT-C
   ACR-C-LF OUT-C ;

: REPORT-JSON-BUFFER ( ptr u8 n ptr u8 n -- ptr u8 n )
   {: path:ptr pathu:n out:ptr cap:n :}
   path pathu REPORT-FILE!
   out cap REPORT-BUFFER!
   REPORT-JSON
   REPORT-OUT$ ;

: USAGE ( -- )
   s" usage: tools/aot-call-report.f binary" 64 die ;

\ The CLI ends a refused report with exit 74 and the refusal in words. Any
\ other code is a defect no command line can cause, and goes on to the engine.
: REPORT-REFUSED ( n -- )
   {: code:n :}
   code E-FS-PATH = if s" aot-call-report: path too long" 74 die then
   code E-FS-OPEN = if s" aot-call-report: cannot open input" 74 die then
   code E-FS-IO = if s" aot-call-report: read failed" 74 die then
   code throw ;

: REPORT-RUN ( -- )
   0 SCRIPT-ARGV$ REPORT-FILE!
   REPORT-BUFFER-OFF
   REPORT-JSON ;

: REPORT-MAIN ( -- )
   SCRIPT-ARGC 1 <> if USAGE then
   [: REPORT-RUN ;] catch
   dup 0= if drop exit then
   REPORT-REFUSED ;

\ What the CLI, the AOT gates and the focused test call; the rest stays private.
public
EXPORT REPORT-PATH-CAP
EXPORT REPORT-BYTES
EXPORT REPORT-STENCILS
EXPORT REPORT-BLS
EXPORT REPORT-FILE!
EXPORT REPORT-BUFFER!
EXPORT REPORT-OUT$
EXPORT JSON-NUM
EXPORT BL?
EXPORT REPORT-COUNT
EXPORT REPORT-JSON-BUFFER
EXPORT REPORT-MAIN
;package
