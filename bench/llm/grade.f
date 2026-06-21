\ grade.f - native isolated candidate grader.
\
\ Load after lib/errors.f, lib/string.f, lib/fs.f, lib/fs-mutate.f,
\ lib/process.f, and lib/process-argv.f.

64 constant GR-USAGE-RC
70 constant GR-REJECT-RC
1000 constant GR-MS-PER-S
32768 constant GR-CAPTURE-CAP
$20000 constant GR-FILE-CAP
10 constant GR-LF
34 constant GR-DQ

create GR-ROOT-BUF FS-PATH-CAP allot
create GR-PROG-BUF FS-PATH-CAP allot
create GR-FILE-BUF GR-FILE-CAP allot
create GR-OUT-BUF GR-CAPTURE-CAP allot
create GR-ERR-BUF GR-CAPTURE-CAP allot
create GR-LF-BUF 1 allot
create GR-DQ-BUF 1 allot

variable GR-ROOT-U
variable GR-PROG-U
variable GR-OUT-U
variable GR-ERR-U
variable GR-KIND
variable GR-CODE

GR-LF GR-LF-BUF c!
GR-DQ GR-DQ-BUF c!

: GR-ROOT$ ( -- ptr u8 n )
   GR-ROOT-BUF GR-ROOT-U @ ;

: GR-PROG$ ( -- ptr u8 n )
   GR-PROG-BUF GR-PROG-U @ ;

: GR-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u dst:ptr up:ptr :}
   u 0 < if E-FS-PATH throw then
   u FS-PATH-CAP > if E-FS-PATH throw then
   a dst u BYTE-COPY
   u up ! ;

: GR-PATH! ( ptr u8 n ptr u8 n ptr u8 ptr n -- ) {: pa:ptr pu na:ptr nu dst:ptr up:ptr :}
   pa pu na nu dst JOIN-PATH up ! ;

: GR-RESET ( -- )
   0 GR-ROOT-U !
   0 GR-PROG-U !
   0 GR-OUT-U !
   0 GR-ERR-U !
   PROC-OUTCOME-EXIT GR-KIND !
   0 GR-CODE ! ;

: GR-APPEND ( ptr u8 n -- ) {: a:ptr u :}
   GR-PROG$ a u APPEND-FILE ;

: GR-LINE ( ptr u8 n -- )
   GR-APPEND
   GR-LF-BUF 1 GR-APPEND ;

: GR-QUOTE ( -- )
   GR-DQ-BUF 1 GR-APPEND ;

: GR-APPEND-FILE ( ptr u8 n -- ) {: path:ptr pathu :}
   path pathu GR-FILE-BUF GR-FILE-CAP READ-ALL {: u :}
   GR-FILE-BUF u GR-LINE ;

: GR-HARNESS ( -- )
   s" variable #BAD 0 #BAD !" GR-LINE
   s" variable AP variable BP" GR-LINE
   s" : G= ( n n -- ) <> if 1 #BAD +! then ;" GR-LINE
   s" : GRADE-REPORT ( -- ) #BAD @ 0= if exit then s" GR-APPEND
   GR-QUOTE
   s"  grade failed" GR-APPEND
   GR-QUOTE
   s"  1 die ;" GR-LINE ;

: GR-PREPARE ( -- )
   GR-RESET
   s" habu-grade" TMPDIR-MKDIR GR-ROOT-BUF GR-ROOT-U GR-COPY!
   GR-ROOT$ s" prog.f" GR-PROG-BUF GR-PROG-U GR-PATH! ;

: GR-CLEANUP ( -- )
   GR-ROOT$ EXISTS? if GR-ROOT$ REMOVE-TREE then ;

: GR-BUILD-PROGRAM ( bool ptr u8 n ptr u8 n -- ) {: nocheck cand:ptr candu vec:ptr vecu :}
   GR-PROG$ s" " WRITE-ALL
   nocheck if s" 0 set-check" GR-LINE then
   cand candu GR-APPEND-FILE
   nocheck if s" ' HB-CHECK-HOOK set-check" GR-LINE then
   GR-HARNESS
   vec vecu GR-APPEND-FILE
   s" GRADE-REPORT" GR-LINE ;

: GR-RUN-CHILD ( n -- ) {: timeout :}
   PROC-ARGV-RESET
   GR-PROG$ PROC-ARGV+
   s" bin/hb" GR-OUT-BUF GR-CAPTURE-CAP GR-ERR-BUF GR-CAPTURE-CAP timeout
   RUN-ARGV-CAPTURE-OUTCOME {: outu erru kind code :}
   outu GR-OUT-U !
   erru GR-ERR-U !
   kind GR-KIND !
   code GR-CODE ! ;

: GR-OUTCOME$ ( -- ptr u8 n )
   GR-KIND @ PROC-OUTCOME-TIMEOUT = if s" timeout" exit then
   GR-KIND @ PROC-OUTCOME-SIGNAL = if s" trap" exit then
   GR-KIND @ PROC-OUTCOME-EXIT <> if s" error" exit then
   GR-CODE @ 0 = if s" pass" exit then
   GR-CODE @ 1 = if s" fail" exit then
   GR-CODE @ GR-REJECT-RC = if s" reject" exit then
   GR-CODE @ 128 >= if s" trap" exit then
   s" error" ;

: GR-RUN-FILES ( bool n ptr u8 n ptr u8 n -- ptr u8 n ) {: nocheck timeout cand:ptr candu vec:ptr vecu :}
   timeout 0 <= if s" error" exit then
   cand candu FILE? 0= if s" error" exit then
   vec vecu FILE? 0= if s" error" exit then
   GR-PREPARE
   nocheck cand candu vec vecu GR-BUILD-PROGRAM
   timeout GR-RUN-CHILD
   GR-CLEANUP
   GR-OUTCOME$ ;

: GR-PARSE-U ( ptr u8 n -- n bool )
   STR>NUMBER? 0= if drop 0 STR-FALSE exit then
   dup 0 <= if drop 0 STR-FALSE exit then
   STR-TRUE ;

: GR-ARG$ ( n -- ptr u8 n )
   SCRIPT-ARGV$ ;

: GR-USAGE ( -- )
   s" usage: bench/llm/grade.f [--no-check] timeout_secs candidate.f vectors.f" GR-USAGE-RC die ;

: GR-TIMEOUT-ARG ( n -- n )
   GR-ARG$ GR-PARSE-U 0= if GR-USAGE then
   GR-MS-PER-S * ;

: GR-CLI-NOCHECK ( -- )
   0 GR-ARG$ s" --no-check" STR= 0= if GR-USAGE then
   STR-TRUE 1 GR-TIMEOUT-ARG 2 GR-ARG$ 3 GR-ARG$ GR-RUN-FILES type cr ;

: GR-CLI-CHECK ( -- )
   STR-FALSE 0 GR-TIMEOUT-ARG 1 GR-ARG$ 2 GR-ARG$ GR-RUN-FILES type cr ;

: GR-CLI ( -- )
   SCRIPT-ARGC 3 <> SCRIPT-ARGC 4 <> and if GR-USAGE then
   SCRIPT-ARGC 4 = if GR-CLI-NOCHECK exit then
   GR-CLI-CHECK ;

: GR-AUTO ( -- )
   SCRIPT-ARGC 0 > if GR-CLI then ;

GR-AUTO
