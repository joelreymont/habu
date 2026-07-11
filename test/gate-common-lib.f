\ gate-common.f - checked helpers for native gate runner slices.
\
\ Load after lib/errors.f, lib/string.f, lib/memory.f, lib/fs.f, lib/fs-mutate.f,
\ lib/process.f, lib/process-argv.f, lib/process-env.f, and
\ lib/test/runner.f, and lib/content-key.f.

require lib/process-fork.f

$40000 constant GE-SRC-CAP
64 constant GE-SRC-MAX
120000 constant GE-TIMEOUT-MS
10 constant GE-LF
32 constant GE-SP
34 constant GE-DQ
0 constant GE-F-DUPFD
10 constant GE-FD-SAVE-MIN
1 constant GE-STDOUT-FD
2 constant GE-STDERR-FD

s" UEND" s" -- ptr n" TRUST
s" USIGS-RESTORE-END" s" n --" TRUST
s" UTERM!" s" --" TRUST
s" JSON-DIAGS" s" -- ptr a" TRUST

create GE-SRC-BUF GE-SRC-CAP allot
create GE-SRC-A GE-SRC-MAX cells allot
create GE-SRC-LEN GE-SRC-MAX cells allot
create GE-ARGV-BUF GE-SRC-CAP allot
create GE-HB-BUF FS-PATH-CAP allot

variable GE-SRC-U
variable GE-SRC-N
variable GE-RD
variable GE-INFD
variable GE-ARGV-U
variable GE-HB-U
variable GE-EVAL-CP
variable GE-EVAL-NDICT
variable GE-EVAL-UEND
variable GE-EVAL-CURRENT
variable GE-EVAL-JSON-DIAGS
variable GE-EVAL-OUT-SAVE
variable GE-EVAL-ERR-SAVE
variable GE-EVAL-STACK-A

: GE-EVAL-STACK-A-FIELD ( -- ptr ptr u8 )
   GE-EVAL-STACK-A 0 ptr-field ;

: GE-EVAL-STACK@ ( -- ptr u8 )
   GE-EVAL-STACK-A-FIELD @ ;

: GE-EVAL-STACK! ( ptr u8 -- )
   GE-EVAL-STACK-A-FIELD ! ;

: GE-EVAL-STACK ( -- ptr u8 n )
   GE-EVAL-STACK@ 0= if MEM-ALLOC-64K drop GE-EVAL-STACK! then
   GE-EVAL-STACK@ 1 MEM-64K-BYTES ;

: GE-STORE-CAPTURE ( len len rc -- ) {: outu:len erru:len rc:rc :}
   rc RC>N GT-OUTCOME-CODE !
   PROC-OUTCOME-EXIT GT-OUTCOME-KIND !
   erru LEN>N GT-ERR-U !
   outu LEN>N GT-OUT-U ! ;

: GE-STORE-OUTCOME ( len len n n -- )
   GT-STORE-RUN ;

: GE-ARGV-RESET ( -- )
   0 GE-ARGV-U ! ;

: GE-ARGV-C ( n -- ) {: c:n :}
   GE-ARGV-U @ 1 + GE-SRC-CAP > if E-STR-CAPACITY throw then
   c GE-ARGV-BUF GE-ARGV-U @ + c!
   GE-ARGV-U @ 1+ GE-ARGV-U ! ;

: GE-ARGV+ ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0 < if E-STR-BOUNDS throw then
   GE-ARGV-U @ u + 3 + GE-SRC-CAP > if E-STR-CAPACITY throw then
   GE-SP GE-ARGV-C
   GE-SP GE-ARGV-C
   a GE-ARGV-BUF GE-ARGV-U @ + u BYTE-COPY
   GE-ARGV-U @ u + GE-ARGV-U !
   GE-LF GE-ARGV-C ;

: GE-ARGV$ ( -- ptr u8 n )
   GE-ARGV-BUF GE-ARGV-U @ ;

: GE-SPAWN-FAIL. ( pid -- ) {: pid:pid :}
   s" spawn raw code: " type pid PID>N . cr
   HB-TARGET-MACOS? if
      s" spawn errno: " type pid PID>N negate . cr
   then ;

: GE-SPAWN-CAPTURE ( ptr u8 ptr a ptr a -- ) {: pathz:ptr argv:ptr envp:ptr :}
   pathz argv envp -1 >FD PROC-OUT-W @ PROC-ERR-W @
   PROC-SPAWN-ARGV-ENV-RAW {: pid:pid :}
   PROC-ARGV-ENV-RESET
   pid PID>N 0 < if pid GE-SPAWN-FAIL. E-PROC-SPAWN PROC-THROW-CAPTURE then
   pid PROC-PID !
   PROC-OUT-W PROC-CLOSE-CELL
   PROC-ERR-W PROC-CLOSE-CELL ;

: GE-SPAWN-STDIN-CAPTURE ( ptr u8 ptr a ptr a -- ) {: pathz:ptr argv:ptr envp:ptr :}
   pathz argv envp PROC-IN-R @ PROC-OUT-W @ PROC-ERR-W @
   PROC-SPAWN-ARGV-ENV-RAW {: pid:pid :}
   PROC-ARGV-ENV-RESET
   pid PID>N 0 < if pid GE-SPAWN-FAIL. E-PROC-SPAWN PROC-THROW-CAPTURE then
   pid PROC-PID !
   PROC-IN-R PROC-CLOSE-CELL
   PROC-OUT-W PROC-CLOSE-CELL
   PROC-ERR-W PROC-CLOSE-CELL ;

: GE-RUN-ENV ( ptr u8 n n -- ) {: path:ptr pathu:n timeout:n :}
   path pathu GS-HELPER-EVENT
   PROC-ENV-INHERIT-MISSING
   path pathu >LEN PROC-ARGV-PREPARE {: pathz:ptr argv:ptr :}
   PROC-ENV-PREPARE {: envp:ptr :}
   timeout >MS PROC-CAPTURE-BEGIN
   pathz argv envp GE-SPAWN-CAPTURE
   GT-OUT-BUF GT-OUT-CAP >LEN GT-ERR-BUF GT-ERR-CAP >LEN
   PROC-RUN-CAPTURE-OUTCOME-LOOP
   PROC-CAPTURE-FINISH-OUTCOME GE-STORE-OUTCOME ;

: GE-RUN-STDIN ( ptr u8 n ptr u8 n n -- ) {: path:ptr pathu:n in:ptr inu:n timeout:n :}
   path pathu GS-HELPER-EVENT
   PROC-ENV-INHERIT-MISSING
   path pathu >LEN PROC-ARGV-PREPARE {: pathz:ptr argv:ptr :}
   PROC-ENV-PREPARE {: envp:ptr :}
   timeout >MS PROC-STDIN-CAPTURE-BEGIN
   pathz argv envp GE-SPAWN-STDIN-CAPTURE
   in inu >LEN GT-OUT-BUF GT-OUT-CAP >LEN GT-ERR-BUF GT-ERR-CAP >LEN
   PROC-RUN-STDIN-CAPTURE-OUTCOME-LOOP
   PROC-CAPTURE-FINISH-OUTCOME GE-STORE-OUTCOME ;

: GE-SPAWN-FILE-CAPTURE ( ptr u8 ptr a ptr a -- ) {: pathz:ptr argv:ptr envp:ptr :}
   pathz argv envp GE-INFD @ >FD PROC-OUT-W @ PROC-ERR-W @
   PROC-SPAWN-ARGV-ENV-RAW {: pid:pid :}
   PROC-ARGV-ENV-RESET
   GE-INFD @ close
   pid PID>N 0 < if pid GE-SPAWN-FAIL. E-PROC-SPAWN PROC-THROW-CAPTURE then
   pid PROC-PID !
   PROC-OUT-W PROC-CLOSE-CELL
   PROC-ERR-W PROC-CLOSE-CELL ;

: GE-RUN-STDIN-FILE ( ptr u8 n ptr u8 n n -- ) {: path:ptr pathu:n inpath:ptr inpathu:n timeout:n :}
   path pathu GS-HELPER-EVENT
   PROC-ENV-INHERIT-MISSING
   inpath inpathu FS-PATHZ open-rd GE-INFD !
   GE-INFD @ 0 < if E-FS-OPEN throw then
   GT-OUT-CAP >LEN GT-ERR-CAP >LEN PROC-CAPTURE-CHECK-CAPS
   path pathu >LEN PROC-ARGV-PREPARE {: pathz:ptr argv:ptr :}
   PROC-ENV-PREPARE {: envp:ptr :}
   timeout >MS PROC-CAPTURE-BEGIN
   pathz argv envp GE-SPAWN-FILE-CAPTURE
   GT-OUT-BUF GT-OUT-CAP >LEN GT-ERR-BUF GT-ERR-CAP >LEN PROC-RUN-CAPTURE-OUTCOME-LOOP
   PROC-CAPTURE-FINISH-OUTCOME GE-STORE-OUTCOME ;

: GE-OUTCOME. ( n -- ) {: kind:n :}
   kind case
      PROC-OUTCOME-EXIT of s" exit" type endof
      PROC-OUTCOME-SIGNAL of s" signal" type endof
      PROC-OUTCOME-TIMEOUT of s" timeout" type endof
      s" unknown" type
   endcase ;

: GE-RC-NAME. ( n -- ) {: rc:n :}
   rc case
      E-PROC-SPAWN of s" E-PROC-SPAWN" type endof
      E-PROC-WAIT of s" E-PROC-WAIT" type endof
      E-PROC-TIMEOUT of s" E-PROC-TIMEOUT" type endof
      E-PROC-OUTPUT of s" E-PROC-OUTPUT" type endof
      E-PROC-TRUNCATED of s" E-PROC-TRUNCATED" type endof
      E-PROC-ENV of s" E-PROC-ENV" type endof
      E-PROC-PATH of s" E-PROC-PATH" type endof
      E-FS-OPEN of s" E-FS-OPEN" type endof
      E-FS-CAPACITY of s" E-FS-CAPACITY" type endof
      E-STR-BOUNDS of s" E-STR-BOUNDS" type endof
      E-STR-CAPACITY of s" E-STR-CAPACITY" type endof
      60 of s" E-PROC-SPAWN" type endof
      59 of s" E-PROC-WAIT" type endof
      58 of s" E-PROC-TIMEOUT" type endof
      57 of s" E-PROC-OUTPUT" type endof
      56 of s" E-PROC-TRUNCATED" type endof
      55 of s" E-PROC-ENV" type endof
      54 of s" E-PROC-PATH" type endof
      70 of s" E-CHECK" type endof
      78 of s" E-DUP-DEFINITION" type endof
      202 of s" E-FS-OPEN" type endof
      198 of s" E-FS-CAPACITY" type endof
      104 of s" E-STR-BOUNDS" type endof
      103 of s" E-STR-CAPACITY" type endof
      s" unmapped" type
   endcase ;

: GE-THROW-REPORT ( n ptr u8 n -- ) {: rc:n label:ptr labelu:n :}
   rc 0= if exit then
   s" FAIL: " type label labelu type cr
   s" throw: " type rc .
   s" name: " type rc GE-RC-NAME. cr
   rc throw ;

: GE-PRINT-OUTCOME ( -- )
   s" outcome: " type GT-OUTCOME-KIND @ GE-OUTCOME.
   s"  code: " type GT-OUTCOME-CODE @ .
   s" rc: " type GT-RC@ . s" (" type GT-RC@ GE-RC-NAME. s" )" type cr ;

: GE-PRINT-CAPTURE-STATS ( -- )
   s" stdout bytes: " type GT-OUT$ nip . s" / " type GT-OUT-CAP . cr
   s" stderr bytes: " type GT-ERR$ nip . s" / " type GT-ERR-CAP . cr ;

: GE-FAIL ( ptr u8 n -- ) {: label:ptr labelu:n :}
   s" FAIL: " type label labelu type cr
   GE-PRINT-OUTCOME
   GE-PRINT-CAPTURE-STATS
   GE-ARGV$ nip 0 > if
      s" argv:" type cr
      GE-ARGV$ type
   then
   s" stdout:" type cr
   GT-OUT$ type
   s" stderr:" type cr
   GT-ERR$ type
   s" gate phase failed" 1 die ;

: GE-EXPECT-OK ( ptr u8 n -- ) {: label:ptr labelu:n :}
   GT-RC@ 0 <> if label labelu GE-FAIL then ;

: GE-EXPECT-RC ( n ptr u8 n -- ) {: want:n label:ptr labelu:n :}
   GT-RC@ want <> if label labelu GE-FAIL then ;

: GE-EXPECT-NONZERO ( ptr u8 n -- ) {: label:ptr labelu:n :}
   GT-RC@ 0= if label labelu GE-FAIL then ;

: GE-EXPECT-SILENT ( ptr u8 n -- ) {: label:ptr labelu:n :}
   GT-OUT$ nip 0 <> if label labelu GE-FAIL then
   GT-ERR$ nip 0 <> if label labelu GE-FAIL then ;

: GE-EXPECT-OUT ( ptr u8 n ptr u8 n -- ) {: want:ptr wantu:n label:ptr labelu:n :}
   GT-OUT$ want wantu STR= 0= if label labelu GE-FAIL then ;

: GE-EXPECT-OUT-HAS ( ptr u8 n ptr u8 n -- ) {: want:ptr wantu:n label:ptr labelu:n :}
   GT-OUT$ want wantu CONTAINS? 0= if label labelu GE-FAIL then ;

: GE-EXPECT-ERR-HAS ( ptr u8 n ptr u8 n -- ) {: want:ptr wantu:n label:ptr labelu:n :}
   GT-ERR$ want wantu CONTAINS? 0= if label labelu GE-FAIL then ;

: GE-EXPECT-ERR-LACKS ( ptr u8 n ptr u8 n -- ) {: want:ptr wantu:n label:ptr labelu:n :}
   GT-ERR$ want wantu CONTAINS? if label labelu GE-FAIL then ;

: GE-SB-LF ( -- )
   GE-LF SB-APPEND-C ;

: GE-OUT-LINE ( ptr u8 n -- )
   SB-APPEND
   GE-SB-LF ;

: GE-SRC-RESET ( -- )
   0 GE-SRC-U !
   0 GE-SRC-N ! ;

: GE-SRC$ ( n -- ptr u8 n ) {: idx:n :}
   idx 0 < if E-STR-BOUNDS throw then
   idx GE-SRC-N @ >= if E-STR-BOUNDS throw then
   idx cells GE-SRC-A + @
   idx cells GE-SRC-LEN + @ ;

: GE-SRC-PATH+ ( ptr u8 n -- ) {: path:ptr pathu:n :}
   GE-SRC-N @ GE-SRC-MAX >= if E-STR-CAPACITY throw then
   path GE-SRC-A GE-SRC-N @ cells + !
   pathu GE-SRC-LEN GE-SRC-N @ cells + !
   GE-SRC-N @ 1+ GE-SRC-N ! ;

: GE-SRC-C ( n -- ) {: c:n :}
   c 0 < if E-STR-BOUNDS throw then
   c STR-BYTE-MAX > if E-STR-BOUNDS throw then
   GE-SRC-U @ 1 + GE-SRC-CAP > if E-STR-CAPACITY throw then
   c GE-SRC-BUF GE-SRC-U @ + c!
   GE-SRC-U @ 1+ GE-SRC-U ! ;

: GE-SRC-U+ ( n -- ) {: n:n :}
   n 0 < if E-STR-BOUNDS throw then
   n 10 >= if n 10 / recurse then
   n 10 mod STR-ZERO + GE-SRC-C ;

: GE-SRC-REPEAT-C ( n n -- ) {: u:n c:n :}
   u 0 < if E-STR-BOUNDS throw then
   u 0 ?do c GE-SRC-C loop ;

: GE-SRC+ ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0 < if E-STR-BOUNDS throw then
   GE-SRC-U @ u + GE-SRC-CAP > if E-STR-CAPACITY throw then
   a GE-SRC-BUF GE-SRC-U @ + u BYTE-COPY
   GE-SRC-U @ u + GE-SRC-U ! ;

: GE-SRC-SP ( -- )
   GE-SP GE-SRC-C ;

: GE-SRC-LF ( -- )
   GE-LF GE-SRC-C ;

: GE-SRC-LINE ( ptr u8 n -- )
   GE-SRC+
   GE-SRC-LF ;

: GE-SRC-S" ( ptr u8 n -- ) {: a:ptr u:n :}
   s" s" GE-SRC+
   GE-DQ GE-SRC-C
   GE-SRC-SP
   a u GE-SRC+
   GE-DQ GE-SRC-C ;

: GE-SRC-CHECK-LINE ( ptr u8 n -- )
   GE-SRC-S"
   s"  CHECK! ." GE-SRC-LINE ;

: GE-SRC-FILE+ ( ptr u8 n -- ) {: path:ptr pathu:n :}
   path pathu GE-SRC-PATH+
   path pathu GE-SRC-BUF GE-SRC-U @ + GE-SRC-CAP GE-SRC-U @ -
   READ-ALL GE-RD !
   GE-SRC-U @ GE-RD @ + GE-SRC-U ! ;

: GE-ARG+ ( ptr u8 n -- )
   2dup GE-ARGV+
   >LEN PROC-ARGV+ ;

: GE-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u:n dst:ptr up:ptr :}
   u 0 < if E-FS-PATH throw then
   u FS-PATH-CAP > if E-FS-PATH throw then
   a dst u BYTE-COPY
   u up ! ;

\ typed-local-lint: allow-bare-local - q keeps the quotation effect from the stack signature.
: GE-FILES-WALK ( ptr a [ ptr u8 n -- ] -- ) {: p:ptr q :}
   p begin dup c@ 0= 0= while
      dup 1+ over c@ q execute
      dup c@ 1 + +
   repeat drop ;

: GE-FILES-RUN ( [ ptr u8 n -- ] ptr a -- )
   swap GE-FILES-WALK ;

: GE-FILES-END? ( ptr u8 n -- bool )
   s" ;GE-FILES" STR= ;

: GE-FILES-ITEM, ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0 < if E-STR-BOUNDS throw then
   u STR-BYTE-MAX > if E-STR-BOUNDS throw then
   u c,
   0 begin dup u < while
      dup a + c@ c,
      1+
   repeat drop ;

: GE-FILES-PARSE ( -- )
   begin
      parse-name dup 0= if 2drop E-STR-BOUNDS throw then
      2dup GE-FILES-END? if 2drop 0 c, exit then
      GE-FILES-ITEM,
   again ;

: GE-FILES: ( -- )
   create GE-FILES-PARSE
   does> ( [ ptr u8 n -- ] -- )
      GE-FILES-RUN ;

: GE-HB! ( ptr u8 n -- )
   GE-HB-BUF GE-HB-U GE-COPY! ;

: GE-HB-RESET ( -- )
   PROC-ARGV-ENV-RESET
   GE-ARGV-RESET ;

: GE-HB$ ( -- ptr u8 n )
   GE-HB-U @ 0 > if GE-HB-BUF GE-HB-U @ exit then
   s" HABU_UNDER_TEST" GETENV dup 0= if
      2drop s" bin/hb" exit
   then
   2dup EXECUTABLE? 0= if E-FS-OPEN throw then ;

: GE-CHECK-EXE ( -- ptr u8 n )
   GE-HB$ ;

: GE-CHECK-SUPPORT-ARGV ( -- )
   s" tools/date.f" GE-ARG+
   s" lib/errors.f" GE-ARG+
   s" lib/string.f" GE-ARG+
   s" lib/memory.f" GE-ARG+
   s" lib/vector.f" GE-ARG+
   s" lib/fs.f" GE-ARG+
   s" lib/fs-mutate.f" GE-ARG+
   s" lib/process.f" GE-ARG+
   s" lib/process-argv.f" GE-ARG+
   s" lib/source.f" GE-ARG+
   s" tools/lint/text.f" GE-ARG+
   s" tools/lint/token.f" GE-ARG+
   s" tools/lint/lib.f" GE-ARG+
   s" tools/lint/json-writer.f" GE-ARG+
   s" tools/lint/source-lex.f" GE-ARG+
   s" tools/diag-origin-core.f" GE-ARG+
   s" tools/json.f" GE-ARG+
   s" tools/json-only-core.f" GE-ARG+
   s" tools/signature-lint-core.f" GE-ARG+
   s" tools/checked-boundary-lint-core.f" GE-ARG+
   s" tools/reserved-name-lint-core.f" GE-ARG+
   s" tools/trust-lint-core.f" GE-ARG+
   s" tools/check-all-errors-core.f" GE-ARG+
   s" tools/argv.f" GE-ARG+
   s" tools/check-core.f" GE-ARG+ ;

: GE-BIN-HB-RUN ( ptr u8 n -- ) {: label:ptr labelu:n :}
   label labelu GT-PROGRESS-RUN
   label labelu GS-INNER-HB-EVENT
   label labelu GS-BOUNDARY-EVENT
   s" bin/hb" GE-TIMEOUT-MS GE-RUN-ENV
   label labelu GE-EXPECT-OK
   label labelu GT-PROGRESS-PASS ;

: GE-HB-RUN ( ptr u8 n -- ) {: label:ptr labelu:n :}
   label labelu GT-PROGRESS-RUN
   label labelu GS-INNER-HB-EVENT
   label labelu GS-BOUNDARY-EVENT
   GE-HB$ GE-TIMEOUT-MS GE-RUN-ENV
   label labelu GE-EXPECT-OK
   label labelu GT-PROGRESS-PASS ;

: GE-HB-RUN-STDIN ( ptr u8 n -- ) {: label:ptr labelu:n :}
   label labelu GT-PROGRESS-RUN
   label labelu GS-INNER-HB-STDIN-EVENT
   label labelu GS-BOUNDARY-EVENT
   GE-HB$ GE-SRC-BUF GE-SRC-U @ GE-TIMEOUT-MS GE-RUN-STDIN
   label labelu GE-EXPECT-OK
   label labelu GT-PROGRESS-PASS ;

: GE-HB-RUN-STDIN-NZ ( ptr u8 n -- ) {: label:ptr labelu:n :}
   label labelu GT-PROGRESS-RUN
   label labelu GS-INNER-HB-STDIN-EVENT
   label labelu GS-BOUNDARY-EVENT
   GE-HB$ GE-SRC-BUF GE-SRC-U @ GE-TIMEOUT-MS GE-RUN-STDIN
   label labelu GE-EXPECT-NONZERO
   label labelu GT-PROGRESS-PASS ;

: GE-EVAL-MARK ( -- )
   cp@ GE-EVAL-CP !
   ndict@ GE-EVAL-NDICT !
   UEND @ GE-EVAL-UEND !
   get-current GE-EVAL-CURRENT !
   JSON-DIAGS @ GE-EVAL-JSON-DIAGS ! ;

: GE-EVAL-FORGET ( -- )
   GE-EVAL-NDICT @ ndict!
   GE-EVAL-CP @ cp!
   GE-EVAL-UEND @ USIGS-RESTORE-END
   GE-EVAL-CURRENT @ set-current
   GE-EVAL-JSON-DIAGS @ JSON-DIAGS !
   UTERM! ;

: GE-EVAL-DUP-FD ( n -- n ) {: fd:n :}
   fd GE-F-DUPFD GE-FD-SAVE-MIN fcntl dup 0 < if E-PROC-OUTPUT throw then ;

: GE-EVAL-DUP2! ( n n -- ) {: src:n dst:n :}
   src dst dup2 dup 0 < if drop E-PROC-OUTPUT throw then drop ;

: GE-EVAL-REDIRECT! ( -- )
   GE-STDOUT-FD GE-EVAL-DUP-FD GE-EVAL-OUT-SAVE !
   GE-STDERR-FD GE-EVAL-DUP-FD GE-EVAL-ERR-SAVE !
   PROC-OUT-W @ FD>N GE-STDOUT-FD GE-EVAL-DUP2!
   PROC-ERR-W @ FD>N GE-STDERR-FD GE-EVAL-DUP2!
   PROC-OUT-W PROC-CLOSE-CELL
   PROC-ERR-W PROC-CLOSE-CELL ;

: GE-EVAL-RESTORE! ( -- )
   GE-EVAL-OUT-SAVE @ GE-STDOUT-FD GE-EVAL-DUP2!
   GE-EVAL-ERR-SAVE @ GE-STDERR-FD GE-EVAL-DUP2!
   GE-EVAL-OUT-SAVE @ close
   GE-EVAL-ERR-SAVE @ close
   -1 GE-EVAL-OUT-SAVE !
   -1 GE-EVAL-ERR-SAVE ! ;

TRUSTED: GE-EVAL-SOURCE-ACT ( -- )
   GE-SRC-BUF GE-SRC-U @ evaluate ;

: GE-EVAL-SOURCE-RUNSTACK ( -- )
   ['] GE-EVAL-SOURCE-ACT GE-EVAL-STACK run-in-stack ;

TRUSTED: GE-EVAL-SOURCE ( -- )
   GE-EVAL-STACK drop
   data-base S0-CELL + @ {: old:n :}
   GE-EVAL-STACK@ data-base S0-CELL + !
   [: GE-EVAL-SOURCE-RUNSTACK ;] catch {: rc:n :}
   old data-base S0-CELL + !
   rc 0 <> if rc throw then ;

\ Store a synthesized in-process outcome (exited rc) straight into the GT
\ runner state; the capture machine no longer stores a (kind code) pair to
\ forge, so the runner copy is the single synthesized-state home.
: GE-EVAL-STORE-RC ( n -- ) {: rc:n :}
   rc >RC PROC-RC !
   PROC-OUT-LEN @ PROC-ERR-LEN @ PROC-OUTCOME-EXIT rc GE-STORE-OUTCOME ;

: GE-EVAL-DRAIN ( -- )
   GT-OUT-BUF GT-OUT-CAP >LEN GT-ERR-BUF GT-ERR-CAP >LEN PROC-RUN-CAPTURE-LOOP ;

\ typed-local-lint: allow-bare-local - q is the captured action quotation.
: GE-CAPTURE-ACTION ( [ -- ] -- n ) {: q :}
   GE-TIMEOUT-MS >MS PROC-CAPTURE-BEGIN
   GE-EVAL-REDIRECT!
   q catch {: rc:n :}
   GE-EVAL-RESTORE!
   GE-EVAL-DRAIN
   PROC-CLOSE-ALL-CAPTURE-FDS
   PROC-CAPTURE-OUTCOME@ GE-STORE-OUTCOME
   rc ;

: GE-EVAL-CAPTURE ( -- )
   [: GE-EVAL-SOURCE ;] GE-CAPTURE-ACTION GE-EVAL-STORE-RC ;

: GE-EVAL-FORK-EXIT ( n -- )
   s" " rot die ;

: GE-EVAL-FORK-CHILD ( -- )
   GE-EVAL-REDIRECT!
   [: GE-EVAL-SOURCE ;] catch {: rc:n :}
   rc GE-EVAL-FORK-EXIT ;

: GE-EVAL-FORK-SPAWN ( -- )
   PROC-FORK-RAW {: pid:pid :}
   pid PID>N 0 < if E-PROC-SPAWN throw then
   pid PID>N 0= if GE-EVAL-FORK-CHILD then
   pid PROC-PID !
   PROC-OUT-W PROC-CLOSE-CELL
   PROC-ERR-W PROC-CLOSE-CELL ;

: GE-EVAL-FORK-CAPTURE ( -- )
   GE-TIMEOUT-MS >MS PROC-CAPTURE-BEGIN
   GE-EVAL-FORK-SPAWN
   GT-OUT-BUF GT-OUT-CAP >LEN GT-ERR-BUF GT-ERR-CAP >LEN
   PROC-RUN-CAPTURE-OUTCOME-LOOP
   PROC-CAPTURE-FINISH-OUTCOME GE-STORE-OUTCOME ;

: GE-EVAL-FORK-BAD ( n ptr u8 n ptr u8 n -- )
   {: rc:n needle:ptr needleu:n label:ptr labelu:n :}
   label labelu GT-PROGRESS-RUN
   s" fork-eval" GS-EVENT
   GE-EVAL-FORK-CAPTURE
   rc label labelu GE-EXPECT-RC
   needle needleu label labelu GE-EXPECT-ERR-HAS
   label labelu GT-PROGRESS-PASS ;

: GE-EVAL-RUN-STDIN ( ptr u8 n -- ) {: label:ptr labelu:n :}
   label labelu GT-PROGRESS-RUN
   s" inprocess-eval" GS-EVENT
   GE-EVAL-MARK
   GE-EVAL-CAPTURE
   GE-EVAL-FORGET
   label labelu GE-EXPECT-OK
   label labelu GT-PROGRESS-PASS ;

: GE-BIN-HB? ( ptr u8 n -- bool )
   s" bin/hb" STR= ;

: GE-REMOVE-BIN-OTHER ( ptr u8 n -- ) {: path:ptr pathu:n :}
   path pathu FILE? if
      path pathu GE-BIN-HB? 0= if path pathu REMOVE-FILE then
   then ;

: GE-CLEAN-BIN ( -- )
   s" bin" [: GE-REMOVE-BIN-OTHER ;] WALK-FILES ;

: GE-CHECK-ARGV ( -- )
   GE-HB-RESET
   s" --load" GE-ARG+
   GE-CHECK-SUPPORT-ARGV
   s" tools/check-main.f" GE-ARG+
   s" --" GE-ARG+ ;

: GE-CHECK-RUN ( ptr u8 n -- ) {: label:ptr labelu:n :}
   GE-CHECK-ARGV
   GE-CHECK-EXE GE-SRC-BUF GE-SRC-U @ GE-TIMEOUT-MS GE-RUN-STDIN
   label labelu GE-EXPECT-OK
   label labelu GE-EXPECT-SILENT ;

: GE-CHECK-RUN-BAD ( n ptr u8 n ptr u8 n -- )
   {: rc:n needle:ptr needleu:n label:ptr labelu:n :}
   GE-CHECK-ARGV
   GE-CHECK-EXE GE-SRC-BUF GE-SRC-U @ GE-TIMEOUT-MS GE-RUN-STDIN
   rc label labelu GE-EXPECT-RC
   needle needleu label labelu GE-EXPECT-ERR-HAS ;

: GE-CHECK-SRC-LIST ( ptr u8 n -- ) {: label:ptr labelu:n :}
   GE-CHECK-ARGV
   s" --source-list" GE-ARG+
   0 begin dup GE-SRC-N @ < while
      dup GE-SRC$ GE-ARG+
      1+
   repeat drop
   GE-CHECK-EXE GE-TIMEOUT-MS GE-RUN-ENV
   label labelu GE-EXPECT-OK
   label labelu GE-EXPECT-SILENT ;
