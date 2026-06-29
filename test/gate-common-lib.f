\ gate-common.f - checked helpers for native gate runner slices.
\
\ Load after lib/errors.f, lib/string.f, lib/memory.f, lib/fs.f, lib/fs-mutate.f,
\ lib/process.f, lib/process-argv.f, lib/process-env.f, and
\ lib/test-runner.f, and lib/content-key.f.


$40000 constant GE-SRC-CAP
64 constant GE-SRC-MAX
120000 constant GE-TIMEOUT-MS
10 constant GE-LF
32 constant GE-SP
34 constant GE-DQ

create GE-SRC-BUF GE-SRC-CAP allot
create GE-SRC-A GE-SRC-MAX cells allot
create GE-SRC-LEN GE-SRC-MAX cells allot
create GE-WARM-BUF FS-PATH-CAP allot
create GE-WARM-TRUST-BUF FS-PATH-CAP allot
create GE-WARM-STAMP-BUF FS-PATH-CAP allot
create GE-WARM-KEY-HEX 80 allot
create GE-WARM-STAMP-RD 80 allot
create GE-ARGV-BUF GE-SRC-CAP allot

variable GE-SRC-U
variable GE-SRC-N
variable GE-RD
variable GE-WARM-U
variable GE-WARM-TRUST-U
variable GE-WARM-STAMP-U
variable GE-WARM-READY
variable GE-INFD
variable GE-ARGV-U

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
   s" helper-spawn" GS-EVENT
   PROC-ENV-INHERIT-MISSING
   path pathu >LEN PROC-ARGV-PREPARE {: pathz:ptr argv:ptr :}
   PROC-ENV-PREPARE {: envp:ptr :}
   timeout >MS PROC-CAPTURE-BEGIN
   pathz argv envp GE-SPAWN-CAPTURE
   GT-OUT-BUF GT-OUT-CAP >LEN GT-ERR-BUF GT-ERR-CAP >LEN
   PROC-RUN-CAPTURE-OUTCOME-LOOP
   PROC-CAPTURE-FINISH-OUTCOME GE-STORE-OUTCOME ;

: GE-RUN-STDIN ( ptr u8 n ptr u8 n n -- ) {: path:ptr pathu:n in:ptr inu:n timeout:n :}
   s" helper-spawn" GS-EVENT
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
   s" helper-spawn" GS-EVENT
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

: GE-WARM$ ( -- ptr u8 n )
   GE-WARM-BUF GE-WARM-U @ ;

: GE-WARM-TRUST$ ( -- ptr u8 n )
   GE-WARM-TRUST-BUF GE-WARM-TRUST-U @ ;

: GE-WARM-ROOT ( -- ptr u8 n )
   s" HABU_GATE_WARM_ROOT" GETENV dup 0= if 2drop GT-ROOT exit then ;

: GE-SUFFIX! ( ptr u8 n ptr u8 n ptr u8 ptr n -- )
   {: a:ptr u:n suf:ptr su:n dst:ptr lenp:ptr :}
   u su + FS-PATH-CAP > if E-FS-PATH throw then
   a dst u BYTE-COPY
   suf dst u + su BYTE-COPY
   u su + lenp ! ;

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

\ typed-local-lint: allow-bare-local - q keeps the quotation effect from the stack signature.
: GE-FILES-WALK ( ptr a [ ptr u8 n -- ] -- ) {: p:ptr q :}
   p begin dup c@ 0= 0= while
      dup 1+ over c@ q execute
      dup c@ 1 + +
   repeat drop ;

: GE-FILES-RUN ( [ ptr u8 n -- ] ptr a -- )
   swap GE-FILES-WALK ;

: GE-FILES: ( -- )
   create GE-FILES-PARSE
   does> ( [ ptr u8 n -- ] -- )
      GE-FILES-RUN ;

GE-FILES: GE-WARM-BAKER-FILES
   lib/errors.f lib/string.f lib/memory.f lib/vector.f lib/fs.f
   lib/fs-mutate.f lib/process.f lib/process-argv.f lib/process-env.f
   lib/source.f lib/codesign.f tools/lint/text.f tools/lint/intern.f
   tools/lint/token.f tools/lint/lib.f tools/warm-image-lib.f
   tools/warm-image.f tools/public-signatures-core.f tools/public-signatures.f
;GE-FILES

GE-FILES: GE-CHECK-SUPPORT-FILES
   tools/date.f lib/errors.f lib/string.f lib/memory.f lib/vector.f
   lib/fs.f lib/fs-mutate.f lib/process.f lib/process-argv.f lib/source.f
   tools/lint/text.f tools/lint/token.f tools/lint/lib.f
   tools/lint/json-writer.f tools/lint/source-lex.f tools/diag-origin-core.f
   tools/json.f tools/json-only-core.f tools/signature-lint-core.f
   tools/checked-boundary-lint-core.f tools/reserved-name-lint-core.f
   tools/trust-lint-core.f tools/check-all-errors-core.f tools/argv.f
   tools/check-core.f
;GE-FILES

: GE-WARM-PATHS ( -- )
   GE-WARM-ROOT 2dup MAKE-DIRS
   s" hb-check-warm" GE-WARM-BUF JOIN-PATH GE-WARM-U !
   GE-WARM$ s" .trust.f" GE-WARM-TRUST-BUF GE-WARM-TRUST-U GE-SUFFIX!
   GE-WARM$ s" .stamp" GE-WARM-STAMP-BUF GE-WARM-STAMP-U GE-SUFFIX! ;

: GE-WARM-STAMP$ ( -- ptr u8 n )
   GE-WARM-STAMP-BUF GE-WARM-STAMP-U @ ;

: GE-WARM-KEY-FILE+ ( ptr u8 n -- ) {: a:ptr u:n :}
   a u CK-FILE+ ;

: GE-WARM-BAKER-KEY ( -- )
   [: GE-WARM-KEY-FILE+ ;] GE-WARM-BAKER-FILES ;

: GE-CHECK-SUPPORT-KEY ( -- )
   [: GE-WARM-KEY-FILE+ ;] GE-CHECK-SUPPORT-FILES ;

: GE-WARM-KEY! ( -- )
   CK-RESET
   s" hb-check-warm-cache-v2" CK-TEXT+
   s" bin/hb" GE-WARM-KEY-FILE+
   GE-WARM-BAKER-KEY
   GE-CHECK-SUPPORT-KEY
   GE-WARM-KEY-HEX CK-FINAL-HEX ;

: GE-WARM-CACHED? ( -- bool )
   GE-WARM$ EXECUTABLE? 0= if 0 0= 0= exit then
   GE-WARM-TRUST$ FILE? 0= if 0 0= 0= exit then
   GE-WARM-STAMP$ FILE? 0= if 0 0= 0= exit then
   GE-WARM-STAMP$ GE-WARM-STAMP-RD 80 READ-ALL
   dup 64 <> if drop 0 0= 0= exit then
   GE-WARM-STAMP-RD swap GE-WARM-KEY-HEX 64 STR= ;

: GE-WARM-TOOL-ARGV ( -- )
   PROC-ARGV-ENV-RESET
   GE-ARGV-RESET
   s" --load" GE-ARG+
   s" lib/errors.f" GE-ARG+
   s" lib/string.f" GE-ARG+
   s" lib/memory.f" GE-ARG+
   s" lib/fs.f" GE-ARG+
   s" lib/fs-mutate.f" GE-ARG+
   s" lib/process.f" GE-ARG+
   s" lib/process-argv.f" GE-ARG+
   s" lib/process-env.f" GE-ARG+
   s" lib/source.f" GE-ARG+
   s" lib/codesign.f" GE-ARG+
   s" tools/warm-image-lib.f" GE-ARG+
   s" tools/warm-image.f" GE-ARG+
   s" --" GE-ARG+
   GE-WARM$ GE-ARG+ ;

: GE-CHECK-SUPPORT-ARGV ( -- )
   [: GE-ARG+ ;] GE-CHECK-SUPPORT-FILES ;

: GE-WARM-BAKE ( -- )
   GE-WARM-PATHS
   GE-WARM-KEY!
   GE-WARM-CACHED? if
      s" warm-cache-hit" GS-EVENT
      -1 GE-WARM-READY !
      exit
   then
   s" warm-cache-miss" GS-EVENT
   s" warm-build" GS-EVENT
   GE-WARM-TOOL-ARGV
   GE-CHECK-SUPPORT-ARGV
   s" bin/hb" GE-TIMEOUT-MS GE-RUN-ENV
   s" warm checker image" GE-EXPECT-OK
   GE-WARM-STAMP$ GE-WARM-KEY-HEX 64 WRITE-ALL
   -1 GE-WARM-READY ! ;

: GE-CHECK-WARM ( -- )
   GE-WARM-READY @ if exit then
   GE-WARM-BAKE ;

: GE-CHECK-EXE ( -- ptr u8 n )
   GE-CHECK-WARM
   GE-WARM$ ;

: GE-HB-RESET ( -- )
   PROC-ARGV-ENV-RESET
   GE-ARGV-RESET ;

: GE-HB$ ( -- ptr u8 n )
   s" HABU_UNDER_TEST" GETENV dup 0= if
      2drop s" bin/hb" exit
   then
   2dup EXECUTABLE? 0= if E-FS-OPEN throw then ;

: GE-BIN-HB-RUN ( ptr u8 n -- ) {: label:ptr labelu:n :}
   label labelu GT-PROGRESS-RUN
   s" inner-hb-spawn" GS-EVENT
   s" boundary-test" GS-EVENT
   s" bin/hb" GE-TIMEOUT-MS GE-RUN-ENV
   label labelu GE-EXPECT-OK
   label labelu GT-PROGRESS-PASS ;

: GE-HB-RUN ( ptr u8 n -- ) {: label:ptr labelu:n :}
   label labelu GT-PROGRESS-RUN
   s" inner-hb-spawn" GS-EVENT
   s" boundary-test" GS-EVENT
   GE-HB$ GE-TIMEOUT-MS GE-RUN-ENV
   label labelu GE-EXPECT-OK
   label labelu GT-PROGRESS-PASS ;

: GE-HB-RUN-STDIN ( ptr u8 n -- ) {: label:ptr labelu:n :}
   label labelu GT-PROGRESS-RUN
   s" inner-hb-stdin" GS-EVENT
   s" boundary-test" GS-EVENT
   GE-HB$ GE-SRC-BUF GE-SRC-U @ GE-TIMEOUT-MS GE-RUN-STDIN
   label labelu GE-EXPECT-OK
   label labelu GT-PROGRESS-PASS ;

: GE-HB-RUN-STDIN-NZ ( ptr u8 n -- ) {: label:ptr labelu:n :}
   label labelu GT-PROGRESS-RUN
   s" inner-hb-stdin" GS-EVENT
   s" boundary-test" GS-EVENT
   GE-HB$ GE-SRC-BUF GE-SRC-U @ GE-TIMEOUT-MS GE-RUN-STDIN
   label labelu GE-EXPECT-NONZERO
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
   GE-CHECK-WARM
   GE-HB-RESET
   s" --load" GE-ARG+
   GE-WARM-TRUST$ GE-ARG+
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
