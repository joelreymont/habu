\ run-result-cache-test.f - focused tests for the gate phase result cache.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/memory.f
\   lib/fs.f lib/fs-mutate.f lib/content-key.f test/run-files.f
\   test/run-result-cache.f test/run-closure-lint.f
\   test/run-result-cache-test.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/fs.f
require lib/fs-mutate.f
require lib/content-key.f
require test/run-files.f
require test/run-result-cache.f
require test/run-closure-lint.f

package RCT

64 constant KEY-U

create ROOT-BUF FS-PATH-CAP allot
create SRC-BUF FS-PATH-CAP allot
create CACHE-BUF FS-PATH-CAP allot
create KEY1 80 allot
create KEY2 80 allot

variable ROOT-LEN
variable SRC-LEN
variable CACHE-LEN

: ROOT$ ( -- ptr u8 n )
   ROOT-BUF ROOT-LEN @ ;

: SRC$ ( -- ptr u8 n )
   SRC-BUF SRC-LEN @ ;

: SETUP ( -- )
   CLEANUP-RESET
   s" habu-result-cache" TMPDIR-MKDIR {: a:ptr u:n :}
   a ROOT-BUF u BYTE-COPY
   u ROOT-LEN !
   ROOT$ CLEANUP-TREE+
   ROOT$ s" src.f" SRC-BUF JOIN-PATH SRC-LEN !
   ROOT$ s" content-key.cache" CACHE-BUF JOIN-PATH CACHE-LEN !
   CONTENT-KEY:CACHE-CLEAR!
   CACHE-BUF CACHE-LEN @ CONTENT-KEY:CACHE-PATH!
   RUN-CLOSURE:PREPARE
   TRC:RESET
   ROOT$ TRC:ROOT! ;

: KEY! ( ptr u8 -- ) {: dst:ptr :}
   CONTENT-KEY:OPEN
   s" result-cache-test" CONTENT-KEY:TEXT+
   SRC$ CONTENT-KEY:FILE+
   dst CONTENT-KEY:FINAL-HEX ;

: STAMP-MISS-THEN-HIT ( -- )
   SRC$ s" alpha" WRITE-ALL
   KEY1 KEY!
   KEY1 TRC:HIT? TFALSE
   0 KEY1 TRC:PENDING+
   TRC:PENDING# 1 T=
   0 TRC:PENDING-PHASE 0 T=
   KEY1 TRC:HIT? TFALSE
   s" rct-phase" KEY1 TRC:STAMP+
   KEY1 TRC:HIT? TTRUE ;

: EDIT-INVALIDATES ( -- )
   SRC$ s" beta-longer" WRITE-ALL
   KEY2 KEY!
   KEY2 KEY-U KEY1 KEY-U T$<>
   KEY2 TRC:HIT? TFALSE ;

: RED-NEVER-CACHED ( -- )
   TRC:RESET
   TRC:PENDING# 0 T=
   8 KEY2 TRC:PENDING+
   TRC:PENDING# 1 T=
   0 TRC:PENDING-PHASE 8 T=
   KEY2 TRC:HIT? TFALSE ;

\ Closure lint over the declared phase file sets. The scanner itself lives in
\ test/run-closure-lint.f and is exercised on fixtures by
\ test/run-closure-lint-test.f; here it runs over the REAL sets, which is the
\ only place a stale declaration can be caught.

: DEBUG-SET! ( -- )
   RUN-CLOSURE:SET-RESET
   [: RUN-CLOSURE:SET+ ;] TR-GATE-HARNESS-FILES
   [: RUN-CLOSURE:SET+ ;] TR-GATE-COMMON-FILES
   [: RUN-CLOSURE:SET+ ;] TR-DEBUG-PHASE-FILES ;

: AOT-NEG-SET! ( -- )
   RUN-CLOSURE:SET-RESET
   [: RUN-CLOSURE:SET+ ;] TR-GATE-HARNESS-FILES
   [: RUN-CLOSURE:SET+ ;] TR-GATE-COMMON-FILES
   [: RUN-CLOSURE:SET+ ;] TR-AOT-NEG-PHASE-FILES ;

: CLOSURE-LINT ( -- )
   RUN-CLOSURE:RESET
   DEBUG-SET! RUN-CLOSURE:RUN
   AOT-NEG-SET! RUN-CLOSURE:RUN
   RUN-CLOSURE:FINDINGS 0 T= ;

: CLEANUP ( -- )
   CONTENT-KEY:CACHE-CLEAR!
   CLEANUP-RUN
   ROOT$ EXISTS? TFALSE ;

: MAIN ( -- )
   T-RESET
   SETUP
   STAMP-MISS-THEN-HIT
   EDIT-INVALIDATES
   RED-NEVER-CACHED
   CLOSURE-LINT
   CLEANUP
   T-REPORT ;

MAIN

;package
