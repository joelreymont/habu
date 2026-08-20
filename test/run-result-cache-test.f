\ run-result-cache-test.f - focused tests for the gate phase result cache.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/memory.f
\   lib/fs.f lib/fs-mutate.f lib/content-key.f test/run-files.f
\   test/run-engine-set.f test/run-result-cache.f test/run-closure-lint.f
\   test/run-result-cache-test.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/fs.f
require lib/fs-mutate.f
require lib/content-key.f
require test/run-files.f
require test/run-engine-set.f
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

\ The engine half of every phase key. The closure lint above walks what a
\ DECLARED member requires; it cannot see the source bin/hb re-reads for itself
\ at process start, and that source - the checker, the core registries, the
\ seeded stdlib - decides what every phase observes. Keying the binary alone
\ therefore left a whole family of edits invisible to the cache: the tree that
\ moved SCHEMA-N@ into package SCHEMA-REG turned the prop/debug phase red while
\ its key stood still, and the gate reported PASS (cached) on a red tree
\ (incident habu-incident-master-red-750d7ee7).
\
\ Naming the paths would not prove much - a set can list a file and fold none of
\ its bytes. What is asserted instead is that the key MOVES with those files:
\ the live SHA-256 of the checker source and of the schema registry source must
\ appear in the fold's own preimage, which is the byte string CONTENT-KEY:FINAL
\ hashes. Edit either file and both sides move together; drop the boot prefix
\ from test/run-engine-set.f and neither digest is there to find.

1 LAYOUT-BUFFER ENG-FOLD CONTENT-KEY:fold

create ENG-DG 32 allot

variable ENG-N
variable ENG-HB?

: ENG-FOLD! ( CONTENT-KEY:fold -- )
   0 ENG-FOLD ! ;

: ENG-FOLD@ ( -- CONTENT-KEY:fold )
   0 ENG-FOLD @ ;

: ENG-SEE ( ptr u8 n -- ) {: a:ptr u:n :}
   ENG-N @ 1+ ENG-N !
   a u s" bin/hb" STR= if -1 ENG-HB? ! then
   ENG-FOLD@ a u CONTENT-KEY:FILE+ ENG-FOLD! ;

: ENG-TRACKS ( ptr u8 n -- ) {: a:ptr u:n :}
   a u ENG-DG SHA256-FILE 0 T=
   ENG-FOLD@ CONTENT-KEY:BUF$ ENG-DG 32 CONTAINS? TTRUE ;

: ENGINE-KEY ( -- )
   0 ENG-N !  0 ENG-HB? !
   CONTENT-KEY:OPEN ENG-FOLD!
   [: ENG-SEE ;] ENGINE-SET:FILES
   s" the engine set carries the binary a phase runs" T-LABEL
   ENG-HB? @ TTRUE
   s" the engine set is more than that binary" T-LABEL
   ENG-N @ 1 > TTRUE
   s" a phase key moves with the checker source the binary re-reads" T-LABEL
   s" src/core/checker.f" ENG-TRACKS
   s" a phase key moves with the schema registry source the binary re-reads" T-LABEL
   s" src/core/type-schema.f" ENG-TRACKS
   ENG-FOLD@ CONTENT-KEY:DISCARD ;

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
   ENGINE-KEY
   CLEANUP
   T-REPORT ;

MAIN

;package
