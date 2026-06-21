\ gate-engine.f - checked runner for engine and public hb gate checks.
\
\ Load after lib/errors.f, lib/string.f, lib/fs.f, lib/fs-mutate.f,
\ lib/process.f, lib/process-argv.f, lib/process-env.f, lib/test-runner.f,
\ lib/build.f, and tools/build-fixpoint.f.

$40000 constant GE-SRC-CAP
120000 constant GE-TIMEOUT-MS
10 constant GE-LF
32 constant GE-SP
34 constant GE-DQ

create GE-SRC-BUF GE-SRC-CAP allot
create GE-SCRIPT-PATH FS-PATH-CAP allot

variable GE-SRC-U
variable GE-RD
variable GE-SCRIPT-U

: GE-RUN-ENV ( ptr u8 n n -- ) {: path:ptr pathu timeout :}
   PROC-ENV-INHERIT-MISSING
   path pathu GT-OUT-BUF GT-OUT-CAP GT-ERR-BUF GT-ERR-CAP timeout
   RUN-ARGV-ENV-CAPTURE
   GT-OUTCOME-CODE !
   PROC-OUTCOME-EXIT GT-OUTCOME-KIND !
   GT-ERR-U !
   GT-OUT-U ! ;

: GE-RUN-STDIN ( ptr u8 n ptr u8 n n -- ) {: path:ptr pathu in:ptr inu timeout :}
   PROC-ENV-INHERIT-MISSING
   path pathu in inu GT-OUT-BUF GT-OUT-CAP GT-ERR-BUF GT-ERR-CAP timeout
   RUN-ARGV-ENV-STDIN-CAPTURE
   GT-OUTCOME-CODE !
   PROC-OUTCOME-EXIT GT-OUTCOME-KIND !
   GT-ERR-U !
   GT-OUT-U ! ;

: GE-FAIL ( ptr u8 n -- ) {: label:ptr labelu :}
   s" FAIL: " type label labelu type cr
   s" rc: " type GT-RC@ . cr
   GT-OUT$ type
   GT-ERR$ type
   s" gate engine phase failed" 1 die ;

: GE-EXPECT-OK ( ptr u8 n -- ) {: label:ptr labelu :}
   GT-RC@ 0 <> if label labelu GE-FAIL then ;

: GE-EXPECT-RC ( n ptr u8 n -- ) {: want label:ptr labelu :}
   GT-RC@ want <> if label labelu GE-FAIL then ;

: GE-EXPECT-NONZERO ( ptr u8 n -- ) {: label:ptr labelu :}
   GT-RC@ 0= if label labelu GE-FAIL then ;

: GE-EXPECT-OUT ( ptr u8 n ptr u8 n -- ) {: want:ptr wantu label:ptr labelu :}
   GT-OUT$ want wantu STR= 0= if label labelu GE-FAIL then ;

: GE-SB-LF ( -- )
   GE-LF SB-APPEND-C ;

: GE-SRC-RESET ( -- )
   0 GE-SRC-U ! ;

: GE-SRC-C ( n -- ) {: c :}
   c 0 < if E-STR-BOUNDS throw then
   c STR-BYTE-MAX > if E-STR-BOUNDS throw then
   GE-SRC-U @ 1 + GE-SRC-CAP > if E-STR-CAPACITY throw then
   c GE-SRC-BUF GE-SRC-U @ + c!
   GE-SRC-U @ 1+ GE-SRC-U ! ;

: GE-SRC+ ( ptr u8 n -- ) {: a:ptr u :}
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

: GE-SRC-S" ( ptr u8 n -- ) {: a:ptr u :}
   s" s" GE-SRC+
   GE-DQ GE-SRC-C
   GE-SRC-SP
   a u GE-SRC+
   GE-DQ GE-SRC-C ;

: GE-SRC-FILE+ ( ptr u8 n -- ) {: path:ptr pathu :}
   path pathu GE-SRC-BUF GE-SRC-U @ + GE-SRC-CAP GE-SRC-U @ -
   READ-ALL GE-RD !
   GE-SRC-U @ GE-RD @ + GE-SRC-U ! ;

: GE-HB-RESET ( -- )
   PROC-ARGV-ENV-RESET ;

: GE-HB-RUN ( ptr u8 n -- ) {: label:ptr labelu :}
   s" bin/hb" GE-TIMEOUT-MS GE-RUN-ENV
   label labelu GE-EXPECT-OK ;

: GE-HB-RUN-STDIN ( ptr u8 n -- ) {: label:ptr labelu :}
   s" bin/hb" GE-SRC-BUF GE-SRC-U @ GE-TIMEOUT-MS GE-RUN-STDIN
   label labelu GE-EXPECT-OK ;

: GE-HB-RUN-STDIN-NZ ( ptr u8 n -- ) {: label:ptr labelu :}
   s" bin/hb" GE-SRC-BUF GE-SRC-U @ GE-TIMEOUT-MS GE-RUN-STDIN
   label labelu GE-EXPECT-NONZERO ;

: GE-CHECK-ARGV ( -- )
   GE-HB-RESET
   s" --load" PROC-ARGV+
   s" lib/errors.f" PROC-ARGV+
   s" lib/string.f" PROC-ARGV+
   s" lib/fs.f" PROC-ARGV+
   s" lib/fs-mutate.f" PROC-ARGV+
   s" lib/process.f" PROC-ARGV+
   s" lib/process-argv.f" PROC-ARGV+
   s" lib/source.f" PROC-ARGV+
   s" tools/argv.f" PROC-ARGV+
   s" tools/check.f" PROC-ARGV+
   s" --" PROC-ARGV+ ;

: GE-CHECK-RUN ( ptr u8 n -- ) {: label:ptr labelu :}
   GE-CHECK-ARGV
   s" bin/hb" GE-SRC-BUF GE-SRC-U @ GE-TIMEOUT-MS GE-RUN-STDIN
   label labelu GE-EXPECT-OK ;

: GE-CHECK-SRC-LIST ( ptr u8 n -- ) {: label:ptr labelu :}
   label labelu GE-CHECK-RUN ;

: GE-TMP-HB-NEW ( -- ptr u8 n )
   s" hb-new" BF-A$ ;

: GE-BIN-HB? ( ptr u8 n -- bool )
   s" bin/hb" STR= ;

: GE-REMOVE-BIN-OTHER ( ptr u8 n -- ) {: path:ptr pathu :}
   path pathu FILE? if
      path pathu GE-BIN-HB? 0= if path pathu REMOVE-FILE then
   then ;

: GE-INSTALL-HB ( -- )
   GE-TMP-HB-NEW s" bin/hb" RENAME-FILE
   s" bin/hb" CHMOD-X
   s" bin" [: GE-REMOVE-BIN-OTHER ;] WALK-FILES ;

: GE-BUILD-FIXPOINT ( -- )
   s" hb-gate-engine" GT-START
   GT-ROOT BF-TMP!
   BF-BUILD-ALL
   GE-INSTALL-HB
   BF-TMP-RESET
   s" PASS: self-rebuild fixpoint" type cr ;

: GE-RUN-EXTRA-FIXTURES ( -- )
   GE-HB-RESET
   s" --load" PROC-ARGV+ s" lib/errors.f" PROC-ARGV+ s" lib/string.f" PROC-ARGV+ s" lib/test.f" PROC-ARGV+ s" lib/fs.f" PROC-ARGV+ s" lib/fs-mutate.f" PROC-ARGV+ s" lib/fs-mutate-test.f" PROC-ARGV+
   s" fs mutation stdlib" GE-HB-RUN
   GE-SRC-RESET s" lib/errors.f" GE-SRC-FILE+ s" lib/string.f" GE-SRC-FILE+ s" lib/test.f" GE-SRC-FILE+ s" lib/fs.f" GE-SRC-FILE+ s" lib/fs-mutate.f" GE-SRC-FILE+ s" lib/fs-mutate-test.f" GE-SRC-FILE+
   s" fs mutation stdlib check" GE-CHECK-SRC-LIST
   GE-HB-RESET
   s" --load" PROC-ARGV+ s" lib/errors.f" PROC-ARGV+ s" lib/test.f" PROC-ARGV+ s" lib/process.f" PROC-ARGV+ s" lib/process-argv.f" PROC-ARGV+ s" lib/process-argv-test.f" PROC-ARGV+
   s" process argv stdlib" GE-HB-RUN
   GE-SRC-RESET s" lib/errors.f" GE-SRC-FILE+ s" lib/process.f" GE-SRC-FILE+ s" lib/process-argv.f" GE-SRC-FILE+
   s" process argv check" GE-CHECK-SRC-LIST
   GE-HB-RESET
   s" --load" PROC-ARGV+ s" lib/errors.f" PROC-ARGV+ s" lib/string.f" PROC-ARGV+ s" lib/test.f" PROC-ARGV+ s" lib/fs.f" PROC-ARGV+ s" lib/process.f" PROC-ARGV+ s" lib/process-argv.f" PROC-ARGV+ s" lib/process-env.f" PROC-ARGV+ s" lib/process-env-test.f" PROC-ARGV+
   s" process env stdlib" GE-HB-RUN
   GE-SRC-RESET s" lib/errors.f" GE-SRC-FILE+ s" lib/string.f" GE-SRC-FILE+ s" lib/fs.f" GE-SRC-FILE+ s" lib/process.f" GE-SRC-FILE+ s" lib/process-argv.f" GE-SRC-FILE+ s" lib/process-env.f" GE-SRC-FILE+
   s" process env check" GE-CHECK-SRC-LIST
   GE-HB-RESET
   s" --load" PROC-ARGV+ s" lib/errors.f" PROC-ARGV+ s" lib/string.f" PROC-ARGV+ s" lib/test.f" PROC-ARGV+ s" lib/fs.f" PROC-ARGV+ s" lib/fs-mutate.f" PROC-ARGV+ s" lib/process.f" PROC-ARGV+ s" lib/process-argv.f" PROC-ARGV+ s" tools/check-repair-hints-test.f" PROC-ARGV+
   s" repair diagnostic hints" GE-HB-RUN
   GE-SRC-RESET s" lib/errors.f" GE-SRC-FILE+ s" lib/string.f" GE-SRC-FILE+ s" lib/test.f" GE-SRC-FILE+ s" lib/fs.f" GE-SRC-FILE+ s" lib/fs-mutate.f" GE-SRC-FILE+ s" lib/process.f" GE-SRC-FILE+ s" lib/process-argv.f" GE-SRC-FILE+ s" tools/check-repair-hints-test.f" GE-SRC-FILE+
   s" repair diagnostic hints check" GE-CHECK-SRC-LIST
   GE-HB-RESET
   s" --load" PROC-ARGV+ s" lib/errors.f" PROC-ARGV+ s" lib/string.f" PROC-ARGV+ s" lib/test.f" PROC-ARGV+ s" lib/fs.f" PROC-ARGV+ s" lib/fs-mutate.f" PROC-ARGV+ s" lib/process.f" PROC-ARGV+ s" lib/process-argv.f" PROC-ARGV+ s" tools/hb-baseline-contracts-test.f" PROC-ARGV+
   s" hb baseline contracts" GE-HB-RUN ;

: GE-ENGINE-SUITE ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" test/engine-suite.f" GE-SRC-FILE+
   s" bin/hb" GE-SRC-BUF GE-SRC-U @ GE-TIMEOUT-MS GE-RUN-STDIN
   s" engine suite died" GE-EXPECT-OK
   SB-RESET s" ok" SB-APPEND GE-SB-LF
   GT-OUT$ SB$ ENDS-WITH? 0= if s" engine suite" GE-FAIL then
   s" PASS: engine suite on bin/hb" type cr ;

: GE-DIV-MOD ( -- )
   GE-HB-RESET GE-SRC-RESET s" 1 0 / ." GE-SRC-LINE
   s" divide by zero trap" GE-HB-RUN-STDIN-NZ
   GE-HB-RESET GE-SRC-RESET s" 1 0 mod ." GE-SRC-LINE
   s" modulo by zero trap" GE-HB-RUN-STDIN-NZ
   GE-HB-RESET GE-SRC-RESET s" 7 2 / . 7 2 mod . cr" GE-SRC-LINE
   s" nonzero div/mod" GE-HB-RUN-STDIN
   SB-RESET s" 3" SB-APPEND GE-SB-LF s" 1" SB-APPEND GE-SB-LF GE-SB-LF
   SB$ s" nonzero div/mod output" GE-EXPECT-OUT
   s" PASS: div/mod by zero traps (no silent 0)" type cr ;

: GE-TRUST-SOURCE ( -- )
   GE-SRC-RESET
   s" w" GE-SRC-S"
   GE-SRC-SP
   s" n -- n" GE-SRC-S"
   s"  trust 7 . : Q 5 dup * . ; Q" GE-SRC-LINE ;

: GE-TRUST-RUN ( -- )
   GE-HB-RESET
   GE-TRUST-SOURCE
   s" checked hb trust/run smoke" GE-HB-RUN-STDIN
   SB-RESET s" 7" SB-APPEND GE-SB-LF s" 25" SB-APPEND GE-SB-LF
   SB$ s" checked hb trust/run smoke output" GE-EXPECT-OUT
   GE-HB-RESET
   GE-SRC-RESET
   s" HOME" GE-SRC-S"
   s"  getenv nip 0 > ." GE-SRC-LINE
   s" getenv" GE-HB-RUN-STDIN
   SB-RESET s" -1" SB-APPEND GE-SB-LF
   SB$ s" getenv output" GE-EXPECT-OUT ;

: GE-WRITE-SCRIPT-ARGV ( -- )
   GT-ROOT s" hb-script-argv.f" GE-SCRIPT-PATH JOIN-PATH GE-SCRIPT-U !
   GE-SRC-RESET
   s" SCRIPT-ARGC ." GE-SRC-LINE
   s" 0 SCRIPT-ARGV$ type cr" GE-SRC-LINE
   s" 1 SCRIPT-ARGV$ type cr" GE-SRC-LINE
   GE-SCRIPT-PATH GE-SCRIPT-U @ GE-SRC-BUF GE-SRC-U @ WRITE-ALL ;

: GE-ARGV-MODES ( -- )
   GE-WRITE-SCRIPT-ARGV
   GE-HB-RESET
   GE-SCRIPT-PATH GE-SCRIPT-U @ PROC-ARGV+
   s" alpha" PROC-ARGV+
   s" beta" PROC-ARGV+
   s" bin/hb" GE-TIMEOUT-MS GE-RUN-ENV
   s" hb script argv mode" GE-EXPECT-OK
   SB-RESET s" 2" SB-APPEND GE-SB-LF s" alpha" SB-APPEND GE-SB-LF s" beta" SB-APPEND GE-SB-LF
   SB$ s" hb script argv mode output" GE-EXPECT-OUT
   GE-HB-RESET
   s" alpha" PROC-ARGV+
   s" beta" PROC-ARGV+
   GE-SRC-RESET
   s" ARGC ." GE-SRC-LINE
   s" 1 ARGV$ type cr" GE-SRC-LINE
   s" 2 ARGV$ type cr" GE-SRC-LINE
   s" bin/hb" GE-SRC-BUF GE-SRC-U @ GE-TIMEOUT-MS GE-RUN-STDIN
   s" hb pipeline argv mode" GE-EXPECT-OK
   SB-RESET s" 3" SB-APPEND GE-SB-LF s" alpha" SB-APPEND GE-SB-LF s" beta" SB-APPEND GE-SB-LF
   SB$ s" hb pipeline argv mode output" GE-EXPECT-OUT
   GE-HB-RESET
   GT-ROOT s" no-such-hb-script.f" GE-SCRIPT-PATH JOIN-PATH GE-SCRIPT-U !
   GE-SCRIPT-PATH GE-SCRIPT-U @ PROC-ARGV+
   s" bin/hb" GE-TIMEOUT-MS GE-RUN-ENV
   74 s" hb missing script rc" GE-EXPECT-RC ;

: GE-GOOD-TYPED ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" : SQOK ( i64 -- i64 ) dup * ;" GE-SRC-LINE
   s" 7 SQOK ." GE-SRC-LINE
   s" hb good typed def" GE-HB-RUN-STDIN
   SB-RESET s" 49" SB-APPEND GE-SB-LF
   SB$ s" hb good typed def output" GE-EXPECT-OUT ;

: GE-BAD-TYPED ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" : SQBAD ( i64 -- i64 ) dup ;" GE-SRC-LINE
   s" 7 SQBAD ." GE-SRC-LINE
   s" hb bad typed def" GE-HB-RUN-STDIN-NZ ;

: GE-DEPTH ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" : QDEPTH ( -- n ) depth ;" GE-SRC-LINE
   s" QDEPTH ." GE-SRC-LINE
   s" hb depth prim certify+run" GE-HB-RUN-STDIN
   SB-RESET s" 0" SB-APPEND GE-SB-LF
   SB$ s" hb depth prim certify+run output" GE-EXPECT-OUT ;

: GE-TRUSTED-SOURCE ( -- )
   GE-SRC-RESET
   s" TRUSTED: TLEAK ( n -- n ) dup ;" GE-SRC-LINE
   s" TUSE ( n -- n ) TLEAK" GE-SRC-S"
   s"  CHECK! ." GE-SRC-LINE
   s" TBAD ( n -- n n ) TLEAK" GE-SRC-S"
   s"  CHECK! ." GE-SRC-LINE
   s" 5 TLEAK . ." GE-SRC-LINE ;

: GE-TRUSTED-EFFECT ( -- )
   GE-HB-RESET
   GE-TRUSTED-SOURCE
   s" hb TRUSTED: effect recording" GE-HB-RUN-STDIN
   SB-RESET
   s" -1" SB-APPEND GE-SB-LF
   s" 0" SB-APPEND GE-SB-LF
   s" 5" SB-APPEND GE-SB-LF
   s" 5" SB-APPEND GE-SB-LF
   SB$ s" hb TRUSTED: effect recording output" GE-EXPECT-OUT ;

: GE-TYPED-SMOKE ( -- )
   GE-GOOD-TYPED
   GE-BAD-TYPED
   GE-DEPTH
   GE-TRUSTED-EFFECT ;

: GE-MAIN ( -- )
   GE-BUILD-FIXPOINT
   GE-RUN-EXTRA-FIXTURES
   GE-ENGINE-SUITE
   GE-DIV-MOD
   GE-TRUST-RUN
   GE-ARGV-MODES
   GE-TYPED-SMOKE
   GT-CLEANUP
   s" PASS: native engine gate phase" type cr ;

GE-MAIN
