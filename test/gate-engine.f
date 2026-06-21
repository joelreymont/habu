\ gate-engine.f - checked runner for engine and public hb gate checks.
\
\ Load after test/gate-common.f, lib/build.f, and tools/build-fixpoint.f.

create GE-SCRIPT-PATH FS-PATH-CAP allot

variable GE-SCRIPT-U

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
