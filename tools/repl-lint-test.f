\ repl-lint-test.f - checked fixtures for tools/repl-lint.f.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/fs.f
\ lib/fs-mutate.f tools/lint/text.f tools/lint/intern.f tools/lint/token.f
\ tools/lint/lib.f tools/repl-lint-core.f tools/repl-lint-test.f

4096 constant RLT-CAP

variable RLT-ROOT-U
variable RLT-SRC-DIR-U
variable RLT-STDIN-U
variable RLT-REPL-U
variable RLT-WATCH-U
variable RLT-STEPPER-U
variable RLT-DEBUG-U

create RLT-ROOT-BUF FS-PATH-CAP allot
create RLT-SRC-DIR-BUF FS-PATH-CAP allot
create RLT-STDIN-BUF FS-PATH-CAP allot
create RLT-REPL-BUF FS-PATH-CAP allot
create RLT-WATCH-BUF FS-PATH-CAP allot
create RLT-STEPPER-BUF FS-PATH-CAP allot
create RLT-DEBUG-BUF FS-PATH-CAP allot
create RLT-OUT RLT-CAP allot
create RLT-ERR RLT-CAP allot

: RLT-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u dst:ptr lenp:ptr :}
   u FS-PATH-CAP > if E-FS-PATH throw then
   a dst u BYTE-COPY
   u lenp ! ;

: RLT-PATH! ( ptr u8 n ptr u8 n ptr u8 ptr n -- ) {: pa:ptr pu na:ptr nu dst:ptr lenp:ptr :}
   pa pu na nu dst JOIN-PATH lenp ! ;

: RLT-ROOT ( -- ptr u8 n )
   RLT-ROOT-BUF RLT-ROOT-U @ ;

: RLT-SRC-DIR ( -- ptr u8 n )
   RLT-SRC-DIR-BUF RLT-SRC-DIR-U @ ;

: RLT-STDIN ( -- ptr u8 n )
   RLT-STDIN-BUF RLT-STDIN-U @ ;

: RLT-REPL ( -- ptr u8 n )
   RLT-REPL-BUF RLT-REPL-U @ ;

: RLT-STEPPER ( -- ptr u8 n )
   RLT-STEPPER-BUF RLT-STEPPER-U @ ;

: RLT-WATCH ( -- ptr u8 n )
   RLT-WATCH-BUF RLT-WATCH-U @ ;

: RLT-DEBUG ( -- ptr u8 n )
   RLT-DEBUG-BUF RLT-DEBUG-U @ ;

: RLT-LF ( -- )
   10 SB-APPEND-C ;

: RLT-DQ ( -- )
   34 SB-APPEND-C ;

: RLT-EMPTY$ ( -- ptr u8 n )
   SB-RESET
   SB$ ;

: RLT-STDIN$ ( -- ptr u8 n )
   SB-RESET
   s" : REPL-SRC s" SB-APPEND RLT-DQ s"  src/habu/repl.f" SB-APPEND RLT-DQ s"  ;" SB-APPEND RLT-LF
   s" : WATCH-SRC s" SB-APPEND RLT-DQ s"  src/habu/debug-watch.f" SB-APPEND RLT-DQ s"  ;" SB-APPEND RLT-LF
   s" : STEP-SRC s" SB-APPEND RLT-DQ s"  src/habu/stepper.f" SB-APPEND RLT-DQ s"  ;" SB-APPEND RLT-LF
   s" : DBG-SRC s" SB-APPEND RLT-DQ s"  src/habu/debug.f" SB-APPEND RLT-DQ s"  ;" SB-APPEND RLT-LF
   SB$ ;

: RLT-GOOD$ ( -- ptr u8 n )
   SB-RESET
   92 SB-APPEND-C s"  die in a line comment is allowed" SB-APPEND RLT-LF
   s" : STRINGY s" SB-APPEND RLT-DQ s"  die" SB-APPEND RLT-DQ s"  drop ." SB-APPEND RLT-DQ s"  bye" SB-APPEND RLT-DQ s"  ;" SB-APPEND RLT-LF
   s" : STACKY ( die in a stack comment ) 1 ;" SB-APPEND RLT-LF
   SB$ ;

: RLT-BAD$ ( -- ptr u8 n )
   s" : BAD die ;" ;

: RLT-GOOD-OUT$ ( -- ptr u8 n )
   SB-RESET
   s" repl-lint: 0 finding(s)" SB-APPEND RLT-LF
   SB$ ;

: RLT-BAD-FINDING$ ( -- ptr u8 n )
   s" FATAL-IN-REPL src/habu/repl.f:1: `die` exits the session" ;

: RLT-BAD-ADVICE$ ( -- ptr u8 n )
   s" use `throw` (the REPL recovers); `die` is for build-time makers only" ;

: RLT-BAD-SUMMARY$ ( -- ptr u8 n )
   s" repl-lint: 1 finding(s)" ;

: RLT-PREPARE ( -- )
   CLEANUP-RESET
   s" habu-repl-lint" TMPDIR-MKDIR {: a:ptr u :}
   a u RLT-ROOT-BUF RLT-ROOT-U RLT-COPY!
   RLT-ROOT CLEANUP-TREE+
   RLT-ROOT s" src/habu" RLT-SRC-DIR-BUF RLT-SRC-DIR-U RLT-PATH!
   RLT-SRC-DIR MAKE-DIRS
   RLT-SRC-DIR s" stdin.f" RLT-STDIN-BUF RLT-STDIN-U RLT-PATH!
   RLT-SRC-DIR s" repl.f" RLT-REPL-BUF RLT-REPL-U RLT-PATH!
   RLT-SRC-DIR s" debug-watch.f" RLT-WATCH-BUF RLT-WATCH-U RLT-PATH!
   RLT-SRC-DIR s" stepper.f" RLT-STEPPER-BUF RLT-STEPPER-U RLT-PATH!
   RLT-SRC-DIR s" debug.f" RLT-DEBUG-BUF RLT-DEBUG-U RLT-PATH!
   RLT-STDIN RLT-STDIN$ WRITE-ALL
   RLT-WATCH RLT-EMPTY$ WRITE-ALL
   RLT-STEPPER RLT-EMPTY$ WRITE-ALL
   RLT-DEBUG RLT-EMPTY$ WRITE-ALL ;

: RLT-RUN-CORE ( -- n n n )
   RLT-ROOT REPL-ROOT!
   RLT-OUT RLT-CAP LINT-OUT-BUFFER!
   REPL-LINT-CHECK {: bad:n :}
   LINT-OUT$ nip LINT-OUT-BUFFER-OFF
   0 bad 0 > if 1 else 0 then ;

: RLT-EXPECT-CODE ( n n n n -- n n ) {: outu:n erru:n code:n expect:n :}
   code expect T=
   outu erru ;

: RLT-TEST-GOOD ( -- )
   RLT-REPL RLT-GOOD$ WRITE-ALL
   RLT-RUN-CORE 0 RLT-EXPECT-CODE {: outu:n erru:n :}
   RLT-OUT outu RLT-GOOD-OUT$ T$=
   RLT-ERR erru RLT-EMPTY$ T$= ;

: RLT-TEST-BAD ( -- )
   RLT-REPL RLT-BAD$ WRITE-ALL
   RLT-RUN-CORE 1 RLT-EXPECT-CODE {: outu:n erru:n :}
   erru 0 T=
   RLT-OUT outu RLT-BAD-FINDING$ CONTAINS? TTRUE
   RLT-OUT outu RLT-BAD-ADVICE$ CONTAINS? TTRUE
   RLT-OUT outu RLT-BAD-SUMMARY$ CONTAINS? TTRUE ;

: RLT-MAIN ( -- )
   T-RESET
   RLT-PREPARE
   RLT-TEST-GOOD
   RLT-TEST-BAD
   CLEANUP-RUN
   RLT-ROOT EXISTS? TFALSE
   T-REPORT
   s" repl-lint-test: ok" type cr ;

RLT-MAIN
