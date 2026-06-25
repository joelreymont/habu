\ hb-build-lib.f - native AOT/REPL build CLI library.
\
\ Load after lib/errors.f, lib/string.f, lib/fs.f, lib/fs-mutate.f,
\ lib/process.f, lib/process-argv.f, lib/process-env.f, lib/build.f,
\ lib/source.f, lib/codesign.f, and tools/build-fixpoint.f.

64 constant HBB-USAGE-RC
66 constant HBB-NOINPUT-RC
74 constant HBB-BUILD-RC
34 constant HBB-DQ
10 constant HBB-LF
120000 constant HBB-TIMEOUT-MS
65536 constant HBB-CAPTURE-CAP

create HBB-SRC-PATH FS-PATH-CAP allot
create HBB-OUT-PATH FS-PATH-CAP allot
create HBB-OUT-BUF HBB-CAPTURE-CAP allot
create HBB-ERR-BUF HBB-CAPTURE-CAP allot
create HBB-LF-BUF 1 allot
HBB-LF HBB-LF-BUF c!

variable HBB-SRC-U
variable HBB-OUT-U
variable HBB-I
variable HBB-REPL
variable HBB-JSON
variable HBB-STRICT
variable HBB-TAIL
variable HBB-FOUND
variable HBB-LINE-START
variable HBB-JSON-FOUND

: HBB-TRUE ( -- bool )
   0 0= ;

: HBB-FALSE ( -- bool )
   HBB-TRUE 0= ;

: HBB-EXIT ( n -- )
   s" " rot die ;

: HBB-USAGE ( -- )
   s" usage: tools/hb-build.f [--repl] [--json-errors] [--strict-signatures] source.f -o out" HBB-USAGE-RC die ;

: HBB-COPY-PATH! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u dst:ptr up:ptr :}
   u FS-PATH-CAP > if E-BUILD-PATH throw then
   a dst u BYTE-COPY
   u up ! ;

: HBB-SRC! ( ptr u8 n -- )
   HBB-SRC-PATH HBB-SRC-U HBB-COPY-PATH! ;

: HBB-OUT! ( ptr u8 n -- )
   HBB-OUT-PATH HBB-OUT-U HBB-COPY-PATH! ;

: HBB-SRC$ ( -- ptr u8 n )
   HBB-SRC-PATH HBB-SRC-U @ ;

: HBB-OUT$ ( -- ptr u8 n )
   HBB-OUT-PATH HBB-OUT-U @ ;

: HBB-WERR ( ptr u8 n -- ) {: a:ptr u :}
   u 0= if exit then
   2 a u write u <> if s" hb-build: stderr write failed" HBB-BUILD-RC die then ;

: HBB-WOUT-ERR ( n -- ) {: u :}
   u 0 > if HBB-OUT-BUF u HBB-WERR then ;

: HBB-WERR-ERR ( n -- ) {: u :}
   u 0 > if HBB-ERR-BUF u HBB-WERR then ;

: HBB-WERR-LF ( -- )
   HBB-LF-BUF 1 HBB-WERR ;

: HBB-LINE-FIRST ( n n -- n ) {: start end :}
   start begin dup end < while
      dup HBB-ERR-BUF + c@ dup 32 = swap 9 = or if
         1+
      else
         exit
      then
   repeat ;

: HBB-LINE-JSON? ( n n -- bool ) {: start end :}
   start end HBB-LINE-FIRST
   dup end >= if drop HBB-FALSE exit then
   HBB-ERR-BUF + c@ 123 = ;

: HBB-WERR-LINE ( n n -- ) {: start end :}
   end start - {: len :}
   len 0 > if HBB-ERR-BUF start + len HBB-WERR then
   HBB-WERR-LF ;

: HBB-WERR-JSON-LINE ( n n -- ) {: start end :}
   start end HBB-LINE-JSON? if
      start end HBB-WERR-LINE
      -1 HBB-JSON-FOUND !
   then ;

: HBB-WERR-JSON-ONLY ( n -- ) {: u :}
   0 HBB-JSON-FOUND !
   0 HBB-LINE-START !
   0 begin dup u < while
      HBB-ERR-BUF over + c@ HBB-LF = if
         HBB-LINE-START @ over HBB-WERR-JSON-LINE
         1+ dup HBB-LINE-START !
      else
         1+
      then
   repeat drop
   HBB-LINE-START @ u < if HBB-LINE-START @ u HBB-WERR-JSON-LINE then
   HBB-JSON-FOUND @ 0= if u HBB-WERR-ERR then ;

: HBB-PATH-HAS-DQ? ( ptr u8 n -- bool )
   HBB-DQ INDEX-OF 0 >= ;

: HBB-INC-I ( -- )
   HBB-I @ 1+ HBB-I ! ;

: HBB-ARG$ ( n -- ptr u8 n )
   SCRIPT-ARGV$ ;

: HBB-ARG= ( n ptr u8 n -- bool ) {: idx pat:ptr patu :}
   idx HBB-ARG$ pat patu STR= ;

: HBB-PARSE-OPTION? ( -- bool )
   HBB-I @ SCRIPT-ARGC >= if HBB-FALSE exit then
   HBB-I @ s" --repl" HBB-ARG= if -1 HBB-REPL ! HBB-INC-I HBB-TRUE exit then
   HBB-I @ s" --json-errors" HBB-ARG= if -1 HBB-JSON ! HBB-INC-I HBB-TRUE exit then
   HBB-I @ s" --strict-signatures" HBB-ARG= if -1 HBB-STRICT ! HBB-INC-I HBB-TRUE exit then
   HBB-I @ HBB-ARG$ s" --" STR= if HBB-INC-I HBB-FALSE exit then
   HBB-I @ HBB-ARG$ s" -" STARTS-WITH? if HBB-USAGE then
   HBB-FALSE ;

: HBB-PARSE-OPTIONS ( -- )
   begin HBB-PARSE-OPTION? while repeat ;

: HBB-PARSE ( -- )
   0 HBB-REPL !
   0 HBB-JSON !
   0 HBB-STRICT !
   0 HBB-I !
   HBB-PARSE-OPTIONS
   SCRIPT-ARGC HBB-I @ - 3 <> if HBB-USAGE then
   HBB-I @ HBB-ARG$ HBB-SRC!
   HBB-I @ 1+ s" -o" HBB-ARG= 0= if HBB-USAGE then
   HBB-I @ 2 + HBB-ARG$ HBB-OUT!
   HBB-SRC$ FILE? 0= if s" hb-build: no such source" HBB-NOINPUT-RC die then
   HBB-SRC$ HBB-PATH-HAS-DQ? if s" hb-build: source path contains a double quote" HBB-USAGE-RC die then ;

: HBB-ENV-TMP? ( -- bool )
   s" HB_TMP" GETENV dup 0= if 2drop HBB-FALSE exit then
   2dup EXISTS? if
      2dup DIR? 0= if s" hb-build: HB_TMP is not a directory" HBB-USAGE-RC die then
   else
      2dup MAKE-DIR
   then
   BF-TMP!
   HBB-TRUE ;

: HBB-PREPARE-TMP ( -- )
   BF-TMP-RESET
   CLEANUP-RESET
   HBB-ENV-TMP? if exit then
   s" hb-build-native" TMPDIR-MKDIR 2dup BF-TMP! CLEANUP-TREE+ ;

: HBB-CLEANUP ( -- )
   CLEANUP-RUN
   BF-TMP-RESET ;

: HBB-CMD-RESET ( -- )
   PROC-ARGV-RESET
   BF-PREPARE-ENV ;

: HBB-ADD-LINT-LOADS ( -- )
   s" --load"  >LEN PROC-ARGV+
   s" lib/errors.f"  >LEN PROC-ARGV+
   s" lib/memory.f"  >LEN PROC-ARGV+
   s" lib/vector.f"  >LEN PROC-ARGV+
   s" tools/lint/text.f"  >LEN PROC-ARGV+ s" tools/lint/token.f" >LEN PROC-ARGV+ s" tools/lint/lib.f" >LEN PROC-ARGV+
   s" tools/lint/json-writer.f"  >LEN PROC-ARGV+
   s" tools/lint/source-lex.f"  >LEN PROC-ARGV+
   s" tools/argv.f"  >LEN PROC-ARGV+ ;

: HBB-ADD-AOT-LINT-CMD ( -- )
   HBB-CMD-RESET
   HBB-ADD-LINT-LOADS
   s" tools/aot-lint.f"  >LEN PROC-ARGV+
   s" --"  >LEN PROC-ARGV+
   HBB-JSON @ if s" --json"  >LEN PROC-ARGV+ then
   HBB-SRC$  >LEN PROC-ARGV+ ;

: HBB-ADD-SIGNATURE-LINT-CMD ( -- )
   HBB-CMD-RESET
   HBB-ADD-LINT-LOADS
   s" tools/signature-lint.f"  >LEN PROC-ARGV+
   s" --"  >LEN PROC-ARGV+
   HBB-JSON @ if s" --json"  >LEN PROC-ARGV+ then
   HBB-SRC$  >LEN PROC-ARGV+ ;

: HBB-ADD-DIAG-ORIGIN-CMD ( -- )
   HBB-CMD-RESET
   s" --load"  >LEN PROC-ARGV+
   s" tools/lint/text.f"  >LEN PROC-ARGV+ s" tools/lint/token.f" >LEN PROC-ARGV+ s" tools/lint/lib.f" >LEN PROC-ARGV+
   s" tools/diag-origin.f"  >LEN PROC-ARGV+
   s" --"  >LEN PROC-ARGV+
   HBB-SRC$  >LEN PROC-ARGV+ ;

: HBB-CAPTURE>N ( len len rc -- n n n ) {: outu erru rc :}
   outu LEN>N erru LEN>N rc RC>N ;

: HBB-RUN-HB-CAPTURE ( -- n n n )
   s" bin/hb" >LEN HBB-OUT-BUF HBB-CAPTURE-CAP >LEN HBB-ERR-BUF HBB-CAPTURE-CAP >LEN
   HBB-TIMEOUT-MS >MS RUN-ARGV-ENV-CAPTURE
   HBB-CAPTURE>N ;

: HBB-RUN-DIAG-CAPTURE ( -- n n n )
   s" bin/hb" >LEN BF-SOURCE-BUF BF-SOURCE-CAP >LEN HBB-ERR-BUF HBB-CAPTURE-CAP >LEN
   HBB-TIMEOUT-MS >MS RUN-ARGV-ENV-CAPTURE
   HBB-CAPTURE>N ;

: HBB-FINISH-TOOL ( n n n -- ) {: outu erru rc :}
   rc 0= if exit then
   outu HBB-WOUT-ERR
   erru HBB-WERR-ERR
   rc HBB-EXIT ;

: HBB-FINISH-DIAG-ORIGIN ( n n n -- n ) {: outu erru rc :}
   rc 0= if
      erru HBB-WERR-ERR
      outu exit
   then
   BF-SOURCE-BUF outu HBB-WERR
   erru HBB-WERR-ERR
   0 rc HBB-EXIT ;

: HBB-RUN-AOT-LINT ( -- )
   HBB-REPL @ if exit then
   HBB-ADD-AOT-LINT-CMD
   HBB-RUN-HB-CAPTURE HBB-FINISH-TOOL ;

: HBB-RUN-SIGNATURE-LINT ( -- )
   HBB-STRICT @ 0= if exit then
   HBB-ADD-SIGNATURE-LINT-CMD
   HBB-RUN-HB-CAPTURE HBB-FINISH-TOOL ;

: HBB-DIAG-ORIGIN-SOURCE ( -- )
   HBB-ADD-DIAG-ORIGIN-CMD
   HBB-RUN-DIAG-CAPTURE HBB-FINISH-DIAG-ORIGIN BF-SOURCE-LEN ! ;

: HBB-DRIVER$ ( -- ptr u8 n )
   HBB-REPL @ if s" src/habu/build.f" else s" src/habu/aot.f" then ;

: HBB-SRC-NAME$ ( -- ptr u8 n )
   HBB-REPL @ if s" hb-build-src" else s" hb-aot-src" then ;

: HBB-CHECK-NAME$ ( -- ptr u8 n )
   s" hb-build-check-src" ;

: HBB-GOT-NAME$ ( -- ptr u8 n )
   HBB-REPL @ if s" hb-build-got" else s" hb-aot-got" then ;

: HBB-MK-NAME$ ( -- ptr u8 n )
   HBB-REPL @ if s" hb-build-mk" else s" hb-aot-mk" then ;

: HBB-MAKER-SRC-NAME$ ( -- ptr u8 n )
   s" hb-maker-src" ;

: HBB-SB-DQ ( -- )
   HBB-DQ SB-APPEND-C ;

: HBB-DIAG-LINE$ ( -- ptr u8 n )
   SB-RESET
   s" s" SB-APPEND
   HBB-SB-DQ
   s"  " SB-APPEND
   HBB-SRC$ SB-APPEND
   HBB-SB-DQ
   s"  DIAG-FILE!" SB-APPEND
   SB$ ;

: HBB-LAST-LINE-START ( -- n )
   0 HBB-TAIL !
   0 HBB-FOUND !
   BF-SOURCE-LEN @ 0 > if BF-SOURCE-LEN @ 1- HBB-TAIL ! then
   begin HBB-TAIL @ 0 > HBB-FOUND @ 0= and while
      BF-SOURCE-BUF HBB-TAIL @ 1- + c@ HBB-LF = if
         -1 HBB-FOUND !
      else
         HBB-TAIL @ 1- HBB-TAIL !
      then
   repeat
   HBB-TAIL @ ;

: HBB-APPEND-DRIVER ( ptr u8 n -- ) {: out:ptr outu :}
   HBB-DRIVER$ BF-SOURCE-BUF BF-SOURCE-CAP READ-ALL BF-SOURCE-LEN !
   HBB-LAST-LINE-START {: tail :}
   out outu BF-OUT$ BF-SOURCE-BUF tail APPEND-FILE
   out outu HBB-DIAG-LINE$ BF-APPEND-LINE
   HBB-JSON @ if out outu s" -1 JSON-DIAGS !" BF-APPEND-LINE then
   out outu BF-OUT$ BF-SOURCE-BUF tail + BF-SOURCE-LEN @ tail - APPEND-FILE
   out outu BF-APPEND-LF ;

: HBB-MAKER-SOURCE ( -- )
   HBB-MAKER-SRC-NAME$ BF-RESET-OUT
   HBB-MAKER-SRC-NAME$ BF-APPEND-COMMON
   HBB-MAKER-SRC-NAME$ BF-APPEND-DRIVER-IO
   HBB-MAKER-SRC-NAME$ HBB-APPEND-DRIVER ;

: HBB-STAGE2-SOURCE ( -- )
   s" stage2-src" BF-RESET-OUT
   s" stage2-src" BF-APPEND-COMMON
   s" stage2-src" BF-APPEND-DRIVER-IO
   s" stage2-src" s" src/habu/maker.f" BF-APPEND-SOURCE ;

: HBB-BUILD-MAKER ( -- )
   HBB-MAKER-SOURCE
   HBB-STAGE2-SOURCE
   s" stage2-got" BF-REMOVE-TMP
   HBB-MK-NAME$ BF-REMOVE-TMP
   s" bin/hb" s" stage2-src" BF-A$ BF-RUN-ENV-PATH-INFILE
   dup 0 <> if s" hb-build: native maker build failed" HBB-BUILD-RC die then drop
   s" stage2-got" BF-EXPECT
   s" stage2-got" HBB-MK-NAME$ BF-RENAME-TMP
   HBB-MK-NAME$ BF-CHMOD-X-TMP ;

: HBB-READ-COMMENTED-SOURCE ( -- )
   HBB-SRC$ BF-SOURCE-BUF BF-SOURCE-CAP READ-ALL BF-SOURCE-LEN !
   BF-SOURCE-BUF BF-SOURCE-LEN @ >LEN SOURCE-BUF SOURCE-CAP >LEN COMMENT-EXPORTS SOURCE-LEN ! ;

: HBB-READ-ORIGIN-COMMENTED-SOURCE ( -- )
   HBB-DIAG-ORIGIN-SOURCE
   BF-SOURCE-BUF BF-SOURCE-LEN @ >LEN SOURCE-BUF SOURCE-CAP >LEN COMMENT-EXPORTS SOURCE-LEN ! ;

: HBB-WRITE-COMMENTED-SOURCE ( ptr u8 n -- ) {: name:ptr nameu :}
   name nameu BF-OUT$ SOURCE-BUF SOURCE-LEN @ LEN>N WRITE-ALL ;

: HBB-TARGET-UNKNOWN ( -- )
   s" hb-build: unknown target" HBB-BUILD-RC die ;

: HBB-APPEND-TARGET-ENV ( -- )
   HB-TARGET-LINUX? if
      HBB-SRC-NAME$ s" src/os/linux/env.f" BF-APPEND-SOURCE
      exit
   then
   HB-TARGET-MACOS? if
      HBB-SRC-NAME$ s" src/os/macos/env.f" BF-APPEND-SOURCE
      exit
   then
   HBB-TARGET-UNKNOWN ;

: HBB-APPEND-TARGET-REPL-TERM ( -- )
   HB-TARGET-LINUX? if
      HBB-SRC-NAME$ s" src/os/linux/repl-term.f" BF-APPEND-SOURCE
      exit
   then
   HB-TARGET-MACOS? if
      HBB-SRC-NAME$ s" src/os/macos/repl-term.f" BF-APPEND-SOURCE
      exit
   then
   HBB-TARGET-UNKNOWN ;

: HBB-RESET-RUNTIME-SOURCE ( -- )
   HBB-SRC-NAME$ BF-RESET-OUT
   HBB-SRC-NAME$ s" 0 set-check" BF-APPEND-LINE
   HBB-SRC-NAME$ s" : TRUST ( ptr u8 n ptr u8 n -- ) 2drop 2drop ;" BF-APPEND-LINE
   HBB-SRC-NAME$ s" src/habu/layout.f" BF-APPEND-SOURCE
   HBB-APPEND-TARGET-ENV
   HBB-SRC-NAME$ s" : SCRIPT-ARG-START ( -- n ) 1 ;" BF-APPEND-LINE
   HBB-SRC-NAME$ s" : SCRIPT-ARGC ( -- n ) ARGC 1 - dup 0 < if drop 0 then ;" BF-APPEND-LINE
   HBB-SRC-NAME$ s" : SCRIPT-ARGV ( i -- z ) 1 + ARGV ;" BF-APPEND-LINE
   HBB-SRC-NAME$ s" : SCRIPT-ARGV$ ( i -- a u ) SCRIPT-ARGV dup ZLEN ;" BF-APPEND-LINE ;

: HBB-APPEND-REPL-TARGET ( -- )
   HBB-APPEND-TARGET-REPL-TERM ;

: HBB-APPEND-COMMENTED-SOURCE ( -- )
   HBB-SRC-NAME$ SOURCE-BUF SOURCE-LEN @ BF-APPEND-BYTES ;

: HBB-PREPARE-AOT-SOURCE ( -- )
   HBB-READ-ORIGIN-COMMENTED-SOURCE
   HBB-SRC-NAME$ HBB-WRITE-COMMENTED-SOURCE ;

: HBB-PREPARE-REPL-SOURCE ( -- )
   HBB-READ-ORIGIN-COMMENTED-SOURCE
   HBB-CHECK-NAME$ HBB-WRITE-COMMENTED-SOURCE
   HBB-READ-COMMENTED-SOURCE
   HBB-RESET-RUNTIME-SOURCE
   HBB-APPEND-COMMENTED-SOURCE
   HBB-SRC-NAME$ BF-APPEND-LF
   HBB-APPEND-REPL-TARGET
   HBB-SRC-NAME$ s" src/habu/repl.f" BF-APPEND-SOURCE ;

: HBB-PREPARE-PROGRAM-SOURCE ( -- )
   HBB-REPL @ if HBB-PREPARE-REPL-SOURCE else HBB-PREPARE-AOT-SOURCE then ;

: HBB-RUN-MAKER-CMD ( -- n n n )
   PROC-ARGV-RESET
   BF-PREPARE-ENV
   HBB-MK-NAME$ BF-A$ >LEN HBB-OUT-BUF HBB-CAPTURE-CAP >LEN HBB-ERR-BUF HBB-CAPTURE-CAP >LEN
   HBB-TIMEOUT-MS >MS RUN-ARGV-ENV-CAPTURE
   HBB-CAPTURE>N ;

: HBB-FINISH-MAKER ( n n n -- ) {: outu erru rc :}
   rc 0= if exit then
   outu HBB-WOUT-ERR
   HBB-JSON @ if erru HBB-WERR-JSON-ONLY else erru HBB-WERR-ERR then
   rc HBB-EXIT ;

: HBB-REMOVE-OUT ( -- )
   HBB-OUT$ 2dup EXISTS? if REMOVE-FILE else 2drop then ;

: HBB-INSTALL-OUT ( -- )
   HBB-GOT-NAME$ BF-EXPECT
   HBB-REMOVE-OUT
   HBB-GOT-NAME$ BF-A$ HBB-OUT$ RENAME-FILE
   HBB-OUT$ CHMOD-X ;

: HBB-RUN-MAKER ( -- )
   HBB-GOT-NAME$ BF-REMOVE-TMP
   HBB-RUN-MAKER-CMD HBB-FINISH-MAKER
   HBB-INSTALL-OUT ;

: HBB-SUCCESS ( -- )
   s" hb-build OK: " type
   HBB-OUT$ type
   HBB-REPL @ if
      s"  (engine+REPL bundle)"
   else
      s"  (AOT, engine stripped)"
   then type
   cr ;

: HBB-BUILD-AOT ( -- )
   HBB-RUN-SIGNATURE-LINT
   HBB-RUN-AOT-LINT
   HBB-BUILD-MAKER
   HBB-PREPARE-PROGRAM-SOURCE
   HBB-RUN-MAKER
   HBB-SUCCESS ;

: HBB-MAIN ( -- )
   HBB-PARSE
   HBB-PREPARE-TMP
   HBB-BUILD-AOT
   HBB-CLEANUP ;
