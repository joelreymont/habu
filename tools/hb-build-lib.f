\ hb-build-lib.f - native AOT build CLI library.
\
\ Load after lib/errors.f, lib/string.f, lib/fs.f, lib/fs-mutate.f,
\ lib/process.f, lib/process-argv.f, lib/process-env.f, lib/build.f, and
\ tools/build-fixpoint.f.

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
variable HBB-JSON
variable HBB-STRICT
variable HBB-TAIL
variable HBB-FOUND

: HBB-TRUE ( -- bool )
   0 0= ;

: HBB-FALSE ( -- bool )
   HBB-TRUE 0= ;

: HBB-EXIT ( n -- )
   s" " rot die ;

: HBB-USAGE ( -- )
   s" usage: tools/hb-build.f [--json-errors] [--strict-signatures] source.f -o out" HBB-USAGE-RC die ;

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
   HBB-I @ s" --json-errors" HBB-ARG= if -1 HBB-JSON ! HBB-INC-I HBB-TRUE exit then
   HBB-I @ s" --strict-signatures" HBB-ARG= if -1 HBB-STRICT ! HBB-INC-I HBB-TRUE exit then
   HBB-I @ HBB-ARG$ s" --" STR= if HBB-INC-I HBB-FALSE exit then
   HBB-I @ HBB-ARG$ s" -" STARTS-WITH? if HBB-USAGE then
   HBB-FALSE ;

: HBB-PARSE-OPTIONS ( -- )
   begin HBB-PARSE-OPTION? while repeat ;

: HBB-PARSE ( -- )
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
   s" --load" PROC-ARGV+
   s" tools/lint/lib.f" PROC-ARGV+
   s" tools/lint/json-writer.f" PROC-ARGV+
   s" tools/lint/source-lex.f" PROC-ARGV+
   s" tools/argv.f" PROC-ARGV+ ;

: HBB-ADD-AOT-LINT-CMD ( -- )
   HBB-CMD-RESET
   HBB-ADD-LINT-LOADS
   s" tools/aot-lint.f" PROC-ARGV+
   s" --" PROC-ARGV+
   HBB-JSON @ if s" --json" PROC-ARGV+ then
   HBB-SRC$ PROC-ARGV+ ;

: HBB-ADD-SIGNATURE-LINT-CMD ( -- )
   HBB-CMD-RESET
   HBB-ADD-LINT-LOADS
   s" tools/signature-lint.f" PROC-ARGV+
   s" --" PROC-ARGV+
   HBB-JSON @ if s" --json" PROC-ARGV+ then
   HBB-SRC$ PROC-ARGV+ ;

: HBB-RUN-HB-CAPTURE ( -- n n n )
   s" bin/hb" HBB-OUT-BUF HBB-CAPTURE-CAP HBB-ERR-BUF HBB-CAPTURE-CAP
   HBB-TIMEOUT-MS RUN-ARGV-ENV-CAPTURE ;

: HBB-FINISH-TOOL ( n n n -- ) {: outu erru rc :}
   rc 0= if exit then
   outu HBB-WOUT-ERR
   erru HBB-WERR-ERR
   rc HBB-EXIT ;

: HBB-RUN-AOT-LINT ( -- )
   HBB-ADD-AOT-LINT-CMD
   HBB-RUN-HB-CAPTURE HBB-FINISH-TOOL ;

: HBB-RUN-SIGNATURE-LINT ( -- )
   HBB-STRICT @ 0= if exit then
   HBB-ADD-SIGNATURE-LINT-CMD
   HBB-RUN-HB-CAPTURE HBB-FINISH-TOOL ;

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

: HBB-APPEND-AOT-DRIVER ( ptr u8 n -- ) {: out:ptr outu :}
   s" src/habu/aot.f" BF-SOURCE-BUF BF-SOURCE-CAP READ-ALL BF-SOURCE-LEN !
   HBB-LAST-LINE-START {: tail :}
   out outu BF-OUT$ BF-SOURCE-BUF tail APPEND-FILE
   out outu HBB-DIAG-LINE$ BF-APPEND-LINE
   HBB-JSON @ if out outu s" -1 JSON-DIAGS !" BF-APPEND-LINE then
   out outu BF-OUT$ BF-SOURCE-BUF tail + BF-SOURCE-LEN @ tail - APPEND-FILE
   out outu BF-APPEND-LF ;

: HBB-STAGE2-SOURCE ( -- )
   s" stage2-src" BF-RESET-OUT
   s" stage2-src" s" 0 set-check" BF-APPEND-LINE
   s" stage2-src" BF-APPEND-COMMON
   s" stage2-src" HBB-APPEND-AOT-DRIVER ;

: HBB-BUILD-MAKER ( -- )
   HBB-STAGE2-SOURCE
   s" stage2-got" BF-REMOVE-TMP
   s" hb-aot-mk" BF-REMOVE-TMP
   s" bin/hb" s" src/habu/stage2.f" BF-RUN-ENV-PATH-INFILE
   dup 0 <> if s" hb-build: native maker build failed" HBB-BUILD-RC die then drop
   s" stage2-got" BF-EXPECT
   s" stage2-got" s" hb-aot-mk" BF-RENAME-TMP
   s" hb-aot-mk" BF-CHMOD-X-TMP ;

: HBB-PREPARE-AOT-SOURCE ( -- )
   HBB-SRC$ BF-SOURCE-BUF BF-SOURCE-CAP READ-ALL BF-SOURCE-LEN !
   s" hb-aot-src" BF-OUT$ BF-SOURCE-BUF BF-SOURCE-LEN @ WRITE-ALL ;

: HBB-RUN-MAKER-CMD ( -- n n n )
   PROC-ARGV-RESET
   BF-PREPARE-ENV
   s" hb-aot-mk" BF-A$ HBB-OUT-BUF HBB-CAPTURE-CAP HBB-ERR-BUF HBB-CAPTURE-CAP
   HBB-TIMEOUT-MS RUN-ARGV-ENV-CAPTURE ;

: HBB-FINISH-MAKER ( n n n -- ) {: outu erru rc :}
   rc 0= if exit then
   outu HBB-WOUT-ERR
   erru HBB-WERR-ERR
   rc HBB-EXIT ;

: HBB-REMOVE-OUT ( -- )
   HBB-OUT$ 2dup EXISTS? if REMOVE-FILE else 2drop then ;

: HBB-INSTALL-AOT ( -- )
   s" hb-aot-got" BF-EXPECT
   HBB-REMOVE-OUT
   s" hb-aot-got" BF-A$ HBB-OUT$ RENAME-FILE
   HBB-OUT$ CHMOD-X ;

: HBB-RUN-MAKER ( -- )
   s" hb-aot-got" BF-REMOVE-TMP
   HBB-RUN-MAKER-CMD HBB-FINISH-MAKER
   HBB-INSTALL-AOT ;

: HBB-SUCCESS ( -- )
   s" hb-build OK: " type
   HBB-OUT$ type
   cr ;

: HBB-BUILD-AOT ( -- )
   HBB-RUN-SIGNATURE-LINT
   HBB-RUN-AOT-LINT
   HBB-BUILD-MAKER
   HBB-PREPARE-AOT-SOURCE
   HBB-RUN-MAKER
   HBB-SUCCESS ;

: HBB-MAIN ( -- )
   HBB-PARSE
   HBB-PREPARE-TMP
   HBB-BUILD-AOT
   HBB-CLEANUP ;
