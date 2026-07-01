\ hb-build-lib.f - native AOT/REPL build CLI library.
\ Load after lib/errors.f, lib/string.f, lib/fs.f, lib/fs-mutate.f,
\ lib/process.f, lib/process-argv.f, lib/process-env.f, lib/build.f,
\ lib/memory.f, lib/source.f, lib/codesign.f, lib/content-key.f,
\ tools/build-fixpoint.f, and tools/warm-run.f.

64 constant HBB-USAGE-RC
66 constant HBB-NOINPUT-RC
74 constant HBB-BUILD-RC
34 constant HBB-DQ
10 constant HBB-LF
120000 constant HBB-TIMEOUT-MS
65536 constant HBB-CAPTURE-CAP

create HBB-SRC-PATH FS-PATH-CAP allot
create HBB-OUT-PATH FS-PATH-CAP allot
create HBB-MAKER-PATH FS-PATH-CAP allot
create HBB-MAKER-NAME-BUF 128 allot
create HBB-MAKER-KEY-HEX 80 allot
create HBB-SRC-DIGEST 40 allot
create HBB-CACHE-ROOT-BUF FS-PATH-CAP allot
create HBB-ARTIFACT-PATH FS-PATH-CAP allot
create HBB-ARTIFACT-TMP-PATH FS-PATH-CAP allot
create HBB-ARTIFACT-LOCK-PATH FS-PATH-CAP allot
create HBB-ARTIFACT-NAME-BUF 128 allot
create HBB-ARTIFACT-KEY-HEX 80 allot
create HBB-LF-BUF 1 allot
HBB-LF HBB-LF-BUF c!

variable HBB-OUT-BUF-A
variable HBB-ERR-BUF-A
variable HBB-SRC-U
variable HBB-OUT-U
variable HBB-MAKER-U
variable HBB-MAKER-NAME-U
variable HBB-CACHE-ROOT-U
variable HBB-ARTIFACT-U
variable HBB-ARTIFACT-TMP-U
variable HBB-ARTIFACT-LOCK-U
variable HBB-ARTIFACT-NAME-U
variable HBB-ARTIFACT-RC
variable HBB-ARTIFACT-CACHE
variable HBB-I
variable HBB-REPL
variable HBB-JSON
variable HBB-STRICT
variable HBB-MAKER-HIT
variable HBB-MAKER-BUILD
variable HBB-MAKER-RUN
variable HBB-ARTIFACT-HIT
variable HBB-LINE-START
variable HBB-JSON-FOUND

: HBB-PTR-U8-FIELD ( ptr a -- ptr ptr u8 )
   0 ptr-field ;

: HBB-PTR-U8@ ( ptr a -- ptr u8 )
   HBB-PTR-U8-FIELD @ ;

: HBB-PTR-U8! ( ptr u8 ptr a -- )
   HBB-PTR-U8-FIELD ! ;

: HBB-ALLOC-BUF ( n -- ptr u8 )
   MEM-ALLOC-BYTES drop ;

: HBB-BUF ( ptr a n -- ptr u8 ) {: slot:ptr cap :}
   slot @ 0= if cap HBB-ALLOC-BUF slot HBB-PTR-U8! then
   slot HBB-PTR-U8@ ;

: HBB-OUT-BUF ( -- ptr u8 )
   HBB-OUT-BUF-A HBB-CAPTURE-CAP HBB-BUF ;

: HBB-ERR-BUF ( -- ptr u8 )
   HBB-ERR-BUF-A HBB-CAPTURE-CAP HBB-BUF ;

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

: HBB-CACHE-ROOT! ( ptr u8 n -- )
   HBB-CACHE-ROOT-BUF HBB-CACHE-ROOT-U HBB-COPY-PATH! ;

: HBB-CACHE-ROOT-SET? ( -- bool )
   HBB-CACHE-ROOT-U @ 0 > ;

: HBB-CACHE-ROOT-BUF$ ( -- ptr u8 n )
   HBB-CACHE-ROOT-BUF HBB-CACHE-ROOT-U @ ;

: HBB-CACHE-ROOT? ( -- bool )
   HBB-CACHE-ROOT-SET? if HBB-TRUE exit then
   s" HABU_BUILD_CACHE" GETENV dup 0= if
      2drop HBB-FALSE exit
   then
   2drop HBB-TRUE ;

: HBB-CACHE-ROOT$ ( -- ptr u8 n )
   HBB-CACHE-ROOT-SET? if HBB-CACHE-ROOT-BUF$ exit then
   s" HABU_BUILD_CACHE" GETENV dup 0= if
      2drop s" hb-build: missing build cache" HBB-BUILD-RC die
   then ;

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

: HBB-RESET-OPTIONS ( -- )
   0 HBB-REPL !
   0 HBB-JSON !
   0 HBB-STRICT ! ;

: HBB-RESET-TRACE ( -- )
   0 HBB-MAKER-HIT !
   0 HBB-MAKER-BUILD !
   0 HBB-MAKER-RUN !
   0 HBB-ARTIFACT-HIT !
   0 HBB-ARTIFACT-CACHE ! ;

: HBB-STRICT-ON ( -- )
   -1 HBB-STRICT ! ;

: HBB-REPL-ON ( -- )
   -1 HBB-REPL ! ;

: HBB-PATHS! ( ptr u8 n ptr u8 n -- )
   {: src:ptr srcu out:ptr outu :}
   src srcu HBB-SRC!
   out outu HBB-OUT! ;

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
   HBB-RESET-OPTIONS
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

: HBB-PREPARE-MAKER-CACHE ( -- )
   HBB-CACHE-ROOT? 0= if exit then
   HBB-CACHE-ROOT$
   2dup EXISTS? if
      2dup DIR? 0= if s" hb-build: HABU_BUILD_CACHE is not a directory" HBB-USAGE-RC die then
      2drop
   else
      MAKE-DIRS
   then ;

: HBB-CLEANUP ( -- )
   CLEANUP-RUN
   BF-TMP-RESET ;

: HBB-CMD-RESET ( -- )
   PROC-ARGV-RESET
   BF-PREPARE-ENV
   PROC-ENV-INHERIT-MISSING ;

: HBB-LOAD-END ( -- )
   s" --"  >LEN PROC-ARGV+ ;

: HBB-ADD-LINT-LOADS ( -- )
   s" --load"  >LEN PROC-ARGV+
   s" lib/errors.f"  >LEN PROC-ARGV+
   s" lib/string.f"  >LEN PROC-ARGV+
   s" lib/memory.f"  >LEN PROC-ARGV+
   s" lib/vector.f"  >LEN PROC-ARGV+
   s" tools/lint/text.f"  >LEN PROC-ARGV+ s" tools/lint/token.f" >LEN PROC-ARGV+ s" tools/lint/lib.f" >LEN PROC-ARGV+
   s" tools/lint/json-writer.f"  >LEN PROC-ARGV+
   s" tools/lint/source-lex.f"  >LEN PROC-ARGV+
   s" tools/argv.f"  >LEN PROC-ARGV+ ;

: HBB-ADD-AOT-LINT-ENTRY ( -- )
   s" tools/aot-lint.f" WR-TOOLS-LOAD if exit then
   HBB-ADD-LINT-LOADS
   s" tools/aot-lint-core.f"  >LEN PROC-ARGV+
   s" tools/aot-lint.f"  >LEN PROC-ARGV+
   HBB-LOAD-END ;

: HBB-ADD-SIGNATURE-LINT-ENTRY ( -- )
   s" tools/signature-lint.f" WR-TOOLS-LOAD if exit then
   HBB-ADD-LINT-LOADS
   s" tools/signature-lint-core.f"  >LEN PROC-ARGV+
   s" tools/signature-lint.f"  >LEN PROC-ARGV+
   HBB-LOAD-END ;

: HBB-ADD-DIAG-ORIGIN-ENTRY ( -- )
   s" tools/diag-origin.f" WR-TOOLS-LOAD if exit then
   s" --load"  >LEN PROC-ARGV+
   s" lib/errors.f"  >LEN PROC-ARGV+
   s" lib/string.f"  >LEN PROC-ARGV+
   s" lib/memory.f"  >LEN PROC-ARGV+
   s" tools/lint/text.f"  >LEN PROC-ARGV+
   s" tools/lint/token.f" >LEN PROC-ARGV+
   s" tools/lint/lib.f" >LEN PROC-ARGV+
   s" tools/diag-origin-core.f"  >LEN PROC-ARGV+
   s" tools/diag-origin.f"  >LEN PROC-ARGV+
   HBB-LOAD-END ;

: HBB-ADD-AOT-LINT-CMD ( -- )
   HBB-CMD-RESET
   HBB-ADD-AOT-LINT-ENTRY
   HBB-JSON @ if s" --json"  >LEN PROC-ARGV+ then
   HBB-SRC$  >LEN PROC-ARGV+ ;

: HBB-ADD-SIGNATURE-LINT-CMD ( -- )
   HBB-CMD-RESET
   HBB-ADD-SIGNATURE-LINT-ENTRY
   HBB-JSON @ if s" --json"  >LEN PROC-ARGV+ then
   HBB-SRC$  >LEN PROC-ARGV+ ;

: HBB-ADD-DIAG-ORIGIN-CMD ( -- )
   HBB-CMD-RESET
   HBB-ADD-DIAG-ORIGIN-ENTRY
   HBB-SRC$  >LEN PROC-ARGV+ ;

: HBB-CAPTURE>N ( len len rc -- n n n ) {: outu erru rc :}
   outu LEN>N erru LEN>N rc RC>N ;

: HBB-RUN-HB-CAPTURE ( -- n n n )
   WR-TOOLS$ >LEN HBB-OUT-BUF HBB-CAPTURE-CAP >LEN HBB-ERR-BUF HBB-CAPTURE-CAP >LEN
   HBB-TIMEOUT-MS >MS RUN-ARGV-ENV-CAPTURE
   HBB-CAPTURE>N ;

: HBB-RUN-DIAG-CAPTURE ( -- n n n )
   WR-TOOLS$ >LEN BF-SOURCE-BUF BF-SOURCE-CAP >LEN HBB-ERR-BUF HBB-CAPTURE-CAP >LEN
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

: HBB-RUN-AOT-LINT-CHILD ( -- )
   HBB-ADD-AOT-LINT-CMD
   HBB-RUN-HB-CAPTURE HBB-FINISH-TOOL ;

: HBB-RUN-SIGNATURE-LINT-CHILD ( -- )
   HBB-ADD-SIGNATURE-LINT-CMD
   HBB-RUN-HB-CAPTURE HBB-FINISH-TOOL ;

defer HBB-AOT-LINT-HOOK ( -- )
defer HBB-SIGNATURE-LINT-HOOK ( -- )

: HBB-INSTALL-CHILD-LINTS ( -- )
   [: HBB-RUN-AOT-LINT-CHILD ;] is HBB-AOT-LINT-HOOK
   [: HBB-RUN-SIGNATURE-LINT-CHILD ;] is HBB-SIGNATURE-LINT-HOOK ;

HBB-INSTALL-CHILD-LINTS

: HBB-RUN-AOT-LINT ( -- )
   HBB-REPL @ if exit then
   HBB-AOT-LINT-HOOK ;

: HBB-RUN-SIGNATURE-LINT ( -- )
   HBB-STRICT @ 0= if exit then
   HBB-SIGNATURE-LINT-HOOK ;

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

: HBB-MAKER-NAME$ ( -- ptr u8 n )
   HBB-MAKER-NAME-BUF HBB-MAKER-NAME-U @ ;

: HBB-ARTIFACT-NAME$ ( -- ptr u8 n )
   HBB-ARTIFACT-NAME-BUF HBB-ARTIFACT-NAME-U @ ;

: HBB-KEY-FILE+ ( ptr u8 n -- ) {: a:ptr u:n :}
   a u CK-FILE+ ;

: HBB-KEY-LOAD-FILES ( -- )
   s" lib/errors.f" HBB-KEY-FILE+
   s" lib/string.f" HBB-KEY-FILE+
   s" lib/memory.f" HBB-KEY-FILE+
   s" lib/fs.f" HBB-KEY-FILE+
   s" lib/fs-mutate.f" HBB-KEY-FILE+
   s" lib/process.f" HBB-KEY-FILE+
   s" lib/process-argv.f" HBB-KEY-FILE+
   s" lib/process-env.f" HBB-KEY-FILE+
   s" lib/source.f" HBB-KEY-FILE+
   s" lib/build.f" HBB-KEY-FILE+
   s" lib/codesign.f" HBB-KEY-FILE+
   s" lib/content-key.f" HBB-KEY-FILE+
   s" tools/build-fixpoint.f" HBB-KEY-FILE+
   s" tools/warm-run.f" HBB-KEY-FILE+
   s" tools/hb-build-lib.f" HBB-KEY-FILE+ ;

: HBB-KEY-COMMON-SOURCES ( -- )
   s" src/habu/hide.f" HBB-KEY-FILE+
   s" src/core/util.f" HBB-KEY-FILE+
   s" src/core/structures.f" HBB-KEY-FILE+
   s" src/core/checker.f" HBB-KEY-FILE+
   s" src/core/render.f" HBB-KEY-FILE+
   s" src/core/check-hook.f" HBB-KEY-FILE+
   s" src/core/structures-effects.f" HBB-KEY-FILE+
   s" src/core/roles.f" HBB-KEY-FILE+
   s" src/core/bytes.f" HBB-KEY-FILE+
   s" src/arch/arm64/asm.f" HBB-KEY-FILE+
   s" src/arch/arm64/icode.f" HBB-KEY-FILE+
   s" src/arch/arm64/mnem.f" HBB-KEY-FILE+
   s" src/habu/layout.f" HBB-KEY-FILE+
   s" src/os/env-base.f" HBB-KEY-FILE+
   s" src/os/script-argv.f" HBB-KEY-FILE+
   s" src/core/enums.f" HBB-KEY-FILE+
   s" src/core/exec-vector.f" HBB-KEY-FILE+
   s" src/core/sha256.f" HBB-KEY-FILE+
   s" src/core/combinators.f" HBB-KEY-FILE+
   s" src/habu/treeshake.f" HBB-KEY-FILE+
   s" src/habu/rt.f" HBB-KEY-FILE+
   s" src/habu/crash.f" HBB-KEY-FILE+
   s" src/os/image-bytes.f" HBB-KEY-FILE+
   s" src/habu/habu1.f" HBB-KEY-FILE+
   s" src/habu/prof.f" HBB-KEY-FILE+
   s" src/habu/regalloc.f" HBB-KEY-FILE+
   s" src/habu/jit.f" HBB-KEY-FILE+
   s" src/habu/habu2.f" HBB-KEY-FILE+
   s" src/habu/xref.f" HBB-KEY-FILE+
   s" src/habu/driver-io.f" HBB-KEY-FILE+
   s" src/habu/maker.f" HBB-KEY-FILE+ ;

: HBB-KEY-LINUX-SOURCES ( -- )
   s" target:linux-aarch64" CK-TEXT+
   s" src/os/linux/target.f" HBB-KEY-FILE+
   s" src/os/linux/layout.f" HBB-KEY-FILE+
   s" src/os/linux/sys.f" HBB-KEY-FILE+
   s" src/os/linux/elf.f" HBB-KEY-FILE+
   s" src/os/linux/sign.f" HBB-KEY-FILE+ ;

: HBB-KEY-MACOS-SOURCES ( -- )
   s" target:macos-aarch64" CK-TEXT+
   s" src/os/macos/target.f" HBB-KEY-FILE+
   s" src/os/macos/layout.f" HBB-KEY-FILE+
   s" src/os/macos/sys.f" HBB-KEY-FILE+
   s" src/os/macos/macho.f" HBB-KEY-FILE+
   s" src/os/macos/sign2.f" HBB-KEY-FILE+ ;

: HBB-KEY-TARGET-SOURCES ( -- )
   HB-TARGET-LINUX? if HBB-KEY-LINUX-SOURCES exit then
   HB-TARGET-MACOS? if HBB-KEY-MACOS-SOURCES exit then
   s" hb-build: unknown target" HBB-BUILD-RC die ;

: HBB-KEY-DRIVER-SOURCES ( -- )
   HBB-REPL @ if
      s" maker-mode:repl" CK-TEXT+
      s" src/habu/verify-source.f" HBB-KEY-FILE+
      s" src/habu/build.f" HBB-KEY-FILE+
      exit
   then
   s" maker-mode:aot" CK-TEXT+
   s" src/habu/aot-closure.f" HBB-KEY-FILE+
   s" src/habu/aot-lib.f" HBB-KEY-FILE+
   s" src/habu/aot.f" HBB-KEY-FILE+ ;

: HBB-MAKER-KEY! ( -- )
   CK-RESET
   s" hb-build-maker-cache-v2" CK-TEXT+
   s" bin/hb" HBB-KEY-FILE+
   HBB-KEY-LOAD-FILES
   HBB-KEY-COMMON-SOURCES
   HBB-KEY-TARGET-SOURCES
   HBB-KEY-DRIVER-SOURCES
   HBB-MAKER-KEY-HEX CK-FINAL-HEX ;

: HBB-MAKER-NAME! ( -- )
   HBB-MK-NAME$ {: a:ptr u:n :}
   u 65 + 128 > if E-BUILD-PATH throw then
   a HBB-MAKER-NAME-BUF u BYTE-COPY
   45 HBB-MAKER-NAME-BUF u + c!
   HBB-MAKER-KEY-HEX HBB-MAKER-NAME-BUF u 1 + + 64 BYTE-COPY
   u 65 + HBB-MAKER-NAME-U ! ;

: HBB-MAKER-TMP$ ( -- ptr u8 n )
   BF-TMP$ HBB-MK-NAME$ HBB-MAKER-PATH JOIN-PATH HBB-MAKER-U !
   HBB-MAKER-PATH HBB-MAKER-U @ ;

: HBB-MAKER-CACHE$ ( ptr u8 n -- ptr u8 n ) {: root:ptr rootu:n :}
   root rootu MAKE-DIRS
   HBB-MAKER-KEY!
   HBB-MAKER-NAME!
   root rootu HBB-MAKER-NAME$ HBB-MAKER-PATH JOIN-PATH HBB-MAKER-U !
   HBB-MAKER-PATH HBB-MAKER-U @ ;

: HBB-MAKER$ ( -- ptr u8 n )
   HBB-CACHE-ROOT? 0= if HBB-MAKER-TMP$ exit then
   HBB-CACHE-ROOT$ HBB-MAKER-CACHE$ ;

: HBB-SUFFIX! ( ptr u8 n ptr u8 n ptr u8 ptr n -- )
   {: a:ptr u suf:ptr su dst:ptr up:ptr :}
   u su + FS-PATH-CAP > if E-BUILD-PATH throw then
   a dst u BYTE-COPY
   suf dst u + su BYTE-COPY
   u su + up ! ;

: HBB-MAKER-SRC-NAME$ ( -- ptr u8 n )
   s" hb-maker-src" ;

: HBB-APPEND-DRIVER ( ptr u8 n -- ) {: out:ptr outu :}
   HBB-REPL @ if out outu s" src/habu/verify-source.f" BF-APPEND-SOURCE then
   HBB-REPL @ 0= if out outu s" src/habu/aot-closure.f" BF-APPEND-SOURCE then
   HBB-REPL @ 0= if out outu s" src/habu/aot-lib.f" BF-APPEND-SOURCE then
   out outu HBB-DRIVER$ BF-APPEND-SOURCE ;

: HBB-MAKER-SOURCE ( -- )
   HBB-MAKER-SRC-NAME$ BF-RESET-OUT
   HBB-MAKER-SRC-NAME$ BF-APPEND-RUN-PRELUDE
   HBB-MAKER-SRC-NAME$ BF-APPEND-COMMON
   HBB-MAKER-SRC-NAME$ BF-APPEND-DRIVER-IO
   HBB-MAKER-SRC-NAME$ HBB-APPEND-DRIVER ;

: HBB-STAGE2-SOURCE ( -- )
   s" stage2-src" BF-RESET-OUT
   s" stage2-src" BF-APPEND-RUN-PRELUDE
   s" stage2-src" BF-APPEND-COMMON
   s" stage2-src" BF-APPEND-DRIVER-IO
   s" stage2-src" s" src/habu/maker.f" BF-APPEND-SOURCE ;

: HBB-MAKER-READY? ( -- bool )
   HBB-MAKER$ EXECUTABLE? ;

: HBB-INSTALL-MAKER ( -- )
   s" stage2-got" BF-A$ HBB-MAKER$ RENAME-FILE
   HBB-MAKER$ CHMOD-X ;

: HBB-BUILD-MAKER-FRESH ( -- )
   HBB-MAKER-READY? if exit then
   HBB-MAKER-SOURCE
   HBB-STAGE2-SOURCE
   s" stage2-got" BF-REMOVE-TMP
   HBB-MK-NAME$ BF-REMOVE-TMP
   s" bin/hb" s" stage2-src" BF-A$ BF-RUN-LOAD-STAGE
   dup 0 <> if
      s" hb-build: native maker build failed" HBB-BUILD-RC die
   then drop
   s" stage2-got" BF-EXPECT
   HBB-INSTALL-MAKER ;

: HBB-BUILD-MAKER ( -- )
   HBB-PREPARE-MAKER-CACHE
   HBB-MAKER-READY? if -1 HBB-MAKER-HIT ! exit then
   -1 HBB-MAKER-BUILD !
   HBB-BUILD-MAKER-FRESH ;

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
   HBB-SRC-NAME$ s" src/habu/bundle-argv.f" BF-APPEND-SOURCE ;

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

: HBB-JSON-FLAG$ ( -- ptr u8 n )
   HBB-JSON @ if s" 1" exit then
   s" 0" ;

: HBB-RUN-MAKER-ARGS ( -- )
   PROC-ARGV-RESET
   HBB-SRC$ >LEN PROC-ARGV+
   HBB-JSON-FLAG$ >LEN PROC-ARGV+ ;

: HBB-RUN-MAKER-CMD ( -- n n n )
   HBB-RUN-MAKER-ARGS
   BF-PREPARE-ENV
   HBB-MAKER$ >LEN HBB-OUT-BUF HBB-CAPTURE-CAP >LEN HBB-ERR-BUF HBB-CAPTURE-CAP >LEN
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

: HBB-ARTIFACT$ ( -- ptr u8 n )
   HBB-ARTIFACT-PATH HBB-ARTIFACT-U @ ;

: HBB-ARTIFACT-TMP$ ( -- ptr u8 n )
   HBB-ARTIFACT-TMP-PATH HBB-ARTIFACT-TMP-U @ ;

: HBB-ARTIFACT-LOCK$ ( -- ptr u8 n )
   HBB-ARTIFACT-LOCK-PATH HBB-ARTIFACT-LOCK-U @ ;

: HBB-OPTION-TEXT+ ( ptr u8 n bool -- )
   if CK-TEXT+ else 2drop then ;

: HBB-SRC-DIGEST+ ( -- )
   s" user-source" CK-TEXT+
   HBB-SRC$ HBB-SRC-DIGEST SHA256-FILE dup 0 <> if throw then drop
   HBB-SRC-DIGEST CK-DIGEST+ ;

: HBB-ARTIFACT-KEY! ( -- )
   HBB-MAKER-KEY!
   CK-RESET
   s" hb-build-artifact-cache-v1" CK-TEXT+
   HBB-MAKER-KEY-HEX 64 CK-TEXT+
   s" strict" HBB-STRICT @ 0 <> HBB-OPTION-TEXT+
   s" json" HBB-JSON @ 0 <> HBB-OPTION-TEXT+
   s" tools/diag-origin-core.f" HBB-KEY-FILE+
   s" tools/diag-origin.f" HBB-KEY-FILE+
   HBB-SRC-DIGEST+
   HBB-ARTIFACT-KEY-HEX CK-FINAL-HEX ;

: HBB-ARTIFACT-NAME! ( -- )
   s" hb-build-out" {: a:ptr u:n :}
   u 65 + 128 > if E-BUILD-PATH throw then
   a HBB-ARTIFACT-NAME-BUF u BYTE-COPY
   45 HBB-ARTIFACT-NAME-BUF u + c!
   HBB-ARTIFACT-KEY-HEX HBB-ARTIFACT-NAME-BUF u 1 + + 64 BYTE-COPY
   u 65 + HBB-ARTIFACT-NAME-U ! ;

: HBB-ARTIFACT-PATHS ( -- )
   HBB-ARTIFACT-KEY!
   HBB-ARTIFACT-NAME!
   HBB-CACHE-ROOT$ HBB-ARTIFACT-NAME$ HBB-ARTIFACT-PATH JOIN-PATH HBB-ARTIFACT-U !
   HBB-ARTIFACT$ s" .tmp" HBB-ARTIFACT-TMP-PATH HBB-ARTIFACT-TMP-U HBB-SUFFIX!
   HBB-ARTIFACT$ s" .lock" HBB-ARTIFACT-LOCK-PATH HBB-ARTIFACT-LOCK-U HBB-SUFFIX! ;

: HBB-PREPARE-ARTIFACT-CACHE ( -- )
   HBB-CACHE-ROOT? 0= if exit then
   HBB-PREPARE-MAKER-CACHE
   HBB-ARTIFACT-PATHS
   -1 HBB-ARTIFACT-CACHE ! ;

: HBB-RESTORE-ARTIFACT? ( -- bool )
   HBB-ARTIFACT-CACHE @ 0= if HBB-FALSE exit then
   HBB-ARTIFACT$ EXECUTABLE? 0= if HBB-FALSE exit then
   HBB-REMOVE-OUT
   HBB-ARTIFACT$ HBB-OUT$ COPY-FILE-STREAM
   HBB-OUT$ CHMOD-X
   -1 HBB-ARTIFACT-HIT !
   HBB-TRUE ;

: HBB-ARTIFACT-LOCK-BUSY? ( -- bool )
   HBB-ARTIFACT-LOCK$ DIR? if HBB-TRUE exit then
   HBB-ARTIFACT-LOCK$ EXISTS? if E-FS-IO throw then
   HBB-FALSE ;

: HBB-TRY-ARTIFACT-LOCK? ( -- bool )
   HBB-ARTIFACT-LOCK$ FS-PATHZ FS-MUT-MODE-PRIVATE-DIR mkdir 0= if HBB-TRUE exit then
   HBB-ARTIFACT-LOCK-BUSY? if HBB-FALSE exit then
   E-FS-IO throw ;

: HBB-RELEASE-ARTIFACT-LOCK ( -- )
   HBB-ARTIFACT-LOCK$ DIR? if HBB-ARTIFACT-LOCK$ REMOVE-DIR then ;

: HBB-INSTALL-ARTIFACT-LOCKED ( -- )
   HBB-ARTIFACT$ EXECUTABLE? if exit then
   HBB-ARTIFACT-TMP$ EXISTS? if HBB-ARTIFACT-TMP$ REMOVE-FILE then
   HBB-OUT$ HBB-ARTIFACT-TMP$ COPY-FILE-STREAM
   HBB-ARTIFACT-TMP$ CHMOD-X
   HBB-ARTIFACT-TMP$ HBB-ARTIFACT$ RENAME-FILE ;

: HBB-INSTALL-ARTIFACT ( -- )
   HBB-ARTIFACT-CACHE @ 0= if exit then
   HBB-ARTIFACT$ EXECUTABLE? if exit then
   HBB-TRY-ARTIFACT-LOCK? 0= if exit then
   [: HBB-INSTALL-ARTIFACT-LOCKED ;] catch HBB-ARTIFACT-RC !
   HBB-RELEASE-ARTIFACT-LOCK
   HBB-ARTIFACT-RC @ 0 <> if HBB-ARTIFACT-RC @ throw then ;

: HBB-RUN-MAKER ( -- )
   -1 HBB-MAKER-RUN !
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

: HBB-BUILD ( -- )
   HBB-RESET-TRACE
   HBB-RUN-SIGNATURE-LINT
   HBB-RUN-AOT-LINT
   HBB-PREPARE-ARTIFACT-CACHE
   HBB-RESTORE-ARTIFACT? if HBB-SUCCESS exit then
   HBB-PREPARE-PROGRAM-SOURCE
   HBB-BUILD-MAKER
   HBB-RUN-MAKER
   HBB-INSTALL-ARTIFACT
   HBB-SUCCESS ;

: HBB-MAIN ( -- )
   HBB-PARSE
   HBB-PREPARE-TMP
   HBB-BUILD
   HBB-CLEANUP ;
