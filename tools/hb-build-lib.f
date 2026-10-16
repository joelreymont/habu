\ hb-build-lib.f - native AOT/REPL build CLI library.

require src/lib/errors.f
require src/lib/string.f
require src/lib/memory.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/le.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/source.f
require src/lib/build.f
require src/lib/codesign.f
require src/lib/content-key.f
require src/lib/adt/option.f                 \ option<NUM:index> STR:INDEX-OF consumer
require src/lib/string-roles.f               \ package STR: the typed string surface
require src/lib/object-resolve.f
require src/lib/build-cache.f
require src/lib/json-write.f
require tools/native-file-inputs.f
require tools/build-support.f
require tools/app-build-image.f
require tools/aot-maker-image.f
require tools/hb-build-report.f
require tools/object-image.f
require tools/event-closure-lib.f

\ The hb-build command's implementation lives in package HB-BUILD-CLI.
\ Everything below is private to it; the export block at the end of the file
\ names the whole surface other files call. HB-BUILD is taken by the report
\ module (tools/hb-build-report.f), a different concern with its own published
\ surface.
package HB-BUILD-CLI
using BUILD-FIXPOINT                     \ shared build paths, child env and source scratch

64 constant HBB-USAGE-RC
66 constant HBB-NOINPUT-RC
67 constant HBB-UNCAUGHT-RC
74 constant HBB-BUILD-RC
34 constant HBB-DQ
10 constant HBB-LF
$2B constant HBB-PLUS
64 constant HBB-HEX-U
96 constant HBB-ABI-CAP
600000 constant HBB-MAKER-DEFAULT-MS
$7FFFFFFF constant HBB-MAKER-MAX-MS
65536 constant HBB-CAPTURE-CAP
72 constant HBB-REPL-DEPS-HEADER-U
80 constant HBB-REPL-SUPPORT-HEADER-U

create HBB-SRC-PATH FS-PATH-CAP allot
create HBB-OUT-PATH FS-PATH-CAP allot
create HBB-MAKER-KEY-HEX 80 allot
create HBB-SRC-CLOSURE-HEX 80 allot
create HBB-CHECKER-ABI-BUF HBB-ABI-CAP allot
create HBB-COMPILER-ABI-BUF HBB-ABI-CAP allot
create HBB-ARTIFACT-PATH FS-PATH-CAP allot
create HBB-STAGE-DIR-BUF FS-PATH-CAP allot
create HBB-STAGE-BUF FS-PATH-CAP allot
create HBB-ARTIFACT-NAME-BUF 128 allot
create HBB-ARTIFACT-KEY-HEX 80 allot
create HBB-REPL-DEPS-PATH FS-PATH-CAP allot
create HBB-REPL-MAKER-INPUTS-PATH FS-PATH-CAP allot
create HBB-REPL-SUPPORT-PATH FS-PATH-CAP allot
create HBB-REPL-SUPPORT-KEY 64 allot
create HBB-REPL-RUNNING-ROOT FS-PATH-CAP allot
create HBB-REPL-SELECTED-ROOT FS-PATH-CAP allot
create HBB-MAKER-RUNNING-ROOT FS-PATH-CAP allot
create HBB-MAKER-SELECTED-ROOT FS-PATH-CAP allot
create HBB-REPL-ENGINE-DIGEST 32 allot
create HBB-REPL-ENGINE-SHA SHA256-FILE-CTX-BYTES allot
create HBB-LF-BUF 1 allot
HBB-LF HBB-LF-BUF c!

\ Preseeded test entry (--preseed-entry / --preseed-seed): a selected non-MAIN
\ entry word plus a forged value-stack seed (big-endian u64 hex, tag last) built
\ into the AOT/object image, so a matched helper reaches its inline invalid-tag
\ die. The entry/seed/mode axis is folded into all three
\ cache layers (artifact key, source-index key, object bytes) so a preseeded run
\ can never restore a stale normal-MAIN artifact. The maker's AOT closure holds
\ an entry name of at most 64 bytes (src/habu/aot-closure.f ENTRY-NAME!).
64 constant HBB-ENTRY-NAME-CAP
2048 constant HBB-SEED-HEX-CAP
create HBB-ENTRY-NAME-BUF HBB-ENTRY-NAME-CAP allot
create HBB-SEED-HEX-BUF HBB-SEED-HEX-CAP allot

variable HBB-OUT-BUF-A
variable HBB-ERR-BUF-A
variable HBB-SRC-U
variable HBB-OUT-U
variable HBB-CHECKER-ABI-U
variable HBB-COMPILER-ABI-U
variable HBB-ARTIFACT-U
variable HBB-REPL-DEPS-U
variable HBB-REPL-MAKER-INPUTS-U
variable HBB-REPL-SUPPORT-U
variable HBB-MAKER-RUNNING-ROOT-U
variable HBB-MAKER-SELECTED-ROOT-U
variable HBB-REPL-RUNNING-ROOT-U
variable HBB-REPL-SELECTED-ROOT-U
variable HBB-REPL-PRODUCER-U
variable HBB-REPL-PRODUCER-READY
variable HBB-STAGE-DIR-U
variable HBB-STAGE-U
variable HBB-ARTIFACT-NAME-U
variable HBB-ARTIFACT-RC
variable HBB-ARTIFACT-CACHE
variable HBB-I
variable HBB-KEY-I
variable HBB-REPL
variable HBB-JSON
variable HBB-REPORT-JSON
variable HBB-SIZE-REPORT
\ Keep the artifact maker report separate from the saved opener image cache.
variable HBB-MAKER-HIT
TYPED-VARIABLE HBB-AOT-MAKER-A ptr u8
variable HBB-AOT-MAKER-U
variable HBB-MAKER-BUILD
variable HBB-MAKER-RUN
variable HBB-ARTIFACT-HIT
variable HBB-OBJECT-HIT
variable HBB-OBJECT-STORE
variable HBB-LINE-START
variable HBB-JSON-FOUND
variable HBB-PRESEED
variable HBB-ENTRY-NAME-U
variable HBB-SEED-HEX-U
variable HBB-PRESEED-MODE
variable HBB-START-NS
variable HBB-ELAPSED-NS
variable HBB-MAKER-TIMEOUT-MS
DYNAMIC-BUFFER HBB-REPL-INPUTS u8
DYNAMIC-BUFFER HBB-REPL-SIDECAR u8
DYNAMIC-BUFFER HBB-REPL-PRODUCER u8
TYPED-VARIABLE HBB-APP-BUILDER-A ptr u8
variable HBB-APP-BUILDER-U

: HBB-PTR-U8-FIELD ( ptr a -- ptr ptr u8 )
   0 ptr-field ;

: HBB-PTR-U8@ ( ptr a -- ptr u8 )
   HBB-PTR-U8-FIELD @ ;

: HBB-PTR-U8! ( ptr u8 ptr a -- )
   HBB-PTR-U8-FIELD ! ;

: HBB-ALLOC-BUF ( n -- ptr u8 )
   MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop ;

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
   s" usage: tools/hb-build.f [--repl] [--json-errors] [--report-json] [--size-report] [--preseed-entry NAME --preseed-seed HEX [--preseed-mode N]] source.f -o out" HBB-USAGE-RC die ;

: HBB-COPY-PATH! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u dst:ptr up:ptr :}
   u FS-PATH-CAP > if E-BUILD-PATH throw then
   a dst u BYTE-COPY
   u up ! ;

: HBB-SRC! ( ptr u8 n -- )
   HBB-SRC-PATH HBB-SRC-U HBB-COPY-PATH! ;

: HBB-OUT! ( ptr u8 n -- )
   HBB-OUT-PATH HBB-OUT-U HBB-COPY-PATH! ;

\ The source path is held in FS-PATH-CAP bytes, so the CLI refuses a longer
\ one by name, before it builds anything.
: HBB-SRC-ARG! ( ptr u8 n -- )
   {: a:ptr u:n :}
   u FS-PATH-CAP > if s" hb-build: source path too long" HBB-USAGE-RC die then
   a u HBB-SRC! ;

\ -o is replaced by renaming over it a sibling made in its directory, named
\ its last name plus a suffix (src/lib/fs-mutate.f REPLACE-STAGED). Before it
\ builds anything the CLI refuses by name an -o with no room for that sibling,
\ longer than SIBLING-PATH-MAX bytes or with a last name empty or longer than
\ SIBLING-NAME-MAX, one in a directory that is not there, and one that is a
\ directory, which a rename cannot replace. An -o with no directory part goes
\ in the working directory.
: HBB-OUT-ARG! ( ptr u8 n -- )
   {: a:ptr u:n :}
   u SIBLING-PATH-MAX > if s" hb-build: output path too long" HBB-USAGE-RC die then
   a u BASENAME nip {: nameu:n :}
   nameu 0= if s" hb-build: output path has no file name" HBB-USAGE-RC die then
   nameu SIBLING-NAME-MAX > if
      s" hb-build: output file name too long" HBB-USAGE-RC die
   then
   u nameu - {: diru:n :}
   diru 0 > if
      a diru DIR? 0= if s" hb-build: no such output directory" HBB-USAGE-RC die then
   then
   a u DIR? if s" hb-build: output path is a directory" HBB-USAGE-RC die then
   a u HBB-OUT! ;

: HBB-SRC$ ( -- ptr u8 n )
   HBB-SRC-PATH HBB-SRC-U @ ;

: HBB-OUT$ ( -- ptr u8 n )
   HBB-OUT-PATH HBB-OUT-U @ ;

: HBB-REPL-DEPS$ ( -- ptr u8 n )
   HBB-REPL-DEPS-PATH HBB-REPL-DEPS-U @ ;

: HBB-REPL-DEPS-SUFFIX$ ( -- ptr u8 n )
   s" .hb-deps" ;

\ A sidecar sits beside the output, but is optional for output names without
\ enough room for the suffix and the atomic writer's own temporary suffix.
: HBB-REPL-DEPS-PATH? ( -- bool )
   HBB-OUT$ {: a:ptr u:n :}
   HBB-REPL-DEPS-SUFFIX$ {: suffix:ptr suffixu:n :}
   u suffixu + SIBLING-PATH-MAX > if HBB-FALSE exit then
   a u BASENAME nip suffixu + SIBLING-NAME-MAX > if HBB-FALSE exit then
   a HBB-REPL-DEPS-PATH u BYTE-COPY
   suffix HBB-REPL-DEPS-PATH u + suffixu BYTE-COPY
   u suffixu + HBB-REPL-DEPS-U !
   HBB-TRUE ;

: HBB-WERR ( ptr u8 n -- ) {: a:ptr u :}
   u 0= if exit then
   2 a u write u <> if s" hb-build: stderr write failed" HBB-BUILD-RC die then ;

: HBB-WOUT-ERR ( n -- ) {: u :}
   u 0 > if HBB-OUT-BUF u HBB-WERR then ;

: HBB-WERR-ERR ( n -- ) {: u :}
   u 0 > if HBB-ERR-BUF u HBB-WERR then ;

: HBB-WERR-LF ( -- )
   HBB-LF-BUF 1 HBB-WERR ;

: HBB-WERR-SEPARATE ( n n -- ) {: outu:n erru:n :}
   erru 0 > if
      HBB-ERR-BUF erru 1- + c@ HBB-LF <> if HBB-WERR-LF then
      exit
   then
   outu 0 > if
      HBB-OUT-BUF outu 1- + c@ HBB-LF <> if HBB-WERR-LF then
   then ;

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
   STR:LENGTH HBB-DQ STR:INDEX-OF MATCH option
     none OF HBB-FALSE ENDOF
     some OF drop HBB-TRUE ENDOF
   ;MATCH ;

: HBB-RESET-OPTIONS ( -- )
   0 HBB-REPL !
   0 HBB-JSON !
   0 HBB-REPORT-JSON !
   0 HBB-SIZE-REPORT !
   0 HBB-PRESEED !
   0 HBB-ENTRY-NAME-U !
   0 HBB-SEED-HEX-U !
   0 HBB-PRESEED-MODE ! ;

: HBB-PRESEED? ( -- bool )
   HBB-PRESEED @ 0 <> ;

: HBB-ENTRY-NAME$ ( -- ptr u8 n )
   HBB-PRESEED? if HBB-ENTRY-NAME-BUF HBB-ENTRY-NAME-U @ else s" MAIN" then ;

: HBB-SEED-HEX$ ( -- ptr u8 n )
   HBB-SEED-HEX-BUF HBB-SEED-HEX-U @ ;

\ A name the maker cannot hold, empty or too long, is refused by name before
\ anything is built.
: HBB-PRESEED-ENTRY! ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0 <= if s" hb-build: --preseed-entry name empty" HBB-USAGE-RC die then
   u HBB-ENTRY-NAME-CAP > if
      s" hb-build: --preseed-entry name too long" HBB-USAGE-RC die
   then
   a HBB-ENTRY-NAME-BUF u BYTE-COPY
   u HBB-ENTRY-NAME-U !
   -1 HBB-PRESEED ! ;

: HBB-PRESEED-SEED! ( ptr u8 n -- ) {: a:ptr u:n :}
   u HBB-SEED-HEX-CAP > if E-STR-CAPACITY throw then
   a HBB-SEED-HEX-BUF u BYTE-COPY
   u HBB-SEED-HEX-U ! ;

: HBB-PRESEED-MODE! ( n -- )
   HBB-PRESEED-MODE ! ;

: HBB-RESET-TRACE ( -- )
   0 HBB-MAKER-HIT !
   0 HBB-MAKER-BUILD !
   0 HBB-MAKER-RUN !
   0 HBB-ARTIFACT-HIT !
   0 HBB-OBJECT-HIT !
   0 HBB-OBJECT-STORE !
   0 HBB-ARTIFACT-CACHE ! ;

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

: HBB-OPT-VALUE$ ( -- ptr u8 n )   \ flag is at HBB-I; advance to and return its value arg
   HBB-INC-I
   HBB-I @ SCRIPT-ARGC >= if HBB-USAGE then
   HBB-I @ HBB-ARG$ ;

: HBB-PARSE-OPTION? ( -- bool )
   HBB-I @ SCRIPT-ARGC >= if HBB-FALSE exit then
   HBB-I @ s" --repl" HBB-ARG= if -1 HBB-REPL ! HBB-INC-I HBB-TRUE exit then
   HBB-I @ s" --json-errors" HBB-ARG= if -1 HBB-JSON ! HBB-INC-I HBB-TRUE exit then
   HBB-I @ s" --report-json" HBB-ARG= if -1 HBB-REPORT-JSON ! HBB-INC-I HBB-TRUE exit then
   HBB-I @ s" --size-report" HBB-ARG= if -1 HBB-SIZE-REPORT ! HBB-INC-I HBB-TRUE exit then
   HBB-I @ s" --preseed-entry" HBB-ARG= if HBB-OPT-VALUE$ HBB-PRESEED-ENTRY! HBB-INC-I HBB-TRUE exit then
   HBB-I @ s" --preseed-seed" HBB-ARG= if HBB-OPT-VALUE$ HBB-PRESEED-SEED! HBB-INC-I HBB-TRUE exit then
   HBB-I @ s" --preseed-mode" HBB-ARG= if
      HBB-OPT-VALUE$ STR>NUMBER? MATCH option
        none OF HBB-USAGE ENDOF
        some OF ENDOF
      ;MATCH
      HBB-PRESEED-MODE! HBB-INC-I HBB-TRUE exit then
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
   HBB-I @ HBB-ARG$ HBB-SRC-ARG!
   HBB-I @ 1+ s" -o" HBB-ARG= 0= if HBB-USAGE then
   HBB-I @ 2 + HBB-ARG$ HBB-OUT-ARG!
   HBB-SRC$ FILE? 0= if s" hb-build: no such source" HBB-NOINPUT-RC die then
   HBB-SRC$ HBB-PATH-HAS-DQ? if s" hb-build: source path contains a double quote" HBB-USAGE-RC die then ;

\ A caller's HB_TMP has to be usable before HB-TMP-MKDIR builds under it: a
\ plain file there is a caller mistake and dies loudly, a missing directory is
\ made. Which base a tree lands in is HB-TMP-MKDIR's answer, not this word's.
: HBB-CHECK-ENV-TMP ( -- )
   s" HB_TMP" GETENV dup 0= if 2drop exit then
   2dup EXISTS? if
      2dup DIR? 0= if s" hb-build: HB_TMP is not a directory" HBB-USAGE-RC die then
   else
      2dup MAKE-DIR
   then
   2drop ;

\ Every build gets a private directory. The engine's build mode writes fixed
\ names into the HB_TMP it is handed (src/habu/aot-lib.f `hb-aot-got`,
\ `hb-aot-obj`; tools/app-build.f `hb-build-got`), so two builds sharing one
\ HB_TMP - every hb-build the gate pool runs at once - read each other's
\ artifacts: one saw its image vanish (E-BUILD-PATH), another ran a stranger's
\ application. A caller's HB_TMP is the base the private directory goes under.
: HBB-PREPARE-TMP ( -- )
   BF-TMP-RESET
   CLEANUP-RESET
   HBB-CHECK-ENV-TMP
   s" hb-build-native" HB-TMP-MKDIR 2dup BF-TMP! CLEANUP-TREE+ ;

: HBB-PREPARE-CACHE ( -- )
   BUILD-CACHE:RESOLVE drop 2drop ;

: HBB-CLEANUP ( -- )
   CLEANUP-RUN
   BF-TMP-RESET ;

: HBB-BAD-MAKER-TIMEOUT ( -- )
   HBB-JSON @ if HB-BUILD:TIMEOUT-ENV-ERROR$ else HB-BUILD:TIMEOUT-ENV-ERROR-TEXT$ then
   HBB-WERR HBB-WERR-LF
   HBB-USAGE-RC HBB-EXIT ;

: HBB-READ-MAKER-TIMEOUT ( -- )
   HBB-MAKER-DEFAULT-MS HBB-MAKER-TIMEOUT-MS !
   s" HB_BUILD_TIMEOUT_MS" GETENV dup 0= if 2drop exit then
   STR>NUMBER? MATCH option
      none OF HBB-BAD-MAKER-TIMEOUT ENDOF
      some OF ENDOF
   ;MATCH {: limit:n :}
   limit 1 < limit HBB-MAKER-MAX-MS > or if HBB-BAD-MAKER-TIMEOUT then
   limit HBB-MAKER-TIMEOUT-MS ! ;

: HBB-CMD-RESET ( -- )
   PROC-ARGV-RESET
   BF-PREPARE-ENV
   PROC-ENV-INHERIT-MISSING ;

: HBB-GOT-NAME$ ( -- ptr u8 n )
   HBB-REPL @ if s" hb-build-got" else s" hb-aot-got" then ;

: HBB-REPL-MAKER-INPUTS$ ( -- ptr u8 n )
   HBB-GOT-NAME$ BF-A$ {: a:ptr u:n :}
   s" .inputs" {: suffix:ptr suffixu:n :}
   u suffixu + FS-PATH-CAP > if E-BUILD-PATH throw then
   a HBB-REPL-MAKER-INPUTS-PATH u BYTE-COPY
   suffix HBB-REPL-MAKER-INPUTS-PATH u + suffixu BYTE-COPY
   u suffixu + HBB-REPL-MAKER-INPUTS-U !
   HBB-REPL-MAKER-INPUTS-PATH HBB-REPL-MAKER-INPUTS-U @ ;

: HBB-OBJ-NAME$ ( -- ptr u8 n )
   s" hb-aot-obj" ;

: HBB-ARTIFACT-NAME$ ( -- ptr u8 n )
   HBB-ARTIFACT-NAME-BUF HBB-ARTIFACT-NAME-U @ ;

: HBB-KEY-FILE+ ( CONTENT-KEY:fold ptr u8 n -- CONTENT-KEY:fold ) {: a:ptr u:n :}
   a u CONTENT-KEY:FILE+ ;

\ Discover the producer's loaded source closure instead of maintaining a
\ second dependency list. The target selector is part of the key; discovery
\ includes every statically reachable target branch.
: HBB-KEY-CLOSURE+ ( CONTENT-KEY:fold ptr u8 n -- CONTENT-KEY:fold ) {: a:ptr u:n :}
   a u EC:BUILD
   0 HBB-KEY-I !
   begin HBB-KEY-I @ EC:COUNT < while
      HBB-KEY-I @ EC:PATH$ CONTENT-KEY:FILE+
      HBB-KEY-I @ 1+ HBB-KEY-I !
   repeat ;

: HBB-KEY-TARGET+ ( CONTENT-KEY:fold -- CONTENT-KEY:fold )
   HB-TARGET-LINUX? if s" target:linux-aarch64" CONTENT-KEY:TEXT+ exit then
   HB-TARGET-MACOS? if s" target:macos-aarch64" CONTENT-KEY:TEXT+ exit then
   HB-TARGET-LINUX-X86-64? if s" target:linux-x86-64" CONTENT-KEY:TEXT+ exit then
   s" hb-build: unknown target" HBB-BUILD-RC die ;

: HBB-MAKER-KEY-IN-ROOT ( -- )
   CONTENT-KEY:OPEN
   s" hb-build-native-producer-v3" CONTENT-KEY:TEXT+
   BF-ENGINE$ HBB-KEY-FILE+
   HBB-KEY-TARGET+
   s" tools/hb-build.f" HBB-KEY-CLOSURE+
   s" tools/aot-maker.f" HBB-KEY-CLOSURE+
   s" tools/aot-build.f" HBB-KEY-CLOSURE+
   HBB-MAKER-KEY-HEX CONTENT-KEY:FINAL-HEX ;

\ The maker resolves support from this invocation's CWD, falling back to the
\ selected engine's root. Hash the same resolved closure for artifact/object
\ identity so a source edit in that root cannot restore an older product.
: HBB-MAKER-KEY! ( -- )
   SOURCE-ROOT:ENGINE$ HBB-MAKER-RUNNING-ROOT
      HBB-MAKER-RUNNING-ROOT-U HBB-COPY-PATH!
   BF-ENGINE$ SOURCE-ROOT:ENGINE-FOR$ HBB-MAKER-SELECTED-ROOT
      HBB-MAKER-SELECTED-ROOT-U HBB-COPY-PATH!
   HBB-MAKER-SELECTED-ROOT HBB-MAKER-SELECTED-ROOT-U @ SOURCE-ROOT:ENGINE!
   [: ['] HBB-MAKER-KEY-IN-ROOT SOURCE-ROOT:WITH-CWD ;] catch {: rc:n :}
   HBB-MAKER-RUNNING-ROOT HBB-MAKER-RUNNING-ROOT-U @ SOURCE-ROOT:ENGINE!
   rc 0<> if rc throw then ;

: HBB-TARGET-UNKNOWN ( -- )
   s" hb-build: unknown target" HBB-BUILD-RC die ;

: HBB-JSON-FLAG$ ( -- ptr u8 n )
   HBB-JSON @ if s" 1" exit then
   s" 0" ;

: HBB-RUN-MAKER-ARGS ( -- )
   HBB-CMD-RESET
   s" --" >LEN PROC-ARGV+
   HBB-SRC$ >LEN PROC-ARGV+
   HBB-JSON-FLAG$ >LEN PROC-ARGV+
   HBB-PRESEED? if
      HBB-ENTRY-NAME$ >LEN PROC-ARGV+
      HBB-SEED-HEX$ >LEN PROC-ARGV+
   then ;

: HBB-FINISH-MAKER-RC ( n n n -- ) {: outu erru rc :}
   rc 0= if exit then
   outu HBB-WOUT-ERR
   HBB-JSON @ if erru HBB-WERR-JSON-ONLY else erru HBB-WERR-ERR then
   rc HBB-EXIT ;

: HBB-MAKER-KIND$ ( -- ptr u8 n )
   HBB-REPL @ if s" repl" else s" aot" then ;

: HBB-MAKER-TIMED-OUT ( n n -- ) {: outu:n erru:n :}
   HBB-JSON @ if
      HBB-MAKER-KIND$ HBB-SRC$ HBB-MAKER-TIMEOUT-MS @
      HBB-OUT-BUF outu HBB-ERR-BUF erru HB-BUILD:MAKER-TIMEOUT-ERROR$
      HBB-WERR HBB-WERR-LF
   else
      outu HBB-WOUT-ERR
      erru HBB-WERR-ERR
      outu erru HBB-WERR-SEPARATE
      HBB-MAKER-KIND$ HBB-SRC$ HBB-MAKER-TIMEOUT-MS @
      HB-BUILD:MAKER-TIMEOUT-ERROR-TEXT$ HBB-WERR HBB-WERR-LF
   then
   E-PROC-TIMEOUT throw ;

: HBB-AOT-MAKER-TIMED-OUT ( -- )
   KEYED-IMAGE:CAPTURE-OUT$ {: out:ptr outu:n :}
   KEYED-IMAGE:CAPTURE-ERR$ {: err:ptr erru:n :}
   out HBB-OUT-BUF outu BYTE-COPY
   err HBB-ERR-BUF erru BYTE-COPY
   outu erru HBB-MAKER-TIMED-OUT ;

: HBB-SELECT-MAKER ( -- )
   HBB-MAKER-TIMEOUT-MS @ AOT-MAKER-IMAGE:SELECT$
   {: image:ptr imageu:n hit:bool :}
   image HBB-AOT-MAKER-A ! imageu HBB-AOT-MAKER-U !
   hit HBB-MAKER-HIT !
   hit 0= HBB-MAKER-BUILD ! ;

\ Select the saved maker before HBB-RUN-MAKER-ARGS stages argv and env: image
\ publication has its own child and staging buffer. RUN loads the linker after
\ OPEN latched the application span.
: HBB-RUN-MAKER-CMD ( -- len len outcome )
   KEYED-IMAGE:CAPTURE-RESET
   ['] HBB-SELECT-MAKER catch {: rc:n :}
   rc E-PROC-TIMEOUT = if HBB-AOT-MAKER-TIMED-OUT then
   rc 0<> if rc throw then
   HBB-RUN-MAKER-ARGS
   HBB-AOT-MAKER-A @ HBB-AOT-MAKER-U @ >LEN
   S\" 1 set-tier\nAOT-MAKER:OPEN\nAOT-MAKER:RUN\n" >LEN
   HBB-OUT-BUF HBB-CAPTURE-CAP >LEN
   HBB-ERR-BUF HBB-CAPTURE-CAP >LEN
   HBB-MAKER-TIMEOUT-MS @ >MS RUN-ARGV-ENV-STDIN-CAPTURE-OUTCOME ;

: HBB-FINISH-MAKER ( len len outcome -- )
   MATCH outcome
      exited OF {: outu:len erru:len rc:n :}
         outu LEN>N erru LEN>N rc HBB-FINISH-MAKER-RC ENDOF
      signaled OF {: outu:len erru:len sig:n :}
         outu LEN>N erru LEN>N sig 128 + HBB-FINISH-MAKER-RC ENDOF
      timeout OF LEN>N swap LEN>N swap HBB-MAKER-TIMED-OUT ENDOF
   ;MATCH ;

\ Every file hb-build writes at -o is written to its sibling and renamed over
\ -o only once it is whole and executable (src/lib/fs-mutate.f REPLACE-STAGED), so
\ a failed install, restore or object write leaves -o as it was and no sibling.
\ HBB-FILLING is set only while the fill runs, so HBB-REPLACE-OUT tells the
\ fill's own failure (its source, the bytes it writes) from the replace's: a
\ sibling the host will not make beside -o, or a rename it refuses, is an
\ output hb-build cannot replace, and the CLI names it (HBB-CLI-MAKER-CODE).
defer HBB-FILL ( ptr u8 n -- )
variable HBB-FILLING

: HBB-FILL-RUN ( ptr u8 n -- )
   -1 HBB-FILLING !
   HBB-FILL
   0 HBB-FILLING ! ;

: HBB-REPLACE-RUN ( -- )
   HBB-OUT$ [: HBB-FILL-RUN ;] REPLACE-STAGED ;

: HBB-REPLACE-OUT ( [ ptr u8 n -- ] -- )
   is HBB-FILL
   0 HBB-FILLING !
   [: HBB-REPLACE-RUN ;] catch {: rc:n :}
   rc 0= if exit then
   HBB-FILLING @ if rc throw then
   rc E-FS-OPEN = rc E-FS-IO = or if E-BUILD-INSTALL throw then
   rc throw ;

\ The engine the child wrote under HB_TMP may be on another filesystem; it is
\ copied.
: HBB-INSTALL-FILL ( ptr u8 n -- )
   {: tmp:ptr tmpu:n :}
   HBB-GOT-NAME$ BF-A$ tmp tmpu COPY-FILE-STREAM
   tmp tmpu CHMOD-X ;

: HBB-INSTALL-OUT ( -- )
   HBB-GOT-NAME$ BF-EXPECT
   [: HBB-INSTALL-FILL ;] HBB-REPLACE-OUT
   HBB-GOT-NAME$ BF-A$ REMOVE-FILE ;

: HBB-ARTIFACT$ ( -- ptr u8 n )
   HBB-ARTIFACT-PATH HBB-ARTIFACT-U @ ;

: HBB-STAGE-DIR$ ( -- ptr u8 n )
   HBB-STAGE-DIR-BUF HBB-STAGE-DIR-U @ ;

: HBB-STAGE$ ( -- ptr u8 n )
   HBB-STAGE-BUF HBB-STAGE-U @ ;

\ The artifact cache's family in the build cache: hb-build-out-<key>, staged in
\ hb-build-out-<seed>-<attempt> work directories.
: HBB-ARTIFACT-STEM$ ( -- ptr u8 n )
   s" hb-build-out" ;

: HBB-ARTIFACT-PREFIX$ ( -- ptr u8 n )
   s" hb-build-out-" ;

: HBB-OPTION-TEXT+ ( CONTENT-KEY:fold ptr u8 n bool -- CONTENT-KEY:fold )
   if CONTENT-KEY:TEXT+ else 2drop then ;

\ The user program's behaviour depends on every file its require/include closure
\ actually loads, so the cache keys fold the whole ordered closure content, not
\ only the top-level source. Discovery rejects fail-closed, so a closure that
\ cannot be reproduced cannot be keyed.
\ Fold the selected-entry / seed / test-mode axis into a content key. A no-op for
\ a normal MAIN build (so non-preseed keys and object bytes stay byte-identical),
\ but for a preseeded run it diverges every cache layer in lockstep: the artifact
\ key, the source-index key (via HBB-CLOSURE-HEX!), and the object source header.
\ Without it a preseeded run would restore a stale normal-MAIN artifact.
: HBB-PRESEED-CK+ ( CONTENT-KEY:fold -- CONTENT-KEY:fold )
   HBB-PRESEED? 0= if exit then
   s" preseed-entry-v1" CONTENT-KEY:TEXT+
   HBB-ENTRY-NAME$ CONTENT-KEY:TEXT+
   HBB-SEED-HEX$ CONTENT-KEY:TEXT+
   SB-RESET s" mode:" SB-APPEND HBB-PRESEED-MODE @ FS-MUT-SB-U SB$ CONTENT-KEY:TEXT+ ;

: HBB-CLOSURE-HEX! ( ptr u8 n ptr u8 -- ) {: a:ptr u:n dst:ptr :}
   CONTENT-KEY:OPEN
   s" user-source-closure-v1" CONTENT-KEY:TEXT+
   a u HBB-KEY-CLOSURE+
   HBB-PRESEED-CK+
   dst CONTENT-KEY:FINAL-HEX ;

: HBB-SRC-CLOSURE-HEX! ( -- )
   HBB-SRC$ HBB-SRC-CLOSURE-HEX HBB-CLOSURE-HEX! ;

\ HBB-ARTIFACT-KEY! selects this build's producer before the maker runs.
\ ABI headers must keep that key when producer files change during the maker.
: HBB-ABI! ( ptr u8 n ptr u8 ptr n -- )
   {: sem:ptr semu:n dst:ptr up:ptr :}
   semu HBB-HEX-U + 1 + HBB-ABI-CAP > if E-STR-CAPACITY throw then
   sem dst semu BYTE-COPY
   HBB-PLUS dst semu + c!
   HBB-MAKER-KEY-HEX dst semu 1 + + HBB-HEX-U BYTE-COPY
   semu HBB-HEX-U + 1 + up ! ;

: HBB-CHECKER-ABI$ ( -- ptr u8 n )
   s" checker-effect-v1" HBB-CHECKER-ABI-BUF HBB-CHECKER-ABI-U HBB-ABI!
   HBB-CHECKER-ABI-BUF HBB-CHECKER-ABI-U @ ;

: HBB-COMPILER-ABI$ ( -- ptr u8 n )
   s" hb-arm64-v1" HBB-COMPILER-ABI-BUF HBB-COMPILER-ABI-U HBB-ABI!
   HBB-COMPILER-ABI-BUF HBB-COMPILER-ABI-U @ ;

: HBB-TARGET-ABI$ ( -- ptr u8 n )
   HB-TARGET-LINUX? if s" linux-aarch64" exit then
   HB-TARGET-MACOS? if s" macos-aarch64" exit then
   HB-TARGET-LINUX-X86-64? if s" linux-x86-64" exit then
   HBB-TARGET-UNKNOWN ;

: HBB-ARTIFACT-KEY! ( -- )
   HBB-MAKER-KEY!
   CONTENT-KEY:OPEN
   s" hb-build-artifact-cache-v3" CONTENT-KEY:TEXT+
   HBB-MAKER-KEY-HEX 64 CONTENT-KEY:TEXT+
   s" json" HBB-JSON @ 0 <> HBB-OPTION-TEXT+
   HBB-SRC-CLOSURE-HEX 64 CONTENT-KEY:TEXT+
   HBB-ARTIFACT-KEY-HEX CONTENT-KEY:FINAL-HEX ;

: HBB-ARTIFACT-NAME! ( -- )
   HBB-ARTIFACT-PREFIX$ {: a:ptr u:n :}
   u 64 + 128 > if E-BUILD-PATH throw then
   a HBB-ARTIFACT-NAME-BUF u BYTE-COPY
   HBB-ARTIFACT-KEY-HEX HBB-ARTIFACT-NAME-BUF u + 64 BYTE-COPY
   u 64 + HBB-ARTIFACT-NAME-U ! ;

: HBB-ARTIFACT-PATHS ( -- )
   HBB-ARTIFACT-KEY!
   HBB-ARTIFACT-NAME!
   BUILD-CACHE:ROOT$ HBB-ARTIFACT-NAME$ HBB-ARTIFACT-PATH JOIN-PATH HBB-ARTIFACT-U ! ;

: HBB-PREPARE-ARTIFACT-CACHE ( -- )
   HBB-PREPARE-CACHE
   \ Keep the source identity selected before lookup through the maker store.
   \ An edit after the maker reads A belongs to the next build, not A's key.
   HBB-SRC-CLOSURE-HEX!
   HBB-ARTIFACT-PATHS
   -1 HBB-ARTIFACT-CACHE ! ;

: HBB-RESTORE-FILL ( ptr u8 n -- )
   {: tmp:ptr tmpu:n :}
   HBB-ARTIFACT$ tmp tmpu COPY-FILE-STREAM
   tmp tmpu CHMOD-X ;

: HBB-RESTORE-OUT ( -- )
   [: HBB-RESTORE-FILL ;] HBB-REPLACE-OUT ;

\ An artifact found in the cache is dated as used (src/lib/build-cache.f), and one a
\ prune took after the check is built again. So is one a prune takes after USED
\ dated it: a copy that fails while the artifact is gone is a miss, and -o is
\ as it was. The copy opens the artifact before it writes a byte, and once open
\ it reads the whole artifact whatever happens to its name.
: HBB-RESTORE-ARTIFACT? ( -- bool )
   HBB-ARTIFACT-CACHE @ 0= if HBB-FALSE exit then
   HBB-ARTIFACT$ EXECUTABLE? 0= if HBB-FALSE exit then
   HBB-ARTIFACT$ BUILD-CACHE:USED 0= if HBB-FALSE exit then
   [: HBB-RESTORE-OUT ;] catch {: code:n :}
   code 0<> if
      HBB-ARTIFACT$ FS-TRY-LSTAT if code throw then
      HBB-FALSE exit
   then
   -1 HBB-ARTIFACT-HIT !
   HBB-TRUE ;

: HBB-OBJECT-LOAD? ( -- bool )
   BUILD-CACHE:ROOT$ OBJRES:ROOT!
   HBB-SRC-CLOSURE-HEX 64 HBB-TARGET-ABI$ HBB-CHECKER-ABI$ HBB-COMPILER-ABI$ OBJRES:LOAD ;

: HBB-OBJ$ ( -- ptr u8 n )
   HBB-OBJ-NAME$ BF-A$ ;

: HBB-READ-OBJECT-TEXT ( -- n )
   HBB-OBJ$ BF-READ-SOURCE
   BF-SOURCE-LEN @ dup 0 <= if E-OBJ-SCHEMA throw then ;

: HBB-BUILD-OBJECT-RECORD ( n -- ) {: textu:n :}
   OBJ:RESET
   HBB-SRC-CLOSURE-HEX 64 OBJ:SOURCE!
   HBB-TARGET-ABI$ OBJ:TARGET!
   HBB-CHECKER-ABI$ OBJ:CHECKER!
   HBB-COMPILER-ABI$ OBJ:COMPILER!
   BF-SOURCE-BUF textu OBJ:TEXT+
   HBB-ENTRY-NAME$ s" --" OBJ:EXPORT+
   HBB-ENTRY-NAME$ 0 s" --" OBJ:DEF+
   HBB-PRESEED? if
      HBB-ENTRY-NAME$ HBB-PRESEED-MODE @ HBB-SEED-HEX$ OBJ:ENTRY+
   then ;

: HBB-STORE-OBJECT ( -- )
   BUILD-CACHE:ROOT$ OBJRES:ROOT!
   HBB-READ-OBJECT-TEXT HBB-BUILD-OBJECT-RECORD
   OBJRES:STORE 2drop
   -1 HBB-OBJECT-STORE ! ;

: HBB-OBJECT-FILL ( ptr u8 n -- )
   {: tmp:ptr tmpu:n :}
   tmp tmpu OBJIMG:WRITE
   tmp tmpu CHMOD-X ;

: HBB-WRITE-OBJECT ( -- )
   OBJIMG:LOAD
   [: HBB-OBJECT-FILL ;] HBB-REPLACE-OUT
   -1 HBB-OBJECT-HIT ! ;

: HBB-OBJECT-HIT? ( -- bool )
   HBB-OBJECT-LOAD? 0= if HBB-FALSE exit then
   HBB-WRITE-OBJECT
   HBB-TRUE ;

\ The artifact is copied into a work directory of its own in the cache and
\ published by rename, so a half-written artifact is never visible under its
\ key, and two builds installing one key each publish whole bytes. The work
\ directory goes whatever the copy did; one a killed build leaves is a stale
\ entry of the family a day later.
: HBB-STAGE-ARTIFACT ( -- )
   HBB-STAGE-DIR$ HBB-ARTIFACT-STEM$ HBB-STAGE-BUF JOIN-PATH HBB-STAGE-U !
   HBB-OUT$ HBB-STAGE$ COPY-FILE-STREAM
   HBB-STAGE$ CHMOD-X
   HBB-STAGE$ HBB-ARTIFACT$ RENAME-FILE ;

: HBB-STAGE-OPEN ( -- )
   BUILD-CACHE:ROOT$ HBB-ARTIFACT-STEM$ MAKE-TEMP-DIR {: a:ptr u:n :}
   a HBB-STAGE-DIR-BUF u BYTE-COPY
   u HBB-STAGE-DIR-U ! ;

\ A published artifact prunes its family, which reports its own failures and
\ never fails the build.
: HBB-INSTALL-ARTIFACT ( -- )
   HBB-ARTIFACT-CACHE @ 0= if exit then
   HBB-ARTIFACT$ EXECUTABLE? if exit then
   HBB-STAGE-OPEN
   [: HBB-STAGE-ARTIFACT ;] catch HBB-ARTIFACT-RC !
   HBB-STAGE-DIR$ REMOVE-TREE
   HBB-ARTIFACT-RC @ 0 <> if HBB-ARTIFACT-RC @ throw then
   HBB-ARTIFACT-PREFIX$ s" " HBB-ARTIFACT-STEM$ HBB-ARTIFACT$ BUILD-CACHE:PRUNE ;

: HBB-RUN-MAKER ( -- )
   -1 HBB-MAKER-RUN !
   HBB-GOT-NAME$ BF-REMOVE-TMP
   HBB-OBJ-NAME$ BF-REMOVE-TMP
   HBB-RUN-MAKER-CMD HBB-FINISH-MAKER ;

: HBB-SUCCESS ( -- )
   s" hb-build OK: " type
   HBB-OUT$ type
   HBB-REPL @ if
      s"  (native application + REPL)"
   else
      s"  (AOT, engine stripped)"
   then type
   cr ;

defer HBB-REPORT-HOOK ( -- )

: HBB-FINISH ( -- )
   mono-ns HBB-START-NS @ - HBB-ELAPSED-NS !
   HBB-REPORT-JSON @ HBB-SIZE-REPORT @ or 0<> if
      s" tools/hb-build-size.f" required
      HBB-REPORT-HOOK
   else
      HBB-SUCCESS
   then ;

\ A plain no-op needs no image byte walk. Explicit report options still ask for
\ the full image census and receive the same report as a newly built image.
: HBB-FINISH-REPL-FRESH ( -- )
   HBB-REPORT-JSON @ HBB-SIZE-REPORT @ or 0<> if HBB-FINISH exit then
   s" hb-build up to date: " type HBB-OUT$ type cr ;

\ The cache path error in the form --json-errors chooses, without its newline.
: HBB-PATH-ERROR$ ( -- ptr u8 n )
   HBB-JSON @ if HB-BUILD:PATH-ERROR$ else HB-BUILD:PATH-ERROR-TEXT$ then ;

: HBB-PATH-ERROR ( -- )
   HBB-PATH-ERROR$ HBB-WERR
   HBB-WERR-LF ;

: HBB-BUILD-BEGIN ( -- )
   mono-ns HBB-START-NS !
   HB-BUILD:RESET
   HBB-RESET-TRACE
   HBB-READ-MAKER-TIMEOUT ;

: HBB-BUILD-REST ( -- )
   HBB-RESTORE-ARTIFACT? if HBB-FINISH exit then
   HBB-OBJECT-HIT? if HBB-INSTALL-ARTIFACT HBB-FINISH exit then
   HBB-RUN-MAKER
   HBB-STORE-OBJECT
   HBB-INSTALL-OUT
   HBB-INSTALL-ARTIFACT
   HBB-FINISH ;

: HBB-REPL-INPUTS-READ ( ptr u8 n -- ptr u8 n )
   {: path:ptr pathu:n :}
   path pathu FILE-SIZE {: size:n :}
   size 0 <= if E-NUNIT-PROFILE throw then
   size HBB-REPL-INPUTS-RESERVE
   path pathu 0 HBB-REPL-INPUTS size READ-ALL size <> if E-FS-IO throw then
   0 HBB-REPL-INPUTS size ;

: HBB-REPL-ENGINE-DIGEST! ( -- )
   HBB-REPL-ENGINE-SHA BF-ENGINE$ HBB-REPL-ENGINE-DIGEST SHA256-FILE-IN
      dup 0<> if throw then drop ;

: HBB-REPL-SUPPORT-PATH$ ( -- ptr u8 n )
   HBB-REPL-SUPPORT-PATH HBB-REPL-SUPPORT-U @ ;

\ Only the engine, invocation directory, and fallback root select fixed build
\ support. Application source and output are checked by the normal sidecar.
: HBB-REPL-SUPPORT-SELECT ( -- )
   HBB-REPL-ENGINE-DIGEST!
   CONTENT-KEY:OPEN
   s" hb-repl-support-v2" CONTENT-KEY:TEXT+
   BF-ENGINE$ CONTENT-KEY:TEXT+
   HBB-REPL-ENGINE-DIGEST CONTENT-KEY:DIGEST+
   SOURCE-ROOT:CWD$ CONTENT-KEY:TEXT+
   HBB-REPL-SELECTED-ROOT HBB-REPL-SELECTED-ROOT-U @ CONTENT-KEY:TEXT+
   HBB-REPL-SUPPORT-KEY CONTENT-KEY:FINAL-HEX
   HBB-REPL-SUPPORT-KEY s" repl-support"
      HBB-REPL-SUPPORT-PATH HBB-REPL-SUPPORT-U KEYED-IMAGE:PATH! ;

: HBB-REPL-FRESH-CHECK ( -- )
   HBB-REPL-DEPS$ HBB-REPL-INPUTS-READ {: bytes:ptr size:n :}
   size HBB-REPL-DEPS-HEADER-U < if E-NUNIT-PROFILE throw then
   bytes 8 s" HBRDEPS3" STR= 0= if E-NUNIT-PROFILE throw then
   bytes 8 + LE:U64@ {: srcu:n :}
   bytes 16 + LE:U64@ {: engu:n :}
   bytes 24 + LE:U64@ {: outu:n :}
   bytes 32 + LE:U64@ {: rootu:n :}
   srcu 0 <= srcu FS-PATH-CAP > or
   engu 0 <= engu FS-PATH-CAP > or or
   outu 0 <= outu FS-PATH-CAP > or or
   rootu 0 <= rootu FS-PATH-CAP > or or if E-NUNIT-PROFILE throw then
   srcu engu + outu + rootu + size HBB-REPL-DEPS-HEADER-U - >= if E-NUNIT-PROFILE throw then
   bytes HBB-REPL-DEPS-HEADER-U + srcu HBB-SRC$ STR= 0= if E-NUNIT-PROFILE throw then
   bytes HBB-REPL-DEPS-HEADER-U srcu + + engu BF-ENGINE$ STR= 0= if E-NUNIT-PROFILE throw then
   ['] HBB-REPL-ENGINE-DIGEST! catch 0<> if E-NUNIT-PROFILE throw then
   bytes 40 + 32 HBB-REPL-ENGINE-DIGEST 32 STR= 0=
      if E-NUNIT-PROFILE throw then
   bytes HBB-REPL-DEPS-HEADER-U srcu + engu + + outu HBB-OUT$ STR= 0= if E-NUNIT-PROFILE throw then
   bytes HBB-REPL-DEPS-HEADER-U srcu + engu + outu + + rootu
      HBB-REPL-SELECTED-ROOT HBB-REPL-SELECTED-ROOT-U @ STR= 0=
      if E-NUNIT-PROFILE throw then
   bytes HBB-REPL-DEPS-HEADER-U srcu + engu + outu + rootu + +
   size HBB-REPL-DEPS-HEADER-U srcu + engu + outu + rootu + - NFILE-INPUTS:CHECK ;

\ The sidecar records the files and resolution queries the actual application
\ load used. A damaged or missing sidecar is a miss; it must never skip a build.
: HBB-REPL-FRESH? ( -- bool )
   HBB-OUT$ EXECUTABLE? 0= if HBB-FALSE exit then
   HBB-REPL-DEPS-PATH? 0= if HBB-FALSE exit then
   HBB-REPL-DEPS$ FILE? 0= if HBB-FALSE exit then
   [: HBB-REPL-FRESH-CHECK ;] catch {: rc:n :}
   HBB-REPL-INPUTS-RELEASE
   rc 0= if HBB-TRUE exit then
   rc E-NUNIT-PROFILE = rc E-NUNIT-FILE = or if HBB-FALSE exit then
   rc throw ;

: HBB-REPL-PRODUCER-CANON ( ptr u8 n -- ptr u8 n bool )
   {: path:ptr pathu:n :}
   path pathu SOURCE-ROOT:CANON-OS {: answer:ptr answeru:n exists:bool :}
   path pathu answer answeru exists NFILE-INPUTS:ADD-CANON
   answer answeru exists ;

: HBB-REPL-STAMP-FILE ( ptr u8 n -- )
   s" " NFILE-INPUTS:ADD-MTIME ;

: HBB-REPL-STAMP-CLOSURE ( ptr u8 n -- )
   SOURCE-ROOT:RESOLVE drop SOURCE-ROOT:RESOLVED-ROOT$
   [: HBB-REPL-STAMP-FILE ;] EC:BUILD-IN-EACH ;

: HBB-REPL-STAMP-LOADS ( -- )
   s" tools/hb-build.f" HBB-REPL-STAMP-CLOSURE
   ['] HBB-REPL-STAMP-FILE HBB-MAKER-TIMEOUT-MS @ APP-BUILD-IMAGE:PATH-EACH$
   HBB-APP-BUILDER-U ! HBB-APP-BUILDER-A ! ;

: HBB-REPL-SUPPORT-CHECK ( -- )
   HBB-REPL-SUPPORT-PATH$ HBB-REPL-INPUTS-READ {: rec:ptr size:n :}
   size HBB-REPL-SUPPORT-HEADER-U <= if E-NUNIT-PROFILE throw then
   rec 8 s" HBRSEL2!" STR= 0= if E-NUNIT-PROFILE throw then
   rec 72 + LE:U64@ size HBB-REPL-SUPPORT-HEADER-U - <>
      if E-NUNIT-PROFILE throw then
   rec HBB-REPL-SUPPORT-HEADER-U +
      size HBB-REPL-SUPPORT-HEADER-U - NFILE-INPUTS:CHECK
   rec 8 + 64 APP-BUILD-IMAGE:CACHED-PATH$
      {: image:ptr imageu:n found:bool :}
   found 0= if exit then
   size HBB-REPL-SUPPORT-HEADER-U - {: rowsu:n :}
   rowsu HBB-REPL-PRODUCER-RESERVE
   rec HBB-REPL-SUPPORT-HEADER-U +
      0 HBB-REPL-PRODUCER rowsu BYTE-COPY
   rowsu HBB-REPL-PRODUCER-U !
   image HBB-APP-BUILDER-A ! imageu HBB-APP-BUILDER-U !
   -1 HBB-REPL-PRODUCER-READY ! ;

: HBB-REPL-SUPPORT-FRESH? ( -- bool )
   HBB-REPL-SUPPORT-PATH$ FILE? 0= if HBB-FALSE exit then
   [: HBB-REPL-SUPPORT-CHECK ;] catch {: rc:n :}
   HBB-REPL-INPUTS-RELEASE
   rc 0= if
      HBB-REPL-PRODUCER-READY @ 0= if HBB-FALSE exit then
      HBB-REPL-SUPPORT-PATH$ BUILD-CACHE:USED if HBB-TRUE exit then
      0 HBB-REPL-PRODUCER-READY ! HBB-FALSE exit
   then
   rc E-NUNIT-PROFILE = rc E-NUNIT-FILE = or if HBB-FALSE exit then
   rc throw ;

: HBB-REPL-SUPPORT-WRITE ( -- )
   HBB-REPL-PRODUCER-U @ {: rowsu:n :}
   HBB-REPL-SUPPORT-HEADER-U rowsu + HBB-REPL-SIDECAR-RESERVE
   0 HBB-REPL-SIDECAR {: rec:ptr :}
   s" HBRSEL2!" drop rec 8 BYTE-COPY
   APP-BUILD-IMAGE:SELECTED-KEY$ drop rec 8 + 64 BYTE-COPY
   rowsu rec 72 + LE:U64!
   0 HBB-REPL-PRODUCER
      rec HBB-REPL-SUPPORT-HEADER-U + rowsu BYTE-COPY
   HBB-REPL-SUPPORT-PATH$ rec HBB-REPL-SUPPORT-HEADER-U rowsu +
      ATOMIC-WRITE-FILE
   s" hb-repl-support-" s" " s" " HBB-REPL-SUPPORT-PATH$ BUILD-CACHE:PRUNE ;

\ Record the selected builder's make dependencies before its image is chosen.
\ An edit while that builder runs belongs to the next invocation.
: HBB-REPL-PRODUCER-STAMP ( -- )
   NFILE-INPUTS:RESET
   HBB-REPL-SUPPORT-SELECT
   HBB-REPL-SUPPORT-FRESH? if exit then
   [: HBB-REPL-PRODUCER-CANON ;] [: SOURCE-ROOT:READ-OS ;] SOURCE-INPUT:USE
   [: HBB-REPL-STAMP-LOADS ;] catch {: rc:n :}
   SOURCE-INPUT:RESET
   rc 0<> if rc throw then
   NFILE-INPUTS:BYTES$ {: rows:ptr rowsu:n :}
   rowsu HBB-REPL-PRODUCER-RESERVE
   rows 0 HBB-REPL-PRODUCER rowsu BYTE-COPY
   rowsu HBB-REPL-PRODUCER-U !
   -1 HBB-REPL-PRODUCER-READY !
   HBB-REPL-SUPPORT-WRITE ;

: HBB-REPL-PRODUCER-PREPARE ( -- )
   0 HBB-REPL-PRODUCER-READY !
   SOURCE-ROOT:ENGINE$ HBB-REPL-RUNNING-ROOT HBB-REPL-RUNNING-ROOT-U HBB-COPY-PATH!
   HBB-REPL-SELECTED-ROOT HBB-REPL-SELECTED-ROOT-U @ SOURCE-ROOT:ENGINE!
   [: ['] HBB-REPL-PRODUCER-STAMP SOURCE-ROOT:WITH-CWD ;] catch {: rc:n :}
   HBB-REPL-RUNNING-ROOT HBB-REPL-RUNNING-ROOT-U @ SOURCE-ROOT:ENGINE!
   NFILE-INPUTS:CLOSE
   rc 0<> if rc throw then ;

: HBB-REPL-ROOT! ( -- )
   BF-ENGINE$ SOURCE-ROOT:ENGINE-FOR$
      HBB-REPL-SELECTED-ROOT HBB-REPL-SELECTED-ROOT-U HBB-COPY-PATH! ;

\ The keyed image already holds fixed app-build support. Resolve it before
\ staging this command's argv/env because image publication stages its own.
: HBB-RESOLVE-APP-BUILDER ( -- )
   KEYED-IMAGE:CAPTURE-RESET
   HBB-REPL-ROOT!
   HBB-REPL-PRODUCER-PREPARE ;

: HBB-APP-BUILDER-TIMED-OUT ( -- )
   KEYED-IMAGE:CAPTURE-OUT$ {: out:ptr outu:n :}
   KEYED-IMAGE:CAPTURE-ERR$ {: err:ptr erru:n :}
   out HBB-OUT-BUF outu BYTE-COPY
   err HBB-ERR-BUF erru BYTE-COPY
   outu erru HBB-MAKER-TIMED-OUT ;

: HBB-RUN-APP-CMD ( -- len len outcome )
   ['] HBB-RESOLVE-APP-BUILDER catch {: rc:n :}
   rc E-PROC-TIMEOUT = if HBB-APP-BUILDER-TIMED-OUT then
   rc 0 <> if rc throw then
   HBB-CMD-RESET
   s" --" >LEN PROC-ARGV+
   HBB-SRC$ >LEN PROC-ARGV+
   HBB-GOT-NAME$ BF-A$ >LEN PROC-ARGV+
   HBB-APP-BUILDER-A @ HBB-APP-BUILDER-U @ >LEN
   S\" 1 set-tier\nAPP-BUILD:RUN-SAVED\n" >LEN
   HBB-OUT-BUF HBB-CAPTURE-CAP >LEN
   HBB-ERR-BUF HBB-CAPTURE-CAP >LEN
   HBB-MAKER-TIMEOUT-MS @ >MS RUN-ARGV-ENV-STDIN-CAPTURE-OUTCOME ;

: HBB-REPL-PACK-INPUTS ( ptr u8 n -- ptr u8 n )
   {: rows:ptr rowsu:n :}
   HBB-SRC$ {: src:ptr srcu:n :}
   BF-ENGINE$ {: eng:ptr engu:n :}
   HBB-OUT$ {: out:ptr outu:n :}
   HBB-REPL-SELECTED-ROOT HBB-REPL-SELECTED-ROOT-U @ {: root:ptr rootu:n :}
   rowsu MEM-MAX-N HBB-REPL-DEPS-HEADER-U - srcu - engu - outu - rootu - >
      if E-NUNIT-PROFILE throw then
   HBB-REPL-DEPS-HEADER-U srcu + engu + outu + rootu + rowsu + {: size:n :}
   size HBB-REPL-SIDECAR-RESERVE
   0 HBB-REPL-SIDECAR {: dst:ptr :}
   s" HBRDEPS3" drop dst 8 BYTE-COPY
   srcu dst 8 + LE:U64!
   engu dst 16 + LE:U64!
   outu dst 24 + LE:U64!
   rootu dst 32 + LE:U64!
   HBB-REPL-ENGINE-DIGEST dst 40 + 32 BYTE-COPY
   src dst HBB-REPL-DEPS-HEADER-U + srcu BYTE-COPY
   eng dst HBB-REPL-DEPS-HEADER-U srcu + + engu BYTE-COPY
   out dst HBB-REPL-DEPS-HEADER-U srcu + engu + + outu BYTE-COPY
   root dst HBB-REPL-DEPS-HEADER-U srcu + engu + outu + + rootu BYTE-COPY
   rows dst HBB-REPL-DEPS-HEADER-U srcu + engu + outu + rootu + + rowsu BYTE-COPY
   dst size ;

: HBB-REPL-SAVE-INPUTS-RUN ( -- )
   HBB-REPL-MAKER-INPUTS$ HBB-REPL-INPUTS-READ
   2dup NFILE-INPUTS:FORMAT
   NFILE-INPUTS:RESET
   NFILE-INPUTS:APPEND
   0 HBB-REPL-PRODUCER HBB-REPL-PRODUCER-U @ NFILE-INPUTS:APPEND
   HBB-OUT$ s" " NFILE-INPUTS:ADD-MTIME
   HBB-REPL-DEPS$ NFILE-INPUTS:BYTES$ HBB-REPL-PACK-INPUTS ATOMIC-WRITE-FILE ;

: HBB-REPL-SAVE-INPUTS ( -- )
   HBB-REPL-DEPS-PATH? HBB-REPL-PRODUCER-READY @ 0<> and if
      [: HBB-REPL-SAVE-INPUTS-RUN ;] catch
   else 0 then
   {: rc:n :}
   NFILE-INPUTS:CLOSE
   HBB-REPL-INPUTS-RELEASE
   HBB-REPL-SIDECAR-RELEASE
   HBB-REPL-PRODUCER-RELEASE
   rc 0<> if rc throw then ;

: HBB-REPL-INVALIDATE-DEPS ( -- )
   HBB-REPL-DEPS-PATH? 0= if exit then
   HBB-REPL-DEPS$ FILE? if HBB-REPL-DEPS$ REMOVE-FILE then ;

: HBB-BUILD-REPL ( -- )
   HBB-PRESEED? if
      s" hb-build: --preseed-entry requires an AOT build" HBB-USAGE-RC die
   then
   HBB-REPL-ROOT!
   HBB-REPL-FRESH? if HBB-FINISH-REPL-FRESH exit then
   HBB-RUN-APP-CMD HBB-FINISH-MAKER
   HBB-REPL-INVALIDATE-DEPS
   HBB-INSTALL-OUT
   HBB-REPL-SAVE-INPUTS
   HBB-FINISH ;

: HBB-BUILD ( -- )
   HBB-BUILD-BEGIN
   HBB-REPL @ if HBB-BUILD-REPL exit then
   HBB-PREPARE-ARTIFACT-CACHE
   HBB-BUILD-REST ;

: HBB-PREPARE-CACHE-CLI ( -- )
   [: HBB-PREPARE-ARTIFACT-CACHE ;] catch {: rc:n :}
   rc 0= if exit then
   rc E-BUILD-PATH = if
      HBB-CLEANUP
      HBB-PATH-ERROR
      HBB-BUILD-RC HBB-EXIT
   then
   rc throw ;

: HBB-CLI-MAKER-CODE ( n -- ) {: rc:n :}
   rc 0= if exit then
   rc E-PROC-TIMEOUT = if
      HBB-CLEANUP
      HBB-UNCAUGHT-RC HBB-EXIT
   then
   rc E-BUILD-INSTALL = if
      HBB-CLEANUP
      s" hb-build: cannot replace output" HBB-BUILD-RC die
   then
   rc throw ;

: HBB-BUILD-CLI ( -- )
   HBB-BUILD-BEGIN
   HBB-REPL @ if [: HBB-BUILD-REPL ;] catch HBB-CLI-MAKER-CODE exit then
   HBB-PREPARE-CACHE-CLI
   [: HBB-BUILD-REST ;] catch HBB-CLI-MAKER-CODE ;

: HBB-MAIN ( -- )
   HBB-PARSE
   HBB-PREPARE-TMP
   HBB-BUILD-CLI
   HBB-CLEANUP ;

\ ---------------------------------------------------------------------------
\ The HB-BUILD-CLI surface: the words other files call, promoted in one place
\ so the whole export list reads at a glance. tools/hb-build.f runs the entry
\ with the package closed.

public

EXPORT HBB-BUILD
EXPORT HBB-EXIT
EXPORT HBB-JSON
EXPORT HBB-MAIN
EXPORT HBB-PATHS!
EXPORT HBB-PRESEED-ENTRY!
EXPORT HBB-PRESEED-SEED!
EXPORT HBB-REPORT-JSON
EXPORT HBB-RESET-OPTIONS
EXPORT HBB-SRC$

;using
;package
