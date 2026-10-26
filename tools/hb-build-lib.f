\ hb-build-lib.f - native AOT/REPL build CLI library.

require src/lib/errors.f
require src/lib/string.f
require src/lib/fmt.f
require src/lib/memory.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/le.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/engine-id.f
require src/os/env-base.f
require src/lib/source.f
require src/lib/build.f
require src/lib/codesign.f
require src/lib/content-key.f
require src/core/sha256.f
require src/lib/adt/option.f                 \ option<NUM:index> STR:INDEX-OF consumer
require src/lib/string-roles.f               \ package STR: the typed string surface
require src/lib/object-resolve.f
require src/lib/build-cache.f
require src/lib/json-write.f
require tools/native-file-inputs.f
require tools/build-support.f
require tools/app-build-image.f
require tools/aot-builder-image.f
require tools/hb-build-report.f
require tools/object-image.f
require tools/event-closure-lib.f
require tools/image-names.f
require tools/native-build-image.f
require tools/build-target.f

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
80 constant HBB-MAKER-SUPPORT-HEADER-U

create HBB-SRC-PATH FS-PATH-CAP allot
create HBB-OUT-PATH FS-PATH-CAP allot
create HBB-MAKER-KEY-HEX 80 allot
create HBB-SRC-CLOSURE-HEX 80 allot
create HBB-SRC-ENTRY-HEX 80 allot
create HBB-INPUTS-SHA SHA256-CTX-BYTES allot
create HBB-INPUTS-DIGEST 32 allot
create HBB-CHECKER-ABI-BUF HBB-ABI-CAP allot
create HBB-COMPILER-ABI-BUF HBB-ABI-CAP allot
create HBB-ARTIFACT-PATH FS-PATH-CAP allot
create HBB-STAGE-DIR-BUF FS-PATH-CAP allot
create HBB-STAGE-BUF FS-PATH-CAP allot
create HBB-ARTIFACT-NAME-BUF 128 allot
create HBB-ARTIFACT-KEY-HEX 80 allot
create HBB-AOT-INPUTS-PATH FS-PATH-CAP allot
create HBB-MAP-SRC FS-PATH-CAP allot
create HBB-MAP-DST FS-PATH-CAP allot
create HBB-PAIR-STAGE FS-PATH-CAP allot
create HBB-PAIR-BACKUP FS-PATH-CAP allot
create HBB-OBJ-KEY 64 allot
create HBB-REPL-DEPS-PATH FS-PATH-CAP allot
create HBB-MAKER-INPUTS-PATH FS-PATH-CAP allot
create HBB-REPL-SUPPORT-PATH FS-PATH-CAP allot
create HBB-REPL-SUPPORT-KEY 64 allot
create HBB-REPL-RUNNING-ROOT FS-PATH-CAP allot
create HBB-REPL-SELECTED-ROOT FS-PATH-CAP allot
create HBB-MAKER-RUNNING-ROOT FS-PATH-CAP allot
create HBB-MAKER-SELECTED-ROOT FS-PATH-CAP allot
create HBB-MAKER-SUPPORT-PATH FS-PATH-CAP allot
create HBB-MAKER-SUPPORT-KEY 64 allot
create HBB-REPL-ENGINE-DIGEST 32 allot
create HBB-REPL-ENGINE-SHA SHA256-FILE-CTX-BYTES allot
create HBB-ENGINE-SELECT-KEY 64 allot
create HBB-ENGINE-ROW-SHA SHA256-CTX-BYTES allot
create HBB-ENGINE-ROW-DIGEST 32 allot
create HBB-ENGINE-SELECT-PATH FS-PATH-CAP allot
create HBB-ENGINE-INPUT-PATH FS-PATH-CAP allot
create HBB-ENGINE-IMAGE-PATH FS-PATH-CAP allot
create HBB-LF-BUF 1 allot
HBB-LF HBB-LF-BUF c!

DYNAMIC-BUFFER HBB-OUT-CAPTURE u8
DYNAMIC-BUFFER HBB-ERR-CAPTURE u8
variable HBB-SRC-U
variable HBB-OUT-U
variable HBB-CHECKER-ABI-U
variable HBB-COMPILER-ABI-U
variable HBB-ARTIFACT-U
variable HBB-AOT-INPUTS-U
variable HBB-MAP-SRC-U
variable HBB-MAP-DST-U
variable HBB-PAIR-STAGE-U
variable HBB-PAIR-BACKUP-U
variable HBB-PAIR-HAD-MAP
variable HBB-PAIR-MOVED
variable HBB-PAIR-COMMITTED
variable HBB-PAIR-KEEP-BACKUP
variable HBB-REPL-DEPS-U
variable HBB-MAKER-INPUTS-U
variable HBB-REPL-SUPPORT-U
variable HBB-MAKER-RUNNING-ROOT-U
variable HBB-MAKER-SELECTED-ROOT-U
variable HBB-MAKER-SUPPORT-U
variable HBB-REPL-RUNNING-ROOT-U
variable HBB-REPL-SELECTED-ROOT-U
variable HBB-REPL-PRODUCER-U
variable HBB-REPL-PRODUCER-READY
variable HBB-STAGE-DIR-U
variable HBB-STAGE-U
variable HBB-ARTIFACT-NAME-U
variable HBB-ARTIFACT-RC
variable HBB-ARTIFACT-READY
variable HBB-ARTIFACT-CACHE
variable HBB-I
variable HBB-KEY-I
variable HBB-REPL
variable HBB-ENGINE
variable HBB-ENGINE-COMPAT
variable HBB-WHITEBOX
variable HBB-ENGINE-TARGET
variable HBB-JSON
variable HBB-REPORT-JSON
variable HBB-SIZE-REPORT
\ Keep the artifact maker report separate from the saved opener image cache.
variable HBB-MAKER-HIT
TYPED-VARIABLE HBB-AOT-BUILDER-A ptr u8
variable HBB-AOT-BUILDER-U
variable HBB-MAKER-BUILD
variable HBB-MAKER-RUN
variable HBB-ARTIFACT-HIT
variable HBB-OBJECT-HIT
variable HBB-OBJECT-STORE
variable HBB-LINE-START
variable HBB-JSON-FOUND
variable HBB-START-NS
variable HBB-ELAPSED-NS
variable HBB-MAKER-TIMEOUT-MS
variable HBB-ENGINE-SELECT-U
variable HBB-ENGINE-INPUT-U
variable HBB-ENGINE-IMAGE-U
variable HBB-ENGINE-SELECT-READY
variable HBB-ENGINE-SUPPORT-FRESH
TYPED-VARIABLE HBB-ENGINE-CHECK-A ptr u8
variable HBB-ENGINE-CHECK-U
DYNAMIC-BUFFER HBB-ENGINE-RECORD u8
DYNAMIC-BUFFER HBB-INPUTS u8
DYNAMIC-BUFFER HBB-REPL-SIDECAR u8
DYNAMIC-BUFFER HBB-REPL-PRODUCER u8
DYNAMIC-BUFFER HBB-MAKER-SUPPORT u8
TYPED-VARIABLE HBB-APP-BUILDER-A ptr u8
variable HBB-APP-BUILDER-U

: HBB-OUT-ROOM ( n -- ptr u8 )
   HBB-OUT-CAPTURE-RESERVE 0 HBB-OUT-CAPTURE ;

: HBB-ERR-ROOM ( n -- ptr u8 )
   HBB-ERR-CAPTURE-RESERVE 0 HBB-ERR-CAPTURE ;

: HBB-OUT-BUF ( -- ptr u8 )
   HBB-CAPTURE-CAP HBB-OUT-ROOM ;

: HBB-ERR-BUF ( -- ptr u8 )
   HBB-CAPTURE-CAP HBB-ERR-ROOM ;

: HBB-TRUE ( -- bool )
   0 0= ;

: HBB-FALSE ( -- bool )
   HBB-TRUE 0= ;

: HBB-EXIT ( n -- )
   s" " rot die ;

: HBB-USAGE ( -- )
   s" usage: tools/hb-build.f [--engine [--whitebox] [--target TARGET] | --repl | --json-errors] [options] [source.f] -o out" HBB-USAGE-RC die ;

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

: HBB-MAP-PATH! ( ptr u8 n ptr u8 -- n ) {: a:ptr u:n dst:ptr :}
   u 4 + FS-PATH-CAP > if E-BUILD-PATH throw then
   a dst u BYTE-COPY
   s" .map" drop dst u + 4 BYTE-COPY
   u 4 + ;

: HBB-MAP-SRC! ( ptr u8 n -- ) HBB-MAP-SRC HBB-MAP-PATH! HBB-MAP-SRC-U ! ;
: HBB-MAP-DST! ( ptr u8 n -- ) HBB-MAP-DST HBB-MAP-PATH! HBB-MAP-DST-U ! ;
: HBB-MAP-SRC$ ( -- ptr u8 n ) HBB-MAP-SRC HBB-MAP-SRC-U @ ;
: HBB-MAP-DST$ ( -- ptr u8 n ) HBB-MAP-DST HBB-MAP-DST-U @ ;

: HBB-MAP-FILL ( ptr u8 n -- ) HBB-MAP-SRC$ 2swap COPY-FILE-STREAM ;
: HBB-MAP-COPY ( ptr u8 n ptr u8 n -- )
   {: src:ptr srcu:n dst:ptr dstu:n :}
   src srcu HBB-MAP-SRC!
   dst dstu HBB-MAP-DST!
   HBB-MAP-SRC$ FILE? 0= if E-BUILD-PATH throw then
   HBB-MAP-DST$ [: HBB-MAP-FILL ;] REPLACE-STAGED ;

: HBB-MAP-REMOVE? ( ptr u8 n -- )
   HBB-MAP-SRC!
   HBB-MAP-SRC$ FILE? if HBB-MAP-SRC$ REMOVE-FILE then ;

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
   0 HBB-ENGINE !
   0 HBB-ENGINE-COMPAT !
   0 HBB-WHITEBOX !
   0 HBB-ENGINE-TARGET !
   0 HBB-JSON !
   0 HBB-REPORT-JSON !
   0 HBB-SIZE-REPORT ! ;

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
   HBB-I @ s" --engine" HBB-ARG= if -1 HBB-ENGINE ! HBB-INC-I HBB-TRUE exit then
   HBB-I @ s" --whitebox" HBB-ARG= if -1 HBB-WHITEBOX ! HBB-INC-I HBB-TRUE exit then
   HBB-I @ s" --target" HBB-ARG= if
      HBB-OPT-VALUE$ BUILD-TARGET:SELECT? 0= if
         s" hb-build: unknown --target profile" HBB-USAGE-RC die
      then
      -1 HBB-ENGINE-TARGET ! HBB-INC-I HBB-TRUE exit then
   HBB-I @ s" --json-errors" HBB-ARG= if -1 HBB-JSON ! HBB-INC-I HBB-TRUE exit then
   HBB-I @ s" --report-json" HBB-ARG= if -1 HBB-REPORT-JSON ! HBB-INC-I HBB-TRUE exit then
   HBB-I @ s" --size-report" HBB-ARG= if -1 HBB-SIZE-REPORT ! HBB-INC-I HBB-TRUE exit then
   HBB-I @ HBB-ARG$ s" --" STR= if HBB-INC-I HBB-FALSE exit then
   HBB-ENGINE @ if HBB-I @ s" -o" HBB-ARG= if HBB-FALSE exit then then
   HBB-I @ HBB-ARG$ s" -" STARTS-WITH? if HBB-USAGE then
   HBB-FALSE ;

: HBB-PARSE-OPTIONS ( -- )
   begin HBB-PARSE-OPTION? while repeat ;

: HBB-PARSE ( -- )
   HBB-RESET-OPTIONS
   BUILD-TARGET:HOST!
   0 HBB-I !
   HBB-PARSE-OPTIONS
   HBB-ENGINE @ if
      HBB-REPL @ 0<> HBB-JSON @ 0<> or if HBB-USAGE then
      SCRIPT-ARGC HBB-I @ - 2 <> if HBB-USAGE then
      HBB-I @ s" -o" HBB-ARG= 0= if HBB-USAGE then
      HBB-I @ 1+ HBB-ARG$ HBB-OUT-ARG!
      s" src/habu/habu2.f" HBB-SRC!
      exit
   then
   HBB-WHITEBOX @ 0<> HBB-ENGINE-TARGET @ 0<> or if HBB-USAGE then
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
   HBB-ENGINE-RECORD-RELEASE
   HBB-OUT-CAPTURE-RELEASE
   HBB-ERR-CAPTURE-RELEASE
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
   HBB-ENGINE @ if s" hb-engine-got" exit then
   HBB-REPL @ if s" hb-build-got" else s" hb-aot-got" then ;

: HBB-MAKER-INPUTS$ ( -- ptr u8 n )
   HBB-GOT-NAME$ BF-A$ {: a:ptr u:n :}
   s" .inputs" {: suffix:ptr suffixu:n :}
   u suffixu + FS-PATH-CAP > if E-BUILD-PATH throw then
   a HBB-MAKER-INPUTS-PATH u BYTE-COPY
   suffix HBB-MAKER-INPUTS-PATH u + suffixu BYTE-COPY
   u suffixu + HBB-MAKER-INPUTS-U !
   HBB-MAKER-INPUTS-PATH HBB-MAKER-INPUTS-U @ ;

: HBB-INPUTS-READ ( ptr u8 n -- ptr u8 n )
   {: path:ptr pathu:n :}
   path pathu FILE-SIZE {: size:n :}
   size 0 <= if E-NUNIT-PROFILE throw then
   size HBB-INPUTS-RESERVE
   path pathu 0 HBB-INPUTS size READ-ALL size <> if E-FS-IO throw then
   0 HBB-INPUTS size ;

: HBB-OBJ-NAME$ ( -- ptr u8 n )
   s" hb-aot-obj" ;

: HBB-ARTIFACT-NAME$ ( -- ptr u8 n )
   HBB-ARTIFACT-NAME-BUF HBB-ARTIFACT-NAME-U @ ;

: HBB-KEY-FILE+ ( CONTENT-KEY:fold ptr u8 n -- CONTENT-KEY:fold )
   SOURCE-ROOT:CANON-OS 0= if SHA-E-OPEN throw then
   CONTENT-KEY:FILE+ ;

\ Discover the producer's loaded source closure instead of maintaining a
\ second dependency list. The target selector is part of the key; discovery
\ includes every statically reachable target branch.
: HBB-MAKER-CLOSURE+ ( CONTENT-KEY:fold ptr u8 n -- CONTENT-KEY:fold )
   {: a:ptr u:n :}
   a u SOURCE-ROOT:RESOLVE drop SOURCE-ROOT:RESOLVED-ROOT$
   EC:BUILD-IN
   0 HBB-KEY-I !
   begin HBB-KEY-I @ EC:COUNT < while
      HBB-KEY-I @ EC:PATH$ {: path:ptr pathu:n :}
      path pathu s" " NFILE-INPUTS:ADD-CONTENT
      path pathu CONTENT-KEY:FILE+
      HBB-KEY-I @ 1+ HBB-KEY-I !
   repeat ;

: HBB-KEY-TARGET+ ( CONTENT-KEY:fold -- CONTENT-KEY:fold )
   HB-TARGET-LINUX? if s" target:linux-aarch64" CONTENT-KEY:TEXT+ exit then
   HB-TARGET-MACOS? if s" target:macos-aarch64" CONTENT-KEY:TEXT+ exit then
   HB-TARGET-LINUX-X86-64? if s" target:linux-x86-64" CONTENT-KEY:TEXT+ exit then
   s" hb-build: unknown target" HBB-BUILD-RC die ;

\ The selected engine's sidecar supplies baked source attribution. Its
\ absence is an input too: restoring the matching map must select a fresh
\ artifact instead of replaying unknown-origin rows from a no-map build.
: HBB-KEY-DONOR-MAP+ ( CONTENT-KEY:fold -- CONTENT-KEY:fold )
   HBB-REPL @ if exit then
   BF-ENGINE$ HBB-MAP-SRC!
   HBB-MAP-SRC$ FILE? if
      s" donor-map:present" CONTENT-KEY:TEXT+
      HBB-MAP-SRC$ HBB-KEY-FILE+
   else
      s" donor-map:missing" CONTENT-KEY:TEXT+
   then ;

: HBB-MAKER-KEY-COLD ( -- )
   CONTENT-KEY:OPEN
   s" hb-build-native-producer-v3" CONTENT-KEY:TEXT+
   BF-ENGINE$ HBB-KEY-FILE+
   HBB-KEY-DONOR-MAP+
   HBB-KEY-TARGET+
   s" tools/hb-build.f" HBB-MAKER-CLOSURE+
   s" tools/aot-builder.f" HBB-MAKER-CLOSURE+
   s" tools/aot-build.f" HBB-MAKER-CLOSURE+
   HBB-MAKER-KEY-HEX CONTENT-KEY:FINAL-HEX ;

: HBB-MAKER-CANON ( ptr u8 n -- ptr u8 n bool )
   {: path:ptr pathu:n :}
   path pathu SOURCE-ROOT:CANON-OS {: answer:ptr answeru:n exists:bool :}
   path pathu answer answeru exists NFILE-INPUTS:ADD-CANON-ONCE
   answer answeru exists ;

: HBB-MAKER-SUPPORT-PATH$ ( -- ptr u8 n )
   HBB-MAKER-SUPPORT-PATH HBB-MAKER-SUPPORT-U @ ;

\ The selector depends only on fixed support identity. The source and path
\ queries are validated in the record it selects.
: HBB-MAKER-SUPPORT-SELECT ( -- )
   CONTENT-KEY:OPEN
   s" hb-maker-support-v1" CONTENT-KEY:TEXT+
   BF-ENGINE$ HBB-KEY-FILE+
   HBB-KEY-DONOR-MAP+
   HBB-KEY-TARGET+
   SOURCE-ROOT:CWD$ CONTENT-KEY:TEXT+
   HBB-MAKER-SELECTED-ROOT HBB-MAKER-SELECTED-ROOT-U @ CONTENT-KEY:TEXT+
   HBB-MAKER-SUPPORT-KEY CONTENT-KEY:FINAL-HEX
   HBB-MAKER-SUPPORT-KEY s" maker-support"
      HBB-MAKER-SUPPORT-PATH HBB-MAKER-SUPPORT-U KEYED-IMAGE:PATH! ;

: HBB-MAKER-SUPPORT-CHECK ( -- )
   HBB-MAKER-SUPPORT-PATH$ FILE-SIZE {: size:n :}
   size HBB-MAKER-SUPPORT-HEADER-U <= if E-NUNIT-PROFILE throw then
   size HBB-MAKER-SUPPORT-RESERVE
   HBB-MAKER-SUPPORT-PATH$ 0 HBB-MAKER-SUPPORT size READ-ALL size <>
      if E-NUNIT-PROFILE throw then
   0 HBB-MAKER-SUPPORT {: rec:ptr :}
   rec 8 s" HBMSEL1!" STR= 0= if E-NUNIT-PROFILE throw then
   rec 72 + LE:U64@ size HBB-MAKER-SUPPORT-HEADER-U - <>
      if E-NUNIT-PROFILE throw then
   rec HBB-MAKER-SUPPORT-HEADER-U +
      size HBB-MAKER-SUPPORT-HEADER-U - NFILE-INPUTS:CHECK
   rec 8 + HBB-MAKER-KEY-HEX 64 BYTE-COPY ;

: HBB-MAKER-SUPPORT-FRESH? ( -- bool )
   HBB-MAKER-SUPPORT-PATH$ FILE? 0= if HBB-FALSE exit then
   [: HBB-MAKER-SUPPORT-CHECK ;] catch {: rc:n :}
   HBB-MAKER-SUPPORT-RELEASE
   rc 0= if
      HBB-MAKER-SUPPORT-PATH$ BUILD-CACHE:USED if HBB-TRUE exit then
      HBB-FALSE exit
   then
   rc E-NUNIT-PROFILE = rc E-NUNIT-FILE = or if HBB-FALSE exit then
   rc throw ;

: HBB-MAKER-SUPPORT-WRITE ( -- )
   NFILE-INPUTS:BYTES$ {: rows:ptr rowsu:n :}
   HBB-MAKER-SUPPORT-HEADER-U rowsu + HBB-MAKER-SUPPORT-RESERVE
   0 HBB-MAKER-SUPPORT {: rec:ptr :}
   s" HBMSEL1!" drop rec 8 BYTE-COPY
   HBB-MAKER-KEY-HEX rec 8 + 64 BYTE-COPY
   rowsu rec 72 + LE:U64!
   rows rec HBB-MAKER-SUPPORT-HEADER-U + rowsu BYTE-COPY
   HBB-MAKER-SUPPORT-PATH$ rec HBB-MAKER-SUPPORT-HEADER-U rowsu +
      ATOMIC-WRITE-FILE
   s" hb-maker-support-" s" " s" " HBB-MAKER-SUPPORT-PATH$ BUILD-CACHE:PRUNE ;

: HBB-MAKER-KEY-IN-ROOT ( -- )
   HBB-MAKER-SUPPORT-SELECT
   HBB-MAKER-SUPPORT-FRESH? if exit then
   NFILE-INPUTS:MARK {: end:n :}
   end NFILE-INPUTS:BYTES$ nip - {: first:n :}
   end NFILE-INPUTS:FROM
   [: [: HBB-MAKER-CANON ;] [: SOURCE-ROOT:READ-OS ;]
      [: HBB-MAKER-KEY-COLD HBB-MAKER-SUPPORT-WRITE ;]
      SOURCE-INPUT:WITH ;] catch {: rc:n :}
   end NFILE-INPUTS:TRUNCATE
   first NFILE-INPUTS:FROM
   rc 0<> if rc throw then ;

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
   s" HABU_AOT_INPUTS" >LEN HBB-MAKER-INPUTS$ >LEN PROC-ENV-SET
   s" --" >LEN PROC-ARGV+
   HBB-SRC$ >LEN PROC-ARGV+
   HBB-JSON-FLAG$ >LEN PROC-ARGV+
   s" --donor" >LEN PROC-ARGV+
   BF-ENGINE$ >LEN PROC-ARGV+ ;

: HBB-FINISH-MAKER-RC ( n n n -- ) {: outu erru rc :}
   rc 0= if
      HBB-ENGINE @ if
         HBB-REPORT-JSON @ 0= if HBB-OUT-BUF outu type then
         erru HBB-WERR-ERR
      then
      exit
   then
   outu HBB-WOUT-ERR
   HBB-JSON @ if erru HBB-WERR-JSON-ONLY else erru HBB-WERR-ERR then
   rc HBB-EXIT ;

: HBB-MAKER-KIND$ ( -- ptr u8 n )
   HBB-ENGINE @ if s" engine" exit then
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

: HBB-AOT-BUILDER-TIMED-OUT ( -- )
   KEYED-IMAGE:CAPTURE-OUT$ {: out:ptr outu:n :}
   KEYED-IMAGE:CAPTURE-ERR$ {: err:ptr erru:n :}
   out HBB-OUT-BUF outu BYTE-COPY
   err HBB-ERR-BUF erru BYTE-COPY
   outu erru HBB-MAKER-TIMED-OUT ;

: HBB-SELECT-MAKER ( -- )
   HBB-MAKER-KEY-HEX HBB-MAKER-TIMEOUT-MS @ AOT-BUILDER-IMAGE:SELECT-PRODUCER$
   {: image:ptr imageu:n hit:bool :}
   image HBB-AOT-BUILDER-A ! imageu HBB-AOT-BUILDER-U !
   hit HBB-MAKER-HIT !
   hit 0= HBB-MAKER-BUILD ! ;

\ The saved AOT builder a build on BF-ENGINE$ runs, settled: the image
\ HBB-RUN-MAKER-CMD selects by its producer key, so a row that settles it
\ once (test/aot-builder-engine.f, test/linker-builder-engine.f) serves every
\ build on that engine in the same cache root. The caller selects BF-ENGINE$.
: HBB-MAKER-IMAGE$ ( n -- ptr u8 n )
   {: timeout:n :}
   HBB-RESET-OPTIONS
   timeout HBB-MAKER-TIMEOUT-MS !
   HBB-MAKER-KEY!
   HBB-SELECT-MAKER
   HBB-AOT-BUILDER-A @ HBB-AOT-BUILDER-U @ ;

\ Select the saved maker before HBB-RUN-MAKER-ARGS stages argv and env: image
\ publication has its own child and staging buffer. RUN loads the linker after
\ OPEN latched the application span.
: HBB-RUN-MAKER-CMD ( -- len len outcome )
   KEYED-IMAGE:CAPTURE-RESET
   ['] HBB-SELECT-MAKER catch {: rc:n :}
   rc E-PROC-TIMEOUT = if HBB-AOT-BUILDER-TIMED-OUT then
   rc 0<> if rc throw then
   HBB-RUN-MAKER-ARGS
   HBB-AOT-BUILDER-A @ HBB-AOT-BUILDER-U @ >LEN
   S\" 1 set-tier\nAOT-BUILDER:OPEN\nAOT-BUILDER:RUN\n" >LEN
   [: HBB-OUT-ROOM ;]
   [: HBB-ERR-ROOM ;]
   HBB-MAKER-TIMEOUT-MS @ >MS RUN-ARGV-ENV-STDIN-GROWING-BOTH-CAPTURE-OUTCOME ;

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

\ The executable's REPLACE-STAGED fill publishes the already staged map only
\ after its executable copy and chmod have completed. The outer rename is the
\ last step; a refusal there puts the prior map back from its sibling.
defer HBB-PAIR-EXE-FILL ( ptr u8 n -- )

: HBB-PAIR-STAGE$ ( -- ptr u8 n )
   HBB-PAIR-STAGE HBB-PAIR-STAGE-U @ ;

: HBB-PAIR-BACKUP$ ( -- ptr u8 n )
   HBB-PAIR-BACKUP HBB-PAIR-BACKUP-U @ ;

: HBB-PAIR-RESERVE ( ptr u8 ptr n -- )
   {: dst:ptr up:ptr :}
   HBB-MAP-DST$ RESERVE-SIBLING {: a:ptr u:n fd:n :}
   a u dst up HBB-COPY-PATH!
   fd FS-ATOMIC:CLOSE-FD 0<> if E-FS-IO throw then
   dst up @ CLEANUP+ ;

: HBB-PAIR-STAGE-RESERVE ( -- )
   HBB-PAIR-STAGE HBB-PAIR-STAGE-U HBB-PAIR-RESERVE ;

: HBB-PAIR-BACKUP-RESERVE ( -- )
   HBB-PAIR-BACKUP HBB-PAIR-BACKUP-U HBB-PAIR-RESERVE ;

: HBB-PAIR-RESERVE-CODE ( n -- )
   {: code:n :}
   code 0= if exit then
   code E-FS-OPEN = code E-FS-IO = or if E-BUILD-INSTALL throw then
   code throw ;

: HBB-PAIR-STAGE-MAP ( -- )
   HBB-MAP-SRC$ FILE? 0= if E-BUILD-PATH throw then
   [: HBB-PAIR-STAGE-RESERVE ;] catch HBB-PAIR-RESERVE-CODE
   HBB-MAP-SRC$ HBB-PAIR-STAGE$ COPY-FILE-STREAM
   HBB-MAP-DST$ FS-TRY-LSTAT if
      [: HBB-PAIR-BACKUP-RESERVE ;] catch HBB-PAIR-RESERVE-CODE
      -1 HBB-PAIR-HAD-MAP !
   then ;

: HBB-PAIR-FILL ( ptr u8 n -- )
   2dup HBB-PAIR-EXE-FILL
   2drop
   HBB-PAIR-HAD-MAP @ if
      HBB-MAP-DST$ HBB-PAIR-BACKUP$ RENAME-FILE
      -1 HBB-PAIR-MOVED !
   then
   HBB-PAIR-STAGE$ HBB-MAP-DST$ RENAME-FILE
   -1 HBB-PAIR-COMMITTED ! ;

: HBB-PAIR-ROLLBACK ( -- )
   HBB-PAIR-MOVED @ if
      HBB-PAIR-BACKUP$ HBB-MAP-DST$ RENAME-FILE
   else
      HBB-PAIR-COMMITTED @ if HBB-MAP-DST$ REMOVE-FILE then
   then ;

: HBB-PAIR-CLEAN-ONE ( ptr u8 n -- )
   {: a:ptr u:n :}
   u 0= if exit then
   a u FS-MUT-ATOMIC-CLEAN-TEMP
   a u CLEANUP-FORGET ;

: HBB-PAIR-CLEAN-BACKUP ( -- )
   HBB-PAIR-KEEP-BACKUP @ if
      HBB-PAIR-BACKUP$ CLEANUP-FORGET
   else
      HBB-PAIR-BACKUP$ HBB-PAIR-CLEAN-ONE
   then ;

: HBB-PAIR-CLEAN ( -- n n )
   [: HBB-PAIR-CLEAN-BACKUP ;] catch
   [: HBB-PAIR-STAGE$ HBB-PAIR-CLEAN-ONE ;] catch ;

: HBB-PAIR-REPORT ( ptr u8 n n ptr u8 n -- )
   {: label:ptr labelu:n code:n path:ptr pathu:n :}
   SB-RESET
   s" hb-build: " SB-APPEND
   label labelu SB-APPEND
   s"  (code " SB-APPEND
   code FMT:SB-INT
   s" ) at " SB-APPEND
   \ A report must not replace the failure it describes, including when stderr
   \ is closed. The path can exceed the string builder's 1 KiB capacity.
   2 SB$ write drop
   2 path pathu write drop
   2 HBB-LF-BUF 1 write drop ;

: HBB-PAIR-REPORT-ROLLBACK ( n -- )
   {: code:n :}
   HBB-PAIR-MOVED @ if
      s" map rollback failed; prior map retained" code HBB-PAIR-BACKUP$ HBB-PAIR-REPORT
   else
      s" map rollback failed; new map retained" code HBB-MAP-DST$ HBB-PAIR-REPORT
   then ;

: HBB-PAIR-REPORT-CLEAN ( n n -- )
   {: backup:n stage:n :}
   backup 0<> if
      s" cannot clean map backup" backup HBB-PAIR-BACKUP$ HBB-PAIR-REPORT
   then
   stage 0<> if
      s" cannot clean staged map" stage HBB-PAIR-STAGE$ HBB-PAIR-REPORT
   then ;

: HBB-PAIR-RUN ( [ ptr u8 n -- ] -- )
   is HBB-PAIR-EXE-FILL
   0 HBB-PAIR-STAGE-U ! 0 HBB-PAIR-BACKUP-U !
   0 HBB-PAIR-HAD-MAP ! 0 HBB-PAIR-MOVED ! 0 HBB-PAIR-COMMITTED !
   0 HBB-PAIR-KEEP-BACKUP !
   [: HBB-PAIR-STAGE-MAP [: HBB-PAIR-FILL ;] HBB-REPLACE-OUT ;] catch {: code:n :}
   code 0<> if
      [: HBB-PAIR-ROLLBACK ;] catch {: recover:n :}
      recover 0<> HBB-PAIR-MOVED @ 0<> and HBB-PAIR-KEEP-BACKUP !
      HBB-PAIR-CLEAN {: backup:n stage:n :}
      recover 0<> if recover HBB-PAIR-REPORT-ROLLBACK then
      backup stage HBB-PAIR-REPORT-CLEAN
      code throw
   then
   HBB-PAIR-CLEAN {: backup:n stage:n :}
   backup stage HBB-PAIR-REPORT-CLEAN
   backup 0<> if backup throw then
   stage 0<> if stage throw then ;

\ The engine the child wrote under HB_TMP may be on another filesystem; it is
\ copied.
: HBB-INSTALL-FILL ( ptr u8 n -- )
   {: tmp:ptr tmpu:n :}
   HBB-GOT-NAME$ BF-A$ tmp tmpu COPY-FILE-STREAM
   tmp tmpu CHMOD-X ;

: HBB-INSTALL-OUT ( -- )
   HBB-GOT-NAME$ BF-EXPECT
   HBB-REPL @ if
      [: HBB-INSTALL-FILL ;] HBB-REPLACE-OUT
      HBB-OUT$ HBB-MAP-REMOVE?
   else
      HBB-GOT-NAME$ BF-A$ HBB-MAP-SRC!
      HBB-OUT$ HBB-MAP-DST!
      [: HBB-INSTALL-FILL ;] HBB-PAIR-RUN
   then
   HBB-GOT-NAME$ BF-A$ HBB-MAP-REMOVE?
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
   HBB-ENGINE @ if s" hb-build-engine" exit then
   s" hb-build-out" ;

: HBB-ARTIFACT-PREFIX$ ( -- ptr u8 n )
   HBB-ENGINE @ if s" hb-build-engine-" exit then
   s" hb-build-out-" ;

: HBB-OPTION-TEXT+ ( CONTENT-KEY:fold ptr u8 n bool -- CONTENT-KEY:fold )
   if CONTENT-KEY:TEXT+ else 2drop then ;

\ The canonical entry selects a prior observed load. Its actual inputs supply
\ the dependency key: static replay cannot know the root a loader scopes at
\ runtime, or whether a body containing a literal import runs at all.
: HBB-ENTRY-HEX! ( ptr u8 n ptr u8 -- )
   {: a:ptr u:n dst:ptr :}
   a u SOURCE-ROOT:ENTRY-RESOLVE drop {: path:ptr pathu:n :}
   CONTENT-KEY:OPEN
   s" user-source-entry-v1" CONTENT-KEY:TEXT+
   path pathu CONTENT-KEY:FILE+
   dst CONTENT-KEY:FINAL-HEX ;


: HBB-SRC-CLOSURE-HEX! ( -- )
   HBB-SRC$ HBB-SRC-ENTRY-HEX HBB-ENTRY-HEX!
   HBB-SRC-ENTRY-HEX HBB-SRC-CLOSURE-HEX 64 BYTE-COPY ;


: HBB-AOT-INPUTS$ ( -- ptr u8 n )
   HBB-AOT-INPUTS-PATH HBB-AOT-INPUTS-U @ ;

\ This index only selects a previous observed load. Its rows must still match
\ this invocation before they can select either an artifact or an object.
\ The rows join both result keys, so replacing the index cannot pair one load's
\ inputs with another load's cached image.
: HBB-AOT-INPUTS-PATH! ( -- )
   CONTENT-KEY:OPEN
   s" hb-build-actual-inputs-v1" CONTENT-KEY:TEXT+
   HBB-SRC-ENTRY-HEX 64 CONTENT-KEY:TEXT+
   HBB-MAKER-KEY-HEX 64 CONTENT-KEY:TEXT+
   HBB-SRC$ CONTENT-KEY:TEXT+
   BF-ENGINE$ CONTENT-KEY:TEXT+
   SOURCE-ROOT:CWD$ CONTENT-KEY:TEXT+
   HBB-MAKER-SELECTED-ROOT HBB-MAKER-SELECTED-ROOT-U @ CONTENT-KEY:TEXT+
   HBB-ARTIFACT-KEY-HEX CONTENT-KEY:FINAL-HEX
   HBB-ARTIFACT-KEY-HEX s" build-inputs"
      HBB-AOT-INPUTS-PATH HBB-AOT-INPUTS-U KEYED-IMAGE:PATH! ;

: HBB-AOT-INPUTS-KEY! ( ptr u8 n -- )
   {: rows:ptr rowsu:n :}
   HBB-INPUTS-SHA rows rowsu HBB-INPUTS-DIGEST SHA256-IN
   CONTENT-KEY:OPEN
   s" user-source-inputs-v1" CONTENT-KEY:TEXT+
   HBB-SRC-ENTRY-HEX 64 CONTENT-KEY:TEXT+
   HBB-INPUTS-DIGEST CONTENT-KEY:DIGEST+
   HBB-SRC-CLOSURE-HEX CONTENT-KEY:FINAL-HEX ;

: HBB-AOT-INPUTS-CHECK ( -- )
   HBB-AOT-INPUTS$ HBB-INPUTS-READ
   2dup NFILE-INPUTS:CHECK HBB-AOT-INPUTS-KEY! ;

: HBB-AOT-INPUTS-FRESH? ( -- bool )
   HBB-AOT-INPUTS$ BUILD-CACHE:USED 0= if HBB-FALSE exit then
   [: HBB-AOT-INPUTS-CHECK ;] catch {: rc:n :}
   HBB-INPUTS-RELEASE
   rc 0= if HBB-TRUE exit then
   rc E-NUNIT-PROFILE = rc E-NUNIT-FILE = or if HBB-FALSE exit then
   HBB-AOT-INPUTS$ FS-TRY-LSTAT 0= if HBB-FALSE exit then
   rc throw ;

\ HBB-PREPARE-ARTIFACT-CACHE selects the producer before the maker runs.
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
   HB-TARGET-LINUX-X86-64? if
      s" hb-x86-64-v1" HBB-COMPILER-ABI-BUF HBB-COMPILER-ABI-U HBB-ABI!
   else
      s" hb-arm64-v1" HBB-COMPILER-ABI-BUF HBB-COMPILER-ABI-U HBB-ABI!
   then
   HBB-COMPILER-ABI-BUF HBB-COMPILER-ABI-U @ ;

: HBB-TARGET-ABI$ ( -- ptr u8 n )
   HB-TARGET-LINUX? if s" linux-aarch64" exit then
   HB-TARGET-MACOS? if s" macos-aarch64" exit then
   HB-TARGET-LINUX-X86-64? if s" linux-x86-64" exit then
   HBB-TARGET-UNKNOWN ;

: HBB-ARTIFACT-KEY! ( -- )
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
   HBB-MAKER-KEY!
   HBB-AOT-INPUTS-PATH!
   HBB-AOT-INPUTS-FRESH? HBB-ARTIFACT-CACHE !
   HBB-ARTIFACT-PATHS ;

: HBB-RESTORE-FILL ( ptr u8 n -- )
   {: tmp:ptr tmpu:n :}
   HBB-ARTIFACT$ tmp tmpu COPY-FILE-STREAM
   tmp tmpu CHMOD-X ;

: HBB-RESTORE-OUT ( -- )
   [: HBB-RESTORE-FILL ;] HBB-PAIR-RUN ;

: HBB-RESTORE-PAIR ( -- )
   HBB-ARTIFACT$ HBB-MAP-SRC!
   HBB-OUT$ HBB-MAP-DST!
   HBB-RESTORE-OUT ;

\ A stopped publisher can leave both cache paths present but from different
\ attempts. The map's image digest is the required pair identity.
: HBB-ARTIFACT-READY-CHECK ( -- bool )
   HBB-ARTIFACT$ EXECUTABLE? 0= if HBB-FALSE exit then
   HBB-ARTIFACT$ HBB-MAP-SRC!
   HBB-MAP-SRC$ FILE? 0= if HBB-FALSE exit then
   HBB-ARTIFACT$ IMAGE-NAMES:LOAD-BOUND
   IMAGE-NAMES:BOUND? ;

: HBB-ARTIFACT-FILES? ( -- bool )
   HBB-ARTIFACT$ FS-TRY-LSTAT
   HBB-ARTIFACT$ HBB-MAP-SRC!
   HBB-MAP-SRC$ FS-TRY-LSTAT and ;

: HBB-ARTIFACT-READY-RUN ( -- )
   HBB-ARTIFACT-READY-CHECK HBB-ARTIFACT-READY ! ;

: HBB-ARTIFACT-READY? ( -- bool )
   [: HBB-ARTIFACT-READY-RUN ;] catch {: code:n :}
   code 0<> if
      code E-FS-OPEN <> code E-FS-IO <> and
      code E-FS-STAT <> and
      code SHA-E-OPEN <> and code SHA-E-READ <> and if code throw then
      HBB-ARTIFACT-FILES? if code throw then
      HBB-FALSE exit
   then
   HBB-ARTIFACT-READY @ 0<> ;

\ An artifact found in the cache is dated as used (src/lib/build-cache.f), and one a
\ prune took after the check is built again. So is one a prune takes after USED
\ dated it: a copy that fails while the artifact is gone is a miss, and -o is
\ as it was. The copy opens the artifact before it writes a byte, and once open
\ it reads the whole artifact whatever happens to its name.
: HBB-RESTORE-ARTIFACT? ( -- bool )
   HBB-ARTIFACT-CACHE @ 0= if HBB-FALSE exit then
   HBB-ARTIFACT-READY? 0= if HBB-FALSE exit then
   HBB-ARTIFACT$ BUILD-CACHE:USED 0= if HBB-FALSE exit then
   HBB-MAP-SRC$ BUILD-CACHE:USED 0= if HBB-FALSE exit then
   [: HBB-RESTORE-PAIR ;] catch {: code:n :}
   code 0<> if
      code E-BUILD-PATH <> code E-FS-OPEN <> and
      code E-FS-IO <> and code E-FS-STAT <> and if code throw then
      HBB-ARTIFACT-FILES? if code throw then
      HBB-FALSE exit
   then
   -1 HBB-ARTIFACT-HIT !
   HBB-TRUE ;

: HBB-OBJECT-LOAD? ( -- bool )
   BUILD-CACHE:ROOT$ OBJRES:ROOT!
   HBB-SRC-CLOSURE-HEX 64 HBB-TARGET-ABI$ HBB-CHECKER-ABI$ HBB-COMPILER-ABI$ OBJRES:LOAD ;

: HBB-OBJ$ ( -- ptr u8 n )
   HBB-OBJ-NAME$ BF-A$ ;

: HBB-READ-OBJECT ( -- n n )
   HBB-OBJ$ BF-READ-SOURCE
   BF-SOURCE-LEN @ 8 < if E-OBJ-SCHEMA throw then
   BF-SOURCE-BUF LE:U32@ {: code:n :}
   BF-SOURCE-BUF 4 + LE:U32@ {: data:n :}
   code 0 <= code BF-SOURCE-LEN @ 8 - > or if E-OBJ-SCHEMA throw then
   data BF-SOURCE-LEN @ 8 - code - <> if E-OBJ-SCHEMA throw then
   code data ;

: HBB-BUILD-OBJECT-RECORD ( n n -- ) {: code:n data:n :}
   OBJ:RESET
   HBB-SRC-CLOSURE-HEX 64 OBJ:SOURCE!
   HBB-TARGET-ABI$ OBJ:TARGET!
   HBB-CHECKER-ABI$ OBJ:CHECKER!
   HBB-COMPILER-ABI$ OBJ:COMPILER!
   BF-SOURCE-BUF 8 + code OBJ:TEXT+
   data 0 > if BF-SOURCE-BUF 8 + code + data OBJ:DATA+ then
   s" MAIN" s" --" OBJ:EXPORT+
   s" MAIN" 0 s" --" OBJ:DEF+ ;

: HBB-AOT-SAVE-INPUTS-RUN ( -- )
   HBB-MAKER-INPUTS$ HBB-INPUTS-READ {: rows:ptr rowsu:n :}
   rows rowsu NFILE-INPUTS:FORMAT
   rows rowsu HBB-AOT-INPUTS-KEY!
   HBB-AOT-INPUTS$ rows rowsu ATOMIC-WRITE-FILE ;

: HBB-STORE-OBJECT ( -- )
   ['] HBB-AOT-SAVE-INPUTS-RUN ['] HBB-INPUTS-RELEASE finally
   s" hb-build-inputs-" s" " s" " HBB-AOT-INPUTS$ BUILD-CACHE:PRUNE
   HBB-ARTIFACT-PATHS
   -1 HBB-ARTIFACT-CACHE !
   BUILD-CACHE:ROOT$ OBJRES:ROOT!
   HBB-READ-OBJECT HBB-BUILD-OBJECT-RECORD
   OBJRES:STORE OBJSTORE:PATH$ {: obj:ptr obju:n :}
   HBB-GOT-NAME$ BF-A$ obj obju HBB-MAP-COPY
   s" " s" .hbo.map" s" " HBB-MAP-DST$ BUILD-CACHE:PRUNE
   -1 HBB-OBJECT-STORE ! ;

: HBB-OBJECT-PATH$ ( -- ptr u8 n )
   HBB-OBJ-KEY OBJ:KEY-HEX
   HBB-OBJ-KEY 64 OBJSTORE:PATH$ ;

: HBB-STAGE-OPEN ( -- )
   BUILD-CACHE:ROOT$ HBB-ARTIFACT-STEM$ MAKE-TEMP-DIR {: a:ptr u:n :}
   a HBB-STAGE-DIR-BUF u BYTE-COPY
   u HBB-STAGE-DIR-U ! ;

: HBB-OBJECT-FILL ( ptr u8 n -- )
   {: tmp:ptr tmpu:n :}
   HBB-STAGE$ tmp tmpu COPY-FILE-STREAM
   tmp tmpu CHMOD-X ;

\ The cached sidecar must name the bytes this relink actually produced. An
\ unbound map misses the object before either output path is published.
: HBB-OBJECT-STAGE ( -- )
   OBJIMG:LOAD
   HBB-STAGE-DIR$ s" object" HBB-STAGE-BUF JOIN-PATH HBB-STAGE-U !
   HBB-STAGE$ OBJIMG:WRITE
   HBB-OBJECT-PATH$ HBB-STAGE$ HBB-MAP-COPY
   HBB-STAGE$ IMAGE-NAMES:LOAD-BOUND
   IMAGE-NAMES:BOUND? 0= if exit then
   HBB-STAGE$ HBB-MAP-SRC!
   HBB-OUT$ HBB-MAP-DST!
   [: HBB-OBJECT-FILL ;] HBB-PAIR-RUN
   -1 HBB-OBJECT-HIT ! ;

: HBB-WRITE-OBJECT? ( -- bool )
   HBB-STAGE-OPEN
   [: HBB-OBJECT-STAGE ;] catch {: code:n :}
   HBB-STAGE-DIR$ REMOVE-TREE
   code 0<> if
      code E-BUILD-PATH <> code E-FS-OPEN <> and
      code E-FS-IO <> and code E-FS-STAT <> and if code throw then
      HBB-OBJECT-PATH$ FS-TRY-LSTAT
      HBB-OBJECT-PATH$ HBB-MAP-SRC!
      HBB-MAP-SRC$ FS-TRY-LSTAT and if code throw then
      HBB-FALSE exit
   then
   HBB-OBJECT-HIT @ 0<> ;

: HBB-OBJECT-HIT? ( -- bool )
   HBB-ARTIFACT-CACHE @ 0= if HBB-FALSE exit then
   HBB-OBJECT-LOAD? 0= if HBB-FALSE exit then
   HBB-OBJECT-PATH$ HBB-MAP-SRC!
   HBB-MAP-SRC$ FILE? 0= if HBB-FALSE exit then
   HBB-MAP-SRC$ BUILD-CACHE:USED 0= if HBB-FALSE exit then
   HBB-WRITE-OBJECT? ;

\ The artifact is copied into a work directory of its own in the cache and
\ published by rename, so a half-written artifact is never visible under its
\ key, and two builds installing one key each publish whole bytes. The work
\ directory goes whatever the copy did; one a killed build leaves is a stale
\ entry of the family a day later.
: HBB-STAGE-ARTIFACT ( -- )
   HBB-STAGE-DIR$ HBB-ARTIFACT-STEM$ HBB-STAGE-BUF JOIN-PATH HBB-STAGE-U !
   HBB-OUT$ HBB-STAGE$ COPY-FILE-STREAM
   HBB-OUT$ HBB-STAGE$ HBB-MAP-COPY
   HBB-STAGE$ CHMOD-X
   HBB-STAGE$ HBB-MAP-SRC!
   HBB-ARTIFACT$ HBB-MAP-DST!
   HBB-MAP-SRC$ HBB-MAP-DST$ RENAME-FILE
   HBB-STAGE$ HBB-ARTIFACT$ RENAME-FILE ;

\ A published artifact prunes its family, which reports its own failures and
\ never fails the build.
: HBB-INSTALL-ARTIFACT ( -- )
   HBB-ARTIFACT-CACHE @ 0= if exit then
   HBB-ARTIFACT-READY? if exit then
   HBB-STAGE-OPEN
   [: HBB-STAGE-ARTIFACT ;] catch HBB-ARTIFACT-RC !
   HBB-STAGE-DIR$ REMOVE-TREE
   HBB-ARTIFACT-RC @ 0 <> if HBB-ARTIFACT-RC @ throw then
   HBB-ARTIFACT-PREFIX$ s" " HBB-ARTIFACT-STEM$ HBB-ARTIFACT$ BUILD-CACHE:PRUNE
   HBB-ARTIFACT$ HBB-MAP-DST!
   HBB-ARTIFACT-PREFIX$ s" .map" HBB-ARTIFACT-STEM$ HBB-MAP-DST$ BUILD-CACHE:PRUNE ;

\ A child that opens the definition cache stores its hb-def- entries in this
\ root and prunes nothing: its dictionary is saved as a product image, so
\ retention belongs here, in the parent, which is never saved. keep is an
\ entry this build published or used in the root, which names the directory.
: HBB-PRUNE-ENTRIES ( ptr u8 n -- ) {: keep:ptr keepu:n :}
   s" hb-def-" s" .hbd" s" " keep keepu BUILD-CACHE:PRUNE ;

: HBB-RUN-MAKER ( -- )
   -1 HBB-MAKER-RUN !
   HBB-GOT-NAME$ BF-REMOVE-TMP
   HBB-GOT-NAME$ BF-A$ HBB-MAP-REMOVE?
   HBB-OBJ-NAME$ BF-REMOVE-TMP
   HBB-RUN-MAKER-CMD HBB-FINISH-MAKER ;

: HBB-SUCCESS ( -- )
   HBB-ENGINE-COMPAT @ if s" native-build OK: " else s" hb-build OK: " then type
   HBB-OUT$ type
   HBB-ENGINE-COMPAT @ if
      HBB-WHITEBOX @ if s"  (whitebox image)" else s"  (product image)" then
   else HBB-ENGINE @ if
      HBB-WHITEBOX @ if s"  (whitebox engine)" else s"  (engine)" then
   else HBB-REPL @ if
      s"  (native application + REPL)"
   else
      s"  (AOT, engine stripped)"
   then then then type
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

\ A build runs on the selected engine, else on the running one.
: HBB-SELECT-ENGINE ( -- )
   BF-ENGINE-SELECTED? 0= if ENGINE-ID:PATH$ BF-ENGINE! then ;

: HBB-BUILD-BEGIN ( -- )
   mono-ns HBB-START-NS !
   HBB-SELECT-ENGINE
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
   HBB-ARTIFACT$ HBB-PRUNE-ENTRIES
   HBB-FINISH ;

\ The selector records separate support and product input spans. Its name is
\ stable context; the support image and artifact use the actual child reads.
: HBB-ENGINE-SELECT$ ( -- ptr u8 n )
   HBB-ENGINE-SELECT-PATH HBB-ENGINE-SELECT-U @ ;

: HBB-ENGINE-SELECT-KEY! ( -- )
   CONTENT-KEY:OPEN
   s" hb-engine-selection-v2" CONTENT-KEY:TEXT+
   NATIVE-BUILD-IMAGE:DONOR$ CONTENT-KEY:TEXT+
   NATIVE-BUILD-IMAGE:ROOT$ CONTENT-KEY:TEXT+
   SOURCE-ROOT:CWD$ CONTENT-KEY:TEXT+
   BUILD-TARGET:CURRENT RTARGET:PROFILE$ CONTENT-KEY:TEXT+
   NATIVE-BUILD-IMAGE:PROGRAM-BYTES$ CONTENT-KEY:TEXT+
   HBB-WHITEBOX @ if s" whitebox" else s" product" then CONTENT-KEY:TEXT+
   HBB-ENGINE-SELECT-KEY CONTENT-KEY:FINAL-HEX
   HBB-ENGINE-SELECT-KEY s" engine-selection"
      HBB-ENGINE-SELECT-PATH HBB-ENGINE-SELECT-U KEYED-IMAGE:PATH! ;

: HBB-ENGINE-ARTIFACT-PATHS ( -- )
   HBB-ARTIFACT-NAME!
   BUILD-CACHE:ROOT$ HBB-ARTIFACT-NAME$ HBB-ARTIFACT-PATH JOIN-PATH
      HBB-ARTIFACT-U ! ;

: HBB-ENGINE-SELECT-READ ( -- )
   HBB-ENGINE-SELECT$ FILE-SIZE {: size:n :}
   size 152 <= if E-NUNIT-PROFILE throw then
   size HBB-ENGINE-RECORD-RESERVE
   HBB-ENGINE-SELECT$ 0 HBB-ENGINE-RECORD size READ-ALL size <>
      if E-FS-IO throw then
   0 HBB-ENGINE-RECORD {: rec:ptr :}
   rec 8 s" HBENGIN2" STR= 0= if E-NUNIT-PROFILE throw then
   rec 136 + LE:U64@ {: supportu:n :}
   rec 144 + LE:U64@ {: productu:n :}
   supportu 0 <= supportu size 152 - > or if E-NUNIT-PROFILE throw then
   productu 0 < productu size 152 supportu + - <> or
      if E-NUNIT-PROFILE throw then
   128 0 ?do
      rec 8 + i + c@ {: c:n :}
      c 48 >= c 57 <= and
      c 97 >= c 102 <= and or 0= if E-NUNIT-PROFILE throw then
   loop
   rec 152 + supportu NFILE-INPUTS:FORMAT
   productu 0<> if
      rec 152 + supportu + productu NFILE-INPUTS:FORMAT
   then ;

: HBB-ENGINE-CHECK-ROWS ( -- )
   HBB-ENGINE-CHECK-A @ HBB-ENGINE-CHECK-U @ NFILE-INPUTS:CHECK ;

: HBB-ENGINE-ENV$ ( ptr u8 n -- ptr u8 n ) {: query:ptr queryu:n :}
   0 begin dup PROC-ENV-N @ COUNT>N < while
      dup >IDX PROC-ENV-SLOT @ {: z:ptr :}
      z query queryu ENV=? if
         drop z queryu 1+ + dup ZLEN exit
      then
      1+
   repeat drop NULL-PTR 0 ;

: HBB-ENGINE-CHECK-PRODUCT ( -- )
   HBB-ENGINE-CHECK-A @ HBB-ENGINE-CHECK-U @
      ['] HBB-ENGINE-ENV$ NFILE-INPUTS:CHECK-WITH ;

: HBB-ENGINE-ROWS-FRESH? ( ptr u8 n [ -- ] -- bool ) {: check :}
   HBB-ENGINE-CHECK-U ! HBB-ENGINE-CHECK-A !
   check catch {: rc:n :}
   rc 0= if HBB-TRUE exit then
   rc E-NUNIT-PROFILE = rc E-NUNIT-FILE = or if HBB-FALSE exit then
   rc throw ;

: HBB-ENGINE-SUPPORT-ROWS$ ( -- ptr u8 n )
   0 HBB-ENGINE-RECORD {: rec:ptr :}
   rec 152 + rec 136 + LE:U64@ ;

: HBB-ENGINE-PRODUCT-ROWS$ ( -- ptr u8 n )
   HBB-ENGINE-SUPPORT-ROWS$ {: rows:ptr supportu:n :}
   0 HBB-ENGINE-RECORD 144 + LE:U64@ {: productu:n :}
   rows supportu + productu ;

: HBB-ENGINE-INPUT$ ( -- ptr u8 n )
   HBB-ENGINE-INPUT-PATH HBB-ENGINE-INPUT-U @ ;

: HBB-ENGINE-INPUT! ( -- )
   s" hb-engine.inputs" BF-A$ HBB-ENGINE-INPUT-PATH HBB-ENGINE-INPUT-U
      HBB-COPY-PATH! ;

: HBB-ENGINE-PRODUCT-ENV ( -- )
   HBB-CMD-RESET
   s" HABU_ENGINE_INPUTS" >LEN HBB-ENGINE-INPUT$ >LEN PROC-ENV-SET
   s" HABU_WHITEBOX_IMAGE" >LEN
   HBB-WHITEBOX @ if s" 1" else s" " drop 0 then >LEN PROC-ENV-SET ;

: HBB-ENGINE-SELECT-FRESH? ( -- bool )
   0 HBB-ENGINE-SELECT-READY !
   0 HBB-ENGINE-SUPPORT-FRESH !
   HBB-ENGINE-SELECT$ FILE? 0= if HBB-FALSE exit then
   [: HBB-ENGINE-SELECT-READ ;] catch {: rc:n :}
   rc 0<> if
      rc E-NUNIT-PROFILE = rc E-NUNIT-FILE = or if HBB-FALSE exit then
      rc throw
   then
   -1 HBB-ENGINE-SELECT-READY !
   HBB-ENGINE-SUPPORT-ROWS$ ['] HBB-ENGINE-CHECK-ROWS HBB-ENGINE-ROWS-FRESH?
      HBB-ENGINE-SUPPORT-FRESH !
   HBB-ENGINE-SUPPORT-FRESH @ 0= if HBB-FALSE exit then
   0 HBB-ENGINE-RECORD 144 + LE:U64@ 0= if HBB-FALSE exit then
   HBB-ENGINE-PRODUCT-ENV
   HBB-ENGINE-PRODUCT-ROWS$ ['] HBB-ENGINE-CHECK-PRODUCT
      HBB-ENGINE-ROWS-FRESH? 0= if HBB-FALSE exit then
   HBB-ENGINE-SELECT$ BUILD-CACHE:USED 0= if HBB-FALSE exit then
   0 HBB-ENGINE-RECORD 8 + HBB-ARTIFACT-KEY-HEX 64 BYTE-COPY
   HBB-ENGINE-ARTIFACT-PATHS
   HBB-TRUE ;

: HBB-ENGINE-IMAGE! ( -- )
   KEYED-IMAGE:CAPTURE-RESET
   0 HBB-MAKER-HIT !
   HBB-ENGINE-SELECT-READY @ HBB-ENGINE-SUPPORT-FRESH @ and 0<> if
      0 HBB-ENGINE-RECORD 72 + 64
      HBB-ENGINE-SUPPORT-ROWS$ NATIVE-BUILD-IMAGE:ADOPT
      NATIVE-BUILD-IMAGE:REUSE? HBB-MAKER-HIT !
   then
   HBB-MAKER-HIT @ 0<> if NATIVE-BUILD-IMAGE:PATH$
   else HBB-REPORT-JSON @ 0<> NATIVE-BUILD-IMAGE:BUILD$ then
      {: image:ptr imageu:n :}
   image imageu HBB-ENGINE-IMAGE-PATH HBB-ENGINE-IMAGE-U HBB-COPY-PATH!
   HBB-MAKER-HIT @ 0= HBB-MAKER-BUILD !
   HBB-ENGINE-COMPAT @ HBB-REPORT-JSON @ 0= and if
      s" native-build: support image " type
      HBB-MAKER-HIT @ 0<> if s" hit" else s" built" then type cr
   then ;

: HBB-ENGINE-SELECT-WRITE ( -- )
   0 HBB-ENGINE-RECORD 136 + LE:U64@
   0 HBB-ENGINE-RECORD 144 + LE:U64@ + 152 + {: size:n :}
   HBB-ENGINE-SELECT$ 0 HBB-ENGINE-RECORD size ATOMIC-WRITE-FILE ;

\ The support child has finished, so its key and rows describe the bytes it
\ actually read. Record that success before a product child can fail.
: HBB-ENGINE-SUPPORT-SELECT! ( -- )
   HBB-ENGINE-SELECT-KEY!
   HBB-ENGINE-SELECT-READY @ 0<> if
      0 HBB-ENGINE-RECORD 72 + 64 NATIVE-BUILD-IMAGE:KEY$ STR=
         if exit then
   then
   NATIVE-BUILD-IMAGE:ROWS$ {: rows:ptr size:n :}
   size 152 + HBB-ENGINE-RECORD-RESERVE
   0 HBB-ENGINE-RECORD {: rec:ptr :}
   s" HBENGIN2" drop rec 8 BYTE-COPY
   64 0 ?do 48 rec 8 + i + c! loop
   NATIVE-BUILD-IMAGE:KEY$ drop rec 72 + 64 BYTE-COPY
   size rec 136 + LE:U64!
   0 rec 144 + LE:U64!
   rows rec 152 + size BYTE-COPY
   HBB-ENGINE-SELECT-WRITE ;

: HBB-ENGINE-CMD ( -- len len outcome )
   ['] HBB-ENGINE-IMAGE! catch {: rc:n :}
   rc E-PROC-TIMEOUT = if HBB-AOT-BUILDER-TIMED-OUT then
   rc 0<> if rc throw then
   HBB-ENGINE-SUPPORT-SELECT!
   HBB-ENGINE-PRODUCT-ENV
   s" --" >LEN PROC-ARGV+
   HBB-GOT-NAME$ BF-A$ >LEN PROC-ARGV+
   HBB-WHITEBOX @ if s" whitebox" >LEN PROC-ARGV+ then
   HBB-ENGINE-TARGET @ if
      s" --target" >LEN PROC-ARGV+
      BUILD-TARGET:CURRENT RTARGET:PROFILE$ >LEN PROC-ARGV+
   then
   HBB-ENGINE-IMAGE-PATH HBB-ENGINE-IMAGE-U @ >LEN
   s" " >LEN
   [: HBB-OUT-ROOM ;]
   [: HBB-ERR-ROOM ;]
   HBB-MAKER-TIMEOUT-MS @ >MS RUN-ARGV-ENV-STDIN-GROWING-BOTH-CAPTURE-OUTCOME ;

: HBB-ENGINE-INPUTS-READ ( -- )
   HBB-ENGINE-INPUT$ FILE-SIZE {: size:n :}
   size 0 <= if E-NUNIT-PROFILE throw then
   NATIVE-BUILD-IMAGE:ROWS$ {: support:ptr supportu:n :}
   size 152 + supportu + HBB-ENGINE-RECORD-RESERVE
   support 0 HBB-ENGINE-RECORD 152 + supportu BYTE-COPY
   HBB-ENGINE-INPUT$ 0 HBB-ENGINE-RECORD 152 + supportu + size
      READ-ALL size <>
      if E-FS-IO throw then
   0 HBB-ENGINE-RECORD 152 + supportu + size NFILE-INPUTS:FORMAT
   HBB-ENGINE-ROW-SHA SHA256-BEGIN
   HBB-ENGINE-ROW-SHA 0 HBB-ENGINE-RECORD 152 + supportu + size SHA256-FEED
   HBB-ENGINE-ROW-SHA HBB-ENGINE-ROW-DIGEST SHA256-END
   CONTENT-KEY:OPEN
   s" hb-engine-artifact-v2" CONTENT-KEY:TEXT+
   NATIVE-BUILD-IMAGE:KEY$ CONTENT-KEY:TEXT+
   HBB-WHITEBOX @ if s" whitebox" else s" product" then CONTENT-KEY:TEXT+
   HBB-ENGINE-ROW-DIGEST CONTENT-KEY:DIGEST+
   HBB-ARTIFACT-KEY-HEX CONTENT-KEY:FINAL-HEX
   HBB-ENGINE-ARTIFACT-PATHS
   0 HBB-ENGINE-RECORD {: rec:ptr :}
   s" HBENGIN2" drop rec 8 BYTE-COPY
   HBB-ARTIFACT-KEY-HEX rec 8 + 64 BYTE-COPY
   NATIVE-BUILD-IMAGE:KEY$ drop rec 72 + 64 BYTE-COPY
   supportu rec 136 + LE:U64!
   size rec 144 + LE:U64! ;

: HBB-BUILD-ENGINE ( -- )
   HBB-PREPARE-CACHE
   -1 HBB-ARTIFACT-CACHE !
   NATIVE-BUILD-IMAGE:CONTEXT!
   HBB-ENGINE-SELECT-KEY!
   HBB-ENGINE-INPUT!
   HBB-ENGINE-SELECT-FRESH? if
      HBB-RESTORE-ARTIFACT? if
         HBB-REPORT-JSON @ 0= if
            s" hb-build: engine artifact hit" type cr
         then
         HBB-FINISH exit
      then
   then
   -1 HBB-MAKER-RUN !
   HBB-ENGINE-CMD HBB-FINISH-MAKER
   HBB-ENGINE-INPUTS-READ
   HBB-INSTALL-OUT
   HBB-INSTALL-ARTIFACT
   HBB-ARTIFACT$ HBB-PRUNE-ENTRIES
   HBB-ENGINE-SELECT-WRITE
   HBB-FINISH ;

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
   HBB-REPL-DEPS$ HBB-INPUTS-READ {: bytes:ptr size:n :}
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
   HBB-INPUTS-RELEASE
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
   HBB-REPL-SUPPORT-PATH$ HBB-INPUTS-READ {: rec:ptr size:n :}
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
   HBB-INPUTS-RELEASE
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
   [: HBB-OUT-ROOM ;]
   [: HBB-ERR-ROOM ;]
   HBB-MAKER-TIMEOUT-MS @ >MS RUN-ARGV-ENV-STDIN-GROWING-BOTH-CAPTURE-OUTCOME ;

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
   HBB-MAKER-INPUTS$ HBB-INPUTS-READ
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
   HBB-INPUTS-RELEASE
   HBB-REPL-SIDECAR-RELEASE
   HBB-REPL-PRODUCER-RELEASE
   rc 0<> if rc throw then ;

: HBB-REPL-INVALIDATE-DEPS ( -- )
   HBB-REPL-DEPS-PATH? 0= if exit then
   HBB-REPL-DEPS$ FILE? if HBB-REPL-DEPS$ REMOVE-FILE then ;

: HBB-BUILD-REPL ( -- )
   HBB-REPL-ROOT!
   HBB-REPL-FRESH? if HBB-FINISH-REPL-FRESH exit then
   HBB-RUN-APP-CMD HBB-FINISH-MAKER
   HBB-REPL-INVALIDATE-DEPS
   HBB-INSTALL-OUT
   HBB-REPL-SAVE-INPUTS
   HBB-REPL-SUPPORT-PATH$ HBB-PRUNE-ENTRIES
   HBB-FINISH ;

: HBB-BUILD ( -- )
   HBB-BUILD-BEGIN
   HBB-ENGINE @ if HBB-BUILD-ENGINE exit then
   HBB-REPL @ if HBB-BUILD-REPL exit then
   HBB-PREPARE-ARTIFACT-CACHE
   HBB-BUILD-REST ;

: HBB-PREPARE-CACHE-CLI ( -- )
   [: HBB-REPL @ if HBB-PREPARE-CACHE else HBB-PREPARE-ARTIFACT-CACHE then ;]
   catch {: rc:n :}
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
      HBB-ENGINE @ if PROC-TIMEOUT-RC HBB-EXIT then
      HBB-UNCAUGHT-RC HBB-EXIT
   then
   rc E-BUILD-INSTALL = if
      [: HBB-CLEANUP ;] catch {: clean:n :}
      clean 0<> if
         s" build cleanup failed" clean HBB-OUT$ HBB-PAIR-REPORT
      then
      s" hb-build: cannot replace output" HBB-BUILD-RC die
   then
   rc throw ;

: HBB-BUILD-CLI ( -- )
   HBB-BUILD-BEGIN
   HBB-ENGINE @ if [: HBB-BUILD-ENGINE ;] catch HBB-CLI-MAKER-CODE exit then
   HBB-PREPARE-CACHE-CLI
   HBB-REPL @ if [: HBB-BUILD-REPL ;] catch HBB-CLI-MAKER-CODE exit then
   [: HBB-BUILD-REST ;] catch HBB-CLI-MAKER-CODE ;

: HBB-MAIN ( -- )
   HBB-PARSE
   HBB-PREPARE-TMP
   HBB-BUILD-CLI
   HBB-CLEANUP ;

: HBB-ENGINE-RUN-PATH ( ptr u8 n bool bool -- )
   {: out:ptr outu:n whitebox:bool target:bool :}
   HBB-RESET-OPTIONS
   -1 HBB-ENGINE !
   -1 HBB-ENGINE-COMPAT !
   whitebox if -1 HBB-WHITEBOX ! then
   target if -1 HBB-ENGINE-TARGET ! then
   out outu HBB-OUT-ARG!
   s" src/habu/habu2.f" HBB-SRC!
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
EXPORT HBB-MAKER-IMAGE$
EXPORT HBB-ENGINE-RUN-PATH
EXPORT HBB-PATHS!
EXPORT HBB-REPORT-JSON
EXPORT HBB-RESET-OPTIONS
EXPORT HBB-SRC$

;using
;package
