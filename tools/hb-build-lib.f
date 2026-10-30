\ hb-build-lib.f - native AOT/REPL build CLI library, package HB-BUILD-CLI:
\ private but for the export block at the end. The name HB-BUILD belongs to the
\ report module, tools/hb-build-report.f.

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
require src/lib/content-key.f
require src/core/sha256.f
require src/lib/adt/option.f                 \ option<NUM:index> STR:INDEX-OF consumer
require src/lib/string-roles.f               \ package STR: the typed string surface
require tools/native-file-inputs.f
require tools/build-out.f
require tools/build-support.f
require tools/app-build-image.f
require tools/aot-builder-image.f
require tools/hb-build-report.f
require tools/event-closure-lib.f
require tools/native-build-image.f
require tools/build-target.f

package HB-BUILD-CLI
using BUILD-FIXPOINT                     \ shared build paths, child env and source scratch

64 constant HBB-USAGE-RC
66 constant HBB-NOINPUT-RC
67 constant HBB-UNCAUGHT-RC
74 constant HBB-BUILD-RC
34 constant HBB-DQ
10 constant HBB-LF
600000 constant HBB-MAKER-DEFAULT-MS
$7FFFFFFF constant HBB-MAKER-MAX-MS
65536 constant HBB-CAPTURE-CAP
80 constant HBB-REPL-DEPS-HEADER-U
\ A support record: the selector key it was written under (64 hex), the
\ producer key it selects (64 hex), the rows length (u64), then the rows.
136 constant HBB-REPL-SUPPORT-HEADER-U
136 constant HBB-MAKER-SUPPORT-HEADER-U
136 constant HBB-ENGINE-SUPPORT-HEADER-U

create HBB-SRC-PATH PATH-CAP allot
create HBB-OUT-PATH PATH-CAP allot
create HBB-MAKER-KEY-HEX 80 allot
create HBB-MAP-SRC PATH-CAP allot
create HBB-MAP-DST PATH-CAP allot
create HBB-PAIR-STAGE PATH-CAP allot
create HBB-PAIR-BACKUP PATH-CAP allot
create HBB-REPL-DEPS-PATH PATH-CAP allot
create HBB-MAKER-INPUTS-PATH PATH-CAP allot
create HBB-COUNTS-PATH PATH-CAP allot
create HBB-REPL-SUPPORT-PATH PATH-CAP allot
create HBB-REPL-SUPPORT-KEY 64 allot
create HBB-REPL-RUNNING-ROOT PATH-CAP allot
create HBB-REPL-SELECTED-ROOT PATH-CAP allot
create HBB-MAKER-RUNNING-ROOT PATH-CAP allot
create HBB-MAKER-SELECTED-ROOT PATH-CAP allot
create HBB-MAKER-SUPPORT-PATH PATH-CAP allot
create HBB-MAKER-SUPPORT-KEY 64 allot
create HBB-REPL-ENGINE-DIGEST 32 allot
create HBB-BUILDER-KEY 64 allot   \ BF-ENGINE$'s engine key, naming its img/
create HBB-REPL-ENGINE-SHA SHA256-FILE-CTX-BYTES allot
create HBB-ENGINE-SELECT-KEY 64 allot
create HBB-ENGINE-SELECT-PATH PATH-CAP allot
create HBB-ENGINE-IMAGE-PATH PATH-CAP allot
create HBB-LF-BUF 1 allot
HBB-LF HBB-LF-BUF c!

DYNAMIC-BUFFER HBB-OUT-CAPTURE u8
DYNAMIC-BUFFER HBB-ERR-CAPTURE u8
variable HBB-SRC-U
variable HBB-OUT-U
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
variable HBB-COUNTS-U
variable HBB-REPL-SUPPORT-U
variable HBB-MAKER-RUNNING-ROOT-U
variable HBB-MAKER-SELECTED-ROOT-U
variable HBB-MAKER-SUPPORT-U
variable HBB-REPL-RUNNING-ROOT-U
variable HBB-REPL-SELECTED-ROOT-U
variable HBB-REPL-PRODUCER-U
variable HBB-REPL-PRODUCER-READY
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
variable HBB-MAKER-HIT
TYPED-VARIABLE HBB-AOT-BUILDER-A ptr u8
variable HBB-AOT-BUILDER-U
variable HBB-MAKER-BUILD
variable HBB-MAKER-RUN
variable HBB-LINE-START
variable HBB-JSON-FOUND
variable HBB-START-NS
variable HBB-ELAPSED-NS
variable HBB-MAKER-TIMEOUT-MS
variable HBB-ENGINE-SELECT-U
variable HBB-ENGINE-IMAGE-U
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
   u PATH-CAP > if E-BUILD-PATH throw then
   a dst u BYTE-COPY
   u up ! ;

: HBB-SRC! ( ptr u8 n -- )
   HBB-SRC-PATH HBB-SRC-U HBB-COPY-PATH! ;

: HBB-OUT! ( ptr u8 n -- )
   HBB-OUT-PATH HBB-OUT-U HBB-COPY-PATH! ;

\ The source path is held in PATH-CAP bytes, so the CLI refuses a longer
\ one by name, before it builds anything.
: HBB-SRC-ARG! ( ptr u8 n -- )
   {: a:ptr u:n :}
   u PATH-CAP > if s" hb-build: source path too long" HBB-USAGE-RC die then
   a u HBB-SRC! ;

\ -o is replaced by renaming over it a sibling made in its directory
\ (src/lib/fs-mutate.f FS:REPLACE-STAGED). Before it builds anything the CLI
\ refuses by name an -o longer than PATH-CAP bytes, one with no last name,
\ one in a directory that is not there, and one that is a directory, which a
\ rename cannot replace. The host alone judges the last name's length. An -o
\ with no directory part goes in the working directory.
: HBB-OUT-ARG! ( ptr u8 n -- )
   {: a:ptr u:n :}
   u PATH-CAP > if s" hb-build: output path too long" HBB-USAGE-RC die then
   a u FS:BASENAME nip {: nameu:n :}
   nameu 0= if s" hb-build: output path has no file name" HBB-USAGE-RC die then
   u nameu - {: diru:n :}
   diru 0 > if
      a diru FS:DIR? 0= if s" hb-build: no such output directory" HBB-USAGE-RC die then
   then
   a u FS:DIR? if s" hb-build: output path is a directory" HBB-USAGE-RC die then
   a u HBB-OUT! ;

: HBB-SRC$ ( -- ptr u8 n )
   HBB-SRC-PATH HBB-SRC-U @ ;

: HBB-OUT$ ( -- ptr u8 n )
   HBB-OUT-PATH HBB-OUT-U @ ;

: HBB-MAP-PATH! ( ptr u8 n ptr u8 -- n ) {: a:ptr u:n dst:ptr :}
   u 4 + PATH-CAP > if E-BUILD-PATH throw then
   a dst u BYTE-COPY
   s" .map" drop dst u + 4 BYTE-COPY
   u 4 + ;

: HBB-MAP-SRC! ( ptr u8 n -- ) HBB-MAP-SRC HBB-MAP-PATH! HBB-MAP-SRC-U ! ;
: HBB-MAP-DST! ( ptr u8 n -- ) HBB-MAP-DST HBB-MAP-PATH! HBB-MAP-DST-U ! ;
: HBB-MAP-SRC$ ( -- ptr u8 n ) HBB-MAP-SRC HBB-MAP-SRC-U @ ;
: HBB-MAP-DST$ ( -- ptr u8 n ) HBB-MAP-DST HBB-MAP-DST-U @ ;

: HBB-MAP-FILL ( ptr u8 n -- ) HBB-MAP-SRC$ 2swap FS:COPY-FILE-STREAM ;
: HBB-MAP-COPY ( ptr u8 n ptr u8 n -- )
   {: src:ptr srcu:n dst:ptr dstu:n :}
   src srcu HBB-MAP-SRC!
   dst dstu HBB-MAP-DST!
   HBB-MAP-SRC$ FS:FILE? 0= if E-BUILD-PATH throw then
   HBB-MAP-DST$ [: HBB-MAP-FILL ;] FS:REPLACE-STAGED ;

: HBB-MAP-REMOVE? ( ptr u8 n -- )
   HBB-MAP-SRC!
   HBB-MAP-SRC$ FS:FILE? if HBB-MAP-SRC$ FS:REMOVE-FILE then ;

: HBB-REPL-DEPS$ ( -- ptr u8 n )
   HBB-REPL-DEPS-PATH HBB-REPL-DEPS-U @ ;

: HBB-REPL-DEPS-SUFFIX$ ( -- ptr u8 n )
   s" .hb-deps" ;

\ A sidecar sits beside the output, but is optional for an output path
\ without room in PATH-CAP for the suffix. A sidecar name the host refuses
\ fails its write.
: HBB-REPL-DEPS-PATH? ( -- bool )
   HBB-OUT$ {: a:ptr u:n :}
   HBB-REPL-DEPS-SUFFIX$ {: suffix:ptr suffixu:n :}
   u suffixu + PATH-CAP > if HBB-FALSE exit then
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
   0 HBB-MAKER-RUN ! ;

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
   HBB-SRC$ FS:FILE? 0= if s" hb-build: no such source" HBB-NOINPUT-RC die then
   HBB-SRC$ HBB-PATH-HAS-DQ? if s" hb-build: source path contains a double quote" HBB-USAGE-RC die then ;

\ Every build gets a private directory under the caller's TMPDIR: the engine's
\ build mode writes fixed names there (src/habu/aot-common.f `hb-aot-got`,
\ `hb-aot-obj`), so builds sharing one TMPDIR, as gate pool builds do, read each
\ other's artifacts. FS:MKTEMP-DIR refuses a base that is missing or not a
\ directory.
: HBB-PREPARE-TMP ( -- )
   BF-TMP-RESET
   FS:CLEANUP-RESET
   s" hb-build-native" FS:MKTEMP-DIR 2dup BF-TMP! FS:CLEANUP-TREE+ ;

\ The out root is made before any maker runs, so a root that cannot be made
\ fails the build up front as a path error rather than midway as a file error.
: HBB-PREPARE-OUT ( -- )
   [: BUILD-OUT:ROOT$ 2drop ;] catch {: rc:n :}
   rc 0= if exit then
   rc E-FS-IO = rc E-FS-CAPACITY = or if E-BUILD-PATH throw then
   rc throw ;

: HBB-CLEANUP ( -- )
   FS:CLEANUP-RUN
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

\ The maker's per-file counts (tools/native-definition-cache.f COUNTS-WRITE):
\ `<reused> <compiled>` and a newline, two nonnegative cells of at most 19
\ decimal digits each. The file lives in the build's TMP directory for one
\ maker run.
40 constant HBB-COUNTS-MAX
HBB-COUNTS-MAX BUFFER: HBB-COUNTS-BUF

: HBB-COUNTS$ ( -- ptr u8 n )
   s" hb-build-counts" BF-A$ {: a:ptr u:n :}
   u PATH-CAP > if E-BUILD-PATH throw then
   a HBB-COUNTS-PATH u BYTE-COPY
   u HBB-COUNTS-U !
   HBB-COUNTS-PATH HBB-COUNTS-U @ ;

\ A failing maker's counts are never read. Every launch removes the file first
\ so that a maker that exits 0 without writing it, or an earlier in-process
\ build sharing this TMP directory, cannot hand this build stale counts.
: HBB-COUNTS-ARM ( -- )
   HBB-COUNTS$ 2dup FS:EXISTS? if FS:REMOVE-FILE else 2drop then
   s" HABU_BUILD_COUNTS" >LEN HBB-COUNTS$ >LEN PROC-ENV-SET ;

: HBB-COUNT ( ptr u8 n -- n )
   STR>NUMBER? MATCH option
      none OF E-BUILD-STATUS throw ENDOF
      some OF ENDOF
   ;MATCH {: v:n :}
   v 0 < if E-BUILD-STATUS throw then
   v ;

\ A successful maker finished its counted session, so a missing or malformed
\ file is a maker that did not report: E-BUILD-STATUS.
: HBB-COUNTS-READ ( -- )
   HBB-COUNTS$ FS:FILE? 0= if E-BUILD-STATUS throw then
   HBB-COUNTS$ FS:FILE-SIZE {: size:n :}
   size 2 < size HBB-COUNTS-MAX > or if E-BUILD-STATUS throw then
   HBB-COUNTS$ HBB-COUNTS-BUF size FS:READ-ALL size <> if E-BUILD-STATUS throw then
   HBB-COUNTS-BUF size 1- + c@ 10 <> if E-BUILD-STATUS throw then
   HBB-COUNTS-BUF size 1- STR-SPACE INDEX-OF MATCH option
      none OF E-BUILD-STATUS throw ENDOF
      some OF IDX>N ENDOF
   ;MATCH {: sp:n :}
   HBB-COUNTS-BUF sp HBB-COUNT
   HBB-COUNTS-BUF sp 1+ + size 2 - sp - HBB-COUNT
   HB-BUILD:CAPTURE-FILES
   HBB-COUNTS$ FS:REMOVE-FILE ;

: HBB-CMD-RESET ( -- )
   PROC-ARGV-RESET
   BF-PREPARE-ENV
   HBB-COUNTS-ARM
   PROC-ENV-INHERIT-MISSING ;

: HBB-GOT-NAME$ ( -- ptr u8 n )
   HBB-ENGINE @ if s" hb-engine-got" exit then
   HBB-REPL @ if s" hb-build-got" else s" hb-aot-got" then ;

: HBB-MAKER-INPUTS$ ( -- ptr u8 n )
   HBB-GOT-NAME$ BF-A$ {: a:ptr u:n :}
   s" .inputs" {: suffix:ptr suffixu:n :}
   u suffixu + PATH-CAP > if E-BUILD-PATH throw then
   a HBB-MAKER-INPUTS-PATH u BYTE-COPY
   suffix HBB-MAKER-INPUTS-PATH u + suffixu BYTE-COPY
   u suffixu + HBB-MAKER-INPUTS-U !
   HBB-MAKER-INPUTS-PATH HBB-MAKER-INPUTS-U @ ;

: HBB-INPUTS-READ ( ptr u8 n -- ptr u8 n )
   {: path:ptr pathu:n :}
   path pathu FS:FILE-SIZE {: size:n :}
   size 0 <= if E-NUNIT-FILE throw then
   size HBB-INPUTS-RESERVE
   path pathu 0 HBB-INPUTS size FS:READ-ALL size <> if E-FS-IO throw then
   0 HBB-INPUTS size ;

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
\ absence is an input too: a producer selected while the map was missing must
\ not serve an engine that has one, or its products carry unknown-origin rows.
: HBB-KEY-DONOR-MAP+ ( CONTENT-KEY:fold -- CONTENT-KEY:fold )
   HBB-REPL @ if exit then
   BF-ENGINE$ HBB-MAP-SRC!
   HBB-MAP-SRC$ FS:FILE? if
      s" donor-map:present" CONTENT-KEY:TEXT+
      HBB-MAP-SRC$ HBB-KEY-FILE+
   else
      s" donor-map:missing" CONTENT-KEY:TEXT+
   then ;

: HBB-MAKER-KEY-BODY ( CONTENT-KEY:fold -- )
   s" hb-build-native-producer-v3" CONTENT-KEY:TEXT+
   BF-ENGINE$ HBB-KEY-FILE+
   HBB-KEY-DONOR-MAP+
   HBB-KEY-TARGET+
   s" tools/hb-build.f" HBB-MAKER-CLOSURE+
   s" tools/aot-builder.f" HBB-MAKER-CLOSURE+
   s" tools/aot-build.f" HBB-MAKER-CLOSURE+
   HBB-MAKER-KEY-HEX CONTENT-KEY:FINAL-HEX ;

: HBB-MAKER-KEY-COLD ( -- )
   [: HBB-MAKER-KEY-BODY ;] CONTENT-KEY:WITH-FOLD ;

: HBB-MAKER-CANON ( ptr u8 n -- ptr u8 n bool )
   {: path:ptr pathu:n :}
   path pathu SOURCE-ROOT:CANON-OS {: answer:ptr answeru:n exists:bool :}
   path pathu answer answeru exists NFILE-INPUTS:ADD-CANON-ONCE
   answer answeru exists ;

: HBB-MAKER-SUPPORT-PATH$ ( -- ptr u8 n )
   HBB-MAKER-SUPPORT-PATH HBB-MAKER-SUPPORT-U @ ;

: HBB-MAKER-SUPPORT-BODY ( CONTENT-KEY:fold -- )
   s" hb-maker-support-v1" CONTENT-KEY:TEXT+
   HBB-BUILDER-KEY KEYED-IMAGE:KEY-HEX-LEN CONTENT-KEY:TEXT+
   HBB-KEY-DONOR-MAP+
   HBB-KEY-TARGET+
   SOURCE-ROOT:CWD$ CONTENT-KEY:TEXT+
   HBB-MAKER-SELECTED-ROOT HBB-MAKER-SELECTED-ROOT-U @ CONTENT-KEY:TEXT+
   HBB-MAKER-SUPPORT-KEY CONTENT-KEY:FINAL-HEX ;

\ The selector depends only on fixed support identity. The source and path
\ queries are validated in the record it selects.
\ The engine enters as its key, the directory its images live in, so the engine
\ is hashed once.
: HBB-MAKER-SUPPORT-SELECT ( -- )
   BF-ENGINE$ HBB-BUILDER-KEY BUILD-OUT:KEY-OF!
   [: HBB-MAKER-SUPPORT-BODY ;] CONTENT-KEY:WITH-FOLD
   s" aot-builder.inputs" HBB-BUILDER-KEY
      HBB-MAKER-SUPPORT-PATH HBB-MAKER-SUPPORT-U KEYED-IMAGE:PATH! ;

: HBB-MAKER-SUPPORT-CHECK ( -- )
   HBB-MAKER-SUPPORT-PATH$ FS:FILE-SIZE {: size:n :}
   size HBB-MAKER-SUPPORT-HEADER-U <= if E-NUNIT-PROFILE throw then
   size HBB-MAKER-SUPPORT-RESERVE
   HBB-MAKER-SUPPORT-PATH$ 0 HBB-MAKER-SUPPORT size FS:READ-ALL size <>
      if E-NUNIT-PROFILE throw then
   0 HBB-MAKER-SUPPORT {: rec:ptr :}
   rec 64 HBB-MAKER-SUPPORT-KEY 64 STR= 0= if E-NUNIT-PROFILE throw then
   rec 128 + LE:U64@ size HBB-MAKER-SUPPORT-HEADER-U - <>
      if E-NUNIT-PROFILE throw then
   rec HBB-MAKER-SUPPORT-HEADER-U +
      size HBB-MAKER-SUPPORT-HEADER-U - NFILE-INPUTS:CHECK
   rec 64 + HBB-MAKER-KEY-HEX 64 BYTE-COPY ;

: HBB-MAKER-SUPPORT-FRESH? ( -- bool )
   HBB-MAKER-SUPPORT-PATH$ FS:FILE? 0= if HBB-FALSE exit then
   [: HBB-MAKER-SUPPORT-CHECK ;] catch {: rc:n :}
   HBB-MAKER-SUPPORT-RELEASE
   rc 0= if HBB-TRUE exit then
   rc E-NUNIT-PROFILE = rc E-NUNIT-FILE = or if HBB-FALSE exit then
   rc throw ;

: HBB-MAKER-SUPPORT-WRITE ( -- )
   NFILE-INPUTS:BYTES$ {: rows:ptr rowsu:n :}
   HBB-MAKER-SUPPORT-HEADER-U rowsu + HBB-MAKER-SUPPORT-RESERVE
   0 HBB-MAKER-SUPPORT {: rec:ptr :}
   HBB-MAKER-SUPPORT-KEY rec 64 BYTE-COPY
   HBB-MAKER-KEY-HEX rec 64 + 64 BYTE-COPY
   rowsu rec 128 + LE:U64!
   rows rec HBB-MAKER-SUPPORT-HEADER-U + rowsu BYTE-COPY
   HBB-MAKER-SUPPORT-PATH$ rec HBB-MAKER-SUPPORT-HEADER-U rowsu +
      FS:ATOMIC-WRITE-FILE ;

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
\ selected engine's root. Hash the same resolved closure for the producer key
\ so a source edit in that root selects a fresh producer.
: HBB-MAKER-KEY! ( -- )
   SOURCE-ROOT:ENGINE$ HBB-MAKER-RUNNING-ROOT
      HBB-MAKER-RUNNING-ROOT-U HBB-COPY-PATH!
   BF-ENGINE$ SOURCE-ROOT:ENGINE-FOR$ HBB-MAKER-SELECTED-ROOT
      HBB-MAKER-SELECTED-ROOT-U HBB-COPY-PATH!
   HBB-MAKER-SELECTED-ROOT HBB-MAKER-SELECTED-ROOT-U @ SOURCE-ROOT:ENGINE!
   [: ['] HBB-MAKER-KEY-IN-ROOT SOURCE-ROOT:WITH-CWD ;] catch {: rc:n :}
   HBB-MAKER-RUNNING-ROOT HBB-MAKER-RUNNING-ROOT-U @ SOURCE-ROOT:ENGINE!
   rc 0<> if rc throw then ;

\ An AOT build computes the producer key before the maker runs:
\ HBB-RUN-MAKER-CMD selects the maker through HBB-SELECT-MAKER, which reads
\ HBB-MAKER-KEY-HEX without computing it.
: HBB-PREPARE-AOT ( -- )
   HBB-PREPARE-OUT
   HBB-MAKER-KEY! ;

: HBB-JSON-FLAG$ ( -- ptr u8 n )
   HBB-JSON @ if s" 1" exit then
   s" 0" ;

\ THE PRODUCT TIER IS THE CALLER'S. A maker child is a saved image, which starts
\ at tier 0, so its first stdin line selects this process's tier for the
\ application it loads; x86-64 has tier 1 only. Each maker's stdin is one of
\ two complete literals chosen by the tier. The tier joins the REPL sidecar, so
\ a product of one tier is never up to date for the other.
: HBB-TIER ( -- n )
   HB-TARGET-LINUX-X86-64? if 1 exit then
   tier@ ;

: HBB-AOT-STDIN$ ( -- ptr u8 n )
   HBB-TIER 0= if S\" 0 set-tier\nAOT-BUILDER:OPEN\nAOT-BUILDER:RUN\n" exit then
   S\" 1 set-tier\nAOT-BUILDER:OPEN\nAOT-BUILDER:RUN\n" ;

: HBB-APP-STDIN$ ( -- ptr u8 n )
   HBB-TIER 0= if S\" 0 set-tier\nAPP-BUILD:RUN-SAVED\n" exit then
   S\" 1 set-tier\nAPP-BUILD:RUN-SAVED\n" ;

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
      HBB-COUNTS-READ
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
\ build on that engine under the same out root. The caller selects BF-ENGINE$.
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
   HBB-AOT-STDIN$ >LEN
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
\ -o only once it is whole and executable (src/lib/fs-mutate.f FS:REPLACE-STAGED), so
\ a failed install leaves -o as it was and no sibling.
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
   HBB-OUT$ [: HBB-FILL-RUN ;] FS:REPLACE-STAGED ;

: HBB-REPLACE-OUT ( [ ptr u8 n -- ] -- )
   is HBB-FILL
   0 HBB-FILLING !
   [: HBB-REPLACE-RUN ;] catch {: rc:n :}
   rc 0= if exit then
   HBB-FILLING @ if rc throw then
   rc E-FS-OPEN = rc E-FS-IO = or if E-BUILD-INSTALL throw then
   rc throw ;

\ The executable's FS:REPLACE-STAGED fill publishes the already staged map only
\ after its executable copy and chmod have completed. The outer rename is the
\ last step; a refusal there puts the prior map back from its sibling.
defer HBB-PAIR-EXE-FILL ( ptr u8 n -- )

: HBB-PAIR-STAGE$ ( -- ptr u8 n )
   HBB-PAIR-STAGE HBB-PAIR-STAGE-U @ ;

: HBB-PAIR-BACKUP$ ( -- ptr u8 n )
   HBB-PAIR-BACKUP HBB-PAIR-BACKUP-U @ ;

: HBB-PAIR-RESERVE ( ptr u8 ptr n -- )
   {: dst:ptr up:ptr :}
   HBB-MAP-DST$ FS:RESERVE-SIBLING {: a:ptr u:n fd:n :}
   a u dst up HBB-COPY-PATH!
   fd FS:CLOSE-FD 0<> if E-FS-IO throw then
   dst up @ FS:CLEANUP+ ;

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
   HBB-MAP-SRC$ FS:FILE? 0= if E-BUILD-PATH throw then
   [: HBB-PAIR-STAGE-RESERVE ;] catch HBB-PAIR-RESERVE-CODE
   HBB-MAP-SRC$ HBB-PAIR-STAGE$ FS:COPY-FILE-STREAM
   HBB-MAP-DST$ FS:TRY-LSTAT if
      [: HBB-PAIR-BACKUP-RESERVE ;] catch HBB-PAIR-RESERVE-CODE
      -1 HBB-PAIR-HAD-MAP !
   then ;

: HBB-PAIR-FILL ( ptr u8 n -- )
   2dup HBB-PAIR-EXE-FILL
   2drop
   HBB-PAIR-HAD-MAP @ if
      HBB-MAP-DST$ HBB-PAIR-BACKUP$ FS:RENAME-FILE
      -1 HBB-PAIR-MOVED !
   then
   HBB-PAIR-STAGE$ HBB-MAP-DST$ FS:RENAME-FILE
   -1 HBB-PAIR-COMMITTED ! ;

: HBB-PAIR-ROLLBACK ( -- )
   HBB-PAIR-MOVED @ if
      HBB-PAIR-BACKUP$ HBB-MAP-DST$ FS:RENAME-FILE
   else
      HBB-PAIR-COMMITTED @ if HBB-MAP-DST$ FS:REMOVE-FILE then
   then ;

: HBB-PAIR-CLEAN-ONE ( ptr u8 n -- )
   {: a:ptr u:n :}
   u 0= if exit then
   a u FS:ATOMIC-CLEAN-TEMP
   a u FS:CLEANUP-FORGET ;

: HBB-PAIR-CLEAN-BACKUP ( -- )
   HBB-PAIR-KEEP-BACKUP @ if
      HBB-PAIR-BACKUP$ FS:CLEANUP-FORGET
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

\ The engine the child wrote under TMPDIR may be on another filesystem; it is
\ copied.
: HBB-INSTALL-FILL ( ptr u8 n -- )
   {: tmp:ptr tmpu:n :}
   HBB-GOT-NAME$ BF-A$ tmp tmpu FS:COPY-FILE-STREAM
   tmp tmpu FS:CHMOD-X ;

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
   HBB-GOT-NAME$ BF-A$ FS:REMOVE-FILE ;

\ A maker that exits without writing its inputs record must not hand this build
\ an earlier build's record, so every maker run removes it first.
: HBB-MAKER-INPUTS-CLEAR ( -- )
   HBB-MAKER-INPUTS$ 2dup FS:EXISTS? if FS:REMOVE-FILE else 2drop then ;

: HBB-RUN-MAKER ( -- )
   -1 HBB-MAKER-RUN !
   HBB-GOT-NAME$ BF-REMOVE-TMP
   HBB-GOT-NAME$ BF-A$ HBB-MAP-REMOVE?
   HBB-MAKER-INPUTS-CLEAR
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

: HBB-PATH-ERROR ( -- )
   HBB-JSON @ if HB-BUILD:PATH-ERROR$ else HB-BUILD:PATH-ERROR-TEXT$ then HBB-WERR
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
   HBB-RUN-MAKER
   HBB-INSTALL-OUT
   HBB-FINISH ;

\ The engine build's support image is selected through
\ img/native-build-support.inputs, a support record as HBB-REPL-SUPPORT-HEADER-U
\ lays out: the rows are checked against this invocation before the key is
\ adopted. The engine build itself always runs.
: HBB-ENGINE-SELECT$ ( -- ptr u8 n )
   HBB-ENGINE-SELECT-PATH HBB-ENGINE-SELECT-U @ ;

: HBB-ENGINE-SELECT-BODY ( CONTENT-KEY:fold -- )
   s" hb-engine-selection-v2" CONTENT-KEY:TEXT+
   NATIVE-BUILD-IMAGE:DONOR$ CONTENT-KEY:TEXT+
   NATIVE-BUILD-IMAGE:ROOT$ CONTENT-KEY:TEXT+
   SOURCE-ROOT:CWD$ CONTENT-KEY:TEXT+
   BUILD-TARGET:CURRENT RTARGET:PROFILE$ CONTENT-KEY:TEXT+
   NATIVE-BUILD-IMAGE:PROGRAM-BYTES$ CONTENT-KEY:TEXT+
   HBB-ENGINE-SELECT-KEY CONTENT-KEY:FINAL-HEX ;

: HBB-ENGINE-SELECT-KEY! ( -- )
   [: HBB-ENGINE-SELECT-BODY ;] CONTENT-KEY:WITH-FOLD
   s" native-build-support.inputs" NATIVE-BUILD-IMAGE:DONOR$ drop
      HBB-ENGINE-SELECT-PATH HBB-ENGINE-SELECT-U KEYED-IMAGE:PATH! ;

: HBB-ENGINE-SUPPORT-ROWS$ ( -- ptr u8 n )
   0 HBB-ENGINE-RECORD {: rec:ptr :}
   rec HBB-ENGINE-SUPPORT-HEADER-U + rec 128 + LE:U64@ ;

: HBB-ENGINE-SELECT-CHECK ( -- )
   HBB-ENGINE-SELECT$ FS:FILE-SIZE {: size:n :}
   size HBB-ENGINE-SUPPORT-HEADER-U <= if E-NUNIT-PROFILE throw then
   size HBB-ENGINE-RECORD-RESERVE
   HBB-ENGINE-SELECT$ 0 HBB-ENGINE-RECORD size FS:READ-ALL size <>
      if E-NUNIT-PROFILE throw then
   0 HBB-ENGINE-RECORD {: rec:ptr :}
   rec 64 HBB-ENGINE-SELECT-KEY 64 STR= 0= if E-NUNIT-PROFILE throw then
   rec 128 + LE:U64@ size HBB-ENGINE-SUPPORT-HEADER-U - <>
      if E-NUNIT-PROFILE throw then
   HBB-ENGINE-SUPPORT-ROWS$ NFILE-INPUTS:CHECK ;

: HBB-ENGINE-SELECT-FRESH? ( -- bool )
   HBB-ENGINE-SELECT$ FS:FILE? 0= if HBB-FALSE exit then
   [: HBB-ENGINE-SELECT-CHECK ;] catch {: rc:n :}
   rc 0= if HBB-TRUE exit then
   rc E-NUNIT-PROFILE = rc E-NUNIT-FILE = or if HBB-FALSE exit then
   rc throw ;

: HBB-ENGINE-IMAGE! ( -- )
   KEYED-IMAGE:CAPTURE-RESET
   0 HBB-MAKER-HIT !
   HBB-ENGINE-SELECT-FRESH? if
      0 HBB-ENGINE-RECORD 64 + 64
      HBB-ENGINE-SUPPORT-ROWS$ NATIVE-BUILD-IMAGE:ADOPT
      NATIVE-BUILD-IMAGE:REUSE? HBB-MAKER-HIT !
   then
   HBB-ENGINE-RECORD-RELEASE
   HBB-MAKER-HIT @ 0<> if NATIVE-BUILD-IMAGE:PATH$
   else HBB-REPORT-JSON @ 0<> NATIVE-BUILD-IMAGE:BUILD$ then
      {: image:ptr imageu:n :}
   image imageu HBB-ENGINE-IMAGE-PATH HBB-ENGINE-IMAGE-U HBB-COPY-PATH!
   HBB-MAKER-HIT @ 0= HBB-MAKER-BUILD !
   HBB-ENGINE-COMPAT @ HBB-REPORT-JSON @ 0= and if
      s" native-build: support image " type
      HBB-MAKER-HIT @ 0<> if s" hit" else s" built" then type cr
   then ;

\ The support child has finished, so its key and rows describe the bytes it
\ actually read. Record that success before the engine child can fail.
: HBB-ENGINE-SUPPORT-SELECT! ( -- )
   HBB-MAKER-HIT @ 0<> if exit then
   NATIVE-BUILD-IMAGE:ROWS$ {: rows:ptr rowsu:n :}
   HBB-ENGINE-SUPPORT-HEADER-U rowsu + HBB-ENGINE-RECORD-RESERVE
   0 HBB-ENGINE-RECORD {: rec:ptr :}
   HBB-ENGINE-SELECT-KEY rec 64 BYTE-COPY
   NATIVE-BUILD-IMAGE:KEY$ drop rec 64 + 64 BYTE-COPY
   rowsu rec 128 + LE:U64!
   rows rec HBB-ENGINE-SUPPORT-HEADER-U + rowsu BYTE-COPY
   HBB-ENGINE-SELECT$ rec HBB-ENGINE-SUPPORT-HEADER-U rowsu +
      FS:ATOMIC-WRITE-FILE
   HBB-ENGINE-RECORD-RELEASE ;

: HBB-ENGINE-CMD ( -- len len outcome )
   ['] HBB-ENGINE-IMAGE! catch {: rc:n :}
   rc E-PROC-TIMEOUT = if HBB-AOT-BUILDER-TIMED-OUT then
   rc 0<> if rc throw then
   HBB-ENGINE-SUPPORT-SELECT!
   HBB-CMD-RESET
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

: HBB-BUILD-ENGINE ( -- )
   HBB-PREPARE-OUT
   NATIVE-BUILD-IMAGE:CONTEXT!
   HBB-ENGINE-SELECT-KEY!
   -1 HBB-MAKER-RUN !
   HBB-ENGINE-CMD HBB-FINISH-MAKER
   HBB-INSTALL-OUT
   HBB-FINISH ;

: HBB-REPL-ENGINE-DIGEST! ( -- )
   HBB-REPL-ENGINE-SHA BF-ENGINE$ HBB-REPL-ENGINE-DIGEST SHA256-FILE-IN
      dup 0<> if throw then drop ;

: HBB-REPL-SUPPORT-PATH$ ( -- ptr u8 n )
   HBB-REPL-SUPPORT-PATH HBB-REPL-SUPPORT-U @ ;

: HBB-REPL-SUPPORT-BODY ( CONTENT-KEY:fold -- )
   s" hb-repl-support-v2" CONTENT-KEY:TEXT+
   BF-ENGINE$ CONTENT-KEY:TEXT+
   HBB-REPL-ENGINE-DIGEST CONTENT-KEY:DIGEST+
   SOURCE-ROOT:CWD$ CONTENT-KEY:TEXT+
   HBB-REPL-SELECTED-ROOT HBB-REPL-SELECTED-ROOT-U @ CONTENT-KEY:TEXT+
   HBB-REPL-SUPPORT-KEY CONTENT-KEY:FINAL-HEX ;

\ Only the engine, invocation directory, and fallback root select fixed build
\ support. Application source and output are checked by the normal sidecar.
: HBB-REPL-SUPPORT-SELECT ( -- )
   HBB-REPL-ENGINE-DIGEST!
   [: HBB-REPL-SUPPORT-BODY ;] CONTENT-KEY:WITH-FOLD
   HBB-REPL-ENGINE-DIGEST HBB-BUILDER-KEY SHA256>HEX
   s" app-builder.inputs" HBB-BUILDER-KEY
      HBB-REPL-SUPPORT-PATH HBB-REPL-SUPPORT-U KEYED-IMAGE:PATH! ;

: HBB-REPL-FRESH-CHECK ( -- )
   HBB-REPL-DEPS$ HBB-INPUTS-READ {: bytes:ptr size:n :}
   size HBB-REPL-DEPS-HEADER-U < if E-NUNIT-PROFILE throw then
   bytes 8 s" HBRDEPS4" STR= 0= if E-NUNIT-PROFILE throw then
   bytes 8 + LE:U64@ {: srcu:n :}
   bytes 16 + LE:U64@ {: engu:n :}
   bytes 24 + LE:U64@ {: outu:n :}
   bytes 32 + LE:U64@ {: rootu:n :}
   srcu 0 <= srcu PATH-CAP > or
   engu 0 <= engu PATH-CAP > or or
   outu 0 <= outu PATH-CAP > or or
   rootu 0 <= rootu PATH-CAP > or or if E-NUNIT-PROFILE throw then
   srcu engu + outu + rootu + size HBB-REPL-DEPS-HEADER-U - >= if E-NUNIT-PROFILE throw then
   bytes HBB-REPL-DEPS-HEADER-U + srcu HBB-SRC$ STR= 0= if E-NUNIT-PROFILE throw then
   bytes HBB-REPL-DEPS-HEADER-U srcu + + engu BF-ENGINE$ STR= 0= if E-NUNIT-PROFILE throw then
   bytes 40 + LE:U64@ HBB-TIER <> if E-NUNIT-PROFILE throw then
   ['] HBB-REPL-ENGINE-DIGEST! catch 0<> if E-NUNIT-PROFILE throw then
   bytes 48 + 32 HBB-REPL-ENGINE-DIGEST 32 STR= 0=
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
   HBB-OUT$ FS:EXECUTABLE? 0= if HBB-FALSE exit then
   HBB-REPL-DEPS-PATH? 0= if HBB-FALSE exit then
   HBB-REPL-DEPS$ FS:FILE? 0= if HBB-FALSE exit then
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
   s" " NFILE-INPUTS:ADD-CTIME ;

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
   rec 64 HBB-REPL-SUPPORT-KEY 64 STR= 0= if E-NUNIT-PROFILE throw then
   rec 128 + LE:U64@ size HBB-REPL-SUPPORT-HEADER-U - <>
      if E-NUNIT-PROFILE throw then
   rec HBB-REPL-SUPPORT-HEADER-U +
      size HBB-REPL-SUPPORT-HEADER-U - NFILE-INPUTS:CHECK
   rec 64 + 64 APP-BUILD-IMAGE:CACHED-PATH$
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
   HBB-REPL-SUPPORT-PATH$ FS:FILE? 0= if HBB-FALSE exit then
   [: HBB-REPL-SUPPORT-CHECK ;] catch {: rc:n :}
   HBB-INPUTS-RELEASE
   rc 0= if
      HBB-REPL-PRODUCER-READY @ 0= if HBB-FALSE exit then
      HBB-TRUE exit
   then
   rc E-NUNIT-PROFILE = rc E-NUNIT-FILE = or if HBB-FALSE exit then
   rc throw ;

: HBB-REPL-SUPPORT-WRITE ( -- )
   HBB-REPL-PRODUCER-U @ {: rowsu:n :}
   HBB-REPL-SUPPORT-HEADER-U rowsu + HBB-REPL-SIDECAR-RESERVE
   0 HBB-REPL-SIDECAR {: rec:ptr :}
   HBB-REPL-SUPPORT-KEY rec 64 BYTE-COPY
   APP-BUILD-IMAGE:SELECTED-KEY$ drop rec 64 + 64 BYTE-COPY
   rowsu rec 128 + LE:U64!
   0 HBB-REPL-PRODUCER
      rec HBB-REPL-SUPPORT-HEADER-U + rowsu BYTE-COPY
   HBB-REPL-SUPPORT-PATH$ rec HBB-REPL-SUPPORT-HEADER-U rowsu +
      FS:ATOMIC-WRITE-FILE ;

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
   HBB-APP-STDIN$ >LEN
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
   s" HBRDEPS4" drop dst 8 BYTE-COPY
   srcu dst 8 + LE:U64!
   engu dst 16 + LE:U64!
   outu dst 24 + LE:U64!
   rootu dst 32 + LE:U64!
   HBB-TIER dst 40 + LE:U64!
   HBB-REPL-ENGINE-DIGEST dst 48 + 32 BYTE-COPY
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
   HBB-OUT$ s" " NFILE-INPUTS:ADD-CTIME
   HBB-REPL-DEPS$ NFILE-INPUTS:BYTES$ HBB-REPL-PACK-INPUTS FS:ATOMIC-WRITE-FILE ;

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
   HBB-REPL-DEPS$ FS:FILE? if HBB-REPL-DEPS$ FS:REMOVE-FILE then ;

: HBB-BUILD-REPL ( -- )
   HBB-REPL-ROOT!
   HBB-REPL-FRESH? if HBB-FINISH-REPL-FRESH exit then
   HBB-MAKER-INPUTS-CLEAR
   HBB-RUN-APP-CMD HBB-FINISH-MAKER
   HBB-REPL-INVALIDATE-DEPS
   HBB-INSTALL-OUT
   HBB-REPL-SAVE-INPUTS
   HBB-FINISH ;

: HBB-BUILD ( -- )
   HBB-BUILD-BEGIN
   HBB-ENGINE @ if HBB-BUILD-ENGINE exit then
   HBB-REPL @ if HBB-BUILD-REPL exit then
   HBB-PREPARE-AOT
   HBB-BUILD-REST ;

: HBB-PREPARE-OUT-CLI ( -- )
   [: HBB-REPL @ if HBB-PREPARE-OUT else HBB-PREPARE-AOT then ;]
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
   HBB-PREPARE-OUT-CLI
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
\ tools/hb-build.f runs the entry with the package closed.

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
