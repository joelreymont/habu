\ hb-build-lib.f - native AOT/REPL build CLI library, package HB-BUILD-CLI:
\ private but for the export block at the end. The name HB-BUILD belongs to the
\ report module, tools/hb-build-report.f.

require src/lib/error.f
require src/lib/string.f
require src/lib/fmt.f
require src/lib/memory.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/le.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/codesign.f
require src/lib/engine-id.f
require src/os/env-base.f
require src/lib/source.f
require src/lib/build.f
require src/lib/content-key.f
require src/lib/adt/option.f                 \ option<NUM:index> STR:INDEX-OF consumer
require src/lib/string-roles.f               \ package STR: the typed string surface
require tools/native-file-inputs.f
require src/lib/build-out.f
require src/habu/linker.f
require tools/build-support.f
require tools/hb-build-core.f
require tools/hb-build-report.f
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
\ ms per maker run, an engine build included: docs/gate.md "Qualifying a candidate"
300000 constant HBB-MAKER-DEFAULT-MS
$7FFFFFFF constant HBB-MAKER-MAX-MS
65536 constant HBB-CAPTURE-CAP
\ A support record: the selector key it was written under (64 hex), the
\ producer key it selects (64 hex), the rows length (u64), then the rows.
136 constant HBB-ENGINE-SUPPORT-HEADER-U

create HBB-SRC-PATH PATH-CAP allot
create HBB-OUT-PATH PATH-CAP allot
create HBB-MAP-SRC PATH-CAP allot
create HBB-MAP-DST PATH-CAP allot
create HBB-PAIR-STAGE PATH-CAP allot
create HBB-PAIR-BACKUP PATH-CAP allot
create HBB-COUNTS-PATH PATH-CAP allot
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
variable HBB-COUNTS-U
variable HBB-I
variable HBB-REPL
variable HBB-ENGINE
variable HBB-ENGINE-COMPAT
variable HBB-WHITEBOX
variable HBB-ENGINE-TARGET
variable HBB-JSON
variable HBB-REPORT-JSON
variable HBB-SIZE-REPORT
variable HBB-MAKER-HIT
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

\ Every build gets a private directory under the caller's TMPDIR: a build
\ writes a fixed name there (HBB-GOT-NAME$), so builds sharing one TMPDIR, as
\ gate pool builds do, would read each other's artifacts. FS:MKTEMP-DIR refuses
\ a base that is missing or not a directory.
: HBB-PREPARE-TMP ( -- )
   BF-TMP-RESET
   FS:CLEANUP-RESET
   s" hb-build-native" FS:MKTEMP-DIR 2dup BF-TMP! FS:CLEANUP-TREE+ ;

\ The out root is made before a build starts, so a root that cannot be made
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

: HBB-SUPPORT-TIMED-OUT ( -- )
   KEYED-IMAGE:CAPTURE-OUT$ {: out:ptr outu:n :}
   KEYED-IMAGE:CAPTURE-ERR$ {: err:ptr erru:n :}
   out HBB-OUT-BUF outu BYTE-COPY
   err HBB-ERR-BUF erru BYTE-COPY
   outu erru HBB-MAKER-TIMED-OUT ;

: HBB-FINISH-MAKER ( len len PROCESS:ending -- )
   MATCH PROCESS:ending
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

\ The program a build wrote under TMPDIR may be on another filesystem, so it is
\ copied; the copy keeps the team signature the product already carries.
: HBB-INSTALL-FILL ( ptr u8 n -- )
   {: tmp:ptr tmpu:n :}
   HBB-GOT-NAME$ BF-A$ tmp tmpu FS:COPY-FILE-STREAM
   tmp tmpu FS:CHMOD-X ;

: HBB-INSTALL-OUT ( -- )
   HBB-GOT-NAME$ BF-EXPECT
   HBB-GOT-NAME$ BF-A$ HBB-MAP-SRC!
   HBB-OUT$ HBB-MAP-DST!
   [: HBB-INSTALL-FILL ;] HBB-PAIR-RUN
   HBB-GOT-NAME$ BF-A$ HBB-MAP-REMOVE?
   HBB-GOT-NAME$ BF-A$ FS:REMOVE-FILE ;

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
      s"  (stripped program)"
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

\ A stripped program, or under --repl hb with the source's files, is linked in
\ this process (tools/hb-build-core.f) into TMP; under --json-errors the checker
\ prints its refusals as JSON.
: HBB-LINK-APP ( -- )
   HBB-GOT-NAME$ BF-REMOVE-TMP
   HBB-GOT-NAME$ BF-A$ HBB-MAP-REMOVE?
   HBB-JSON @ 0<> DIAG-JSON!
   HBB-SRC$ HBB-GOT-NAME$ BF-A$
   HBB-REPL @ if HB-BUILD-CORE:REPL else HB-BUILD-CORE:APP then
   LINKER:LINKED LINKER:COMPILED HB-BUILD:CAPTURE-FILES ;

\ A link that threw names the file it was loading, except under --json-errors,
\ whose stderr holds only JSON.
: HBB-APP-FAILED ( n -- )
   {: rc:n :}
   HBB-JSON @ 0= if
      s" hb-build: " HBB-WERR  HB-BUILD-CORE:ROW$ HBB-WERR  s"  threw " HBB-WERR
      SB-RESET  rc FMT:SB-INT  SB$ HBB-WERR
      rc ERROR:NAME$ {: name:ptr nameu:n :}
      nameu 0<> if s"  " HBB-WERR  name nameu HBB-WERR then
      HBB-WERR-LF
   then
   HBB-CLEANUP
   HBB-BUILD-RC HBB-EXIT ;

\ The engine child signs what it writes; LINKER:WRITE signs ad hoc, so a linked
\ program is signed for the team here (src/lib/codesign.f).
: HBB-BUILD-REST ( -- )
   ['] HBB-LINK-APP catch {: rc:n :}
   rc 0<> if rc HBB-APP-FAILED then
   HBB-GOT-NAME$ BF-A$ CODESIGN:ENSURE
   HBB-INSTALL-OUT
   HBB-FINISH ;

\ The engine build's support image is selected through
\ img/native-build-support.inputs, a support record as HBB-ENGINE-SUPPORT-HEADER-U
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

: HBB-ENGINE-CMD ( -- len len PROCESS:ending )
   ['] HBB-ENGINE-IMAGE! catch {: rc:n :}
   rc E-PROC-TIMEOUT = if HBB-SUPPORT-TIMED-OUT then
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

: HBB-BUILD ( -- )
   HBB-BUILD-BEGIN
   HBB-ENGINE @ if HBB-BUILD-ENGINE exit then
   HBB-PREPARE-OUT
   HBB-BUILD-REST ;

: HBB-PREPARE-OUT-CLI ( -- )
   ['] HBB-PREPARE-OUT catch {: rc:n :}
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
      HBB-ENGINE @ if PROCESS:TIMEOUT-RC HBB-EXIT then
      HBB-UNCAUGHT-RC HBB-EXIT
   then
   rc E-BUILD-INSTALL = if
      [: HBB-CLEANUP ;] catch {: clean:n :}
      clean 0<> if
         s" build cleanup failed" clean HBB-OUT$ HBB-PAIR-REPORT
      then
      s" hb-build: cannot replace output" HBB-BUILD-RC die
   then
   rc CODESIGN:E-IDENTITY = if
      HBB-CLEANUP
      s" hb-build: cannot sign output" HBB-BUILD-RC die
   then
   rc throw ;

: HBB-BUILD-CLI ( -- )
   HBB-BUILD-BEGIN
   HBB-ENGINE @ if [: HBB-BUILD-ENGINE ;] catch HBB-CLI-MAKER-CODE exit then
   HBB-PREPARE-OUT-CLI
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
EXPORT HBB-ENGINE-RUN-PATH
EXPORT HBB-PATHS!
EXPORT HBB-REPORT-JSON
EXPORT HBB-RESET-OPTIONS
EXPORT HBB-SRC$

;using
;package
