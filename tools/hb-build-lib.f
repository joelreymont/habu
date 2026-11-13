\ hb-build-lib.f - hb-build's command line, package HB-BUILD-CLI:
\ private but for the export block at the end. The name HB-BUILD belongs to the
\ report module, tools/hb-build-report.f.

require src/lib/error.f
require src/lib/string.f
require src/lib/fmt.f
require src/lib/memory.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/at-exit.f
require src/lib/codesign.f
require src/lib/source.f
require src/lib/build.f
require src/lib/adt/option.f                 \ option<NUM:index> STR:INDEX-OF consumer
require src/lib/string-roles.f               \ package STR: the typed string surface
require src/lib/build-out.f
require src/compiler/target/model.f
require src/habu/linker.f
require tools/hb-build-core.f
require tools/hb-build-report.f
require tools/image-size-lib.f

package HB-BUILD-CLI

64 constant HBB-USAGE-RC
66 constant HBB-NOINPUT-RC
74 constant HBB-BUILD-RC
34 constant HBB-DQ
10 constant HBB-LF

create HBB-SRC-PATH PATH-CAP allot
create HBB-OUT-PATH PATH-CAP allot
create HBB-MAP-SRC PATH-CAP allot
create HBB-MAP-DST PATH-CAP allot
create HBB-PAIR-STAGE PATH-CAP allot
create HBB-PAIR-BACKUP PATH-CAP allot
create HBB-GOT-PATH PATH-CAP allot
create HBB-LF-BUF 1 allot
HBB-LF HBB-LF-BUF c!

variable HBB-SRC-U
variable HBB-OUT-U
variable HBB-MAP-SRC-U
variable HBB-MAP-DST-U
variable HBB-PAIR-STAGE-U
variable HBB-PAIR-BACKUP-U
variable HBB-GOT-U
variable HBB-PAIR-HAD-MAP
variable HBB-PAIR-MOVED
variable HBB-PAIR-COMMITTED
variable HBB-PAIR-KEEP-BACKUP
variable HBB-I
variable HBB-REPL
variable HBB-ENGINE
variable HBB-JSON
variable HBB-REPORT-JSON
variable HBB-SIZE-REPORT
TYPED-VARIABLE HBB-TARGET RTARGET:resolved-target
variable HBB-START-NS
variable HBB-ELAPSED-NS

: HBB-WASM? ( -- bool ) HBB-TARGET @ RTARGET:ARCH@ CTARGET-ARCH:WASM CTARGET-ARCH:EQ ;

: HBB-TRUE ( -- bool )
   0 0= ;

: HBB-FALSE ( -- bool )
   HBB-TRUE 0= ;

: HBB-EXIT ( n -- )
   s" " rot die ;

: HBB-USAGE ( -- )
   s" usage: tools/hb-build.f [--engine | --repl | --json-errors] [--target name] [options] [source.f] -o out" HBB-USAGE-RC die ;

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

: HBB-WERR ( ptr u8 n -- ) {: a:ptr u :}
   u 0= if exit then
   2 a u write u <> if s" hb-build: stderr write failed" HBB-BUILD-RC die then ;

: HBB-WERR-LF ( -- )
   HBB-LF-BUF 1 HBB-WERR ;

: HBB-PATH-HAS-DQ? ( ptr u8 n -- bool )
   STR:LENGTH HBB-DQ STR:INDEX-OF MATCH option
     none OF HBB-FALSE ENDOF
     some OF drop HBB-TRUE ENDOF
   ;MATCH ;

: HBB-RESET-OPTIONS ( -- )
   0 HBB-REPL !
   0 HBB-ENGINE !
   0 HBB-JSON !
   0 HBB-REPORT-JSON !
   0 HBB-SIZE-REPORT !
   RTARGET:HOST-TARGET HBB-TARGET ! ;

: HBB-INC-I ( -- )
   HBB-I @ 1+ HBB-I ! ;

: HBB-ARG$ ( n -- ptr u8 n )
   SCRIPT-ARGV$ ;

: HBB-ARG= ( n ptr u8 n -- bool ) {: idx pat:ptr patu :}
   idx HBB-ARG$ pat patu STR= ;

: HBB-TARGET-OPTION ( -- )
   HBB-INC-I
   HBB-I @ SCRIPT-ARGC >= if HBB-USAGE then
   HBB-I @ HBB-ARG$ {: a:ptr u:n :}
   a u RTARGET:PLATFORM? 0= if
      s" hb-build: unknown target " HBB-WERR  a u HBB-WERR  HBB-WERR-LF
      HBB-USAGE-RC HBB-EXIT
   then
   a u RTARGET:RESOLVE HBB-TARGET !
   HBB-INC-I ;

: HBB-PARSE-OPTION? ( -- bool )
   HBB-I @ SCRIPT-ARGC >= if HBB-FALSE exit then
   HBB-I @ s" --repl" HBB-ARG= if -1 HBB-REPL ! HBB-INC-I HBB-TRUE exit then
   HBB-I @ s" --engine" HBB-ARG= if -1 HBB-ENGINE ! HBB-INC-I HBB-TRUE exit then
   HBB-I @ s" --json-errors" HBB-ARG= if -1 HBB-JSON ! HBB-INC-I HBB-TRUE exit then
   HBB-I @ s" --report-json" HBB-ARG= if -1 HBB-REPORT-JSON ! HBB-INC-I HBB-TRUE exit then
   HBB-I @ s" --size-report" HBB-ARG= if -1 HBB-SIZE-REPORT ! HBB-INC-I HBB-TRUE exit then
   HBB-I @ s" --target" HBB-ARG= if HBB-TARGET-OPTION HBB-TRUE exit then
   HBB-I @ HBB-ARG$ s" --" STR= if HBB-INC-I HBB-FALSE exit then
   HBB-ENGINE @ if HBB-I @ s" -o" HBB-ARG= if HBB-FALSE exit then then
   HBB-I @ HBB-ARG$ s" -" STARTS-WITH? if HBB-USAGE then
   HBB-FALSE ;

: HBB-PARSE-OPTIONS ( -- )
   begin HBB-PARSE-OPTION? while repeat ;

: HBB-PARSE ( -- )
   HBB-RESET-OPTIONS
   0 HBB-I !
   HBB-PARSE-OPTIONS
   HBB-ENGINE @ 0<> HBB-REPL @ 0<> or if HBB-TARGET @ HB-BUILD-CORE:REFUSE-WASM then
   HBB-ENGINE @ if
      HBB-REPL @ 0<> HBB-JSON @ 0<> or if HBB-USAGE then
      SCRIPT-ARGC HBB-I @ - 2 <> if HBB-USAGE then
      HBB-I @ s" -o" HBB-ARG= 0= if HBB-USAGE then
      HBB-I @ 1+ HBB-ARG$ HBB-OUT-ARG!
      exit
   then
   SCRIPT-ARGC HBB-I @ - 3 <> if HBB-USAGE then
   HBB-I @ HBB-ARG$ HBB-SRC-ARG!
   HBB-I @ 1+ s" -o" HBB-ARG= 0= if HBB-USAGE then
   HBB-I @ 2 + HBB-ARG$ HBB-OUT-ARG!
   HBB-SRC$ FS:FILE? 0= if s" hb-build: no such source" HBB-NOINPUT-RC die then
   HBB-SRC$ HBB-PATH-HAS-DQ? if s" hb-build: source path contains a double quote" HBB-USAGE-RC die then ;

: HBB-GOT-NAME$ ( -- ptr u8 n )
   HBB-ENGINE @ if s" hb-engine-got" exit then
   HBB-REPL @ if s" hb-build-got" else s" hb-aot-got" then ;

\ The program a build writes, with its .map beside it.
: HBB-GOT$ ( -- ptr u8 n )
   HBB-GOT-PATH HBB-GOT-U @ ;

\ Every build gets a private directory under the caller's TMPDIR: a build
\ writes a fixed name there (HBB-GOT-NAME$), so builds sharing one TMPDIR, as
\ gate pool builds do, would read each other's artifacts. HBB-CLEANUP removes
\ the directory whole. FS:MKTEMP-DIR refuses a base that is missing or not a
\ directory.
: HBB-PREPARE-TMP ( -- )
   FS:CLEANUP-RESET
   s" hb-build-native" FS:MKTEMP-DIR 2dup FS:CLEANUP-TREE+ {: dir:ptr diru:n :}
   HBB-GOT-NAME$ {: name:ptr nameu:n :}
   diru 1 + nameu + PATH-CAP > if E-BUILD-PATH throw then
   dir diru name nameu HBB-GOT-PATH FS:JOIN-PATH HBB-GOT-U ! ;

\ The out root is made before a build starts, so a root that cannot be made
\ fails the build up front as a path error rather than midway as a file error.
: HBB-PREPARE-OUT ( -- )
   [: BUILD-OUT:BASE$ 2drop ;] catch {: rc:n :}
   rc 0= if exit then
   rc E-FS-IO = rc E-FS-CAPACITY = or if E-BUILD-PATH throw then
   rc throw ;

: HBB-CLEANUP ( -- )
   FS:CLEANUP-RUN ;

\ Every file hb-build writes at -o is written to its sibling and renamed over
\ -o only once it is whole, a program executable (src/lib/fs-mutate.f FS:REPLACE-STAGED), so
\ a failed install leaves -o as it was and no sibling.
\ HBB-FILLING is set only while the fill runs, so HBB-REPLACE-OUT tells the
\ fill's own failure (its source, the bytes it writes) from the replace's: a
\ sibling the host will not make beside -o, or a rename it refuses, is an
\ output hb-build cannot replace, and the CLI names it (HBB-CLI-CODE).
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
   HBB-GOT$ tmp tmpu FS:COPY-FILE-STREAM
   tmp tmpu FS:CHMOD-X ;

DYNAMIC-BUFFER HBB-BUILT u8             \ a built file's bytes while HBB-SAME-PAIR compares them
DYNAMIC-BUFFER HBB-HELD u8              \ the installed file's bytes beside them
TYPED-VARIABLE HBB-SAME-OUT bool

\ The file read whole into the buffer the reserve and buffer words name; its
\ length, 0 when it is missing.
: HBB-READ ( ptr u8 n [ n -- ] [ n -- ptr u8 ] -- n )
   {: a:ptr u:n reserve buf :}
   a u FS:OPEN-SIZED {: file:fd size:n found:bool :}
   found 0= if 0 exit then
   size reserve execute
   file 0 buf execute size FS:READ-SIZED
   size ;

\ True when the built file and the installed one, a regular file and no
\ symlink, compare equal by the quotation. HBB-SAME-OUT? releases the buffers.
: HBB-SAME-FILE? ( ptr u8 n ptr u8 n [ ptr u8 n ptr u8 n -- bool ] -- bool )
   {: a:ptr u:n b:ptr bu:n same :}
   b bu FS:FILE? 0= b bu FS:SYMLINK? or if false exit then
   a u [: HBB-BUILT-RESERVE ;] [: HBB-BUILT ;] HBB-READ {: au:n :}
   b bu [: HBB-HELD-RESERVE ;] [: HBB-HELD ;] HBB-READ {: hu:n :}
   0 HBB-BUILT au 0 HBB-HELD hu same execute ;

\ The ad hoc build and a team-signed -o hold the same code whatever signed
\ them; the team check runs codesign, so it goes last.
: HBB-SAME-PAIR ( -- )
   HBB-GOT$ HBB-OUT$ [: CODESIGN:SAME-CODE? ;] HBB-SAME-FILE? 0= if exit then
   HBB-MAP-SRC$ HBB-MAP-DST$ [: STR= ;] HBB-SAME-FILE? 0= if exit then
   HBB-OUT$ CODESIGN:TEAM-SIGNED? HBB-SAME-OUT ! ;

\ True when -o is team-signed and it and its map hold what the build made, so the
\ install leaves both untouched; an unreadable -o counts as differing.
: HBB-SAME-OUT? ( -- bool )
   false HBB-SAME-OUT !
   [: HBB-SAME-PAIR ;] catch {: rc:n :}
   HBB-BUILT-RELEASE HBB-HELD-RELEASE
   rc 0= HBB-SAME-OUT @ and ;

\ A Wasm module is copied with no execute bit, and no map is written beside it.
: HBB-MODULE-FILL ( ptr u8 n -- )
   {: tmp:ptr tmpu:n :}
   HBB-GOT$ tmp tmpu FS:COPY-FILE-STREAM ;

: HBB-SAME-MODULE ( -- )
   HBB-GOT$ HBB-OUT$ [: STR= ;] HBB-SAME-FILE? HBB-SAME-OUT ! ;

\ True when -o already holds the module's bytes, as HBB-SAME-OUT? tells a program's.
: HBB-SAME-MODULE? ( -- bool )
   false HBB-SAME-OUT !
   [: HBB-SAME-MODULE ;] catch {: rc:n :}
   HBB-BUILT-RELEASE HBB-HELD-RELEASE
   rc 0= HBB-SAME-OUT @ and ;

: HBB-INSTALL-MODULE ( -- )
   HBB-SAME-MODULE? if exit then
   [: HBB-MODULE-FILL ;] HBB-REPLACE-OUT ;

\ Only a program that replaces -o is team-signed, under TMPDIR before it is
\ staged: an unchanged build signs nothing.
: HBB-INSTALL-OUT ( -- )
   HBB-GOT$ FS:FILE? 0= if E-BUILD-PATH throw then
   HBB-WASM? if HBB-INSTALL-MODULE exit then
   HBB-GOT$ HBB-MAP-SRC!
   HBB-OUT$ HBB-MAP-DST!
   HBB-SAME-OUT? if exit then
   HBB-GOT$ CODESIGN:ENSURE
   [: HBB-INSTALL-FILL ;] HBB-PAIR-RUN ;

: HBB-SUCCESS ( -- )
   s" hb-build OK: " type
   HBB-OUT$ type
   HBB-WASM? if
      s"  (Wasm module)"
   else HBB-ENGINE @ if
      s"  (engine)"
   else HBB-REPL @ if
      s"  (native application + REPL)"
   else
      s"  (stripped program)"
   then then then type
   cr ;

: HBB-MEASURE-OUT ( -- )
   HBB-OUT$ IMAGE-SIZE:MEASURE ;

: HBB-SIZE-LINE ( -- )
   s" hb-build " type
   HBB-OUT$ IMAGE-SIZE:.SUMMARY ;

: HBB-CAPTURE-REPORT ( -- )
   HBB-ELAPSED-NS @
   HB-BUILD:CAPTURE
   IMAGE-SIZE:TOTAL-BYTES IMAGE-SIZE:CODE-BYTES IMAGE-SIZE:NAME-BYTES
   IMAGE-SIZE:DATA-WRITTEN IMAGE-SIZE:DATA-ZERO IMAGE-SIZE:PAD-BYTES
   IMAGE-SIZE:OTHER-BYTES
   HB-BUILD:CAPTURE-SIZE ;

\ --report-json prints the report object alone, size numbers included, so its
\ stdout parses as JSON.
: HBB-REPORT ( -- )
   HBB-MEASURE-OUT
   HBB-CAPTURE-REPORT
   HBB-REPORT-JSON @ if HB-BUILD:REPORT$ type cr exit then
   HBB-SUCCESS
   HBB-SIZE-REPORT @ if IMAGE-SIZE:REPORT HBB-SIZE-LINE then ;

: HBB-FINISH ( -- )
   mono-ns HBB-START-NS @ - HBB-ELAPSED-NS !
   HBB-REPORT-JSON @ HBB-SIZE-REPORT @ or 0<> if HBB-REPORT else HBB-SUCCESS then ;

: HBB-PATH-ERROR ( -- )
   HBB-JSON @ if HB-BUILD:PATH-ERROR$ else HB-BUILD:PATH-ERROR-TEXT$ then HBB-WERR
   HBB-WERR-LF ;

: HBB-BUILD-BEGIN ( -- )
   mono-ns HBB-START-NS !
   HB-BUILD:RESET ;

\ hb, a stripped program, or under --repl hb with the source's files, is linked
\ in this process (tools/hb-build-core.f) into HBB-GOT$; under --json-errors
\ the checker prints its refusals as JSON.
: HBB-LINK ( -- )
   HBB-JSON @ 0<> DIAG-JSON!
   HBB-ENGINE @ if
      HBB-GOT$ HBB-TARGET @ HB-BUILD-CORE:ENGINE
   else
      HBB-SRC$ HBB-GOT$ HBB-TARGET @
      HBB-REPL @ if HB-BUILD-CORE:REPL else HB-BUILD-CORE:APP then
   then
   LINKER:LINKED LINKER:COMPILED HB-BUILD:CAPTURE-FILES ;

\ A link that threw names the file it was loading, except under --json-errors,
\ whose stderr holds only JSON.
: HBB-LINK-FAILED ( n -- )
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

\ hb-build's exit action: a process ended inside a build leaves no program and -o
\ as it was; the line names the file it was loading, if any (not --json-errors).
: HBB-EXITED ( -- )
   LINKER:OPEN? 0= if exit then
   HBB-JSON @ 0= if
      HB-BUILD-CORE:ROW$ {: row:ptr rowu:n :}
      rowu 0= if
         s" hb-build: the build ended the process before its first file" HBB-WERR
      else
         s" hb-build: " HBB-WERR  row rowu HBB-WERR
         s"  ended the process during the build" HBB-WERR
      then
      HBB-WERR-LF
   then
   HBB-BUILD-RC HBB-EXIT ;

: HBB-BUILD-REST ( -- )
   ['] HBB-LINK catch {: rc:n :}
   rc 0<> if rc HBB-LINK-FAILED then
   HBB-INSTALL-OUT
   HBB-FINISH ;

: HBB-PREPARE-OUT-CLI ( -- )
   ['] HBB-PREPARE-OUT catch {: rc:n :}
   rc 0= if exit then
   rc E-BUILD-PATH = if
      HBB-CLEANUP
      HBB-PATH-ERROR
      HBB-BUILD-RC HBB-EXIT
   then
   rc throw ;

: HBB-CLI-CODE ( n -- ) {: rc:n :}
   rc 0= if exit then
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
   HBB-PREPARE-OUT-CLI
   [: HBB-BUILD-REST ;] catch HBB-CLI-CODE ;

: HBB-MAIN ( -- )
   HBB-PARSE
   [: HBB-EXITED ;] AT-EXIT:ADD
   HBB-PREPARE-TMP
   HBB-BUILD-CLI
   HBB-CLEANUP ;

\ ---------------------------------------------------------------------------
\ tools/hb-build.f runs the entry with the package closed.

public

EXPORT HBB-MAIN

;package
