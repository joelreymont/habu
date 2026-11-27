\ hb-build-lib.f - hb-build's command line, package HB-BUILD-CLI:
\ private but for the export block at the end. The name HB-BUILD belongs to the
\ report module, tools/hb-build-report.f.

require src/lib/error.f
require src/lib/string.f
require src/lib/string-option.f
require src/lib/fmt.f
require src/lib/memory.f
require src/lib/heap.f
require src/lib/vector.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/at-exit.f
require src/lib/codesign.f
require src/lib/source.f
require src/lib/adt/option.f
require src/lib/build-out.f
require src/compiler/target/model.f
require src/habu/linker.f
require tools/hb-build-core.f
require tools/hb-build-report.f
require tools/image-size-lib.f
require tools/image-names.f
require src/core/exit-status.f
require src/compiler/target.f

package HB-BUILD-CLI

34 constant HBB-DQ
10 constant HBB-LF

create HBB-SRC-PATH PATH-CAP allot
create HBB-OUT-PATH PATH-CAP allot
create HBB-MAP-SRC PATH-CAP allot
create HBB-MAP-DST PATH-CAP allot
create HBB-GOT-PATH PATH-CAP allot
create HBB-LF-BUF 1 allot
HBB-LF HBB-LF-BUF c!

variable HBB-SRC-U
variable HBB-OUT-U
variable HBB-MAP-SRC-U
variable HBB-MAP-DST-U
variable HBB-GOT-U
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

: HBB-EXIT ( n -- )
   s" " swap die ;

: HBB-USAGE ( -- )
   s" usage: tools/hb-build.f [--engine | --repl | --json-errors] [--target name] [options] [source.f] -o out"
   EXIT-STATUS:USAGE die ;

: HBB-COPY-PATH! ( str ^u8 ^n -- ) {: s:str dst:^ up:^ :}
   s STR:LEN PATH-CAP > if E-BUILD-PATH throw then
   s STR:DATA dst s STR:LEN BYTE-COPY
   s STR:LEN up ! ;

: HBB-SRC! ( str -- )
   HBB-SRC-PATH HBB-SRC-U HBB-COPY-PATH! ;

: HBB-OUT! ( str -- )
   HBB-OUT-PATH HBB-OUT-U HBB-COPY-PATH! ;

\ The source path is held in PATH-CAP bytes, so the CLI rejects a longer
\ one by name, before it builds anything.
: HBB-SRC-ARG! ( str -- )
   {: s:str :}
   s STR:LEN PATH-CAP > if s" hb-build: source path too long" EXIT-STATUS:USAGE die then
   s HBB-SRC! ;

\ -o is replaced by renaming over it a sibling made in its directory
\ (src/lib/fs-mutate.f FS:REPLACE-STAGED). Before it builds anything the CLI
\ rejects by name an -o longer than PATH-CAP bytes, one with no last name,
\ one in a directory that is not there, and one that is a directory, which a
\ rename cannot replace. The host alone judges the last name's length. An -o
\ with no directory part goes in the working directory.
: HBB-OUT-ARG! ( str -- )
   {: s:str :}
   s STR:LEN PATH-CAP > if s" hb-build: output path too long" EXIT-STATUS:USAGE die then
   s FS:BASENAME STR:LEN {: nameu:n :}
   nameu 0= if s" hb-build: output path has no file name" EXIT-STATUS:USAGE die then
   s STR:LEN nameu - {: diru:n :}
   diru 0 > if
      s STR:DATA diru >str FS:DIR? 0= if
         s" hb-build: no such output directory" EXIT-STATUS:USAGE die
      then
   then
   s FS:DIR? if s" hb-build: output path is a directory" EXIT-STATUS:USAGE die then
   s HBB-OUT! ;

: HBB-SRC$ ( -- str )
   HBB-SRC-PATH HBB-SRC-U @ >str ;

: HBB-OUT$ ( -- str )
   HBB-OUT-PATH HBB-OUT-U @ >str ;

: HBB-MAP-SRC! ( str -- )
   HBB-MAP-SRC IMAGE-NAMES:MAP-PATH! 0= if E-BUILD-PATH throw then
   HBB-MAP-SRC-U ! ;

: HBB-MAP-DST! ( str -- )
   HBB-MAP-DST IMAGE-NAMES:MAP-PATH! 0= if E-BUILD-PATH throw then
   HBB-MAP-DST-U ! ;

: HBB-MAP-SRC$ ( -- str ) HBB-MAP-SRC HBB-MAP-SRC-U @ >str ;
: HBB-MAP-DST$ ( -- str ) HBB-MAP-DST HBB-MAP-DST-U @ >str ;

: HBB-MAP-FILL ( str -- ) HBB-MAP-SRC$ swap FS:COPY-FILE-STREAM ;

: HBB-WERR ( str -- ) {: s:str :}
   s STR:LEN 0= if exit then
   2 s write s STR:LEN <> if s" hb-build: stderr write failed" EXIT-STATUS:IOERR die then ;

: HBB-WERR-LF ( -- )
   HBB-LF-BUF 1 >str HBB-WERR ;

: HBB-PATH-HAS-DQ? ( str -- bool )
   HBB-DQ STR:INDEX-OF MATCH option
     none OF false ENDOF
     some OF drop true ENDOF
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

: HBB-ARG$ ( n -- str )
   SCRIPT-ARGV$ ;

: HBB-ARG= ( n str -- bool ) {: idx pat:str :}
   idx HBB-ARG$ pat STR:= ;

: HBB-TARGET-OPTION ( -- )
   HBB-INC-I
   HBB-I @ SCRIPT-ARGC >= if HBB-USAGE then
   HBB-I @ HBB-ARG$ {: arg:str :}
   arg RTARGET:PLATFORM? 0= if
      s" hb-build: unknown target " HBB-WERR  arg HBB-WERR  HBB-WERR-LF
      EXIT-STATUS:USAGE HBB-EXIT
   then
   arg RTARGET:RESOLVE HBB-TARGET !
   HBB-INC-I ;

: HBB-PARSE-OPTION? ( -- bool )
   HBB-I @ SCRIPT-ARGC >= if false exit then
   HBB-I @ s" --repl" HBB-ARG= if -1 HBB-REPL ! HBB-INC-I true exit then
   HBB-I @ s" --engine" HBB-ARG= if -1 HBB-ENGINE ! HBB-INC-I true exit then
   HBB-I @ s" --json-errors" HBB-ARG= if -1 HBB-JSON ! HBB-INC-I true exit then
   HBB-I @ s" --report-json" HBB-ARG= if -1 HBB-REPORT-JSON ! HBB-INC-I true exit then
   HBB-I @ s" --size-report" HBB-ARG= if -1 HBB-SIZE-REPORT ! HBB-INC-I true exit then
   HBB-I @ s" --target" HBB-ARG= if HBB-TARGET-OPTION true exit then
   HBB-I @ HBB-ARG$ s" --" STR:= if HBB-INC-I false exit then
   HBB-ENGINE @ if HBB-I @ s" -o" HBB-ARG= if false exit then then
   HBB-I @ HBB-ARG$ s" -" STR:STARTS-WITH? if HBB-USAGE then
   false ;

: HBB-PARSE-OPTIONS ( -- )
   begin HBB-PARSE-OPTION? while repeat ;

: HBB-PARSE ( -- )
   HBB-RESET-OPTIONS
   0 HBB-I !
   HBB-PARSE-OPTIONS
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
   HBB-SRC$ FS:FILE? 0= if s" hb-build: no such source" EXIT-STATUS:NOINPUT die then
   HBB-SRC$ HBB-PATH-HAS-DQ? if
      s" hb-build: source path contains a double quote" EXIT-STATUS:USAGE die
   then ;

: HBB-GOT-NAME$ ( -- str )
   HBB-ENGINE @ if s" hb-engine-got" exit then
   HBB-REPL @ if s" hb-build-got" else s" hb-aot-got" then ;

\ The program a build writes, with its .map beside it.
: HBB-GOT$ ( -- str )
   HBB-GOT-PATH HBB-GOT-U @ >str ;

\ Every build gets a private directory under the caller's TMPDIR: a build
\ writes a fixed name there (HBB-GOT-NAME$), so builds sharing one TMPDIR, as
\ gate pool builds do, would read each other's artifacts. HBB-CLEANUP removes
\ the directory whole. FS:MKTEMP-DIR rejects a base that is missing or not a
\ directory. The directory is made when the link writes, so a build that ends
\ before then leaves nothing in TMPDIR.
: HBB-PREPARE-TMP ( -- )
   s" hb-build-native" FS:MKTEMP-DIR dup FS:CLEANUP-TREE+ {: dir:str :}
   HBB-GOT-NAME$ {: name:str :}
   dir STR:LEN 1 + name STR:LEN + PATH-CAP > if E-BUILD-PATH throw then
   dir name HBB-GOT-PATH FS:JOIN-PATH HBB-GOT-U ! ;

\ A TMPDIR that cannot hold the build's directory fails before any file loads.
: HBB-TMP-PROBE ( -- )
   s" hb-build-native" FS:MKTEMP-DIR FS:REMOVE-DIR ;

\ The out root is made before a build starts, so a root that cannot be made
\ fails the build up front as a path error rather than midway as a file error.
: HBB-PREPARE-OUT ( -- )
   [: BUILD-OUT:BASE$ drop ;] catch {: rc:n :}
   rc 0= if exit then
   rc E-FS-IO = rc E-FS-CAPACITY = or if E-BUILD-PATH throw then
   rc throw ;

: HBB-CLEANUP ( -- )
   FS:CLEANUP-RUN ;

\ A replace's E-FS-OPEN or E-FS-IO, before the rename, is E-BUILD-INSTALL, the
\ file kept.
: HBB-REPLACE-ERROR ( n -- )
   {: rc:n :}
   rc 0= if exit then
   rc E-FS-OPEN = rc E-FS-IO = or if E-BUILD-INSTALL throw then
   rc throw ;

: HBB-REPLACE ( str [ str -- ] -- )
   {: s:str fill :}
   [: s fill FS:REPLACE-STAGED ;] catch HBB-REPLACE-ERROR ;

\ The program a build wrote under TMPDIR may be on another filesystem, so it is
\ copied; the copy keeps the team signature the product already carries.
: HBB-INSTALL-FILL ( str -- )
   {: tmp:str :}
   HBB-GOT$ tmp FS:COPY-FILE-STREAM
   tmp FS:CHMOD-X ;

TYPED-VARIABLE HBB-BUILT ^VEC:vector<u8>  \ a built file's bytes while HBB-SAME-PAIR compares them
TYPED-VARIABLE HBB-HELD ^VEC:vector<u8>   \ the installed file's bytes beside them
TYPED-VARIABLE HBB-SAME-OUT bool

: HBB-BUFS-NEW ( -- )
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW HBB-BUILT !
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW HBB-HELD ! ;

\ Either cell may hold NULL-PTR when HBB-BUFS-NEW threw part way.
: HBB-BUFS-DELETE ( -- )
   HBB-BUILT @ VEC:DELETE  NULL-PTR HBB-BUILT !
   HBB-HELD @ VEC:DELETE  NULL-PTR HBB-HELD ! ;

\ The vector holds the file whole, empty when it is missing.
: HBB-READ ( str ^VEC:vector<u8> -- )
   {: s:str v:^ :}
   v 0 VEC:RESIZE
   s FS:OPEN-SIZED {: file:fd size:n found:bool :}
   found 0= if exit then
   file v size FS:READ-SIZED ;

\ True when the built file and the installed one, a regular file and no
\ symlink, compare equal by the quotation; HBB-BUFS-NEW made the vectors.
: HBB-SAME-FILE? ( str str [ str str -- bool ] -- bool )
   {: s:str b:str same :}
   b FS:FILE? 0= b FS:SYMLINK? or if false exit then
   s HBB-BUILT @ HBB-READ  b HBB-HELD @ HBB-READ
   HBB-BUILT @ VEC:BYTES HBB-HELD @ VEC:BYTES same execute ;

\ The ad hoc build and a team-signed -o hold the same code whatever signed
\ them; the team check runs codesign, so it goes last.
: HBB-SAME-PAIR ( -- )
   HBB-GOT$ HBB-OUT$ [: CODESIGN:SAME-CODE? ;] HBB-SAME-FILE? 0= if exit then
   HBB-MAP-SRC$ HBB-MAP-DST$ [: STR:= ;] HBB-SAME-FILE? 0= if exit then
   HBB-OUT$ CODESIGN:TEAM-SIGNED? HBB-SAME-OUT ! ;

\ True when -o is team-signed and it and its map hold what the build made, so the
\ install leaves both untouched; an unreadable -o counts as differing.
: HBB-SAME-OUT? ( -- bool )
   false HBB-SAME-OUT !
   [: HBB-BUFS-NEW HBB-SAME-PAIR ;] catch {: rc:n :}
   HBB-BUFS-DELETE
   rc 0= HBB-SAME-OUT @ and ;

\ A Wasm module is copied with no execute bit, and no map is written beside it.
: HBB-MODULE-FILL ( str -- )
   {: tmp:str :}
   HBB-GOT$ tmp FS:COPY-FILE-STREAM ;

: HBB-SAME-MODULE ( -- )
   HBB-GOT$ HBB-OUT$ [: STR:= ;] HBB-SAME-FILE? HBB-SAME-OUT ! ;

\ True when -o already holds the module's bytes, as HBB-SAME-OUT? tells a program's.
: HBB-SAME-MODULE? ( -- bool )
   false HBB-SAME-OUT !
   [: HBB-BUFS-NEW HBB-SAME-MODULE ;] catch {: rc:n :}
   HBB-BUFS-DELETE
   rc 0= HBB-SAME-OUT @ and ;

: HBB-INSTALL-MODULE ( -- )
   HBB-SAME-MODULE? if exit then
   HBB-OUT$ [: HBB-MODULE-FILL ;] HBB-REPLACE ;

\ Only a program that replaces -o is team-signed, under TMPDIR before it is
\ staged: an unchanged build signs nothing.
: HBB-INSTALL-OUT ( -- )
   HBB-GOT$ FS:FILE? 0= if E-BUILD-PATH throw then
   HBB-WASM? if HBB-INSTALL-MODULE exit then
   HBB-GOT$ HBB-MAP-SRC!
   HBB-OUT$ HBB-MAP-DST!
   HBB-SAME-OUT? if exit then
   HBB-MAP-SRC$ FS:FILE? 0= if E-BUILD-PATH throw then
   HBB-GOT$ CODESIGN:ENSURE
   \ The map is in -o's directory, whose sync after -o's rename covers it.
   [: HBB-MAP-DST$ [: HBB-MAP-FILL ;] FS:REPLACE-CACHED ;] catch
   HBB-REPLACE-ERROR
   HBB-OUT$ [: HBB-INSTALL-FILL ;] HBB-REPLACE ;

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
\ the checker prints its rejections as JSON.
: HBB-LINK ( -- )
   HBB-JSON @ 0<> REJECTION:JSON !
   HBB-ENGINE @ if
      [: HBB-PREPARE-TMP HBB-GOT$ ;] HBB-TARGET @ HB-BUILD-CORE:ENGINE
   else
      HBB-SRC$ [: HBB-PREPARE-TMP HBB-GOT$ ;] HBB-TARGET @
      HBB-REPL @ if HB-BUILD-CORE:REPL else HB-BUILD-CORE:APP then
   then
   LINKER:LINKED LINKER:COMPILED HB-BUILD:CAPTURE-FILES ;

\ A status in 1..255 exits as itself. Any other throw prints the record it carries, else nothing once the link
\ sent a rejection, whose record is out, else a line naming where the build was.
: HBB-LINK-FAILED ( n bool -- )
   {: rc:n sent:bool :}
   rc 1 >= rc 255 <= and if HBB-CLEANUP rc HBB-EXIT then
   REJECTION:CARRIED? if
      rc ERROR:UNCAUGHT  HBB-CLEANUP  rc ERROR:STATUS HBB-EXIT
   then
   sent if HBB-CLEANUP rc ERROR:STATUS HBB-EXIT then
   HBB-JSON @ 0= if
      s" hb-build: " HBB-WERR  HB-BUILD-CORE:WHERE$ HBB-WERR
      s"  threw " HBB-WERR
      SB-RESET  rc FMT:SB-INT  SB$ HBB-WERR
      rc ERROR:NAME$ {: name:str :}
      name STR:LEN 0<> if s"  " HBB-WERR  name HBB-WERR then
      HBB-WERR-LF
   then
   HBB-CLEANUP
   rc ERROR:STATUS HBB-EXIT ;

\ Exit action in a build: names where, keeps -o as it was (not --json-errors).
: HBB-EXITED ( -- )
   LINKER:OPEN? 0= if exit then
   HBB-JSON @ 0= if
      s" hb-build: " HBB-WERR  HB-BUILD-CORE:WHERE$ HBB-WERR
      s"  ended the process during the build" HBB-WERR
      HBB-WERR-LF
   then
   EXIT-STATUS:REJECTED HBB-EXIT ;

: HBB-BUILD-REST ( -- )
   REJECTION:SENT {: sent:n :}
   ['] HBB-LINK catch {: rc:n :}
   rc 0<> if rc  REJECTION:SENT sent <>  HBB-LINK-FAILED then
   HBB-INSTALL-OUT
   HBB-FINISH ;

: HBB-PREPARE-OUT-CLI ( -- )
   ['] HBB-PREPARE-OUT catch {: rc:n :}
   rc 0= if exit then
   rc E-BUILD-PATH = if
      HBB-CLEANUP
      HBB-PATH-ERROR
      EXIT-STATUS:IOERR HBB-EXIT
   then
   rc throw ;

: HBB-CLI-CODE ( n -- ) {: rc:n :}
   rc 0= if exit then
   rc E-BUILD-INSTALL = if
      HBB-CLEANUP
      s" hb-build: cannot replace output" EXIT-STATUS:IOERR die
   then
   rc E-FS-SYNC = if
      HBB-CLEANUP
      s" hb-build: output replaced, its directory not synced" EXIT-STATUS:IOERR die
   then
   rc CODESIGN:E-IDENTITY = if
      HBB-CLEANUP
      s" hb-build: cannot sign output" EXIT-STATUS:UNAVAILABLE die
   then
   rc throw ;

: HBB-BUILD-CLI ( -- )
   HBB-BUILD-BEGIN
   HBB-PREPARE-OUT-CLI
   [: HBB-BUILD-REST ;] catch HBB-CLI-CODE ;

: HBB-MAIN ( -- )
   HBB-PARSE
   [: HBB-EXITED ;] AT-EXIT:ADD
   FS:CLEANUP-RESET HBB-TMP-PROBE
   HBB-BUILD-CLI
   HBB-CLEANUP ;

\ ---------------------------------------------------------------------------
\ tools/hb-build.f runs the entry with the package closed.

public

EXPORT HBB-MAIN

;package
