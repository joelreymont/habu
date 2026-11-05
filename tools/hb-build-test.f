\ hb-build-test.f - fixture for tools/hb-build-lib.f: the REPL build and its
\ report through the CLI, failed installs and pair publications, rejected inputs,
\ string-literal bodies against `create` cells, and image size classes.
\ tools/hb-build-test-lib.f lists the other hb-build rows.
\ Run: bin/hb --load tools/hb-build-test.f

require tools/hb-build-test-lib.f
require tools/image-size-lib.f          \ this fixture measures image bytes directly
require test/linker-builder-engine.f
require test/app-builder-engine.f
require src/lib/string.f
require src/lib/fmt.f                        \ FMT:.INT

using BUILD-FIXPOINT                     \ the build tmp root

\ The shared fixture's words are private words of the library's package, so
\ this row reopens it the way tools/hb-build-test-lib.f does.
package HB-BUILD-CLI

\ The REPL and AOT sources are padded with spaces to this many bytes, so both
\ builds read an input that large.
4096 constant HBT-LARGE-CHUNK-U
$40000 1 - constant HBT-LARGE-INPUT-U
create HBT-LARGE-CHUNK HBT-LARGE-CHUNK-U allot

\ The failed install's -o and the directory it sits in, and the files that
\ directory holds.
variable HBT-INST-DIR-U
variable HBT-INST-OUT-U
create HBT-INST-DIR-BUF PATH-CAP allot
create HBT-INST-OUT-BUF PATH-CAP allot
variable HBT-INST-FILES

: HBT-NEW-TMP ( -- ptr u8 n )
   HBT-NEW-TMP-BUF HBT-NEW-TMP-U @ ;

: HBT-WRITE-LARGE ( ptr u8 n ptr u8 n -- ) {: path:ptr pathu:n src:ptr srcu:n :}
   HBT-LARGE-CHUNK-U 0 ?do 32 HBT-LARGE-CHUNK i + c! loop
   path pathu src srcu FS:WRITE-ALL
   HBT-LARGE-INPUT-U srcu - {: spaces:n :}
   spaces HBT-LARGE-CHUNK-U / 0 ?do
      path pathu HBT-LARGE-CHUNK HBT-LARGE-CHUNK-U FS:APPEND-FILE
   loop
   spaces HBT-LARGE-CHUNK-U mod {: rem:n :}
   rem 0 > if path pathu HBT-LARGE-CHUNK rem FS:APPEND-FILE then ;

: HBT-ADD-BAD ( -- )
   s" --json-errors"  >LEN PROC-ARGV+
   HBT-BAD-SRC  >LEN PROC-ARGV+
   s" -o"  >LEN PROC-ARGV+
   HBT-BAD-OUT  >LEN PROC-ARGV+ ;

: HBT-ADD-REPORT ( -- )
   s" --repl" >LEN PROC-ARGV+
   s" --report-json" >LEN PROC-ARGV+
   HBT-REPL-SRC >LEN PROC-ARGV+
   s" -o" >LEN PROC-ARGV+
   HBT-REPL-OUT >LEN PROC-ARGV+ ;

\ Both report flags over stale bytes at -o: exit 0, empty stderr, the report object
\ (not the size table) on stdout; HBT-RUN-REPL then runs the image that replaced them.
: CLI-REPORT ( -- )
   HBT-REPL-OUT s" stale" FS:WRITE-ALL
   HBT-ARGV-BASE-REPL
   s" --size-report" >LEN PROC-ARGV+
   HBT-ADD-REPORT
   HBT-RUN-HB-BUILD {: outu:n erru:n rc:n :}
   rc 0 T=
   erru 0 T=
   HBT-OUT outu JSON:T-FALSE JSON:T-FALSE JSON:T-FALSE CHECK-REPORT ;

\ The install stages the engine in a sibling of -o and renames it over -o
\ (HBB-INSTALL-OUT). A directory at -o lets the sibling be filled and made
\ executable, then refuses the rename: the build throws E-BUILD-INSTALL, -o is still
\ the directory, and no file is left in the directory that holds it.
: HBT-INST-DIR ( -- ptr u8 n )
   HBT-INST-DIR-BUF HBT-INST-DIR-U @ ;

: HBT-INST-OUT ( -- ptr u8 n )
   HBT-INST-OUT-BUF HBT-INST-OUT-U @ ;

: HBT-INST-FILE ( ptr u8 n -- )
   2drop 1 HBT-INST-FILES +! ;

: HBT-INST-FILES@ ( -- n )
   0 HBT-INST-FILES !
   HBT-INST-DIR [: HBT-INST-FILE ;] FS:WALK-FILES
   HBT-INST-FILES @ ;

: HBT-INSTALL-FAIL ( -- )
   HBT-ROOT s" install" HBT-INST-DIR-BUF HBT-INST-DIR-U HBT-PATH!
   HBT-INST-DIR s" out" HBT-INST-OUT-BUF HBT-INST-OUT-U HBT-PATH!
   HBT-INST-DIR FS:MAKE-DIR
   HBT-INST-OUT FS:MAKE-DIR
   HBT-REPL-SRC HBT-INST-OUT HBT-HBB-PREPARE-REPL
   [: HBB-BUILD ;] E-BUILD-INSTALL TTHROWSQ
   HBB-GOT-NAME$ BF-REMOVE-TMP
   BF-TMP-RESET
   HBT-INST-OUT FS:DIR? TTRUE
   HBT-INST-FILES@ 0 T= ;

\ A failed pair publication leaves -o and its map as they were. An AOT install
\ publishes the maker's executable and map together (HBB-PAIR-RUN). The maker's
\ output is planted under TMPDIR: without its map the install refuses before
\ anything moves, unreadable it cannot be copied, and a directory at -o refuses
\ the executable's rename after the new map was staged. A sibling left behind
\ is an extra file beside -o and its map.
73 constant HBT-EXEC-ONLY                  \ 0111: runnable, unreadable
493 constant HBT-EXEC-READ                 \ 0755
16 constant HBT-PUB-CAP
create HBT-PUB-BUF HBT-PUB-CAP allot
create HBT-PUB-DIR-BUF PATH-CAP allot
create HBT-PUB-OUT-BUF PATH-CAP allot
variable HBT-PUB-DIR-U
variable HBT-PUB-OUT-U
variable HBT-PUB-FILES

: HBT-PUB-DIR ( -- ptr u8 n )
   HBT-PUB-DIR-BUF HBT-PUB-DIR-U @ ;

: HBT-PUB-OUT ( -- ptr u8 n )
   HBT-PUB-OUT-BUF HBT-PUB-OUT-U @ ;

: HBT-PUB-COUNT ( ptr u8 n -- )
   2drop HBT-PUB-FILES @ 1+ HBT-PUB-FILES ! ;

: HBT-PUB-ALONE ( n -- ) {: count:n :}
   0 HBT-PUB-FILES !
   HBT-PUB-DIR [: HBT-PUB-COUNT ;] FS:WALK-FILES
   HBT-PUB-FILES @ count T= ;

: HBT-PUB-HOLDS ( ptr u8 n -- )
   {: want:ptr wantu:n :}
   HBT-PUB-OUT FS:FILE? {: there:bool :}
   there TTRUE
   there 0= if exit then
   HBT-PUB-OUT HBT-PUB-BUF HBT-PUB-CAP FS:READ-ALL {: u:n :}
   HBT-PUB-BUF u want wantu T$= ;

: HBT-PUB-MAP-HOLDS ( ptr u8 n -- )
   {: want:ptr wantu:n :}
   HBT-PUB-OUT HBB-MAP-DST!
   HBB-MAP-DST$ HBT-PUB-BUF HBT-PUB-CAP FS:READ-ALL {: u:n :}
   HBT-PUB-BUF u want wantu T$= ;

: HBT-PUB-SET-MAKER ( -- )
   HBT-TMP BF-TMP!
   HBB-GOT-NAME$ BF-A$ s" source-image" FS:WRITE-ALL
   HBB-GOT-NAME$ BF-A$ HBB-MAP-SRC!
   HBB-MAP-SRC$ s" source-map" FS:WRITE-ALL ;

: HBT-PUB-PAIR-FAILS ( -- )
   HBT-ROOT s" publish" HBT-PUB-DIR-BUF HBT-PUB-DIR-U HBT-PATH!
   HBT-PUB-DIR s" out" HBT-PUB-OUT-BUF HBT-PUB-OUT-U HBT-PATH!
   HBT-PUB-DIR FS:MAKE-DIR
   HBT-PUB-OUT s" previous" FS:WRITE-ALL
   HBT-PUB-OUT HBB-MAP-DST!
   HBB-MAP-DST$ s" prior-map" FS:WRITE-ALL
   HBB-RESET-OPTIONS
   HBT-AOT-SRC HBT-PUB-OUT HBB-PATHS!
   HBT-PUB-SET-MAKER
   HBB-GOT-NAME$ BF-A$ HBB-MAP-REMOVE?
   [: HBB-INSTALL-OUT ;] E-BUILD-PATH TTHROWSQ
   s" previous" HBT-PUB-HOLDS
   s" prior-map" HBT-PUB-MAP-HOLDS
   2 HBT-PUB-ALONE
   HBT-PUB-SET-MAKER
   HBB-GOT-NAME$ BF-A$ HBT-EXEC-ONLY FS:CHMOD-MODE
   [: HBB-INSTALL-OUT ;] E-FS-OPEN TTHROWSQ
   HBB-GOT-NAME$ BF-A$ HBT-EXEC-READ FS:CHMOD-MODE
   s" previous" HBT-PUB-HOLDS
   s" prior-map" HBT-PUB-MAP-HOLDS
   2 HBT-PUB-ALONE
   HBT-PUB-OUT FS:REMOVE-FILE
   HBT-PUB-OUT FS:MAKE-DIR
   [: HBB-INSTALL-OUT ;] E-BUILD-INSTALL TTHROWSQ
   HBT-PUB-OUT FS:DIR? TTRUE
   s" prior-map" HBT-PUB-MAP-HOLDS
   1 HBT-PUB-ALONE
   HBB-MAP-DST$ FS:REMOVE-FILE
   HBT-PUB-OUT FS:REMOVE-DIR
   HBB-GOT-NAME$ BF-A$ HBB-MAP-REMOVE?
   HBB-GOT-NAME$ BF-A$ FS:REMOVE-FILE
   BF-TMP-RESET
   HBT-PUB-DIR FS:REMOVE-DIR ;

\ A build that fails invalidates the report the last one left: this runs after
\ HBT-STRIPPED-IMAGE, whose in-process build left a valid one, and builds the
\ same source again onto an -o that is a directory, which the install refuses.
\ A missing source cannot stand in: its include dies with exit 74 rather than
\ throwing.
PATH-CAP BUFFER: HBT-DIR-OUT-BUF
: HBT-REPORT-INVALIDATION ( -- )
   HB-BUILD:VALID? TTRUE
   HBT-ROOT s" dir-out" HBT-DIR-OUT-BUF FS:JOIN-PATH {: diru:n :}
   HBT-DIR-OUT-BUF diru FS:MAKE-DIR
   HBT-AOT-SRC HBT-DIR-OUT-BUF diru HBT-HBB-PREPARE-AOT
   [: HBB-BUILD ;] catch E-BUILD-INSTALL T=
   BF-TMP-RESET
   HB-BUILD:VALID? TFALSE
   [: HB-BUILD:MAKER-HIT? drop ;] catch E-BUILD-STATUS T= ;

: HBT-REPL-ARGS-EXPECTED$ ( -- ptr u8 n )
   SB-RESET
   s" 10" SB-APPEND
   HBB-LF SB-APPEND-C
   HBB-LF SB-APPEND-C
   s" 81" SB-APPEND
   HBB-LF SB-APPEND-C
   HBB-LF SB-APPEND-C
   s" 2" SB-APPEND
   HBB-LF SB-APPEND-C
   HBB-LF SB-APPEND-C
   s" alpha" SB-APPEND
   HBB-LF SB-APPEND-C
   SB$ ;

: HBT-RUN-REPL-ARGS ( -- )
   PROC-ARGV-ENV-RESET
   s" alpha"  >LEN PROC-ARGV+
   s" beta"  >LEN PROC-ARGV+
   PROC-ENV-INHERIT-MISSING
   HBT-REPL-OUT >LEN HBT-RUN-OUT HBT-CAPTURE-CAP >LEN HBT-RUN-ERR HBT-CAPTURE-CAP >LEN
   HBT-TIMEOUT-MS >MS RUN-ARGV-ENV-CAPTURE HBT-CAPTURE>N {: outn:n errn:n rcn:n :}
   rcn 0 <> if s" repl args rc: " type rcn FMT:.INT cr HBT-RUN-OUT outn type HBT-RUN-ERR errn type then
   rcn 0 T=
   HBT-RUN-ERR errn HBT-EMPTY$ T$=
   HBT-RUN-OUT outn HBT-REPL-ARGS-EXPECTED$ T-STR= 0= if
      s" repl args stdout: " type HBT-RUN-OUT outn type cr
      s" actual len: " type outn FMT:.INT cr
      s" expect len: " type HBT-REPL-ARGS-EXPECTED$ nip FMT:.INT cr
   then
   HBT-RUN-OUT outn HBT-REPL-ARGS-EXPECTED$ T$= ;

\ ---- where each image class's bytes went --------------------------------------
\ IMAGE-SIZE:MEASURE refuses unless the classes sum to the file's length; this
\ pins that sum to the real length and to the summary's five terms plus `other`.
: HBT-SIZE-SUM ( -- n )
   IMAGE-SIZE:CODE-BYTES IMAGE-SIZE:NAME-BYTES +
   IMAGE-SIZE:DATA-WRITTEN + IMAGE-SIZE:DATA-ZERO +
   IMAGE-SIZE:PAD-BYTES + IMAGE-SIZE:OTHER-BYTES + ;

: HBT-SIZE-MEASURE ( ptr u8 n -- ) {: a:ptr u:n :}
   a u IMAGE-SIZE:MEASURE
   IMAGE-SIZE:TOTAL-BYTES  a u FS:FILE-SIZE T=
   HBT-SIZE-SUM  IMAGE-SIZE:TOTAL-BYTES T= ;

\ Reads the image file rather than a report about it.
: HBT-SIZE-AOT ( -- )
   HBT-AOT-OUT HBT-SIZE-MEASURE
   IMAGE-SIZE:CLASS$ s" stripped" T$=
   \ A stripped image encodes its window as non-zero runs, so not one zero
   \ byte of it travels.
   IMAGE-SIZE:DATA-ZERO 0 T=
   IMAGE-SIZE:DATA-WRITTEN 0 > TTRUE
   IMAGE-SIZE:CODE-BYTES 0 > TTRUE
   \ ... and it carries no dictionary at all.
   IMAGE-SIZE:NAME-BYTES 0 T=
   \ No region either, and no DATA owner.
   IMAGE-SIZE:REGION-CODE 0 T=
   IMAGE-SIZE:REGION-NAMES 0 T=
   IMAGE-SIZE:REGION-UNOWNED 0 T=
   IMAGE-SIZE:DATA-OWNERS 0 T= ;

\ The stripped image's program: a nonzero DATA byte MAIN prints, so its output
\ proves MAIN ran and read the restored byte.
: HBT-IMAGE-SRC$ ( -- ptr u8 n )
   s" create HBT-DATA 41 c, : MAIN ( -- ) HBT-DATA c@ . cr ;" ;

\ The requirement a team-signed AOT product meets: the AOT maker's identifier
\ (src/habu/sign-id.f SIGN-ID:PROG$) and a certificate of team 8TW82D98K7.
: HBT-TEAM-REQ ( -- ptr u8 n )
   s\" =anchor apple generic and identifier \"hb-prog\" and certificate leaf[subject.OU] = \"8TW82D98K7\"" ;

\ codesign -v -R exits nonzero for any other signer, ad hoc included.
: HBT-EXPECT-TEAM ( ptr u8 n -- ) {: a:ptr u:n :}
   HB-TARGET-MACOS? 0= if exit then
   PROC-ARGV-RESET
   s" -v" >LEN PROC-ARGV+
   s" -R" >LEN PROC-ARGV+
   HBT-TEAM-REQ >LEN PROC-ARGV+
   a u >LEN PROC-ARGV+
   CODESIGN:TOOL >LEN -1 >FD -1 >FD -1 >FD PROC-RUN-ARGV-IO-RC
   MATCH result ok OF ENDOF err OF ENDOF ;MATCH 0 T= ;

\ One stripped image, built from a padded source, runs and is measured.
: HBT-STRIPPED-IMAGE ( -- )
   HBT-AOT-SRC HBT-IMAGE-SRC$ HBT-WRITE-LARGE
   HBT-REMOVE-AOT-OUT
   HBT-AOT-SRC HBT-AOT-OUT HBT-HBB-PREPARE-AOT
   -1 HBB-REPORT-JSON !
   HBB-BUILD
   HBT-AOT-OUT >LEN HBT-RUN-OUT HBT-CAPTURE-CAP >LEN HBT-RUN-ERR HBT-CAPTURE-CAP >LEN
   HBT-TIMEOUT-MS >MS RUN-CAPTURE HBT-CAPTURE>N {: outn:n errn:n rcn:n :}
   rcn 0 T=
   errn 0 T=
   HBT-RUN-OUT outn S\" 41\n\n" T$=
   HBT-AOT-OUT HBT-EXPECT-TEAM
   HBT-SIZE-AOT
   HBT-REMOVE-AOT-OUT
   BF-TMP-RESET ;

: HBT-LITB-SRC ( -- ptr u8 n )
   HBT-LITB-SRC-BUF HBT-LITB-SRC-U @ ;

: HBT-LITB-OUT ( -- ptr u8 n )
   HBT-LITB-OUT-BUF HBT-LITB-OUT-U @ ;

: HBT-LITC-SRC ( -- ptr u8 n )
   HBT-LITC-SRC-BUF HBT-LITC-SRC-U @ ;

\ A string literal's body is declared text: src/habu/intern.f writes
\ one row per interned body, and src/habu/aot-closure.f CELL-LITERAL? reads those
\ rows, never the bytes, for every cell src/habu/aot-common.f SCAN-DATA-CELL would
\ refuse. Tender's stripped server is the case: `s" {pm}"` and the 01 byte after
\ it, read as one aligned cell, spell an address inside the emitted code span.
\ The child derives its bytes from a live code address: a fixed Linux address
\ does not hold in a Mach-O builder with ASLR.
: HBT-LITB-SRC$ ( -- ptr u8 n )
   S\" require test/stripped-literal-subject.f\n' STRIPPED-LITERAL:ANCHOR STRIPPED-LITERAL:BODY-SOURCE evaluate\n" ;

\ MAIN verifies the length and bytes before printing this witness.
: HBT-LITB-EXPECTED$ ( -- ptr u8 n )
   S\" literal\n" ;

\ The same eight bytes in a `create` cell are refused: raw storage declares
\ nothing, so the value is the code pointer it spells. If the pattern drifted
\ out of the code span this row goes red instead of both passing.
: HBT-LITC-SRC$ ( -- ptr u8 n )
   S\" require test/stripped-literal-subject.f\n' STRIPPED-LITERAL:ANCHOR STRIPPED-LITERAL:RAW\n: MAIN ( -- ) STRIPPED-LITERAL:VALUE . cr ;\n" ;

\ The control runs first: its refusal proves the pattern lands in this build's
\ code span, so the link that follows is about the declaration. The subject
\ requires src/lib/codegen.f, which the engine bakes, and neither program reaches
\ the linker's load, so both link on the image (test/preloaded-engine.f rule 3).
: HBT-STRIPPED-LITERAL-BODY ( -- )
   HBT-LITC-SRC HBT-LITC-SRC$ FS:WRITE-ALL
   HBT-LITC-SRC HBT-RUN-MAKER {: cout:n cerr:n crc:n :}
   crc 70 T=
   HBB-ERR-BUF cerr s" holds an undeclared code/dict pointer" CONTAINS? TTRUE
   HBB-ERR-BUF cerr s" word=LIT-CELL" CONTAINS? TTRUE

   HBT-LITB-SRC HBT-LITB-SRC$ FS:WRITE-ALL
   HBT-LITB-OUT HBT-REMOVE-FILE?
   HBT-LITB-SRC HBT-LITB-OUT HBT-HBB-PREPARE-AOT HBT-HBB-BUILD-OUT
   HBT-LITB-OUT FS:FILE? TTRUE
   HBT-LITB-OUT >LEN HBT-RUN-OUT HBT-CAPTURE-CAP >LEN HBT-RUN-ERR HBT-CAPTURE-CAP >LEN
   HBT-TIMEOUT-MS >MS RUN-CAPTURE HBT-CAPTURE>N {: outn:n errn:n rcn:n :}
   rcn 0 <> if HBT-RUN-OUT outn type HBT-RUN-ERR errn type then
   rcn 0 T=
   errn 0 T=
   HBT-RUN-OUT outn HBT-LITB-EXPECTED$ T$=
   HBT-LITB-OUT HBT-REMOVE-FILE? ;

\ A program that prints more than HBB-CAPTURE-CAP bytes while it compiles: a
\ fixed capture fails closed when full (E-PROC-TRUNCATED), so the maker's stdout
\ capture grows. It prints HBT-LOUD-LINES lines of HBT-LOUD-LINE-U bytes, one
\ line past the cap, then HBT-LOUD-LAST$, the line a short capture loses.
64 constant HBT-LOUD-LINE-U
HBB-CAPTURE-CAP HBT-LOUD-LINE-U / 1 + constant HBT-LOUD-LINES
variable HBT-LOUD-SRC-U
create HBT-LOUD-SRC-BUF PATH-CAP allot

: HBT-LOUD-SRC ( -- ptr u8 n )
   HBT-LOUD-SRC-BUF HBT-LOUD-SRC-U @ ;

: HBT-LOUD-LAST$ ( -- ptr u8 n )
   s" hbt-loud-last" ;

: HBT-LOUD-SRC$ ( -- ptr u8 n )
   SB-RESET
   s" : LOUD ( -- ) " SB-APPEND
   HBT-LOUD-LINES FMT:SB-U
   S\"  0 ?do s\" " SB-APPEND
   HBT-LOUD-LINE-U 1 - 0 ?do s" x" SB-APPEND loop
   S\" \" type cr loop ;\nLOUD\n: LOUD-LAST ( -- ) s\" " SB-APPEND
   HBT-LOUD-LAST$ SB-APPEND
   S\" \" type cr ;\nLOUD-LAST\n: MAIN ( -- ) ;\n" SB-APPEND
   SB$ ;

: HBT-LOUD-MAKER ( -- )
   HBT-ROOT s" loud.f" HBT-LOUD-SRC-BUF HBT-LOUD-SRC-U HBT-PATH!
   HBT-LOUD-SRC HBT-LOUD-SRC$ FS:WRITE-ALL
   HBT-LOUD-SRC HBT-RUN-MAKER {: outu:n erru:n rc:n :}
   rc 0 <> if erru HBB-ERR-ROOM erru type then
   rc 0 T=
   outu HBB-CAPTURE-CAP > TTRUE
   outu HBB-OUT-ROOM outu HBT-LOUD-LAST$ CONTAINS? TTRUE ;

\ Rejected input is checked in the real app-build child, which refuses it with
\ the checker's code and diagnostic.
: HBT-BUILD-REPL-BAD ( -- )
   HBT-REPL-BAD-SRC HBT-RUN-APP {: outu:n erru:n rc:n :}
   rc 70 T=
   outu 0 T=
   HBB-ERR-BUF erru s" expected: i64" CONTAINS? TTRUE
   HBB-ERR-BUF erru s" actual: bool" CONTAINS? TTRUE ;

\ The line FS:MKTEMP-DIR prints for a TMPDIR that is no directory.
: HBT-NOT-DIR-LINE$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   SB-RESET
   s" hb: TMPDIR is not a directory: " SB-APPEND
   a u SB-APPEND
   S\" \n" SB-APPEND
   SB$ ;

\ The line hb prints for a throw nothing caught.
: HBT-UNCAUGHT$ ( n -- ptr u8 n ) {: code:n :}
   SB-RESET
   s" hb: uncaught throw code " SB-APPEND
   code FMT:SB-INT
   SB$ ;

\ A missing TMPDIR is refused by name before the build starts, and nothing is
\ made: not the directory, not the image. With a usable TMPDIR the same program
\ reaches the lint child, whose refusal the CLI passes through.
: HBT-BUILD-MISSING-TMP ( -- )
   HBT-NEW-TMP FS:EXISTS? TFALSE
   HBT-NEW-TMP HBT-ARGV-BASE-TMP
   HBT-ADD-BAD
   HBT-RUN-HB-BUILD 0 T<>
   {: outu erru :}
   HBT-OUT outu HBT-EMPTY$ T$=
   HBT-ERR erru HBT-NEW-TMP HBT-NOT-DIR-LINE$ CONTAINS? TTRUE
   HBT-ERR erru E-FS-DIR HBT-UNCAUGHT$ CONTAINS? TTRUE
   HBT-NEW-TMP FS:EXISTS? TFALSE
   HBT-BAD-OUT FS:EXISTS? TFALSE
   HBT-ARGV-BASE
   HBT-ADD-BAD
   HBT-RUN-HB-BUILD 0 T<>
   {: lintoutu linterru :}
   HBT-OUT lintoutu HBT-EMPTY$ T$=
   HBT-ERR linterru s" E-AOT-UNSUPPORTED" CONTAINS? TTRUE
   HBT-BAD-OUT FS:EXISTS? TFALSE ;

\ Public so the driver below runs it with the package CLOSED: the subtests
\ drive real builds, which resolve names in whatever package scope is open.
public
: HBT-MAIN ( -- )
   T-RESET
   LINKER-BUILDER-ENGINE:ENSURE LINKER-BUILDER-ENGINE:HOST$ HBT-KEYED!
   APP-BUILDER-ENGINE:ENSURE
   HBT-PREPARE
   HBT-REPL-SRC HBT-REPL-SRC$ HBT-WRITE-LARGE
   CLI-REPORT
   HBT-INSTALL-FAIL
   HBT-PUB-PAIR-FAILS
   HBT-RUN-REPL
   HBT-RUN-REPL-ARGS
   HBT-BUILD-REPL-BAD
   HBT-LOUD-MAKER
   HBT-BUILD-MISSING-TMP
   HBT-STRIPPED-LITERAL-BODY
   HBT-STRIPPED-IMAGE
   HBT-REPORT-INVALIDATION
   FS:CLEANUP-RUN
   HBT-ROOT FS:EXISTS? TFALSE
   T-REPORT
   s" hb-build-test: ok" type cr ;

;package

;using

HB-BUILD-CLI:HBT-MAIN
