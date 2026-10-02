\ hb-build-test.f - checked fixture for tools/hb-build-lib.f: the REPL build
\ and its report through the CLI, an install that fails, the report a failed
\ build invalidates, the cache keys, the rejected inputs and the size of a
\ snapshot and a stripped image. tools/hb-build-test-lib.f lists the other
\ hb-build rows.
\ Run: bin/hb --load tools/hb-build-test.f

require tools/hb-build-test-lib.f
require test/preloaded-engine.f

using BUILD-FIXPOINT                     \ the build tmp root

\ The shared fixture's words are private words of the library's package, so
\ this row reopens it the way tools/hb-build-test-lib.f does.
package HB-BUILD-CLI

variable HBT-DEP-SRC-U
variable HBT-ENTRY-SRC-U

create HBT-DEP-SRC-BUF FS-PATH-CAP allot
create HBT-ENTRY-SRC-BUF FS-PATH-CAP allot

create HBT-KEY-A 64 allot
create HBT-KEY-B 64 allot

\ The two images the lost-blob case measures: a byte-for-byte copy of a built
\ one, and the same copy with a single word of its startup rewritten.
variable HBT-LOST-CPY-U
variable HBT-LOST-OUT-U
create HBT-LOST-CPY-BUF FS-PATH-CAP allot
create HBT-LOST-OUT-BUF FS-PATH-CAP allot
variable HBT-SEQ-AT      \ the startup's x9 code-base + offset sequence, or -1
variable HBT-SEQ-N       \ how many the image holds
variable HBT-SEQ-IP      \ that scan's cursor

\ The failed install's -o and the directory it sits in, and the files that
\ directory holds.
variable HBT-INST-DIR-U
variable HBT-INST-OUT-U
create HBT-INST-DIR-BUF FS-PATH-CAP allot
create HBT-INST-OUT-BUF FS-PATH-CAP allot
variable HBT-INST-FILES

: HBT-NEW-TMP ( -- ptr u8 n )
   HBT-NEW-TMP-BUF HBT-NEW-TMP-U @ ;

: HBT-DEP-SRC ( -- ptr u8 n )
   HBT-DEP-SRC-BUF HBT-DEP-SRC-U @ ;

: HBT-ENTRY-SRC ( -- ptr u8 n )
   HBT-ENTRY-SRC-BUF HBT-ENTRY-SRC-U @ ;

: HBT-APPEND-CACHE-MUTATION ( -- )
   SB-RESET
   s" \\ cache key mutation" SB-APPEND
   HBB-LF SB-APPEND-C
   HBT-AOT-SRC SB$ APPEND-FILE ;

: HBT-HBB-KEY-AOT ( ptr u8 n ptr u8 -- )
   {: src:ptr srcu:n dst:ptr :}
   src srcu HBT-AOT-OUT HBT-HBB-PREPARE-AOT
   HBB-PREPARE-ARTIFACT-CACHE
   HBB-ARTIFACT-CACHE @ 0= if E-BUILD-SOURCE throw then
   HBB-ARTIFACT-KEY-HEX dst 64 BYTE-COPY
   BF-TMP-RESET ;

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

: HBT-CACHE-KEY-CHANGES ( -- )
   HBT-AOT-SRC HBT-AOT-SRC$ WRITE-ALL
   HBT-AOT-SRC HBT-KEY-A HBT-HBB-KEY-AOT
   HBT-APPEND-CACHE-MUTATION
   HBT-AOT-SRC HBT-KEY-B HBT-HBB-KEY-AOT
   HBT-KEY-A 64 HBT-KEY-B 64 STR= TFALSE
   HBT-AOT-SRC HBT-AOT-SRC$ WRITE-ALL ;

\ --report-json through the CLI: a REPL build exits 0 with nothing on stderr
\ and the report object on stdout, HBB-SUCCESS writing HB-BUILD:REPORT$, its
\ cache source none. HBB-BUILD-REPL consults no cache and sets no trace flag,
\ so the five false fields are the reset trace as the report writes it, not a
\ cache that was asked and missed. CHECK-REPORT takes the expected cache
\ fields from this process's options, so they are set to the CLI's build.
\ The output already holds stale bytes, and no other case builds over one: the
\ CLI renames its copy onto -o (HBB-INSTALL-OUT), and HBT-RUN-REPL's exact
\ stdout proves the new image replaced them.
: CLI-REPORT ( -- )
   HBT-REPL-OUT s" stale" WRITE-ALL
   HBT-ARGV-BASE-REPL
   HBT-ADD-REPORT
   HBT-RUN-HB-BUILD {: outu:n erru:n rc:n :}
   rc 0 T=
   erru 0 T=
   HBB-RESET-OPTIONS HBB-REPL-ON
   HBT-OUT outu JR:T-FALSE JR:T-FALSE JR:T-FALSE JR:T-FALSE JR:T-FALSE CHECK-REPORT ;

\ The install stages the engine in a sibling of -o and renames it over -o
\ (HBB-INSTALL-OUT). A directory at -o lets the sibling be filled and made
\ executable, then refuses the rename: the build throws E-FS-IO, -o is still
\ the directory, and no file is left in the directory that holds it.
: HBT-INST-DIR ( -- ptr u8 n )
   HBT-INST-DIR-BUF HBT-INST-DIR-U @ ;

: HBT-INST-OUT ( -- ptr u8 n )
   HBT-INST-OUT-BUF HBT-INST-OUT-U @ ;

: HBT-INST-FILE ( ptr u8 n -- )
   2drop 1 HBT-INST-FILES +! ;

: HBT-INST-FILES@ ( -- n )
   0 HBT-INST-FILES !
   HBT-INST-DIR [: HBT-INST-FILE ;] WALK-FILES
   HBT-INST-FILES @ ;

: HBT-INSTALL-FAIL ( -- )
   HBT-ROOT s" install" HBT-INST-DIR-BUF HBT-INST-DIR-U HBT-PATH!
   HBT-INST-DIR s" out" HBT-INST-OUT-BUF HBT-INST-OUT-U HBT-PATH!
   HBT-INST-DIR MAKE-DIR
   HBT-INST-OUT MAKE-DIR
   HBT-REPL-SRC HBT-INST-OUT HBT-HBB-PREPARE-REPL
   [: HBB-BUILD ;] E-FS-IO TTHROWSQ
   HBB-GOT-NAME$ BF-REMOVE-TMP
   BF-TMP-RESET
   HBT-INST-OUT DIR? TTRUE
   HBT-INST-FILES@ 0 T= ;

\ A build that fails invalidates the report the last one left: this runs after
\ HBT-SIZE-AOT-LOST-BLOB, whose in-process build left a valid one. It is the
\ last case, since it leaves the cache root naming a file.
: HBT-REPORT-INVALIDATION ( -- )
   HB-BUILD:VALID? TTRUE
   HBT-BAD-OUT s" cache path file" WRITE-ALL
   HBT-BAD-OUT BUILD-CACHE:ROOT!
   HBT-AOT-SRC HBT-AOT-OUT HBT-HBB-PREPARE-AOT
   [: HBB-BUILD ;] catch E-BUILD-PATH T=
   BF-TMP-RESET
   HB-BUILD:VALID? TFALSE
   [: HB-BUILD:CACHE-ROOT$ 2drop ;] catch E-BUILD-STATUS T= ;

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
   rcn 0 <> if s" repl args rc: " type rcn . cr HBT-RUN-OUT outn type HBT-RUN-ERR errn type then
   rcn 0 T=
   HBT-RUN-ERR errn HBT-EMPTY$ T$=
   HBT-RUN-OUT outn HBT-REPL-ARGS-EXPECTED$ T-STR= 0= if
      s" repl args stdout: " type HBT-RUN-OUT outn type cr
      s" actual len: " type outn . cr
      s" expect len: " type HBT-REPL-ARGS-EXPECTED$ nip . cr
   then
   HBT-RUN-OUT outn HBT-REPL-ARGS-EXPECTED$ T$= ;

: HBT-ARG+ ( ptr u8 n -- )
   >LEN PROC-ARGV+ ;

: HBT-IMGDUMP-ARGV ( -- )
   PROC-ARGV-ENV-RESET
   s" --load" HBT-ARG+
   s" lib/errors.f" HBT-ARG+
   s" lib/string.f" HBT-ARG+
   s" lib/memory.f" HBT-ARG+
   s" lib/fs.f" HBT-ARG+
   s" tools/imgdump.f" HBT-ARG+
   s" --" HBT-ARG+
   HBT-REPL-OUT HBT-ARG+ ;

: HBT-IMGDUMP-NAME$ ( -- ptr u8 n )
   s" hb-build-imgdump" ;

: HBT-IMGDUMP-DUMP$ ( -- ptr u8 n )
   HBT-IMGDUMP-NAME$ BF-A$ ;

\ The dump, read back through its own size. A buffer sized before the child runs
\ cannot bound it (see below), so the file's size is what sizes the read.
: HBT-IMGDUMP-READ$ ( -- ptr u8 n )
   HBT-IMGDUMP-DUMP$ FILE-SIZE MEM-ALLOC-64K-SPAN {: buf:ptr cap:n :}
   HBT-IMGDUMP-DUMP$ buf cap READ-ALL {: u:n :}
   buf u ;

: HBT-IMGDUMP-RC ( outcome -- n )
   MATCH outcome
     exited   OF ENDOF
     signaled OF 128 + ENDOF
     timeout  OF E-PROC-TIMEOUT throw ENDOF
   ;MATCH ;

\ ---- where each image class's bytes went --------------------------------------
\ tools/image-size-lib.f attributes every byte of the image to a class and
\ refuses to answer unless the classes sum to the file's own length, so MEASURE
\ returning at all is the sum-to-length proof; these cases pin that the sum is
\ the file's REAL length (not a number the walk invented), that the summary's
\ six terms plus `other` are the same total, and the one fact that separates the
\ two application classes -- a snapshot writes its zero bytes and a stripped
\ image never does.
: HBT-SIZE-SUM ( -- n )
   IMAGE-SIZE:CODE-BYTES IMAGE-SIZE:NAME-BYTES +
   IMAGE-SIZE:DATA-WRITTEN + IMAGE-SIZE:DATA-ZERO +
   IMAGE-SIZE:PAD-BYTES + IMAGE-SIZE:OTHER-BYTES + ;

: HBT-SIZE-MEASURE ( ptr u8 n -- ) {: a:ptr u:n :}
   a u IMAGE-SIZE:MEASURE
   IMAGE-SIZE:TOTAL-BYTES  a u FILE-SIZE T=
   HBT-SIZE-SUM  IMAGE-SIZE:TOTAL-BYTES T= ;

: HBT-SIZE-REPL ( -- )
   HBT-REPL-OUT HBT-SIZE-MEASURE
   IMAGE-SIZE:CLASS$ s" repl-snapshot" T$=
   \ A snapshot stores its fixed DATA prefix and code maps as they stand, zeros
   \ included, even when its heap goes in the cell grid: the class exists and
   \ is never empty.
   IMAGE-SIZE:DATA-ZERO 0 > TTRUE
   IMAGE-SIZE:CODE-BYTES 0 > TTRUE
   IMAGE-SIZE:NAME-BYTES 0 > TTRUE
   \ The region payload is attributed and not just classified: the code band
   \ splits into what records own, the out-of-line names beside it and the code
   \ no record owns, and the DATA window into owners. Each of those partitions
   \ is checked against the payload it covers inside MEASURE, so the case both
   \ pins that the walk ran and that every charge added up.
   IMAGE-SIZE:REGION-CODE 0 > TTRUE
   IMAGE-SIZE:REGION-NAMES 0 > TTRUE
   IMAGE-SIZE:REGION-UNOWNED 0 > TTRUE
   IMAGE-SIZE:DATA-OWNERS 0 > TTRUE ;

\ Builds its own image, because this case reads the file rather than a report
\ about it.
: HBT-SIZE-AOT ( -- )
   HBT-TMP BUILD-CACHE:ROOT!
   HBT-AOT-SRC HBT-AOT-SRC$ WRITE-ALL
   HBT-REMOVE-AOT-OUT
   HBT-AOT-SRC HBT-AOT-OUT HBT-HBB-PREPARE-AOT
   HBB-BUILD
   HBT-AOT-OUT HBT-SIZE-MEASURE
   IMAGE-SIZE:CLASS$ s" stripped" T$=
   \ The other half of the same fact: a stripped image encodes its window as
   \ non-zero runs, so not one zero byte of it travels.
   IMAGE-SIZE:DATA-ZERO 0 T=
   IMAGE-SIZE:DATA-WRITTEN 0 > TTRUE
   IMAGE-SIZE:CODE-BYTES 0 > TTRUE
   \ ... and it carries no dictionary at all.
   IMAGE-SIZE:NAME-BYTES 0 T=
   \ No region either, and no DATA owner: these run after the snapshot case in
   \ one process, so they also pin that the attribution answers for the image in
   \ hand and never with the last one's numbers.
   IMAGE-SIZE:REGION-CODE 0 T=
   IMAGE-SIZE:REGION-NAMES 0 T=
   IMAGE-SIZE:REGION-UNOWNED 0 T=
   IMAGE-SIZE:DATA-OWNERS 0 T=
   HBT-REMOVE-ARTIFACT
   HBT-REMOVE-AOT-OUT
   BF-TMP-RESET ;

\ ---- an image that copies a DATA blob this reader cannot locate ---------------
\ tools/image-size-lib.f FIND-BLOB reads the blob's address out of the startup's
\ four-word src/habu/aot-lib.f TEXT-ADR, sequence into x9. An image whose
\ sequence it no longer recognises has none to count, and that used to be the
\ empty-capture-window shape: the tool reported the blob and the rows as code
\ with `data 0 written` and exited 0. It now looks for the copy loop as well,
\ which no empty window emits, and refuses by name.
\ The subject is one word of a REAL image: the sequence's `adr x12` -- the code
\ base its offset is added to -- rewritten to `adr x13`, which is what a drifted
\ scratch register looks like to this reader. The control is the same image
\ written back unpatched and measured green, so the refusal is that word's and
\ not the copy's.
: HBT-LOST-CPY ( -- ptr u8 n )
   HBT-LOST-CPY-BUF HBT-LOST-CPY-U @ ;

: HBT-LOST-OUT ( -- ptr u8 n )
   HBT-LOST-OUT-BUF HBT-LOST-OUT-U @ ;

: HBT-IMG-U32@ ( ptr u8 n -- n ) {: a:ptr off:n :}
   a off + c@
   a off 1+ + c@ 8 lshift or
   a off 2 + + c@ 16 lshift or
   a off 3 + + c@ 24 lshift or ;

: HBT-IMG-U32! ( n ptr u8 n -- ) {: w:n a:ptr off:n :}
   w $FF and a off + c!
   w 8 rshift $FF and a off 1+ + c!
   w 16 rshift $FF and a off 2 + + c!
   w 24 rshift $FF and a off 3 + + c! ;

\ The image, read whole: the patch is one word inside it and the file is written
\ back from the same bytes, so a copy that is not faithful cannot pass as one.
: HBT-AOT-IMAGE$ ( -- ptr u8 n )
   HBT-AOT-OUT FILE-SIZE MEM-ALLOC-64K-SPAN {: buf:ptr cap:n :}
   HBT-AOT-OUT buf cap READ-ALL {: u:n :}
   buf u ;

\ Every x9 sequence in the file, by the shape module's own test, so the case
\ patches what the tool reads rather than a word it believes is there.
: HBT-SCAN-TEXT-ADR9 ( ptr u8 n -- ) {: a:ptr u:n :}
   -1 HBT-SEQ-AT !  0 HBT-SEQ-N !  CODE-OFF HBT-SEQ-IP !
   begin HBT-SEQ-IP @ 16 + u <= while
      a HBT-SEQ-IP @ HBT-IMG-U32@
      a HBT-SEQ-IP @ 4 + HBT-IMG-U32@
      a HBT-SEQ-IP @ 8 + HBT-IMG-U32@
      a HBT-SEQ-IP @ 12 + HBT-IMG-U32@
      HBT-SEQ-IP @ CODE-OFF 9 AOT-STARTUP-SHAPE:TEXT-ADR-SEQ? if
         HBT-SEQ-N @ 1+ HBT-SEQ-N !
         HBT-SEQ-AT @ 0 < if HBT-SEQ-IP @ HBT-SEQ-AT ! then
      then
      HBT-SEQ-IP @ 4 + HBT-SEQ-IP !
   repeat ;

\ Measured by the tool itself, in a child: the refusal is a `die` and ends the
\ process, which is the behaviour under test.
: HBT-MEASURE-CHILD ( ptr u8 n -- n n n ) {: img:ptr imgu:n :}
   PROC-ARGV-ENV-RESET
   s" --load" HBT-ARG+
   s" tools/engine-size.f" HBT-ARG+
   s" --" HBT-ARG+
   img imgu HBT-ARG+
   PROC-ENV-INHERIT-MISSING
   HBT-RUN-HB-BUILD ;

: HBT-SIZE-AOT-LOST-BLOB ( -- )
   HBT-TMP BUILD-CACHE:ROOT!
   HBT-ROOT s" lostcopy" HBT-LOST-CPY-BUF HBT-LOST-CPY-U HBT-PATH!
   HBT-ROOT s" lostblob" HBT-LOST-OUT-BUF HBT-LOST-OUT-U HBT-PATH!
   HBT-AOT-SRC HBT-AOT-SRC$ WRITE-ALL
   HBT-REMOVE-AOT-OUT
   HBT-AOT-SRC HBT-AOT-OUT HBT-HBB-PREPARE-AOT
   HBB-BUILD
   HBT-AOT-IMAGE$ {: a:ptr u:n :}
   a u HBT-SCAN-TEXT-ADR9
   HBT-SEQ-N @ 1 T=
   HBT-LOST-CPY a u WRITE-ALL
   HBT-LOST-CPY HBT-MEASURE-CHILD {: outu:n erru:n rc:n :}
   HBT-ERR erru HBT-EMPTY$ T$=
   rc 0 T=
   HBT-OUT outu s" restored DATA window: " CONTAINS? TTRUE
   13 CODE-OFF HBT-SEQ-AT @ 8 + - A64ASM:ENC-ADR
   a HBT-SEQ-AT @ 8 + HBT-IMG-U32!
   HBT-LOST-OUT a u WRITE-ALL
   HBT-LOST-OUT HBT-MEASURE-CHILD {: outu2:n erru2:n rc2:n :}
   rc2 0 T<>
   HBT-ERR erru2
   s" image-size: the startup copies a DATA blob but no code base + offset sequence names it"
   CONTAINS? TTRUE
   HBT-LOST-CPY HBT-REMOVE-FILE?
   HBT-LOST-OUT HBT-REMOVE-FILE?
   HBT-REMOVE-ARTIFACT
   HBT-REMOVE-AOT-OUT
   BF-TMP-RESET ;

\ imgdump prints one line per dictionary record of the image it reads, so its
\ output is the size of that image's dictionary - 407,041 bytes for the REPL
\ application this case builds, and growing with the engine. A bounded capture
\ buffer is the wrong instrument for it: PROC-READ-OR-PROBE-STREAM fails closed
\ when the buffer fills (E-PROC-TRUNCATED), so the case died on its own
\ measurement rather than on the image. stdout goes to a file, which has no size
\ chosen in advance; stderr stays a bounded capture because an empty stderr is
\ what this case asserts.
: HBT-IMGDUMP-REPL ( -- )
   HBT-TMP BF-TMP!
   HBT-IMGDUMP-ARGV
   PROC-ENV-INHERIT-MISSING
   s" bin/hb" HBT-IMGDUMP-DUMP$ HBT-RUN-ERR HBT-CAPTURE-CAP >LEN
   HBT-TIMEOUT-MS >MS BF-RUN-ARGV-ENV-OUTFILE      \ ( len outcome )
   swap LEN>N {: errn:n :}
   HBT-IMGDUMP-RC 0 T=
   HBT-RUN-ERR errn HBT-EMPTY$ T$=
   HBT-IMGDUMP-READ$ s" + " CONTAINS? TTRUE
   HBT-IMGDUMP-NAME$ BF-REMOVE-TMP
   BF-TMP-RESET ;

\ Rejected input is checked in the real app-build child, which refuses it with
\ the checker's code and diagnostic.
: HBT-BUILD-REPL-BAD ( -- )
   HBT-REPL-BAD-SRC HBT-RUN-APP {: outu:n erru:n rc:n :}
   rc 70 T=
   outu 0 T=
   HBB-ERR-BUF erru s" expected: i64" CONTAINS? TTRUE
   HBB-ERR-BUF erru s" actual: bool" CONTAINS? TTRUE ;

: HBT-BUILD-MISSING-TMP ( -- )
   HBT-NEW-TMP EXISTS? TFALSE
   HBT-NEW-TMP HBT-ARGV-BASE-TMP
   HBT-ADD-BAD
   HBT-RUN-HB-BUILD 0 T<>
   {: outu erru :}
   HBT-OUT outu HBT-EMPTY$ T$=
   HBT-ERR erru s" E-AOT-UNSUPPORTED" CONTAINS? TTRUE
   HBT-NEW-TMP DIR? TTRUE
   HBT-BAD-OUT EXISTS? TFALSE ;

\ The AOT/object cache keys fold the whole require/include closure, so a
\ content edit to a required file (not just the top-level source) must change the
\ key. This is the property a single-file digest could not provide.
: HBT-ENTRY-CLOSURE$ ( -- ptr u8 n )
   SB-RESET
   s" require " SB-APPEND
   HBT-DEP-SRC SB-APPEND
   HBB-LF SB-APPEND-C
   SB$ ;

: HBT-ARTIFACT-KEY ( ptr u8 -- ) {: dst:ptr :}
   HBB-ARTIFACT-KEY!
   HBB-ARTIFACT-KEY-HEX dst 64 BYTE-COPY ;

: HBT-CLOSURE-KEY-CHANGES ( -- )
   HBB-RESET-OPTIONS
   HBT-TMP BUILD-CACHE:ROOT!
   HBT-ROOT s" dep-closure.f" HBT-DEP-SRC-BUF HBT-DEP-SRC-U HBT-PATH!
   HBT-ROOT s" entry-closure.f" HBT-ENTRY-SRC-BUF HBT-ENTRY-SRC-U HBT-PATH!
   HBT-DEP-SRC s\" \\ dep v1\n" WRITE-ALL
   HBT-ENTRY-SRC HBT-ENTRY-CLOSURE$ WRITE-ALL
   HBT-ENTRY-SRC HBB-SRC!
   HBT-KEY-A HBT-ARTIFACT-KEY
   HBT-DEP-SRC s\" \\ dep v2 changed\n" APPEND-FILE
   HBT-KEY-B HBT-ARTIFACT-KEY
   HBT-KEY-A 64 HBT-KEY-B 64 STR= TFALSE
   HBT-ENTRY-SRC HBB-SRC!
   HBB-SRC-CLOSURE-HEX! HBB-SRC-CLOSURE-HEX HBT-KEY-A 64 BYTE-COPY
   HBT-DEP-SRC s\" \\ dep v3 changed\n" APPEND-FILE
   HBB-SRC-CLOSURE-HEX! HBB-SRC-CLOSURE-HEX HBT-KEY-B 64 BYTE-COPY
   HBT-KEY-A 64 HBT-KEY-B 64 STR= TFALSE ;

\ tools/dynamic-tail-manifest.f is a behaviour-bearing dependency of the
\ discovery producer (tools/source-discovery.f requires it, and its rows steer
\ closure computation), so its content must fold into the producer cache key. The
\ key preimage records each tool source through CONTENT-KEY:FILE+, which appends
\ the path fragment and then the file's content digest, so the presence of the
\ manifest path in the preimage (CONTENT-KEY:BUF$) proves its content
\ participates in the key. If the manifest is missing from
\ HBB-KEY-LOAD-FILES a manifest edit silently reuses a stale hb-build artifact.
: HBT-MAKER-KEY-FOLDS-MANIFEST ( -- )
   CONTENT-KEY:OPEN
   HBB-KEY-LOAD-FILES
   dup CONTENT-KEY:BUF$ s" tools/dynamic-tail-manifest.f" CONTAINS? TTRUE
   CONTENT-KEY:DISCARD ;

\ Public so the driver below runs it with the package CLOSED: the subtests
\ drive real builds, which resolve names in whatever package scope is open.
public
: HBT-MAIN ( -- )
   T-RESET
   PRELOADED-ENGINE:LINKER$ APP-IMAGE-ENGINE:PATH$ HBT-KEYED!
   HBT-MAKER-KEY-FOLDS-MANIFEST
   HBT-PREPARE
   CLI-REPORT
   HBT-INSTALL-FAIL
   HBT-CACHE-KEY-CHANGES
   HBT-CLOSURE-KEY-CHANGES
   HBT-RUN-REPL
   HBT-RUN-REPL-ARGS
   HBT-IMGDUMP-REPL
   HBT-SIZE-REPL
   HBT-BUILD-REPL-BAD
   HBT-BUILD-MISSING-TMP
   HBT-SIZE-AOT
   HBT-SIZE-AOT-LOST-BLOB
   HBT-REPORT-INVALIDATION
   CLEANUP-RUN
   HBT-ROOT EXISTS? TFALSE
   T-REPORT
   s" hb-build-test: ok" type cr ;

;package

;using

HB-BUILD-CLI:HBT-MAIN
