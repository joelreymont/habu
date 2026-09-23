\ hb-build-test.f - checked fixture for tools/hb-build-lib.f: the build, CLI
\ and AOT groups. The stripped-window groups run from
\ tools/hb-build-stripped-test.f and tools/hb-build-stripped-cells-test.f, on
\ gate rows of their own, because one row running all three took 353-355 s of
\ the gate's 360 s child timeout.
\ Run: bin/hb --load tools/hb-build-test.f

require tools/hb-build-test-lib.f

using BUILD-FIXPOINT                     \ the build tmp root and engine override

\ The shared fixture's words are private words of the library's package, so
\ this half reopens it the way tools/hb-build-test-lib.f does.
package HB-BUILD-CLI

64 constant HBT-KEY-U

variable HBT-DEP-SRC-U
variable HBT-ENTRY-SRC-U
variable HBT-ENG-U

create HBT-DEP-SRC-BUF FS-PATH-CAP allot
create HBT-ENTRY-SRC-BUF FS-PATH-CAP allot
create HBT-ENG-BUF FS-PATH-CAP allot
create HBT-ABI-ALT 96 allot

create HBT-AOT-HEX 80 allot
create HBT-SRC-KEY 80 allot
create HBT-KEY-A 64 allot
create HBT-KEY-B 64 allot

create HBT-REPORT-BUF FS-PATH-CAP allot
variable HBT-EXP-SRC-U
variable HBT-EXP-OUT-U
create HBT-EXP-SRC-BUF FS-PATH-CAP allot
create HBT-EXP-OUT-BUF FS-PATH-CAP allot

\ The quoted-path fixture below owns the writer's bytes; json-write holds none.
6 constant HBT-ESCAPE-MAX
FS-PATH-CAP HBT-ESCAPE-MAX * 2 + constant HBT-QUOTE-CAP
HBT-QUOTE-CAP BUFFER: HBT-QUOTE-BUF
TYPED-VARIABLE HBT-QUOTE-WRITER JSON-WRITE:writer

create HBT-EXP-DG 32 allot
create HBT-EXP-HEX1 64 allot
create HBT-EXP-HEX2 64 allot

: HBT-NEW-TMP ( -- ptr u8 n )
   HBT-NEW-TMP-BUF HBT-NEW-TMP-U @ ;

: HBT-BAD-OUT ( -- ptr u8 n )
   HBT-BAD-OUT-BUF HBT-BAD-OUT-U @ ;

: HBT-REPL-BAD-OUT ( -- ptr u8 n )
   HBT-REPL-BAD-OUT-BUF HBT-REPL-BAD-OUT-U @ ;

: HBT-SPAN-SRC ( -- ptr u8 n )
   HBT-SPAN-SRC-BUF HBT-SPAN-SRC-U @ ;

: HBT-SPAN-OUT ( -- ptr u8 n )
   HBT-SPAN-OUT-BUF HBT-SPAN-OUT-U @ ;

: HBT-DEP-SRC ( -- ptr u8 n )
   HBT-DEP-SRC-BUF HBT-DEP-SRC-U @ ;

: HBT-ENTRY-SRC ( -- ptr u8 n )
   HBT-ENTRY-SRC-BUF HBT-ENTRY-SRC-U @ ;

: HBT-APPEND-CACHE-MUTATION ( -- )
   SB-RESET
   s" \\ cache key mutation" SB-APPEND
   HBB-LF SB-APPEND-C
   HBT-AOT-SRC SB$ APPEND-FILE ;

\ RUN's last line calls NULL$, and that is the point of it: NULL$ is a colon
\ word of the ENGINE's own prefix, so it sits outside this window's code and the
\ capture records a call site for it whose callee no index of this payload can
\ name (habu2.f EMIT-AOT-SITES leaves such a site its name; the seed resolves it
\ in the engine it boots). Every other call here is to a primitive, which binds
\ to a seeded record index, so the two site kinds travel in one built image and
\ a build that can only emit one of them fails this case.
: HBT-AOT-SRC2$ ( -- ptr u8 n )
   S\" package HBT-NATIVE\n: LOADING ( -- ) tier@ 1 <> if -9040 throw then ;\nLOADING\npublic\n: INC ( n -- n ) 1+ ;\n: APPLY ( n [ n -- n ] -- n ) execute ;\n: RUN ( -- ) 41 [: INC ;] APPLY 42 <> if -9041 throw then NULL$ nip 0 <> if -9045 throw then ;\n;package\n: MAIN ( -- ) HBT-NATIVE:RUN ;\n" ;

\ A zero divisor in a STRIPPED image. The division is compiled by the native
\ compiler (the image is built at tier 1, which `LOADING` holds it to), and its
\ refusal branches to the engine's own `throw` - an address outside this
\ payload's window that the closure walker has to follow and the build has to
\ relocate, exactly as it does for the terminator's `die`. An image whose branch
\ was left pointing at the building engine's text does not come back here with a
\ code at all, and one whose refusal was dropped answers zero and throws -9051.
\ The code is ARITH-ABI:E-DIV-ZERO, spelled out because a built source is a
\ string and cannot see this file's constants.
: HBT-AOT-DIVZ-SRC$ ( -- ptr u8 n )
   S\" package HBT-DIVZ\n: LOADING ( -- ) tier@ 1 <> if -9050 throw then ;\nLOADING\nvariable A\nvariable B\nvariable R\n: DZ ( n n -- n ) / ;\n: TRY ( -- n ) [: A @ B @ DZ R ! ;] catch ;\npublic\n: RUN ( -- ) 7 A ! 0 B ! TRY dup . cr -6400 <> if -9051 throw then 7 A ! 2 B ! TRY 0 <> if -9052 throw then R @ 3 <> if -9053 throw then ;\n;package\n: MAIN ( -- ) HBT-DIVZ:RUN ;\n" ;

: HBT-SPAN-SRC$ ( -- ptr u8 n )
   S\" require lib/span.f\nrequire lib/memory.f\npackage HBT-SPAN\n: LOADING ( -- ) tier@ 1 <> if -9060 throw then ;\nLOADING\n64 SPAN-BUFFER: SB\npublic\n: RUN ( -- )\n   SB SPAN:LEN . cr\n   128 MEM:BYTES-ALLOC-LEN MEM:ALLOC-SPAN {: s :}\n   s\" span\" s SPAN:COPY\n   s 0 4 SPAN:SUB SPAN:$ type cr\n   s 8 SPAN:SKIP 16 SPAN:TAKE SPAN:LEN . cr\n   s MEM:FREE-SPAN ;\n;package\n: MAIN ( -- ) HBT-SPAN:RUN ;\n" ;

: HBT-SPAN-EXPECTED$ ( -- ptr u8 n )
   S\" 64\n\nspan\n16\n\n" ;

\ A definer whose `does>` clause is EMPTY, in a stripped image: the created word
\ keeps the RET `create` emitted (habu2.f DOES-REC:ELIDE-EMPTY), so it reads its
\ cell with no branch out of its own body, and the image prints what was stored.
: HBT-AOT-DOES-SRC$ ( -- ptr u8 n )
   S\" package HBT-DOES\n: LOADING ( -- ) tier@ 1 <> if -9070 throw then ;\nLOADING\n: SLOT: ( -- ) create 0 , does> ( -- ptr n ) ;\nSLOT: S1\npublic\n: RUN ( -- ) 42 S1 ! S1 @ dup . cr 42 <> if -9071 throw then ;\n;package\n: MAIN ( -- ) HBT-DOES:RUN ;\n" ;

\ ONE FOREIGN CALL IN A STRIPPED IMAGE. The declarer's generated word reaches
\ package FFI's staged call, which resolves `getpid` through the loader at the
\ FIRST CALL - in the image's own process, never the builder's - so the program
\ prints 1 only if the image carried everything that resolution needs: the FFI
\ table, in the program's own DATA window; the loader's two GOT slots, which
\ src/os/linux/elf.f relocates in every image it writes; and the engine's
\ text-base cell, which src/os/linux/layout.f locates those slots from and
\ src/habu/aot-owned-cells.f claims TEXT-BASE. Left at the fresh mapping's zero
\ that cell made this program build without a word of complaint and take SIGSEGV
\ inside DLSYM-SLOT's `@`.
: HBT-AOT-FFI-SRC$ ( -- ptr u8 n )
   S\" require lib/ffi-abi.f\nPROCESS-SYMBOLS\nFUNCTION: GETPID-CALL getpid ( -- n ) ;FUNCTION\n: MAIN ( -- ) GETPID-CALL 0 > if 1 else 0 then . cr ;\n" ;

: HBT-HBB-PREPARE-REPL ( ptr u8 n ptr u8 n -- )
   HBB-RESET-OPTIONS
   HBB-REPL-ON
   HBB-PATHS!
   HBT-TMP BF-TMP! ;

: HBT-HBB-PREPARE-AOT ( ptr u8 n ptr u8 n -- )
   HBB-RESET-OPTIONS
   HBB-PATHS!
   HBT-TMP BF-TMP! ;

: HBT-HBB-BUILD-OUT ( -- )
   HBB-BUILD
   BF-TMP-RESET ;

: HBT-REMOVE-AOT-OUT ( -- )
   HBT-AOT-OUT HBT-REMOVE-FILE? ;

: HBT-REMOVE-ARTIFACT ( -- )
   HBB-ARTIFACT$ HBT-REMOVE-FILE? ;

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

: HBT-BAD-CACHE-ENV ( -- )
   PROC-ENV-RESET
   s" HB_TMP" >LEN HBT-TMP >LEN PROC-ENV+
   s" HABU_BUILD_CACHE" >LEN HBT-BAD-OUT >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING ;

: HBT-ADD-PATH-ERROR ( -- )
   s" --json-errors" >LEN PROC-ARGV+
   HBT-AOT-SRC >LEN PROC-ARGV+
   s" -o" >LEN PROC-ARGV+
   HBT-AOT-OUT >LEN PROC-ARGV+ ;

: HBT-ADD-PATH-ERROR-TEXT ( -- )
   HBT-AOT-SRC >LEN PROC-ARGV+
   s" -o" >LEN PROC-ARGV+
   HBT-AOT-OUT >LEN PROC-ARGV+ ;

: HBT-PATH-ERROR-TEXT$ ( -- ptr u8 n )
   SB-RESET
   s" hb-build: schema=hb-build-error version=1 code=E-BUILD-PATH cache_selected=true cache_root=" SB-APPEND
   HBT-QUOTE-WRITER HBT-QUOTE-BUF HBT-QUOTE-CAP JSON-WRITE:OPEN
   HBT-BAD-OUT JSON-WRITE:STRING JSON-WRITE:$ SB-APPEND
   s"  cache_source=explicit cause=E-FS-DIR" SB-APPEND
   HBB-LF SB-APPEND-C
   SB$ ;

here CELL 1- and CELL swap - CELL 1- and allot
create READER-STATE JR:STORAGE-BYTES allot

: REPORT-STRING= ( JR:reader ptr u8 n -- JR:reader ) {: want:ptr wantu:n :}
   JR:TOKEN JR:T-STR T=
   HBT-REPORT-BUF FS-PATH-CAP JR:STR {: gotu:n :}
   HBT-REPORT-BUF gotu want wantu T$= ;

: CHECK-REPORT ( ptr u8 n n n n n n -- )
   {: a:ptr u:n artifact:n object:n maker:n built:n ran:n :}
   READER-STATE JR:STORAGE-BYTES a u JR:INIT
   JR:NEXT JR:T-OBJ T=
   s" schema" JR:FIND-KEY TTRUE
   s" hb-build-report" REPORT-STRING=
   s" version" JR:FIND-KEY TTRUE
   JR:TOKEN JR:T-INT T=
   JR:INT 1 T=
   s" cache_root" JR:FIND-KEY TTRUE
   HBB-REPL @ if NULL$ else HBT-TMP then REPORT-STRING=
   s" cache_source" JR:FIND-KEY TTRUE
   HBB-REPL @ if s" none" else s" explicit" then REPORT-STRING=
   s" artifact_hit" JR:FIND-KEY TTRUE
   JR:TOKEN artifact T=
   s" object_hit" JR:FIND-KEY TTRUE
   JR:TOKEN object T=
   s" maker_hit" JR:FIND-KEY TTRUE
   JR:TOKEN maker T=
   s" maker_built" JR:FIND-KEY TTRUE
   JR:TOKEN built T=
   s" maker_ran" JR:FIND-KEY TTRUE
   JR:TOKEN ran T=
   s" elapsed_ns" JR:FIND-KEY TTRUE
   JR:TOKEN JR:T-INT T=
   JR:INT 0 >= TTRUE
   JR:CLOSE ;

: BUILD-REPL ( -- )
   HBT-REPL-SRC HBT-REPL-OUT HBT-HBB-PREPARE-REPL
   HBT-HBB-BUILD-OUT
   HBT-REPL-OUT FILE? TTRUE
   HB-BUILD:REPORT$ JR:T-FALSE JR:T-FALSE JR:T-FALSE JR:T-FALSE JR:T-FALSE CHECK-REPORT ;

\ A second capture compiles current sources and never restores a legacy maker.
: REBUILD-REPL ( -- )
   HBT-REPL-OUT FILE? if HBT-REPL-OUT REMOVE-FILE then
   HBT-REPL-SRC HBT-REPL-OUT HBT-HBB-PREPARE-REPL
   HBT-HBB-BUILD-OUT
   HBB-ARTIFACT-HIT @ 0= TTRUE
   HBB-MAKER-RUN @ 0= TTRUE
   HBT-REPL-OUT FILE? TTRUE
   HB-BUILD:REPORT$ JR:T-FALSE JR:T-FALSE JR:T-FALSE JR:T-FALSE JR:T-FALSE CHECK-REPORT ;

: HBT-CACHE-KEY-CHANGES ( -- )
   HBT-AOT-SRC HBT-AOT-SRC$ WRITE-ALL
   HBT-AOT-SRC HBT-KEY-A HBT-HBB-KEY-AOT
   HBT-APPEND-CACHE-MUTATION
   HBT-AOT-SRC HBT-KEY-B HBT-HBB-KEY-AOT
   HBT-KEY-A 64 HBT-KEY-B 64 STR= TFALSE
   HBT-AOT-SRC HBT-AOT-SRC$ WRITE-ALL ;

: CLI-REPORT ( -- )
   HBT-ARGV-BASE
   HBT-ADD-REPORT
   HBT-RUN-HB-BUILD {: outu:n erru:n rc:n :}
   rc 0 T=
   erru 0 T=
   HBT-OUT outu JR:T-FALSE JR:T-FALSE JR:T-FALSE JR:T-FALSE JR:T-FALSE CHECK-REPORT ;

: CHECK-PATH-JSON ( -- )
   HBT-ARGV-BASE
   HBT-BAD-CACHE-ENV
   HBT-ADD-PATH-ERROR
   HBT-RUN-HB-BUILD {: outu:n erru:n rc:n :}
   rc HBB-BUILD-RC T=
   outu 0 T=
   READER-STATE JR:STORAGE-BYTES HBT-ERR erru JR:INIT
   JR:NEXT JR:T-OBJ T=
   s" schema" JR:FIND-KEY TTRUE
   s" hb-build-error" REPORT-STRING=
   s" version" JR:FIND-KEY TTRUE
   JR:TOKEN JR:T-INT T=
   JR:INT 1 T=
   s" code" JR:FIND-KEY TTRUE
   s" E-BUILD-PATH" REPORT-STRING=
   s" cache_selected" JR:FIND-KEY TTRUE
   JR:TOKEN JR:T-TRUE T=
   s" cache_root" JR:FIND-KEY TTRUE
   HBT-BAD-OUT REPORT-STRING=
   s" cache_source" JR:FIND-KEY TTRUE
   s" explicit" REPORT-STRING=
   s" cause" JR:FIND-KEY TTRUE
   s" E-FS-DIR" REPORT-STRING=
   JR:NEXT JR:T-OBJ-END T=
   JR:NEXT JR:T-END T=
   JR:CLOSE ;

: HBT-CHECK-PATH-TEXT ( -- )
   HBT-ARGV-BASE
   HBT-BAD-CACHE-ENV
   HBT-ADD-PATH-ERROR-TEXT
   HBT-RUN-HB-BUILD {: text-outu:n text-erru:n text-rc:n :}
   text-rc HBB-BUILD-RC T=
   text-outu 0 T=
   HBT-ERR text-erru HBT-PATH-ERROR-TEXT$ T$= ;

: CLI-PATH-ERROR ( -- )
   HBT-BAD-OUT s" cache path file" WRITE-ALL
   CHECK-PATH-JSON
   HBT-CHECK-PATH-TEXT
   HBT-BAD-OUT REMOVE-FILE ;

: HBT-REPORT-INVALIDATION ( -- )
   HB-BUILD:VALID? TTRUE
   HBT-BAD-OUT s" cache path file" WRITE-ALL
   HBT-BAD-OUT BUILD-CACHE:ROOT!
   HBT-AOT-SRC HBT-AOT-OUT HBT-HBB-PREPARE-AOT
   [: HBB-BUILD ;] catch E-BUILD-PATH T=
   BF-TMP-RESET
   HB-BUILD:VALID? TFALSE
   [: HB-BUILD:CACHE-ROOT$ 2drop ;] catch E-BUILD-STATUS T=
   HBT-BAD-OUT REMOVE-FILE
   HBT-TMP BUILD-CACHE:ROOT! ;

\ Run an image at a path of its own and hold it to its whole output, for a case
\ that builds somewhere other than HBT-AOT-OUT or HBT-REPL-OUT.
: HBT-RUN-IMAGE-OUT ( ptr u8 n ptr u8 n -- ) {: img:ptr imgu:n want:ptr wantu:n :}
   img imgu >LEN HBT-RUN-OUT HBT-CAPTURE-CAP >LEN HBT-RUN-ERR HBT-CAPTURE-CAP >LEN
   HBT-TIMEOUT-MS >MS RUN-CAPTURE HBT-CAPTURE>N {: outn:n errn:n rcn:n :}
   rcn 0 T=
   HBT-RUN-ERR errn HBT-EMPTY$ T$=
   HBT-RUN-OUT outn want wantu T$= ;

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
     timeout  OF -1 ENDOF
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
   \ A snapshot copies its DATA window verbatim, zeros included, and they are
   \ most of the image: the class exists and is never empty.
   IMAGE-SIZE:DATA-ZERO 0 > TTRUE
   IMAGE-SIZE:DATA-ZERO IMAGE-SIZE:DATA-WRITTEN > TTRUE
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

\ Builds its own image: every AOT case above removes its output as its last
\ act, and this one has to read the file rather than a report about it.
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

\ Rejected input is compiled in the real snapshot child, before an image exists.
: HBT-BUILD-REPL-BAD ( -- )
   HBT-ARGV-BASE
   s" --repl" >LEN PROC-ARGV+
   HBT-REPL-BAD-SRC >LEN PROC-ARGV+
   s" -o" >LEN PROC-ARGV+
   HBT-REPL-BAD-OUT >LEN PROC-ARGV+
   HBT-RUN-HB-BUILD {: outu:n erru:n rc:n :}
   rc 70 T=
   HBT-OUT outu HBT-EMPTY$ T$=
   HBT-ERR erru s" expected: i64" CONTAINS? TTRUE
   HBT-ERR erru s" actual: bool" CONTAINS? TTRUE
   HBT-REPL-BAD-OUT EXISTS? TFALSE ;

\ MAIN must satisfy the application's empty input/output contract at build time.
: HBT-REFUSE-MAIN ( ptr u8 n -- )
   HBT-REPL-BAD-SRC 2swap WRITE-ALL
   HBT-ARGV-BASE
   s" --repl" >LEN PROC-ARGV+
   HBT-REPL-BAD-SRC >LEN PROC-ARGV+
   s" -o" >LEN PROC-ARGV+
   HBT-REPL-BAD-OUT >LEN PROC-ARGV+
   HBT-RUN-HB-BUILD {: outu:n erru:n rc:n :}
   rc 70 T= outu 0 T=
   HBT-ERR erru s" in enter" CONTAINS? TTRUE
   HBT-REPL-BAD-OUT EXISTS? TFALSE ;

: HBT-BAD-MAIN-EFFECTS ( -- )
   s" : MAIN ( n -- n ) 1+ ;" HBT-REFUSE-MAIN
   s" : MAIN ( -- n ) 7 ;" HBT-REFUSE-MAIN ;

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

: HBT-AOT-JIT-REJECT ( -- )
   HBT-REPL-BAD-SRC s" 0 set-tier : MAIN ( -- ) ;" WRITE-ALL
   HBT-ARGV-BASE
   HBT-REPL-BAD-SRC >LEN PROC-ARGV+
   s" -o" >LEN PROC-ARGV+
   HBT-REPL-BAD-OUT >LEN PROC-ARGV+
   HBT-RUN-HB-BUILD {: outu:n erru:n rc:n :}
   rc 70 T= outu 0 T=
   HBT-ERR erru s" executable build requires native tier 1" CONTAINS? TTRUE
   HBT-REPL-BAD-OUT EXISTS? TFALSE ;

\ The object cache key is the ordered-closure hex of the source (a self-contained
\ AOT source closes over only itself), so the fixtures that pre-store objects must
\ key them the same way hb-build now does.
: HBT-AOT-HEX! ( -- )
   HBT-AOT-SRC HBB-SRC!
   HBB-SRC-CLOSURE-HEX!
   HBB-SRC-CLOSURE-HEX HBT-AOT-HEX 64 BYTE-COPY ;

: HBT-AOT-SOURCE-KEY! ( -- )
   HBT-AOT-HEX HBT-KEY-U HBB-TARGET-ABI$ HBB-CHECKER-ABI$ HBB-COMPILER-ABI$
   HBT-SRC-KEY OBJIDX:SOURCE-KEY-HEX ;

: HBT-BUILD-EXIT-OBJ ( ptr u8 n -- ) {: target:ptr targetu:n :}
   HBT-AOT-HEX!
   OBJ:RESET
   HBT-AOT-HEX HBT-KEY-U OBJ:SOURCE!
   target targetu OBJ:TARGET!
   HBB-CHECKER-ABI$ OBJ:CHECKER!
   HBB-COMPILER-ABI$ OBJ:COMPILER!
   ASM-INIT
   0 0 MOVZ,
   NR-EXIT-GROUP SYS,
   CODE ASM-LEN OBJ:TEXT+
   s" MAIN" s" --" OBJ:EXPORT+
   s" MAIN" 0 s" --" OBJ:DEF+ ;

: HBT-STORE-AOT-OBJ ( -- )
   HBB-RESET-OPTIONS
   HBT-TMP OBJRES:ROOT!
   HBB-TARGET-ABI$ HBT-BUILD-EXIT-OBJ
   OBJRES:STORE nip HBT-KEY-U T= ;

: HBT-STORE-WRONG-AOT-OBJ ( -- )
   HBB-RESET-OPTIONS
   HBT-TMP OBJRES:ROOT!
   s" wrong-aarch64" HBT-BUILD-EXIT-OBJ
   OBJSTORE:STORE {: key:ptr keyu:n :}
   HBT-AOT-SOURCE-KEY!
   HBT-SRC-KEY HBT-KEY-U key keyu OBJIDX:STORE ;

: HBT-RUN-AOT ( -- )
   HBT-AOT-OUT >LEN HBT-RUN-OUT HBT-CAPTURE-CAP >LEN HBT-RUN-ERR HBT-CAPTURE-CAP >LEN
   HBT-TIMEOUT-MS >MS RUN-CAPTURE HBT-CAPTURE>N {: outn:n errn:n rcn:n :}
   rcn 0 T=
   outn 0 T=
   errn 0 T= ;

\ The same run, for an image that is MEANT to print: the refusal case below
\ prints the code it caught, so a silent image would pass HBT-RUN-AOT above for
\ exactly the wrong reason.
: HBT-RUN-AOT-PRINTS ( ptr u8 n -- ) {: want:ptr wantu:n :}
   HBT-AOT-OUT >LEN HBT-RUN-OUT HBT-CAPTURE-CAP >LEN HBT-RUN-ERR HBT-CAPTURE-CAP >LEN
   HBT-TIMEOUT-MS >MS RUN-CAPTURE HBT-CAPTURE>N {: outn:n errn:n rcn:n :}
   rcn 0 T=
   errn 0 T=
   HBT-RUN-OUT outn want wantu CONTAINS? TTRUE ;

: HBT-OBJ-LOAD? ( -- bool )
   HBB-RESET-OPTIONS
   HBT-TMP OBJRES:ROOT!
   HBT-AOT-HEX!
   HBT-AOT-HEX HBT-KEY-U HBB-TARGET-ABI$ HBB-CHECKER-ABI$ HBB-COMPILER-ABI$ OBJRES:LOAD ;

: BUILD-AOT-OBJECT-PRODUCER ( -- )
   HBT-TMP BUILD-CACHE:ROOT!
   HBT-AOT-SRC HBT-AOT-SRC$ WRITE-ALL
   HBT-REMOVE-AOT-OUT
   HBT-AOT-SRC HBT-AOT-OUT HBT-HBB-PREPARE-AOT
   HBB-BUILD
   HBB-MAKER-RUN @ 0 <> TTRUE
   HBB-OBJECT-HIT @ 0= TTRUE
   HBB-OBJECT-STORE @ 0 <> TTRUE
   HB-BUILD:REPORT$ JR:T-FALSE JR:T-FALSE JR:T-FALSE JR:T-FALSE JR:T-TRUE CHECK-REPORT
   HBT-OBJ-LOAD? TTRUE
   HBT-REMOVE-ARTIFACT
   HBT-REMOVE-AOT-OUT
   HBT-AOT-SRC HBT-AOT-OUT HBT-HBB-PREPARE-AOT
   HBB-BUILD
   HBB-OBJECT-HIT @ 0 <> TTRUE
   HBB-OBJECT-STORE @ 0= TTRUE
   HBB-MAKER-RUN @ 0= TTRUE
   HBB-MAKER-BUILD @ 0= TTRUE
   HB-BUILD:REPORT$ JR:T-FALSE JR:T-TRUE JR:T-FALSE JR:T-FALSE JR:T-FALSE CHECK-REPORT
   HBT-RUN-AOT
   HBT-REMOVE-ARTIFACT
   HBT-REMOVE-AOT-OUT
   BF-TMP-RESET ;

: BUILD-AOT-NATIVE ( -- )
   HBT-TMP BUILD-CACHE:ROOT!
   HBT-AOT-SRC HBT-AOT-SRC2$ WRITE-ALL
   HBT-REMOVE-AOT-OUT
   HBT-AOT-SRC HBT-AOT-OUT HBT-HBB-PREPARE-AOT
   HBB-BUILD
   HBB-ARTIFACT-HIT @ 0= TTRUE
   HBB-OBJECT-HIT @ 0= TTRUE
   HBB-MAKER-HIT @ 0= TTRUE
   HBB-MAKER-RUN @ 0 <> TTRUE
   HB-BUILD:REPORT$ JR:T-FALSE JR:T-FALSE JR:T-FALSE JR:T-FALSE JR:T-TRUE CHECK-REPORT
   HBT-RUN-AOT
   HBT-REMOVE-ARTIFACT
   HBT-REMOVE-AOT-OUT
   HBT-AOT-SRC HBT-AOT-SRC$ WRITE-ALL
   BF-TMP-RESET ;

\ THE CONTRACT THAT DOES NOT DEPEND ON THE TIER. A program that divides by zero
\ catches ARITH-ABI:E-DIV-ZERO and carries on, and a built executable is where
\ that used to stop being true: the lowering's guard ended in a `brk`, so the
\ same source that refused by name under `bin/hb` died with a register dump once
\ it was an image. The catch is asserted THROUGH A BUILT AND EXECUTED IMAGE
\ because the relocation of the refusal's branch is what makes it true and
\ nothing short of running the image exercises it.
: BUILD-AOT-DIV-REFUSAL ( -- )
   HBT-TMP BUILD-CACHE:ROOT!
   HBT-AOT-SRC HBT-AOT-DIVZ-SRC$ WRITE-ALL
   HBT-REMOVE-AOT-OUT
   HBT-AOT-SRC HBT-AOT-OUT HBT-HBB-PREPARE-AOT
   HBB-BUILD
   S\" -6400\n" HBT-RUN-AOT-PRINTS
   HBT-REMOVE-ARTIFACT
   HBT-REMOVE-AOT-OUT
   HBT-AOT-SRC HBT-AOT-SRC$ WRITE-ALL
   BF-TMP-RESET ;

\ A `create ... does>` definer whose clause yields a multi-cell value, built and
\ RUN as an image: the clause is compiled at tier 1 (LOADING asserts the tier),
\ the created word yields lib/span.f's two-cell span, and the span set it belongs
\ to - allocate, copy, skip, take, sub, release - is exercised through that
\ value. Nothing short of running the image shows the created word's row
\ surviving the build (dot habu-give-a-does-97cd0db2).
\ BUILD-AOT-SPAN below is the stripped twin of this case, on the same source.
: BUILD-REPL-SPAN ( -- )
   HBT-TMP BUILD-CACHE:ROOT!
   HBT-SPAN-SRC HBT-SPAN-SRC$ WRITE-ALL
   HBT-SPAN-OUT HBT-REMOVE-FILE?
   HBT-SPAN-SRC HBT-SPAN-OUT HBT-HBB-PREPARE-REPL
   HBT-HBB-BUILD-OUT
   HBT-SPAN-OUT FILE? TTRUE
   HBT-SPAN-OUT HBT-SPAN-EXPECTED$ HBT-RUN-IMAGE-OUT
   HBT-REMOVE-ARTIFACT
   HBT-SPAN-OUT HBT-REMOVE-FILE? ;

\ THE SAME SOURCE WITH THE NAMES SHAKEN OUT. `SPAN-BUFFER:` is a `create ...
\ does>` definer whose clause has a body, so the created word's last instruction
\ is the plain B that habu2.f DOESPATCH:EMIT wrote over its RET, aimed at the
\ parent's `;does` companion record. Only src/habu/aot-closure.f SCAN-DIRECT
\ following a direct branch OUT of the member puts that record in the closure;
\ without it the relocation refused this very program with `aot: PC-relative
\ target removed or outside closure`. Running the image is what proves the clause
\ was copied and retargeted rather than merely counted.
: BUILD-AOT-SPAN ( -- )
   HBT-TMP BUILD-CACHE:ROOT!
   HBT-SPAN-SRC HBT-SPAN-SRC$ WRITE-ALL
   HBT-SPAN-OUT HBT-REMOVE-FILE?
   HBT-SPAN-SRC HBT-SPAN-OUT HBT-HBB-PREPARE-AOT
   HBT-HBB-BUILD-OUT
   HBT-SPAN-OUT FILE? TTRUE
   HBT-SPAN-OUT HBT-SPAN-EXPECTED$ HBT-RUN-IMAGE-OUT
   HBT-REMOVE-ARTIFACT
   HBT-SPAN-OUT HBT-REMOVE-FILE? ;

\ THE OTHER HALF OF THE DOES> RULE. An EMPTY clause compiles no instruction, so
\ elaborate.f STAGE-DOES-ENTRY publishes the companion record and patches
\ nothing: the created word keeps its RET and emits no branch at all. Such a
\ definer must still build and run stripped - the closure has nothing extra to
\ follow and the unreached companion record is shaken out with every other name.
: BUILD-AOT-DOES-EMPTY ( -- )
   HBT-TMP BUILD-CACHE:ROOT!
   HBT-AOT-SRC HBT-AOT-DOES-SRC$ WRITE-ALL
   HBT-REMOVE-AOT-OUT
   HBT-AOT-SRC HBT-AOT-OUT HBT-HBB-PREPARE-AOT
   HBB-BUILD
   S\" 42\n" HBT-RUN-AOT-PRINTS
   HBT-REMOVE-ARTIFACT
   HBT-REMOVE-AOT-OUT
   HBT-AOT-SRC HBT-AOT-SRC$ WRITE-ALL
   BF-TMP-RESET ;

: BUILD-AOT-FFI ( -- )
   HBT-TMP BUILD-CACHE:ROOT!
   HBT-AOT-SRC HBT-AOT-FFI-SRC$ WRITE-ALL
   HBT-REMOVE-AOT-OUT
   HBT-AOT-SRC HBT-AOT-OUT HBT-HBB-PREPARE-AOT
   HBB-BUILD
   S\" 1\n\n" HBT-RUN-AOT-PRINTS
   HBT-REMOVE-ARTIFACT
   HBT-REMOVE-AOT-OUT
   HBT-AOT-SRC HBT-AOT-SRC$ WRITE-ALL
   BF-TMP-RESET ;

: BUILD-AOT-PRESEED ( -- )
   HBT-AOT-SRC s" : ALTERNATE ( n n -- ) 42 <> if -9042 throw then 10 <> if -9043 throw then ; : MAIN ( -- ) -9044 throw ;" WRITE-ALL
   HBT-REMOVE-AOT-OUT
   HBT-ARGV-BASE
   s" --preseed-entry" >LEN PROC-ARGV+
   s" ALTERNATE" >LEN PROC-ARGV+
   s" --preseed-seed" >LEN PROC-ARGV+
   s" 000000000000000A000000000000002a" >LEN PROC-ARGV+
   HBT-AOT-SRC >LEN PROC-ARGV+
   s" -o" >LEN PROC-ARGV+
   HBT-AOT-OUT >LEN PROC-ARGV+
   HBT-RUN-HB-BUILD {: outu:n erru:n rc:n :}
   rc 0 T= erru 0 T=
   HBT-OUT outu s" hb-build OK" CONTAINS? TTRUE
   HBT-RUN-AOT
   HBT-REMOVE-AOT-OUT
   HBT-AOT-SRC HBT-AOT-SRC$ WRITE-ALL ;

: HBT-BUILD-AOT-OBJECT-HIT ( -- )
   HBT-TMP BUILD-CACHE:ROOT!
   HBT-STORE-AOT-OBJ
   HBT-AOT-SRC HBT-AOT-OUT HBT-HBB-PREPARE-AOT
   HBT-HBB-BUILD-OUT
   HBB-OBJECT-HIT @ 0 <> TTRUE
   HBB-OBJECT-STORE @ 0= TTRUE
   HBB-MAKER-RUN @ 0= TTRUE
   HBB-MAKER-BUILD @ 0= TTRUE
   HBT-AOT-OUT FILE? TTRUE
   HBT-RUN-AOT ;

\ EXPORT keeps one body (dot habu-compiler-pkg-re-688212c1): the stripped AOT
\ binary carries no names, so a program calling a word through its defining
\ package AND a re-exported alias must be byte-identical to the same program
\ calling the defining name twice — a second body or a diverged call target
\ changes the bytes. Both variants build to the SAME output path so the ad-hoc
\ signature identifier cannot differ; the alias variant also runs, proving
\ both names execute the one body.
: HBT-EXP-SRC ( -- ptr u8 n )
   HBT-EXP-SRC-BUF HBT-EXP-SRC-U @ ;

: HBT-EXP-OUT ( -- ptr u8 n )
   HBT-EXP-OUT-BUF HBT-EXP-OUT-U @ ;

: HBT-EXP-COMMON ( -- )
   s" package XA" SB-APPEND HBB-LF SB-APPEND-C
   s" public" SB-APPEND HBB-LF SB-APPEND-C
   s" : W ( i64 -- i64 ) dup * ;" SB-APPEND HBB-LF SB-APPEND-C
   s" ;package" SB-APPEND HBB-LF SB-APPEND-C ;

: HBT-EXP-REF-SRC$ ( -- ptr u8 n )
   SB-RESET
   HBT-EXP-COMMON
   s" : MAIN ( -- ) 5 XA:W . cr 5 XA:W . cr ;" SB-APPEND HBB-LF SB-APPEND-C
   SB$ ;

: HBT-EXP-ALIAS-SRC$ ( -- ptr u8 n )
   SB-RESET
   HBT-EXP-COMMON
   s" package XB" SB-APPEND HBB-LF SB-APPEND-C
   s" public" SB-APPEND HBB-LF SB-APPEND-C
   s" EXPORT XA:W" SB-APPEND HBB-LF SB-APPEND-C
   s" ;package" SB-APPEND HBB-LF SB-APPEND-C
   s" : MAIN ( -- ) 5 XA:W . cr 5 XB:W . cr ;" SB-APPEND HBB-LF SB-APPEND-C
   SB$ ;

: HBT-EXP-BUILD ( ptr u8 n -- ) {: sa:ptr su:n :}
   HBT-EXP-SRC sa su WRITE-ALL
   HBT-EXP-OUT HBT-REMOVE-FILE?
   HBT-EXP-SRC HBT-EXP-OUT HBT-HBB-PREPARE-AOT
   HBB-BUILD
   HBT-REMOVE-ARTIFACT ;

: HBT-EXP-HASH ( ptr u8 -- ) {: hex:ptr :}
   HBT-EXP-OUT HBT-EXP-DG SHA256-FILE 0 T=
   HBT-EXP-DG hex SHA256>HEX ;

: HBT-EXP-RUN ( -- )
   HBT-EXP-OUT >LEN HBT-RUN-OUT HBT-CAPTURE-CAP >LEN HBT-RUN-ERR HBT-CAPTURE-CAP >LEN
   HBT-TIMEOUT-MS >MS RUN-CAPTURE HBT-CAPTURE>N {: outn:n errn:n rcn:n :}
   rcn 0 T=
   errn 0 T=
   HBT-RUN-OUT outn s" 25" CONTAINS? TTRUE ;

: HBT-BUILD-AOT-EXPORT-ONE-BODY ( -- )
   HBT-TMP BUILD-CACHE:ROOT!
   HBT-ROOT s" exp.f" HBT-EXP-SRC-BUF HBT-EXP-SRC-U HBT-PATH!
   HBT-ROOT s" exp" HBT-EXP-OUT-BUF HBT-EXP-OUT-U HBT-PATH!
   HBT-EXP-REF-SRC$ HBT-EXP-BUILD
   HBT-EXP-HEX1 HBT-EXP-HASH
   HBT-EXP-ALIAS-SRC$ HBT-EXP-BUILD
   HBT-EXP-HEX2 HBT-EXP-HASH
   HBT-EXP-HEX1 64 HBT-EXP-HEX2 64 T$=
   HBT-EXP-RUN
   HBT-EXP-OUT HBT-REMOVE-FILE?
   BF-TMP-RESET ;

: HBT-ENG ( -- ptr u8 n )
   HBT-ENG-BUF HBT-ENG-U @ ;

: HBT-WRITE-ENG ( -- )
   HBT-ROOT s" engine-alt" HBT-ENG-BUF HBT-ENG-U HBT-PATH!
   HBT-ENG s" not-the-real-engine" WRITE-ALL ;

: HBT-ABI-MAKER-SUFFIX ( -- )
   HBB-CHECKER-ABI$ {: a:ptr u:n :}
   a u HBT-KEY-U - BYTE+ HBT-KEY-U HBB-MAKER-KEY-HEX HBT-KEY-U STR= TTRUE ;

: HBT-ABI-MAKER-QUALIFIED ( -- )
   HBB-RESET-OPTIONS
   HBB-CHECKER-ABI$ s" checker-effect-v1+" STARTS-WITH? TTRUE
   HBB-COMPILER-ABI$ s" hb-arm64-v1+" STARTS-WITH? TTRUE
   HBB-CHECKER-ABI$ nip 82 T=
   HBB-COMPILER-ABI$ nip 76 T=
   HBT-ABI-MAKER-SUFFIX ;

: HBT-ENGINE-KEY-FLIP ( -- )
   HBT-ABI-MAKER-QUALIFIED
   HBT-WRITE-ENG
   HBT-OBJ-LOAD? TTRUE
   HBT-ENG BF-ENGINE!
   HBT-OBJ-LOAD? TFALSE
   BF-ENGINE-RESET
   HBT-OBJ-LOAD? TTRUE ;

: HBT-ALT-CHECKER$ ( -- ptr u8 n )
   HBB-CHECKER-ABI$ {: a:ptr u:n :}
   a HBT-ABI-ALT u BYTE-COPY
   HBT-ABI-ALT u 1 - + c@ STR-ZERO = if STR-ZERO 1 + else STR-ZERO then
   HBT-ABI-ALT u 1 - + c!
   HBT-ABI-ALT u ;

: HBT-PRODUCER-KEY-MISS ( -- )
   HBB-RESET-OPTIONS
   HBT-TMP OBJRES:ROOT!
   HBT-AOT-HEX!
   HBT-ALT-CHECKER$ {: alt:ptr altu:n :}
   HBT-AOT-HEX HBT-KEY-U HBB-TARGET-ABI$ alt altu HBB-COMPILER-ABI$ OBJRES:LOAD TFALSE ;

: HBT-BUILD-AOT-WRONG-OBJECT-FAILS ( -- )
   HBT-AOT-SRC HBT-AOT-SRC2$ WRITE-ALL
   HBT-AOT-OUT FILE? if HBT-AOT-OUT REMOVE-FILE then
   HBT-STORE-WRONG-AOT-OBJ
   HBT-AOT-SRC HBT-AOT-OUT HBT-HBB-PREPARE-AOT
   [: HBB-BUILD ;] E-OBJ-SCHEMA TTHROWSQ
   HBB-MAKER-RUN @ 0= TTRUE
   HBT-AOT-OUT EXISTS? TFALSE ;

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
\ the path fragment and then the file's content digest with no earlier return, so
\ the presence of the manifest path in the preimage (CONTENT-KEY:BUF$) proves its
\ content participates in the key. If the manifest is missing from
\ HBB-KEY-LOAD-FILES a manifest edit silently reuses a stale hb-build artifact.
: HBT-MAKER-KEY-FOLDS-MANIFEST ( -- )
   CONTENT-KEY:CACHE-CLEAR!
   CONTENT-KEY:OPEN
   HBB-KEY-LOAD-FILES
   dup CONTENT-KEY:BUF$ s" tools/dynamic-tail-manifest.f" CONTAINS? TTRUE
   CONTENT-KEY:DISCARD ;

\ Public so the driver below runs it with the package CLOSED: the subtests
\ drive real builds, which resolve names in whatever package scope is open.
public
: HBT-MAIN ( -- )
   T-RESET
   HBT-MAKER-KEY-FOLDS-MANIFEST
   HBT-PREPARE
   BUILD-REPL
   REBUILD-REPL
   CLI-REPORT
   CLI-PATH-ERROR
   HBT-REPORT-INVALIDATION
   HBT-CACHE-KEY-CHANGES
   HBT-CLOSURE-KEY-CHANGES
   HBT-RUN-REPL
   HBT-RUN-REPL-ARGS
   HBT-IMGDUMP-REPL
   HBT-SIZE-REPL
   HBT-BUILD-REPL-BAD
   HBT-BAD-MAIN-EFFECTS
   HBT-BUILD-MISSING-TMP
   BUILD-AOT-OBJECT-PRODUCER
   BUILD-AOT-NATIVE
   BUILD-AOT-DIV-REFUSAL
   BUILD-REPL-SPAN
   BUILD-AOT-SPAN
   BUILD-AOT-DOES-EMPTY
   BUILD-AOT-FFI
   HBT-SIZE-AOT
   BUILD-AOT-PRESEED
   HBT-AOT-JIT-REJECT
   HBT-BUILD-AOT-OBJECT-HIT
   HBT-BUILD-AOT-EXPORT-ONE-BODY
   HBT-ENGINE-KEY-FLIP
   HBT-PRODUCER-KEY-MISS
   HBT-BUILD-AOT-WRONG-OBJECT-FAILS
   CLEANUP-RUN
   HBT-ROOT EXISTS? TFALSE
   T-REPORT
   s" hb-build-test: ok" type cr ;

;package

;using

HB-BUILD-CLI:HBT-MAIN
