\ hb-build-test-lib.f - the fixture the hb-build gate rows share: the scratch
\ tree, the fixture sources, HBT-PREPARE, the helpers that run hb-build and the
\ build report check. It defines no MAIN; each row file loads it and runs the
\ groups it owns:
\   tools/hb-build-test.f                     CLI REPL build and report, cache
\                                             keys, rejected inputs, image size
\   tools/hb-build-cli-errors-test.f          cache path error, MAIN effects
\   tools/hb-build-timeout-test.f             maker deadlines and diagnostics
\   tools/hb-build-timeout-env-test.f         deadline override validation
\   tools/hb-build-timeout-json-test.f        empty override, JSON refusal
\   tools/hb-build-aot-test.f                 AOT build and run, one program each
\   tools/hb-build-aot-cache-test.f           object cache and its keys
\   tools/hb-build-stripped-test.f            library state, engine cells, ptr mark
\   tools/hb-build-stripped-chain-test.f      baked constants, chain, open path
\   tools/hb-build-stripped-lifecycle-test.f  lifecycle registry, number parsing
\   tools/hb-build-stripped-cells-test.f      mapped cells, uncarried table
\   tools/hb-build-stripped-cache-test.f      link equality, cached table cells
\   tools/hb-build-large-source-test.f        literal bodies, large source
\   tools/hb-build-repl-twin-test.f           two REPL builds, restored LASTC
\ test/gate-stdlib-cases.f registers each file as a row of its own and says why.

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/fs.f
require lib/fs-root.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/source.f
require lib/build.f
require lib/codesign.f
require lib/content-key.f
require lib/build-cache.f
require lib/json-write.f
require lib/float.f
require lib/json-read.f
require lib/object.f
require lib/object-cache.f
require lib/object-index.f
require lib/object-resolve.f
require lib/object-link.f
require tools/build-fixpoint.f
require tools/cli-run.f
require tools/object-image.f
require tools/hb-build-report.f
require tools/hb-build-lib.f

using BUILD-FIXPOINT                     \ the build tmp root

\ This fixture drives the hb-build library's internals, so it REOPENS package
\ HB-BUILD-CLI rather than importing a public surface: exporting those
\ internals would widen the library's interface for the benefit of its own
\ test. The local fixture scopes this file used to carry (HBT, HBT-CAP,
\ HBT-JSON) were there only because the file had no package of its own; they
\ are ordinary private words of the library's package now.
package HB-BUILD-CLI

65536 constant HBT-CAPTURE-CAP
600000 constant HBT-TIMEOUT-MS
64 constant HBT-KEY-U

variable HBT-ROOT-U
variable HBT-TMP-U
variable HBT-NEW-TMP-U
variable HBT-BAD-SRC-U
variable HBT-BAD-OUT-U
variable HBT-REPL-SRC-U
variable HBT-REPL-OUT-U
variable HBT-REPL-BAD-SRC-U
variable HBT-REPL-BAD-OUT-U
variable HBT-AOT-SRC-U
variable HBT-AOT-OUT-U
variable HBT-SPAN-SRC-U
variable HBT-SPAN-OUT-U

create HBT-ROOT-BUF FS-PATH-CAP allot
create HBT-TMP-BUF FS-PATH-CAP allot
create HBT-NEW-TMP-BUF FS-PATH-CAP allot
create HBT-BAD-SRC-BUF FS-PATH-CAP allot
create HBT-BAD-OUT-BUF FS-PATH-CAP allot
create HBT-REPL-SRC-BUF FS-PATH-CAP allot
create HBT-REPL-OUT-BUF FS-PATH-CAP allot
create HBT-REPL-BAD-SRC-BUF FS-PATH-CAP allot
create HBT-REPL-BAD-OUT-BUF FS-PATH-CAP allot
create HBT-AOT-SRC-BUF FS-PATH-CAP allot
create HBT-AOT-OUT-BUF FS-PATH-CAP allot
create HBT-SPAN-SRC-BUF FS-PATH-CAP allot
create HBT-SPAN-OUT-BUF FS-PATH-CAP allot

create HBT-OUT HBT-CAPTURE-CAP allot
create HBT-ERR HBT-CAPTURE-CAP allot
create HBT-RUN-OUT HBT-CAPTURE-CAP allot
create HBT-RUN-ERR HBT-CAPTURE-CAP allot

create HBT-REPORT-BUF FS-PATH-CAP allot
create HBT-AOT-HEX 80 allot

\ The keyed images a row's builds run on (HBT-KEYED!); empty, the engine.
create HBT-LINKER-BUF FS-PATH-CAP allot
create HBT-SAVER-BUF FS-PATH-CAP allot
variable HBT-LINKER-U
variable HBT-SAVER-U

\ The three stripped-window fixtures: an application whose own require closure
\ owns the library cells it touches, one that reads the engine runtime cells the
\ stripped entry owns, and one that reaches an engine cell nothing claims.
variable HBT-LIB-SRC-U
variable HBT-LIB-OUT-U
variable HBT-LIB-DIR-U
variable HBT-CELLS-SRC-U
variable HBT-CELLS-OUT-U
variable HBT-UNOWNED-SRC-U
variable HBT-PMK-SRC-U
variable HBT-PPH-SRC-U
variable HBT-PPH-OUT-U
variable HBT-TABLE-SRC-U
variable HBT-CHAIN-SRC-U
variable HBT-CHAIN-OUT-U
variable HBT-PTRC-SRC-U
variable HBT-PTRC-OUT-U
variable HBT-PTRU-SRC-U
variable HBT-OPENP-SRC-U
variable HBT-OPENP-OUT-U
variable HBT-LIFE-SRC-U
variable HBT-LIFE-OUT-U
variable HBT-HOOK-SRC-U
variable HBT-HOOK-OUT-U
variable HBT-NUMP-SRC-U
variable HBT-NUMP-OUT-U
variable HBT-MAPC-SRC-U
variable HBT-MAPD-SRC-U
variable HBT-MAPL-SRC-U
variable HBT-MAPL-OUT-U
variable HBT-MAPL-OUT2-U
variable HBT-TWICE-CACHE-U
variable HBT-LITB-SRC-U
variable HBT-LITB-OUT-U
variable HBT-LITC-SRC-U
create HBT-LIB-SRC-BUF FS-PATH-CAP allot
create HBT-LIB-OUT-BUF FS-PATH-CAP allot
create HBT-LIB-DIR-BUF FS-PATH-CAP allot
create HBT-CELLS-SRC-BUF FS-PATH-CAP allot
create HBT-CELLS-OUT-BUF FS-PATH-CAP allot
create HBT-UNOWNED-SRC-BUF FS-PATH-CAP allot
create HBT-PMK-SRC-BUF FS-PATH-CAP allot
create HBT-PPH-SRC-BUF FS-PATH-CAP allot
create HBT-PPH-OUT-BUF FS-PATH-CAP allot
create HBT-TABLE-SRC-BUF FS-PATH-CAP allot
create HBT-CHAIN-SRC-BUF FS-PATH-CAP allot
create HBT-CHAIN-OUT-BUF FS-PATH-CAP allot
create HBT-PTRC-SRC-BUF FS-PATH-CAP allot
create HBT-PTRC-OUT-BUF FS-PATH-CAP allot
create HBT-PTRU-SRC-BUF FS-PATH-CAP allot
create HBT-OPENP-SRC-BUF FS-PATH-CAP allot
create HBT-OPENP-OUT-BUF FS-PATH-CAP allot
create HBT-LIFE-SRC-BUF FS-PATH-CAP allot
create HBT-LIFE-OUT-BUF FS-PATH-CAP allot
create HBT-HOOK-SRC-BUF FS-PATH-CAP allot
create HBT-HOOK-OUT-BUF FS-PATH-CAP allot
create HBT-NUMP-SRC-BUF FS-PATH-CAP allot
create HBT-NUMP-OUT-BUF FS-PATH-CAP allot
create HBT-MAPC-SRC-BUF FS-PATH-CAP allot
create HBT-MAPD-SRC-BUF FS-PATH-CAP allot
create HBT-MAPL-SRC-BUF FS-PATH-CAP allot
create HBT-MAPL-OUT-BUF FS-PATH-CAP allot
create HBT-MAPL-OUT2-BUF FS-PATH-CAP allot
create HBT-TWICE-CACHE-BUF FS-PATH-CAP allot
create HBT-LITB-SRC-BUF FS-PATH-CAP allot
create HBT-LITB-OUT-BUF FS-PATH-CAP allot
create HBT-LITC-SRC-BUF FS-PATH-CAP allot

: HBT-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u dst:ptr lenp:ptr :}
   u FS-PATH-CAP > if E-FS-PATH throw then
   a dst u BYTE-COPY
   u lenp ! ;

: HBT-PATH! ( ptr u8 n ptr u8 n ptr u8 ptr n -- ) {: pa:ptr pu na:ptr nu dst:ptr lenp:ptr :}
   pa pu na nu dst JOIN-PATH lenp ! ;

: HBT-ROOT ( -- ptr u8 n )
   HBT-ROOT-BUF HBT-ROOT-U @ ;

: HBT-TMP ( -- ptr u8 n )
   HBT-TMP-BUF HBT-TMP-U @ ;

: HBT-BAD-SRC ( -- ptr u8 n )
   HBT-BAD-SRC-BUF HBT-BAD-SRC-U @ ;

: HBT-BAD-OUT ( -- ptr u8 n )
   HBT-BAD-OUT-BUF HBT-BAD-OUT-U @ ;

: HBT-REPL-SRC ( -- ptr u8 n )
   HBT-REPL-SRC-BUF HBT-REPL-SRC-U @ ;

: HBT-REPL-OUT ( -- ptr u8 n )
   HBT-REPL-OUT-BUF HBT-REPL-OUT-U @ ;

: HBT-REPL-BAD-SRC ( -- ptr u8 n )
   HBT-REPL-BAD-SRC-BUF HBT-REPL-BAD-SRC-U @ ;

: HBT-REPL-BAD-OUT ( -- ptr u8 n )
   HBT-REPL-BAD-OUT-BUF HBT-REPL-BAD-OUT-U @ ;

: HBT-AOT-SRC ( -- ptr u8 n )
   HBT-AOT-SRC-BUF HBT-AOT-SRC-U @ ;

: HBT-AOT-OUT ( -- ptr u8 n )
   HBT-AOT-OUT-BUF HBT-AOT-OUT-U @ ;

: HBT-EMPTY$ ( -- ptr u8 n )
   SB-RESET
   SB$ ;

: HBT-BAD-SRC$ ( -- ptr u8 n )
   s" : MAIN ( -- ) 0 0 patch32 ;" ;

\ MAIN executes after restoration; loading the source only installs its state.
: HBT-REPL-SRC$ ( -- ptr u8 n )
   S\" package HBT-APP\npublic\n5 constant FIVE\ncreate PAD 8 allot\nvariable SLOT\ndefer APPLY ( n -- n )\n: SQ ( n -- n ) FIVE drop PAD drop SLOT drop dup * ;\n: INC ( n -- n ) 1+ ;\n: INSTALL-APPLY ( -- ) [: INC ;] is APPLY ;\n: SHOW-ARGS ( -- ) SCRIPT-ARGC 0 > if SCRIPT-ARGC . cr 0 SCRIPT-ARGV$ type cr then ;\n: RUN ( -- ) 9 APPLY . cr 9 SQ . cr SHOW-ARGS ;\nINSTALL-APPLY\n;package\n: MAIN ( -- ) HBT-APP:RUN ;\n" ;

: HBT-REPL-BAD-SRC$ ( -- ptr u8 n )
   SB-RESET
   s" : RBAD ( i64 -- i64 ) 0= ;" SB-APPEND
   HBB-LF SB-APPEND-C
   s" EXPORT RBAD" SB-APPEND
   HBB-LF SB-APPEND-C
   SB$ ;

: HBT-AOT-SRC$ ( -- ptr u8 n )
   s" : MAIN ( -- ) ; \ trailing source comment" ;

\ RUN's last line calls NULL$, and that is the point of it: NULL$ is a colon
\ word of the ENGINE's own prefix, so it sits outside this window's code and the
\ capture records a call site for it whose callee no index of this payload can
\ name (habu2.f EMIT-AOT-SITES leaves such a site its name; the seed resolves it
\ in the engine it boots). Every other call here is to a primitive, which binds
\ to a seeded record index, so the two site kinds travel in one built image and
\ a build that can only emit one of them fails this case.
: HBT-AOT-SRC2$ ( -- ptr u8 n )
   S\" package HBT-NATIVE\n: LOADING ( -- ) tier@ 1 <> if -9040 throw then ;\nLOADING\npublic\n: INC ( n -- n ) 1+ ;\n: APPLY ( n [ n -- n ] -- n ) execute ;\n: RUN ( -- ) 41 [: INC ;] APPLY 42 <> if -9041 throw then NULL$ nip 0 <> if -9045 throw then ;\n;package\n: MAIN ( -- ) HBT-NATIVE:RUN ;\n" ;

: HBT-TWICE-CACHE ( -- ptr u8 n )
   HBT-TWICE-CACHE-BUF HBT-TWICE-CACHE-U @ ;

: HBT-MAPL-SRC ( -- ptr u8 n )
   HBT-MAPL-SRC-BUF HBT-MAPL-SRC-U @ ;

: HBT-MAPL-OUT ( -- ptr u8 n )
   HBT-MAPL-OUT-BUF HBT-MAPL-OUT-U @ ;

\ THE MAPPED-CELL PROGRAM WITH ITS ALLOCATION INSIDE MAIN.
\ tools/hb-build-stripped-cells-test.f HBT-MAPC-SRC$ allocates at load time and
\ is refused; this one links, runs and prints. The cell the image carries is
\ the zero it was captured with and the pointer is taken in the new process,
\ which is what the refusal's suggestion names. The printed byte is read back
\ out of the run-time buffer, so the pinned line proves the image reached its
\ own mapping and not merely that it exited zero.
\ tools/hb-build-stripped-cache-test.f HBT-STRIPPED-SAME-TWICE runs its first
\ link against that line and links it a second time for byte equality.
: HBT-MAPL-SRC$ ( -- ptr u8 n )
   SB-RESET
   S\" require lib/memory.f\nPTR-VARIABLE BUF\n: MAIN ( -- )\n" SB-APPEND
   S\"    MEM-ALLOC-64K drop BUF !\n   BUF @ {: p:ptr :}\n" SB-APPEND
   S\"    65 p c!  p 1 type cr ;\n" SB-APPEND
   SB$ ;

: HBT-PREPARE ( -- )
   CLEANUP-RESET
   s" habu-hb-build" HB-TMP-MKDIR {: a:ptr u :}
   a u HBT-ROOT-BUF HBT-ROOT-U HBT-COPY!
   HBT-ROOT CLEANUP-TREE+
   HBT-ROOT s" hbtmp" HBT-TMP-BUF HBT-TMP-U HBT-PATH!
   HBT-TMP MAKE-DIR
   HBT-ROOT s" hbtmp-new" HBT-NEW-TMP-BUF HBT-NEW-TMP-U HBT-PATH!
   HBT-ROOT s" bad.f" HBT-BAD-SRC-BUF HBT-BAD-SRC-U HBT-PATH!
   HBT-ROOT s" bad" HBT-BAD-OUT-BUF HBT-BAD-OUT-U HBT-PATH!
   HBT-ROOT s" repl.f" HBT-REPL-SRC-BUF HBT-REPL-SRC-U HBT-PATH!
   HBT-ROOT s" repl" HBT-REPL-OUT-BUF HBT-REPL-OUT-U HBT-PATH!
   HBT-ROOT s" repl-bad.f" HBT-REPL-BAD-SRC-BUF HBT-REPL-BAD-SRC-U HBT-PATH!
   HBT-ROOT s" repl-bad" HBT-REPL-BAD-OUT-BUF HBT-REPL-BAD-OUT-U HBT-PATH!
   HBT-ROOT s" aot.f" HBT-AOT-SRC-BUF HBT-AOT-SRC-U HBT-PATH!
   HBT-ROOT s" aot" HBT-AOT-OUT-BUF HBT-AOT-OUT-U HBT-PATH!
   HBT-ROOT s" span.f" HBT-SPAN-SRC-BUF HBT-SPAN-SRC-U HBT-PATH!
   HBT-ROOT s" spanimg" HBT-SPAN-OUT-BUF HBT-SPAN-OUT-U HBT-PATH!
   HBT-ROOT s" libstate.f" HBT-LIB-SRC-BUF HBT-LIB-SRC-U HBT-PATH!
   HBT-ROOT s" libstate" HBT-LIB-OUT-BUF HBT-LIB-OUT-U HBT-PATH!
   HBT-ROOT s" libdir" HBT-LIB-DIR-BUF HBT-LIB-DIR-U HBT-PATH!
   HBT-ROOT s" cells.f" HBT-CELLS-SRC-BUF HBT-CELLS-SRC-U HBT-PATH!
   HBT-ROOT s" cells" HBT-CELLS-OUT-BUF HBT-CELLS-OUT-U HBT-PATH!
   HBT-ROOT s" unowned.f" HBT-UNOWNED-SRC-BUF HBT-UNOWNED-SRC-U HBT-PATH!
   HBT-ROOT s" ptrmark.f" HBT-PMK-SRC-BUF HBT-PMK-SRC-U HBT-PATH!
   HBT-ROOT s" pph.f" HBT-PPH-SRC-BUF HBT-PPH-SRC-U HBT-PATH!
   HBT-ROOT s" pph" HBT-PPH-OUT-BUF HBT-PPH-OUT-U HBT-PATH!
   HBT-ROOT s" table.f" HBT-TABLE-SRC-BUF HBT-TABLE-SRC-U HBT-PATH!
   HBT-ROOT s" chain.f" HBT-CHAIN-SRC-BUF HBT-CHAIN-SRC-U HBT-PATH!
   HBT-ROOT s" chain" HBT-CHAIN-OUT-BUF HBT-CHAIN-OUT-U HBT-PATH!
   HBT-ROOT s" ptrcell.f" HBT-PTRC-SRC-BUF HBT-PTRC-SRC-U HBT-PATH!
   HBT-ROOT s" ptrcell" HBT-PTRC-OUT-BUF HBT-PTRC-OUT-U HBT-PATH!
   HBT-ROOT s" ptrunowned.f" HBT-PTRU-SRC-BUF HBT-PTRU-SRC-U HBT-PATH!
   HBT-ROOT s" openpath.f" HBT-OPENP-SRC-BUF HBT-OPENP-SRC-U HBT-PATH!
   HBT-ROOT s" openpath" HBT-OPENP-OUT-BUF HBT-OPENP-OUT-U HBT-PATH!
   HBT-ROOT s" lifecycle.f" HBT-LIFE-SRC-BUF HBT-LIFE-SRC-U HBT-PATH!
   HBT-ROOT s" lifecycle" HBT-LIFE-OUT-BUF HBT-LIFE-OUT-U HBT-PATH!
   HBT-ROOT s" lifehook.f" HBT-HOOK-SRC-BUF HBT-HOOK-SRC-U HBT-PATH!
   HBT-ROOT s" lifehook" HBT-HOOK-OUT-BUF HBT-HOOK-OUT-U HBT-PATH!
   HBT-ROOT s" numparse.f" HBT-NUMP-SRC-BUF HBT-NUMP-SRC-U HBT-PATH!
   HBT-ROOT s" numparse" HBT-NUMP-OUT-BUF HBT-NUMP-OUT-U HBT-PATH!
   HBT-ROOT s" mapcell.f" HBT-MAPC-SRC-BUF HBT-MAPC-SRC-U HBT-PATH!
   HBT-ROOT s" mapdecl.f" HBT-MAPD-SRC-BUF HBT-MAPD-SRC-U HBT-PATH!
   HBT-ROOT s" maplate.f" HBT-MAPL-SRC-BUF HBT-MAPL-SRC-U HBT-PATH!
   HBT-ROOT s" maplate" HBT-MAPL-OUT-BUF HBT-MAPL-OUT-U HBT-PATH!
   HBT-ROOT s" maplate2" HBT-MAPL-OUT2-BUF HBT-MAPL-OUT2-U HBT-PATH!
   HBT-ROOT s" cache-twice" HBT-TWICE-CACHE-BUF HBT-TWICE-CACHE-U HBT-PATH!
   HBT-ROOT s" litbody.f" HBT-LITB-SRC-BUF HBT-LITB-SRC-U HBT-PATH!
   HBT-ROOT s" litbody" HBT-LITB-OUT-BUF HBT-LITB-OUT-U HBT-PATH!
   HBT-ROOT s" litcell.f" HBT-LITC-SRC-BUF HBT-LITC-SRC-U HBT-PATH!
   HBT-TWICE-CACHE MAKE-DIR
   HBT-BAD-SRC HBT-BAD-SRC$ WRITE-ALL
   HBT-REPL-SRC HBT-REPL-SRC$ WRITE-ALL
   HBT-REPL-BAD-SRC HBT-REPL-BAD-SRC$ WRITE-ALL
   HBT-AOT-SRC HBT-AOT-SRC$ WRITE-ALL
   BUILD-CACHE:RESET
   HBT-TMP BUILD-CACHE:ROOT! ;

\ THE ENGINE A BUILD'S CHILD RUNS. Before it reaches its subject a maker child
\ compiles the AOT linker, 5.1 to 6.8 s of each one measured alone, and an
\ app-build child compiles the image saver, 3.3 s. A row whose subjects need
\ neither load hands HBT-KEYED! the keyed images that hold them
\ (test/preloaded-engine.f LINKER$, test/app-image-engine.f PATH$) before it
\ stages any argv. Its AOT builds, maker refusals and CLI spawns' makers then
\ run the production maker script on the linker image, and its REPL builds, app
\ refusals and REPL CLI spawns (HBT-ARGV-BASE-REPL) run on the saver image
\ (docs/gate.md). A row with no maker to run hands an empty linker and loads
\ only the saver's module. The -SOURCE words keep one build on the engine,
\ which compiles the linker above the application: for a subject that needs a
\ module in the linker's lib closure, which the linker image refuses by name
\ (test/preloaded-engine.f rule 3), and for a case about that order. A row that
\ records no image builds every program on the engine.
: HBT-KEYED! ( ptr u8 n ptr u8 n -- ) {: linker:ptr linkeru:n saver:ptr saveru:n :}
   linker linkeru HBT-LINKER-BUF HBT-LINKER-U HBT-COPY!
   saver saveru HBT-SAVER-BUF HBT-SAVER-U HBT-COPY! ;

: HBT-LINKER ( -- ptr u8 n )
   HBT-LINKER-BUF HBT-LINKER-U @ ;

: HBT-SAVER ( -- ptr u8 n )
   HBT-SAVER-BUF HBT-SAVER-U @ ;

: HBT-ENGINE! ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0= if BF-ENGINE-RESET exit then
   a u BF-ENGINE! ;

\ The CLI as docs/native-applications.md documents it: tools/hb-build.f alone
\ requires its library, so every row spawns the command a user runs. Its maker
\ or app-build child runs the image named second, unless that is empty.
: HBT-ARGV-CLI ( ptr u8 n ptr u8 n -- ) {: tmp:ptr tmpu:n eng:ptr engu:n :}
   PROC-ARGV-RESET
   PROC-ENV-RESET
   s" HB_TMP" >LEN tmp tmpu >LEN PROC-ENV+
   s" HABU_BUILD_CACHE" >LEN HBT-TMP >LEN PROC-ENV+
   engu 0 > if
      s" HABU_FIXPOINT_ENGINE" >LEN eng engu >LEN PROC-ENV+
   then
   PROC-ENV-INHERIT-MISSING
   s" --load"  >LEN PROC-ARGV+
   s" tools/hb-build.f"  >LEN PROC-ARGV+
   s" --"  >LEN PROC-ARGV+ ;

: HBT-ARGV-BASE-TMP ( ptr u8 n -- )
   HBT-LINKER HBT-ARGV-CLI ;

: HBT-ARGV-BASE ( -- )
   HBT-TMP HBT-ARGV-BASE-TMP ;

\ A --repl build's CLI, whose app-build child runs the saver image.
: HBT-ARGV-BASE-REPL ( -- )
   HBT-TMP HBT-SAVER HBT-ARGV-CLI ;

: HBT-TIMEOUT-ENV ( ptr u8 n -- )
   PROC-ENV-RESET
   s" HB_TMP" >LEN HBT-TMP >LEN PROC-ENV+
   s" HABU_BUILD_CACHE" >LEN HBT-TMP >LEN PROC-ENV+
   s" HABU_FIXPOINT_ENGINE" >LEN HBT-AOT-OUT >LEN PROC-ENV+
   s" HB_BUILD_TIMEOUT_MS" >LEN 2swap >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING ;

: HBT-ADD-TIMEOUT-ARGS ( -- )
   HBT-AOT-SRC >LEN PROC-ARGV+
   s" -o" >LEN PROC-ARGV+
   HBT-REPL-BAD-OUT >LEN PROC-ARGV+ ;

: HBT-CAPTURE>N ( result<pcap:captured,pcap:failed> -- n n n )   \ outn errn code (0 on clean exit)
   MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE {: o:len e:len :} o LEN>N e LEN>N 0 ENDOF
     err OF PCAP-FAILED:UNMAKE  {: o:len e:len c:rc :} o LEN>N e LEN>N c RC>N ENDOF
   ;MATCH ;

: HBT-RUN-HB-BUILD ( -- n n n )
   s" bin/hb" >LEN HBT-OUT HBT-CAPTURE-CAP >LEN HBT-ERR HBT-CAPTURE-CAP >LEN
   HBT-TIMEOUT-MS >MS RUN-ARGV-ENV-CAPTURE
   HBT-CAPTURE>N ;

: HBT-REMOVE-FILE? ( ptr u8 n -- )
   2dup FILE? if REMOVE-FILE else 2drop then ;

\ The first byte two spans differ at, or -1 for identical. A pinned -1 names the
\ offset on failure instead of printing two images into the capture.
: HBT-DIFF-AT ( ptr u8 n ptr u8 n -- n ) {: a:ptr au:n b:ptr bu:n :}
   au bu min 0 ?do
      a i + c@  b i + c@ <> if i unloop exit then
   loop
   au bu <> if au bu min exit then
   -1 ;

: HBT-REPL-EXPECTED$ ( -- ptr u8 n )
   SB-RESET
   s" 10" SB-APPEND
   HBB-LF SB-APPEND-C
   HBB-LF SB-APPEND-C
   s" 81" SB-APPEND
   HBB-LF SB-APPEND-C
   HBB-LF SB-APPEND-C
   SB$ ;

: HBT-RUN-REPL ( -- )
   HBT-REPL-OUT >LEN HBT-RUN-OUT HBT-CAPTURE-CAP >LEN HBT-RUN-ERR HBT-CAPTURE-CAP >LEN
   HBT-TIMEOUT-MS >MS RUN-CAPTURE HBT-CAPTURE>N {: outn errn rcn :}
   rcn 0 <> if s" repl rc: " type rcn . cr HBT-RUN-OUT outn type HBT-RUN-ERR errn type then
   rcn 0 T=
   HBT-RUN-ERR errn HBT-EMPTY$ T$=
   HBT-RUN-OUT outn HBT-REPL-EXPECTED$ T$= ;

: HBT-HBB-PREPARE-REPL ( ptr u8 n ptr u8 n -- )
   HBB-RESET-OPTIONS
   HBB-REPL-ON
   HBB-PATHS!
   HBT-TMP BF-TMP!
   HBT-SAVER HBT-ENGINE! ;

: HBT-HBB-PREPARE-AOT ( ptr u8 n ptr u8 n -- )
   HBB-RESET-OPTIONS
   HBB-PATHS!
   HBT-TMP BF-TMP!
   HBT-LINKER HBT-ENGINE! ;

: HBT-HBB-PREPARE-AOT-SOURCE ( ptr u8 n ptr u8 n -- )
   HBT-HBB-PREPARE-AOT
   BF-ENGINE-RESET ;

\ A program that builds is built in this process: HBB-BUILD runs what
\ tools/hb-build.f runs once it has parsed argv (for an AOT build the lint
\ child, the maker, the object store and the install; for a REPL build the
\ app-build child and the install), without the library load a CLI spawn
\ pays before it reads argv. What the CLI wraps around them does not
\ run here: HBB-PREPARE-TMP's private directory (the children's HB_TMP is
\ HBT-TMP), HBB-BUILD-CLI's exit mapping and HBB-CLEANUP. A refusal ends the
\ row in a die, the child's diagnostic on stderr: a lint refusal in
\ HBB-FINISH-TOOL's, a maker refusal in HBB-FINISH-MAKER's. A row spawns the
\ CLI only where the CLI is the subject: its options (CLI-REPORT,
\ BUILD-AOT-PRESEED), a missing HB_TMP made and a lint refusal passed through
\ (HBT-BUILD-MISSING-TMP), the cache path error's exit and JSON bytes
\ (tools/hb-build-cli-errors-test.f), a maker refusal passed through (below)
\ and the maker deadline and its override (the timeout rows).
: HBT-HBB-BUILD-OUT ( -- )
   HBB-BUILD
   BF-TMP-RESET ;

\ A program the link refuses goes through the same lint child and the same
\ maker invocation, HBB-RUN-MAKER-CMD, but keeps the maker's ( outu erru rc )
\ where HBB-FINISH-MAKER would die, so its code and its diagnostic (the maker
\ writes it to stderr, into HBB-ERR-BUF) are asserted on the child that made
\ them. HBT-RUN-APP does the same for the REPL build's app-build child,
\ HBB-RUN-APP-CMD. A timeout still throws through HBB-MAKER-TIMED-OUT with its
\ captured diagnostic. That the CLI passes a refusal through - the child's
\ code, its stderr byte for byte, nothing on stdout, no image installed - is
\ asserted once per build path: tools/hb-build-stripped-test.f
\ HBT-STRIPPED-NO-ENTRY for AOT, tools/hb-build-cli-errors-test.f
\ HBT-REFUSE-MAIN-CLI for REPL.
: HBT-MAKER-CAPTURE>N ( len len outcome -- n n n )
   MATCH outcome
      exited OF {: outu:len erru:len rc:n :}
         outu LEN>N erru LEN>N rc ENDOF
      signaled OF {: outu:len erru:len sig:n :}
         outu LEN>N erru LEN>N sig 128 + ENDOF
      timeout OF LEN>N swap LEN>N swap HBB-MAKER-TIMED-OUT ENDOF
   ;MATCH ;

: HBT-MAKER-RUN ( ptr u8 n -- n n n )
   HBB-RESET-OPTIONS
   HBB-SRC!
   HBT-TMP BF-TMP!
   HBB-BUILD-BEGIN
   HBB-RUN-MAKER-CMD HBT-MAKER-CAPTURE>N
   BF-TMP-RESET ;

: HBT-RUN-MAKER ( ptr u8 n -- n n n )
   HBT-LINKER HBT-ENGINE!
   HBT-MAKER-RUN ;

: HBT-RUN-MAKER-SOURCE ( ptr u8 n -- n n n )
   BF-ENGINE-RESET
   HBT-MAKER-RUN ;

: HBT-RUN-APP ( ptr u8 n -- n n n )
   HBT-SAVER HBT-ENGINE!
   HBB-RESET-OPTIONS
   HBB-REPL-ON
   HBB-SRC!
   HBT-TMP BF-TMP!
   HBB-BUILD-BEGIN
   HBB-RUN-APP-CMD HBT-MAKER-CAPTURE>N
   BF-TMP-RESET ;

: HBT-REMOVE-AOT-OUT ( -- )
   HBT-AOT-OUT HBT-REMOVE-FILE? ;

: HBT-REMOVE-ARTIFACT ( -- )
   HBB-ARTIFACT$ HBT-REMOVE-FILE? ;

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

\ The object cache key is the ordered-closure hex of the source (a self-contained
\ AOT source closes over only itself), so the fixtures that pre-store objects must
\ key them the same way hb-build now does.
: HBT-AOT-HEX! ( -- )
   HBT-AOT-SRC HBB-SRC!
   HBB-SRC-CLOSURE-HEX!
   HBB-SRC-CLOSURE-HEX HBT-AOT-HEX 64 BYTE-COPY ;

: HBT-OBJ-LOAD? ( -- bool )
   HBB-RESET-OPTIONS
   HBT-TMP OBJRES:ROOT!
   HBT-AOT-HEX!
   HBT-AOT-HEX HBT-KEY-U HBB-TARGET-ABI$ HBB-CHECKER-ABI$ HBB-COMPILER-ABI$ OBJRES:LOAD ;

: HBT-RUN-AOT ( -- )
   HBT-AOT-OUT >LEN HBT-RUN-OUT HBT-CAPTURE-CAP >LEN HBT-RUN-ERR HBT-CAPTURE-CAP >LEN
   HBT-TIMEOUT-MS >MS RUN-CAPTURE HBT-CAPTURE>N {: outn:n errn:n rcn:n :}
   rcn 0 T=
   outn 0 T=
   errn 0 T= ;

;package

;using
