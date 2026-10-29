\ hb-build-test-lib.f - the fixture the hb-build gate rows share: the scratch
\ tree, the fixture sources, HBT-PREPARE, the hb-build runners and the report
\ check. It defines no MAIN; each row listed below loads it and runs its own
\ groups. test/gate-stdlib-cases.f registers each row and says why.

require src/lib/errors.f
require src/lib/string.f
require src/lib/test.f
require src/lib/memory.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/source.f
require src/lib/build.f
require src/lib/codesign.f
require src/lib/content-key.f
require src/lib/json-write.f
require src/lib/float.f
require src/lib/json-read.f
require tools/build-fixpoint.f
require tools/hb-build-report.f
require tools/hb-build-lib.f
require src/lib/engine-candidate.f
require src/lib/fmt.f                        \ FMT:.INT

using BUILD-FIXPOINT                     \ the build tmp root

\ The hb-build rows, each loading this file and running the groups it owns:
\   tools/hb-build-test.f                     CLI REPL build and report,
\                                             rejected inputs, image size,
\                                             large sources, literal bodies
\   tools/hb-build-cli-errors-test.f          cache path error, refused paths
\                                             and entry names, MAIN effects
\   tools/hb-build-timeout-test.f             maker deadlines and diagnostics
\   tools/hb-build-timeout-env-test.f         deadline override validation
\   tools/hb-build-timeout-json-test.f        empty override, JSON refusal
\   tools/hb-build-aot-test.f                 AOT build and run, one program each
\   tools/hb-build-aot-inputs-test.f          observed source and environment inputs
\   tools/hb-build-aot-image-test.f           saved AOT builder, reused maker image
\   tools/hb-build-stripped-test.f            library state, a fork, engine cells,
\                                             quotation fields, ptr mark
\   tools/hb-build-stripped-chain-test.f      baked constants, chain, open path
\   tools/hb-build-stripped-lifecycle-test.f  lifecycle registry, number parsing
\   tools/hb-build-stripped-cells-test.f      mapped cells, uncarried table
\   tools/hb-build-stripped-cache-test.f      link equality, cached table cells
\   tools/hb-build-repl-twin-test.f           two REPL builds, restored LASTC
\   tools/hb-build-repl-fresh-test.f          no-op output, dynamic child edits
\   tools/hb-build-repl-image-test.f          saved REPL builder, CWD shadow, tier

\ Reopens HB-BUILD-CLI to drive its internals: exporting them would widen the
\ library's interface for its own test.
package HB-BUILD-CLI

65536 constant HBT-CAPTURE-CAP
600000 constant HBT-TIMEOUT-MS

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

create HBT-ROOT-BUF PATH-CAP allot
create HBT-TMP-BUF PATH-CAP allot
create HBT-NEW-TMP-BUF PATH-CAP allot
create HBT-BAD-SRC-BUF PATH-CAP allot
create HBT-BAD-OUT-BUF PATH-CAP allot
create HBT-REPL-SRC-BUF PATH-CAP allot
create HBT-REPL-OUT-BUF PATH-CAP allot
create HBT-REPL-BAD-SRC-BUF PATH-CAP allot
create HBT-REPL-BAD-OUT-BUF PATH-CAP allot
create HBT-AOT-SRC-BUF PATH-CAP allot
create HBT-AOT-OUT-BUF PATH-CAP allot
create HBT-SPAN-SRC-BUF PATH-CAP allot
create HBT-SPAN-OUT-BUF PATH-CAP allot

create HBT-OUT HBT-CAPTURE-CAP allot
create HBT-ERR HBT-CAPTURE-CAP allot
create HBT-RUN-OUT HBT-CAPTURE-CAP allot
create HBT-RUN-ERR HBT-CAPTURE-CAP allot

create HBT-REPORT-BUF PATH-CAP allot

\ The engine a row's AOT builds run on (HBT-KEYED!); empty, the engine.
create HBT-LINKER-BUF PATH-CAP allot
variable HBT-LINKER-U

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
variable HBT-LIFE-SRC-U
variable HBT-LIFE-OUT-U
variable HBT-HOOK-SRC-U
variable HBT-HOOK-OUT-U
variable HBT-MAPC-SRC-U
variable HBT-MAPD-SRC-U
variable HBT-MAPL-SRC-U
variable HBT-MAPL-OUT-U
variable HBT-MAPL-OUT2-U
variable HBT-LITB-SRC-U
variable HBT-LITB-OUT-U
variable HBT-LITC-SRC-U
create HBT-LIB-SRC-BUF PATH-CAP allot
create HBT-LIB-OUT-BUF PATH-CAP allot
create HBT-LIB-DIR-BUF PATH-CAP allot
create HBT-CELLS-SRC-BUF PATH-CAP allot
create HBT-CELLS-OUT-BUF PATH-CAP allot
create HBT-UNOWNED-SRC-BUF PATH-CAP allot
create HBT-PMK-SRC-BUF PATH-CAP allot
create HBT-PPH-SRC-BUF PATH-CAP allot
create HBT-PPH-OUT-BUF PATH-CAP allot
create HBT-TABLE-SRC-BUF PATH-CAP allot
create HBT-CHAIN-SRC-BUF PATH-CAP allot
create HBT-CHAIN-OUT-BUF PATH-CAP allot
create HBT-PTRC-SRC-BUF PATH-CAP allot
create HBT-PTRC-OUT-BUF PATH-CAP allot
create HBT-PTRU-SRC-BUF PATH-CAP allot
create HBT-LIFE-SRC-BUF PATH-CAP allot
create HBT-LIFE-OUT-BUF PATH-CAP allot
create HBT-HOOK-SRC-BUF PATH-CAP allot
create HBT-HOOK-OUT-BUF PATH-CAP allot
create HBT-MAPC-SRC-BUF PATH-CAP allot
create HBT-MAPD-SRC-BUF PATH-CAP allot
create HBT-MAPL-SRC-BUF PATH-CAP allot
create HBT-MAPL-OUT-BUF PATH-CAP allot
create HBT-MAPL-OUT2-BUF PATH-CAP allot
create HBT-LITB-SRC-BUF PATH-CAP allot
create HBT-LITB-OUT-BUF PATH-CAP allot
create HBT-LITC-SRC-BUF PATH-CAP allot

: HBT-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u dst:ptr lenp:ptr :}
   u PATH-CAP > if E-FS-PATH throw then
   a dst u BYTE-COPY
   u lenp ! ;

: HBT-PATH! ( ptr u8 n ptr u8 n ptr u8 ptr n -- ) {: pa:ptr pu na:ptr nu dst:ptr lenp:ptr :}
   pa pu na nu dst FS:JOIN-PATH lenp ! ;

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
   s" TRUSTED: MAIN ( -- ) 0 0 patch32 ;" ;

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
   s" create HBT-DATA 41 c, : MAIN ( -- ) HBT-DATA c@ 41 <> if -9046 throw then ; \ trailing source comment" ;

\ NULL$ is a colon word of the engine's own prefix, outside this window's code:
\ its call site has no callee index in this payload (habu2.f EMIT-AOT-SITES
\ leaves it its name; the seed resolves it in the engine it boots). Every other
\ call is to a primitive, bound to a seeded record index, so a build that can
\ emit only one of the two site kinds fails this case.
: HBT-AOT-SRC2$ ( -- ptr u8 n )
   S\" package HBT-NATIVE\n: LOADING ( -- ) tier@ 1 <> if -9040 throw then ;\nLOADING\npublic\n: INC ( n -- n ) 1+ ;\n: APPLY ( n [ n -- n ] -- n ) execute ;\n: RUN ( -- ) 41 [: INC ;] APPLY 42 <> if -9041 throw then NULL$ nip 0 <> if -9045 throw then ;\n;package\n: MAIN ( -- ) HBT-NATIVE:RUN ;\n" ;

: HBT-MAPL-SRC ( -- ptr u8 n )
   HBT-MAPL-SRC-BUF HBT-MAPL-SRC-U @ ;

: HBT-MAPL-OUT ( -- ptr u8 n )
   HBT-MAPL-OUT-BUF HBT-MAPL-OUT-U @ ;

\ Allocates inside MAIN, unlike tools/hb-build-stripped-cells-test.f
\ HBT-MAPC-SRC$, which allocates at load time and is refused: the image carries
\ the captured zero and the pointer is taken in the new process. The printed
\ byte is read back from the run-time buffer, so the pinned line proves the
\ image reached its own mapping. HBT-STRIPPED-SAME-TWICE links it twice for byte
\ equality. HBT-CALL is bound at load time: its producer address must stay out
\ of the sparse DATA payload, since startup restores it from the stable XT row.
: HBT-MAPL-SRC$ ( -- ptr u8 n )
   SB-RESET
   S\" require src/lib/memory.f\npackage HBT-ATTR\n: HIDDEN ( -- n ) 65 ;\npublic\nEXPORT HIDDEN\n: PUBLIC ( -- n ) HIDDEN ;\n;package\n: HBT-GLOBAL ( -- n ) HBT-ATTR:PUBLIC ;\n: HBT-DEAD ( -- n ) 99 ;\n" SB-APPEND
   S\" defer HBT-CALL ( -- n )\n: HBT-BIND ( -- ) [: HBT-GLOBAL ;] is HBT-CALL ;\nHBT-BIND\nPTR-VARIABLE BUF\n: MAIN ( -- )\n" SB-APPEND
   S\"    MEM-ALLOC-64K drop BUF !\n   BUF @ {: p:ptr :}\n" SB-APPEND
   S\"    [: 0 ;] execute drop\n   HBT-CALL p c!  p 1 type cr\n" SB-APPEND
   S\"    MEM:ALLOC-64K {: q:ptr cap:NUM:alloc-byte-len :}\n" SB-APPEND
   S\"    q cap MEM:RELEASE-BYTES ;\n" SB-APPEND
   SB$ ;

: HBT-PREPARE ( -- )
   FS:CLEANUP-RESET
   s" habu-hb-build" FS:MKTEMP-DIR {: a:ptr u :}
   a u HBT-ROOT-BUF HBT-ROOT-U HBT-COPY!
   HBT-ROOT FS:CLEANUP-TREE+
   HBT-ROOT s" hbtmp" HBT-TMP-BUF HBT-TMP-U HBT-PATH!
   HBT-TMP FS:MAKE-DIR
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
   HBT-ROOT s" lifecycle.f" HBT-LIFE-SRC-BUF HBT-LIFE-SRC-U HBT-PATH!
   HBT-ROOT s" lifecycle" HBT-LIFE-OUT-BUF HBT-LIFE-OUT-U HBT-PATH!
   HBT-ROOT s" lifehook.f" HBT-HOOK-SRC-BUF HBT-HOOK-SRC-U HBT-PATH!
   HBT-ROOT s" lifehook" HBT-HOOK-OUT-BUF HBT-HOOK-OUT-U HBT-PATH!
   HBT-ROOT s" mapcell.f" HBT-MAPC-SRC-BUF HBT-MAPC-SRC-U HBT-PATH!
   HBT-ROOT s" mapdecl.f" HBT-MAPD-SRC-BUF HBT-MAPD-SRC-U HBT-PATH!
   HBT-ROOT s" maplate.f" HBT-MAPL-SRC-BUF HBT-MAPL-SRC-U HBT-PATH!
   HBT-ROOT s" maplate" HBT-MAPL-OUT-BUF HBT-MAPL-OUT-U HBT-PATH!
   HBT-ROOT s" maplate2" HBT-MAPL-OUT2-BUF HBT-MAPL-OUT2-U HBT-PATH!
   HBT-ROOT s" litbody.f" HBT-LITB-SRC-BUF HBT-LITB-SRC-U HBT-PATH!
   HBT-ROOT s" litbody" HBT-LITB-OUT-BUF HBT-LITB-OUT-U HBT-PATH!
   HBT-ROOT s" litcell.f" HBT-LITC-SRC-BUF HBT-LITC-SRC-U HBT-PATH!
   HBT-BAD-SRC HBT-BAD-SRC$ FS:WRITE-ALL
   HBT-REPL-SRC HBT-REPL-SRC$ FS:WRITE-ALL
   HBT-REPL-BAD-SRC HBT-REPL-BAD-SRC$ FS:WRITE-ALL
   HBT-AOT-SRC HBT-AOT-SRC$ FS:WRITE-ALL ;

\ The engine HBT-KEYED! records runs a row's AOT builds, maker refusals and CLI
\ makers: the linker image (test/linker-builder-engine.f HOST$) for a row whose
\ subjects test/preloaded-engine.f rule 3 admits there, else the engine. REPL
\ builds, app refusals, HBT-ARGV-BASE-REPL spawns and HBT-RUN-APP run on
\ ENGINE-CANDIDATE:PATH$. Keyed builders are the family images the row's load
\ closure grants, built once in the process's out root (tools/build-out.f,
\ test/image-grant.f); a case that overrides that root says so beside it.
\ HBT-HBB-PREPARE-AOT-SOURCE builds on the engine: for a subject that reaches a
\ word or unclaimed cell of a linker lib module the engine does not bake (the
\ linker image refuses it by name, rule 3) and for a case about that order.
\ A subject that only requires such a module links on the image.
: HBT-KEYED! ( ptr u8 n -- )
   HBT-LINKER-BUF HBT-LINKER-U HBT-COPY! ;

: HBT-LINKER ( -- ptr u8 n )
   HBT-LINKER-BUF HBT-LINKER-U @ ;

: HBT-ENGINE! ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0= if BF-ENGINE-RESET exit then
   a u BF-ENGINE! ;

\ The CLI as tools/hb-build.f's header documents it: tools/hb-build.f alone
\ requires its library, so every row spawns the command a user runs. Its maker
\ or app-build child runs the engine named second, unless that is empty.
: HBT-ARGV-CLI ( ptr u8 n ptr u8 n -- )
   {: tmp:ptr tmpu:n eng:ptr engu:n :}
   PROC-ARGV-RESET
   PROC-ENV-RESET
   s" TMPDIR" >LEN tmp tmpu >LEN PROC-ENV+
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

\ A --repl build's CLI, whose app-build child runs the product engine.
: HBT-ARGV-BASE-REPL ( -- )
   HBT-TMP ENGINE-CANDIDATE:PATH$ HBT-ARGV-CLI ;

: HBT-TIMEOUT-ENV ( ptr u8 n -- )
   PROC-ENV-RESET
   s" TMPDIR" >LEN HBT-TMP >LEN PROC-ENV+
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
   2dup FS:FILE? if FS:REMOVE-FILE else 2drop then ;

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

\ Leave in the string builder what test/tier0-product-subject.f's MAIN prints
\ when the program loaded at TIER: the text every tier prints, then the tier.
: HBT-SUBJECT-OUT ( n -- )
   SB-RESET
   S\" 18\n\n5\n\n7\n\n8\n\n0\n\n4\n\n-9081\n\n10\n\n15\n\n10\n\n7\n\n40\n\n3.500000\n\n3\n\n00004444555566667777PPPPTTTTpppp\n00004444555566667777PPPPTTTTpppp\n00004444555566667777\tPPPPTTTTpppp\n00004444555566667777PPPPTTTTpppp\n00004444555566667777\tPPPPTTTTpppp\n00004444555566667777\tPPPPTTTTpppp\n" SB-APPEND
   0= if S\" 0\n\n" else S\" 1\n\n" then SB-APPEND ;

: HBT-RUN-REPL ( -- )
   HBT-REPL-OUT >LEN HBT-RUN-OUT HBT-CAPTURE-CAP >LEN HBT-RUN-ERR HBT-CAPTURE-CAP >LEN
   HBT-TIMEOUT-MS >MS RUN-CAPTURE HBT-CAPTURE>N {: outn errn rcn :}
   rcn 0 <> if s" repl rc: " type rcn FMT:.INT cr HBT-RUN-OUT outn type HBT-RUN-ERR errn type then
   rcn 0 T=
   HBT-RUN-ERR errn HBT-EMPTY$ T$=
   HBT-RUN-OUT outn HBT-REPL-EXPECTED$ T$= ;

: HBT-HBB-PREPARE-REPL ( ptr u8 n ptr u8 n -- )
   HBB-RESET-OPTIONS
   HBB-REPL-ON
   HBB-PATHS!
   HBT-TMP BF-TMP!
   ENGINE-CANDIDATE:PATH$ BF-ENGINE! ;

: HBT-HBB-PREPARE-AOT ( ptr u8 n ptr u8 n -- )
   HBB-RESET-OPTIONS
   HBB-PATHS!
   HBT-TMP BF-TMP!
   HBT-LINKER HBT-ENGINE! ;

: HBT-HBB-PREPARE-AOT-SOURCE ( ptr u8 n ptr u8 n -- )
   HBT-HBB-PREPARE-AOT
   BF-ENGINE-RESET ;

\ HBB-BUILD runs here what tools/hb-build.f runs after parsing argv, without
\ the library load a CLI spawn pays. HBB-PREPARE-TMP's directory (the children's
\ TMPDIR is HBT-TMP), HBB-BUILD-CLI's exit mapping and HBB-CLEANUP do not run
\ here, and a refusal ends the row in a die. A row spawns the CLI only where
\ the CLI is the subject.
: HBT-HBB-BUILD-OUT ( -- )
   HBB-BUILD
   BF-TMP-RESET ;

\ Keeps the maker's ( outu erru rc ) where HBB-FINISH-MAKER would die, so a link
\ refusal's code and stderr (HBB-ERR-BUF) are asserted on the child that made
\ them; HBT-RUN-APP does the same for HBB-RUN-APP-CMD. A timeout still throws
\ through HBB-MAKER-TIMED-OUT. The CLI's pass-through of a refusal is asserted
\ once per build path: tools/hb-build-stripped-test.f HBT-STRIPPED-NO-ENTRY
\ (AOT), tools/hb-build-cli-errors-test.f HBT-REFUSE-MAIN-CLI (REPL).
: HBT-MAKER-CAPTURE>N ( len len outcome -- n n n )
   MATCH outcome
      exited OF {: outu:len erru:len rc:n :}
         outu LEN>N erru LEN>N rc ENDOF
      signaled OF {: outu:len erru:len sig:n :}
         outu LEN>N erru LEN>N sig 128 + ENDOF
      timeout OF LEN>N swap LEN>N swap HBB-MAKER-TIMED-OUT ENDOF
   ;MATCH ;

: HBT-RUN-MAKER ( ptr u8 n -- n n n )
   HBT-LINKER HBT-ENGINE!
   HBB-RESET-OPTIONS
   HBB-SRC!
   HBT-TMP BF-TMP!
   HBB-BUILD-BEGIN
   HBB-MAKER-KEY!
   HBB-RUN-MAKER-CMD HBT-MAKER-CAPTURE>N
   BF-TMP-RESET ;

: HBT-RUN-APP ( ptr u8 n -- n n n )
   ENGINE-CANDIDATE:PATH$ BF-ENGINE!
   HBB-RESET-OPTIONS
   HBB-REPL-ON
   HBB-SRC!
   HBT-TMP BF-TMP!
   HBB-BUILD-BEGIN
   HBB-RUN-APP-CMD HBT-MAKER-CAPTURE>N
   BF-TMP-RESET ;

: HBT-REMOVE-AOT-OUT ( -- )
   HBT-AOT-OUT HBT-REMOVE-FILE? ;

TYPED-VARIABLE READER-STATE JR:cursor

: REPORT-STRING= ( JR:reader ptr u8 n -- JR:reader ) {: want:ptr wantu:n :}
   JR:TOKEN JR:T-STR T=
   HBT-REPORT-BUF PATH-CAP JR:STR {: gotu:n :}
   HBT-REPORT-BUF gotu want wantu T$= ;

: CHECK-REPORT ( ptr u8 n n n n -- )
   {: a:ptr u:n maker:n built:n ran:n :}
   READER-STATE a u JR:INIT
   JR:NEXT JR:T-OBJ T=
   s" schema" JR:FIND-KEY TTRUE
   s" hb-build-report" REPORT-STRING=
   s" version" JR:FIND-KEY TTRUE
   JR:TOKEN JR:T-INT T=
   JR:INT 1 T=
   s" maker_hit" JR:FIND-KEY TTRUE
   JR:TOKEN maker T=
   s" maker_built" JR:FIND-KEY TTRUE
   JR:TOKEN built T=
   s" maker_ran" JR:FIND-KEY TTRUE
   JR:TOKEN ran T=
   \ maker_ran is the AOT maker's trace: a REPL build reports it false and still
   \ counts the files its own maker loaded.
   s" files_reused" JR:FIND-KEY TTRUE
   JR:TOKEN JR:T-INT T=
   s" files_compiled" JR:FIND-KEY TTRUE
   JR:TOKEN JR:T-INT T=
   s" elapsed_ns" JR:FIND-KEY TTRUE
   JR:TOKEN JR:T-INT T=
   JR:INT 0 >= TTRUE
   JR:CLOSE ;

: HBT-RUN-AOT ( -- )
   HBT-AOT-OUT >LEN HBT-RUN-OUT HBT-CAPTURE-CAP >LEN HBT-RUN-ERR HBT-CAPTURE-CAP >LEN
   HBT-TIMEOUT-MS >MS RUN-CAPTURE HBT-CAPTURE>N {: outn:n errn:n rcn:n :}
   rcn 0 T=
   outn 0 T=
   errn 0 T= ;

;package

;using
