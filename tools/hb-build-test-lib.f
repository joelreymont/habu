\ hb-build-test-lib.f - the fixture the three hb-build gate rows share.
\ Loaded by tools/hb-build-test.f, tools/hb-build-stripped-test.f and
\ tools/hb-build-stripped-cells-test.f: the scratch tree, the fixture sources,
\ HBT-PREPARE and the helpers that run hb-build. It defines no MAIN; each row
\ file runs the groups it owns.

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

\ This fixture drives the hb-build library's internals, so it REOPENS package
\ HB-BUILD-CLI rather than importing a public surface: exporting those
\ internals would widen the library's interface for the benefit of its own
\ test. The local fixture scopes this file used to carry (HBT, HBT-CAP,
\ HBT-JSON) were there only because the file had no package of its own; they
\ are ordinary private words of the library's package now.
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

\ The three stripped-window fixtures: an application whose own require closure
\ owns the library cells it touches, one that reads the engine runtime cells the
\ stripped entry owns, and one that reaches an engine cell nothing claims.
variable HBT-LIB-SRC-U
variable HBT-LIB-OUT-U
variable HBT-LIB-DIR-U
variable HBT-CELLS-SRC-U
variable HBT-CELLS-OUT-U
variable HBT-UNOWNED-SRC-U
variable HBT-UNOWNED-OUT-U
variable HBT-PMK-SRC-U
variable HBT-PMK-OUT-U
variable HBT-PPH-SRC-U
variable HBT-PPH-OUT-U
variable HBT-TABLE-SRC-U
variable HBT-TABLE-OUT-U
variable HBT-CHAIN-SRC-U
variable HBT-CHAIN-OUT-U
variable HBT-PTRC-SRC-U
variable HBT-PTRC-OUT-U
variable HBT-PTRU-SRC-U
variable HBT-PTRU-OUT-U
variable HBT-OPENP-SRC-U
variable HBT-OPENP-OUT-U
variable HBT-LIFE-SRC-U
variable HBT-LIFE-OUT-U
variable HBT-HOOK-SRC-U
variable HBT-HOOK-OUT-U
variable HBT-NUMP-SRC-U
variable HBT-NUMP-OUT-U
variable HBT-MAPC-SRC-U
variable HBT-MAPC-OUT-U
variable HBT-MAPD-SRC-U
variable HBT-MAPD-OUT-U
variable HBT-MAPL-SRC-U
variable HBT-MAPL-OUT-U
variable HBT-MAPL-OUT2-U
variable HBT-TWICE-CACHE-U
variable HBT-LITB-SRC-U
variable HBT-LITB-OUT-U
variable HBT-LITC-SRC-U
variable HBT-LITC-OUT-U
create HBT-LIB-SRC-BUF FS-PATH-CAP allot
create HBT-LIB-OUT-BUF FS-PATH-CAP allot
create HBT-LIB-DIR-BUF FS-PATH-CAP allot
create HBT-CELLS-SRC-BUF FS-PATH-CAP allot
create HBT-CELLS-OUT-BUF FS-PATH-CAP allot
create HBT-UNOWNED-SRC-BUF FS-PATH-CAP allot
create HBT-UNOWNED-OUT-BUF FS-PATH-CAP allot
create HBT-PMK-SRC-BUF FS-PATH-CAP allot
create HBT-PMK-OUT-BUF FS-PATH-CAP allot
create HBT-PPH-SRC-BUF FS-PATH-CAP allot
create HBT-PPH-OUT-BUF FS-PATH-CAP allot
create HBT-TABLE-SRC-BUF FS-PATH-CAP allot
create HBT-TABLE-OUT-BUF FS-PATH-CAP allot
create HBT-CHAIN-SRC-BUF FS-PATH-CAP allot
create HBT-CHAIN-OUT-BUF FS-PATH-CAP allot
create HBT-PTRC-SRC-BUF FS-PATH-CAP allot
create HBT-PTRC-OUT-BUF FS-PATH-CAP allot
create HBT-PTRU-SRC-BUF FS-PATH-CAP allot
create HBT-PTRU-OUT-BUF FS-PATH-CAP allot
create HBT-OPENP-SRC-BUF FS-PATH-CAP allot
create HBT-OPENP-OUT-BUF FS-PATH-CAP allot
create HBT-LIFE-SRC-BUF FS-PATH-CAP allot
create HBT-LIFE-OUT-BUF FS-PATH-CAP allot
create HBT-HOOK-SRC-BUF FS-PATH-CAP allot
create HBT-HOOK-OUT-BUF FS-PATH-CAP allot
create HBT-NUMP-SRC-BUF FS-PATH-CAP allot
create HBT-NUMP-OUT-BUF FS-PATH-CAP allot
create HBT-MAPC-SRC-BUF FS-PATH-CAP allot
create HBT-MAPC-OUT-BUF FS-PATH-CAP allot
create HBT-MAPD-SRC-BUF FS-PATH-CAP allot
create HBT-MAPD-OUT-BUF FS-PATH-CAP allot
create HBT-MAPL-SRC-BUF FS-PATH-CAP allot
create HBT-MAPL-OUT-BUF FS-PATH-CAP allot
create HBT-MAPL-OUT2-BUF FS-PATH-CAP allot
create HBT-TWICE-CACHE-BUF FS-PATH-CAP allot
create HBT-LITB-SRC-BUF FS-PATH-CAP allot
create HBT-LITB-OUT-BUF FS-PATH-CAP allot
create HBT-LITC-SRC-BUF FS-PATH-CAP allot
create HBT-LITC-OUT-BUF FS-PATH-CAP allot

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

: HBT-REPL-SRC ( -- ptr u8 n )
   HBT-REPL-SRC-BUF HBT-REPL-SRC-U @ ;

: HBT-REPL-OUT ( -- ptr u8 n )
   HBT-REPL-OUT-BUF HBT-REPL-OUT-U @ ;

: HBT-REPL-BAD-SRC ( -- ptr u8 n )
   HBT-REPL-BAD-SRC-BUF HBT-REPL-BAD-SRC-U @ ;

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

: HBT-TWICE-CACHE ( -- ptr u8 n )
   HBT-TWICE-CACHE-BUF HBT-TWICE-CACHE-U @ ;

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
   HBT-ROOT s" unowned" HBT-UNOWNED-OUT-BUF HBT-UNOWNED-OUT-U HBT-PATH!
   HBT-ROOT s" ptrmark.f" HBT-PMK-SRC-BUF HBT-PMK-SRC-U HBT-PATH!
   HBT-ROOT s" ptrmark" HBT-PMK-OUT-BUF HBT-PMK-OUT-U HBT-PATH!
   HBT-ROOT s" pph.f" HBT-PPH-SRC-BUF HBT-PPH-SRC-U HBT-PATH!
   HBT-ROOT s" pph" HBT-PPH-OUT-BUF HBT-PPH-OUT-U HBT-PATH!
   HBT-ROOT s" table.f" HBT-TABLE-SRC-BUF HBT-TABLE-SRC-U HBT-PATH!
   HBT-ROOT s" table" HBT-TABLE-OUT-BUF HBT-TABLE-OUT-U HBT-PATH!
   HBT-ROOT s" chain.f" HBT-CHAIN-SRC-BUF HBT-CHAIN-SRC-U HBT-PATH!
   HBT-ROOT s" chain" HBT-CHAIN-OUT-BUF HBT-CHAIN-OUT-U HBT-PATH!
   HBT-ROOT s" ptrcell.f" HBT-PTRC-SRC-BUF HBT-PTRC-SRC-U HBT-PATH!
   HBT-ROOT s" ptrcell" HBT-PTRC-OUT-BUF HBT-PTRC-OUT-U HBT-PATH!
   HBT-ROOT s" ptrunowned.f" HBT-PTRU-SRC-BUF HBT-PTRU-SRC-U HBT-PATH!
   HBT-ROOT s" ptrunowned" HBT-PTRU-OUT-BUF HBT-PTRU-OUT-U HBT-PATH!
   HBT-ROOT s" openpath.f" HBT-OPENP-SRC-BUF HBT-OPENP-SRC-U HBT-PATH!
   HBT-ROOT s" openpath" HBT-OPENP-OUT-BUF HBT-OPENP-OUT-U HBT-PATH!
   HBT-ROOT s" lifecycle.f" HBT-LIFE-SRC-BUF HBT-LIFE-SRC-U HBT-PATH!
   HBT-ROOT s" lifecycle" HBT-LIFE-OUT-BUF HBT-LIFE-OUT-U HBT-PATH!
   HBT-ROOT s" lifehook.f" HBT-HOOK-SRC-BUF HBT-HOOK-SRC-U HBT-PATH!
   HBT-ROOT s" lifehook" HBT-HOOK-OUT-BUF HBT-HOOK-OUT-U HBT-PATH!
   HBT-ROOT s" numparse.f" HBT-NUMP-SRC-BUF HBT-NUMP-SRC-U HBT-PATH!
   HBT-ROOT s" numparse" HBT-NUMP-OUT-BUF HBT-NUMP-OUT-U HBT-PATH!
   HBT-ROOT s" mapcell.f" HBT-MAPC-SRC-BUF HBT-MAPC-SRC-U HBT-PATH!
   HBT-ROOT s" mapcell" HBT-MAPC-OUT-BUF HBT-MAPC-OUT-U HBT-PATH!
   HBT-ROOT s" mapdecl.f" HBT-MAPD-SRC-BUF HBT-MAPD-SRC-U HBT-PATH!
   HBT-ROOT s" mapdecl" HBT-MAPD-OUT-BUF HBT-MAPD-OUT-U HBT-PATH!
   HBT-ROOT s" maplate.f" HBT-MAPL-SRC-BUF HBT-MAPL-SRC-U HBT-PATH!
   HBT-ROOT s" maplate" HBT-MAPL-OUT-BUF HBT-MAPL-OUT-U HBT-PATH!
   HBT-ROOT s" maplate2" HBT-MAPL-OUT2-BUF HBT-MAPL-OUT2-U HBT-PATH!
   HBT-ROOT s" cache-twice" HBT-TWICE-CACHE-BUF HBT-TWICE-CACHE-U HBT-PATH!
   HBT-ROOT s" litbody.f" HBT-LITB-SRC-BUF HBT-LITB-SRC-U HBT-PATH!
   HBT-ROOT s" litbody" HBT-LITB-OUT-BUF HBT-LITB-OUT-U HBT-PATH!
   HBT-ROOT s" litcell.f" HBT-LITC-SRC-BUF HBT-LITC-SRC-U HBT-PATH!
   HBT-ROOT s" litcell" HBT-LITC-OUT-BUF HBT-LITC-OUT-U HBT-PATH!
   HBT-TWICE-CACHE MAKE-DIR
   HBT-BAD-SRC HBT-BAD-SRC$ WRITE-ALL
   HBT-REPL-SRC HBT-REPL-SRC$ WRITE-ALL
   HBT-REPL-BAD-SRC HBT-REPL-BAD-SRC$ WRITE-ALL
   HBT-AOT-SRC HBT-AOT-SRC$ WRITE-ALL
   BUILD-CACHE:RESET
   HBT-TMP BUILD-CACHE:ROOT! ;

: HBT-ARGV-BASE-TMP ( ptr u8 n -- )
   PROC-ARGV-RESET
   PROC-ENV-RESET
   s" HB_TMP" >LEN 2swap >LEN PROC-ENV+
   s" HABU_BUILD_CACHE" >LEN HBT-TMP >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   s" --load"  >LEN PROC-ARGV+
   s" tools/hb-build.f"  >LEN PROC-ARGV+
   s" --"  >LEN PROC-ARGV+ ;

: HBT-ARGV-BASE ( -- )
   HBT-TMP HBT-ARGV-BASE-TMP ;

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

;package
