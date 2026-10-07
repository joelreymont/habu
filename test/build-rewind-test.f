\ build-rewind-test.f - the build ABI's dictionary rewind, at the process entry.
\
\ WHAT THIS PINS. Every generated engine source opens with the core-prefix
\ rewind (src/habu/prefix-rewind.f's top-level text): it returns the
\ compiling host to the end of its own core prefix so the target's source
\ compiles on top of the host's live copy instead of an orphaned one. The
\ dictionary half of that rewind lowers the record count BELOW the engine's seal
\ floor - the watermark that says "records under here are the engine's own" - and
\ the floor is armed before any entry runs, on every boot mode. So the rewind
\ needs the ONE engine seam allowed to lower past it, `seed-ndict!`, and not the
\ public `ndict!`. That seam is a top-level boundary primitive: no checked body
\ names it, so the payload `include`s the rewind at its own top level.
\
\ RED-FIRST, AND WHY IT HAS TO BE A SPAWN. With the rewind on public `ndict!`
\ this payload exits 83 (ENGINE-ERROR:SEAL-VIOLATION) with ZERO bytes on both
\ streams: the guard is a trap, not a throw, so there is no diagnostic and no
\ in-process observation of it - the only report is a child's exit status. That
\ silence is what made the same defect read as "hb-build: native maker build
\ failed, rc 83" one layer up, with nothing naming the line.
\
\ WHAT THE PAYLOAD ASSERTS FOR ITSELF. The numbers live in the child, so the
\ child checks them: that there was something above the mark to discard, that the
\ rewind landed exactly ON the mark, and that the floor followed it down instead
\ of standing above the dictionary it describes (SEAL-CAPTURE, the rewind's last
\ act). The checks after the rewind are a word the payload ticks before it and
\ executes after it, the way every caller of the rewind runs code past it. The
\ second case then proves the floor is a floor still: public `ndict!` below the
\ re-armed watermark traps, from the same payload, after the rewind.
\
\ Run: bin/hb --load test/build-rewind-test.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f

package BUILD-REWIND-TEST

$4000 constant BR-CAP
60000 constant BR-TIMEOUT-MS

create BR-ROOT  FS-PATH-CAP allot   variable BR-ROOT-U
create BR-OK    FS-PATH-CAP allot   variable BR-OK-U
create BR-FLOOR FS-PATH-CAP allot   variable BR-FLOOR-U
create BR-OUT   BR-CAP allot
create BR-ERR   BR-CAP allot

: BR-ROOT$ ( -- ptr u8 n )   BR-ROOT BR-ROOT-U @ ;
: BR-OK$ ( -- ptr u8 n )     BR-OK BR-OK-U @ ;
: BR-FLOOR$ ( -- ptr u8 n )  BR-FLOOR BR-FLOOR-U @ ;

: BR-LINE ( ptr u8 n -- )
   SB-APPEND
   s\" \n" SB-APPEND ;

\ The first row is tools/build-fixpoint.f BF-APPEND-RUN-PRELUDE's own: the
\ `0 set-check` window the generated sources open before their rewind.
: BR-PRELUDE ( -- )
   SB-RESET
   s" 0 set-check" BR-LINE ;

\ BR-AFTER holds the checks past the rewind: the payload ticks it before the
\ rewind takes its name and executes it after, since the rewind removes records
\ and keeps code. It is defined before BR-BEFORE, so the record count the checks
\ compare is the one the rewind leaves. BR-REWIND ends it, after the floor
\ case's tail.
: BR-AFTER-BODY ( -- )
   s" : BR-AFTER ( -- )" BR-LINE
   s"    ndict@ PREFIX-MARK:DICT <> if" BR-LINE
   s\"       s\" build-rewind: not at the mark\" 74 die then" BR-LINE
   s\"    s\" true\" 0 XREF-FIND-WL-INDEX 0 >= if" BR-LINE
   s\"       s\" build-rewind: post-prefix prelude survived\" 74 die then" BR-LINE
   s"    SEAL-NDICT@ ndict@ <> if" BR-LINE
   s\"       s\" build-rewind: floor not re-armed\" 74 die then" BR-LINE
   s\"    s\" build-rewind: rewound\" type cr" BR-LINE ;

\ The floor case's tail: public `ndict!` below the re-armed floor, after the
\ rewind.
: BR-FLOOR-TAIL ( -- )
   s"    0 ndict!" BR-LINE
   s\"    s\" build-rewind: floor did not refuse\" type cr" BR-LINE ;

\ BR-AFTER's end, the check before the rewind, then the rewind itself between
\ BR-AFTER's tick and its execution.
: BR-REWIND ( -- )
   s"    ;" BR-LINE
   s" : BR-BEFORE ( -- )" BR-LINE
   s"    ndict@ PREFIX-MARK:DICT > 0= if" BR-LINE
   s\"       s\" build-rewind: nothing above the mark\" 74 die then ;" BR-LINE
   s" ' BR-AFTER BR-BEFORE" BR-LINE
   s" include src/habu/prefix-rewind.f" BR-LINE
   s" execute" BR-LINE ;

: BR-WRITE-OK ( -- )
   BR-PRELUDE
   BR-AFTER-BODY
   BR-REWIND
   BR-OK$ SB$ WRITE-ALL ;

: BR-WRITE-FLOOR ( -- )
   BR-PRELUDE
   BR-AFTER-BODY
   BR-FLOOR-TAIL
   BR-REWIND
   BR-FLOOR$ SB$ WRITE-ALL ;

: BR-SETUP ( -- )
   CLEANUP-RESET
   s" habu-build-rewind" HB-TMP-MKDIR {: a:ptr u:n :}
   a BR-ROOT u BYTE-COPY
   u BR-ROOT-U !
   BR-ROOT$ CLEANUP-TREE+
   BR-ROOT$ s" rewind.f" BR-OK JOIN-PATH BR-OK-U !
   BR-ROOT$ s" floor.f"  BR-FLOOR JOIN-PATH BR-FLOOR-U !
   BR-WRITE-OK
   BR-WRITE-FLOOR ;

\ The real build command line: tools/build-fixpoint.f COMPILER-BUILD:ARGV spells
\ it `--build <payload> -- <tmp>`.
: BR-ARGV ( ptr u8 n -- ) {: src:ptr srcu:n :}
   PROC-ARGV-RESET
   s" --build" >LEN PROC-ARGV+
   src srcu >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   BR-ROOT$ >LEN PROC-ARGV+ ;

: BR-RUN ( -- n n n )                              \ -> outu erru rc
   s" bin/hb" >LEN BR-OUT BR-CAP >LEN BR-ERR BR-CAP >LEN BR-TIMEOUT-MS >MS
   RUN-ARGV-CAPTURE
   MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE {: o:len e:len :} o LEN>N e LEN>N 0 ENDOF
     err OF PCAP-FAILED:UNMAKE {: o:len e:len c:rc :} o LEN>N e LEN>N c RC>N ENDOF
   ;MATCH ;

: BR-OUT$ ( n -- ptr u8 n ) {: u:n :}   BR-OUT u ;
: BR-ERR$ ( n -- ptr u8 n ) {: u:n :}   BR-ERR u ;

\ ---- cases -----------------------------------------------------------------

: BR-REWIND-RUNS ( -- )
   BR-OK$ BR-ARGV
   BR-RUN {: outu:n erru:n rc:n :}
   s" a --build payload's core-prefix rewind exits 0" T-LABEL
   rc 0 T=
   s" ... and lands on the mark with the floor re-armed" T-LABEL
   outu BR-OUT$ s" build-rewind: rewound" CONTAINS? TTRUE
   s" ... and says nothing on stderr" T-LABEL
   erru 0 T= ;

: BR-FLOOR-HOLDS ( -- )
   BR-FLOOR$ BR-ARGV
   BR-RUN {: outu:n erru:n rc:n :}
   s" public ndict! below the re-armed floor still traps 83" T-LABEL
   rc ENGINE-ERROR:SEAL-VIOLATION T=
   s" ... after the same payload's rewind reported success" T-LABEL
   outu BR-OUT$ s" build-rewind: rewound" CONTAINS? TTRUE
   s" ... and the store past the floor never ran" T-LABEL
   outu BR-OUT$ s" build-rewind: floor did not refuse" CONTAINS? TFALSE ;

\ Refuse the whole invalid index range before deriving a dictionary address.
: BR-SEED-REFUSED ( ptr u8 n -- ) {: value:ptr valueu:n :}
   SB-RESET
   value valueu SB-APPEND
   s"  seed-ndict!" BR-LINE
   BR-FLOOR$ SB$ WRITE-ALL
   BR-FLOOR$ BR-ARGV
   BR-RUN {: outu:n erru:n rc:n :}
   value valueu T-LABEL
   rc 74 T=
   outu 0 T= ;

: BR-SEED-BOUNDS ( -- )
   s" -1" BR-SEED-REFUSED
   s" $8000000000000000" BR-SEED-REFUSED
   s" ndict@" BR-SEED-REFUSED
   s" ndict@ 1+" BR-SEED-REFUSED ;

public

: BUILD-REWIND-TEST-MAIN ( -- )
   T-RESET
   BR-SETUP
   BR-REWIND-RUNS
   BR-FLOOR-HOLDS
   BR-SEED-BOUNDS
   CLEANUP-RUN
   T-REPORT
   s" build-rewind-test: ok" type cr ;

;package

BUILD-REWIND-TEST:BUILD-REWIND-TEST-MAIN
