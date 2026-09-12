\ build-rewind-test.f - the build ABI's dictionary rewind, at the process entry.
\
\ WHAT THIS PINS. Every generated engine source opens with the core-prefix
\ rewind (src/habu/prefix-rewind.f PREFIX-REWIND:TO-CORE): it returns the
\ compiling host to the end of its own core prefix so the target's source
\ compiles on top of the host's live copy instead of an orphaned one. The
\ dictionary half of that rewind lowers the record count BELOW the engine's seal
\ floor - the watermark that says "records under here are the engine's own" - and
\ the floor is armed before any entry runs, on every boot mode. So the rewind
\ needs the ONE engine seam allowed to lower past it, `seed-ndict!`, and not the
\ public `ndict!`.
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
\ act). The second case then proves the floor is a floor still: public `ndict!`
\ below the re-armed watermark traps, from the same payload, after the rewind.
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

\ The first three rows are tools/build-fixpoint.f BF-APPEND-RUN-PRELUDE's own:
\ the refresh prelude, the rewind, and the named checking boundary the generated
\ sources call instead of a raw `0 set-check`. `include` and not `require`
\ because both files are payload-only and carry no registry row.
: BR-PRELUDE ( -- )
   SB-RESET
   s" include src/habu/hide.f" BR-LINE
   s" include src/habu/prefix-rewind.f" BR-LINE
   s" BFR-CHECK-OFF" BR-LINE ;

\ One body, because the rewind discards this word's own record while the body
\ runs: a check that needed a second top-level lookup afterwards would be
\ looking for a name the rewind had just taken away, and a check defined after
\ it would raise the very count it is comparing.
: BR-REWIND-BODY ( -- )
   s" : BR-RUN ( -- )" BR-LINE
   s"    ndict@ PREFIX-MARK:DICT > 0= if" BR-LINE
   s\"       s\" build-rewind: nothing above the mark\" 74 die then" BR-LINE
   s"    PREFIX-REWIND:TO-CORE" BR-LINE
   s"    ndict@ PREFIX-MARK:DICT <> if" BR-LINE
   s\"       s\" build-rewind: not at the mark\" 74 die then" BR-LINE
   s"    SEAL-NDICT@ ndict@ <> if" BR-LINE
   s\"       s\" build-rewind: floor not re-armed\" 74 die then" BR-LINE
   s\"    s\" build-rewind: rewound\" type cr ;" BR-LINE
   s" BR-RUN" BR-LINE ;

: BR-FLOOR-BODY ( -- )
   s" : BR-BELOW ( -- )" BR-LINE
   s"    0 ndict!" BR-LINE
   s\"    s\" build-rewind: floor did not refuse\" type cr ;" BR-LINE
   s" BR-BELOW" BR-LINE ;

: BR-WRITE-OK ( -- )
   BR-PRELUDE
   BR-REWIND-BODY
   BR-OK$ SB$ WRITE-ALL ;

: BR-WRITE-FLOOR ( -- )
   BR-PRELUDE
   BR-REWIND-BODY
   BR-FLOOR-BODY
   BR-FLOOR$ SB$ WRITE-ALL ;

: BR-SETUP ( -- )
   CLEANUP-RESET
   s" habu-build-rewind" TMPDIR-MKDIR {: a:ptr u:n :}
   a BR-ROOT u BYTE-COPY
   u BR-ROOT-U !
   BR-ROOT$ CLEANUP-TREE+
   BR-ROOT$ s" rewind.f" BR-OK JOIN-PATH BR-OK-U !
   BR-ROOT$ s" floor.f"  BR-FLOOR JOIN-PATH BR-FLOOR-U !
   BR-WRITE-OK
   BR-WRITE-FLOOR ;

\ The real build command line: tools/build-fixpoint.f COMPILER-BUILD:ARGV spells
\ it `--build <payload> -- <tmp>`, and the payload-only rewind exists at no other
\ entry.
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

public

: BUILD-REWIND-TEST-MAIN ( -- )
   T-RESET
   BR-SETUP
   BR-REWIND-RUNS
   BR-FLOOR-HOLDS
   CLEANUP-RUN
   T-REPORT
   s" build-rewind-test: ok" type cr ;

;package

BUILD-REWIND-TEST:BUILD-REWIND-TEST-MAIN
