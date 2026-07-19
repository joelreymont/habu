\ aot-data-span-forge.f - AOT DATA-reserve span-guard boot regression
\ (dot habu-guard-aot-data-49de2ee6).
\
\ Proves EM-AOT-RELOC-DATA's span bound (habu2.f) both directions. That reserve --
\ and its guard -- runs ONLY on interactive REPL entry (AOT-SEED-ARM-CELL), so both
\ cases boot an engine UNDER A PTY; a piped/batch boot never reaches the reserve.
\   RED : a forged image whose baked LAOTDATASIZE exceeds the DATA region
\         (HABU_AOT_SPAN = 2*DATA-SIZE, well past the seed headroom top - seedDP)
\         must die at boot naming "hb: AOT data span out of range" and exit 82
\         (AOT-OWNER-RC). On the unfixed base the same image boots silently (the
\         forged span is accepted with no bound check), so this case is red-first.
\   GREEN: the unforged engine (its real few-KB span) must still boot to the REPL
\         prompt and exit 0 -- the maximal legal reserve must not be over-rejected.
\
\ Spawn-only helper: test/aot-wid-suite.f runs it as a child and gates on its exit
\ code; it is not a TEST:SUITE member (so suite-coverage-lint does not scan it,
\ like test/aot-wid-build.f). The PTY primitives below mirror test/proc-pty.f
\ verbatim; factoring them into a shared lib is out of this dot's scope. Linux gate
\ hosts provide /dev/ptmx + a mounted /dev/pts (docs/process-pty.md).
\
\ Standalone:
\   bin/hb --load lib/errors.f lib/string.f lib/memory.f lib/fs.f lib/fs-mutate.f \
\     lib/process.f lib/process-argv.f lib/process-env.f lib/test.f \
\     test/aot-data-span-forge.f

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/test.f

package AOT-DATA-SPAN-FORGE

\ Any span >= DATA-SIZE overflows the region (the seed headroom is always strictly
\ under DATA-SIZE), so 2*DATA-SIZE is unambiguously oversized. Kept as a literal
\ for the child env string; the drift self-check below fails loud if DATA-SIZE moves.
67108864 constant OVERSIZED-SPAN                       \ = 2 * DATA-SIZE (33554432)
: OVERSIZED-SPAN$ ( -- ptr u8 n ) s" 67108864" ;

240000 constant BUILD-TIMEOUT-MS

\ --- PTY constants + poll tuning (mirror test/proc-pty.f) ---
$40045431 constant LINUX-TIOCSPTLCK
$80045430 constant LINUX-TIOCGPTN
2   constant PTY-OPEN-RDWR
10  constant PTY-POLL-MS
500 constant PTY-EXPECT-MAX-POLLS         \ up to ~5s to let the child boot + write

$8000 constant CAP
create OUT CAP allot    variable OUT-U
create ERR CAP allot    variable ERR-U
create RBUF 4096 allot  variable RN
create PTYNAME 128 allot   variable PTY-U
create CH 1 allot
variable PTYNUM   variable MFD   variable SFD   variable PID

create ROOT-BUF FS-PATH-CAP allot    variable ROOT-U
create HBPWID-BUF FS-PATH-CAP allot   variable HBPWID-U

: ROOT$ ( -- ptr u8 n )   ROOT-BUF ROOT-U @ ;
: HBPWID$ ( -- ptr u8 n ) HBPWID-BUF HBPWID-U @ ;
: PLAIN$ ( -- ptr u8 n )  s" bin/hb" ;
: ERR$ ( -- ptr u8 n )    ERR ERR-U @ ;
: RBUF$ ( -- ptr u8 n )   RBUF RN @ ;

: SETUP ( -- )
   CLEANUP-RESET
   s" habu-aot-span" TMPDIR-MKDIR {: a:ptr u:n :}
   a ROOT-BUF u BYTE-COPY  u ROOT-U !
   ROOT$ CLEANUP-TREE+
   ROOT$ s" hb-pwid" HBPWID-BUF JOIN-PATH HBPWID-U ! ;

\ --- build the forged oversized image: spawn aot-wid-build with HABU_AOT_SPAN, so
\ its emitted hb-pwid bakes LAOTDATASIZE = OVERSIZED-SPAN (see aot-wid-build.f
\ SPAN-FORGE-LINE). Private HB_TMP so nothing in the tree is touched.
: BUILD-FORGED ( -- )
   PROC-ENV-RESET
   s" HB_TMP" >LEN ROOT$ >LEN PROC-ENV+
   s" HABU_AOT_SPAN" >LEN OVERSIZED-SPAN$ >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   s" test/aot-wid-build.f" >LEN PROC-ARGV+
   PLAIN$ >LEN  OUT CAP >LEN  ERR CAP >LEN  BUILD-TIMEOUT-MS >MS
   RUN-ARGV-ENV-CAPTURE
   MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE {: o:len e:len :}
            o LEN>N OUT-U !  e LEN>N ERR-U ! ENDOF
     err OF PCAP-FAILED:UNMAKE {: o:len e:len c:rc :}
            o LEN>N OUT-U !  e LEN>N ERR-U !
            s" aot-data-span-forge: forged builder stderr:" type cr  ERR$ type cr
            s" aot-data-span-forge: forged build failed" c RC>N die ENDOF
   ;MATCH ;

\ --- PTY plumbing (mirror test/proc-pty.f) ---
: READ+ {: fd :} ( fd -- )
   fd FD>N RBUF RN @ + 4096 RN @ - read
   dup 0 > if RN @ + RN ! else drop then ;

: MFD-READ-READY? ( -- bool )        \ one poll+read on the master
   MFD @ >FD PTY-POLL-MS >MS POLL-IN COUNT>N 0 > if MFD @ >FD READ+ 0 0= exit then
   0 0= 0= ;

\ Poll the master until the accumulated output contains a/u, or the poll budget is
\ spent (the child needs ~0.5s to boot before it writes). Mirrors proc-pty EXPECT-WAIT?.
: WAIT-FOR ( ptr u8 n -- bool ) {: a:ptr u :}
   RBUF$ a u CONTAINS? if 0 0= exit then
   0 begin dup PTY-EXPECT-MAX-POLLS < while
      MFD-READ-READY? drop
      RBUF$ a u CONTAINS? if drop 0 0= exit then
      1 +
   repeat drop
   0 0= 0= ;

: SEND-C {: c :} ( c -- )
   c CH c!
   MFD @ CH 1 write drop ;

: PTY-PATH-C ( n -- ) {: c :}
   c PTYNAME PTY-U @ + c!
   PTY-U @ 1 + PTY-U ! ;
: PTY-PATH+ ( ptr u8 n -- ) {: a:ptr u :}
   0 begin dup u < while  dup a + c@ PTY-PATH-C  1 + repeat drop ;
: PTY-PATH-U+ ( n -- ) {: n :}
   n 10 >= if n 10 / recurse then  n 10 mod 48 + PTY-PATH-C ;
: PTY-PATH-BUILD ( -- )              \ /dev/pts/<PTYNUM>\0
   0 PTY-U !  s" /dev/pts/" PTY-PATH+  PTYNUM @ PTY-PATH-U+  0 PTY-PATH-C ;

: OPEN-PTY ( -- )                    \ Linux ptmx master + /dev/pts/N slave
   s" /dev/ptmx" >LEN PROC-PATHZ PTY-OPEN-RDWR 0 open MFD !
   MFD @ 2 > TTRUE
   MFD @ >FD FD-CLOEXEC!
   0 PTYNUM !
   MFD @ LINUX-TIOCSPTLCK PTYNUM ioctl 0 T=
   MFD @ LINUX-TIOCGPTN PTYNUM ioctl 0 T=
   PTY-PATH-BUILD
   PTYNAME PTY-OPEN-RDWR 0 open SFD !
   SFD @ 2 > TTRUE ;

: PTY-SPAWN ( ptr u8 n -- ) {: e:ptr eu:n :}   \ boot engine e with the slave on fd 0/1/2
   0 RN !
   OPEN-PTY
   e eu >LEN  SFD @ >FD SFD @ >FD SFD @ >FD  PROC-SPAWN-IO PID !
   PID @ 0 > TTRUE
   SFD @ close ;

\ --- the two directions ---
: ASSERT-FORGED-DIES ( -- )
   HBPWID$ PTY-SPAWN                 \ boot the forged image under a PTY
   s" AOT data span guard: forged span prints the named boot die" T-LABEL
   s" hb: AOT data span out of range" WAIT-FOR TTRUE   \ its fd-2 die reaches the master
   s" AOT data span guard: forged span exits 82 (AOT-OWNER-RC)" T-LABEL
   PID @ >PID PROC-WAIT-RC MATCH result
     ok  OF drop 1 0 T= ENDOF          \ unexpected clean exit -> fail
     err OF 82 T= ENDOF                \ expected: exit code == 82
   ;MATCH
   MFD @ close ;

: ASSERT-LEGAL-BOOTS ( -- )
   PLAIN$ PTY-SPAWN                  \ boot the unforged engine under a PTY
   s" AOT data span guard: legal engine reaches the REPL prompt" T-LABEL
   s" habu> " WAIT-FOR TTRUE         \ boot banner + prompt appear (span reserve passed)
   s" AOT data span guard: legal engine does not fire the span die" T-LABEL
   RBUF$ s" hb: AOT data span out of range" CONTAINS? 0= TTRUE
   4 SEND-C                          \ Ctrl-D: leave the REPL
   s" AOT data span guard: legal engine exits 0" T-LABEL
   PID @ >PID PROC-WAIT-RC MATCH result
     ok  OF 0 T= ENDOF
     err OF drop 1 0 T= ENDOF
   ;MATCH
   MFD @ close ;

: BODY ( -- )
   s" AOT data span guard: forged span literal is 2*DATA-SIZE" T-LABEL
   DATA-SIZE 2 *  OVERSIZED-SPAN  T=
   HB-TARGET-LINUX? 0= if
      s" aot-data-span-forge: PTY boot cases run on linux only; skipped" type cr exit then
   SETUP
   BUILD-FORGED
   s" AOT data span guard: forged variant image exists after build" T-LABEL
   HBPWID$ EXISTS? TTRUE
   ASSERT-FORGED-DIES
   ASSERT-LEGAL-BOOTS ;

public

: RUN ( -- )
   T-RESET
   CLEANUP-RESET
   [: BODY ;] catch {: code:n :}
   CLEANUP-RUN
   code 0 <> if code throw then
   T-REPORT
   s" aot-data-span-forge: ok" type cr ;

;package

AOT-DATA-SPAN-FORGE:RUN
