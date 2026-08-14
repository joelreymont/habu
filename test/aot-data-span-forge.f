\ aot-data-span-forge.f - AOT seed-pass boot regressions that need a real terminal
\ (dots habu-guard-aot-data-49de2ee6 and habu-bake-the-aot-7ececce8).
\
\ Everything here boots a built engine under a PTY. That used to be forced: the
\ AOT seed was armed at the interactive REPL entry and nowhere else, so a piped or
\ --load boot never seeded the blob and was silent about every case below. Since
\ dot habu-decide-arm-the-5234727b the seed runs at the end of the engine prefix
\ on EVERY boot, so these cases could be written as batch fixtures, and the
\ cheaper batch vehicle is where new seed cases belong -
\ test/aot-seed-batch-suite.f is that vehicle. These stay on the PTY because they
\ are already written, already spawn-driven, and each one reads output the
\ boot-run prints: rewriting a passing forge is churn, not coverage. The one thing
\ a batch fixture still cannot say is what the engine does when it is entered
\ INTERACTIVELY, since the boot-run entry words ask TTY? themselves.
\
\ THE SPAN GUARD. Proves EM-AOT-RELOC-DATA's span bound (habu2.f) both directions.
\   RED : a forged image whose baked LAOTDATASIZE exceeds the DATA region
\         (HABU_AOT_SPAN = 2*DATA-SIZE, well past the seed headroom top - seedDP)
\         must die at boot naming "hb: AOT data span out of range" and exit 82
\         (ENGINE-ERROR:AOT-SEED). On the unfixed base the same image boots silently (the
\         forged span is accepted with no bound check), so this case is red-first.
\   GREEN: the unforged engine (its real few-KB span) must still boot to the REPL
\         prompt and exit 0 -- the maximal legal reserve must not be over-rejected.
\
\ THE WINDOW'S CONTENT. Proves what EM-AOT-RELOC-DATA carries, now that it copies
\ the captured DATA window instead of reserving it zeroed.
\   CONTENT: a variant whose window holds an INITIALISED cell and a word that reads
\         it must report that value at boot. On the zeroed reserve the same engine
\         reports 0, so this case is red-first.
\   TRAP  : a variant whose window holds a `defer` nothing installs must die
\         "defer: unset execution vector" (exit 76) when the boot-run calls it.
\         The capture zeroes every declared address cell, because the value there
\         is a code address in the BUILDING host and baking one would make the
\         image depend on the run that produced it; the seed then writes the
\         engine's own defer-unset xt back. Zero would have been a jump to address
\         0 - measured as SIGSEGV, no diagnostic - so this case is what says the
\         masking cannot silently break a vector, only surface a missing boot-run
\         entry.
\
\ THE WIDE FORMAT (dot habu-widen-the-aot-089f5faf). Proves what only a boot can
\ say about a capture window past the 64 KiB world the format used to live in.
\   BIG   : a variant whose window is several times 65535 bytes, with a data cell,
\         a non-inlinable callee and a reporter defined ABOVE the filler, must
\         report the cell's magic at boot. That value can only arrive if a call
\         site AND a DATA site whose blob offsets do not fit sixteen bits were
\         both recorded and patched by the seed. On the pre-widening format the
\         build never got this far - the capture died "aot-capture: blob exceeds
\         buffer" - so the case is red-first by refusal rather than by a wrong
\         answer. test/aot-wide-format-suite.f owns the build half, which runs on
\         every host; this is the half that needs the terminal.
\   EXT   : a variant whose window holds a word whose NAME is too long for its
\         dictionary record must find that word at boot. The boot-run resolves
\         its entry words through LFIND, and for such a record LFIND compares the
\         token against the bytes the record's [24] cell points at - which the
\         seed sets to the baked name pool. A wrong pointer finds nothing and the
\         boot-run exits $52 silently, so the reported magic is the cell's proof.
\         On the pre-widening format the build never got this far either: the
\         capture refused the record outright.
\ THE PRE-WINDOW ELIMINATION (dot habu-aot-pre-window-0b01043c).
\   PREWIN: a variant whose window holds a word naming a PREFIX data word. The
\         engine used to copy that word's short body in, carrying an address below
\         the window's DATA span, and the capture refused the build; it now
\         declines the copy and emits the call, which the capture records BY NAME.
\         So the report can only carry the prefix word's own initialised first cell
\         if the seed resolved that name in the engine it was booting and patched
\         the call to it. test/aot-wide-format-suite.f owns the build half, which
\         runs on every host and reads the capture tables directly; this is the
\         half that says the name reaches the right address at boot.
\   XTSITE: a variant in which one of the window's own code-address chains was
\         turned into a NAMED code site keyed on a different word. The seed must
\         resolve that name and write its entry into the chain, so the engine
\         reports 22 (the named word) rather than 11 (the address the rebase
\         would have written) - and the capture zeroed the lanes, so there is no
\         host address underneath either answer.
\
\ Spawn-only helper: test/aot-wid-suite.f runs it as a child and gates on its exit
\ code; it is not a TEST:SUITE member, like test/aot-wid-build.f. The PTY
\ primitives below mirror test/proc-pty.f
\ verbatim; factoring them into a shared lib is out of this dot's scope. Linux gate
\ hosts provide /dev/ptmx + a mounted /dev/pts (docs/process-pty.md).
\
\ Standalone:
\   bin/hb --load lib/errors.f lib/string.f lib/fmt.f lib/memory.f lib/fs.f lib/fs-mutate.f \
\     lib/process.f lib/process-argv.f lib/process-env.f lib/test.f \
\     test/aot-data-span-forge.f

require lib/errors.f
require lib/string.f
require lib/fmt.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/test.f

package AOT-DATA-SPAN-FORGE

\ Any span >= DATA-SIZE overflows the region (the seed headroom is always strictly
\ under DATA-SIZE), so 2*DATA-SIZE is unambiguously oversized. DATA-SIZE is
\ per-target ($2000000 = 32 MiB on Linux, $10000000000 = 1 TiB on macOS), so the
\ forged span is computed from it rather than pinned to one host's literal. The
\ child needs it as a decimal string for its HABU_AOT_SPAN env var, so RENDER-SPAN
\ formats OVERSIZED-SPAN into the persistent SPAN-BUF once and OVERSIZED-SPAN$
\ hands back that buffer; the coherence self-check in BODY re-parses the rendered
\ text and fails loud if the number-to-text rendering ever drifts.
DATA-SIZE 2 * constant OVERSIZED-SPAN                  \ = 2 * DATA-SIZE (per-target)

create SPAN-BUF 32 allot   variable SPAN-U             \ decimal text of OVERSIZED-SPAN

: RENDER-SPAN ( -- )                                   \ format OVERSIZED-SPAN into SPAN-BUF
   SB-RESET  OVERSIZED-SPAN FMT:SB-U  SB$ {: a:ptr u:n :}
   a SPAN-BUF u BYTE-COPY  u SPAN-U ! ;

: OVERSIZED-SPAN$ ( -- ptr u8 n )   SPAN-BUF SPAN-U @ ;

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

\ --- build a window-content variant: same builder, a different mode. The fixture
\ is defined at top level in the maker and taken in by a widened re-capture, so the
\ engine under test carries it in its own capture window (aot-wid-build.f
\ BAKE-FIXTURE-LINES / TRAP-FIXTURE-LINES). ---
: BUILD-MODE ( ptr u8 n -- ) {: k:ptr ku:n :}
   PROC-ENV-RESET
   s" HB_TMP" >LEN ROOT$ >LEN PROC-ENV+
   k ku >LEN  s" 1" >LEN PROC-ENV+
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
            s" aot-data-span-forge: window-content builder stderr:" type cr  ERR$ type cr
            s" aot-data-span-forge: window-content build failed" c RC>N die ENDOF
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
   s" AOT data span guard: forged span exits 82 (ENGINE-ERROR:AOT-SEED)" T-LABEL
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

\ --- the window's content, both halves ---------------------------------------
\ The magic is the value aot-wid-build.f stores into the fixture cell; matching it
\ in the boot report means the bytes travelled from the capture window into the
\ image and the cell's DATA address literal was rebased onto the seeded DP.
: CONTENT-MAGIC$ ( -- ptr u8 n )
   s" awb-cell=6510728274268543578" ;

: ASSERT-CONTENT-TRAVELS ( -- )
   HBPWID$ PTY-SPAWN
   s" AOT window content: the initialised cell reports its value at boot" T-LABEL
   CONTENT-MAGIC$ WAIT-FOR TTRUE
   s" AOT window content: it is not the zeroed reserve's answer" T-LABEL
   RBUF$ s" awb-cell=0" CONTAINS? 0= TTRUE
   4 SEND-C
   s" AOT window content: the engine still exits 0" T-LABEL
   PID @ >PID PROC-WAIT-RC MATCH result
     ok  OF 0 T= ENDOF
     err OF drop 1 0 T= ENDOF
   ;MATCH
   MFD @ close ;

: ASSERT-TRAP-DIES-NAMED ( -- )
   HBPWID$ PTY-SPAWN
   s" AOT declared cell: an uninstalled vector dies by name" T-LABEL
   s" defer: unset execution vector" WAIT-FOR TTRUE
   s" AOT declared cell: it exits 76 (EXEC-VECTOR-RC), not a fault" T-LABEL
   PID @ >PID PROC-WAIT-RC MATCH result
     ok  OF drop 1 0 T= ENDOF
     err OF 76 T= ENDOF
   ;MATCH
   MFD @ close ;

: PROBE-WINDOW-CONTENT ( -- )
   s" HABU_AOT_BAKE" BUILD-MODE
   s" AOT window content: the content variant built" T-LABEL
   HBPWID$ EXISTS? TTRUE
   ASSERT-CONTENT-TRAVELS
   s" HABU_AOT_TRAP" BUILD-MODE
   s" AOT declared cell: the trap variant built" T-LABEL
   HBPWID$ EXISTS? TTRUE
   ASSERT-TRAP-DIES-NAMED ;

\ --- the wide format's boot half ----------------------------------------------
\ The magic is the value aot-wid-build.f stores into the big-window fixture cell.
\ Reading it back at boot means the seed resolved a call site and rebased a DATA
\ literal whose blob offsets are both far past 65535.
: BIG-MAGIC$ ( -- ptr u8 n )
   s" awb-big=6510711284817812058" ;

: ASSERT-BIG-WINDOW-BOOTS ( -- )
   HBPWID$ PTY-SPAWN
   s" AOT wide format: an over-64 KiB window reports its magic at boot" T-LABEL
   BIG-MAGIC$ WAIT-FOR TTRUE
   s" AOT wide format: it is not the zeroed or unrelocated answer" T-LABEL
   RBUF$ s" awb-big=0" CONTAINS? 0= TTRUE
   4 SEND-C
   s" AOT wide format: the over-64 KiB engine exits 0" T-LABEL
   PID @ >PID PROC-WAIT-RC MATCH result
     ok  OF 0 T= ENDOF
     err OF drop 1 0 T= ENDOF
   ;MATCH
   MFD @ close ;

: PROBE-BIG-WINDOW ( -- )
   s" HABU_AOT_BIG" BUILD-MODE
   s" AOT wide format: the over-64 KiB variant built" T-LABEL
   HBPWID$ EXISTS? TTRUE
   ASSERT-BIG-WINDOW-BOOTS ;

\ The out-of-line name, proved where it can only be proved. The reporter's own
\ name is longer than a dictionary record can hold, so its record keeps the name
\ elsewhere and the seed points [24] at the pooled bytes. The boot-run resolves
\ its entries through LFIND, which for such a record compares the token against
\ the bytes at [24] - so a wrong pointer there finds nothing and EM-AOT-BOOTRUN
\ exits $52 with no report at all. Reading the magic back is the pointer's proof.
: EXT-MAGIC$ ( -- ptr u8 n )
   s" awb-ext=6510767442340633178" ;

: ASSERT-EXT-NAME-BOOTS ( -- )
   HBPWID$ PTY-SPAWN
   s" AOT out-of-line name: the long-named word is found and runs at boot" T-LABEL
   EXT-MAGIC$ WAIT-FOR TTRUE
   4 SEND-C
   s" AOT out-of-line name: the engine exits 0, not the boot-run's not-found" T-LABEL
   PID @ >PID PROC-WAIT-RC MATCH result
     ok  OF 0 T= ENDOF
     err OF drop 1 0 T= ENDOF
   ;MATCH
   MFD @ close ;

: PROBE-EXT-NAME ( -- )
   s" HABU_AOT_EXT" BUILD-MODE
   s" AOT out-of-line name: the variant built" T-LABEL
   HBPWID$ EXISTS? TTRUE
   ASSERT-EXT-NAME-BOOTS ;

\ The named code site, whose whole content is WHICH of two addresses ends up in
\ the chain. The window holds a quotation returning 11 and a word returning 22;
\ the capture moved the quotation's chain out of the rebased list and into the
\ named list keyed on the word. So 22 is the named row, 11 is the rebase that
\ would have happened without it, and the zero the capture left is a fault.
: XT-MAGIC$ ( -- ptr u8 n )       s" awb-xt=22" ;
: XT-REBASED$ ( -- ptr u8 n )     s" awb-xt=11" ;

: ASSERT-XTSITE-BOOTS ( -- )
   HBPWID$ PTY-SPAWN
   s" AOT named code site: the chain carries the word the row names" T-LABEL
   XT-MAGIC$ WAIT-FOR TTRUE
   s" AOT named code site: not the address the rebase would have written" T-LABEL
   RBUF$ XT-REBASED$ CONTAINS? 0= TTRUE
   4 SEND-C
   s" AOT named code site: the engine exits 0" T-LABEL
   PID @ >PID PROC-WAIT-RC MATCH result
     ok  OF 0 T= ENDOF
     err OF drop 1 0 T= ENDOF
   ;MATCH
   MFD @ close ;

: PROBE-XTSITE ( -- )
   s" HABU_AOT_XTSITE" BUILD-MODE
   s" AOT named code site: the variant built" T-LABEL
   HBPWID$ EXISTS? TTRUE
   ASSERT-XTSITE-BOOTS ;

\ The pre-window elimination, proved where the relocation actually happens (dot
\ habu-aot-pre-window-0b01043c). The window word reads the first cell of a PREFIX
\ `create` whose body the engine used to copy into it, carrying an address the
\ window could not describe. The engine now declines that copy and emits a call,
\ which the capture records by NAME and the seed resolves in the engine it is
\ booting - so what reaches the report is whatever that name resolves to THERE.
\ The prefix word is sha256.f's HH0, whose first cell is the SHA-256 seed constant
\ $6a09e667 = 1779033703, initialised by the prefix load of the built engine. Only
\ the right address in the right engine yields it: a call that resolved to some
\ other word, or a read of the zeroed window DATA, gives a different number, and a
\ name the seed could not find never reports at all (EM-AOT-PATCH-SITES exits $51).
: PRE-MAGIC$ ( -- ptr u8 n )     s" awb-pre=1779033703" ;
: PRE-ZEROED$ ( -- ptr u8 n )    s" awb-pre=0" ;

: ASSERT-PREWINDOW-BOOTS ( -- )
   HBPWID$ PTY-SPAWN
   s" AOT pre-window: the relocated call reads the prefix word's own cell" T-LABEL
   PRE-MAGIC$ WAIT-FOR TTRUE
   s" AOT pre-window: not the zero an unrelocated or window-DATA read gives" T-LABEL
   RBUF$ PRE-ZEROED$ CONTAINS? 0= TTRUE
   4 SEND-C
   s" AOT pre-window: the engine exits 0" T-LABEL
   PID @ >PID PROC-WAIT-RC MATCH result
     ok  OF 0 T= ENDOF
     err OF drop 1 0 T= ENDOF
   ;MATCH
   MFD @ close ;

: PROBE-PREWINDOW ( -- )
   s" HABU_AOT_PREWIN" BUILD-MODE
   s" AOT pre-window: the variant built" T-LABEL
   HBPWID$ EXISTS? TTRUE
   ASSERT-PREWINDOW-BOOTS ;

: BODY ( -- )
   RENDER-SPAN
   s" AOT data span guard: rendered span text parses back to 2*DATA-SIZE" T-LABEL
   OVERSIZED-SPAN$ STR>NUMBER? MATCH option
     some OF  DATA-SIZE 2 *  T=  ENDOF
     none OF  T-FAIL  ENDOF
   ;MATCH
   HB-TARGET-LINUX? 0= if
      s" aot-data-span-forge: PTY boot cases run on linux only; skipped" type cr exit then
   SETUP
   BUILD-FORGED
   s" AOT data span guard: forged variant image exists after build" T-LABEL
   HBPWID$ EXISTS? TTRUE
   ASSERT-FORGED-DIES
   ASSERT-LEGAL-BOOTS
   PROBE-WINDOW-CONTENT
   PROBE-BIG-WINDOW
   PROBE-EXT-NAME
   PROBE-XTSITE
   PROBE-PREWINDOW ;

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
