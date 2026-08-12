\ aot-wide-format-suite.f - the AOT capture format past its 64 KiB world
\ (dot habu-widen-the-aot-089f5faf).
\
\ WHAT THIS LOCKS. Every offset in the baked ahead-of-time frame used to be a
\ u16: the call-site rows (blob-off, name-off), the DATA-site list, the CODE-site
\ list and the window's declared-address-cell list. Sixteen bits cannot name a
\ byte past 65535, so a captured window could never be larger than that whatever
\ the buffers said - and the capture died at AOT-BLOB-CAP with "blob exceeds
\ buffer" before it could get there. The compiler chain this seed exists to carry
\ measures 1.15 MB. With the fields widened to u32 and the buffers lifted to
\ match, a window several times the old ceiling must capture, bake and boot.
\
\ HOW IT IS PROVEN, AND WHAT IS PROVEN WHERE. test/aot-wid-build.f is spawned in
\ a child process with HABU_AOT_BIG=1 and a private HB_TMP. That mode compiles a
\ few hundred filler words and then, ABOVE them, the three things that have to
\ survive the crossing: a data cell, a callee too long for the inliner to copy,
\ and a reporter that calls the callee and prints the cell. The window is taken
\ in by the same widened re-capture the other window fixtures use - the real
\ AOT-CAPTURE:CAPTURE entry point, not a stand-in - and the builder then reads
\ its own capture tables and DIES unless all three of these clear the old
\ ceiling: the blob length, the highest call-site blob offset, and the highest
\ DATA-site blob offset. Those refusals are the assertions; the three lines this
\ suite matches are printed only on the far side of them, so a window that
\ stopped being big enough fails the build instead of quietly proving nothing.
\
\ NOT COVERED HERE, and covered rather than faked: that the engine so built
\ actually BOOTS and reports the magic. The AOT seed is armed at the interactive
\ REPL entry and nowhere else, so only a PTY boot can observe it; that half lives
\ in test/aot-data-span-forge.f beside the other seed-pass boot regressions, and
\ runs on Linux hosts, which are the ones this tree's PTY helper supports. What
\ this suite adds on every host is that the capture and the bake of an
\ over-64 KiB window succeed at all, which is the half that used to be impossible.
\
\ Cost: one child engine build, larger than the plain variant because the maker
\ compiles the filler. Registered as `TEST:SUITE aot-wide-format` in
\ test/gate-stdlib-cases.f. Run standalone:
\   bin/hb --load test/aot-wide-format-suite.f

require lib/errors.f
require lib/string.f
require lib/fmt.f
require lib/test.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f

package AOT-WIDE-FORMAT

$8000 constant CAP
240000 constant BUILD-TIMEOUT-MS
30000  constant PROBE-TIMEOUT-MS

create OUT CAP allot     variable OUT-U
create ERR CAP allot     variable ERR-U
create EMPTY 1 allot                 \ zero-length stdin
variable RC

create ROOT-BUF FS-PATH-CAP allot    variable ROOT-U
create HB-BUF FS-PATH-CAP allot      variable HB-U

: ROOT$ ( -- ptr u8 n ) ROOT-BUF ROOT-U @ ;
: HB$ ( -- ptr u8 n )   HB-BUF HB-U @ ;
: PLAIN$ ( -- ptr u8 n ) s" bin/hb" ;
: OUT$ ( -- ptr u8 n )  OUT OUT-U @ ;
: ERR$ ( -- ptr u8 n )  ERR ERR-U @ ;

: SETUP ( -- )
   CLEANUP-RESET
   s" habu-aot-wide" TMPDIR-MKDIR {: a:ptr u:n :}
   a ROOT-BUF u BYTE-COPY  u ROOT-U !
   ROOT$ CLEANUP-TREE+
   ROOT$ s" hb-pwid" HB-BUF JOIN-PATH HB-U ! ;

: BUILDER-ARGV ( -- )
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   s" test/aot-wid-build.f" >LEN PROC-ARGV+ ;

: BUILD-BIG ( -- )
   PROC-ENV-RESET
   s" HB_TMP" >LEN ROOT$ >LEN PROC-ENV+
   s" HABU_AOT_BIG" >LEN s" 1" >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   BUILDER-ARGV
   PLAIN$ >LEN  OUT CAP >LEN  ERR CAP >LEN  BUILD-TIMEOUT-MS >MS
   RUN-ARGV-ENV-CAPTURE
   MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE {: o:len e:len :}
            o LEN>N OUT-U !  e LEN>N ERR-U !  0 RC ! ENDOF
     err OF PCAP-FAILED:UNMAKE {: o:len e:len c:rc :}
            o LEN>N OUT-U !  e LEN>N ERR-U !  c RC>N RC ! ENDOF
   ;MATCH ;

\ The built engine still runs an ordinary batch program. A batch boot never arms
\ the seed, so this says nothing about the blob; it says the image the widened
\ bake produced is a working engine, which is what makes the PTY sibling's
\ report attributable to the seed rather than to a broken build.
: BATCH-OK ( -- )
   PROC-ARGV-RESET
   HB$ >LEN  s" 7 6 * . cr" >LEN  OUT CAP >LEN  ERR CAP >LEN  PROBE-TIMEOUT-MS >MS
   RUN-ARGV-STDIN-CAPTURE MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE {: o:len e:len :}
            o LEN>N OUT-U !  e LEN>N ERR-U !  0 RC ! ENDOF
     err OF PCAP-FAILED:UNMAKE {: o:len e:len c:rc :}
            o LEN>N OUT-U !  e LEN>N ERR-U !  c RC>N RC ! ENDOF
   ;MATCH ;

: BODY ( -- )
   SETUP
   BUILD-BIG
   s" a capture window past the old 64 KiB ceiling builds cleanly" T-LABEL
   RC @ 0 T=
   RC @ 0 <> if s" aot-wide-format: builder stderr:" type cr  ERR$ type cr  RC @ throw then
   s" the captured blob passed 64 KiB" T-LABEL
   OUT$ s" aot-wid-build: big-blob " CONTAINS? TTRUE
   s" a call site was recorded above the old u16 offset ceiling" T-LABEL
   OUT$ s" aot-wid-build: big-site " CONTAINS? TTRUE
   s" a DATA site was recorded above the old u16 offset ceiling" T-LABEL
   OUT$ s" aot-wid-build: big-dsite " CONTAINS? TTRUE
   s" the over-64 KiB variant image exists after the build" T-LABEL
   HB$ EXISTS? TTRUE
   BATCH-OK
   s" the over-64 KiB variant still runs a batch program" T-LABEL
   RC @ 0 T=
   s" and computes with it" T-LABEL
   OUT$ s" 42" CONTAINS? TTRUE ;

public

: RUN ( -- )
   T-RESET
   CLEANUP-RESET
   [: BODY ;] catch {: code:n :}
   CLEANUP-RUN
   code 0 <> if code throw then
   T-REPORT
   s" aot-wide-format: ok" type cr ;

;package

AOT-WIDE-FORMAT:RUN
