\ pre-trust-defer.f - capability + fail-closed regressions for the pre-trust defer
\ pending table. A `defer NAME ( E )` declared
\ in the engine prefix BEFORE `: TRUST` (src/core/checker.f) is copied into a fixed
\ pending table (src/habu/layout.f PD-*) and drained by the DRAIN-PRETRUST prim,
\ called by its bare token right after `: TRUST`. All three properties only show at
\ engine PREFIX load. The suite builds one cold stdin engine with the selected
\ host and copies the src/lib trees ONCE to a private root. Each case patches
\ the copy, boots that cold engine with CWD = the root, then restores the files -- the
\ real workspace tree is never touched. The canonical native registry runs it.
\ Cases:
\   positive  - a pre-trust defer ( -- n ) + a post-hook CHECKED selftest that
\               `is`-installs [: 42 ;] and round-trips it: boots exit 0 and the
\               piped call prints 42. Proves capture -> drain -> trust row ->
\               checker-defer row -> checked `is` fit -> runtime dispatch.
\   overflow  - PD-CAP+headroom pre-trust defers (appended to exec-vector.f, the
\               earliest file where a defer is legal) overflow the table ->
\               C-PD-DIE-FULL, exit 72, table-full message.
\   undrained - the WHOLE bare-token drain region (between the PTD-REGRESSION-BLANK
\               sentinels) is blanked, so DRAIN-PRETRUST is never called and the
\               prefix's own real pre-trust defers stay captured-but-undrained.
\               The checker rejects the first checked reference to an undrained
\               defer: CHECKER-CALLS:INSTALL at CWIN-STATE, exit 70.
\   hook blank - disabling the check hook alone refuses later generated
\               constructor declarations, exit 76.
\   seal control/backstop - disabling both drain and hook otherwise meets that
\               constructor refusal before reaching the final seal. Append the
\               real SEAL-CAPTURE directly after the blanked hook, then exit with
\               a success marker. With the drain intact this reaches the marker,
\               exit 0; without it the seal refuses first, exit 73, naming the
\               real pending TFAM-RESOLVE-XT. Neither probe continues with an
\               artificially early seal floor.
\ Every child exit code is asserted through CHILD-RC, which prints the child's own
\ stdout/stderr and this process's launch context (lib/test/spawn-report.f) when
\ the code is not the expected one -- an unexpected exit arrives with the child's
\ diagnostic attached instead of as a bare number.

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/process-cwd.f
require lib/test/spawn-report.f
require lib/codesign.f
require tools/build-fixpoint.f

package PRE-TRUST-DEFER-TEST
using BUILD-FIXPOINT
private

$8000 constant CAP                                   \ capture buffers (prefix diagnostics are small)
$400000 constant FILE-CAP                            \ per-file patch buffer (checker.f is the largest)
20000 constant TIMEOUT-MS

create OUT CAP allot
create ERR CAP allot
create FILE-BUF FILE-CAP allot   variable FILE-U

create ROOT-BUF FS-PATH-CAP allot   variable ROOT-U
create DST-BUF  FS-PATH-CAP allot   variable DST-U
create SUB-BUF  FS-PATH-CAP allot   variable SUB-U
create HB-BUF   FS-PATH-CAP allot   variable HB-U

variable LAST-OUT-U
variable LAST-ERR-U

: ABS? ( ptr u8 n -- bool ) {: a:ptr u:n :}  u 0 >  a c@ [char] / =  and ;

: HB$ ( -- ptr u8 n )                                \ selected host builds the cold fixture once
   s" HABU_UNDER_TEST" GETENV dup 0= if 2drop s" bin/hb" then {: e:ptr eu:n :}
   e eu ABS? if e eu exit then
   s" PWD" GETENV e eu HB-BUF JOIN-PATH HB-U ! HB-BUF HB-U @ ;

: ROOT$ ( -- ptr u8 n )  ROOT-BUF ROOT-U @ ;

\ ---- tree copy: replicate src/ into ROOT/src (parents + files) -----------------

: PARENT-U ( ptr u8 n -- n ) {: a:ptr u:n :}          \ length of the dir prefix (0 = no slash)
   u begin dup 0 > while 1- dup a + c@ [char] / = if 1+ exit then repeat drop 0 ;

: COPY-ONE ( ptr u8 n -- ) {: a:ptr u:n :}            \ copy src-relative file a u into ROOT/a
   ROOT$ a u DST-BUF JOIN-PATH DST-U !
   DST-BUF DST-U @ {: d:ptr du:n :}
   d du PARENT-U {: pu:n :}
   pu 0 > if d pu MAKE-DIRS then
   a u d du COPY-FILE-STREAM ;

: COPY-ENTRY ( ptr u8 n -- )  2dup FILE? if COPY-ONE else 2drop then ;

\ The sandbox has to hold everything the engine reads at boot, or the child dies
\ naming a missing prefix file instead of reaching the refusal a case is about.
\ Since dot habu-seed-the-stdlib-d8e3a757 that is no longer only src/: the cold
\ prefix also loads the checked stdlib out of lib/. Both trees are walked whole
\ rather than listing the prefix files, so a later prefix row inside either tree
\ cannot silently leave this sandbox short.
: COPY-SRC-TREE ( -- )
   s" src" [: COPY-ENTRY ;] WALK-FILES
   s" lib" [: COPY-ENTRY ;] WALK-FILES ;

: SUB$ ( ptr u8 n -- ptr u8 n )                       \ ROOT/<rel> absolute path
   ROOT$ 2swap SUB-BUF JOIN-PATH SUB-U ! SUB-BUF SUB-U @ ;

: COLD$ ( -- ptr u8 n ) s" hb-cold" SUB$ ;

\ ---- patches -------------------------------------------------------------------

\ exec-vector.f (prefix position 5) is the earliest file where a `defer` is legal:
\ it defines DEFER-UNSET and ends at global scope, and it loads before checker.f's
\ `: TRUST`, so defers appended here are pre-trust.
: APPEND-DEFERS ( n -- ) {: count:n :}                \ append `count` uniquely-named ( -- ) pre-trust defers
   s" src/core/exec-vector.f" SUB$ {: p:ptr pu:n :}
   count 0 do
      SB-RESET
      s\" \ndefer PTDX-" SB-APPEND
      65 i 8 / + SB-APPEND-C   65 i 8 mod + SB-APPEND-C
      s"  ( -- )" SB-APPEND
      p pu SB$ APPEND-FILE
   loop ;

: APPEND-POS-DEFER ( -- )                             \ the positive case's ( -- n ) pre-trust defer
   s" src/core/exec-vector.f" SUB$ S\" \ndefer PTDX-POS ( -- n )\n" APPEND-FILE ;

\ check-hook.f installs the check hook at its own load, so source appended AFTER it
\ compiles CHECKED -- the selftest's `is` must certify through the drained
\ checker-defer row, and the runtime check proves the installed body dispatches.
: APPEND-POS-SELFTEST ( -- )
   s" src/core/check-hook.f" SUB$
   S\" \n: PTD-POS-SELFTEST ( -- )\n   [: 42 ;] is PTDX-POS\n   PTDX-POS 42 <> IF s\" pre-trust-defer-test: positive round-trip failed\" 76 die THEN ;\nPTD-POS-SELFTEST\n"
   APPEND-FILE ;

\ SCAN-SUB ( hay-a hay-u needle-a needle-u -- off | -1 ): first byte offset of the
\ needle, or -1. A plain byte scan so no option-fold plumbing is needed here.
: SCAN-SUB ( ptr u8 n ptr u8 n -- n ) {: ha:ptr hu:n na:ptr nu:n :}
   hu nu < if -1 exit then
   0 begin dup hu nu - <= while
      dup ha + nu na nu STR= if exit then           \ offset i stays on the stack
      1+
   repeat drop -1 ;

: LOAD-FILE ( ptr u8 n -- ) {: a:ptr u:n :}
   a u SUB$ FILE-BUF FILE-CAP READ-ALL FILE-U ! ;

: STORE-FILE ( ptr u8 n -- ) {: a:ptr u:n :}
   a u SUB$ FILE-BUF FILE-U @ WRITE-ALL ;

\ Blank a sentinel-delimited region of one copied source file, overwriting it
\ with spaces (length preserved, newlines included) so the whole region becomes
\ the tail of the `\` comment line that opens it. The sentinels are unique, so a
\ missing one is a fixture fault, not a silent no-op patch.
: BLANK-REGION ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: fa:ptr fu:n ba:ptr bu:n ea:ptr eu:n :}
   fa fu LOAD-FILE
   FILE-BUF FILE-U @ ba bu SCAN-SUB {: s:n :}
   FILE-BUF FILE-U @ ea eu SCAN-SUB {: e:n :}
   s 0 < e 0 < or if s" pre-trust-defer-test: fixture sentinels missing" 1 die then
   e eu +
   s do  32 FILE-BUF i + c!  loop                          \ blank [BEGIN, END] with spaces
   fa fu STORE-FILE ;

\ Blank the bare-token drain region so DRAIN-PRETRUST is never called: the
\ prefix's own real pre-trust defers then stay captured-but-undrained.
: BLANK-DRAIN ( -- )
   s" src/core/checker.f"
   s" PTD-REGRESSION-BLANK-BEGIN" s" PTD-REGRESSION-BLANK-END" BLANK-REGION ;

\ Blank the bare INSTALL call that arms the source checker hook.
: BLANK-CHECK-HOOK ( -- )
   s" src/core/check-hook.f"
   s" PTD-HOOK-BLANK-BEGIN" s" PTD-HOOK-BLANK-END" BLANK-REGION ;

\ Isolate the production seal guard before later constructor declarations need
\ the disabled hook. Both drained and undrained probes receive this same patch.
: APPEND-SEAL-PROBE ( -- )
   s" src/core/check-hook.f" SUB$
   S\" \nSEAL-CAPTURE\ns\" pre-trust seal passed\" 0 die\n" APPEND-FILE ;

\ ---- spawn + assert ------------------------------------------------------------

\ Boot the private cold engine with CWD = ROOT, capture out/err, and ALWAYS give the
\ child an explicit stdin pipe. A capture spawn with infd < 0 skips the dup2 and
\ hands the child the launcher's own fd 0 (src/habu/habu1.f SPAWN-DUP2-ACTION),
\ while posix_spawn makes it a process-group leader. Launched from a terminal the
\ child engine therefore found a tty on fd 0, entered the REPL, and its terminal
\ ioctl stopped it with SIGTTOU as a background process group: the boot never
\ returned and the case died on the 20s timeout (E-PROC-TIMEOUT) instead of
\ reporting an exit code. The empty pipe makes the child see a closed stdin - the
\ state every case here assumes - from a pipe, a terminal, or a gate pool slot
\ alike.
: CAPTURE-RC ( result<pcap:captured,pcap:failed> -- n )
   MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE {: o:len e:len :} o LEN>N LAST-OUT-U !  e LEN>N LAST-ERR-U !  0 ENDOF
     err OF PCAP-FAILED:UNMAKE {: o:len e:len c:rc :} o LEN>N LAST-OUT-U !  e LEN>N LAST-ERR-U !  c RC>N ENDOF
   ;MATCH ;

: SPAWN-STDIN-RC ( ptr u8 n -- n ) {: in:ptr inu:n :}
   PROC-ARGV-RESET
   COLD$ >LEN  ROOT$ >LEN  in inu >LEN  OUT CAP >LEN  ERR CAP >LEN  TIMEOUT-MS >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-STDIN-CAPTURE CAPTURE-RC ;

: SPAWN-RC ( -- n )                                    \ boot with an empty stdin
   s" " SPAWN-STDIN-RC ;

: OUT$ ( -- ptr u8 n )  OUT LAST-OUT-U @ ;
: ERR$ ( -- ptr u8 n )  ERR LAST-ERR-U @ ;

\ Assert a child boot's exit code. On a mismatch the child's own stdout/stderr
\ and this process's launch context are printed first: every failure in this file
\ is a child that exited differently than expected, and the reason is always in
\ what that child printed.
: CHILD-RC ( ptr u8 n n n -- ) {: la:ptr lu:n got:n want:n :}
   la lu T-LABEL
   got want <> if la lu want got OUT$ ERR$ SPAWN-REPORT:CHILD then
   got want T= ;

\ The production source appender and image writer emit one unseeded engine.
\ Its output path is an argv element, so no path is interpolated into source.
: BUILD-COLD ( -- )
   ROOT$ BF-TMP!
   s" cold-src.f" BF-RESET-OUT
   s" cold-src.f" BF-APPEND-RUN-PRELUDE
   s" cold-src.f" BF-APPEND-COMMON
   s" cold-src.f" COMPILER-BUILD:SEAL
   s" cold-src.f" BF-APPEND-DRIVER-IO
   s" cold-src.f" s" test/pre-trust-cold-engine.f" BF-APPEND-SOURCE
   BF-TMP-RESET
   PROC-ARGV-RESET
   s" --build" >LEN PROC-ARGV+
   s" cold-src.f" SUB$ >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   COLD$ >LEN PROC-ARGV+
   HB$ >LEN OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS
   RUN-ARGV-CAPTURE CAPTURE-RC {: rc:n :}
   s" build the cold-prefix fixture engine" rc 0 CHILD-RC
   rc 0<> if CLEANUP-RUN s" pre-trust-defer-test: cold engine build failed" 76 die then
   COLD$ FILE? 0= if CLEANUP-RUN s" pre-trust-defer-test: cold engine output missing" 76 die then ;

: FRESH-ROOT ( -- )
   CLEANUP-RESET
   s" habu-pre-trust-defer" TMPDIR-MKDIR {: a:ptr u:n :}  a ROOT-BUF u BYTE-COPY  u ROOT-U !
   ROOT$ CLEANUP-TREE+
   COPY-SRC-TREE
   BUILD-COLD ;

\ The tree is copied ONCE; each case patches at most three files and restores
\ the pristine copies afterwards (cases are sequential and independent), so the
\ suite pays one tree copy and one cold engine build for all six child boots.
: RESTORE-FILES ( -- )
   s" src/core/exec-vector.f" COPY-ONE
   s" src/core/check-hook.f" COPY-ONE
   s" src/core/checker.f" COPY-ONE ;

: POSITIVE-CASE ( -- )
   APPEND-POS-DEFER
   APPEND-POS-SELFTEST
   s" pre-trust defer drains, checked is installs, boots"
      s" PTDX-POS . cr" SPAWN-STDIN-RC 0 CHILD-RC
   s" installed body round-trips 42 at top level" T-LABEL
   OUT$ s" 42" CONTAINS? TTRUE
   RESTORE-FILES ;

: OVERFLOW-CASE ( -- )
   64 APPEND-DEFERS                                    \ PD-CAP=48 + headroom -> the 49th dies
   s" pre-trust defer table overflow exits 72" SPAWN-RC 72 CHILD-RC
   s" overflow names the table-full diagnostic" T-LABEL
   ERR$ s" pre-trust defer table full" CONTAINS? TTRUE
   RESTORE-FILES ;

\ Blank the bare DRAIN-PRETRUST token outright so the drain never runs. No
\ synthetic defer is injected: the prefix's OWN real pre-trust defers stay
\ captured-but-undrained. CHECKER-CALLS:INSTALL now reads CWIN-STATE before its
\ `is` installation; without the drain that first reference has no trusted effect.
: UNDRAINED-CHECKED-CASE ( -- )
   BLANK-DRAIN
   s" blanked drain: the checker refuses an undrained defer reference, exits 70"
      SPAWN-RC 70 CHILD-RC
   s" undrained reference names the non-certified definition" T-LABEL
   ERR$ s" hook: non-certified definition" CONTAINS? TTRUE
   s" undrained reference names the failing defer" T-LABEL
   ERR$ s" at 'CWIN-STATE'" CONTAINS? TTRUE
   RESTORE-FILES ;

\ Normal prefix continuation without its check hook still refuses generated
\ constructor declarations. This is separate from the isolated seal probes.
: HOOK-BLANK-CONTROL-CASE ( -- )
   BLANK-CHECK-HOOK
   s" blanked check hook alone refuses the prefix declarations, exits 76"
      SPAWN-RC 76 CHILD-RC
   s" hook-blank control names the refused constructor plan" T-LABEL
   ERR$ s" incomplete generated constructor plan" CONTAINS? TTRUE
   s" hook-blank control is distinguishable from the backstop's 73" T-LABEL
   76 73 T<>
   RESTORE-FILES ;

: EARLY-SEAL-CONTROL-CASE ( -- )
   BLANK-CHECK-HOOK
   APPEND-SEAL-PROBE
   s" drained table passes the real seal guard, exits 0" SPAWN-RC 0 CHILD-RC
   s" drained seal control reaches the success marker" T-LABEL
   ERR$ s" pre-trust seal passed" CONTAINS? TTRUE
   RESTORE-FILES ;

\ The same early seal probe with real pending defers must refuse before its
\ success marker. It exercises BSEALCAP without later declarations masking it.
: UNDRAINED-BACKSTOP-CASE ( -- )
   BLANK-DRAIN
   BLANK-CHECK-HOOK
   APPEND-SEAL-PROBE
   s" blanked drain leaves real prefix defers undrained, exits 73" SPAWN-RC 73 CHILD-RC
   s" undrained names the backstop diagnostic" T-LABEL
   ERR$ s" undrained pre-trust defer" CONTAINS? TTRUE
   s" undrained names the real prefix defer TFAM-RESOLVE-XT" T-LABEL
   ERR$ s" TFAM-RESOLVE-XT" CONTAINS? TTRUE
   s" undrained seal refuses before the success marker" T-LABEL
   ERR$ s" pre-trust seal passed" CONTAINS? TFALSE
   RESTORE-FILES ;

public

: RUN ( -- )
   T-RESET
   FRESH-ROOT
   POSITIVE-CASE
   OVERFLOW-CASE
   UNDRAINED-CHECKED-CASE
   HOOK-BLANK-CONTROL-CASE
   EARLY-SEAL-CONTROL-CASE
   UNDRAINED-BACKSTOP-CASE
   CLEANUP-RUN
   T-REPORT
   s" pre-trust-defer: ok" type cr ;

;using
;package

PRE-TRUST-DEFER-TEST:RUN
