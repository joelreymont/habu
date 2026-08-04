\ pre-trust-defer.f - capability + fail-closed regressions for the pre-trust defer
\ pending table (dot habu-engine-pre-trust-77410827). A `defer NAME ( E )` declared
\ in the engine prefix BEFORE `: TRUST` (src/core/checker.f) is copied into a fixed
\ pending table (src/habu/layout.f PD-*) and drained by the DRAIN-PRETRUST prim,
\ called by its bare token right after `: TRUST`. All three properties only show at
\ engine PREFIX load, which is re-read from source at boot, so the suite copies the
\ src tree ONCE to a private root; each case patches the copy, boots the
\ engine-under-test with CWD = that root, then restores the touched files -- the
\ real workspace tree is never touched. Suite weight is five child-engine boots
\ (~2s total), so it runs in the standalone stdlib gate instead of the fast tier.
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
\               An engine in that state refuses to boot, and it refuses TWICE, in
\               this order -- one case each, because asserting only the second one
\               made this fixture red the moment the first one started firing:
\                 1. the CHECKER refuses. The first checked `is` binding an
\                    undrained pre-trust defer has no checker-defer row, so the
\                    check hook rejects it: exit 70, "hook: non-certified
\                    definition: <word> at 'is'". Today that word is
\                    src/habu/xref.f INSTALL (`[: LIVE ;] is PKG-LIVE-XT`), the
\                    first such site loaded after src/core/check-hook.f.
\                 2. the RUNTIME backstop refuses. Reached only once the checker
\                    is out of the way, so this case also blanks check-hook.f's
\                    bare INSTALL call (between the PTD-HOOK-BLANK sentinels):
\                    the table is then non-empty at SEAL-CAPTURE -> BSEALCAP,
\                    exit 73, "undrained pre-trust defer" naming TFAM-RESOLVE-XT.
\                    A control case boots the SAME hook-blanked tree with the
\                    drain intact and requires exit 0, so the 73 is attributable
\                    to the undrained table and not to the missing hook.
\               Together: an engine that cannot run the drain refuses to run
\               rather than proceed with un-installed checker hooks (an old engine
\               lacking the prim sees the bare token as E-UNDEFINED, exit 70, and
\               never boots either). The prior runtime-lookup shim
\               (TRUSTED: DRAIN-PRETRUST-COMPAT) and its shim-specific "compat"
\               lookup-miss case were retired 2026-07-19 when the bare token
\               replaced the shim; the gate cannot depend on a historical fixpoint
\               binary, so the previous-fixpoint boot proof stays in the landing
\               report.
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

package PRE-TRUST-DEFER-TEST
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

: HB$ ( -- ptr u8 n )                                \ ABSOLUTE engine path (child runs with CWD = temp root)
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

: COPY-SRC-TREE ( -- )  s" src" [: COPY-ENTRY ;] WALK-FILES ;

: SUB$ ( ptr u8 n -- ptr u8 n )                       \ ROOT/<rel> absolute path
   ROOT$ 2swap SUB-BUF JOIN-PATH SUB-U ! SUB-BUF SUB-U @ ;

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

\ Blank the bare INSTALL call that arms the source checker hook, so nothing after
\ check-hook.f is checked and the undrained table reaches the runtime backstop.
: BLANK-CHECK-HOOK ( -- )
   s" src/core/check-hook.f"
   s" PTD-HOOK-BLANK-BEGIN" s" PTD-HOOK-BLANK-END" BLANK-REGION ;

\ ---- spawn + assert ------------------------------------------------------------

\ Boot the engine under test with CWD = ROOT, capture out/err, and ALWAYS give the
\ child an explicit stdin pipe. A capture spawn with infd < 0 skips the dup2 and
\ hands the child the launcher's own fd 0 (src/habu/habu1.f SPAWN-DUP2-ACTION),
\ while posix_spawn makes it a process-group leader. Launched from a terminal the
\ child engine therefore found a tty on fd 0, entered the REPL, and its terminal
\ ioctl stopped it with SIGTTOU as a background process group: the boot never
\ returned and the case died on the 20s timeout (E-PROC-TIMEOUT) instead of
\ reporting an exit code. The empty pipe makes the child see a closed stdin - the
\ state every case here assumes - from a pipe, a terminal, or a gate pool slot
\ alike. test/gate-env-stdin-tty-test.f holds the same property for GE-RUN-ENV.
: SPAWN-STDIN-RC ( ptr u8 n -- n ) {: in:ptr inu:n :}
   PROC-ARGV-RESET
   HB$ >LEN  ROOT$ >LEN  in inu >LEN  OUT CAP >LEN  ERR CAP >LEN  TIMEOUT-MS >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-STDIN-CAPTURE
   MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE {: o:len e:len :} o LEN>N LAST-OUT-U !  e LEN>N LAST-ERR-U !  0 ENDOF
     err OF PCAP-FAILED:UNMAKE {: o:len e:len c:rc :} o LEN>N LAST-OUT-U !  e LEN>N LAST-ERR-U !  c RC>N ENDOF
   ;MATCH ;

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

: FRESH-ROOT ( -- )
   CLEANUP-RESET
   s" habu-pre-trust-defer" TMPDIR-MKDIR {: a:ptr u:n :}  a ROOT-BUF u BYTE-COPY  u ROOT-U !
   ROOT$ CLEANUP-TREE+
   COPY-SRC-TREE ;

\ The tree is copied ONCE; each case patches at most three files and restores
\ the pristine copies afterwards (cases are sequential and independent), so the
\ suite pays one ~90-file copy + five child boots instead of five full copies.
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
\ captured-but-undrained. The FIRST engine word that then fails is a checked `is`
\ binding one of them -- it has no checker-defer row, so the check hook rejects
\ the definition and the boot dies at exit 70 naming the token. Asserting the
\ exit-73 backstop here instead is what rotted this fixture: the backstop is real
\ but unreachable while the checker refuses earlier, and the case went red the day
\ a checked `is` site was added to the prefix.
: UNDRAINED-CHECKED-CASE ( -- )
   BLANK-DRAIN
   s" blanked drain: the checker refuses the first `is` on an undrained defer, exits 70"
      SPAWN-RC 70 CHILD-RC
   s" undrained-is names the non-certified definition" T-LABEL
   ERR$ s" hook: non-certified definition" CONTAINS? TTRUE
   s" undrained-is names the failing token" T-LABEL
   ERR$ s" at 'is'" CONTAINS? TTRUE
   RESTORE-FILES ;

\ Control for the backstop case: the SAME hook-blanked tree with the drain intact
\ must boot clean. Without it the next case's exit 73 could be blamed on the
\ missing check hook instead of on the undrained table.
: HOOK-BLANK-CONTROL-CASE ( -- )
   BLANK-CHECK-HOOK
   s" blanked check hook alone still boots" SPAWN-RC 0 CHILD-RC
   RESTORE-FILES ;

\ Drain blanked AND check hook blanked: nothing after check-hook.f is checked, so
\ the checked-`is` refusal above cannot fire and the RUNTIME backstop is reached.
\ The table is non-empty at SEAL-CAPTURE -> BSEALCAP fails closed at exit 73,
\ naming TFAM-RESOLVE-XT. This is the property that let the runtime-lookup shim
\ go: an engine that cannot execute the drain refuses to boot rather than run with
\ un-installed checker hooks. TFAM-RESOLVE-XT is asserted by name so the case
\ fails loudly for re-audit if that specific prefix hook is ever removed.
: UNDRAINED-BACKSTOP-CASE ( -- )
   BLANK-DRAIN
   BLANK-CHECK-HOOK
   s" blanked drain leaves real prefix defers undrained, exits 73" SPAWN-RC 73 CHILD-RC
   s" undrained names the backstop diagnostic" T-LABEL
   ERR$ s" undrained pre-trust defer" CONTAINS? TTRUE
   s" undrained names the real prefix defer TFAM-RESOLVE-XT" T-LABEL
   ERR$ s" TFAM-RESOLVE-XT" CONTAINS? TTRUE
   RESTORE-FILES ;

public

: RUN ( -- )
   T-RESET
   FRESH-ROOT
   POSITIVE-CASE
   OVERFLOW-CASE
   UNDRAINED-CHECKED-CASE
   HOOK-BLANK-CONTROL-CASE
   UNDRAINED-BACKSTOP-CASE
   CLEANUP-RUN
   T-REPORT
   s" pre-trust-defer: ok" type cr ;

;package

PRE-TRUST-DEFER-TEST:RUN
