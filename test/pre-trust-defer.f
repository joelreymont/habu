\ pre-trust-defer.f - capability + fail-closed regressions for the pre-trust defer
\ pending table (dot habu-engine-pre-trust-77410827). A `defer NAME ( E )` declared
\ in the engine prefix BEFORE `: TRUST` (src/core/checker.f) is copied into a fixed
\ pending table (src/habu/layout.f PD-*) and drained by the DRAIN-PRETRUST prim,
\ reached through the TRUSTED: DRAIN-PRETRUST-COMPAT runtime-lookup shim right
\ after `: TRUST`. All four properties only show at engine PREFIX load, which is
\ re-read from source at boot, so the suite copies the src tree ONCE to a private
\ root; each case patches the copy, boots the engine-under-test with CWD = that
\ root, then restores the touched files -- the real workspace tree is never
\ touched. Suite weight is four child-engine boots (~1s total): documented in the
\ manual/heavy set (tools/suite-coverage-lint-core.f), not fast-tier forked. Cases:
\   positive  - a pre-trust defer ( -- n ) + a post-hook CHECKED selftest that
\               `is`-installs [: 42 ;] and round-trips it: boots exit 0 and the
\               piped call prints 42. Proves capture -> drain -> trust row ->
\               checker-defer row -> checked `is` fit -> runtime dispatch.
\   compat    - the shim's looked-up prim name is patched to a miss: boots exit 0.
\               Pins the old-engine tolerance property (an engine without the
\               DRAIN-PRETRUST prim takes exactly this search-wl-miss branch); the
\               gate cannot depend on a historical fixpoint binary, so the miss
\               branch is pinned on the current engine instead -- the one-off
\               previous-fixpoint boot proof is recorded in the landing report.
\   overflow  - PD-CAP+headroom pre-trust defers (appended to exec-vector.f, the
\               earliest file where a defer is legal) overflow the table ->
\               C-PD-DIE-FULL, exit 72, table-full message.
\   undrained - one pre-trust defer + the shim region blanked leaves the table
\               non-empty at SEAL-CAPTURE -> BSEALCAP backstop, exit 73,
\               undrained message naming the defer.

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

: LOAD-CHECKER ( -- )
   s" src/core/checker.f" SUB$ FILE-BUF FILE-CAP READ-ALL FILE-U ! ;

: STORE-CHECKER ( -- )
   s" src/core/checker.f" SUB$ FILE-BUF FILE-U @ WRITE-ALL ;

\ Blank the shim region (drain call) between the unique PTD-REGRESSION-BLANK
\ sentinels, overwriting with spaces (length preserved): an injected pre-trust
\ defer then stays captured-but-undrained -> non-empty table at SEAL-CAPTURE ->
\ exit 73.
: BLANK-DRAIN ( -- )
   LOAD-CHECKER
   FILE-BUF FILE-U @ s" PTD-REGRESSION-BLANK-BEGIN" SCAN-SUB {: s:n :}
   FILE-BUF FILE-U @ s" PTD-REGRESSION-BLANK-END" SCAN-SUB {: e:n :}
   s 0 < e 0 < or if s" pre-trust-defer-test: fixture sentinels missing" 1 die then
   e 24 +                                                  \ 24 = len("PTD-REGRESSION-BLANK-END")
   s do  32 FILE-BUF i + c!  loop                          \ blank [BEGIN, END) with spaces (shim gone)
   STORE-CHECKER ;

\ Patch the shim's looked-up prim name to a guaranteed miss (same length, one byte
\ overwritten inside the s" ..." literal -- the scan needle includes the search-wl
\ tail so the PRIM: axiom spelling elsewhere in the file cannot match).
: PATCH-SHIM-MISS ( -- )
   LOAD-CHECKER
   FILE-BUF FILE-U @ S\" s\" DRAIN-PRETRUST\" 0 search-wl" SCAN-SUB {: off:n :}
   off 0 < if s" pre-trust-defer-test: shim lookup anchor missing" 1 die then
   [char] X FILE-BUF off 16 + + c!                         \ byte 16 = the final T of s" DRAIN-PRETRUST -> ...PRETRUSX
   STORE-CHECKER ;

\ ---- spawn + assert ------------------------------------------------------------

: SPAWN-RC ( -- n )                                    \ boot the engine under test with CWD = ROOT; capture out/err
   PROC-ARGV-RESET
   HB$ >LEN  ROOT$ >LEN  OUT CAP >LEN  ERR CAP >LEN  TIMEOUT-MS >MS
   RUN-ARGV-ENV-CWD-CAPTURE {: outu erru rc :}   \ typed-local-lint: allow-bare-local
   outu LEN>N LAST-OUT-U !
   erru LEN>N LAST-ERR-U !
   rc RC>N ;

: SPAWN-STDIN-RC ( ptr u8 n -- n ) {: in:ptr inu:n :}  \ same, piping a stdin program
   PROC-ARGV-RESET
   HB$ >LEN  ROOT$ >LEN  in inu >LEN  OUT CAP >LEN  ERR CAP >LEN  TIMEOUT-MS >MS
   RUN-ARGV-ENV-CWD-STDIN-CAPTURE {: outu erru rc :}   \ typed-local-lint: allow-bare-local
   outu LEN>N LAST-OUT-U !
   erru LEN>N LAST-ERR-U !
   rc RC>N ;

: OUT$ ( -- ptr u8 n )  OUT LAST-OUT-U @ ;
: ERR$ ( -- ptr u8 n )  ERR LAST-ERR-U @ ;

: FRESH-ROOT ( -- )
   CLEANUP-RESET
   s" habu-pre-trust-defer" TMPDIR-MKDIR {: a:ptr u:n :}  a ROOT-BUF u BYTE-COPY  u ROOT-U !
   ROOT$ CLEANUP-TREE+
   COPY-SRC-TREE ;

\ The tree is copied ONCE; each case patches at most three files and restores
\ the pristine copies afterwards (cases are sequential and independent), so the
\ suite pays one ~90-file copy + four child boots instead of four full copies.
: RESTORE-FILES ( -- )
   s" src/core/exec-vector.f" COPY-ONE
   s" src/core/check-hook.f" COPY-ONE
   s" src/core/checker.f" COPY-ONE ;

: POSITIVE-CASE ( -- )
   APPEND-POS-DEFER
   APPEND-POS-SELFTEST
   s" pre-trust defer drains, checked is installs, boots" T-LABEL
   s" PTDX-POS . cr" SPAWN-STDIN-RC 0 T=
   s" installed body round-trips 42 at top level" T-LABEL
   OUT$ s" 42" CONTAINS? TTRUE
   RESTORE-FILES ;

: COMPAT-MISS-CASE ( -- )
   PATCH-SHIM-MISS
   s" shim lookup miss boots (old-engine tolerance)" T-LABEL
   SPAWN-RC 0 T=
   RESTORE-FILES ;

: OVERFLOW-CASE ( -- )
   64 APPEND-DEFERS                                    \ PD-CAP=48 + headroom -> the 49th dies
   s" pre-trust defer table overflow exits 72" T-LABEL
   SPAWN-RC 72 T=
   s" overflow names the table-full diagnostic" T-LABEL
   ERR$ s" pre-trust defer table full" CONTAINS? TTRUE
   RESTORE-FILES ;

: UNDRAINED-CASE ( -- )
   1 APPEND-DEFERS
   BLANK-DRAIN
   s" undrained pre-trust defer table exits 73" T-LABEL
   SPAWN-RC 73 T=
   s" undrained names the backstop diagnostic" T-LABEL
   ERR$ s" undrained pre-trust defer" CONTAINS? TTRUE ;

public

: RUN ( -- )
   T-RESET
   FRESH-ROOT
   POSITIVE-CASE
   COMPAT-MISS-CASE
   OVERFLOW-CASE
   UNDRAINED-CASE
   CLEANUP-RUN
   T-REPORT
   s" pre-trust-defer: ok" type cr ;

;package

PRE-TRUST-DEFER-TEST:RUN
