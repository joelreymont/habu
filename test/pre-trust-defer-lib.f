\ Pre-trust pending-table admission through the real source load. A `defer`
\ before checker.f's `: TRUST` enters the fixed PD table; DRAIN-PRETRUST must
\ publish its checked effect before capture, and SEAL-CAPTURE refuses leftovers.
\ Each entry copies src/lib to a private root and restores patched files between
\ cases. ARM builds one unseeded cold-prefix engine from build-fixpoint's source
\ appender. Intel loads the same copied prefix through native-build's target
\ window for every patched case, and the positive case emits and runs a native
\ image. The real workspace tree is never patched.
\
\ The ARM cold boot reaches CHECKER-CALLS:INSTALL and generated-constructor
\ planning before its seal; those two ordering controls stay ARM-only. Intel's
\ native source transfer reaches the pending-table seal first. The registered
\ compiler-native-hookless-reject E2E covers its distinct hook-off compiler path.
\ Every child status is checked with its own stdout/stderr on mismatch.

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
require lib/tree-copy.f
require tools/build-fixpoint.f
require test/suite-budget.f

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
create SUB-BUF  FS-PATH-CAP allot   variable SUB-U
create HB-BUF   FS-PATH-CAP allot   variable HB-U

variable LAST-OUT-U
variable LAST-ERR-U

: ABS? ( ptr u8 n -- bool ) {: a:ptr u:n :}  u 0 >  a c@ [char] / =  and ;

: HB$ ( -- ptr u8 n )                                \ selected host builds the private fixture
   s" HABU_UNDER_TEST" GETENV dup 0= if 2drop s" bin/hb" then {: e:ptr eu:n :}
   e eu ABS? if e eu exit then
   \ against the real working directory: a gate's `env -i` child has no PWD
   e eu FS-PATHZ HB-BUF FS-PATH-CAP realpath {: n:n :}
   n 0 <= if E-FS-PATH throw then
   n HB-U ! HB-BUF HB-U @ ;

: ROOT$ ( -- ptr u8 n )  ROOT-BUF ROOT-U @ ;

: NATIVE? ( -- bool ) HB-TARGET-LINUX-X86-64? ;

\ Both builders need the complete prefix trees. Intel's native build also loads
\ its tool closure from the private root, so a patched source file is the one
\ both the loader and its compiler read.
: COPY-SRC-TREE ( -- )
   s" src" ROOT$ TREE-COPY:TREE
   s" lib" ROOT$ TREE-COPY:TREE
   NATIVE? if s" tools" ROOT$ TREE-COPY:TREE then ;

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
      65 i 16 / + SB-APPEND-C   65 i 16 mod + SB-APPEND-C
      s"  ( -- )" SB-APPEND
      p pu SB$ APPEND-FILE
   loop ;

: APPEND-POS-DEFER ( -- )                             \ the positive case's ( -- n ) pre-trust defer
   s" src/core/exec-vector.f" SUB$ S\" \ndefer PTDX-POS ( -- n )\n" APPEND-FILE ;

: LONG-NAME$ ( -- ptr u8 n )                         \ 49 bytes; its qualified tail is only 39
   s" PTDX-NAME:ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLM" ;

: APPEND-LONG-NAME ( -- )
   SB-RESET
   S\" \ns\" defer " SB-APPEND  LONG-NAME$ SB-APPEND
   S\"  ( -- )\" evaluate\n" SB-APPEND
   s" src/core/exec-vector.f" SUB$ SB$ APPEND-FILE ;

: APPEND-LONG-SIG ( -- )                             \ a valid ( -- n ) effect with 65 inner bytes
   SB-RESET
   S\" \ndefer PTDX-SIG ( -- n" SB-APPEND
   60 0 do 32 SB-APPEND-C loop
   S\" )\n" SB-APPEND
   s" src/core/exec-vector.f" SUB$ SB$ APPEND-FILE ;

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

: NATIVE-BUILD-RC ( -- n )
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   s" tools/native-build.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   COLD$ >LEN PROC-ARGV+
   HB$ >LEN ROOT$ >LEN s" " >LEN OUT CAP >LEN ERR CAP >LEN
      SUITE-BUDGET:CHILD-MS >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-STDIN-CAPTURE CAPTURE-RC ;

: SPAWN-RC ( -- n )
   NATIVE? if NATIVE-BUILD-RC exit then
   s" " SPAWN-STDIN-RC ;

: OUT$ ( -- ptr u8 n )  OUT LAST-OUT-U @ ;
: ERR$ ( -- ptr u8 n )  ERR LAST-ERR-U @ ;

\ A mismatch includes the child's stdout/stderr and launch context, whether
\ the child was a cold boot or a native source build.
: CHILD-RC ( ptr u8 n n n -- ) {: la:ptr lu:n got:n want:n :}
   la lu T-LABEL
   got want <> if la lu want got OUT$ ERR$ SPAWN-REPORT:CHILD then
   got want T= ;

\ ARM recovery's source appender and image writer emit one unseeded engine.
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
   s" habu-pre-trust-defer" HB-TMP-MKDIR {: a:ptr u:n :}  a ROOT-BUF u BYTE-COPY  u ROOT-U !
   ROOT$ CLEANUP-TREE+
   COPY-SRC-TREE
   NATIVE? 0= if BUILD-COLD then ;

\ Cases in one entry share a private tree and restore the pristine files.
: RESTORE-FILES ( -- )
   s" src/core/exec-vector.f" ROOT$ TREE-COPY:FILE
   s" src/core/check-hook.f" ROOT$ TREE-COPY:FILE
   s" src/core/checker.f" ROOT$ TREE-COPY:FILE
   s" src/core/roles.f" ROOT$ TREE-COPY:FILE
   s" lib/string.f" ROOT$ TREE-COPY:FILE ;

: POSITIVE-CASE ( -- )
   APPEND-POS-DEFER
   APPEND-POS-SELFTEST
   NATIVE? if
      s" native prefix drains and checks the defer before capture"
         NATIVE-BUILD-RC 0 CHILD-RC
   then
   s" pre-trust defer drains, checked is installs, boots"
      s" PTDX-POS . cr" SPAWN-STDIN-RC 0 CHILD-RC
   s" installed body round-trips 42 at top level" T-LABEL
   OUT$ s" 42" CONTAINS? TTRUE
   RESTORE-FILES ;

: OVERFLOW-CASE ( -- )
   PD-CAP 2 * APPEND-DEFERS                            \ also overflows a doubled target built by an old donor
   s" pre-trust defer table overflow exits 72" SPAWN-RC 72 CHILD-RC
   s" overflow names the table-full diagnostic" T-LABEL
   ERR$ s" pre-trust defer table full" CONTAINS? TTRUE
   RESTORE-FILES ;

: SLOT-FULL-ERR ( ptr u8 n -- )
   {: a:ptr u:n :}
   SB-RESET  s" hb: pre-trust defer table full: " SB-APPEND
   a u SB-APPEND  10 SB-APPEND-C
   NATIVE? if 10 SB-APPEND-C then
   ERR$ SB$ T$= ;

: NAME-OVERFLOW-CASE ( -- )
   APPEND-LONG-NAME
   s" pre-trust original name overflow hard exits 72 inside evaluate" SPAWN-RC 72 CHILD-RC
   s" name overflow preserves the complete original qualified token" T-LABEL
   LONG-NAME$ SLOT-FULL-ERR
   RESTORE-FILES ;

: SIG-OVERFLOW-CASE ( -- )
   APPEND-LONG-SIG
   s" pre-trust effect overflow exits 72" SPAWN-RC 72 CHILD-RC
   s" effect overflow preserves the diagnostic and defer token" T-LABEL
   s" PTDX-SIG" SLOT-FULL-ERR
   RESTORE-FILES ;

: LINEAR-NO-HOOK-CASE ( -- )
   s" src/core/exec-vector.f" SUB$
   S\" \nLINEAR: NO-HOOK:GUARD ( n -- n )\n" APPEND-FILE
   s" a checker-less qualified LINEAR name refuses before registration"
      SPAWN-RC NATIVE? if 74 else 70 then CHILD-RC
   s" the fallback names the qualified token" T-LABEL
   ERR$ s" NO-HOOK:GUARD" CONTAINS? TTRUE
   NATIVE? if
      s" native build preserves the source throw code 70" T-LABEL
      OUT$ s" native-build: uncaught throw code 70" CONTAINS? TTRUE
   then
   RESTORE-FILES ;

\ ARM's checker.f tail has its registrar before the hook. Intel's native target
\ transfers that registrar and installs the hook before roles.f; the same
\ declaration there reaches the payload refusal instead of a missing registrar.
: LINEAR-CHECKER-UNIT-CASE ( -- )
   NATIVE? if
      s" src/core/roles.f" SUB$
         S\" \nLINEAR: PRE-HOOK:BAD-PAYLOAD ( n -- n )\n" APPEND-FILE
   else
      s" src/core/checker.f" SUB$
         S\" \nLINEAR: PRE-HOOK:BAD-PAYLOAD ( n -- n )\n" APPEND-FILE
   then
   s" a pre-hook qualified LINEAR name reaches its payload refusal"
      SPAWN-RC NATIVE? if 74 else 67 then CHILD-RC
   s" the payload refusal retains the certifier's throw" T-LABEL
   NATIVE? if OUT$ else ERR$ then
      s" uncaught throw code 7195" CONTAINS? TTRUE
   RESTORE-FILES ;

\ In the ARM cold boot, CHECKER-CALLS:INSTALL reads CWIN-STATE before seal.
\ Blanking the drain makes that first checked `is` refuse the real pending defer.
: UNDRAINED-CHECKED-CASE ( -- )
   BLANK-DRAIN
   s" blanked drain: the checker refuses an undrained defer reference, exits 70"
      SPAWN-RC 70 CHILD-RC
   s" undrained reference names the non-certified definition" T-LABEL
   ERR$ s" hook: non-certified definition" CONTAINS? TTRUE
   s" undrained reference names the failing defer" T-LABEL
   ERR$ s" at 'CWIN-STATE'" CONTAINS? TTRUE
   RESTORE-FILES ;

\ ARM cold-prefix continuation without its check hook still refuses generated
\ constructor declarations. This is separate from the isolated seal probes.
\ The refusal needs a GENERATED CONSTRUCTOR declaration in the cold prefix after
\ the blanked hook, and the case used to get one by accident: lib/string.f, a
\ prefix row, carried package STR and its str:split STRUCTURE. When that surface
\ moved to lib/string-roles.f - which is not a prefix row - the cold prefix had
\ no such declaration left, the child ran past every case this was meant to
\ catch, and died at the first compiled reference to an uncertified word instead
\ (`E-UNDEFINED: GETENV` while compiling src/core/top-row.f, rc 70). Nothing was
\ wrong with the engine; the fixture had lost its subject. So the case plants its
\ own declaration in a prefix row and restores the file afterwards, and what it
\ asserts no longer depends on which library happens to declare a structure.
: APPEND-CTOR-PROBE ( -- )
   s" lib/string.f" SUB$
   S\" \npackage PTD-CTOR\npublic\nSTRUCTURE probe 0\n  FIELD one n\n;STRUCTURE\n;package\n"
   APPEND-FILE ;

: HOOK-BLANK-CONTROL-CASE ( -- )
   BLANK-CHECK-HOOK
   APPEND-CTOR-PROBE
   s" blanked check hook alone refuses the prefix declarations, exits 76"
      SPAWN-RC 76 CHILD-RC
   s" hook-blank control names the refused constructor plan" T-LABEL
   ERR$ s" incomplete generated constructor plan" CONTAINS? TTRUE
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

: BEGIN-CASES ( -- )
   T-RESET
   FRESH-ROOT ;

: END-CASES ( -- )
   CLEANUP-RUN
   T-REPORT
   s" pre-trust-defer: ok" type cr ;

public

: RUN-POSITIVE ( -- )
   BEGIN-CASES
   POSITIVE-CASE
   END-CASES ;

: RUN-TABLE ( -- )
   BEGIN-CASES
   OVERFLOW-CASE
   NAME-OVERFLOW-CASE
   SIG-OVERFLOW-CASE
   NATIVE? 0= if UNDRAINED-CHECKED-CASE then
   END-CASES ;

: RUN-TYPE ( -- )
   BEGIN-CASES
   LINEAR-NO-HOOK-CASE
   LINEAR-CHECKER-UNIT-CASE
   END-CASES ;

: RUN-SEAL ( -- )
   BEGIN-CASES
   NATIVE? 0= if HOOK-BLANK-CONTROL-CASE then
   EARLY-SEAL-CONTROL-CASE
   END-CASES ;

: RUN-SEAL-BACKSTOP ( -- )
   BEGIN-CASES
   UNDRAINED-BACKSTOP-CASE
   END-CASES ;

;using
;package
