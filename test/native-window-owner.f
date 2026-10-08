\ native-window-owner.f - the window's checker is the sole certifier for window
\ source.
\
\ Each case runs test/native-window-owner-child.f - the tools/native-build.f
\ window reduced to the checker handover - in its own engine child, and asserts
\ the verdict that window reached for one fixture included straight after
\ src/core/cell-effects.f:
\   - a family the window itself declares resolves (0);
\   - a family nothing declares does not (E-CAST-FAM 7131). This is the control:
\     without it the accept case above would pass against a checker that
\     certifies nothing at all;
\   - a family only the host that opened the window declares does not
\     (E-CAST-FAM, never E-CAST-OWNER 7135). A product engine hosting the build
\     left its retained checker answering window certifications: that refused
\     the first case and resolved this one in a package the window cannot see;
\   - a prefix word compiled with the hook cell empty has its declaration as
\     its row (test/native-window-declared-row.f), at both tiers;
\   - a hook-less tier-1 definition whose declaration records no row, of the
\     name an owner-private primitive types, is refused catchably
\     (E-NELAB-UNDER -8304), alone and under a catch the window survives with
\     the row before it kept (test/native-window-private-axiom.f, -catch.f),
\     even a row of its own name that CHECK! recorded and the row after that
\     (-prior.f). Its rollback cuts only the rows recorded since its
\     definition began; asking the axiom instead, it died 76 (`checker:
\     missing signature truncation mark`), and cutting the live rows of the
\     symbol its name binds, it took the CHECK! row and every row after it.
\
\ The bindings fixture then loads its source closure at tier 0, and the last
\ case is ONE window at the optimizing tier that loads every tier-1 fixture
\ (TIER1-CASE below), the qualified-name gate among them: compiling the window's
\ core prefix through the optimizing chain is nearly all of a case's cost, so
\ the fixtures share one.
\
\ Every child takes the long row's child deadline (test/suite-budget.f): the
\ tier-1 window runs about 24 s of CPU, and the bound is there to catch a hang,
\ not to time a build.
\
\ Run: bin/hb --load test/native-window-owner.f

require lib/test.f
require lib/string.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require test/whitebox-child.f
require test/suite-budget.f              \ CHILD-MS, every child's hang guard

package NW-OWNER-TEST

$4000 constant IO-CAP

create OUT IO-CAP allot
create ERR IO-CAP allot

\ Every child here reopens the engine's native build window, which the sealed
\ product refuses: `hb: internal engine word: DECLARATIONS`, exit 70. So they
\ all run on the engine test/whitebox-child.f names.
: PREPARE ( -- )
   CLEANUP-RESET
   s" native-window-owner" WHITEBOX-CHILD:PROVIDE ;

: ARG+ ( ptr u8 n -- ) >LEN PROC-ARGV+ ;

: CHILD$ ( -- ptr u8 n ) s" test/native-window-owner-child.f" ;

: ARGS! ( ptr u8 n -- ) {: fx:ptr fxu:n :}
   PROC-ARGV-RESET
   s" --load" ARG+
   CHILD$ ARG+
   s" --" ARG+
   fx fxu ARG+
   WHITEBOX-CHILD:ENV! ;

: WINDOW-IS ( ptr u8 n ptr u8 n -- ) {: fx:ptr fxu:n want:ptr wantu:n :}
   fx fxu ARGS!
   WHITEBOX-CHILD:ENGINE$ >LEN OUT IO-CAP >LEN ERR IO-CAP >LEN
   SUITE-BUDGET:CHILD-MS >MS
   RUN-ARGV-ENV-CAPTURE-OUTCOME PROC-OUTCOME>RC RC>N {: outu:len erru:len rc:n :}
   rc 0 <> if ERR erru LEN>N type cr then
   rc 0 T=
   OUT outu LEN>N want wantu T$= ;

\ ---- the same window, compiled by the OPTIMIZING tier -------------------------
\ Tier 1 is the one that reaches the checker as a CALLER: it has the checker scan
\ each definition because the scan fills the source tape it elaborates from, so
\ every question the tier-1 fixtures are about is asked for real only there.
\ Tier 0 reads the engine's hook cell and asks nothing else, which is why the
\ three tier-0 cast cases pass on a compiler that resolves the checker by name.
\
\ STDERR IS HALF THE CLAIM. A TRUSTED: definition's body is scanned only for that
\ tape and its verdict is never enforced, so nothing may be rendered about it. The
\ suppression is the OWNER's, reached through the declaration record: a compiler
\ that bumped the quiet counter by name bumped the counter of the checker it was
\ compiled into while the window's checker did the rendering, and this window then
\ printed `habu: in install: at 'set-preflight'` for check-hook.f's own INSTALL --
\ measured on the engine before the fix, with the same `window: 0` on stdout. So
\ stderr is asserted byte-exact: the bindings window expects none, and the tier-1
\ window exactly the one refusal a fixture judges, the optimizing compiler's
\ `ncomp: cannot compile NCOMP-NAME-A:SAME` from the qualified-name gate. Any
\ other byte means a scan nobody judges reached a renderer again.
: WINDOW-RESULT ( ptr u8 n ptr u8 n -- ) {: want:ptr wantu:n ewant:ptr ewantu:n :}
   WHITEBOX-CHILD:ENGINE$ >LEN OUT IO-CAP >LEN ERR IO-CAP >LEN
   SUITE-BUDGET:CHILD-MS >MS
   RUN-ARGV-ENV-CAPTURE-OUTCOME PROC-OUTCOME>RC RC>N {: outu:len erru:len rc:n :}
   rc 0 <> if ERR erru LEN>N type cr then
   rc 0 T=
   OUT outu LEN>N want wantu T$=
   ERR erru LEN>N ewant ewantu T$= ;

\ The source loader and layout a fixture's real require closure needs.
: SOURCE-DEPS+ ( -- )
   HB-TARGET-LINUX? if
      s" src/os/linux/target.f" ARG+
      s" src/os/linux/layout.f" ARG+
   else
      s" src/os/macos/target.f" ARG+
      s" src/os/macos/layout.f" ARG+
   then
   s" src/habu/stack-abi.f" ARG+
   s" src/habu/layout.f" ARG+
   s" src/core/bytes.f" ARG+
   s" src/os/env-base.f" ARG+
   s" src/core/include.f" ARG+ ;

\ The compiler target declares families, so its require closure follows the
\ same declaration prefix as LOAD-TARGET, after the early handover checks.
: DECL-DEPS+ ( -- )
   s" src/core/declaration-transaction.f" ARG+
   s" src/core/generated-declaration.f" ARG+
   s" src/core/decl-event.f" ARG+
   s" src/core/structure-make.f" ARG+
   s" src/core/structure-decl.f" ARG+
   s" src/core/enum-decl.f" ARG+
   s" src/core/structures.f" ARG+ ;

: BINDINGS-CASE ( -- )
   s" test/native-window-owner-bindings.f" ARGS!
   SOURCE-DEPS+
   S\" window: 0\n" s" " WINDOW-RESULT ;

\ The tier-1 window loads its script arguments in order, then the fixture
\ argument last; a fixture that refuses stops it at its own code. The order is
\ each claim's precondition:
\   - straight after src/core/cell-effects.f, as each ran in a window of its
\     own: the window's own family resolves (cast-ok), the checked store
\     adopted the bootstrap one (call-store), and the rebuilt checker's
\     pointer-pool clear (test/compiler/native-checker-storage.f);
\   - the source loader and layout the later require closures need;
\   - the qualified-name gate (test/compiler/native-qualified-name.f), once
\     src/habu/xref.f and lib/errors.f are in: it reopens XREF, patch32's
\     owner, to move a pending record to another namespace, which the
\     optimizing compiler's name-identity gate must refuse;
\   - the declared row of a prefix word compiled with the hook cell empty
\     (declared-row): the tier-1 scan's row, else the declaration;
\   - the dictionary boundary (fixed), asserting a FRESH owner at its load;
\   - test/native-window-capture.f, the fixture argument: the capture seam.
\     It requires the fixtures whose checks it runs - the family readers and
\     the compiler adapter, asserting the by-name regime at their load, then
\     payload validation and tape-detach - and runs those checks in the order
\     their preconditions need. It loads the prefix-boundary rollback
\     (test/compiler/native-prefix-rollback.f) between payload's preparations,
\     which compact the no-return rows without a boundary, a branch the
\     build's own capture never takes, and tape-detach's, which compact
\     against its mark.
: TIER1-CASE ( -- )
   PROC-ARGV-RESET
   s" --load" ARG+
   s" test/compiler/aot-mode.f" ARG+
   CHILD$ ARG+
   s" --" ARG+
   s" test/native-window-capture.f" ARG+
   s" test/native-window-cast-ok.f" ARG+
   s" test/native-window-call-store.f" ARG+
   s" test/compiler/native-checker-storage.f" ARG+
   DECL-DEPS+
   SOURCE-DEPS+
   s" src/core/enums.f" ARG+
   s" src/core/sha256.f" ARG+
   s" src/core/type-family-sha.f" ARG+
   s" src/core/combinators.f" ARG+
   s" src/habu/code-span.f" ARG+
   s" src/habu/xref.f" ARG+
   s" lib/errors.f" ARG+
   s" test/compiler/native-qualified-name.f" ARG+
   s" src/core/generated-declaration-dictionary.f" ARG+
   s" src/core/generated-declaration-protection.f" ARG+
   s" src/core/dynamic-storage.f" ARG+
   s" test/native-window-declared-row.f" ARG+
   s" test/native-window-owner-fixed.f" ARG+
   WHITEBOX-CHILD:ENV!
   S\" window: 0\n" S\" ncomp: cannot compile NCOMP-NAME-A:SAME\n" WINDOW-RESULT ;

: OWNER-CASES ( -- )
   PREPARE
   s" test/native-window-cast-ok.f"       S\" window: 0\n"    WINDOW-IS
   s" test/native-window-cast-bad.f"      S\" window: 7131\n" WINDOW-IS
   s" test/native-window-cast-host-bad.f" S\" window: 7131\n" WINDOW-IS
   s" test/native-window-declared-row.f"  S\" window: 0\n"    WINDOW-IS
   s" test/native-window-private-axiom.f" S\" window: -8304\n" WINDOW-IS
   s" test/native-window-private-axiom-catch.f" S\" -8304\n-1\nwindow: 0\n" WINDOW-IS
   s" test/native-window-private-axiom-prior.f" S\" -1\n-8304\n-1\n-1\nwindow: 0\n" WINDOW-IS
   BINDINGS-CASE
   TIER1-CASE ;

\ ---- a recovery row at the handover -----------------------------------------
\ A refused definition's row is a fact of the run that refused it, and the
\ transfer re-records every row it imports as an active one. So it refuses this
\ one by name before recreating anything (src/core/checker.f TRANSFER-ROW);
\ carried, the row became active in the window and the window ended `window: 0`.
\ The child's --recovery-row mode leaves that row in the retained checker just
\ before the transfer.
: RECOVERY-CASE ( -- )
   s" test/native-window-cast-ok.f" ARGS!
   s" --recovery-row" ARG+
   WHITEBOX-CHILD:ENGINE$ >LEN OUT IO-CAP >LEN ERR IO-CAP >LEN SUITE-BUDGET:CHILD-MS >MS
   RUN-ARGV-ENV-CAPTURE-OUTCOME PROC-OUTCOME>RC RC>N {: outu:len erru:len rc:n :}
   rc 76 <> if OUT outu LEN>N type ERR erru LEN>N type cr then
   rc 76 T=
   ERR erru LEN>N s" checker: a failed declaration's row does not transfer"
   CONTAINS? TTRUE ;

\ Public so the driver below runs it with the package closed.
public

: RUN ( -- )
   T-RESET
   [: OWNER-CASES RECOVERY-CASE ;] [: CLEANUP-RUN ;] finally
   T-REPORT
   s" native-window-owner: ok" type cr ;

;package

NW-OWNER-TEST:RUN
