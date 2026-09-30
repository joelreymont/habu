\ Focused registry/load-path acceptance for the stdlib gate entry guard.
\ Run: bin/hb --load test/gate-entry-guard-test.f

require lib/test.f
require test/gate-images.f
require test/gate-entry-guard.f
require test/gate-stdlib-lib.f

package ENTRY-GUARD-TEST

\ The gate reads the registry's load graph, then walks it (SUITE-SETUP).
: GUARD ( -- )
   GATE-IMAGES:DERIVE
   ENTRY-GUARD:CHECK ;

T-RESET

\ Registered entries are roots; direct and helper imports run another root.
TEST:RESET
TEST:SUITE target test/gate-entry-guard-target.f TEST:;SUITE
TEST:SUITE direct test/gate-entry-guard-import-wrapper.f TEST:;SUITE
' GUARD E-SUITE-ROW TTHROWS

TEST:RESET
TEST:SUITE target test/gate-entry-guard-target.f TEST:;SUITE
TEST:SUITE helper test/gate-entry-guard-helper-wrapper.f TEST:;SUITE
' GUARD E-SUITE-ROW TTHROWS

TEST:RESET
TEST:SUITE target test/gate-entry-guard-target.f TEST:;SUITE
TEST:SUITE include test/gate-entry-guard-include-wrapper.f TEST:;SUITE
' GUARD E-SUITE-ROW TTHROWS

TEST:RESET
TEST:SUITE target test/gate-entry-guard-target.f TEST:;SUITE
TEST:SUITE required test/gate-entry-guard-required-wrapper.f TEST:;SUITE
' GUARD E-SUITE-ROW TTHROWS

\ Comments do not consume the path literal; aliases keep the same identity.
TEST:RESET
TEST:SUITE target test/gate-entry-guard-target.f TEST:;SUITE
TEST:SUITE commented test/gate-entry-guard-commented-required.f TEST:;SUITE
' GUARD E-SUITE-ROW TTHROWS

TEST:RESET
TEST:SUITE target test/gate-entry-guard-target.f TEST:;SUITE
TEST:SUITE alias test/gate-entry-guard-alias-wrapper.f TEST:;SUITE
' GUARD E-SUITE-ROW TTHROWS

TEST:RESET
TEST:SUITE target ./test/gate-entry-guard-target.f TEST:;SUITE
TEST:SUITE direct test/gate-entry-guard-import-wrapper.f TEST:;SUITE
' GUARD E-SUITE-ROW TTHROWS

\ Exact literals consumed by load helpers are another way to run a root.
TEST:RESET
TEST:WHITEBOX-SUITE target test/gate-entry-guard-target.f TEST:;SUITE
TEST:SUITE diagnostics test/gate-entry-guard-diagnostics.f TEST:;SUITE
' GUARD E-SUITE-ROW TTHROWS

TEST:RESET
TEST:SUITE target test/gate-entry-guard-target.f TEST:;SUITE
TEST:SUITE alias test/gate-entry-guard-alias-launch.f TEST:;SUITE
' GUARD E-SUITE-ROW TTHROWS

\ A launched file runs in a child, so what it imports is walked too.
TEST:RESET
TEST:SUITE target test/gate-entry-guard-target.f TEST:;SUITE
TEST:SUITE launch test/gate-entry-guard-launch-wrapper.f TEST:;SUITE
' GUARD E-SUITE-ROW TTHROWS

\ An inert helper, comments and a discarded path literal are allowed.
TEST:RESET
TEST:SUITE target test/gate-entry-guard-target.f TEST:;SUITE
TEST:SUITE wrapper test/gate-entry-guard-wrapper.f TEST:;SUITE
TEST:SUITE read-only test/gate-entry-guard-read-only.f TEST:;SUITE
GUARD

\ A tier prefix and repeated script argv do not add another execution root.
TEST:RESET
TEST:SUITE first test/compiler/aot-mode.f ENTRIES test/gate-entry-guard-wrapper.f -- first TEST:;SUITE
TEST:SUITE second test/compiler/aot-mode.f ENTRIES test/gate-entry-guard-wrapper.f -- second TEST:;SUITE
GUARD

\ A file may launch itself, even as an entry tier twins share.
TEST:RESET
TEST:SUITE self test/gate-entry-guard-self-launch.f TEST:;SUITE
TEST:SUITE self-aot test/compiler/aot-mode.f ENTRIES test/gate-entry-guard-self-launch.f TEST:;SUITE
GUARD

\ A row may import one of its own entries from another, unless another row
\ registers that entry too.
TEST:RESET
TEST:SUITE pair test/gate-entry-guard-import-wrapper.f test/gate-entry-guard-target.f TEST:;SUITE
GUARD
TEST:SUITE target test/gate-entry-guard-target.f TEST:;SUITE
' GUARD E-SUITE-ROW TTHROWS

\ Shared preloads are loaded by each row but are not suite entries.
TEST:RESET
TEST:SUITE first lib/test.f ENTRIES test/gate-entry-guard-wrapper.f TEST:;SUITE
TEST:SUITE second lib/test.f ENTRIES test/gate-entry-guard-read-only.f TEST:;SUITE
GUARD

\ An import in a preload can still launch another row's entry.
TEST:RESET
TEST:SUITE target test/gate-entry-guard-target.f TEST:;SUITE
TEST:SUITE wrapper test/gate-entry-guard-import-wrapper.f ENTRIES test/gate-entry-guard-wrapper.f TEST:;SUITE
' GUARD E-SUITE-ROW TTHROWS

\ A preload path that aliases another entry must be rejected directly.
TEST:RESET
TEST:SUITE target test/gate-entry-guard-target.f TEST:;SUITE
TEST:SUITE alias ./test/gate-entry-guard-target.f ENTRIES test/gate-entry-guard-wrapper.f TEST:;SUITE
' GUARD E-SUITE-ROW TTHROWS

\ Every file after ENTRIES is an entry, including the first of several.
TEST:RESET
TEST:SUITE target test/gate-entry-guard-target.f test/gate-entry-guard-read-only.f TEST:;SUITE
TEST:SUITE wrapper test/gate-entry-guard-import-wrapper.f TEST:;SUITE
' GUARD E-SUITE-ROW TTHROWS

\ Every registered file must be one the load graph holds.
TEST:RESET
TEST:SUITE missing test/gate-entry-guard-missing.f TEST:;SUITE
' GUARD E-SUITE-ROW TTHROWS

\ The real gate setup rejects the registry before fixture or pool work starts.
STDLIB-GATE:MAIN
TEST:SUITE target test/gate-entry-guard-target.f TEST:;SUITE
TEST:SUITE direct test/gate-entry-guard-import-wrapper.f TEST:;SUITE
' TEST:RUN E-SUITE-ROW TTHROWS

;package

T-REPORT
s" gate-entry-guard-test: ok" type cr
