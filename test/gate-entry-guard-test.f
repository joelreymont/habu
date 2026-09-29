\ Focused registry/load-path acceptance for the stdlib gate entry guard.
\ Run: bin/hb --load test/gate-entry-guard-test.f

require lib/test.f
require test/gate-entry-guard.f
require test/gate-stdlib-lib.f

T-RESET

\ Registered entries are roots; direct and helper imports run another root.
TEST:RESET
TEST:SUITE target test/gate-entry-guard-target.f TEST:;SUITE
TEST:SUITE direct test/gate-entry-guard-import-wrapper.f TEST:;SUITE
' ENTRY-GUARD:CHECK E-SUITE-ROW TTHROWS

TEST:RESET
TEST:SUITE target test/gate-entry-guard-target.f TEST:;SUITE
TEST:SUITE helper test/gate-entry-guard-helper-wrapper.f TEST:;SUITE
' ENTRY-GUARD:CHECK E-SUITE-ROW TTHROWS

TEST:RESET
TEST:SUITE target test/gate-entry-guard-target.f TEST:;SUITE
TEST:SUITE include test/gate-entry-guard-include-wrapper.f TEST:;SUITE
' ENTRY-GUARD:CHECK E-SUITE-ROW TTHROWS

TEST:RESET
TEST:SUITE target test/gate-entry-guard-target.f TEST:;SUITE
TEST:SUITE required test/gate-entry-guard-required-wrapper.f TEST:;SUITE
' ENTRY-GUARD:CHECK E-SUITE-ROW TTHROWS

\ Comments do not consume the path literal; aliases keep the same identity.
TEST:RESET
TEST:SUITE target test/gate-entry-guard-target.f TEST:;SUITE
TEST:SUITE commented test/gate-entry-guard-commented-required.f TEST:;SUITE
' ENTRY-GUARD:CHECK E-SUITE-ROW TTHROWS

TEST:RESET
TEST:SUITE target test/gate-entry-guard-target.f TEST:;SUITE
TEST:SUITE alias test/gate-entry-guard-alias-wrapper.f TEST:;SUITE
' ENTRY-GUARD:CHECK E-SUITE-ROW TTHROWS

TEST:RESET
TEST:SUITE target ./test/gate-entry-guard-target.f TEST:;SUITE
TEST:SUITE direct test/gate-entry-guard-import-wrapper.f TEST:;SUITE
' ENTRY-GUARD:CHECK E-SUITE-ROW TTHROWS

\ Exact literals consumed by load helpers are another way to run a root.
TEST:RESET
TEST:WHITEBOX-SUITE target test/gate-entry-guard-target.f TEST:;SUITE
TEST:SUITE diagnostics test/gate-entry-guard-diagnostics.f TEST:;SUITE
' ENTRY-GUARD:CHECK E-SUITE-ROW TTHROWS

TEST:RESET
TEST:SUITE target test/gate-entry-guard-target.f TEST:;SUITE
TEST:SUITE alias test/gate-entry-guard-alias-launch.f TEST:;SUITE
' ENTRY-GUARD:CHECK E-SUITE-ROW TTHROWS

\ An inert helper, comments and a discarded path literal are allowed.
TEST:RESET
TEST:SUITE target test/gate-entry-guard-target.f TEST:;SUITE
TEST:SUITE wrapper test/gate-entry-guard-wrapper.f TEST:;SUITE
TEST:SUITE read-only test/gate-entry-guard-read-only.f TEST:;SUITE
ENTRY-GUARD:CHECK

\ A tier prefix and repeated script argv do not add another execution root.
TEST:RESET
TEST:SUITE first test/compiler/aot-mode.f test/gate-entry-guard-wrapper.f -- first TEST:;SUITE
TEST:SUITE second test/compiler/aot-mode.f test/gate-entry-guard-wrapper.f -- second TEST:;SUITE
ENTRY-GUARD:CHECK

\ The real gate setup rejects the registry before fixture or pool work starts.
STDLIB-GATE:MAIN
TEST:SUITE target test/gate-entry-guard-target.f TEST:;SUITE
TEST:SUITE direct test/gate-entry-guard-import-wrapper.f TEST:;SUITE
' TEST:RUN E-SUITE-ROW TTHROWS

T-REPORT
s" gate-entry-guard-test: ok" type cr
