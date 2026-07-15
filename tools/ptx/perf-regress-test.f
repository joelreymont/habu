\ perf-regress-test.f - checked gate fixture for the perf-regression scan.
\ Runs the substantive committed-registry regression scan in-process (argv-free),
\ plus a deterministic regression for the cross-test state leak fixed by dot
\ habu-fix-perf-regress-b74ab681: after compare-style ADD-LINE fixtures, a LOAD
\ that stats a missing path throws before parsing, and its diagnostic accessors
\ (PERF:LINE@ / PERF:LAST-LINE$) must report the reset state, never a stale line
\ left by an earlier test. The CLI tools/ptx/perf-regress.f resolves its registry
\ path from ambient SCRIPT-ARGV, so it is spawn-only (a fresh image has clean
\ argv); this fixture carries the automatic in-process gate instead.

require lib/test.f
require lib/string.f
require lib/fmt.f
require tools/ptx/perf-compare.f

package PERF-REG-T

9 constant PRG-TAB

: TAB+ ( -- )
   PRG-TAB SB-APPEND-C ;

: PRG-DEV-ROW+ ( ptr u8 n ptr u8 n n -- ) {: ka:ptr ku:n da:ptr du:n v:n :}   \ GFLOPS row kernel ka/ku on device da/du, note "device isolation fixture"
   SB-RESET
   ka ku SB-APPEND TAB+
   s" 8" SB-APPEND TAB+ s" 8" SB-APPEND TAB+
   s" 16" SB-APPEND TAB+ s" 16" SB-APPEND TAB+
   s" 10" SB-APPEND TAB+ s" 4096" SB-APPEND TAB+
   s" GFLOPS" SB-APPEND TAB+ v SB-INT TAB+
   da du SB-APPEND TAB+ s" 2026-07-13" SB-APPEND TAB+
   s" device isolation fixture" SB-APPEND
   SB$ PERF:ADD-LINE ;

: PRG-LOAD-MISSING ( -- )   \ LOAD a path that does not stat: RESET then FILE-SIZE throws E-FS-STAT
   s" tools/ptx/perf-rows-does-not-exist.tsv" PERF:LOAD ;

: PRG-COMMITTED-SCAN ( -- )   \ the substantive automatic gate: committed registry has no regression
   s" tools/ptx/perf-rows.tsv" PERF:LOAD
   PERF:ROW# 0 > TTRUE
   PERF:SCAN 0 T= ;

: PRG-HERMETIC-TESTS ( -- )   \ a failed LOAD after prior fixtures must not leak a stale line
   PERF:RESET
   s" MMN" s" dev-25w" 2000 PRG-DEV-ROW+
   PERF:LAST-LINE$ nip 0 > TTRUE              \ the fixture row populated the line cursor
   [: PRG-LOAD-MISSING ;] E-FS-STAT TTHROWSQ  \ LOAD stats a missing path and throws before parse
   PERF:LINE@ 0 T=                            \ RESET cleared the line counter
   PERF:LAST-LINE$ nip 0 T= ;                 \ and the line buffer: no stale fixture row leaks

: PRG-FIXTURE-THEN-SCAN ( -- )   \ compare-style fixtures then a committed scan in ONE process stays clean
   PERF:RESET
   s" MMN" s" dev-25w" 2000 PRG-DEV-ROW+
   s" MMN" s" dev-25w" 500 PRG-DEV-ROW+
   PRG-COMMITTED-SCAN
   PERF:LAST-LINE$ nip 0 > TTRUE ;            \ the last line is a committed row, not a fixture leak

T-RESET
PRG-COMMITTED-SCAN
PRG-HERMETIC-TESTS
PRG-FIXTURE-THEN-SCAN
T-REPORT

;package
