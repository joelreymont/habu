\ codegen-compare-calibrate.f - the calibration row of a measurement pass.
\ One concern: measuring an empty call on each path, and declaring it the row
\ every other row of that path is divided by.
\
\ WHY THERE IS ONLY ONE COPY, AND WHY IT IS THE SAME EMPTY WORD FOR EVERY
\ CORPUS. A cost in a committed table is a ratio: how many times an empty call
\ this row is. That is what lets a table survive being regenerated on a faster
\ machine, and it only holds while the denominator means the same thing in every
\ table. Each corpus used to carry its own copy of these two cases - four of
\ each, five counting the harness's own tests - and a copy that drifted would
\ have rescaled a whole table without any compiler having changed. The four
\ tables now share one denominator by construction rather than by four files
\ agreeing.
\
\ `CODEGEN-CORPUS:NOOP` is `: NOOP ( -- ) ;` and `NOOP-N` is the same program
\ compiled by the chain. Nothing about either is particular to the first
\ corpus's subject matter: they are the floor of a call on each path.

require lib/prelude.f
require lib/string.f
require tools/codegen-compare-core.f
require tools/codegen-compare-corpus.f
require tools/codegen-compare-migrated.f

package CODEGEN-CALIBRATE

public

\ The old column's floor: the engine's own code for the empty word, measured and
\ declared this path's calibration row. It is measured first in every pass, and
\ CALIBRATE is called explicitly rather than the first row being assumed, so a
\ reordered case list cannot divide by the wrong measurement.
: OLD ( -- )
   s" CODEGEN-CORPUS:NOOP"
   [: CODEGEN-CORPUS:NOOP ;]
   [: ;]
   CODEGEN-COMPARE:MEASURE
   CODEGEN-COMPARE:CALIBRATE ;

\ The new column's floor: the chain's code for the same empty word. It returns
\ nothing, so it has no output to compare; what it measures is the floor of a
\ call on this path, which every other new row is divided by.
: NEW ( -- )
   s" CODEGEN-CORPUS:NOOP" s" CODEGEN-CORPUS:NOOP-N"
   [: CODEGEN-CORPUS:NOOP-N ;]
   [: ;]
   CODEGEN-COMPARE:MEASURE-NEW
   CODEGEN-COMPARE:CALIBRATE ;

;package
