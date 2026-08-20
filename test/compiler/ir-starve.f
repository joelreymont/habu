\ ir-starve.f - drive one compilation context's scratch to the mapping edge.
\
\ The shared fixture behind every IR table's torn-row case. An IR-ARENA never
\ allocates on its own: it grows by taking a fresh, doubled span from its
\ owning context's mapping (src/compiler/ir/arena.f, GROW-TO), so the way to
\ make a growth fail on the real path is to spend that mapping first. This is
\ not a hypothetical: src/compiler/ir/context.f MAP-BYTES records three
\ doublings driven by real passes refusing with E-IR-CTX-SCRATCH.
\
\ WHY THE MARGIN IS WHAT IT IS. An arena is created with
\ `min(ceiling, SEED-CELLS)` cells, and SEED-CELLS is eight. An arena whose
\ ceiling is eight cells or fewer therefore starts at its own maximum and can
\ never grow - a request past it is E-IR-ARENA-FULL and touches no scratch. So
\ every growth that reaches scratch at all starts from a capacity of eight and
\ asks for at least nine cells, seventy-two bytes. Leaving sixty-four bytes
\ makes every arena growth in that context refuse, and nothing smaller.
\
\ Sixty-four bytes is also exactly one eight-cell seed span, so do not create
\ an arena after starving: creation would spend the margin and succeed. Starve
\ after the tables exist, which is the order the real failure happens in - the
\ mapping fills up as a compilation runs, long after its tables were made.

require lib/prelude.f
require lib/errors.f
require src/compiler/ir/context.f

package IR-STARVE
private

\ Pinned from src/compiler/ir/context.f: the per-context mapping and the header
\ that sits at its front. EXACT-CASE in ir-starve-test.f fails loudly if either
\ pin stops matching the real mapping, so a silent under-starve - a fixture that
\ quietly stops proving anything - is not possible.
$80000 constant MAP-BYTES
16 CDIGEST:SLOT-BYTES * constant HDR-BYTES
MAP-BYTES HDR-BYTES - constant SCRATCH-CAP

public

\ The bytes left free by EDGE: under one arena doubling, over nothing.
64 constant MARGIN

\ Spend every scratch byte but MARGIN. A context with nothing left to spend
\ already refuses, which is a fixture error rather than a starved context, so
\ it is named rather than silently accepted.
: EDGE ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   SCRATCH-CAP c IR-CTX:SCRATCH-USED - MARGIN - {: burn:n :}
   burn 1 < if E-IR-CTX-SCRATCH throw then
   c burn IR-CTX:SCRATCH-TAKE 2drop ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;package
