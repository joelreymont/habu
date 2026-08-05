\ codegen-compare-ns.f - a picosecond count as nanoseconds. One concern: how a
\ time is written down.
\
\ WHY IT IS PICOSECONDS UNDERNEATH. A per-call cost in this harness is a few
\ nanoseconds, so whole nanoseconds would leave one significant digit and a
\ table of them would say every small row costs the same. The store keeps
\ thousandths of a nanosecond and this is the one place that turns one into
\ text, so the report's rows, the report's floors and the cross-corpus ranking
\ are the same number written the same way rather than three roundings.
\
\ A NEGATIVE TIME IS PRINTED AS ONE. A row can measure faster than its own entry
\ floor, which means it is at the resolution of the measurement; the minus sign
\ is how a reader is told that, and clamping it to zero would hide exactly the
\ rows a reader should not trust.

require lib/errors.f
require lib/prelude.f
require lib/string.f
require lib/fmt.f
require tools/codegen-compare-core.f

package CODEGEN-NS

private

: PER-NS ( -- n )
   CODEGEN-COMPARE:PICOS-PER-NS ;

\ The fractional thousandths, always three digits, so a column of times lines up
\ and 1.020 is not written 1.20.
: FRACTION ( n -- ) {: frac:n :}
   frac 100 < if s" 0" SB-APPEND then
   frac 10 < if s" 0" SB-APPEND then
   frac FMT:SB-INT ;

public

: NS$ ( n -- ptr u8 n ) {: picos:n :}
   SB-RESET
   picos 0 < if s" -" SB-APPEND then
   picos 0 < if picos negate else picos then {: q:n :}
   q PER-NS / FMT:SB-INT
   s" ." SB-APPEND
   q PER-NS mod FRACTION
   SB$ ;

: .NS ( n -- )
   NS$ type ;

;package
