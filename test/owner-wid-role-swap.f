\ owner-wid-role-swap.f - a sibling nominal locator-index must not substitute for
\ another at check time.
\
\ Negative checker fixture: OWNER-ROW-OFF takes an owner-row-index and
\ OWNER-ROW-SWAPPED hands it a prot-row-index instead. The two indices are
\ distinct nominal roles, so CHECK! must reject the swap (E-UNDEFINED-free rc 70).
\ NOMINAL: types render through the type-family substrate, so the diagnostic
\ reads "expected: owner-row-idx<> actual: prot-row-idx<>" (the <> is the
\ zero-arity family application). test/owner-wid-child.f loads this against the
\ built AOT and snapshot engines and asserts the rejection names both roles,
\ proving the restored image still refuses nominal locator confusion.
\
\ Same-type semantic-role gap (docs/forth.md "Same-cell values need nominal
\ roles"): on master the real owner-WID and protected-WID registry row indices
\ are both bare `n` (src/habu/aot-capture.f addresses rows as `idx OWNER-WID-ROW *`
\ and `idx 4 *`), so a real owner-row/prot-row swap is NOT rejected today. These
\ NOMINAL: indices are a faithful toy stand-in that pins the checker mechanism;
\ minting the real nominal row-index types is tracked by a dot.

require lib/type/value-nominal.f

package OWNER-WID-ROLE-SWAP

NOMINAL: OWNER-ROW-IDX
NOMINAL: PROT-ROW-IDX

: OWNER-ROW-OFF ( owner-row-idx -- n )
   OWNER-ROW-IDX>N ;

: OWNER-ROW-SWAPPED ( prot-row-idx -- n )
   OWNER-ROW-OFF ;

;package
