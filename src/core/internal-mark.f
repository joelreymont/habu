\ internal-mark.f - the last prefix source before top-row.f. It loads
\ prefix-boundary.f, which installs the checker's FIRST-RECORD-XT as it loads,
\ and holds ENGINE-INTERNAL, a sealed package with no word. It changes no record
\ an earlier file made: a word is restricted by its package's private section,
\ and the checker refuses a checked call it has no row for.
require src/core/prefix-boundary.f

package ENGINE-INTERNAL
public

seal-package
;package
