\ gate-lint-tools-package-child.f - resident fork package-scope regression.
\
\ The lint-tools runner loads this file inside a fork.  Its package must open
\ successfully, proving the fork inherited global scope rather than the runner's
\ own package scope.

package GATE-LINT-TOOLS-PACKAGE-CHILD

public

: RUN ( -- ) ;

;package

GATE-LINT-TOOLS-PACKAGE-CHILD:RUN
