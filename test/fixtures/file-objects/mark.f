\ mark.f - stores NOTHING's xt into its own FIRST, then count.f's SPARE, then its
\ own SECOND, so the build's cell rows hold an earlier file's cell between two of
\ this file's: a warm build's program must not depend on the order cells were
\ marked. Loaded after count.f. NOW is immediate and the file seals its package,
\ so the object carries both flags; KEEP-NOW keeps NOW in the program.

package FILE-OBJECTS-MARK
private
variable FIRST
variable SECOND
: NOTHING ( -- ) ;
: NOW ( -- ) ; immediate
' NOTHING FIRST xt!
' NOTHING FILE-OBJECTS-COUNT:SPARE xt!
' NOTHING SECOND xt!
variable KEEP-NOW  ' NOW KEEP-NOW xt!
seal-package
;package
