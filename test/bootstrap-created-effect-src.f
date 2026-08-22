\ bootstrap-created-effect-src.f - a created word's published effect is enforced.
\
\ `variable` publishes `-- ptr a` from its definer. The caller below declares
\ `( -- )` for it - the effect the check hook infers from the seeded body
\ "NAME create" when nothing publishes - so the file certifies only if the
\ published row is missing or ignored.

package BCE
variable CELL
s" BOOTSTRAP-CREATED-ARMED" type cr
public
: WRONG ( -- ) CELL ;
;package
