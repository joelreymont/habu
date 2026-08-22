\ bootstrap-created-raw-src.f - a variable's cell cannot mint a nominal family.
\
\ The publication routes through `trust-raw` (src/core/checker.f TRUST-RAW), so
\ every type variable in the published effect is minted TVK-RAW: it admits plain
\ scalars and refuses to bind a nominal family. Reading a plain cell back as a
\ sealed nominal with no converter in between must therefore be refused.

package BCR
NEWTYPE nom-id 0
variable CELL
s" BOOTSTRAP-CREATED-ARMED" type cr
public
: LEAK ( -- nom-id ) CELL @ ;
;package
