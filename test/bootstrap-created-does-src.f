\ bootstrap-created-does-src.f - a does>-created cell cannot mint a nominal family.
\
\ The third publisher: the effect a `does>` clause declares for the words its
\ defining word creates. It is published through `trust-raw` for the same reason
\ - the created word owns a cell of raw dictionary storage - so a declared
\ `( -- a )` carries a TVK-RAW variable and cannot be read back as a nominal.

package BCD
NEWTYPE dow-id 0
: MAKE ( n -- ) create , does> ( -- a ) @ ;
7 MAKE CELL
s" BOOTSTRAP-CREATED-ARMED" type cr
public
: LEAK ( -- dow-id ) CELL ;
;package
