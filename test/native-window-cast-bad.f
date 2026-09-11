\ native-window-cast-bad.f - the control: a cast naming a family that exists
\ nowhere. A checker that certifies nothing accepts this, so the accept case
\ next door proves nothing without it. E-CAST-FAM.
package NW-BAD
public
NEWTYPE alpha 0
private
CAST: MINT-X ( n -- NW-BAD:nosuchfam )
;package
