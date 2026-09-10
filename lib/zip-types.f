\ zip-types.f - inspectable identities; live ownership is checked by ZIP.
package ZIP
public
NEWTYPE archive 0
NEWTYPE entry 0
$7A00 constant E-LIBRARY
$7A01 constant E-OPEN
$7A02 constant E-HANDLE
$7A03 constant E-ENTRY
$7A04 constant E-READ
$7A05 constant E-WRITE
$7A06 constant E-READONLY
$7A07 constant E-SIZE
$7A08 constant E-COMMIT
$7A09 constant E-PATH
private
CAST: >ARCHIVE ( n -- archive )
CAST: ARCHIVE>N ( archive -- n )
CAST: >ENTRY ( n -- entry )
CAST: ENTRY>N ( entry -- n )
;package
