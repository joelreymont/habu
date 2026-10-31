\ zip-types.f - inspectable identities; live ownership is checked by ZIP.
\ STORAGE CLASS. CALLER-OWNED: the module keeps no state.
package ZIP
public
NEWTYPE archive
NEWTYPE entry
NEWTYPE input
\ $7A00 stays unused: the first call reports a libzip that does not open as
\ E-FFI-DLOPEN and a missing symbol as E-FFI-DLSYM.
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
CAST: >INPUT ( n -- input )
CAST: INPUT>N ( input -- n )
CAST: >ARCHIVE-SLOT ( ptr n -- ptr archive )
CAST: >INPUT-SLOT ( ptr n -- ptr input )
;package
