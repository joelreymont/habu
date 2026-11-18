split-str
\ zip-types.f - inspectable identities; live ownership is checked by ZIP.
\ STORAGE CLASS. CALLER-OWNED: the module keeps no state.
require src/lib/error.f

package ZIP
public
NEWTYPE archive
NEWTYPE entry
NEWTYPE input
\ $7A00 stays unused: the first call reports a libzip that does not open as
\ E-FFI-DLOPEN and a missing symbol as E-FFI-DLSYM.
$7A01 s" not an archive this reader takes: its end record or directory is malformed, or libzip cannot open it"
   ERROR:CODE E-OPEN
$7A02 s" an archive or member input that is closed or was never opened" ERROR:CODE E-HANDLE
$7A03 s" an entry index outside the archive, an entry of another archive, or a removed entry" ERROR:CODE E-ENTRY
$7A04 s" a member's bytes disagree with the archive's records or its CRC, or libzip failed to read or close it"
   ERROR:CODE E-READ
$7A06 s" a change to an archive opened read-only: open it with ZIP:EDIT" ERROR:CODE E-READONLY
$7A07 s" a length that is negative or too large for an archive's fields, or no handle numbers left" ERROR:CODE E-SIZE
$7A08 s" the new archive could not be built or written beside the old: the handle stays open to close or commit again"
   ERROR:CODE E-COMMIT
$7A09 s" an empty archive path" ERROR:CODE E-PATH
private
CAST: >ARCHIVE ( n -- archive )
CAST: ARCHIVE>N ( archive -- n )
CAST: >ENTRY ( n -- entry )
CAST: ENTRY>N ( entry -- n )
CAST: >INPUT ( n -- input )
CAST: INPUT>N ( input -- n )
CAST: >ARCHIVE-SLOT ( ^n -- ^archive )
CAST: >INPUT-SLOT ( ^n -- ^input )
;package
