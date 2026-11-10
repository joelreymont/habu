\ seal-first.f - the sealed program's first file, built by tools/hb-build.f:
\ it declares the package seal-second.f fills and seals, and a package whose
\ word seal-second.f exports into it.

package FILE-OBJECTS-SEAL-OTHER
public
: Z ( -- ) s" Z ran" type cr ;
;package

package FILE-OBJECTS-SEAL
public
: A ( -- ) s" A ran" type cr ;
;package
