\ edit-b.f - reads build.f's generated edit/a.f's V, so its object names a.f's.

package FILE-OBJECTS-EDIT-B
public
: GET ( -- n ) FILE-OBJECTS-EDIT-A:V @ ;
;package
