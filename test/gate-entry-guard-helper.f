\ require test/gate-entry-guard-target.f is only documentation here.
\ s" test/gate-entry-guard-target.f" is also inert in this comment.
require lib/string.f

package ENTRY-GUARD-HELPER
public
: VALUE ( -- n ) 2 ;
: PATH-LENGTH ( -- n ) s" test/gate-entry-guard-target.f" nip ;
;package
