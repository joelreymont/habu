\ exists-user.f - requires src/lib/fs.f and calls its word before
\ exists-again.f does.
require src/lib/fs.f
package RQC-EXISTS-USER
public
: HERE? ( -- bool ) s" ." FS:EXISTS? ;
;package
