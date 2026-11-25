\ exists-good.f - exists-bad.f with src/lib/fs.f required.
require src/lib/fs.f
package RQC-EXISTS-GOOD
public
: HERE? ( -- bool ) s" ." FS:EXISTS? ;
;package
