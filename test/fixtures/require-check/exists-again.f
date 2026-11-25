\ exists-again.f - calls src/lib/fs.f's word, which only exists-user.f
\ requires.
require test/fixtures/require-check/exists-user.f
package RQC-EXISTS-AGAIN
public
: THERE? ( -- bool ) s" .." FS:EXISTS? ;
;package
