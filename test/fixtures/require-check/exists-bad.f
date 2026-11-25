\ exists-bad.f - calls a word of src/lib/fs.f, which it does not require.
package RQC-EXISTS-BAD
public
: HERE? ( -- bool ) s" ." FS:EXISTS? ;
;package
