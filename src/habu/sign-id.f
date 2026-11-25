\ sign-id.f - the code-signature identifier of each kind of image. Writers name
\ it only through these words: on macOS it is signed bytes counted in the
\ CodeDirectory, SuperBlob, LC_CODE_SIGNATURE and __LINKEDIT sizes, so a
\ one-byte-shorter name makes a one-byte-shorter image. Linux discards it.

package SIGN-ID

public

: PROG$ ( -- str )
   s" hb-prog" ;

: ENGINE$ ( -- str )
   s" hb" ;

;package
