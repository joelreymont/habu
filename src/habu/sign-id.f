\ sign-id.f - the code-signature identifier of each kind of image. Writers name
\ it only through these words: on macOS it is signed bytes (sign2.f CODESIG2-BODY)
\ counted in the CodeDirectory, SuperBlob, LC_CODE_SIGNATURE and __LINKEDIT sizes,
\ so a one-byte-shorter name makes a one-byte-shorter image.
\ macho.f MACHO-SIG-MAX prices the longer of the two; Linux discards it. A leaf:
\ snap-lib.f SNAP:PERSIST signs from a booted engine that cannot load driver-io.f.

package SIGN-ID

public

: PROG$ ( -- ptr u8 n )
   s" hb-prog" ;

: ENGINE$ ( -- ptr u8 n )
   s" hb" ;

;package
