\ sign-id.f - the code-signature identifier of each kind of image.
\
\ Every writer names its identifier through one of these, never a literal of its
\ own: on macOS the identifier is signed bytes (src/os/macos/sign2.f CODESIG2-BODY),
\ held in the CodeDirectory and counted in its size, the SuperBlob's, and the
\ header's LC_CODE_SIGNATURE and __LINKEDIT sizes, so a writer with another name
\ writes another file for the same program. A one-byte-shorter name made a
\ one-byte-shorter image (tools/hb-build-stripped-cache-test.f
\ HBT-STRIPPED-OBJECT-RELINK). src/os/macos/macho.f MACHO-SIG-MAX prices the
\ longer of the two. Linux signers discard the identifier.
\
\ The words are a leaf of their own because two kinds of process sign images.
\ A builder signs through src/habu/driver-io.f DRV-EMIT-IMAGE; a booted engine
\ signs a snapshot through src/habu/snap-lib.f SNAP:PERSIST and cannot load
\ driver-io.f, which stops at E-UNDEFINED: MBUF. This file needs nothing, so
\ both require it.

package SIGN-ID

public

: PROG$ ( -- ptr u8 n )
   s" hb-prog" ;

: ENGINE$ ( -- ptr u8 n )
   s" hb" ;

;package
