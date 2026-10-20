\ The rooted and ordinary variants of the suite's initial compiler build.
\ PROVIDE copies both into a consumer's private tree. The suite publishes the
\ complete bundle before dependent rows; this module never builds a compiler.
require src/lib/errors.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process-env.f
require test/compiler-images.f

package C2-IMAGE-PAIR

75 constant PAIR-RC

create SOURCE FS-PATH-CAP allot
create DEST FS-PATH-CAP allot
create FROM FS-PATH-CAP allot
create TO FS-PATH-CAP allot
variable SOURCE-U
variable DEST-U

: SOURCE$ ( -- ptr u8 n ) SOURCE SOURCE-U @ ;
: DEST$ ( -- ptr u8 n ) DEST DEST-U @ ;

: BUNDLE$ ( -- ptr u8 n )
   s" HABU_GATE_COMPILER_IMAGES" >LEN PROC-ENV-DEFAULT$? if
      LEN>N
   else
      2drop COMPILER-IMAGES:ROOT$
   then
   dup 0= if
      2drop
      s" c2-image-pair: HABU_GATE_COMPILER_IMAGES is not set; initialize the suite compiler bundle before running this consumer" PAIR-RC die
   then ;

: SOURCE-AT ( ptr u8 n -- ptr u8 n )
   SOURCE$ 2swap FROM JOIN-PATH FROM swap ;

: DEST-AT ( ptr u8 n -- ptr u8 n )
   DEST$ 2swap TO JOIN-PATH TO swap ;

: NEED-IMAGE ( ptr u8 n -- )
   SOURCE-AT 2dup EXECUTABLE? if 2drop exit then
   s" c2-image-pair: missing or nonexecutable compiler image: " type type cr
   s" c2-image-pair: invalid HABU_GATE_COMPILER_IMAGES bundle" PAIR-RC die ;

: NEED-FILE ( ptr u8 n -- )
   SOURCE-AT 2dup FILE? if 2drop exit then
   s" c2-image-pair: missing compiler artifact: " type type cr
   s" c2-image-pair: invalid HABU_GATE_COMPILER_IMAGES bundle" PAIR-RC die ;

: COPY ( ptr u8 n ptr u8 n -- )
   {: source:ptr sourceu:n dest:ptr destu:n :}
   source sourceu SOURCE-AT dest destu DEST-AT COPY-FILE-STREAM ;

public

\ The initializer owns the bundle. Consumers keep real private products and
\ the same capture's names beside each variant for replay and diagnostics.
: PROVIDE ( ptr u8 n -- )
   {: dest:ptr destu:n :}
   destu 0 <= if E-FS-PATH throw then
   destu FS-PATH-CAP > if E-FS-CAPACITY throw then
   dest DEST destu BYTE-COPY destu DEST-U !
   BUNDLE$ {: source:ptr sourceu:n :}
   sourceu FS-PATH-CAP > if E-FS-CAPACITY throw then
   source SOURCE sourceu BYTE-COPY sourceu SOURCE-U !
   s" hb-root" NEED-IMAGE
   s" hb-ordinary" NEED-FILE
   s" hb-root.map" NEED-FILE
   s" hb-ordinary.map" NEED-FILE
   s" hb-root" s" hb-root" COPY
   s" hb-ordinary" s" hb-ordinary" COPY
   s" hb-root.map" s" hb-root.map" COPY
   s" hb-ordinary.map" s" hb-ordinary.map" COPY
   s" hb-root" DEST-AT CHMOD-X
   s" hb-ordinary" DEST-AT CHMOD-X ;

;package
