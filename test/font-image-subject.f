\ font-image-subject.f - live FONT handles are retired before image capture.
\ Build: bin/hb --load tools/hb-build.f -- --repl test/font-image-subject.f -o build/font-image
\ Run: build/font-image
require src/lib/font.f
require src/lib/test.f
require src/lib/fs.f
require src/lib/memory.f

package FONT-IMAGE-SUBJECT
private

TYPED-VARIABLE OLD-FONT FONT:font
TYPED-VARIABLE OLD-IMAGE FONT:image

: FONT-PATH$ ( -- ptr u8 n )
   s" /System/Library/Fonts/Supplemental/Arial.ttf" ;

: LOAD-FONT ( -- FONT:font )
   FONT-PATH$ FILE-SIZE {: size:n :}
   size MEM-ALLOC-BYTES {: data:ptr cap:n :}
   FONT-PATH$ data cap READ-ALL cap T=
   data cap 0 FONT:OPEN {: face:FONT:font :}
   data cap MEM:BYTES-ALLOC-LEN MEM:RELEASE-BYTES
   face ;

: WARM ( -- )
   T-RESET
   LOAD-FONT dup OLD-FONT !
   s" A" 1000 FONT:RENDER OLD-IMAGE !
   T-REPORT ;

: STALE-FONT ( -- ) OLD-FONT @ FONT:CLOSE ;
: STALE-IMAGE ( -- ) OLD-IMAGE @ FONT:SVG$ 2drop ;

public

: RUN ( -- )
   T-RESET
   [: STALE-FONT ;] FONT:E-HANDLE TTHROWSQ
   [: STALE-IMAGE ;] FONT:E-HANDLE TTHROWSQ
   LOAD-FONT {: face:FONT:font :}
   face s" B" 1000 FONT:RENDER {: image:FONT:image :}
   image FONT:SVG$ {: svg:ptr bytes:n :}
   s" build/font-image.svg" svg bytes WRITE-ALL
   image FONT:FREE face FONT:CLOSE
   T-REPORT ;

WARM
;package

: MAIN ( -- ) FONT-IMAGE-SUBJECT:RUN ;
