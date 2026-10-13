\ font-native.f - native HarfBuzz FONT test on installed macOS fonts.
\ Run: bin/hb --load test/font-native.f
require src/lib/font.f
require src/lib/test.f
require src/lib/fs.f
require src/lib/memory.f
require src/lib/string.f

package FONT-NATIVE-TEST
private

TYPED-VARIABLE FACE FONT:font
TYPED-VARIABLE COLOR-FACE FONT:font
TYPED-VARIABLE IMAGE FONT:image

: FONT-PATH$ ( -- ptr u8 n )
   s" /System/Library/Fonts/Supplemental/Arial.ttf" ;

: READ-FONT ( ptr u8 n -- ptr u8 n )
   {: path:ptr len:n :}
   path len FILE-SIZE {: size:n :}
   size MEM-ALLOC-BYTES {: data:ptr cap:n :}
   path len data cap READ-ALL cap T=
   data cap ;

: INPUT-FONT ( -- ptr u8 n ) FONT-PATH$ READ-FONT ;

: PATH-COUNT ( ptr u8 n -- n ) {: svg:ptr size:n :}
   size 5 < if 0 exit then
   0 size 4 - 0 ?do
      svg i + 5 s" <path" STR= if 1+ then
   loop ;

: WRITE-IMAGE ( FONT:image ptr u8 n -- )
   {: image:FONT:image path:ptr len:n :}
   image FONT:SVG$ {: svg:ptr bytes:n :}
   path len svg bytes WRITE-ALL ;

: DRAW-CHECK ( FONT:font -- )
   {: face:FONT:font :}
   face s\" A\xCC\x81\xCC\x81\xCC\x81\xCC\x81\xCC\x81" 1000 FONT:RENDER
      {: accents:FONT:image :}
   accents FONT:BOUNDS {: x:r y:r w:r h:r :}
   x 0.0 f< T-ASSERT
   y -1500.0 f< T-ASSERT
   h 1500.0 f> T-ASSERT
   w 600.0 f> T-ASSERT
   x -1.4648 f< T-ASSERT
   y -1581.5273 f< T-ASSERT
   x w f+ 668.457 f> T-ASSERT
   y h f+ 0.0 f= T-ASSERT
   accents FONT:SVG$ s" viewBox='" CONTAINS? 0= T-ASSERT
   accents FONT:SVG$ s\" viewBox=\"-2 -1582 671 1582\"" CONTAINS? TTRUE
   accents s" build/font-accents.svg" WRITE-IMAGE
   accents FONT:FREE

   \ HarfBuzz's ink box ends at -9; its emitted `o` path reaches -9.0146.
   face s" o" 17 FONT:RENDER {: round:FONT:image :}
   round FONT:SVG$ s\" viewBox=\"0 -10 9 11\"" CONTAINS? TTRUE
   round FONT:FREE

   face s\" \xD0\x94" 400 FONT:RENDER {: greek:FONT:image :}
   greek FONT:SVG$ s" <path" CONTAINS? TTRUE
   greek s" build/font-greek.svg" WRITE-IMAGE
   greek FONT:FREE

   face s"  " 1000 FONT:RENDER {: spaces:FONT:image :}
   spaces FONT:BOUNDS {: sx:r sy:r sw:r sh:r :}
   sx 0.0 f= sy 0.0 f= and sw 0.0 f= and sh 0.0 f= and T-ASSERT
   spaces FONT:SVG$ s" width='0' height='0'" CONTAINS? TTRUE
   spaces FONT:FREE

   face s" A" 250 FONT:RENDER {: small:FONT:image :}
   small FONT:BOUNDS {: ax:r ay:r aw:r ah:r :}
   aw 100.0 f> ah 100.0 f> and T-ASSERT
   small IMAGE !
   small FONT:FREE ;

: LIGATURE-CHECK ( -- )
   s" /System/Library/Fonts/Supplemental/Baskerville.ttc"
      READ-FONT {: data:ptr size:n :}
   data size 0 FONT:OPEN {: face:FONT:font :}
   data size MEM:BYTES-ALLOC-LEN MEM:RELEASE-BYTES
   face s" fi" 1000 FONT:RENDER {: ligature:FONT:image :}
   ligature FONT:SVG$ PATH-COUNT 1 T=
   ligature s" build/font-ligature.svg" WRITE-IMAGE
   ligature FONT:FREE face FONT:CLOSE ;

: CFF-BOUNDS-CHECK ( -- )
   s" /System/Library/Fonts/Supplemental/STIXGeneral.otf"
      READ-FONT {: data:ptr size:n :}
   data size 0 FONT:OPEN {: face:FONT:font :}
   data size MEM:BYTES-ALLOC-LEN MEM:RELEASE-BYTES
   face s" j" 23 FONT:RENDER {: img:FONT:image :}
   img FONT:SVG$ s\" viewBox=\"-2 -16 7 22\"" CONTAINS? TTRUE
   img FONT:FREE face FONT:CLOSE ;

: COLOR-ONLY ( -- )
   COLOR-FACE @ s\" \xF0\x9F\x94\xA9" 1000 FONT:RENDER FONT:FREE ;

: COLOR-CHECK ( -- )
   s" /System/Library/Fonts/Apple Color Emoji.ttc"
      READ-FONT {: data:ptr size:n :}
   data size 0 FONT:OPEN {: face:FONT:font :}
   data size MEM:BYTES-ALLOC-LEN MEM:RELEASE-BYTES
   face COLOR-FACE !
   [: COLOR-ONLY ;] FONT:E-GLYPH TTHROWSQ
   face FONT:CLOSE ;

: MISSING ( -- )
   FACE @ s\" \xE4\xB8\xAD" 1000 FONT:RENDER FONT:FREE ;

: BAD-UTF8 ( -- )
   FACE @ s\" \xFF" 1000 FONT:RENDER FONT:FREE ;

: MIXED-SCRIPT ( -- )
   FACE @ s\" A\xCE\xA9" 1000 FONT:RENDER FONT:FREE ;

: RTL-TEXT ( -- )
   FACE @ s\" \xD7\x90" 1000 FONT:RENDER FONT:FREE ;

: BIDI-CONTROL ( -- )
   FACE @ s\" A\xE2\x80\xAE" 1000 FONT:RENDER FONT:FREE ;

: BAD-FONT ( -- )
   s" not a font" 0 FONT:OPEN FONT:CLOSE ;

: STALE-FONT ( -- ) FACE @ FONT:CLOSE ;
: STALE-IMAGE ( -- ) IMAGE @ FONT:SVG$ 2drop ;

: RUN ( -- )
   T-RESET
   INPUT-FONT {: data:ptr size:n :}
   data size 0 FONT:OPEN {: face:FONT:font :}
   data size MEM:BYTES-ALLOC-LEN MEM:RELEASE-BYTES
   face FACE !
   face DRAW-CHECK
   LIGATURE-CHECK
   CFF-BOUNDS-CHECK
   COLOR-CHECK
   [: MISSING ;] FONT:E-GLYPH TTHROWSQ
   [: BAD-UTF8 ;] FONT:E-TEXT TTHROWSQ
   [: MIXED-SCRIPT ;] FONT:E-TEXT TTHROWSQ
   [: RTL-TEXT ;] FONT:E-TEXT TTHROWSQ
   [: BIDI-CONTROL ;] FONT:E-TEXT TTHROWSQ
   [: BAD-FONT ;] FONT:E-OPEN TTHROWSQ
   [: STALE-IMAGE ;] FONT:E-HANDLE TTHROWSQ
   face s" B" 1000 FONT:RENDER FONT:FREE
   face FONT:CLOSE
   [: STALE-FONT ;] FONT:E-HANDLE TTHROWSQ
   T-REPORT ;

RUN
;package
