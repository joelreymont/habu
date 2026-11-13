\ font-native.f - native HarfBuzz FONT test on each platform's installed fonts:
\ on Linux, DejaVu Sans, URW's Nimbus Roman and Noto Color Emoji, each found
\ by its file name under /usr/share/fonts.
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

: FONT-ROOT$ ( -- ptr u8 n ) s" /usr/share/fonts" ;

\ The walk joins every path within PATH-CAP bytes, so FOUND holds any it gives.
PATH-CAP BUFFER: FOUND
variable FOUND-U
PTR-VARIABLE WANT-P
variable WANT-U

: FOUND$ ( -- ptr u8 n ) FOUND FOUND-U @ ;
: WANT$ ( -- ptr u8 n ) WANT-P @ WANT-U @ ;

\ A walk callback has no context, so the file name is read from WANT$.
: KEEP-MATCH ( ptr u8 n -- )
   {: a:ptr u:n :}
   a u FS:BASENAME WANT$ STR= 0= if exit then
   a FOUND u BYTE-COPY u FOUND-U ! ;

\ The path is valid until the next FIND-FONT. A missing FONT-ROOT$ is a file
\ not found: a case labelled with the file's name fails, the path is empty.
: FIND-FONT ( ptr u8 n -- ptr u8 n )
   {: name:ptr len:n :}
   name WANT-P ! len WANT-U ! 0 FOUND-U !
   FONT-ROOT$ FS:DIR? if
      FONT-ROOT$ [: KEEP-MATCH ;] FS:WALK-FILES
   then
   FOUND-U @ 0= if
      name len T-LABEL false T-ASSERT T-LABEL-CLEAR
   then
   FOUND$ ;

: FONT-PATH$ ( -- ptr u8 n )
   HB-TARGET-MACOS? if
      s" /System/Library/Fonts/Supplemental/Arial.ttf" exit
   then
   s" DejaVuSans.ttf" FIND-FONT ;

: LIGATURE-PATH$ ( -- ptr u8 n )
   HB-TARGET-MACOS? if
      s" /System/Library/Fonts/Supplemental/Baskerville.ttc" exit
   then
   s" DejaVuSans.ttf" FIND-FONT ;

: CFF-PATH$ ( -- ptr u8 n )
   HB-TARGET-MACOS? if
      s" /System/Library/Fonts/Supplemental/STIXGeneral.otf" exit
   then
   s" NimbusRoman-Regular.otf" FIND-FONT ;

: COLOR-PATH$ ( -- ptr u8 n )
   HB-TARGET-MACOS? if
      s" /System/Library/Fonts/Apple Color Emoji.ttc" exit
   then
   s" NotoColorEmoji.ttf" FIND-FONT ;

\ The accents' binary32 ink left, top and right edges from HarfBuzz, the top
\ past the em as the accents stack, then the viewBox that rounds them outward.
: ACCENT-BOX ( -- r r r ptr u8 n )
   HB-TARGET-MACOS? if
      -1.4648 -1581.5273 668.457
      s\" viewBox=\"-2 -1582 671 1582\"" exit
   then
   7.8126 -1281.2343 675.7812 s\" viewBox=\"7 -1282 669 1282\"" ;

\ HarfBuzz's ink box of `o` ends at -9 in Arial at 17, at -14 in DejaVu Sans
\ at 25; the emitted path reaches -9.0146 or -14.0015, and the viewBox holds it.
: ROUND-CASE ( -- n ptr u8 n )
   HB-TARGET-MACOS? if 17 s\" viewBox=\"0 -10 9 11\"" exit then
   25 s\" viewBox=\"1 -15 13 16\"" ;

: READ-FONT ( ptr u8 n -- ptr u8 n )
   {: path:ptr len:n :}
   \ An empty path is a font FIND-FONT did not find: its case has failed.
   len 0= if path 0 exit then
   path len FS:FILE-SIZE {: size:n :}
   size MEM-ALLOC-BYTES {: data:ptr cap:n :}
   path len data cap FS:READ-ALL cap T=
   data cap ;

: INPUT-FONT ( -- ptr u8 n ) FONT-PATH$ READ-FONT ;

: PATH-COUNT ( ptr u8 n -- n ) {: svg:ptr size:n :}
   size 5 < if 0 exit then
   0 size 4 - 0 ?do
      svg i + 5 s" <path" STR= if 1+ then
   loop ;

: DRAW-CHECK ( FONT:font -- )
   {: face:FONT:font :}
   face s\" A\xCC\x81\xCC\x81\xCC\x81\xCC\x81\xCC\x81" 1000 FONT:RENDER
      {: accents:FONT:image :}
   accents FONT:BOUNDS {: x:r y:r w:r h:r :}
   ACCENT-BOX {: left:r top:r right:r box:ptr box-len:n :}
   x left f< T-ASSERT
   y top f< T-ASSERT
   x w f+ right f> T-ASSERT
   y h f+ 0.0 f= T-ASSERT
   accents FONT:SVG$ s" viewBox='" CONTAINS? 0= T-ASSERT
   accents FONT:SVG$ box box-len CONTAINS? TTRUE
   accents FONT:FREE

   ROUND-CASE {: em:n view:ptr view-len:n :}
   face s" o" em FONT:RENDER {: round:FONT:image :}
   round FONT:SVG$ view view-len CONTAINS? TTRUE
   round FONT:FREE

   face s\" \xD0\x94" 400 FONT:RENDER {: greek:FONT:image :}
   greek FONT:SVG$ s" <path" CONTAINS? TTRUE
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
   LIGATURE-PATH$ READ-FONT {: data:ptr size:n :}
   size 0= if exit then
   data size 0 FONT:OPEN {: face:FONT:font :}
   data size MEM:BYTES-ALLOC-LEN MEM:RELEASE-BYTES
   face s" fi" 1000 FONT:RENDER {: ligature:FONT:image :}
   ligature FONT:SVG$ PATH-COUNT 1 T=
   ligature FONT:FREE face FONT:CLOSE ;

: CFF-BOUNDS-CHECK ( -- )
   CFF-PATH$ READ-FONT {: data:ptr size:n :}
   size 0= if exit then
   data size 0 FONT:OPEN {: face:FONT:font :}
   data size MEM:BYTES-ALLOC-LEN MEM:RELEASE-BYTES
   face s" j" 23 FONT:RENDER {: img:FONT:image :}
   \ Both platforms' faces give this box; Nimbus Roman's ink box of `j` ends at 5,
   \ and its emitted path passes it.
   img FONT:SVG$ s\" viewBox=\"-2 -16 7 22\"" CONTAINS? TTRUE
   img FONT:FREE face FONT:CLOSE ;

: COLOR-ONLY ( -- )
   COLOR-FACE @ s\" \xF0\x9F\x94\xA9" 1000 FONT:RENDER FONT:FREE ;

: COLOR-CHECK ( -- )
   COLOR-PATH$ READ-FONT {: data:ptr size:n :}
   size 0= if exit then
   data size 0 FONT:OPEN {: face:FONT:font :}
   data size MEM:BYTES-ALLOC-LEN MEM:RELEASE-BYTES
   face COLOR-FACE !
   [: COLOR-ONLY ;] FONT:E-GLYPH TTHROWSQ
   face FONT:CLOSE ;

: MISSING ( -- )
   FACE @ s\" \xE4\xB8\xAD" 1000 FONT:RENDER FONT:FREE ;

\ One glyph at the largest em: its ink passes 2^24 units.
: TOO-LARGE ( -- )
   FACE @ s" B" $1FFFFFF FONT:RENDER FONT:FREE ;

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
   size 0= if T-REPORT exit then
   data size 0 FONT:OPEN {: face:FONT:font :}
   data size MEM:BYTES-ALLOC-LEN MEM:RELEASE-BYTES
   face FACE !
   face DRAW-CHECK
   LIGATURE-CHECK
   CFF-BOUNDS-CHECK
   COLOR-CHECK
   [: MISSING ;] FONT:E-GLYPH TTHROWSQ
   FONT:E-GLYPH ERROR:NAME$ s" FONT:E-GLYPH" T$=
   FONT:E-GLYPH ERROR:TEXT$ nip 0<> TTRUE
   [: TOO-LARGE ;] FONT:E-EXTENT TTHROWSQ
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
