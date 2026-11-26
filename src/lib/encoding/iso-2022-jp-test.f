\ iso-2022-jp-test.f - the ISO-2022-JP decoder of
\ src/lib/encoding/iso-2022-jp.f, in fatal mode.
\ Run: bin/hb --load src/lib/encoding/iso-2022-jp-test.f
\ Expected bytes are written from the Standard and the pinned
\ data/encoding/index-jis0208.txt, not computed. ESC ( B selects ASCII,
\ ESC ( J Roman, ESC ( I katakana, ESC $ @ and ESC $ B jis0208.

require src/lib/encoding/fault.f
require src/lib/encoding/iso-2022-jp.f
require src/lib/heap.f
require src/lib/test/assert.f
require src/lib/vector.f

\ Reopens the package so the cases reach its private decoder.
package ENCODING

\ The output, NULL-PTR while it is not made.
TYPED-VARIABLE IJT-OUT ^VEC:vector<u8>

: IJT= ( str str -- )
   {: in:str want:str :}
   IJT-OUT @ 0 VEC:RESIZE
   in IJT-OUT @ DECODE-ISO-2022-JP
   IJT-OUT @ VEC:BYTES want T$= ;

\ The decoder throws E-DECODE, leaving the bytes appended before.
: IJT-REJECTS ( str str -- )
   {: in:str want:str :}
   IJT-OUT @ 0 VEC:RESIZE
   [: in IJT-OUT @ DECODE-ISO-2022-JP ;] E-DECODE TTHROWSQ
   IJT-OUT @ VEC:BYTES want T$= ;

: IJT-ESCAPES ( -- )
   s" " s" " IJT=
   s\" a\\~" s\" a\\~" IJT=                                \ ASCII from the start
   s\" \x1B(Ba" s" a" IJT=
   s\" \x1B(J\\~a" s\" \xC2\xA5\xE2\x80\xBEa" IJT=          \ Roman U+00A5 U+203E
   s\" \x1B(I\x31\x5F" s\" \xEF\xBD\xB1\xEF\xBE\x9F" IJT=   \ katakana U+FF71 U+FF9F
   s\" \x1B$@\x46\x7C" s\" \xE6\x97\xA5" IJT=              \ jis0208 3569, U+65E5
   s\" \x1B$B\x21\x21\x46\x7C" s\" \xE3\x80\x80\xE6\x97\xA5" IJT=
   s\" \x1B$B\x46\x7C\x1B(Ba" s\" \xE6\x97\xA5a" IJT=
   s\" \x1B(Ja\x1B(B\\" s\" a\\" IJT=                     \ back to ASCII
   s\" \x1B$B" s" " IJT= ;                                \ the end in Lead byte

: IJT-DOUBLE-ESCAPES ( -- )
   s\" \x1B(B\x1B(J" s" " IJT-REJECTS
   s\" a\x1B(J\x1B(Ba" s" a" IJT-REJECTS
   s\" \x1B$B\x46\x7C\x1B(I\x1B$@" s\" \xE6\x97\xA5" IJT-REJECTS ;

\ A bad byte in each state.
: IJT-BAD-BYTES ( -- )
   s\" a\x0E" s" a" IJT-REJECTS                            \ ASCII
   s\" a\x80" s" a" IJT-REJECTS
   s\" \x1B(Ja\x0F" s" a" IJT-REJECTS                      \ Roman
   s\" \x1B(Ja\x80" s" a" IJT-REJECTS
   s\" \x1B(I\x31\x60" s\" \xEF\xBD\xB1" IJT-REJECTS        \ katakana
   s\" \x1B(I\x20" s" " IJT-REJECTS
   s\" \x1B$B\x46\x7C\x20" s\" \xE6\x97\xA5" IJT-REJECTS    \ Lead byte
   s\" \x1B$B\x46\x7C\x80" s\" \xE6\x97\xA5" IJT-REJECTS
   s\" \x1B$B\x7F" s" " IJT-REJECTS
   s\" \x1B$B\x46\x7C\x46\x80" s\" \xE6\x97\xA5" IJT-REJECTS \ Trail byte
   s\" \x1B$B\x46\x1B(Ba" s" " IJT-REJECTS
   s\" \x1B$B\x46\x7F" s" " IJT-REJECTS
   s\" \x1B$B\x46\x20" s" " IJT-REJECTS
   s\" \x1B$B\x29\x21" s" " IJT-REJECTS                    \ jis0208 752 is empty
   s\" a\x1BA" s" a" IJT-REJECTS                           \ Escape start
   s\" a\x1B(A" s" a" IJT-REJECTS                          \ Escape
   s\" a\x1B$(" s" a" IJT-REJECTS ;

\ The input ending in each state that cuts a sequence off.
: IJT-CUT-OFF ( -- )
   s\" \x1B$B\x46\x7C\x46" s\" \xE6\x97\xA5" IJT-REJECTS   \ Trail byte
   s\" a\x1B" s" a" IJT-REJECTS                            \ Escape start
   s\" a\x1B(" s" a" IJT-REJECTS                           \ Escape
   s\" a\x1B$" s" a" IJT-REJECTS ;

: IJT-MAIN ( -- )
   T-RESET
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW IJT-OUT !
   IJT-ESCAPES
   IJT-DOUBLE-ESCAPES
   IJT-BAD-BYTES
   IJT-CUT-OFF
   IJT-OUT @ VEC:DELETE  NULL-PTR IJT-OUT !
   T-REPORT
   s" iso-2022-jp-test: ok" type cr ;

IJT-MAIN

;package
