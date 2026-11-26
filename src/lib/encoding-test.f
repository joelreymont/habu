\ encoding-test.f - the Encoding Standard's labels, BOM sniff and decoders
\ through src/lib/encoding.f: the rules the index files do not hold. Expected
\ bytes are written from the Standard and the pinned indexes, not computed; a
\ four-byte gb18030 sequence names its pointer,
\ (((b1-$81)*10+b2-$30)*126+b3-$81)*10+b4-$30.
\ Run: bin/hb --load src/lib/encoding-test.f

require src/lib/adt/option.f
require src/lib/encoding.f
require src/lib/encoding/data.f
require src/lib/encoding/fault.f
require src/lib/heap.f
require src/lib/test/assert.f
require src/lib/utf16.f
require src/lib/vector.f

package ENCODING-TEST

\ The vector, NULL-PTR while it is not made.
TYPED-VARIABLE ENC-OUT ^VEC:vector<u8>

: RELEASE ( -- )
   ENC-OUT @ VEC:DELETE  NULL-PTR ENC-OUT ! ;

\ NULL-PTR goes in before NEW: a rejected NEW leaves no vector to delete twice.
: FRESH ( -- )
   RELEASE  HEAP:ALLOCATOR 0 [type] u8 VEC:NEW ENC-OUT ! ;

: GBK ( -- ENCODING:encoding ) ENCODING-ENCODING:GBK ;
: GB18030 ( -- ENCODING:encoding ) ENCODING-ENCODING:GB18030 ;
: BIG5 ( -- ENCODING:encoding ) ENCODING-ENCODING:BIG5 ;
: EUC-KR ( -- ENCODING:encoding ) ENCODING-ENCODING:EUC-KR ;
: EUC-JP ( -- ENCODING:encoding ) ENCODING-ENCODING:EUC-JP ;
: SHIFT-JIS ( -- ENCODING:encoding ) ENCODING-ENCODING:SHIFT-JIS ;
: UTF-8 ( -- ENCODING:encoding ) ENCODING-ENCODING:UTF-8 ;
: UTF-16BE ( -- ENCODING:encoding ) ENCODING-ENCODING:UTF-16BE ;
: UTF-16LE ( -- ENCODING:encoding ) ENCODING-ENCODING:UTF-16LE ;
: W-874 ( -- ENCODING:encoding ) ENCODING-ENCODING:WINDOWS-874 ;
: W-1252 ( -- ENCODING:encoding ) ENCODING-ENCODING:WINDOWS-1252 ;

: DECODES ( str ENCODING:encoding str -- )
   {: s:str e want:str :}
   ENC-OUT @ 0 VEC:RESIZE
   s e ENC-OUT @ ENCODING:DECODE
   ENC-OUT @ VEC:BYTES want T$= ;

\ The throw's code, then the bytes appended before it.
: REJECTS ( str ENCODING:encoding n str -- )
   {: s:str e code:n want:str :}
   ENC-OUT @ 0 VEC:RESIZE
   [: s e ENC-OUT @ ENCODING:DECODE ;] code TTHROWSQ
   ENC-OUT @ VEC:BYTES want T$= ;

: CUT ( str ENCODING:encoding str -- )
   {: s:str e want:str :}
   s e ENCODING:E-DECODE want REJECTS ;

: MULTI-BYTE-ASCII ( -- )
   s\" \x00a~\x7F" GBK s\" \x00a~\x7F" DECODES
   s\" \x00a~\x7F" GB18030 s\" \x00a~\x7F" DECODES
   s\" \x00a~\x7F" BIG5 s\" \x00a~\x7F" DECODES
   s\" \x00a~\x7F" EUC-KR s\" \x00a~\x7F" DECODES ;

: GB18030-RULES ( ENCODING:encoding -- )
   {: e :}
   s\" \x80" e s\" \xE2\x82\xAC" DECODES                \ U+20AC
   s\" \xB0\xA1" e s\" \xE5\x95\x8A" DECODES            \ pointer 9026, U+554A
   s\" \x81\x30\x81\x30" e s\" \xC2\x80" DECODES        \ 0, U+0080
   s\" \x84\x31\xA4\x39" e s\" \xEF\xBF\xBF" DECODES    \ 39419, U+FFFF
   s\" \x81\x35\xF4\x37" e s\" \xEE\x9F\x87" DECODES    \ 7457, U+E7C7
   s\" \x90\x30\x81\x30" e s\" \xF0\x90\x80\x80" DECODES \ 189000, U+10000
   s\" \xE3\x32\x9A\x35" e s\" \xF4\x8F\xBF\xBF" DECODES ; \ 1237575, U+10FFFF

\ Pointers 1133, 1135, 1164 and 1166.
: BIG5-PAIRS ( -- )
   s\" \x88\x62" BIG5 s\" \xC3\x8A\xCC\x84" DECODES     \ U+00CA U+0304
   s\" \x88\x64" BIG5 s\" \xC3\x8A\xCC\x8C" DECODES     \ U+00CA U+030C
   s\" \x88\xA3" BIG5 s\" \xC3\xAA\xCC\x84" DECODES     \ U+00EA U+0304
   s\" \x88\xA5" BIG5 s\" \xC3\xAA\xCC\x8C" DECODES ;   \ U+00EA U+030C

: EUC-JP-CASES ( -- )
   s" " EUC-JP s" " DECODES
   s" Ab~" EUC-JP s" Ab~" DECODES
   s\" a\x8E\xB1" EUC-JP s\" a\xEF\xBD\xB1" DECODES    \ katakana U+FF71
   s\" \x8F\xA2\xAF" EUC-JP s\" \xCB\x98" DECODES      \ jis0212 108, U+02D8
   s\" \xC6\xFC" EUC-JP s\" \xE6\x97\xA5" DECODES      \ jis0208 3569, U+65E5
   s\" \xA1\xA1x" EUC-JP s\" \xE3\x80\x80x" DECODES    \ jis0208 0, U+3000
   s\" a\xA9\xA1" EUC-JP s" a" CUT                     \ jis0208 752 is empty
   s\" a\x8F\xA1\xA1" EUC-JP s" a" CUT                 \ jis0212 0 is empty
   s\" a\xC6\x41" EUC-JP s" a" CUT                     \ an ASCII byte after a lead
   s\" a\xC6\xA0" EUC-JP s" a" CUT                     \ a trail outside $A1-$FE
   s\" a\xC6\xFF" EUC-JP s" a" CUT
   s\" a\x8E\xE0" EUC-JP s" a" CUT                     \ $8E then no katakana
   s\" a\x8E\xA0" EUC-JP s" a" CUT
   s\" a\x8F\x41\xA1" EUC-JP s" a" CUT                 \ $8F then an ASCII byte
   s\" a\x8F\xA2\xA0" EUC-JP s" a" CUT                 \ $8F, a lead, a bad trail
   s\" a\x8F\xA2\xFF" EUC-JP s" a" CUT
   s\" a\x80" EUC-JP s" a" CUT                         \ a byte that leads nothing
   s\" a\xFF" EUC-JP s" a" CUT
   s\" a\xC6" EUC-JP s" a" CUT                         \ cut off after a lead
   s\" a\x8E" EUC-JP s" a" CUT
   s\" a\x8F\xA2" EUC-JP s" a" CUT ;

: SHIFT-JIS-CASES ( -- )
   s" " SHIFT-JIS s" " DECODES
   s\" Ab\x80" SHIFT-JIS s\" Ab\xC2\x80" DECODES       \ $80 is U+0080
   s\" a\xB1" SHIFT-JIS s\" a\xEF\xBD\xB1" DECODES     \ katakana U+FF71
   s\" \xF0\x40" SHIFT-JIS s\" \xEE\x80\x80" DECODES   \ pointer 8836, U+E000
   s\" \xF9\xFC" SHIFT-JIS s\" \xEE\x9D\x97" DECODES   \ pointer 10715, U+E757
   s\" \x93\xFA" SHIFT-JIS s\" \xE6\x97\xA5" DECODES   \ jis0208 3569, U+65E5
   s\" \x81\x40x" SHIFT-JIS s\" \xE3\x80\x80x" DECODES \ jis0208 0, U+3000
   s\" a\x85\x40" SHIFT-JIS s" a" CUT                  \ jis0208 752 is empty
   s\" a\x93\x7F" SHIFT-JIS s" a" CUT                  \ a trail outside both ranges
   s\" a\x93\x3F" SHIFT-JIS s" a" CUT
   s\" a\x93\xFD" SHIFT-JIS s" a" CUT
   s\" a\xFC\xFF" SHIFT-JIS s" a" CUT
   s\" a\xA0" SHIFT-JIS s" a" CUT                      \ a byte that leads nothing
   s\" a\xFD" SHIFT-JIS s" a" CUT
   s\" a\x93" SHIFT-JIS s" a" CUT ;                    \ cut off after a lead

\ iso-8859-8-i reads iso-8859-8's index; windows-874 has no $DB.
: SINGLE-BYTE ( -- )
   s\" a\x80" W-874 s\" a\xE2\x82\xAC" DECODES
   s\" a\xDB" W-874 ENCODING:E-DECODE s" a" REJECTS
   s\" \xE0" ENCODING-ENCODING:ISO-8859-8-I s\" \xD7\x90" DECODES ;

: OTHERS ( -- )
   s\" a\x80\xFF" ENCODING-ENCODING:X-USER-DEFINED
   s\" a\xEF\x9E\x80\xEF\x9F\xBF" DECODES               \ U+F780, U+F7FF
   s" " ENCODING-ENCODING:REPLACEMENT s" " DECODES
   s" a" ENCODING-ENCODING:REPLACEMENT ENCODING:E-DECODE s" " REJECTS
   s\" a\xFF" UTF-8 ENCODING:E-DECODE s" " REJECTS ;

: UTF16-ERRORS ( -- )
   s\" a\x00b" UTF-16LE UTF16:E-UTF16 s" " REJECTS      \ odd length
   s\" a\x00\x3D\xD8" UTF-16LE UTF16:E-UTF16 s" a" REJECTS \ high at the end
   s\" \x00a\xDC\x00" UTF-16BE UTF16:E-UTF16 s" a" REJECTS ; \ low alone

\ One mark of the encoding itself goes; a second, another's, or one in another
\ encoding is text.
: MARKS ( -- )
   s\" \xEF\xBB\xBFa" UTF-8 s" a" DECODES
   s\" \xEF\xBB\xBF\xEF\xBB\xBF" UTF-8 s\" \xEF\xBB\xBF" DECODES
   s\" \xFE\xFF\x00a" UTF-16BE s" a" DECODES
   s\" \xFF\xFEa\x00" UTF-16LE s" a" DECODES
   s\" \xFF\xFE" UTF-16BE s\" \xEF\xBF\xBE" DECODES     \ U+FFFE
   s\" \xEF\xBB\xBF" W-1252 s\" \xC3\xAF\xC2\xBB\xC2\xBF" DECODES ;

\ A sequence the input's end cuts off, a lead before an ASCII byte that
\ makes no pointer, and a lead or a trail outside its range.
: CUTS ( -- )
   s\" a\x81" GB18030 s" a" CUT
   s\" a\x81\x30" GB18030 s" a" CUT
   s\" a\x81\x30\x81" GB18030 s" a" CUT
   s\" a\x88" BIG5 s" a" CUT
   s\" a\xB0" EUC-KR s" a" CUT
   s\" a\x81 b" GB18030 s" a" CUT
   s\" a\x88 b" BIG5 s" a" CUT
   s\" a\xB0 b" EUC-KR s" a" CUT
   s\" a\xFF" GB18030 s" a" CUT
   s\" a\x81\x7F" GB18030 s" a" CUT
   s\" a\x81\xFF" GB18030 s" a" CUT
   s\" a\x80" BIG5 s" a" CUT
   s\" a\xFF" BIG5 s" a" CUT
   s\" a\x88\x3F" BIG5 s" a" CUT
   s\" a\x88\x7F" BIG5 s" a" CUT
   s\" a\x88\xA0" BIG5 s" a" CUT
   s\" a\x88\xFF" BIG5 s" a" CUT
   s\" a\x80" EUC-KR s" a" CUT
   s\" a\xFF" EUC-KR s" a" CUT
   s\" a\xB0\x40" EUC-KR s" a" CUT
   s\" a\xB0\xFF" EUC-KR s" a" CUT ;

: SOME= ( option<ENCODING:encoding> ENCODING:encoding -- )
   {: want :}
   MATCH option
      none OF false TTRUE ENDOF
      some OF want ENCODING-ENCODING:EQ TTRUE ENDOF
   ;MATCH ;

: NONE? ( option<ENCODING:encoding> -- )
   MATCH option
      none OF true TTRUE ENDOF
      some OF drop false TTRUE ENDOF
   ;MATCH ;

\ TAB, LF, FF, CR and SPACE are trimmed; VT is not ASCII whitespace.
: LABELS ( -- )
   s\" \x09\x0A\x0C\x0D Latin1 \x0D\x0C\x0A\x09" ENCODING:LABEL W-1252 SOME=
   s" UTF-8" ENCODING:LABEL UTF-8 SOME=
   s" Big5-HKSCS" ENCODING:LABEL BIG5 SOME=
   s" utf-9" ENCODING:LABEL NONE?
   s\" \x0Butf-8" ENCODING:LABEL NONE?
   s" " ENCODING:LABEL NONE? ;

: SNIFFS ( -- )
   s\" \xEF\xBB\xBFx" ENCODING:BOM UTF-8 SOME=
   s\" \xFE\xFF" ENCODING:BOM UTF-16BE SOME=
   s\" \xFF\xFEx" ENCODING:BOM UTF-16LE SOME=
   s" ab" ENCODING:BOM NONE?
   s\" \xEF\xBB" ENCODING:BOM NONE? ;

: MAIN ( -- )
   T-RESET
   FRESH
   MULTI-BYTE-ASCII
   GB18030 GB18030-RULES
   GBK GB18030-RULES
   BIG5-PAIRS
   EUC-JP-CASES
   SHIFT-JIS-CASES
   SINGLE-BYTE
   OTHERS
   UTF16-ERRORS
   MARKS
   CUTS
   RELEASE
   LABELS
   SNIFFS
   T-REPORT
   s" encoding-test: ok" type cr ;

MAIN

;package
