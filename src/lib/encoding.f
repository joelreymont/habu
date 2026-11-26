\ encoding.f - package ENCODING: the WHATWG Encoding Standard's labels, BOM
\ sniff and decoders, in fatal mode, writing UTF-8. DECODE appends to a
\ vector without clearing it; the first decoder error throws E-DECODE
\ (UTF16:E-UTF16 for UTF-16) and leaves what was appended before it. Each
\ decoder but ISO-2022-JP's reads one whole sequence at a cursor and appends
\ it or throws, so a sequence the input's end cuts off is an error. The
\ indexes (src/lib/encoding/data.f) hold no surrogate, 0 or scalar past
\ U+10FFFF, so the output is valid UTF-8.
\ STORAGE CLASS. PROCESS-WIDE read-only tables; CALLER-OWNED output.

require src/lib/adt/option.f
require src/lib/encoding/data.f
require src/lib/encoding/fault.f
require src/lib/encoding/iso-2022-jp.f
require src/lib/encoding/japanese.f
require src/lib/utf16.f
require src/lib/utf8-scalar.f
require src/lib/vector.f

package ENCODING

private

: BYTE@ ( n str -- n )
   {: at:n s:str :}
   s STR:DATA at + c@ ;

\ The byte at a cursor past a sequence's lead; the input's end is E-DECODE.
: TRAIL@ ( n str -- n )
   {: at:n s:str :}
   at s STR:LEN >= if E-DECODE throw then
   at s BYTE@ ;

\ TRAIL@ less lo, or E-DECODE for a byte outside lo..hi.
: TRAIL-IN ( n str n n -- n )
   {: at:n s:str lo:n hi:n :}
   at s TRAIL@ {: b:n :}
   b lo < b hi > or if E-DECODE throw then
   b lo - ;

: IN? ( n n n -- bool )
   {: b:n lo:n hi:n :}
   b lo >= b hi <= and ;

: ASCII? ( n -- bool ) $80 < ;

\ An index's code point, where 0 is a pointer the index has none for.
: MAPPED ( n ^VEC:vector<u8> -- )
   {: cp:n v:^ :}
   cp 0= if E-DECODE throw then
   cp v UTF8:APPEND-SCALAR ;

\ Runs STEP from cursor 0 until it passes the input's end; STEP answers the
\ cursor after the sequence it appended.
: EACH ( str ^VEC:vector<u8> [ n str ^VEC:vector<u8> -- n ] -- )
   {: s:str v:^ step :}
   0 begin dup s STR:LEN < while s v step execute repeat
   drop ;

\ The input after a leading mark, or the whole input without one.
: UNMARKED ( str str -- str )
   {: s:str mark:str :}
   s mark STR:STARTS-WITH? 0= if s exit then
   s STR:DATA mark STR:LEN +  s STR:LEN mark STR:LEN -  >str ;

: UTF8-MARK ( -- str ) s\" \xEF\xBB\xBF" ;
: BE-MARK ( -- str ) s\" \xFE\xFF" ;
: LE-MARK ( -- str ) s\" \xFF\xFE" ;

: DECODE-UTF8 ( str ^VEC:vector<u8> -- )
   {: s:str v:^ :}
   s UTF8-MARK UNMARKED {: t:str :}
   t UTF8:VALID? 0= if E-DECODE throw then
   t v VEC:APPEND ;

: DECODE-UTF16BE ( str ^VEC:vector<u8> -- )
   {: s:str v:^ :}
   s BE-MARK UNMARKED v UTF16:APPEND-BE>UTF8 ;

: DECODE-UTF16LE ( str ^VEC:vector<u8> -- )
   {: s:str v:^ :}
   s LE-MARK UNMARKED v UTF16:APPEND-LE>UTF8 ;

\ TABLE answers the cell of a pointer, the byte less $80.
: SINGLE-BYTE ( n [ n -- ^n ] ^VEC:vector<u8> -- )
   {: b:n table v:^ :}
   b ASCII? if b v UTF8:APPEND-SCALAR exit then
   b $80 - table execute @ v MAPPED ;

: DECODE-SINGLE ( str ^VEC:vector<u8> [ n -- ^n ] -- )
   {: s:str v:^ table :}
   s STR:LEN 0 ?do i s BYTE@ table v SINGLE-BYTE loop ;

: USER-DEFINED ( n ^VEC:vector<u8> -- )
   {: b:n v:^ :}
   b ASCII? if b v UTF8:APPEND-SCALAR exit then
   b $80 - $F780 + v UTF8:APPEND-SCALAR ;

: DECODE-USER-DEFINED ( str ^VEC:vector<u8> -- )
   {: s:str v:^ :}
   s STR:LEN 0 ?do i s BYTE@ v USER-DEFINED loop ;

: DECODE-REPLACEMENT ( str ^VEC:vector<u8> -- )
   {: s:str v:^ :}
   s STR:LEN 0<> if E-DECODE throw then ;

\ The ranges are in pointer order and the first starts at pointer 0: the
\ last range at or below a pointer, by halving.
: RANGE-POINTER ( n -- n ) 2 * INDEX-GB18030-RANGES @ ;

: RANGE-AT ( n -- n )
   {: p:n :}
   0 GB18030-RANGE-COUNT
   begin over 1+ over < while
      2dup + 2 / dup RANGE-POINTER p > if nip else rot drop swap then
   repeat
   drop ;

\ The Standard's "index gb18030 ranges code point".
: RANGES-SCALAR ( n -- n )
   {: p:n :}
   p 39419 > p 189000 < and p 1237575 > or if E-DECODE throw then
   p 7457 = if $E7C7 exit then
   p RANGE-AT {: r:n :}
   r 2 * 1+ INDEX-GB18030-RANGES @  p +  r RANGE-POINTER - ;

\ Four bytes from a lead at the cursor whose second byte is a digit.
: GB18030-FOUR ( n str ^VEC:vector<u8> -- n )
   {: at:n s:str v:^ :}
   at s BYTE@ $81 - 10 *
   at 1+ s $30 $39 TRAIL-IN + 126 *
   at 2 + s $81 $FE TRAIL-IN + 10 *
   at 3 + s $30 $39 TRAIL-IN +
   RANGES-SCALAR v UTF8:APPEND-SCALAR
   at 4 + ;

: GB18030-POINTER ( n n -- n )
   {: lead:n b:n :}
   b $40 $7E IN? b $80 $FE IN? or 0= if E-DECODE throw then
   lead $81 - 190 *  b +  b $7F < if $40 else $41 then - ;

: GB18030-STEP ( n str ^VEC:vector<u8> -- n )
   {: at:n s:str v:^ :}
   at s BYTE@ {: b:n :}
   b ASCII? if b v UTF8:APPEND-SCALAR at 1+ exit then
   b $80 = if $20AC v UTF8:APPEND-SCALAR at 1+ exit then
   b $FF = if E-DECODE throw then
   at 1+ s TRAIL@ $30 $39 IN? if at s v GB18030-FOUR exit then
   b at 1+ s BYTE@ GB18030-POINTER INDEX-GB18030 @ v MAPPED
   at 2 + ;

: BIG5-POINTER ( n n -- n )
   {: lead:n b:n :}
   b $40 $7E IN? b $A1 $FE IN? or 0= if E-DECODE throw then
   lead $81 - 157 *  b +  b $7F < if $40 else $62 then - ;

: TWO ( n n ^VEC:vector<u8> -- )
   {: first:n second:n v:^ :}
   first v UTF8:APPEND-SCALAR  second v UTF8:APPEND-SCALAR ;

\ Four pointers give a letter and a combining mark.
: BIG5-SCALARS ( n ^VEC:vector<u8> -- )
   {: p:n v:^ :}
   p 1133 = if $CA $304 v TWO exit then
   p 1135 = if $CA $30C v TWO exit then
   p 1164 = if $EA $304 v TWO exit then
   p 1166 = if $EA $30C v TWO exit then
   p INDEX-BIG5 @ v MAPPED ;

: BIG5-STEP ( n str ^VEC:vector<u8> -- n )
   {: at:n s:str v:^ :}
   at s BYTE@ {: b:n :}
   b ASCII? if b v UTF8:APPEND-SCALAR at 1+ exit then
   b $81 $FE IN? 0= if E-DECODE throw then
   b at 1+ s TRAIL@ BIG5-POINTER v BIG5-SCALARS
   at 2 + ;

: EUC-KR-POINTER ( n n -- n )
   {: lead:n b:n :}
   b $41 $FE IN? 0= if E-DECODE throw then
   lead $81 - 190 *  b $41 - + ;

: EUC-KR-STEP ( n str ^VEC:vector<u8> -- n )
   {: at:n s:str v:^ :}
   at s BYTE@ {: b:n :}
   b ASCII? if b v UTF8:APPEND-SCALAR at 1+ exit then
   b $81 $FE IN? 0= if E-DECODE throw then
   b at 1+ s TRAIL@ EUC-KR-POINTER INDEX-EUC-KR @ v MAPPED
   at 2 + ;

\ ASCII whitespace: TAB, LF, FF, CR and SPACE.
: WHITE? ( n -- bool )
   {: c:n :}
   c 9 = c 10 = or c 12 = or c 13 = or c 32 = or ;

: WHITE-AT? ( n str -- bool )
   {: at:n s:str :}
   at s STR:LEN >= if false exit then
   at s BYTE@ WHITE? ;

: TRIM-WHITE ( str -- str )
   {: s:str :}
   0 begin dup s WHITE-AT? while 1+ repeat {: start:n :}
   s STR:LEN begin dup start > if dup 1- s WHITE-AT? else false then
   while 1- repeat {: end:n :}
   s STR:DATA start +  end start -  >str ;

: LABEL-ENCODING ( n -- encoding )
   LABEL-AT {: name:str e:encoding :} e ;

public

\ The Standard's "get an encoding": the label with ASCII whitespace trimmed
\ from both ends, matched ASCII case-insensitively.
: LABEL ( str -- option<ENCODING:encoding> )
   TRIM-WHITE {: t:str :}
   LABEL-COUNT 0 ?do
      i LABEL-AT drop t STR:=CI if
         i LABEL-ENCODING OPTION:SOME unloop exit
      then
   loop
   OPTION:NONE ;

\ The Standard's "BOM sniff".
: BOM ( str -- option<ENCODING:encoding> )
   {: s:str :}
   s UTF8-MARK STR:STARTS-WITH? if ENCODING-ENCODING:UTF-8 OPTION:SOME exit then
   s BE-MARK STR:STARTS-WITH? if ENCODING-ENCODING:UTF-16BE OPTION:SOME exit then
   s LE-MARK STR:STARTS-WITH? if ENCODING-ENCODING:UTF-16LE OPTION:SOME exit then
   OPTION:NONE ;

\ One leading byte-order mark of UTF-8, UTF-16BE or UTF-16LE is dropped, as
\ TextDecoder does by default; another encoding's mark is text.
: DECODE ( str ENCODING:encoding ^VEC:vector<u8> -- )
   {: s:str e:encoding v:^ :}
   s v e MATCH encoding
      utf-8 OF DECODE-UTF8 ENDOF
      ibm866 OF [: INDEX-IBM866 ;] DECODE-SINGLE ENDOF
      iso-8859-2 OF [: INDEX-ISO-8859-2 ;] DECODE-SINGLE ENDOF
      iso-8859-3 OF [: INDEX-ISO-8859-3 ;] DECODE-SINGLE ENDOF
      iso-8859-4 OF [: INDEX-ISO-8859-4 ;] DECODE-SINGLE ENDOF
      iso-8859-5 OF [: INDEX-ISO-8859-5 ;] DECODE-SINGLE ENDOF
      iso-8859-6 OF [: INDEX-ISO-8859-6 ;] DECODE-SINGLE ENDOF
      iso-8859-7 OF [: INDEX-ISO-8859-7 ;] DECODE-SINGLE ENDOF
      iso-8859-8 OF [: INDEX-ISO-8859-8 ;] DECODE-SINGLE ENDOF
      iso-8859-8-i OF [: INDEX-ISO-8859-8 ;] DECODE-SINGLE ENDOF
      iso-8859-10 OF [: INDEX-ISO-8859-10 ;] DECODE-SINGLE ENDOF
      iso-8859-13 OF [: INDEX-ISO-8859-13 ;] DECODE-SINGLE ENDOF
      iso-8859-14 OF [: INDEX-ISO-8859-14 ;] DECODE-SINGLE ENDOF
      iso-8859-15 OF [: INDEX-ISO-8859-15 ;] DECODE-SINGLE ENDOF
      iso-8859-16 OF [: INDEX-ISO-8859-16 ;] DECODE-SINGLE ENDOF
      koi8-r OF [: INDEX-KOI8-R ;] DECODE-SINGLE ENDOF
      koi8-u OF [: INDEX-KOI8-U ;] DECODE-SINGLE ENDOF
      macintosh OF [: INDEX-MACINTOSH ;] DECODE-SINGLE ENDOF
      windows-874 OF [: INDEX-WINDOWS-874 ;] DECODE-SINGLE ENDOF
      windows-1250 OF [: INDEX-WINDOWS-1250 ;] DECODE-SINGLE ENDOF
      windows-1251 OF [: INDEX-WINDOWS-1251 ;] DECODE-SINGLE ENDOF
      windows-1252 OF [: INDEX-WINDOWS-1252 ;] DECODE-SINGLE ENDOF
      windows-1253 OF [: INDEX-WINDOWS-1253 ;] DECODE-SINGLE ENDOF
      windows-1254 OF [: INDEX-WINDOWS-1254 ;] DECODE-SINGLE ENDOF
      windows-1255 OF [: INDEX-WINDOWS-1255 ;] DECODE-SINGLE ENDOF
      windows-1256 OF [: INDEX-WINDOWS-1256 ;] DECODE-SINGLE ENDOF
      windows-1257 OF [: INDEX-WINDOWS-1257 ;] DECODE-SINGLE ENDOF
      windows-1258 OF [: INDEX-WINDOWS-1258 ;] DECODE-SINGLE ENDOF
      x-mac-cyrillic OF [: INDEX-X-MAC-CYRILLIC ;] DECODE-SINGLE ENDOF
      gbk OF [: GB18030-STEP ;] EACH ENDOF
      gb18030 OF [: GB18030-STEP ;] EACH ENDOF
      big5 OF [: BIG5-STEP ;] EACH ENDOF
      euc-jp OF DECODE-EUC-JP ENDOF
      iso-2022-jp OF DECODE-ISO-2022-JP ENDOF
      shift-jis OF DECODE-SHIFT-JIS ENDOF
      euc-kr OF [: EUC-KR-STEP ;] EACH ENDOF
      replacement OF DECODE-REPLACEMENT ENDOF
      utf-16be OF DECODE-UTF16BE ENDOF
      utf-16le OF DECODE-UTF16LE ENDOF
      x-user-defined OF DECODE-USER-DEFINED ENDOF
   ;MATCH ;

;package
