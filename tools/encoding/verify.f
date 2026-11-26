\ verify.f - independent exhaustive verifier of the Encoding Standard's
\ decoders (src/lib/encoding.f) and their table (src/lib/encoding/data.f).
\ It reads data/encoding/'s index files with a parser of its own and
\ encodings.json with JSON:PARSE, and checks, through ENCODING:DECODE and
\ ENCODING:LABEL only: each byte $80-$FF of every single-byte encoding and
\ each byte pair of every multi-byte decoder's lead and trail ranges, which
\ give the code point or points of the pointer they form or, where the index
\ has none, throw E-DECODE; that some pair decoded each pointer an index
\ holds; gb18030's four-byte sequences at the edges of its ranges; every
\ label as written, upper-cased and with ASCII whitespace on both sides; and
\ data.f against a fresh regeneration, byte for byte. A four-byte sequence's
\ pointer is (((b1-$81)*10+b2-$30)*126+b3-$81)*10+b4-$30.

require src/lib/prelude.f
require src/lib/string.f
require src/lib/memory.f
require src/lib/fs.f
require src/lib/codegen.f
require src/lib/fmt.f
require src/lib/heap.f
require src/lib/vector.f
require src/lib/adt/option.f
require src/lib/json/value.f
require src/lib/encoding.f
require src/lib/encoding/data.f
require src/lib/encoding/fault.f
require tools/encoding/generate.f

package ENCODING-VERIFY

private

-7903 s" the Encoding Standard's pinned data disagrees with the decoders or their table"
   ERROR:FAULT E-MISMATCH

\ No decoder forms a two-byte pointer at or past this.
23940 constant POINTER-CAPACITY
512 constant RANGE-CAPACITY
10 constant LINE-FEED
9 constant TAB
35 constant HASH
95 constant UNDERSCORE
45 constant HYPHEN

\ The code point at each pointer of the index being checked; 0 for none.
DYNAMIC-BUFFER TRUTH n
\ 1 where a pass decoded the pointer's code point.
DYNAMIC-BUFFER REACHED n
\ Each gb18030 range's first pointer, then its code point.
DYNAMIC-BUFFER RANGES n
variable RANGE-N
\ A pass's counts: pointers decoded, empty pairs rejected.
variable POINTS
variable EMPTIES
TYPED-VARIABLE ENC ENCODING:encoding
TYPED-VARIABLE IN-VEC ^VEC:vector<u8>
TYPED-VARIABLE WANT-VEC ^VEC:vector<u8>
TYPED-VARIABLE OUT-VEC ^VEC:vector<u8>

variable LABEL-TOTAL
variable ENCODING-TOTAL
\ What VARIANT's evaluated text stored through VARIANT!.
TYPED-VARIABLE VARIANT-ENC ENCODING:encoding
TYPED-VARIABLE VARIANT-SET bool

CODEGEN:BUFFER PATH
CODEGEN:BUFFER TEXT
CODEGEN:BUFFER LABEL-TEXT

: ENCODINGS-PATH$ ( -- str )
   s" data/encoding/encodings.json" ;

: DATA-PATH$ ( -- str )
   s" src/lib/encoding/data.f" ;

: INDEX-PATH ( str -- str ) {: name:str :}
   PATH CODEGEN:RESET
   s" data/encoding/index-" PATH CODEGEN:APPEND-STRING
   name PATH CODEGEN:APPEND-STRING
   s" .txt" PATH CODEGEN:APPEND-STRING
   PATH CODEGEN:CONTENTS ;

\ The whole file, in a buffer the process keeps.
: READ-INPUT ( str -- str )
   dup FS:FILE-SIZE 1+ MEM-ALLOC-64K-SPAN {: path:str buf:str :}
   path buf STR:DATA buf STR:LEN FS:READ-ALL {: got:n :}
   buf STR:DATA got >str ;

\ Index files

: DATA-LINE? ( str -- bool ) {: s:str :}
   s STR:LEN 0= if false exit then
   s STR:DATA c@ HASH <> ;

\ Runs q on each line that is neither empty nor a `#` comment.
: EACH-LINE ( str [ str -- ] -- ) {: s:str q :}
   0 begin dup s STR:LEN <= while
      {: start:n :}
      s LINE-FEED start STR:SPLIT-NEXT {: line:str next:n valid:bool :}
      valid 0= if E-MISMATCH throw then
      line DATA-LINE? if line q execute then
      next
   repeat drop ;

\ The tab-separated field of a line that starts at start.
: FIELD-AT ( str n -- str n ) {: s:str start:n :}
   s TAB start STR:SPLIT-NEXT {: f:str next:n valid:bool :}
   valid 0= if E-MISMATCH throw then
   f next ;

\ A digit's value; 99 for a byte that is no digit.
: DIGIT-OF ( n -- n ) {: c:n :}
   c 48 >= c 57 <= and if c 48 - exit then
   c 65 >= c 70 <= and if c 55 - exit then
   c 97 >= c 102 <= and if c 87 - exit then
   99 ;

: NUMBER ( str n -- n ) {: s:str base:n :}
   s STR:LEN 0= s STR:LEN 8 > or if E-MISMATCH throw then
   0 s STR:LEN 0 ?do
      base * s STR:DATA i + c@ DIGIT-OF
      dup base >= if E-MISMATCH throw then +
   loop ;

\ An index line's pointer and code point: blanks, the decimal pointer, a
\ tab, 0x and the code point's hex, then a tab and a comment or nothing.
: LINE-ENTRY ( str -- n n ) {: line:str :}
   line 0 FIELD-AT {: p:str next:n :}
   line next FIELD-AT drop {: c:str :}
   c s" 0x" STR:STARTS-WITH? 0= if E-MISMATCH throw then
   p STR:LTRIM 10 NUMBER
   c STR:DATA 2 + c STR:LEN 2 - >str 16 NUMBER ;

: TRUTH+ ( str -- )
   LINE-ENTRY {: p:n cp:n :}
   p POINTER-CAPACITY >= cp 0= or if E-MISMATCH throw then
   p TRUTH @ 0<> if E-MISMATCH throw then
   cp p TRUTH ! ;

: LOAD-INDEX ( str -- )
   POINTER-CAPACITY 0 ?do 0 i TRUTH !  0 i REACHED ! loop
   INDEX-PATH READ-INPUT [: TRUTH+ ;] EACH-LINE ;

\ Each pointer the index holds was decoded by some pass.
: CHECK-REACHED ( -- )
   POINTER-CAPACITY 0 ?do
      i TRUTH @ 0<> i REACHED @ 0= and if E-MISMATCH throw then
   loop ;

\ Expected bytes, encoded here as the Standard's UTF-8 encoder does.

: WANT+ ( n -- )
   WANT-VEC @ VEC:PUSH c! ;

\ A continuation byte of a scalar's bits from shift on.
: TAIL+ ( n n -- )
   rshift $3F and $80 or WANT+ ;

: LEAD+ ( n n n -- ) {: cp:n shift:n mark:n :}
   cp shift rshift mark or WANT+ ;

: SCALAR+ ( n -- ) {: cp:n :}
   cp $80 < if cp WANT+ exit then
   cp $800 < if cp 6 $C0 LEAD+ cp 0 TAIL+ exit then
   cp $10000 < if
      cp 12 $E0 LEAD+ cp 6 TAIL+ cp 0 TAIL+ exit
   then
   cp 18 $F0 LEAD+ cp 12 TAIL+ cp 6 TAIL+ cp 0 TAIL+ ;

\ The pointer's code point from the index, or false where it has none.
: INDEXED ( n -- bool ) {: p:n :}
   p TRUTH @ {: cp:n :}
   cp 0= if false exit then
   1 p REACHED !  cp SCALAR+  true ;

\ Big5's four pointers that give a letter and a combining mark.
: BIG5-WANT ( n -- bool ) {: p:n :}
   p 1133 = if $CA SCALAR+ $304 SCALAR+ true exit then
   p 1135 = if $CA SCALAR+ $30C SCALAR+ true exit then
   p 1164 = if $EA SCALAR+ $304 SCALAR+ true exit then
   p 1166 = if $EA SCALAR+ $30C SCALAR+ true exit then
   p INDEXED ;

\ Shift_JIS pointers 8836-10715 are the user-defined U+E000 on.
: SHIFT-JIS-WANT ( n -- bool ) {: p:n :}
   p 8836 >= p 10715 <= and if p 8836 - $E000 + SCALAR+ true exit then
   p INDEXED ;

\ Decoding

: RESET ( ^VEC:vector<u8> -- )
   0 VEC:RESIZE ;

: IN+ ( n -- )
   IN-VEC @ VEC:PUSH c! ;

: PAIR-IN ( str n n -- ) {: pre:str lead:n b:n :}
   IN-VEC @ RESET  pre IN-VEC @ VEC:APPEND  lead IN+  b IN+ ;

\ IN-VEC's bytes in ENC give WANT-VEC's.
: DECODES ( -- )
   OUT-VEC @ RESET
   IN-VEC @ VEC:BYTES ENC @ OUT-VEC @ ENCODING:DECODE
   OUT-VEC @ VEC:BYTES WANT-VEC @ VEC:BYTES STR:=
   0= if E-MISMATCH throw then
   1 POINTS +! ;

\ IN-VEC's bytes in ENC throw E-DECODE.
: REJECTS ( -- )
   OUT-VEC @ RESET
   [: IN-VEC @ VEC:BYTES ENC @ OUT-VEC @ ENCODING:DECODE ;] catch
   {: code:n :}
   code 0= if E-MISMATCH throw then
   code ENCODING:E-DECODE <> if code throw then
   1 EMPTIES +! ;

\ IN-VEC's bytes form pointer p; want writes its bytes or answers false.
: CHECK ( n [ n -- bool ] -- ) {: p:n want :}
   WANT-VEC @ RESET
   p want execute if DECODES else REJECTS then ;

: REPORT ( str str -- ) {: index:str name:str :}
   s" encoding-verify: index " type index type
   s"  through " type name type s" : " type
   POINTS @ FMT:INT$ type s"  pointers, " type
   EMPTIES @ FMT:INT$ type s"  empty" type cr ;

\ Runs pairs in e and prints its counts.
: PASS ( str str ENCODING:encoding [ -- ] -- )
   {: index:str name:str e pairs :}
   e ENC !  0 POINTS !  0 EMPTIES !
   pairs execute
   index name REPORT ;

\ Single-byte encodings: pointer is the byte less $80.

: BYTES ( -- )
   $100 $80 ?do
      IN-VEC @ RESET  i IN+
      i $80 - [: INDEXED ;] CHECK
   loop ;

: SINGLE-AS ( str str ENCODING:encoding -- ) {: index:str name:str e :}
   index LOAD-INDEX
   index name e [: BYTES ;] PASS
   CHECK-REACHED ;

: SINGLE ( str ENCODING:encoding -- ) {: index:str e :}
   index index e SINGLE-AS ;

: SINGLE-BYTES ( -- )
   s" ibm866" ENCODING-ENCODING:IBM866 SINGLE
   s" iso-8859-2" ENCODING-ENCODING:ISO-8859-2 SINGLE
   s" iso-8859-3" ENCODING-ENCODING:ISO-8859-3 SINGLE
   s" iso-8859-4" ENCODING-ENCODING:ISO-8859-4 SINGLE
   s" iso-8859-5" ENCODING-ENCODING:ISO-8859-5 SINGLE
   s" iso-8859-6" ENCODING-ENCODING:ISO-8859-6 SINGLE
   s" iso-8859-7" ENCODING-ENCODING:ISO-8859-7 SINGLE
   s" iso-8859-8" ENCODING-ENCODING:ISO-8859-8 SINGLE
   s" iso-8859-8" s" iso-8859-8-i"
   ENCODING-ENCODING:ISO-8859-8-I SINGLE-AS
   s" iso-8859-10" ENCODING-ENCODING:ISO-8859-10 SINGLE
   s" iso-8859-13" ENCODING-ENCODING:ISO-8859-13 SINGLE
   s" iso-8859-14" ENCODING-ENCODING:ISO-8859-14 SINGLE
   s" iso-8859-15" ENCODING-ENCODING:ISO-8859-15 SINGLE
   s" iso-8859-16" ENCODING-ENCODING:ISO-8859-16 SINGLE
   s" koi8-r" ENCODING-ENCODING:KOI8-R SINGLE
   s" koi8-u" ENCODING-ENCODING:KOI8-U SINGLE
   s" macintosh" ENCODING-ENCODING:MACINTOSH SINGLE
   s" windows-874" ENCODING-ENCODING:WINDOWS-874 SINGLE
   s" windows-1250" ENCODING-ENCODING:WINDOWS-1250 SINGLE
   s" windows-1251" ENCODING-ENCODING:WINDOWS-1251 SINGLE
   s" windows-1252" ENCODING-ENCODING:WINDOWS-1252 SINGLE
   s" windows-1253" ENCODING-ENCODING:WINDOWS-1253 SINGLE
   s" windows-1254" ENCODING-ENCODING:WINDOWS-1254 SINGLE
   s" windows-1255" ENCODING-ENCODING:WINDOWS-1255 SINGLE
   s" windows-1256" ENCODING-ENCODING:WINDOWS-1256 SINGLE
   s" windows-1257" ENCODING-ENCODING:WINDOWS-1257 SINGLE
   s" windows-1258" ENCODING-ENCODING:WINDOWS-1258 SINGLE
   s" x-mac-cyrillic" ENCODING-ENCODING:X-MAC-CYRILLIC SINGLE ;

\ gb18030 and gbk two-byte: lead $81-$FE, trail $40-$7E or $80-$FE.

: GB-POINTER ( n n -- n ) {: lead:n b:n :}
   lead $81 - 190 *  b +  b $7F < if $40 else $41 then - ;

: GB-PAIR ( n n -- ) {: lead:n b:n :}
   s" " lead b PAIR-IN
   lead b GB-POINTER [: INDEXED ;] CHECK ;

: GB-LEAD ( n -- ) {: lead:n :}
   $7F $40 ?do lead i GB-PAIR loop
   $FF $80 ?do lead i GB-PAIR loop ;

: GB-PAIRS ( -- )
   $FF $81 ?do i GB-LEAD loop ;

: GB18030$ ( -- str ) s" gb18030" ;

: GB18030-INDEX ( -- )
   GB18030$ LOAD-INDEX
   GB18030$ s" gbk" ENCODING-ENCODING:GBK [: GB-PAIRS ;] PASS
   GB18030$ GB18030$ ENCODING-ENCODING:GB18030 [: GB-PAIRS ;] PASS
   CHECK-REACHED ;

\ gb18030 four-byte

: RANGE+ ( str -- )
   LINE-ENTRY {: p:n cp:n :}
   RANGE-N @ RANGE-CAPACITY >= if E-MISMATCH throw then
   p RANGE-N @ 2 * RANGES !  cp RANGE-N @ 2 * 1+ RANGES !
   1 RANGE-N +! ;

: LOAD-RANGES ( -- )
   0 RANGE-N !
   s" gb18030-ranges" INDEX-PATH READ-INPUT [: RANGE+ ;] EACH-LINE ;

: FIRST@ ( n -- n ) 2 * RANGES @ ;

\ The Standard's "index gb18030 ranges code point"; 0 for null.
: RANGES-SCALAR ( n -- n ) {: p:n :}
   p 39419 > p 189000 < and p 1237575 > or if 0 exit then
   p 7457 = if $E7C7 exit then
   -1 RANGE-N @ 0 ?do i FIRST@ p <= if drop i then loop {: r:n :}
   r 0 < if 0 exit then
   r 2 * 1+ RANGES @  p +  r FIRST@ - ;

: RANGES-WANT ( n -- bool )
   RANGES-SCALAR dup 0= if drop false exit then
   SCALAR+ true ;

: FOUR-IN ( n -- ) {: p:n :}
   IN-VEC @ RESET
   p 12600 / $81 + IN+
   p 12600 mod 1260 / $30 + IN+
   p 1260 mod 10 / $81 + IN+
   p 10 mod $30 + IN+ ;

: FOUR ( n -- ) {: p:n :}
   p FOUR-IN  p [: RANGES-WANT ;] CHECK ;

\ Each range's first pointer and the one before it, then the special
\ pointers and those either side.
: FOUR-POINTERS ( -- )
   RANGE-N @ 0 ?do
      i FIRST@ dup 0 > if dup 1- FOUR then FOUR
   loop
   7456 FOUR 7457 FOUR 7458 FOUR 39419 FOUR 39420 FOUR
   188999 FOUR 189000 FOUR 189001 FOUR
   1237574 FOUR 1237575 FOUR 1237576 FOUR ;

: FOUR-BYTE ( -- )
   LOAD-RANGES
   s" gb18030-ranges" GB18030$ ENCODING-ENCODING:GB18030
   [: FOUR-POINTERS ;] PASS ;

\ Big5: lead $81-$FE, trail $40-$7E or $A1-$FE.

: BIG5-POINTER ( n n -- n ) {: lead:n b:n :}
   lead $81 - 157 *  b +  b $7F < if $40 else $62 then - ;

: BIG5-PAIR ( n n -- ) {: lead:n b:n :}
   s" " lead b PAIR-IN
   lead b BIG5-POINTER [: BIG5-WANT ;] CHECK ;

: BIG5-LEAD ( n -- ) {: lead:n :}
   $7F $40 ?do lead i BIG5-PAIR loop
   $FF $A1 ?do lead i BIG5-PAIR loop ;

: BIG5-INDEX ( -- )
   s" big5" LOAD-INDEX
   s" big5" s" big5" ENCODING-ENCODING:BIG5
   [: $FF $81 ?do i BIG5-LEAD loop ;] PASS
   CHECK-REACHED ;

\ EUC-KR: lead $81-$FE, trail $41-$FE.

: EUC-KR-PAIR ( n n -- ) {: lead:n b:n :}
   s" " lead b PAIR-IN
   lead $81 - 190 * b $41 - + [: INDEXED ;] CHECK ;

: EUC-KR-LEAD ( n -- ) {: lead:n :}
   $FF $41 ?do lead i EUC-KR-PAIR loop ;

: EUC-KR-INDEX ( -- )
   s" euc-kr" LOAD-INDEX
   s" euc-kr" s" euc-kr" ENCODING-ENCODING:EUC-KR
   [: $FF $81 ?do i EUC-KR-LEAD loop ;] PASS
   CHECK-REACHED ;

\ jis0208 and jis0212: lead and trail each 94 bytes from base, behind pre.

: JIS-PAIR ( str n n n -- ) {: pre:str base:n lead:n b:n :}
   pre lead b PAIR-IN
   lead base - 94 *  b base - + [: INDEXED ;] CHECK ;

: JIS-LEAD ( str n n -- ) {: pre:str base:n lead:n :}
   base 94 + base ?do pre base lead i JIS-PAIR loop ;

: JIS-PAIRS ( str n -- ) {: pre:str base:n :}
   base 94 + base ?do pre base i JIS-LEAD loop ;

\ ISO-2022-JP's escape to jis0208.
: JIS0208-ESCAPE$ ( -- str ) s\" \x1B$B" ;

\ Shift_JIS: lead $81-$9F or $E0-$FC, trail $40-$7E or $80-$FC.

: SHIFT-JIS-POINTER ( n n -- n ) {: lead:n b:n :}
   lead $A0 < if $81 else $C1 then {: lead-off:n :}
   b $7F < if $40 else $41 then {: off:n :}
   lead lead-off - 188 *  b +  off - ;

: SHIFT-JIS-PAIR ( n n -- ) {: lead:n b:n :}
   s" " lead b PAIR-IN
   lead b SHIFT-JIS-POINTER [: SHIFT-JIS-WANT ;] CHECK ;

: SHIFT-JIS-LEAD ( n -- ) {: lead:n :}
   $7F $40 ?do lead i SHIFT-JIS-PAIR loop
   $FD $80 ?do lead i SHIFT-JIS-PAIR loop ;

: SHIFT-JIS-PAIRS ( -- )
   $A0 $81 ?do i SHIFT-JIS-LEAD loop
   $FD $E0 ?do i SHIFT-JIS-LEAD loop ;

: JIS0208$ ( -- str ) s" jis0208" ;

: JIS0208-INDEX ( -- )
   JIS0208$ LOAD-INDEX
   JIS0208$ s" euc-jp" ENCODING-ENCODING:EUC-JP
   [: s" " $A1 JIS-PAIRS ;] PASS
   JIS0208$ s" iso-2022-jp" ENCODING-ENCODING:ISO-2022-JP
   [: JIS0208-ESCAPE$ $21 JIS-PAIRS ;] PASS
   JIS0208$ s" shift_jis" ENCODING-ENCODING:SHIFT-JIS
   [: SHIFT-JIS-PAIRS ;] PASS
   CHECK-REACHED ;

: JIS0212-INDEX ( -- )
   s" jis0212" LOAD-INDEX
   s" jis0212" s" euc-jp" ENCODING-ENCODING:EUC-JP
   [: s\" \x8F" $A1 JIS-PAIRS ;] PASS
   CHECK-REACHED ;

\ Labels

\ A constructor's tail byte: upper case, with `_` as `-`.
: CONSTRUCTOR-BYTE ( n -- n )
   dup UNDERSCORE = if drop HYPHEN exit then
   STR:ASCII-UPPER ;

\ The encoding named, from its variant's constructor.
: VARIANT ( str -- ENCODING:encoding ) {: name:str :}
   TEXT CODEGEN:RESET
   s" ENCODING-ENCODING:" TEXT CODEGEN:APPEND-STRING
   name STR:LEN 0 ?do
      name STR:DATA i + c@ CONSTRUCTOR-BYTE TEXT CODEGEN:APPEND-BYTE
   loop
   s"  ENCODING-VERIFY:VARIANT!" TEXT CODEGEN:APPEND-STRING
   false VARIANT-SET !
   TEXT CODEGEN:CONTENTS evaluate-closed
   VARIANT-SET @ 0= if E-MISMATCH throw then
   VARIANT-ENC @ ;

\ LABEL gives want for s.
: GIVES ( str ENCODING:encoding -- ) {: s:str want :}
   s ENCODING:LABEL MATCH option
      none OF E-MISMATCH throw ENDOF
      some OF want ENCODING-ENCODING:EQ 0= if E-MISMATCH throw then ENDOF
   ;MATCH ;

: LABEL$+ ( str -- )
   LABEL-TEXT CODEGEN:APPEND-STRING ;

: UPPER ( str -- str ) {: s:str :}
   LABEL-TEXT CODEGEN:RESET
   s STR:LEN 0 ?do
      s STR:DATA i + c@ STR:ASCII-UPPER LABEL-TEXT CODEGEN:APPEND-BYTE
   loop
   LABEL-TEXT CODEGEN:CONTENTS ;

\ TAB, LF, FF, CR and SPACE on both sides.
: PADDED ( str -- str ) {: s:str :}
   LABEL-TEXT CODEGEN:RESET
   s\" \x09\x0A\x0C\x0D " LABEL$+
   s LABEL$+
   s\"  \x0D\x0C\x0A\x09" LABEL$+
   LABEL-TEXT CODEGEN:CONTENTS ;

: CHECK-LABEL ( str ENCODING:encoding -- ) {: s:str e :}
   s e GIVES  s UPPER e GIVES  s PADDED e GIVES
   1 LABEL-TOTAL +! ;

: CHECK-ENCODING ( JSON:value -- ) {: enc :}
   enc s" name" JSON:GET [: VARIANT ;] JSON:WITH-STRING {: e :}
   enc s" labels" JSON:GET
   [: [: e CHECK-LABEL ;] JSON:WITH-STRING ;] JSON:EACH
   1 ENCODING-TOTAL +! ;

: CHECK-GROUP ( JSON:value -- )
   s" encodings" JSON:GET [: CHECK-ENCODING ;] JSON:EACH ;

: CHECK-LABELS ( -- )
   0 LABEL-TOTAL !  0 ENCODING-TOTAL !
   ENCODINGS-PATH$ READ-INPUT JSON:PARSE [: CHECK-GROUP ;] JSON:EACH
   s" encoding-verify: " type LABEL-TOTAL @ FMT:INT$ type
   s"  labels of " type ENCODING-TOTAL @ FMT:INT$ type
   s"  encodings" type cr ;

: CHECK-REGENERATION ( -- )
   ENCODING-TOOL:RENDER {: generated:str :}
   DATA-PATH$ READ-INPUT {: file:str :}
   generated file STR:= 0= if E-MISMATCH throw then ;

: NEW-VECTOR ( -- ^VEC:vector<u8> )
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW ;

: MAKE ( -- )
   NEW-VECTOR IN-VEC !  NEW-VECTOR WANT-VEC !  NEW-VECTOR OUT-VEC !
   POINTER-CAPACITY TRUTH-RESERVE
   POINTER-CAPACITY REACHED-RESERVE
   RANGE-CAPACITY 2 * RANGES-RESERVE ;

: RELEASE ( -- )
   IN-VEC @ VEC:DELETE  WANT-VEC @ VEC:DELETE  OUT-VEC @ VEC:DELETE ;

public

\ The hook of VARIANT's text, which runs at top level.
: VARIANT! ( ENCODING:encoding -- )
   VARIANT-ENC !  true VARIANT-SET ! ;

: VERIFY ( -- )
   MAKE
   SINGLE-BYTES
   GB18030-INDEX
   FOUR-BYTE
   BIG5-INDEX
   EUC-KR-INDEX
   JIS0208-INDEX
   JIS0212-INDEX
   RELEASE
   CHECK-LABELS
   CHECK-REGENERATION
   s" encoding-verify: ok" type cr ;

;package
