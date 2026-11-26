\ generate.f - deterministic generator of src/lib/encoding/data.f, the
\ Encoding Standard's encodings, labels and index tables, from the pinned
\ files in data/encoding/. encodings.json gives the encodings in the
\ Standard's order with their labels. An index line is blanks, a decimal
\ pointer, a tab, 0x and the code point's hex, then a tab and a comment; a
\ table holds the code point at each pointer and 0 where the index has none.
\ gb18030-ranges' lines are each range's first pointer and its code point.

require src/lib/prelude.f
require src/lib/string.f
require src/lib/memory.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/codegen.f
require src/lib/json/value.f

package ENCODING-TOOL

public

-7900 s" a pinned Encoding Standard file has a line or label this generator does not read"
   ERROR:FAULT E-SYNTAX
-7901 s" an index value is 0, a surrogate or above 10FFFF, a pointer repeats or is at or past its table's size, or the ranges are out of order"
   ERROR:FAULT E-RANGE
-7902 s" an Encoding Standard file cannot be read or its SHA-256 is not the pinned one: restore the pinned file"
   ERROR:FAULT E-DIGEST

private

$10FFFF constant SCALAR-MAX
$D800 constant SURROGATE-FIRST
$DFFF constant SURROGATE-LAST
\ A pointer has at most 7 decimal digits, a code point 6 hex digits.
7 constant POINTER-DIGITS
6 constant CODE-DIGITS
128 constant SINGLE-BYTE-SIZE
10 constant LINE-FEED
9 constant TAB
32 constant SPACE
33 constant BANG
34 constant QUOTE
35 constant HASH
126 constant TILDE
95 constant UNDERSCORE
45 constant HYPHEN
\ Numbers per `s"` line of data.f.
8 constant LINE-NUMBERS
64 constant SHA256-HEX-LENGTH
create INPUT-DIGEST SHA256-HEX-LENGTH allot
create DIGEST-CTX SHA256-FILE-CTX-BYTES allot

\ The code point at each pointer of the index being read; 0 for none.
DYNAMIC-BUFFER CODES n
variable LABEL-N
variable RANGE-N
variable LAST-POINTER

CODEGEN:BUFFER OUTPUT
CODEGEN:BUFFER PATH

: OUTPUT-PATH$ ( -- str )
   s" src/lib/encoding/data.f" ;

: ENCODINGS-PATH$ ( -- str )
   s" data/encoding/encodings.json" ;

: ENCODINGS-DIGEST$ ( -- str )
   s" 078212b3697f60b81225b6671bd9da2604497abff0aa8e96e4d46605c02ac9e7" ;

: RANGES-NAME$ ( -- str )
   s" gb18030-ranges" ;

: RANGES-DIGEST$ ( -- str )
   s" 874c6b6f6f74cf7d427ad228d5b41ddd9354fffd92a2259bf429f86e6baa7a1e" ;

\ Runs q on each pointer index's name, table size and SHA-256, the
\ single-byte ones in the Standard's order of their encodings.
: EACH-INDEX ( [ str n str -- ] -- ) {: q :}
   s" ibm866" SINGLE-BYTE-SIZE
   s" 1796f4955dbc5c94e3951a940e9250c819580ce5dd1c9ac396ef73a26e938264" q execute
   s" iso-8859-2" SINGLE-BYTE-SIZE
   s" 86a09a772c5ff8631dc47b8838f0b7751dc31d53fd2eb4262f0fc858e638468b" q execute
   s" iso-8859-3" SINGLE-BYTE-SIZE
   s" d49c03f3e8cab4fbf861b4471ce121beb937dbd56dfd3106b997326c4b97da0d" q execute
   s" iso-8859-4" SINGLE-BYTE-SIZE
   s" d93b8b6606e1a12ccf28418a629911b497cb7a354c8a740207992059c50917b3" q execute
   s" iso-8859-5" SINGLE-BYTE-SIZE
   s" b42bf0bb8d49701efbd8c0fb2bc7330985ce84c576ac51efe86aeff6b47aea5d" q execute
   s" iso-8859-6" SINGLE-BYTE-SIZE
   s" 84b979a90c6a4fb2a2dffe37994647cd4836f918550b46d329b0d798f4b84d52" q execute
   s" iso-8859-7" SINGLE-BYTE-SIZE
   s" fc1d4295d73dfd0dd6e1b2c2ee3de7a52c6e94ddb09358161d02a2adb08a5151" q execute
   s" iso-8859-8" SINGLE-BYTE-SIZE
   s" 25a8f4366e7935f2c5d59f34e59c400683a1df86546776e7d4e527ae7235cc9e" q execute
   s" iso-8859-10" SINGLE-BYTE-SIZE
   s" 6c1863fca2ad99eed23cc77cce075fcd3cac6bd7b50a584d3067456c2180f16e" q execute
   s" iso-8859-13" SINGLE-BYTE-SIZE
   s" 5c5fe48042ef633a88d1aeed72dd3a2e1fb5202b70c5b7696b53e76ac5e56752" q execute
   s" iso-8859-14" SINGLE-BYTE-SIZE
   s" 310ec047fbd33d8c02de309f0bc8562e453e7d873cc2093005a5d954fc8f0032" q execute
   s" iso-8859-15" SINGLE-BYTE-SIZE
   s" 1171d20bd7c98b5755e8802480d2799527a99847531284cd8d860ffda7cde449" q execute
   s" iso-8859-16" SINGLE-BYTE-SIZE
   s" e7bee177fdf60c5167b7c44351cd25f51caf9780889493c7855c0eb222a31660" q execute
   s" koi8-r" SINGLE-BYTE-SIZE
   s" 482889b92f244a03fd33ceee13a248a5fd479a3d1633019cc3b21910b8824f52" q execute
   s" koi8-u" SINGLE-BYTE-SIZE
   s" 3b8bfcaa2b7fb125e9790cc4964c4c9e51eee2c9c6e2a80ba7dfd8901546a2c6" q execute
   s" macintosh" SINGLE-BYTE-SIZE
   s" 5e2b0aa162f3032cf6c96a119f59f698da80c16d58014080a6bd246d7ade3c38" q execute
   s" windows-874" SINGLE-BYTE-SIZE
   s" 81b42398e8bef2acc004f38ed6ae2c4d162dfdc367e5ddfcb1cb752ac8814a1e" q execute
   s" windows-1250" SINGLE-BYTE-SIZE
   s" 857f28be0681aae376f1671ab388d8e141cff9648a68803b53887bfdf950f464" q execute
   s" windows-1251" SINGLE-BYTE-SIZE
   s" bf29b6293d8b5f8011804c575b78a3b589a3fd6a4ace29ff788e3445157a13c0" q execute
   s" windows-1252" SINGLE-BYTE-SIZE
   s" 2c5fa192d566f92b8b9e6e116255b1f94016b4ece5b9dacb3b98be70b8c60795" q execute
   s" windows-1253" SINGLE-BYTE-SIZE
   s" 38390cd918ac2c78c47be62bd486598e7d129e0adceb140c8a7f0165ddbbf3f5" q execute
   s" windows-1254" SINGLE-BYTE-SIZE
   s" 88b7acb81b616bec879521d371eee5766b9306eb050b814e4dce9468e0725822" q execute
   s" windows-1255" SINGLE-BYTE-SIZE
   s" 4a3ca10dd8ef4ce06b929f6bf66d7ea8932ab3c06e07162b0f8a9536a692e6ac" q execute
   s" windows-1256" SINGLE-BYTE-SIZE
   s" a3e9e6fcf4c3eda90275c791d5d7dad08700374ebe67b17cabdfff1e48f984c1" q execute
   s" windows-1257" SINGLE-BYTE-SIZE
   s" 7e56420f3460befff856365b37733f9eabc1342e82be5999ce34c5eb518fddda" q execute
   s" windows-1258" SINGLE-BYTE-SIZE
   s" 2236cdac45f5970629881d09908d13af96965f1c67617160734cbefb54aa3176" q execute
   s" x-mac-cyrillic" SINGLE-BYTE-SIZE
   s" a01be0035d0cfbb8da0af780eeb6e7c6169ab26d748d03d15ccd4cd969c5b786" q execute
   \ The sizes are the decoders' pointer limits: Shift_JIS forms
   \ jis0208 pointers up to 11279.
   s" gb18030" 23940
   s" 746b3c55f1a8ec4b90b451f384437a17fd37cd51cd668456a28071a758d10784" q execute
   s" big5" 19782
   s" 08e24270c8e95d998c994c03f907e972480dc01f58743e078654cc466203c8ff" q execute
   s" jis0208" 11280
   s" 341dcde7e8b984e9c7bbf5ed75c8da7c6087d47083a1a2b3ed558bfd5bef9468" q execute
   s" jis0212" 8836
   s" 6ac698a473822b282ff4f4d4f17873b970612ef9bd232e03941854c09b67c7ee" q execute
   s" euc-kr" 23940
   s" 89af20dd867c84cefb710b1790229786cfef2bf11916361a210d81b90381e267" q execute ;

: PATH+ ( str -- )
   PATH CODEGEN:APPEND-STRING ;

: INDEX-PATH ( str -- str ) {: name:str :}
   PATH CODEGEN:RESET
   s" data/encoding/index-" PATH+ name PATH+ s" .txt" PATH+
   PATH CODEGEN:CONTENTS ;

: CHECK-DIGEST ( str str -- ) {: path:str want:str :}
   DIGEST-CTX path INPUT-DIGEST SHA256-FILE-HEX-IN 0 <> if E-DIGEST throw then
   INPUT-DIGEST SHA256-HEX-LENGTH >str want STR:= 0= if E-DIGEST throw then ;

: CHECK-INDEX-DIGEST ( str n str -- ) {: name:str size:n want:str :}
   name INDEX-PATH want CHECK-DIGEST ;

: CHECK-INPUT-DIGESTS ( -- )
   ENCODINGS-PATH$ ENCODINGS-DIGEST$ CHECK-DIGEST
   RANGES-NAME$ INDEX-PATH RANGES-DIGEST$ CHECK-DIGEST
   [: CHECK-INDEX-DIGEST ;] EACH-INDEX ;

\ The whole file, in a buffer the process keeps.
: READ-INPUT ( str -- str )
   dup FS:FILE-SIZE 1+ MEM-ALLOC-64K-SPAN {: path:str buf:str :}
   path buf STR:DATA buf STR:LEN FS:READ-ALL {: got:n :}
   buf STR:DATA got >str ;

\ Runs q on each line that is neither empty nor a `#` comment.
: DATA-LINES ( str [ str -- ] -- ) {: s:str q :}
   0 begin dup s STR:LEN <= while
      {: start:n :}
      s LINE-FEED start STR:SPLIT-NEXT {: line:str next:n ok:bool :}
      ok 0= if E-SYNTAX throw then
      line STR:LEN 0<> if
         line STR:DATA c@ HASH <> if line q execute then
      then
      next
   repeat drop ;

: DECIMAL ( str -- n ) {: s:str :}
   s STR:DIGITS? 0= s STR:LEN POINTER-DIGITS > or if E-SYNTAX throw then
   0 s STR:LEN 0 ?do
      10 * s STR:DATA i + c@ STR:DIGIT-VALUE +
   loop ;

: HEX-DIGIT ( n -- n )
   dup STR:HEX-DIGIT? 0= if E-SYNTAX throw then
   STR:HEX-DIGIT-VALUE ;

\ 0x and up to CODE-DIGITS hex digits.
: HEX ( str -- n ) {: s:str :}
   s s" 0x" STR:STARTS-WITH? 0= if E-SYNTAX throw then
   s STR:LEN 3 < s STR:LEN CODE-DIGITS 2 + > or if E-SYNTAX throw then
   0 s STR:LEN 2 ?do
      16 * s STR:DATA i + c@ HEX-DIGIT +
   loop ;

: LINE-POINTER ( str -- n n ) {: line:str :}
   line TAB 0 STR:SPLIT-NEXT {: p:str next:n ok:bool :}
   line TAB next STR:SPLIT-NEXT {: c:str after:n more:bool :}
   ok more and 0= if E-SYNTAX throw then
   p STR:LTRIM DECIMAL c HEX ;

: SCALAR? ( n -- bool ) {: cp:n :}
   cp 0 > cp SCALAR-MAX <= and
   cp SURROGATE-FIRST < cp SURROGATE-LAST > or and ;

: PLACE ( str n -- ) {: line:str size:n :}
   line LINE-POINTER {: at:n cp:n :}
   cp SCALAR? 0= at size >= or if E-RANGE throw then
   at CODES @ 0<> if E-RANGE throw then
   cp at CODES ! ;

: READ-INDEX ( str n -- ) {: name:str size:n :}
   size CODES-RESERVE
   size 0 ?do 0 i CODES ! loop
   name INDEX-PATH READ-INPUT [: size PLACE ;] DATA-LINES ;

: OUT$ ( str -- )
   OUTPUT CODEGEN:APPEND-STRING ;

: OUT-N ( n -- )
   OUTPUT CODEGEN:APPEND-DECIMAL ;

: OUT-BYTE ( n -- )
   OUTPUT CODEGEN:APPEND-BYTE ;

: OUT-LINE ( str -- )
   OUT$ LINE-FEED OUT-BYTE ;

\ Upper case.
: OUT-UPPER ( str -- ) {: s:str :}
   s STR:LEN 0 ?do s STR:DATA i + c@ STR:ASCII-UPPER OUT-BYTE loop ;

\ A variant is the encoding's name with `_` as `-`.
: VARIANT-BYTE ( str n -- n ) {: s:str at:n :}
   s STR:DATA at + c@ dup UNDERSCORE = if drop HYPHEN then ;

\ The variant, lower case.
: OUT-VARIANT ( str -- ) {: s:str :}
   s STR:LEN 0= if E-SYNTAX throw then
   s STR:LEN 0 ?do s i VARIANT-BYTE STR:ASCII-LOWER OUT-BYTE loop ;

\ The variant's constructor.
: OUT-CONSTRUCTOR ( str -- ) {: s:str :}
   s" ENCODING-ENCODING:" OUT$
   s STR:LEN 0 ?do s i VARIANT-BYTE STR:ASCII-UPPER OUT-BYTE loop ;

\ Cells 0 to n-1 of CODES, LINE-NUMBERS to a `s"` line.
: OUT-NUMBERS ( n -- ) {: n:n :}
   n 0 ?do
      i LINE-NUMBERS mod 0= if s"    s" OUT$ QUOTE OUT-BYTE SPACE OUT-BYTE then
      i CODES @ OUT-N
      i 1+ LINE-NUMBERS mod 0= i 1+ n = or
      if QUOTE OUT-BYTE s"  CODES," OUT-LINE else SPACE OUT-BYTE then
   loop ;

: OUT-TABLE-HEAD ( n str -- ) {: size:n name:str :}
   size OUT-N s"  TYPED-BUFFER INDEX-" OUT$ name OUT-UPPER s"  n" OUT-LINE
   s" 0 INDEX-" OUT$ name OUT-UPPER s" " OUT-LINE ;

: OUT-INDEX ( str n str -- ) {: name:str size:n want:str :}
   name size READ-INDEX
   size name OUT-TABLE-HEAD
   size OUT-NUMBERS
   s" drop" OUT-LINE
   s" " OUT-LINE ;

\ A range's pointer and code point, at cells 2r and 2r+1.
: PLACE-RANGE ( str -- ) {: line:str :}
   line LINE-POINTER {: at:n cp:n :}
   cp SCALAR? 0= if E-RANGE throw then
   RANGE-N @ 0<> at LAST-POINTER @ <= and if E-RANGE throw then
   at LAST-POINTER !
   RANGE-N @ 2 * 2 + CODES-RESERVE
   at RANGE-N @ 2 * CODES !
   cp RANGE-N @ 2 * 1+ CODES !
   1 RANGE-N +! ;

: OUT-RANGES ( -- )
   0 RANGE-N !
   RANGES-NAME$ INDEX-PATH READ-INPUT [: PLACE-RANGE ;] DATA-LINES
   RANGE-N @ OUT-N s"  constant GB18030-RANGE-COUNT" OUT-LINE
   RANGE-N @ 2 * RANGES-NAME$ OUT-TABLE-HEAD
   RANGE-N @ 2 * OUT-NUMBERS
   s" drop" OUT-LINE
   s" " OUT-LINE ;

\ Runs q on each encoding's object, in the Standard's order.
: EACH-ENCODING ( JSON:value [ JSON:value -- ] -- ) {: doc q :}
   doc [: s" encodings" JSON:GET q JSON:EACH ;] JSON:EACH ;

: NAME-OF ( JSON:value [ str -- ] -- ) {: enc q :}
   enc s" name" JSON:GET q JSON:WITH-STRING ;

: OUT-ENUM-VARIANT ( JSON:value -- )
   s"    " OUT$ [: OUT-VARIANT ;] NAME-OF s" " OUT-LINE ;

: OUT-ENUM ( JSON:value -- ) {: doc :}
   s" public" OUT-LINE
   s" " OUT-LINE
   s" ENUM encoding DERIVE eq" OUT-LINE
   doc [: OUT-ENUM-VARIANT ;] EACH-ENCODING
   s" ;ENUM" OUT-LINE
   s" " OUT-LINE ;

\ A label is written inside `s"`: printable ASCII with no quote.
: CHECK-LABEL ( str -- ) {: s:str :}
   s STR:LEN 0= if E-SYNTAX throw then
   s STR:LEN 0 ?do
      s STR:DATA i + c@ {: c:n :}
      c BANG < c TILDE > or c QUOTE = or if E-SYNTAX throw then
   loop ;

: COUNT-LABEL ( JSON:value -- ) {: label :}
   1 LABEL-N +! ;

: COUNT-LABELS ( JSON:value -- )
   s" labels" JSON:GET [: COUNT-LABEL ;] JSON:EACH ;

: OUT-LABEL ( str str -- ) {: label:str variant:str :}
   label CHECK-LABEL
   s"    s" OUT$ QUOTE OUT-BYTE SPACE OUT-BYTE label OUT$ QUOTE OUT-BYTE
   SPACE OUT-BYTE variant OUT-CONSTRUCTOR s"  LABEL," OUT-LINE ;

: OUT-LABELS-OF ( str JSON:value -- ) {: variant:str enc :}
   enc s" labels" JSON:GET
   [: [: variant OUT-LABEL ;] JSON:WITH-STRING ;] JSON:EACH ;

: OUT-ENCODING-LABELS ( JSON:value -- ) {: enc :}
   enc [: enc OUT-LABELS-OF ;] NAME-OF ;

: OUT-LABELS ( JSON:value -- ) {: doc :}
   0 LABEL-N !
   doc [: COUNT-LABELS ;] EACH-ENCODING
   LABEL-N @ OUT-N s"  constant LABEL-COUNT" OUT-LINE
   s" LABEL-COUNT TYPED-BUFFER LABELS label-entry" OUT-LINE
   s" " OUT-LINE
   s" : LABEL, ( n str encoding -- n ) {: at:n s:str e:encoding :}" OUT-LINE
   s"    s e LABEL-ENTRY-MAKE at LABELS !" OUT-LINE
   s"    at 1+ ;" OUT-LINE
   s" " OUT-LINE
   s" 0" OUT-LINE
   doc [: OUT-ENCODING-LABELS ;] EACH-ENCODING
   s" drop" OUT-LINE
   s" " OUT-LINE
   s" \ The n-th label of encodings.json, from 0, and its encoding." OUT-LINE
   s" : LABEL-AT ( n -- str encoding )" OUT-LINE
   s"    LABELS @ LABEL-ENTRY-UNMAKE ;" OUT-LINE
   s" " OUT-LINE ;

: OUT-PREAMBLE ( -- )
   s" \ data.f - generated Encoding Standard encodings, labels and indexes." OUT-LINE
   s" \ Generated by tools/encoding/generate.f; do not edit." OUT-LINE
   s" \ Part of package ENCODING. An INDEX- table holds the code point at each" OUT-LINE
   s" \ pointer of its index and 0 where the index has none; a single-byte" OUT-LINE
   s" \ encoding's pointer is its byte less $80. INDEX-GB18030-RANGES holds each" OUT-LINE
   s" \ range's first pointer then its code point, in pointer order." OUT-LINE
   s" \ STORAGE CLASS. PROCESS-WIDE read-only tables; any task may read them." OUT-LINE
   s" " OUT-LINE
   s" require src/lib/prelude.f" OUT-LINE
   s" " OUT-LINE
   s" package ENCODING" OUT-LINE
   s" " OUT-LINE ;

: OUT-WRITERS ( -- )
   s" private" OUT-LINE
   s" " OUT-LINE
   s" \ Stores a line's decimal numbers, one space apart, from at on." OUT-LINE
   s" : CODES, ( ^n str -- ^n ) {: s:str :}" OUT-LINE
   s"    0 s STR:LEN 0 ?do" OUT-LINE
   s"       s STR:DATA i + c@ dup 32 = if" OUT-LINE
   s"          drop over ! cell+ 0" OUT-LINE
   s"       else" OUT-LINE
   s"          48 - swap 10 * +" OUT-LINE
   s"       then" OUT-LINE
   s"    loop" OUT-LINE
   s"    over ! cell+ ;" OUT-LINE
   s" " OUT-LINE
   s" STRUCTURE label-entry FIELD name str FIELD target encoding ;STRUCTURE" OUT-LINE
   s" " OUT-LINE ;

public

: RENDER ( -- str )
   CHECK-INPUT-DIGESTS
   ENCODINGS-PATH$ READ-INPUT JSON:PARSE {: doc :}
   OUTPUT CODEGEN:RESET
   OUT-PREAMBLE
   doc OUT-ENUM
   OUT-WRITERS
   doc OUT-LABELS
   [: OUT-INDEX ;] EACH-INDEX
   OUT-RANGES
   s" ;package" OUT-LINE
   OUTPUT CODEGEN:CONTENTS ;

: WRITE-PINNED ( -- )
   OUTPUT-PATH$ RENDER FS:ATOMIC-WRITE-FILE ;

;package
