\ artifacts.f - checked replay artifact path and SHA-256 JSON fields.
\
\ Load after lib/errors.f, lib/string.f, lib/fs.f, and src/core/sha256.f.

64 constant BA-SHA-LEN
$2000 constant BA-JSON-CAP

8 constant BA-BS
9 constant BA-TAB
10 constant BA-LF
12 constant BA-FF
13 constant BA-CR
32 constant BA-SP
34 constant BA-DQ
44 constant BA-COMMA-C
48 constant BA-ZERO
58 constant BA-COLON-C
92 constant BA-BACKSLASH
123 constant BA-LBRACE
125 constant BA-RBRACE

-3203 constant E-BM-ARTIFACT-MISSING
-3204 constant E-BM-ARTIFACT-HASH
-3205 constant E-BM-ARTIFACT-CAPACITY

create BA-PROMPT-PATH FS-PATH-CAP allot
create BA-RESPONSE-PATH FS-PATH-CAP allot
create BA-CANDIDATE-PATH FS-PATH-CAP allot
create BA-DIAGNOSTIC-PATH FS-PATH-CAP allot
create BA-PROMPT-SHA BA-SHA-LEN allot
create BA-RESPONSE-SHA BA-SHA-LEN allot
create BA-CANDIDATE-SHA BA-SHA-LEN allot
create BA-DIAGNOSTIC-SHA BA-SHA-LEN allot
create BA-JSON-BUF BA-JSON-CAP allot

variable BA-PROMPT-U
variable BA-RESPONSE-U
variable BA-CANDIDATE-U
variable BA-DIAGNOSTIC-U
variable BA-JSON-U

: BA-COPY-PATH ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u dst:ptr lenp:ptr :}
   u 0 <= if E-BM-ARTIFACT-MISSING throw then
   u FS-PATH-CAP > if E-BM-ARTIFACT-CAPACITY throw then
   a dst u BYTE-COPY
   u lenp ! ;

: BA-PROMPT! ( ptr u8 n -- )
   BA-PROMPT-PATH BA-PROMPT-U BA-COPY-PATH ;

: BA-RESPONSE! ( ptr u8 n -- )
   BA-RESPONSE-PATH BA-RESPONSE-U BA-COPY-PATH ;

: BA-CANDIDATE! ( ptr u8 n -- )
   BA-CANDIDATE-PATH BA-CANDIDATE-U BA-COPY-PATH ;

: BA-DIAGNOSTIC! ( ptr u8 n -- )
   BA-DIAGNOSTIC-PATH BA-DIAGNOSTIC-U BA-COPY-PATH ;

: BA-PROMPT$ ( -- ptr u8 n )
   BA-PROMPT-PATH BA-PROMPT-U @ ;

: BA-RESPONSE$ ( -- ptr u8 n )
   BA-RESPONSE-PATH BA-RESPONSE-U @ ;

: BA-CANDIDATE$ ( -- ptr u8 n )
   BA-CANDIDATE-PATH BA-CANDIDATE-U @ ;

: BA-DIAGNOSTIC$ ( -- ptr u8 n )
   BA-DIAGNOSTIC-PATH BA-DIAGNOSTIC-U @ ;

: BA-PROMPT-SHA$ ( -- ptr u8 n )
   BA-PROMPT-SHA BA-SHA-LEN ;

: BA-RESPONSE-SHA$ ( -- ptr u8 n )
   BA-RESPONSE-SHA BA-SHA-LEN ;

: BA-CANDIDATE-SHA$ ( -- ptr u8 n )
   BA-CANDIDATE-SHA BA-SHA-LEN ;

: BA-DIAGNOSTIC-SHA$ ( -- ptr u8 n )
   BA-DIAGNOSTIC-SHA BA-SHA-LEN ;

: BA-RESET ( -- )
   0 BA-PROMPT-U !
   0 BA-RESPONSE-U !
   0 BA-CANDIDATE-U !
   0 BA-DIAGNOSTIC-U !
   0 BA-JSON-U ! ;

: BA-RECORD ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- )
   {: prompt:ptr promptu response:ptr responseu candidate:ptr candidateu diag:ptr diagu :}
   prompt promptu BA-PROMPT!
   response responseu BA-RESPONSE!
   candidate candidateu BA-CANDIDATE!
   diag diagu BA-DIAGNOSTIC! ;

: BA-HASH-ONE ( ptr u8 n ptr u8 -- ) {: path:ptr pathu dst:ptr :}
   pathu 0 <= if E-BM-ARTIFACT-MISSING throw then
   path pathu FILE? 0= if E-BM-ARTIFACT-MISSING throw then
   path pathu dst SHA256-FILE-HEX {: rc :}
   rc 0 <> if E-BM-ARTIFACT-HASH throw then ;

: BA-HASH-ALL ( -- )
   BA-PROMPT$ BA-PROMPT-SHA BA-HASH-ONE
   BA-RESPONSE$ BA-RESPONSE-SHA BA-HASH-ONE
   BA-CANDIDATE$ BA-CANDIDATE-SHA BA-HASH-ONE
   BA-DIAGNOSTIC$ BA-DIAGNOSTIC-SHA BA-HASH-ONE ;

: BA-JSON-RESET ( -- )
   0 BA-JSON-U ! ;

: BA-JSON-C ( n -- ) {: c :}
   BA-JSON-U @ 1+ BA-JSON-CAP > if E-BM-ARTIFACT-CAPACITY throw then
   c BA-JSON-BUF BA-JSON-U @ + c!
   BA-JSON-U @ 1+ BA-JSON-U ! ;

: BA-JSON-RAW ( ptr u8 n -- ) {: a:ptr u :}
   u 0 < if E-BM-ARTIFACT-CAPACITY throw then
   BA-JSON-U @ u + BA-JSON-CAP > if E-BM-ARTIFACT-CAPACITY throw then
   a BA-JSON-BUF BA-JSON-U @ + u BYTE-COPY
   BA-JSON-U @ u + BA-JSON-U ! ;

: BA-HEX ( n -- n )
   dup 10 < if BA-ZERO + else 55 + then ;

: BA-U00 ( n -- ) {: c :}
   BA-BACKSLASH BA-JSON-C
   117 BA-JSON-C
   BA-ZERO BA-JSON-C
   BA-ZERO BA-JSON-C
   c 4 rshift BA-HEX BA-JSON-C
   c $F and BA-HEX BA-JSON-C ;

: BA-ESC-C ( n -- ) {: c :}
   c BA-DQ = if BA-BACKSLASH BA-JSON-C BA-DQ BA-JSON-C exit then
   c BA-BACKSLASH = if BA-BACKSLASH BA-JSON-C BA-BACKSLASH BA-JSON-C exit then
   c BA-BS = if BA-BACKSLASH BA-JSON-C 98 BA-JSON-C exit then
   c BA-FF = if BA-BACKSLASH BA-JSON-C 102 BA-JSON-C exit then
   c BA-LF = if BA-BACKSLASH BA-JSON-C 110 BA-JSON-C exit then
   c BA-CR = if BA-BACKSLASH BA-JSON-C 114 BA-JSON-C exit then
   c BA-TAB = if BA-BACKSLASH BA-JSON-C 116 BA-JSON-C exit then
   c BA-SP < if c BA-U00 exit then
   c BA-JSON-C ;

: BA-JSON-STRING ( ptr u8 n -- ) {: a:ptr u :}
   BA-DQ BA-JSON-C
   0 begin dup u < while
      dup a + c@ BA-ESC-C
      1+
   repeat drop
   BA-DQ BA-JSON-C ;

: BA-JSON-KEY-SUFFIX ( ptr u8 n ptr u8 n -- ) {: key:ptr keyu suffix:ptr suffixu :}
   BA-DQ BA-JSON-C
   key keyu BA-JSON-RAW
   suffix suffixu BA-JSON-RAW
   BA-DQ BA-JSON-C
   BA-COLON-C BA-JSON-C ;

: BA-JSON-COMMA ( -- )
   BA-COMMA-C BA-JSON-C ;

: BA-ARTIFACT-JSON-FIELDS ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: stem:ptr stemu path:ptr pathu sha:ptr shau :}
   stem stemu s" _path" BA-JSON-KEY-SUFFIX
   path pathu BA-JSON-STRING
   BA-JSON-COMMA
   stem stemu s" _sha256" BA-JSON-KEY-SUFFIX
   sha shau BA-JSON-STRING ;

: BA-JSON$ ( -- ptr u8 n )
   BA-HASH-ALL
   BA-JSON-RESET
   BA-LBRACE BA-JSON-C
   s" prompt" BA-PROMPT$ BA-PROMPT-SHA$ BA-ARTIFACT-JSON-FIELDS
   BA-JSON-COMMA
   s" raw_response" BA-RESPONSE$ BA-RESPONSE-SHA$ BA-ARTIFACT-JSON-FIELDS
   BA-JSON-COMMA
   s" extracted_candidate" BA-CANDIDATE$ BA-CANDIDATE-SHA$ BA-ARTIFACT-JSON-FIELDS
   BA-JSON-COMMA
   s" checker_diagnostics" BA-DIAGNOSTIC$ BA-DIAGNOSTIC-SHA$ BA-ARTIFACT-JSON-FIELDS
   BA-RBRACE BA-JSON-C
   BA-JSON-BUF BA-JSON-U @ ;
