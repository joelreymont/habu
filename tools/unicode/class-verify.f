split-str
\ class-verify.f - independent exhaustive verifier for Unicode class tables.

require src/lib/prelude.f
require src/lib/string.f
require src/lib/memory.f
require src/lib/fs.f
require src/lib/unicode/class.f
require tools/unicode/class-generate.f

package UNICODE-CLASS-VERIFY

private

-7806 s" the Unicode source data is malformed or disagrees with the class table" ERROR:FAULT E-MISMATCH
-7807 s" a class table range is reversed, outside the scalars, crosses the surrogates or does not follow the range before it"
   ERROR:FAULT E-CANONICAL

$10FFFF constant SCALAR-MAX
$110000 constant SCALAR-COUNT
$D800 constant SURROGATE-FIRST
$DFFF constant SURROGATE-LAST
1 constant LETTER-BIT
2 constant NUMBER-BIT
4 constant SPACE-BIT
8 constant ALPHABETIC-BIT
16 constant UPPERCASE-BIT
32 constant MARK-BIT
64 constant OPEN-BIT
128 constant CLOSE-BIT
256 constant MATH-SYMBOL-BIT
512 constant OTHER-SYMBOL-BIT
111 constant OPEN-TYPE
99 constant CLOSE-TYPE
10 constant LINE-FEED
59 constant SEMICOLON
35 constant HASH
46 constant DOT

\ Each scalar's class bits.
DYNAMIC-BUFFER TRUTH n
\ Each scalar's Bidi_Paired_Bracket, zero for none.
DYNAMIC-BUFFER PAIR-TRUTH n
variable VERIFY-PENDING
variable VERIFY-PENDING-LO
variable VERIFY-PENDING-BIT
create DIGEST-HEX 64 allot
create UCV-FSHA-CTX SHA256-FILE-CTX-BYTES allot

: UNICODE-PATH$ ( -- str )
   s" data/unicode/16.0.0/UnicodeData.txt" ;

: PROPERTIES-PATH$ ( -- str )
   s" data/unicode/16.0.0/PropList.txt" ;

: BRACKETS-PATH$ ( -- str )
   s" data/unicode/16.0.0/BidiBrackets.txt" ;

: DATA-PATH$ ( -- str )
   s" src/lib/unicode/class-data.f" ;

: EXPECTED-UNICODE-DIGEST$ ( -- str )
   s" ff58e5823bd095166564a006e47d111130813dcf8bf234ef79fa51a870edb48f" ;

: EXPECTED-PROPERTIES-DIGEST$ ( -- str )
   s" 53d614508e2a0b2305a8aa21cd60d993de9326cdf65993660dfcce4503548583" ;

: EXPECTED-BRACKETS-DIGEST$ ( -- str )
   s" b8f32554c6f658821fb0ee742d21c5b1f2086b9bf13071fed04894b022f93d67" ;

: HEX-DIGIT ( n -- n bool ) {: c:n :}
   c 48 >= c 57 <= and if c 48 - true exit then
   c 65 >= c 70 <= and if c 55 - true exit then
   c 97 >= c 102 <= and if c 87 - true exit then
   0 false ;

: HEX-FOLD ( str n n -- n ) {: a:^ u:n pos:n acc:n :}
   pos u >= if acc exit then
   a pos + c@ HEX-DIGIT 0= if drop E-MISMATCH throw then {: digit:n :}
   a u pos 1+ acc 16 * digit + recurse ;

: PARSE-HEX ( str -- n ) {: a:^ u:n :}
   u 0= u 6 > or if E-MISMATCH throw then
   a u 0 0 HEX-FOLD ;

: BYTE-INDEX ( str n -- n ) {: a:^ u:n c:n :}
   0 begin dup u < while
      dup a + c@ c = if exit then
      1+
   repeat ;

: NEXT-FIELD ( str n -- str n ) {: a:^ u:n start:n :}
   a u SEMICOLON start SPLIT-NEXT {: fa:^ fu:n next:n valid:bool :}
   valid 0= next u > or if E-MISMATCH throw then
   fa fu next ;

: CATEGORY-SECOND? ( n str -- bool ) {: c:n allowed:^ allowedu:n :}
   allowed allowedu c BYTE-INDEX allowedu < ;

: SYMBOL-BIT ( n -- n ) {: c:n :}
   c s" mcko" CATEGORY-SECOND? 0= if E-MISMATCH throw then
   c 109 = if MATH-SYMBOL-BIT exit then
   c 111 = if OTHER-SYMBOL-BIT exit then
   0 ;

: CLASS-BIT ( str -- n ) {: a:^ u:n :}
   u 2 <> if E-MISMATCH throw then
   a c@ 76 = if
      a 1+ c@ s" ultmo" CATEGORY-SECOND? 0= if E-MISMATCH throw then
      LETTER-BIT ALPHABETIC-BIT or a 1+ c@ 117 = if UPPERCASE-BIT or then exit
   then
   a c@ 78 = if
      a 1+ c@ s" dlo" CATEGORY-SECOND? 0= if E-MISMATCH throw then
      a 1+ c@ 108 = if NUMBER-BIT ALPHABETIC-BIT or else NUMBER-BIT then exit
   then
   a c@ 77 = if a 1+ c@ s" nce" CATEGORY-SECOND? 0= if E-MISMATCH throw then MARK-BIT exit then
   a c@ 80 = if a 1+ c@ s" cdseifo" CATEGORY-SECOND? 0= if E-MISMATCH throw then 0 exit then
   a c@ 83 = if a 1+ c@ SYMBOL-BIT exit then
   a c@ 90 = if a 1+ c@ s" slp" CATEGORY-SECOND? 0= if E-MISMATCH throw then 0 exit then
   a c@ 67 = if a 1+ c@ s" cfson" CATEGORY-SECOND? 0= if E-MISMATCH throw then 0 exit then
   E-MISMATCH throw ;

: FIRST-NAME? ( str -- bool )
   s" , First>" ENDS-WITH? ;

: LAST-NAME? ( str -- bool )
   s" , Last>" ENDS-WITH? ;

: SCALAR ( n -- n ) {: cp:n :}
   cp 0 < cp SCALAR-MAX > or if E-MISMATCH throw then
   cp ;

: TRUTH+ ( n n -- ) {: cp:n bit:n :}
   cp SCALAR {: safe:n :}
   safe TRUTH dup @ bit or swap ! ;

: MARK-RANGE-AT ( n n n -- ) {: cp:n hi:n bit:n :}
   cp begin dup hi <= while
      dup bit TRUTH+
      1+
   repeat drop ;

: MARK-RANGE ( n n n -- ) {: lo:n hi:n bit:n :}
   lo SCALAR drop hi SCALAR drop
   hi lo < if E-MISMATCH throw then
   bit 0= if exit then
   lo SURROGATE-LAST <= hi SURROGATE-FIRST >= and if E-MISMATCH throw then
   lo hi bit MARK-RANGE-AT ;

: VERIFY-UNICODE-ROW ( str -- ) {: line:^ lineu:n :}
   line lineu TRIM {: row:^ rowu:n :}
   rowu 0= if exit then
   row rowu SEMICOLON COUNT-CHAR 14 <> if E-MISMATCH throw then
   row rowu 0 NEXT-FIELD {: cpa:^ cpu:n next1:n :}
   row rowu next1 NEXT-FIELD {: name:^ nameu:n next2:n :}
   row rowu next2 NEXT-FIELD {: cat:^ catu:n next3:n :}
   next3 drop
   cpa cpu TRIM PARSE-HEX {: cp:n :}
   cat catu TRIM CLASS-BIT {: bit:n :}
   VERIFY-PENDING @ if
      name nameu LAST-NAME? 0= if E-MISMATCH throw then
      bit VERIFY-PENDING-BIT @ <> if E-MISMATCH throw then
      VERIFY-PENDING-LO @ cp bit MARK-RANGE
      false VERIFY-PENDING !
      exit
   then
   name nameu LAST-NAME? if E-MISMATCH throw then
   name nameu FIRST-NAME? if
      cp SCALAR VERIFY-PENDING-LO !
      bit VERIFY-PENDING-BIT !
      true VERIFY-PENDING !
      exit
   then
   cp bit TRUTH+ ;

: VERIFY-UNICODE-BYTES ( str -- ) {: a:^ u:n :}
   false VERIFY-PENDING !
   0 begin dup u <= while
      {: start:n :}
      a u LINE-FEED start SPLIT-NEXT {: line:^ lineu:n next:n valid:bool :}
      valid 0= if E-MISMATCH throw then
      line lineu VERIFY-UNICODE-ROW
      next
   repeat drop
   VERIFY-PENDING @ if E-MISMATCH throw then ;

: RANGE-PART ( str -- n n ) {: a:^ u:n :}
   a u DOT BYTE-INDEX {: pos:n :}
   pos u = if a u TRIM PARSE-HEX dup exit then
   pos 0= pos 1+ u >= or if E-MISMATCH throw then
   a pos + 1+ c@ DOT <> if E-MISMATCH throw then
   a pos TRIM PARSE-HEX
   a pos 2 + + u pos 2 + - TRIM PARSE-HEX ;

: VERIFY-PROP-ROW ( str -- ) {: line:^ lineu:n :}
   line lineu HASH BYTE-INDEX {: comment:n :}
   line comment TRIM {: row:^ rowu:n :}
   rowu 0= if exit then
   row rowu 0 NEXT-FIELD {: range:^ rangeu:n next:n :}
   row next + rowu next - TRIM {: name:^ nameu:n :}
   name nameu s" White_Space" STR= if SPACE-BIT else
      name nameu s" Other_Alphabetic" STR= if ALPHABETIC-BIT else
         name nameu s" Other_Uppercase" STR= if UPPERCASE-BIT else exit then
      then
   then {: bit:n :}
   range rangeu TRIM RANGE-PART bit MARK-RANGE ;

: VERIFY-PROPS-BYTES ( str -- ) {: a:^ u:n :}
   0 begin dup u <= while
      {: start:n :}
      a u LINE-FEED start SPLIT-NEXT {: line:^ lineu:n next:n valid:bool :}
      valid 0= if E-MISMATCH throw then
      line lineu VERIFY-PROP-ROW
      next
   repeat drop ;

: BRACKET-BIT ( str -- n ) {: a:^ u:n :}
   u 1 <> if E-MISMATCH throw then
   a c@ OPEN-TYPE = if OPEN-BIT exit then
   a c@ CLOSE-TYPE = if CLOSE-BIT exit then
   E-MISMATCH throw ;

: PAIR+ ( n n -- ) {: cp:n pair:n :}
   cp SCALAR PAIR-TRUTH {: at:^ :}
   at @ 0<> if E-MISMATCH throw then
   pair SCALAR at ! ;

: VERIFY-BRACKET-ROW ( str -- ) {: line:^ lineu:n :}
   line lineu HASH BYTE-INDEX {: comment:n :}
   line comment TRIM {: row:^ rowu:n :}
   rowu 0= if exit then
   row rowu 0 NEXT-FIELD {: cpa:^ cpu:n next1:n :}
   row rowu next1 NEXT-FIELD {: pa:^ pu:n next2:n :}
   row next2 + rowu next2 - TRIM BRACKET-BIT {: bit:n :}
   cpa cpu TRIM PARSE-HEX {: cp:n :}
   cp pa pu TRIM PARSE-HEX PAIR+
   cp bit TRUTH+ ;

: VERIFY-BRACKETS-BYTES ( str -- ) {: a:^ u:n :}
   0 begin dup u <= while
      {: start:n :}
      a u LINE-FEED start SPLIT-NEXT {: line:^ lineu:n next:n valid:bool :}
      valid 0= if E-MISMATCH throw then
      line lineu VERIFY-BRACKET-ROW
      next
   repeat drop ;

: LOAD-UNICODE-TRUTH ( -- )
   UNICODE-PATH$ 2dup FS:FILE-SIZE 1+ MEM-ALLOC-64K-SPAN {: path:^ pathu:n buf:^ cap:n :}
   path pathu buf cap FS:READ-ALL {: got:n :}
   buf got VERIFY-UNICODE-BYTES ;

: LOAD-PROPERTY-TRUTH ( -- )
   PROPERTIES-PATH$ 2dup FS:FILE-SIZE 1+ MEM-ALLOC-64K-SPAN {: path:^ pathu:n buf:^ cap:n :}
   path pathu buf cap FS:READ-ALL {: got:n :}
   buf got VERIFY-PROPS-BYTES ;

: LOAD-BRACKET-TRUTH ( -- )
   BRACKETS-PATH$ 2dup FS:FILE-SIZE 1+ MEM-ALLOC-64K-SPAN {: path:^ pathu:n buf:^ cap:n :}
   path pathu buf cap FS:READ-ALL {: got:n :}
   buf got VERIFY-BRACKETS-BYTES ;

: CHECK-DIGEST ( str str -- ) {: path:^ pathu:n want:^ wantu:n :}
   wantu 64 <> if UNICODE-CLASS-TOOL:E-DIGEST throw then
   UCV-FSHA-CTX path pathu DIGEST-HEX SHA256-FILE-HEX-IN 0 <> if UNICODE-CLASS-TOOL:E-DIGEST throw then
   DIGEST-HEX 64 want wantu STR= 0= if UNICODE-CLASS-TOOL:E-DIGEST throw then ;

: CHECK-DIGESTS ( -- )
   UNICODE-PATH$ EXPECTED-UNICODE-DIGEST$ CHECK-DIGEST
   PROPERTIES-PATH$ EXPECTED-PROPERTIES-DIGEST$ CHECK-DIGEST
   BRACKETS-PATH$ EXPECTED-BRACKETS-DIGEST$ CHECK-DIGEST ;

: CHECK-RANGE ( n n n n -- n n ) {: idx:n prev-hi:n lo:n hi:n :}
   lo hi > lo 0 < or hi SCALAR-MAX > or if E-CANONICAL throw then
   lo SURROGATE-LAST <= hi SURROGATE-FIRST >= and if E-CANONICAL throw then
   idx 0 > if lo prev-hi 1+ <= if E-CANONICAL throw then then
   idx 1+ hi ;

: CHECK-TABLE ( ^n n -- ) {: ranges:^ count:n :}
   0 -2
   begin over count < while
      over 2 * cells ranges + {: pair:^ :}
      pair @ pair cell+ @ CHECK-RANGE
   repeat
   2drop ;

: CHECK-TABLES ( -- )
   UNICODE-CLASS-DATA:LETTER-RANGES CHECK-TABLE
   UNICODE-CLASS-DATA:NUMBER-RANGES CHECK-TABLE
   UNICODE-CLASS-DATA:WHITE-SPACE-RANGES CHECK-TABLE
   UNICODE-CLASS-DATA:LETTER-NUMBER-RANGES CHECK-TABLE
   UNICODE-CLASS-DATA:OTHER-ALPHABETIC-RANGES CHECK-TABLE
   UNICODE-CLASS-DATA:UPPERCASE-LETTER-RANGES CHECK-TABLE
   UNICODE-CLASS-DATA:OTHER-UPPERCASE-RANGES CHECK-TABLE
   UNICODE-CLASS-DATA:MARK-RANGES CHECK-TABLE
   UNICODE-CLASS-DATA:MATH-SYMBOL-RANGES CHECK-TABLE
   UNICODE-CLASS-DATA:OTHER-SYMBOL-RANGES CHECK-TABLE ;

: TRUTH-BIT? ( n n -- bool ) {: cp:n bit:n :}
   cp TRUTH @ bit and 0= 0= ;

: SAME-BOOL? ( bool bool -- bool )
   if else 0= then ;

\ Throws E-MISMATCH unless the source truth and the table answer agree.
: AGREE ( bool bool -- ) SAME-BOOL? 0= if E-MISMATCH throw then ;

: CHECK-CLASSES ( n -- ) {: cp:n :}
   cp LETTER-BIT TRUTH-BIT? cp UNICODE-CLASS:LETTER? AGREE
   cp NUMBER-BIT TRUTH-BIT? cp UNICODE-CLASS:NUMBER? AGREE
   cp SPACE-BIT TRUTH-BIT? cp UNICODE-CLASS:WHITE-SPACE? AGREE
   cp ALPHABETIC-BIT TRUTH-BIT? cp UNICODE-CLASS:ALPHABETIC? AGREE
   cp UPPERCASE-BIT TRUTH-BIT? cp UNICODE-CLASS:UPPERCASE? AGREE
   cp MARK-BIT TRUTH-BIT? cp UNICODE-CLASS:MARK? AGREE
   cp MATH-SYMBOL-BIT TRUTH-BIT? cp UNICODE-CLASS:MATH-SYMBOL? AGREE
   cp OTHER-SYMBOL-BIT TRUTH-BIT? cp UNICODE-CLASS:OTHER-SYMBOL? AGREE ;

: PAIR-TRUTH@ ( n -- n ) {: cp:n :}
   cp PAIR-TRUTH @ {: pair:n :}
   pair 0= if cp else pair then ;

: CHECK-BRACKETS ( n -- ) {: cp:n :}
   cp OPEN-BIT TRUTH-BIT? cp UNICODE-CLASS:OPEN-BRACKET? AGREE
   cp CLOSE-BIT TRUTH-BIT? cp UNICODE-CLASS:CLOSE-BRACKET? AGREE
   cp PAIR-TRUTH@ cp UNICODE-CLASS:PAIRED-BRACKET <> if E-MISMATCH throw then ;

: CHECK-SCALAR ( n -- )
   dup CHECK-CLASSES CHECK-BRACKETS ;

: CHECK-NON-SCALAR ( n -- ) {: cp:n :}
   cp UNICODE-CLASS:OPEN-BRACKET? if E-MISMATCH throw then
   cp UNICODE-CLASS:CLOSE-BRACKET? if E-MISMATCH throw then
   cp UNICODE-CLASS:PAIRED-BRACKET cp <> if E-MISMATCH throw then
   cp UNICODE-CLASS:MATH-SYMBOL? if E-MISMATCH throw then
   cp UNICODE-CLASS:OTHER-SYMBOL? if E-MISMATCH throw then ;

: CHECK-ALL-SCALARS ( -- )
   0 begin dup SCALAR-MAX <= while
      dup SURROGATE-FIRST < over SURROGATE-LAST > or
      if dup CHECK-SCALAR else dup CHECK-NON-SCALAR then
      1+
   repeat drop ;

: CHECK-REGENERATION ( -- )
   UNICODE-CLASS-TOOL:LOAD-PINNED
   UNICODE-CLASS-TOOL:RENDER {: generated:^ generatedu:n :}
   DATA-PATH$ 2dup FS:FILE-SIZE 1+ MEM-ALLOC-64K-SPAN {: path:^ pathu:n buf:^ cap:n :}
   path pathu buf cap FS:READ-ALL {: got:n :}
   generated generatedu buf got STR= 0= if E-MISMATCH throw then ;

public

: VERIFY ( -- )
   CHECK-DIGESTS
   SCALAR-COUNT TRUTH-RESERVE
   SCALAR-COUNT PAIR-TRUTH-RESERVE
   LOAD-UNICODE-TRUTH
   LOAD-PROPERTY-TRUTH
   LOAD-BRACKET-TRUTH
   CHECK-TABLES
   CHECK-ALL-SCALARS
   -1 CHECK-NON-SCALAR
   SCALAR-MAX 1+ CHECK-NON-SCALAR
   CHECK-REGENERATION ;

;package
