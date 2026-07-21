\ class-verify.f - independent exhaustive verifier for Unicode class tables.

require lib/prelude.f
require lib/string.f
require lib/memory.f
require lib/fs.f
require lib/unicode/class.f
require tools/unicode/class-generate.f

package UNICODE-CLASS-VERIFY

private

-7806 constant E-MISMATCH
-7807 constant E-CANONICAL

$10FFFF constant SCALAR-MAX
$110000 constant SCALAR-COUNT
$D800 constant SURROGATE-FIRST
$DFFF constant SURROGATE-LAST
1 constant LETTER-BIT
2 constant NUMBER-BIT
4 constant SPACE-BIT
10 constant LINE-FEED
59 constant SEMICOLON
35 constant HASH
46 constant DOT

variable TRUTH
variable VERIFY-PENDING
variable VERIFY-PENDING-LO
variable VERIFY-PENDING-BIT
create DIGEST-HEX 64 allot
create OUTPUT-LOCK 65 allot

: FALSE-VALUE ( -- bool )   0 0= 0= ;
: TRUE-VALUE ( -- bool )    0 0= ;

: UNICODE-PATH$ ( -- ptr u8 n )
   s" data/unicode/16.0.0/UnicodeData.txt" ;

: PROPERTIES-PATH$ ( -- ptr u8 n )
   s" data/unicode/16.0.0/PropList.txt" ;

: DATA-PATH$ ( -- ptr u8 n )
   s" lib/unicode/class-data.f" ;

: OUTPUT-LOCK-PATH$ ( -- ptr u8 n )
   s" data/unicode/16.0.0/class-data.sha256" ;

: EXPECTED-UNICODE-DIGEST$ ( -- ptr u8 n )
   s" ff58e5823bd095166564a006e47d111130813dcf8bf234ef79fa51a870edb48f" ;

: EXPECTED-PROPERTIES-DIGEST$ ( -- ptr u8 n )
   s" 53d614508e2a0b2305a8aa21cd60d993de9326cdf65993660dfcce4503548583" ;

: HEX-DIGIT ( n -- n bool ) {: c:n :}
   c 48 >= c 57 <= and if c 48 - TRUE-VALUE exit then
   c 65 >= c 70 <= and if c 55 - TRUE-VALUE exit then
   c 97 >= c 102 <= and if c 87 - TRUE-VALUE exit then
   0 FALSE-VALUE ;

: HEX-FOLD ( ptr u8 n n n -- n ) {: a:ptr u:n pos:n acc:n :}
   pos u >= if acc exit then
   a pos + c@ HEX-DIGIT 0= if drop E-MISMATCH throw then {: digit:n :}
   a u pos 1+ acc 16 * digit + recurse ;

: PARSE-HEX ( ptr u8 n -- n ) {: a:ptr u:n :}
   u 0= u 6 > or if E-MISMATCH throw then
   a u 0 0 HEX-FOLD ;

: BYTE-INDEX ( ptr u8 n n -- n ) {: a:ptr u:n c:n :}
   0 begin dup u < while
      dup a + c@ c = if exit then
      1+
   repeat ;

: NEXT-FIELD ( ptr u8 n n -- ptr u8 n n ) {: a:ptr u:n start:n :}
   a u SEMICOLON start SPLIT-NEXT {: fa:ptr fu:n next:n valid:bool :}
   valid 0= next u > or if E-MISMATCH throw then
   fa fu next ;

: CATEGORY-SECOND? ( n ptr u8 n -- bool ) {: c:n allowed:ptr allowedu:n :}
   allowed allowedu c BYTE-INDEX allowedu < ;

: CLASS-BIT ( ptr u8 n -- n ) {: a:ptr u:n :}
   u 2 <> if E-MISMATCH throw then
   a c@ 76 = if
      a 1+ c@ s" ultmo" CATEGORY-SECOND? 0= if E-MISMATCH throw then
      LETTER-BIT exit
   then
   a c@ 78 = if
      a 1+ c@ s" dlo" CATEGORY-SECOND? 0= if E-MISMATCH throw then
      NUMBER-BIT exit
   then
   a c@ 77 = if a 1+ c@ s" nce" CATEGORY-SECOND? 0= if E-MISMATCH throw then 0 exit then
   a c@ 80 = if a 1+ c@ s" cdseifo" CATEGORY-SECOND? 0= if E-MISMATCH throw then 0 exit then
   a c@ 83 = if a 1+ c@ s" mcko" CATEGORY-SECOND? 0= if E-MISMATCH throw then 0 exit then
   a c@ 90 = if a 1+ c@ s" slp" CATEGORY-SECOND? 0= if E-MISMATCH throw then 0 exit then
   a c@ 67 = if a 1+ c@ s" cfson" CATEGORY-SECOND? 0= if E-MISMATCH throw then 0 exit then
   E-MISMATCH throw ;

: FIRST-NAME? ( ptr u8 n -- bool )
   s" , First>" ENDS-WITH? ;

: LAST-NAME? ( ptr u8 n -- bool )
   s" , Last>" ENDS-WITH? ;

: SCALAR ( n -- n ) {: cp:n :}
   cp 0 < cp SCALAR-MAX > or if E-MISMATCH throw then
   cp ;

: TRUTH+ ( n n -- ) {: cp:n bit:n :}
   cp SCALAR {: safe:n :}
   TRUTH 0 ptr-field @ safe + dup c@ bit or swap c! ;

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

: VERIFY-UNICODE-ROW ( ptr u8 n -- ) {: line:ptr lineu:n :}
   line lineu TRIM {: row:ptr rowu:n :}
   rowu 0= if exit then
   row rowu SEMICOLON COUNT-CHAR 14 <> if E-MISMATCH throw then
   row rowu 0 NEXT-FIELD {: cpa:ptr cpu:n next1:n :}
   row rowu next1 NEXT-FIELD {: name:ptr nameu:n next2:n :}
   row rowu next2 NEXT-FIELD {: cat:ptr catu:n next3:n :}
   next3 drop
   cpa cpu TRIM PARSE-HEX {: cp:n :}
   cat catu TRIM CLASS-BIT {: bit:n :}
   VERIFY-PENDING @ if
      name nameu LAST-NAME? 0= if E-MISMATCH throw then
      bit VERIFY-PENDING-BIT @ <> if E-MISMATCH throw then
      VERIFY-PENDING-LO @ cp bit MARK-RANGE
      FALSE-VALUE VERIFY-PENDING !
      exit
   then
   name nameu LAST-NAME? if E-MISMATCH throw then
   name nameu FIRST-NAME? if
      cp SCALAR VERIFY-PENDING-LO !
      bit VERIFY-PENDING-BIT !
      TRUE-VALUE VERIFY-PENDING !
      exit
   then
   cp bit TRUTH+ ;

: VERIFY-UNICODE-BYTES ( ptr u8 n -- ) {: a:ptr u:n :}
   FALSE-VALUE VERIFY-PENDING !
   0 begin dup u <= while
      {: start:n :}
      a u LINE-FEED start SPLIT-NEXT {: line:ptr lineu:n next:n valid:bool :}
      valid 0= if E-MISMATCH throw then
      line lineu VERIFY-UNICODE-ROW
      next
   repeat drop
   VERIFY-PENDING @ if E-MISMATCH throw then ;

: RANGE-PART ( ptr u8 n -- n n ) {: a:ptr u:n :}
   a u DOT BYTE-INDEX {: pos:n :}
   pos u = if a u TRIM PARSE-HEX dup exit then
   pos 0= pos 1+ u >= or if E-MISMATCH throw then
   a pos + 1+ c@ DOT <> if E-MISMATCH throw then
   a pos TRIM PARSE-HEX
   a pos 2 + + u pos 2 + - TRIM PARSE-HEX ;

: VERIFY-PROP-ROW ( ptr u8 n -- ) {: line:ptr lineu:n :}
   line lineu HASH BYTE-INDEX {: comment:n :}
   line comment TRIM {: row:ptr rowu:n :}
   rowu 0= if exit then
   row rowu 0 NEXT-FIELD {: range:ptr rangeu:n next:n :}
   row next + rowu next - TRIM s" White_Space" STR= 0= if exit then
   range rangeu TRIM RANGE-PART SPACE-BIT MARK-RANGE ;

: VERIFY-PROPS-BYTES ( ptr u8 n -- ) {: a:ptr u:n :}
   0 begin dup u <= while
      {: start:n :}
      a u LINE-FEED start SPLIT-NEXT {: line:ptr lineu:n next:n valid:bool :}
      valid 0= if E-MISMATCH throw then
      line lineu VERIFY-PROP-ROW
      next
   repeat drop ;

: LOAD-UNICODE-TRUTH ( -- )
   UNICODE-PATH$ 2dup FILE-SIZE 1+ MEM-ALLOC-64K-SPAN {: path:ptr pathu:n buf:ptr cap:n :}
   path pathu buf cap READ-ALL {: got:n :}
   buf got VERIFY-UNICODE-BYTES ;

: LOAD-PROPERTY-TRUTH ( -- )
   PROPERTIES-PATH$ 2dup FILE-SIZE 1+ MEM-ALLOC-64K-SPAN {: path:ptr pathu:n buf:ptr cap:n :}
   path pathu buf cap READ-ALL {: got:n :}
   buf got VERIFY-PROPS-BYTES ;

: CHECK-DIGEST ( ptr u8 n ptr u8 n -- ) {: path:ptr pathu:n want:ptr wantu:n :}
   wantu 64 <> if UNICODE-CLASS-TOOL:E-DIGEST throw then
   path pathu DIGEST-HEX SHA256-FILE-HEX 0 <> if UNICODE-CLASS-TOOL:E-DIGEST throw then
   DIGEST-HEX 64 want wantu STR= 0= if UNICODE-CLASS-TOOL:E-DIGEST throw then ;

: CHECK-DIGESTS ( -- )
   UNICODE-PATH$ EXPECTED-UNICODE-DIGEST$ CHECK-DIGEST
   PROPERTIES-PATH$ EXPECTED-PROPERTIES-DIGEST$ CHECK-DIGEST ;

: CHECK-OUTPUT-LOCK ( -- )
   OUTPUT-LOCK-PATH$ OUTPUT-LOCK 65 READ-ALL 65 <> if UNICODE-CLASS-TOOL:E-DIGEST throw then
   OUTPUT-LOCK 64 + c@ LINE-FEED <> if UNICODE-CLASS-TOOL:E-DIGEST throw then
   DATA-PATH$ OUTPUT-LOCK 64 CHECK-DIGEST ;

: CHECK-RANGE ( n n n n -- n n ) {: idx:n prev-hi:n lo:n hi:n :}
   lo hi > lo 0 < or hi SCALAR-MAX > or if E-CANONICAL throw then
   lo SURROGATE-LAST <= hi SURROGATE-FIRST >= and if E-CANONICAL throw then
   idx 0 > if lo prev-hi 1+ <= if E-CANONICAL throw then then
   idx 1+ hi ;

: CHECK-TABLE ( n [ n -- n n ] -- ) {: count:n getter :} \ typed-local-lint: allow-bare-local - quotation effect
   0 -2
   begin over count < while
      over getter execute CHECK-RANGE
   repeat
   2drop ;

: CHECK-TABLES ( -- )
   UNICODE-CLASS-DATA:LETTER-RANGE-COUNT
   [: UNICODE-CLASS-DATA:LETTER-RANGE@ ;] CHECK-TABLE
   UNICODE-CLASS-DATA:NUMBER-RANGE-COUNT
   [: UNICODE-CLASS-DATA:NUMBER-RANGE@ ;] CHECK-TABLE
   UNICODE-CLASS-DATA:WHITE-SPACE-RANGE-COUNT
   [: UNICODE-CLASS-DATA:WHITE-SPACE-RANGE@ ;] CHECK-TABLE ;

: TRUTH-BIT? ( n n -- bool ) {: cp:n bit:n :}
   TRUTH 0 ptr-field @ cp + c@ bit and 0= 0= ;

: SAME-BOOL? ( bool bool -- bool )
   if else 0= then ;

: CHECK-SCALAR ( n -- ) {: cp:n :}
   cp LETTER-BIT TRUTH-BIT? cp UNICODE-CLASS:LETTER? SAME-BOOL? 0= if E-MISMATCH throw then
   cp NUMBER-BIT TRUTH-BIT? cp UNICODE-CLASS:NUMBER? SAME-BOOL? 0= if E-MISMATCH throw then
   cp SPACE-BIT TRUTH-BIT? cp UNICODE-CLASS:WHITE-SPACE? SAME-BOOL? 0= if E-MISMATCH throw then ;

: CHECK-ALL-SCALARS ( -- )
   0 begin dup SCALAR-MAX <= while
      dup SURROGATE-FIRST < over SURROGATE-LAST > or if dup CHECK-SCALAR then
      1+
   repeat drop ;

: CHECK-REGENERATION ( -- )
   UNICODE-CLASS-TOOL:LOAD-PINNED
   UNICODE-CLASS-TOOL:RENDER {: generated:ptr generatedu:n :}
   DATA-PATH$ 2dup FILE-SIZE 1+ MEM-ALLOC-64K-SPAN {: path:ptr pathu:n buf:ptr cap:n :}
   path pathu buf cap READ-ALL {: got:n :}
   generated generatedu buf got STR= 0= if E-MISMATCH throw then ;

public

: VERIFY ( -- )
   CHECK-DIGESTS
   CHECK-OUTPUT-LOCK
   SCALAR-COUNT MEM-ALLOC-BYTES drop TRUTH 0 ptr-field !
   LOAD-UNICODE-TRUTH
   LOAD-PROPERTY-TRUTH
   CHECK-TABLES
   CHECK-ALL-SCALARS
   CHECK-REGENERATION ;

;package
