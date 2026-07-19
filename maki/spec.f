\ maki/spec.f - SPEC:, the einsum-like golden-authoring surface (docs/golden-syntax.md
\ candidate C). One schematic line is parsed by a defining word that derives three
\ things from the SAME parsed structure:
\
\   SPEC: GGEMM  O[m n] = A[ IX[m] k ] B[n k] * +SUM k ;
\
\   (1) the checked candidate-B golden code - two generated words, GGEMM-EL (the
\       inner contraction element) and GGEMM (the outer free loops + store),
\       written against the EXTENT:/TENSOR:/ITENSOR:/ix<extent> machinery
\       (maki/extent.f + maki/extent-tensor.f) and certified through the same
\       XG-EVAL checked-codegen boundary. So the golden is CHECKED, not trusted.
\   (2) the planner dataflow record - the free (output) index variables, the
\       contracted index variables, and per input tensor its index structure
\       including any gather. Exposed as SPEC-* query words (see bottom).
\   (3) the PROMOTE shape obligations - the output shape (free extents) and the
\       contraction extents. Exposed as SPEC-*-EXTENT@ query words.
\
\ SURFACE (design decisions, alternatives recorded in the dot report):
\   - A token before `[` is a TENSOR/GATHER name matched EXACTLY against the
\     TENSOR:/ITENSOR: declarations (case-sensitive, upper-case per convention).
\     SPEC: appends `@` (read a factor / gather) or `!` (write the output).
\   - A bare token inside `[...]` is an INDEX VARIABLE (lower-case, math style);
\     its extent is `#` + the upper-cased variable (`m` -> `#M`), and its crossing
\     is `>#M`. This mirrors the math: index `m` lives in dimension `M`.
\   - A gather is `NAME[var]` nested inside a factor's bracket (`A[ IX[m] k ]`).
\   - The product combiner (multiply all factors) and the reduction (sum-contract over
\     the index list) each have an ASCII spelling that stays legal forever plus a
\     normalized Unicode spelling. Product: `*` or `·` (U+00B7 MIDDLE DOT / U+22C5 DOT
\     OPERATOR). Summation: the trailing `+SUM <index>` or the prefix `Σ<index>` (U+03A3
\     GREEK CAPITAL SIGMA / U+2211 N-ARY SUMMATION). The two lookalikes in each pair lex
\     to ONE token, so identical-looking source is identical to the lexer; any OTHER
\     non-ASCII byte is a named E-SPEC-SYNTAX reject that prints the offending codepoint.
\     docs/golden-syntax.md fixes the canonical pretty form.
\
\ WHAT SPEC: EXPRESSES vs WHAT STILL NEEDS HAND-WRITTEN BODIES: SPEC: covers the
\ gathered-GEMM family - up to 2 free (output) indices and up to 2 contraction
\ indices (the habu i/j loop-counter limit, since a free index and a contraction
\ index each need a loop level), any number of product factors, and one gather per
\ factor index. Beyond that arity, or any op the multiply-then-sum schematic cannot
\ state (nonlinearities, softmax, movement ops), stays a hand-written candidate-B
\ body. The generated loop counter is NOT extent-typed (tracked by
\ habu-extent-bound-loop-a70a49b3), so generated bodies use the explicit `i >#EXT`
\ crossing exactly as the hand-written GGEMM in maki/extent-tensor-test.f does.
\
\ FAIL CLOSED: every malformed spec is a named throw before any code is generated -
\ unknown extent (E-SPEC-EXTENT), an index bound to neither the output nor the
\ contraction (E-SPEC-UNBOUND), an undeclared tensor or a gather where a data
\ tensor is required (E-SPEC-TENSOR), a factor whose index count does not match the
\ tensor's rank or an unsupported free/contraction arity (E-SPEC-ARITY), and any
\ grammar violation (E-SPEC-SYNTAX). An extent-flipping spec is caught one layer
\ deeper: the generated accessor call is rejected by the checker at XG-EVAL time
\ (the candidate-B flip protection). maki -> habu only; maki owns -5019, -5036..-5039.

require maki/extent-tensor.f
require lib/string.f                 \ STR=, ASCII-UPPER: token compares + index-var -> extent-name fold
require lib/adt/option.f             \ option<>: index-variable and tensor lookups return present/absent
require lib/type/deftype.f           \ DEFTYPE: the factor-index (SP-FI) table index is its own type

-5036 constant E-SPEC-SYNTAX    \ malformed spec grammar (brackets, =, keywords, terminator)
-5037 constant E-SPEC-EXTENT    \ an index variable's extent #<UPPER> is not declared
-5038 constant E-SPEC-UNBOUND   \ a factor index is neither a free (output) nor a contraction index
-5039 constant E-SPEC-TENSOR    \ a tensor/gather name is not declared, or has the wrong kind
-5019 constant E-SPEC-ARITY     \ factor index count != tensor rank, or free/contraction arity > 2

package MAKI

private

\ ---- names of the current spec (stable across the whole derivation) ----------
create SP-NAME-BUF 32 allot   variable SP-NAME-U
create SP-OUT-BUF  32 allot   variable SP-OUT-U
: SP-NAME! ( ptr u8 n -- ) {: a:ptr u:n :}  u 32 > if E-SPEC-SYNTAX throw then  a SP-NAME-BUF u BYTE-COPY u SP-NAME-U ! ;
: SP-OUT!  ( ptr u8 n -- ) {: a:ptr u:n :}  u 32 > if E-SPEC-SYNTAX throw then  a SP-OUT-BUF  u BYTE-COPY u SP-OUT-U ! ;

public
: SPEC-NAME$ ( -- ptr u8 n )  SP-NAME-BUF SP-NAME-U @ ;
: SPEC-OUT$  ( -- ptr u8 n )  SP-OUT-BUF  SP-OUT-U @ ;
private

\ ---- parsed index-variable lists (free = output indices; ct = contraction) ----
16 constant SP-IDX-CAP           \ max variables in a list
8  constant SP-IDX-NAME          \ max index-variable name bytes
create SP-FREE   SP-IDX-CAP SP-IDX-NAME * allot
create SP-FREE-L SP-IDX-CAP cells allot
variable SP-FREE-N
create SP-CT     SP-IDX-CAP SP-IDX-NAME * allot
create SP-CT-L   SP-IDX-CAP cells allot
variable SP-CT-N

: SP-IDX-SLOT ( ptr a n -- ptr a )  SP-IDX-NAME *  + ;
: SP-LIST+ ( ptr u8 n ptr a ptr a ptr a -- ) {: a:ptr u:n base:ptr lb:ptr cv:ptr :}
   u SP-IDX-NAME > if E-SPEC-SYNTAX throw then
   cv @ SP-IDX-CAP >= if E-SPEC-ARITY throw then
   a  base cv @ SP-IDX-SLOT  u BYTE-COPY
   u  cv @ cells lb + !
   cv @ 1 + cv ! ;
: SP-FREE+ ( ptr u8 n -- )  SP-FREE SP-FREE-L SP-FREE-N SP-LIST+ ;
: SP-CT+   ( ptr u8 n -- )  SP-CT   SP-CT-L   SP-CT-N   SP-LIST+ ;

public
: SPEC-FREE-N ( -- n )  SP-FREE-N @ ;
: SPEC-CT-N   ( -- n )  SP-CT-N @ ;
: SPEC-FREE@ ( n -- ptr u8 n ) {: i:n :}  SP-FREE i SP-IDX-SLOT  i cells SP-FREE-L + @ ;
: SPEC-CT@   ( n -- ptr u8 n ) {: i:n :}  SP-CT   i SP-IDX-SLOT  i cells SP-CT-L   + @ ;
private

\ ---- parsed factors: each = tensor name + a window into the factor-index list.
\ A factor index is (var, gather-tensor-or-empty): B[n k] has plain indices n,k;
\ A[ IX[m] k ] has a gather index (var=m, gather=IX) then a plain index k. --------
8  constant SP-FAC-CAP
32 constant SP-TNAME
create SP-FAC-T   SP-FAC-CAP SP-TNAME * allot
create SP-FAC-TL  SP-FAC-CAP cells allot
create SP-FAC-OFF SP-FAC-CAP cells allot
create SP-FAC-CNT SP-FAC-CAP cells allot
variable SP-FAC-N
32 constant SP-FI-CAP
create SP-FI-VAR   SP-FI-CAP SP-IDX-NAME * allot
create SP-FI-VARL  SP-FI-CAP cells allot
create SP-FI-GATH  SP-FI-CAP SP-TNAME * allot
create SP-FI-GATHL SP-FI-CAP cells allot
variable SP-FI-N

\ sp-fi is the row index into the factor-index (SP-FI-*) parallel arrays: its own
\ type, so a factor number or a raw counter cannot index the table without the
\ explicit `>SP-FI` crossing. The SP-FAC offset column is the base row of a factor.
public
DEFTYPE SP-FI
private

: SP-FAC-T-SLOT   ( n -- ptr a )      SP-TNAME *  SP-FAC-T + ;   \ n = factor number
: SP-FI-VAR-SLOT  ( sp-fi -- ptr a )  SP-FI>N SP-IDX-NAME * SP-FI-VAR + ;
: SP-FI-GATH-SLOT ( sp-fi -- ptr a )  SP-FI>N SP-TNAME * SP-FI-GATH + ;
: SP-FAC-OFF@     ( n -- sp-fi )      cells SP-FAC-OFF + @ >SP-FI ;   \ a factor's base SP-FI row

public
: SP-FI-VAR@  ( sp-fi -- ptr u8 n ) {: fi:sp-fi :}  fi SP-FI-VAR-SLOT   fi SP-FI>N cells SP-FI-VARL + @ ;
: SP-FI-GATH@ ( sp-fi -- ptr u8 n ) {: fi:sp-fi :}  fi SP-FI-GATH-SLOT  fi SP-FI>N cells SP-FI-GATHL + @ ;
private

: SP-FI+ ( ptr u8 n ptr u8 n -- ) {: va:ptr vu:n ga:ptr gu:n :}
   SP-FI-N @ SP-FI-CAP >= if E-SPEC-ARITY throw then
   vu SP-IDX-NAME > gu SP-TNAME > or if E-SPEC-SYNTAX throw then
   SP-FI-N @ >SP-FI {: fi:sp-fi :}
   va fi SP-FI-VAR-SLOT vu BYTE-COPY   vu fi SP-FI>N cells SP-FI-VARL + !
   ga fi SP-FI-GATH-SLOT gu BYTE-COPY  gu fi SP-FI>N cells SP-FI-GATHL + !
   SP-FI-N @ 1 + SP-FI-N ! ;
: SP-FAC+ ( ptr u8 n n n -- ) {: na:ptr nu:n off:n cnt:n :}
   SP-FAC-N @ SP-FAC-CAP >= if E-SPEC-ARITY throw then
   nu SP-TNAME > if E-SPEC-SYNTAX throw then
   na SP-FAC-N @ SP-FAC-T-SLOT nu BYTE-COPY  nu SP-FAC-N @ cells SP-FAC-TL + !
   off SP-FAC-N @ cells SP-FAC-OFF + !  cnt SP-FAC-N @ cells SP-FAC-CNT + !
   SP-FAC-N @ 1 + SP-FAC-N ! ;

public
: SPEC-FAC-N ( -- n )  SP-FAC-N @ ;
: SPEC-FAC-NAME@ ( n -- ptr u8 n ) {: i:n :}  i SP-FAC-T-SLOT  i cells SP-FAC-TL + @ ;
: SPEC-FAC-RANK@ ( n -- n ) cells SP-FAC-CNT + @ ;
: SPEC-FAC-IDX@ ( n n -- ptr u8 n ) {: f:n k:n :}   \ k-th factor index's variable
   f SP-FAC-OFF@ SP-FI>N k + >SP-FI SP-FI-VAR@ ;
: SPEC-FAC-GATHER@ ( n n -- ptr u8 n ) {: f:n k:n :}  \ k-th factor index's gather ("" if none)
   f SP-FAC-OFF@ SP-FI>N k + >SP-FI SP-FI-GATH@ ;
private

\ ---- lexer over the collected spec body: whitespace separates, and `[ ] = *`
\ are single-character tokens (so `O[m` lexes to `O` `[` `m`). ------------------
$400 constant SP-SRC-CAP
create SP-SRC SP-SRC-CAP allot
variable SP-SRC-U
variable SP-POS
variable SP-PB-A  variable SP-PB-U  variable SP-PB?

\ ---- Unicode math spellings (dot habu-unicode-math-spellings): the confusable set is
\ normalized so identical-looking codepoints lex to ONE token. U+03A3 GREEK CAPITAL
\ SIGMA and U+2211 N-ARY SUMMATION both lex to the summation token `+SUM`; U+00B7 MIDDLE
\ DOT and U+22C5 DOT OPERATOR both lex to the product token `*`. The ASCII spellings stay
\ legal forever. Any OTHER non-ASCII byte is a named E-SPEC-SYNTAX reject whose diagnostic
\ prints the offending codepoint. Only these four codepoints are decoded - no general
\ Unicode tables. ---------------------------------------------------------------------
$03A3 constant CP-SIGMA        \ GREEK CAPITAL SIGMA
$2211 constant CP-NARY-SUM     \ N-ARY SUMMATION
$00B7 constant CP-MIDDLE-DOT   \ MIDDLE DOT
$22C5 constant CP-DOT-OP       \ DOT OPERATOR
create SP-SUM-TOK  43 c, 83 c, 85 c, 77 c,   \ canonical summation token bytes "+SUM"
create SP-STAR-TOK 42 c,                     \ canonical product token byte "*"
: SP-SUM$  ( -- ptr u8 n )  SP-SUM-TOK  4 ;
: SP-STAR$ ( -- ptr u8 n )  SP-STAR-TOK 1 ;
: SP-HI? ( n -- bool )  127 > ;   \ a byte with the high bit set starts a non-ASCII (UTF-8) sequence

\ the low 6 bits of the continuation byte at SP-POS+off (caller ensured the offset is in-bounds).
: SP-U8@ ( n -- n ) {: off:n :}  SP-SRC SP-POS @ + off + c@  $3F and ;
\ byte-length of a UTF-8 sequence from its lead byte; 1 for a byte that is not a multi-byte lead.
: SP-LEAD-LEN ( n -- n )
   dup $E0 and $C0 = if drop 2 exit then
   dup $F0 and $E0 = if drop 3 exit then
   dup $F8 and $F0 = if drop 4 exit then
   drop 1 ;
\ decode the UTF-8 sequence at SP-POS to ( codepoint byte-length ). A byte that is not a
\ valid lead, or a sequence truncated by the buffer end, yields ( lead-byte 1 ) so the
\ reject path can still name the offending byte.
: SP-DECODE ( -- n n )
   SP-SRC SP-POS @ + c@  dup SP-LEAD-LEN  {: b0:n len:n :}
   len 1 = if b0 1 exit then
   SP-POS @ len + SP-SRC-U @ > if b0 1 exit then
   len 2 = if  b0 $1F and 6 lshift   1 SP-U8@ or                             2 exit then
   len 3 = if  b0 $0F and 12 lshift  1 SP-U8@ 6 lshift or   2 SP-U8@ or      3 exit then
   b0 $07 and 18 lshift  1 SP-U8@ 12 lshift or  2 SP-U8@ 6 lshift or  3 SP-U8@ or  4 ;

\ the reject diagnostic: a spec-owned buffer holding the message for the most recent
\ non-ASCII reject, so it is queryable in-process (SPEC-REJECT$) with no dependence on
\ the checker's engine-internal sink. Populated just before the E-SPEC-SYNTAX throw.
public
256 constant SP-RJ-CAP
create SP-RJ-BUF SP-RJ-CAP allot
variable SP-RJ-U
: SPEC-REJECT$ ( -- ptr u8 n )  SP-RJ-BUF SP-RJ-U @ ;
private
: SP-RJ-C ( n -- ) {: c:n :}
   SP-RJ-U @ SP-RJ-CAP >= if E-SPEC-SYNTAX throw then
   c SP-RJ-BUF SP-RJ-U @ + c!  SP-RJ-U @ 1 + SP-RJ-U ! ;
: SP-RJ+ ( ptr u8 n -- ) {: a:ptr u:n :}  0 begin dup u < while dup a + c@ SP-RJ-C 1 + repeat drop ;
: SP-HEX-NIB ( n -- )   \ append one uppercase hex digit to the reject diagnostic
   dup 10 < if [char] 0 + else 10 - [char] A + then  SP-RJ-C ;
: SP-HEX ( n n -- ) {: cp:n w:n :}   \ append cp in hex, at least width (w) digits, high-to-low
   cp 16 >= w 1 > or if  cp 16 /  w 1 -  RECURSE  then
   cp 15 and SP-HEX-NIB ;
: SP-DIAG-CP ( n -- ) {: cp:n :}   \ record "...U+<hex>..." naming the offending codepoint (no throw)
   0 SP-RJ-U !
   s" spec: non-ASCII codepoint U+" SP-RJ+
   cp 4 SP-HEX
   s"  is not a legal equation token" SP-RJ+ ;
\ decode the non-ASCII sequence at SP-POS: a confusable-set member returns its canonical
\ token; anything else names its codepoint and rejects the equation.
: SP-UNI-TOK ( -- ptr u8 n )
   SP-DECODE {: cp:n len:n :}
   SP-POS @ len + SP-POS !
   cp CP-SIGMA = cp CP-NARY-SUM = or if SP-SUM$ exit then
   cp CP-MIDDLE-DOT = cp CP-DOT-OP = or if SP-STAR$ exit then
   cp SP-DIAG-CP  E-SPEC-SYNTAX throw ;

: SP-DELIM? ( n -- bool ) {: c:n :}  c 91 = c 93 = or c 61 = or c 42 = or ;   \ [ ] = *
: SP-WS?    ( n -- bool ) {: c:n :}  c 32 = c 9 = or c 10 = or c 13 = or ;
: SP-SRC-C ( n -- ) {: c:n :}
   SP-SRC-U @ SP-SRC-CAP >= if E-EXT-CAP throw then
   c SP-SRC SP-SRC-U @ + c!  SP-SRC-U @ 1 + SP-SRC-U ! ;
: SP-SRC+ ( ptr u8 n -- ) {: a:ptr u:n :}  0 begin dup u < while dup a + c@ SP-SRC-C 1 + repeat drop ;

\ set SP-SRC directly from a string (string-driven checks, no stream parse).
: SP-LOAD$ ( ptr u8 n -- ) {: a:ptr u:n :}
   u SP-SRC-CAP > if E-EXT-CAP throw then
   a SP-SRC u BYTE-COPY  u SP-SRC-U ! ;

\ collect the spec body (after the name) up to a standalone `;` into SP-SRC.
: SP-COLLECT ( -- )
   0 SP-SRC-U !
   begin
      parse-name dup 0= if 2drop E-SPEC-SYNTAX throw then
      2dup s" ;" STR= if 2drop exit then
      SP-SRC+  32 SP-SRC-C
   again ;

: SP-AT-WORD? ( -- bool )
   SP-POS @ SP-SRC-U @ >= if false exit then
   SP-SRC SP-POS @ + c@ {: c:n :}  c SP-WS? 0=  c SP-DELIM? 0= and  c SP-HI? 0= and ;
: SP-SKIP-WS ( -- )
   begin
      SP-POS @ SP-SRC-U @ >= if exit then
      SP-SRC SP-POS @ + c@ SP-WS? 0= if exit then
      SP-POS @ 1 + SP-POS !
   again ;
: SP-NEXT ( -- ptr u8 n )   \ next token; ( ptr 0 ) at end
   SP-PB? @ if 0 SP-PB? ! SP-PB-A @ SP-PB-U @ exit then
   SP-SKIP-WS
   SP-POS @ SP-SRC-U @ >= if SP-SRC 0 exit then
   SP-SRC SP-POS @ + c@ SP-HI? if SP-UNI-TOK exit then   \ non-ASCII: normalize the math operator or reject
   SP-SRC SP-POS @ + {: a:ptr :}
   a c@ SP-DELIM? if SP-POS @ 1 + SP-POS ! a 1 exit then
   SP-POS @ {: start:n :}
   begin SP-AT-WORD? while SP-POS @ 1 + SP-POS ! repeat
   a  SP-POS @ start - ;
: SP-PUSH ( ptr u8 n -- )  SP-PB-U !  SP-PB-A !  -1 SP-PB? ! ;
: SP-PEEK ( -- ptr u8 n )  SP-NEXT 2dup SP-PUSH ;

: SP-DELIM-TOK? ( ptr u8 n -- bool ) {: a:ptr u:n :}  u 1 =  a c@ SP-DELIM? and ;
: SP-EXPECT ( ptr u8 n -- ) {: a:ptr u:n :}
   SP-NEXT a u STR= 0= if E-SPEC-SYNTAX throw then ;
: SP-WORD ( -- ptr u8 n )   \ next token must be a word (not delimiter, not end)
   SP-NEXT dup 0= if 2drop E-SPEC-SYNTAX throw then
   2dup SP-DELIM-TOK? if E-SPEC-SYNTAX throw then ;
: SP-EMPTY ( -- ptr u8 n )  SP-SRC 0 ;

\ ---- recursive-descent parser: OUT [free] = factor... [* ] +SUM ct... ---------
: SP-PARSE-FREE ( -- )   \ index vars until ]
   begin
      SP-PEEK s" ]" STR= if exit then
      SP-WORD SP-FREE+
   again ;
: SP-PARSE-FACTOR-INDEX ( -- )   \ a plain var, or a gather NAME[var]
   SP-WORD {: wa:ptr wu:n :}
   SP-PEEK s" [" STR= if
      SP-NEXT 2drop                       \ consume [
      SP-WORD {: ia:ptr iu:n :}           \ inner index var
      s" ]" SP-EXPECT
      ia iu  wa wu  SP-FI+                \ index var = inner, gather = NAME
   else
      wa wu  SP-EMPTY  SP-FI+             \ plain var, no gather
   then ;
: SP-PARSE-FACTOR-NAMED ( ptr u8 n -- )   \ [ index... ] for a factor whose NAME is already read
   {: na:ptr nu:n :}
   s" [" SP-EXPECT
   SP-FI-N @ {: off:n :}
   begin
      SP-PEEK s" ]" STR= if
         SP-NEXT 2drop
         na nu  off  SP-FI-N @ off -  SP-FAC+
         exit
      then
      SP-PARSE-FACTOR-INDEX
   again ;
: SP-PARSE-FACTOR ( -- )   \ NAME [ index... ]
   SP-WORD SP-PARSE-FACTOR-NAMED ;
variable SP-STAR?     \ a product token (* or ·) appeared among/after the factors
variable SP-PREFIX?   \ the summation was written as a prefix Σ, not a trailing +SUM
: SP-PARSE-FACTORS ( -- )   \ factors, separated/terminated by optional product tokens; stops at +SUM or end
   begin
      SP-PEEK s" *" STR= if SP-NEXT 2drop  -1 SP-STAR? ! then
      SP-PEEK dup 0= if 2drop exit then
      2dup s" +SUM" STR= if 2drop exit then
      2drop  SP-PARSE-FACTOR
   again ;
: SP-PARSE-SUM ( -- )   \ '+SUM' peeked; consume it + contraction vars to end
   SP-NEXT 2drop
   begin
      SP-PEEK dup 0= if 2drop exit then
      2drop  SP-WORD SP-CT+
   again ;
\ prefix summation: after the Σ token, the contraction index list runs until the first
\ factor (a word followed by `[`). That first factor is parsed here; SP-PARSE-FACTORS
\ parses any that follow.
: SP-PARSE-PREFIX-CT ( -- )
   begin
      SP-WORD {: wa:ptr wu:n :}
      SP-PEEK s" [" STR= if wa wu SP-PARSE-FACTOR-NAMED exit then
      wa wu SP-CT+
   again ;
: SP-PARSE-REDUCTION ( -- )   \ exactly one reduction: a trailing +SUM, unless a prefix Σ already gave it
   SP-PEEK s" +SUM" STR= if
      SP-PREFIX? @ if E-SPEC-SYNTAX throw then
      SP-PARSE-SUM exit
   then
   SP-PREFIX? @ 0= if E-SPEC-SYNTAX throw then ;
: SP-PARSE ( -- )
   0 SP-FREE-N !  0 SP-CT-N !  0 SP-FAC-N !  0 SP-FI-N !
   0 SP-STAR? !  0 SP-PREFIX? !
   0 SP-POS !  0 SP-PB? !
   SP-WORD SP-OUT!
   s" [" SP-EXPECT   SP-PARSE-FREE   s" ]" SP-EXPECT
   s" =" SP-EXPECT
   SP-PEEK s" +SUM" STR= if
      SP-NEXT 2drop  -1 SP-PREFIX? !  SP-PARSE-PREFIX-CT
   then
   SP-PARSE-FACTORS
   SP-PARSE-REDUCTION
   SP-NEXT dup 0= 0= if 2drop E-SPEC-SYNTAX throw then 2drop ;

\ ---- index-variable -> extent surface (#<UPPER>) and role resolution ----------
create SP-EXT-BUF 16 allot
variable SP-EXT-U
: SP-EXT$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}   \ index var -> #<UPPER> surface name
   0 SP-EXT-U !
   [char] # SP-EXT-BUF c!  1 SP-EXT-U !
   u 0 ?do  a i + c@ ASCII-UPPER  SP-EXT-BUF SP-EXT-U @ + c!  SP-EXT-U @ 1 + SP-EXT-U !  loop
   SP-EXT-BUF SP-EXT-U @ ;
: SP-EXT-SLOT ( ptr u8 n -- xr-slot )   \ registry slot of the var's extent, or E-SPEC-EXTENT
   SP-EXT$ XR-FIND MATCH option
      none OF E-SPEC-EXTENT throw ENDOF
      some OF ENDOF
   ;MATCH ;
\ position of a variable in the free / contraction index list, or none.
: SP-FREE-POS ( ptr u8 n -- option<n> ) {: a:ptr u:n :}
   SP-FREE-N @ 0 ?do  a u i SPEC-FREE@ STR= if i OPTION:SOME unloop exit then loop  OPTION:NONE ;
: SP-CT-POS ( ptr u8 n -- option<n> ) {: a:ptr u:n :}
   SP-CT-N @ 0 ?do  a u i SPEC-CT@ STR= if i OPTION:SOME unloop exit then loop  OPTION:NONE ;
: SP-FREE? ( ptr u8 n -- bool )  SP-FREE-POS MATCH option  none OF false ENDOF  some OF drop true ENDOF ;MATCH ;
: SP-CT?   ( ptr u8 n -- bool )  SP-CT-POS   MATCH option  none OF false ENDOF  some OF drop true ENDOF ;MATCH ;

\ ---- code emitters (append candidate-B source into the XG buffer) -------------
\ a nested loop position (0-outer..count-1-inner) -> the habu loop counter i/j.
: SP-EMIT-COUNTER ( n n -- ) {: pos:n cnt:n :}
   cnt 1 - pos - {: d:n :}
   d 0 = if s" i " XG+ exit then
   d 1 = if s" j " XG+ exit then
   E-SPEC-ARITY throw ;
\ the runtime source of one index var: a free index is a projected local f<pos>,
\ a contraction index is the loop counter for its nesting depth.
\ a free index is a projected local f<pos>; a contraction index is the loop counter
\ for its nesting depth (SP-EMIT-COUNTER reads the contraction position).
: SP-EMIT-FREE-SRC ( n -- ) {: pos:n :}  s" f" XG+ pos XG-INT s"  " XG+ ;
: SP-EMIT-CT-SRC ( ptr u8 n -- ) {: a:ptr u:n :}
   a u SP-CT-POS MATCH option
      none OF E-SPEC-UNBOUND throw ENDOF
      some OF SP-CT-N @ SP-EMIT-COUNTER ENDOF
   ;MATCH ;
: SP-EMIT-IDX-SRC ( ptr u8 n -- ) {: a:ptr u:n :}
   a u SP-FREE-POS MATCH option
      none OF a u SP-EMIT-CT-SRC ENDOF
      some OF SP-EMIT-FREE-SRC ENDOF
   ;MATCH ;
: SP-EMIT-INJ ( ptr u8 n -- )  s" >" XG+  SP-EXT$ XG+  s"  " XG+ ;   \ >#<UPPER>
\ one factor index: <source> >#<ext> [<gather>@]
: SP-EMIT-FACTOR-IDX ( n n -- ) {: f:n k:n :}
   f k SPEC-FAC-IDX@ {: va:ptr vu:n :}
   va vu SP-EMIT-IDX-SRC
   va vu SP-EMIT-INJ
   f k SPEC-FAC-GATHER@ {: ga:ptr gu:n :}
   gu 0 > if ga gu XG+ s" @ " XG+ then ;
: SP-EMIT-FACTOR ( n -- ) {: f:n :}   \ one factor: its indices then <tensor>@
   f SPEC-FAC-RANK@ 0 ?do  f i SP-EMIT-FACTOR-IDX  loop
   f SPEC-FAC-NAME@ XG+  s" @ " XG+ ;
: SP-EMIT-EL-SIG ( -- )   \ ( ix<free0-tail> ix<free1-tail> ... )
   SP-FREE-N @ 0 ?do
      s" ix<" XG+  i SPEC-FREE@ SP-EXT-SLOT XR-TAIL@ XG+  s" > " XG+
   loop ;
: SP-EMIT-PROJ ( -- )   \ project free indices to f0..f{F-1}; stack top is the last
   SP-FREE-N @ 0 ?do
      SP-FREE-N @ 1 - i -  {: k:n :}
      s" IX>N {: f" XG+  k XG-INT  s" :n :} " XG+
   loop ;
: SP-EMIT-CT-OPEN ( -- )
   SP-CT-N @ 0 ?do  i SPEC-CT@ SP-EXT$ XG+  s"  0 ?do " XG+  loop ;
: SP-EMIT-CT-CLOSE ( -- )
   SP-CT-N @ 0 ?do  s" loop " XG+  loop ;
\ the inner contraction element as a CANDIDATE (NAME-EL ( sig -- r ) body, no `:`/`;`
\ wrapper) so it can be either compiled (SP-EMIT-EL) or checker-scored (SPEC-CAND:).
\ Free indices are ix-typed args projected to plain locals; the contraction extent(s)
\ drive the accumulation loop; the factor product accumulates. The i>#EXT crossing is
\ explicit (habu-extent-bound-loop-a70a49b3).
: SP-EL-CORE ( -- )
   SPEC-NAME$ XG+  s" -EL ( " XG+  SP-EMIT-EL-SIG  s" -- r ) " XG+
   SP-EMIT-PROJ
   s" 0.0 " XG+
   SP-EMIT-CT-OPEN
   SP-FAC-N @ 0 ?do  i SP-EMIT-FACTOR  loop
   SP-FAC-N @ 1 - 0 ?do  s" f* " XG+  loop
   s" f+ " XG+
   SP-EMIT-CT-CLOSE ;
: SP-EMIT-EL ( -- )   \ compile the element word: : NAME-EL ( sig -- r ) body ;
   XG-RESET  s" : " XG+  SP-EL-CORE  s" ; " XG+  XG-EVAL ;
: SP-EMIT-FREE-INJECT ( -- )   \ <counter> >#<free-ext> per free index
   SP-FREE-N @ 0 ?do
      i SP-FREE-N @ SP-EMIT-COUNTER
      i SPEC-FREE@ SP-EMIT-INJ
   loop ;
\ GGEMM: the outer free loops. For each free-index tuple, call <NAME>-EL and store
\ the result into the output tensor.
: SP-EMIT-OUTER ( -- )
   XG-RESET
   s" : " XG+  SPEC-NAME$ XG+  s"  ( -- ) " XG+
   SP-FREE-N @ 0 ?do  i SPEC-FREE@ SP-EXT$ XG+  s"  0 ?do " XG+  loop
   SP-EMIT-FREE-INJECT  SPEC-NAME$ XG+  s" -EL " XG+
   SP-EMIT-FREE-INJECT  SPEC-OUT$ XG+   s" ! " XG+
   SP-FREE-N @ 0 ?do  s" loop " XG+  loop
   s" ; " XG+
   XG-EVAL ;

\ ---- semantic validation: every malformed spec is a named throw BEFORE any code
\ is generated (no partial word definitions on a bad spec). --------------------
: SP-TENSOR-SLOT ( ptr u8 n -- tr-slot )   \ registry slot of a tensor/gather, or E-SPEC-TENSOR
   TR-FIND MATCH option
      none OF E-SPEC-TENSOR throw ENDOF
      some OF ENDOF
   ;MATCH ;
: SP-CHK-RANK ( tr-slot n -- ) {: slot:tr-slot rk:n :}
   slot TR-RANK@ rk <> if E-SPEC-ARITY throw then ;
: SP-CHK-DATA ( ptr u8 n n -- ) {: a:ptr u:n rk:n :}   \ require a declared data tensor of rank rk
   a u SP-TENSOR-SLOT {: slot:tr-slot :}
   slot TR-KIND@ TR-KIND-DATA? 0= if E-SPEC-TENSOR throw then
   slot rk SP-CHK-RANK ;
: SP-CHK-GATHER ( ptr u8 n n -- ) {: a:ptr u:n rk:n :}  \ require a declared gather of rank rk
   a u SP-TENSOR-SLOT {: slot:tr-slot :}
   slot TR-KIND@ TR-KIND-GATHER? 0= if E-SPEC-TENSOR throw then
   slot rk SP-CHK-RANK ;
: SP-CHK-VAR-EXT ( ptr u8 n -- )  SP-EXT-SLOT drop ;
: SP-CHK-BOUND ( ptr u8 n -- ) {: a:ptr u:n :}
   a u SP-FREE? if exit then
   a u SP-CT? if exit then
   E-SPEC-UNBOUND throw ;
: SP-CHK-FACTOR-IDX ( n n -- ) {: f:n k:n :}
   f k SPEC-FAC-IDX@ {: va:ptr vu:n :}
   va vu SP-CHK-VAR-EXT
   va vu SP-CHK-BOUND
   f k SPEC-FAC-GATHER@ {: ga:ptr gu:n :}
   gu 0 > if  ga gu 1 SP-CHK-GATHER  then ;   \ a gather is a rank-1 ITENSOR
: SP-CHK-FACTOR ( n -- ) {: f:n :}
   f SPEC-FAC-NAME@  f SPEC-FAC-RANK@  SP-CHK-DATA
   f SPEC-FAC-RANK@ 0 ?do  f i SP-CHK-FACTOR-IDX  loop ;
: SP-VALIDATE ( -- )
   SP-FREE-N @ 1 < SP-FREE-N @ 2 > or if E-SPEC-ARITY throw then   \ 1..2 free indices
   SP-CT-N @ 1 < SP-CT-N @ 2 > or if E-SPEC-ARITY throw then       \ 1..2 contraction indices
   SP-FAC-N @ 0= if E-SPEC-SYNTAX throw then
   SP-FAC-N @ 1 > SP-STAR? @ 0= and if E-SPEC-SYNTAX throw then    \ >1 factor requires *
   SP-FAC-N @ 1 = SP-STAR? @ and    if E-SPEC-SYNTAX throw then    \ 1 factor forbids *
   SPEC-OUT$ SP-FREE-N @ SP-CHK-DATA                              \ output: data tensor, rank == free count
   SP-FREE-N @ 0 ?do  i SPEC-FREE@ SP-CHK-VAR-EXT  loop
   SP-CT-N @ 0 ?do  i SPEC-CT@ SP-CHK-VAR-EXT  loop
   SP-FAC-N @ 0 ?do  i SP-CHK-FACTOR  loop ;

\ ---- equation registry (docs/model-unified.md stage 1: "How an equation joins the
\ trainable graph"). A SPEC:-declared einsum becomes ONE `equation` op-kind
\ (maki/op-kind.f) referenceable inside a MODEL: composition. The registry maps the
\ equation NAME -> (operand/factor count, the output (rows,cols) and per-factor
\ (rows,cols) the composition extent-check verifies against the operands, and the xt
\ of a generated RUN word the executor calls). It extends the state SPEC: already
\ keeps (the generated NAME kernel + the SP-* dataflow) rather than a parallel store.
\
\ STAGE-1 COMPOSABILITY: an equation joins a composition only when its factors map
\ cleanly to the 2D rows x cols IR (docs/batch-sequence-design.md section 4): 1..2
\ free indices, 1..3 gather-free factors, every factor rank 1..2. A gather factor or
\ a rank-3 composite index (the batch/head extent-role product) is stage-2 work, so
\ such an equation is simply not registered as a composable op (using it in a MODEL:
\ body is the unknown-op reject E-CAD-OP, never a wrong run). The generated kernel +
\ SP-* dataflow are unaffected: SPEC: still derives them for every valid equation.
public
DEFTYPE EQ-SLOT                       \ registry row index: its own type

private
32 constant EQ-CAP                     \ max registered equations
8  constant EQ-FCAP                    \ max factors per equation (matches SP-FAC-CAP)
32 constant EQ-NAME-CAP
create EQ-NAMES  EQ-CAP EQ-NAME-CAP * allot
create EQ-NLEN   EQ-CAP cells allot
create EQ-K-A    EQ-CAP cells allot          \ operand/factor count
create EQ-ROWS-A EQ-CAP cells allot          \ output rows (free-index-0 extent)
create EQ-COLS-A EQ-CAP cells allot          \ output cols (free-index-1 extent, or 1)
create EQ-FROW-A EQ-CAP EQ-FCAP * cells allot \ per-factor rows (slot-major)
create EQ-FCOL-A EQ-CAP EQ-FCAP * cells allot \ per-factor cols (slot-major)
create EQ-XT-A   EQ-CAP cells allot           \ generated RUN word xt (raw)
variable EQ-N

: EQ-NAME-PTR ( eq-slot -- ptr a )  EQ-SLOT>N EQ-NAME-CAP *  EQ-NAMES + ;

public
: EQ-NAME@ ( eq-slot -- ptr u8 n ) {: s:eq-slot :}  s EQ-NAME-PTR  s EQ-SLOT>N cells EQ-NLEN + @ ;
: EQ-K@    ( eq-slot -- n )  EQ-SLOT>N cells EQ-K-A    + @ ;
: EQ-ROWS@ ( eq-slot -- n )  EQ-SLOT>N cells EQ-ROWS-A + @ ;
: EQ-COLS@ ( eq-slot -- n )  EQ-SLOT>N cells EQ-COLS-A + @ ;
: EQ-FROW@ ( eq-slot n -- n ) {: s:eq-slot k:n :}  s EQ-SLOT>N EQ-FCAP * k + cells EQ-FROW-A + @ ;
: EQ-FCOL@ ( eq-slot n -- n ) {: s:eq-slot k:n :}  s EQ-SLOT>N EQ-FCAP * k + cells EQ-FCOL-A + @ ;

\ NAME -> registry slot; absent = option<eq-slot> none, so a caller must handle it.
: EQ-FIND ( ptr u8 n -- option<eq-slot> ) {: a:ptr u:n :}
   EQ-N @ 0 ?do
      a u  i >EQ-SLOT EQ-NAME@  STR= if  i >EQ-SLOT OPTION:SOME  unloop exit  then
   loop  OPTION:NONE ;

\ ---- executor transfer cells: the executor writes each operand buffer + the output
\ buffer here, then calls EQ-EXEC; the generated RUN word binds the equation's tensors
\ from these cells and runs the kernel. Single node runs at a time, so one set serves.
create EQ-ARG EQ-FCAP cells allot   \ per-operand buffer pointer
variable EQ-OUT                      \ output buffer pointer
: EQ-ARG-SET! ( ptr a n -- ) {: p:ptr k:n :}
   k 0 < k EQ-FCAP >= or if E-SPEC-ARITY throw then  p EQ-ARG k cells + ! ;
: EQ-OUT-SET! ( ptr a -- )  EQ-OUT ! ;

\ EQ-EXEC runs the equation's RUN word (raw xt from the registry). TRUSTED: the xt is
\ a spec-registry cell whose provenance is a word this file generated + captured, but
\ execute of a fetched cell is not checker-expressible; this is the audited boundary.
TRUSTED: EQ-EXEC ( eq-slot -- )  EQ-SLOT>N cells EQ-XT-A + @ execute ;

private
: EQ-NAME! ( ptr u8 n eq-slot -- ) {: a:ptr u:n s:eq-slot :}
   u EQ-NAME-CAP > if E-SPEC-SYNTAX throw then
   a s EQ-NAME-PTR u BYTE-COPY  u s EQ-SLOT>N cells EQ-NLEN + ! ;
\ EQ-XT! is referenced from the generated RUN source (interpret level inside XG-EVAL),
\ so the tick-captured xt is stored into its row without crossing the XG-EVAL effect.
: EQ-XT! ( n n -- ) {: x:n s:n :}  x EQ-XT-A s cells + ! ;

\ ---- extent magnitudes off the parsed spec (SP-EXT-SLOT resolves an index variable's
\ #<UPPER> extent to its registry value). Free index i and factor f's index j. -------
: EQ-FREE-EXT ( n -- n ) {: i:n :}  i SPEC-FREE@ SP-EXT-SLOT XR-VAL@ ;
: EQ-FAC-EXT  ( n n -- n ) {: f:n j:n :}  f j SPEC-FAC-IDX@ SP-EXT-SLOT XR-VAL@ ;

\ a factor is stage-1 plain when it is rank 1..2 and carries no gather index.
: EQ-FAC-PLAIN? ( n -- bool ) {: f:n :}
   f SPEC-FAC-RANK@ dup 1 < swap 2 > or if false exit then
   f SPEC-FAC-RANK@ 0 ?do  f i SPEC-FAC-GATHER@ nip 0 > if false unloop exit then  loop
   true ;
: EQ-COMPOSABLE? ( -- bool )
   SP-FREE-N @ dup 1 < swap 2 > or if false exit then
   SP-FAC-N @  dup 1 < swap 3 > or if false exit then
   SP-FAC-N @ 0 ?do  i EQ-FAC-PLAIN? 0= if false unloop exit then  loop
   true ;

\ output (rows,cols) from the free extents; a single free index is a rows x 1 column.
: EQ-OUT-DIMS ( -- n n )
   0 EQ-FREE-EXT   SP-FREE-N @ 2 = if 1 EQ-FREE-EXT else 1 then ;
\ store factor f's (rows,cols) into the registry row; a rank-1 factor is rows x 1.
: EQ-FAC-DIMS! ( eq-slot n -- ) {: s:eq-slot f:n :}
   f 0 EQ-FAC-EXT   s EQ-SLOT>N EQ-FCAP * f + cells EQ-FROW-A + !
   f SPEC-FAC-RANK@ 2 = if f 1 EQ-FAC-EXT else 1 then
      s EQ-SLOT>N EQ-FCAP * f + cells EQ-FCOL-A + ! ;

\ generate `: <NAME>-RUN ( -- ) <bind each factor from EQ-ARG> <bind out from EQ-OUT>
\ <NAME> ;` (one checked XG-EVAL), then capture its xt into registry row sl (a SECOND
\ XG-EVAL: a `:`-definition and an interpret-level tick must not share one evaluate).
: EQ-GEN-RUN ( n -- ) {: sl:n :}
   XG-RESET
   s" : " XG+  SPEC-NAME$ XG+  s" -RUN ( -- ) " XG+
   SP-FAC-N @ 0 ?do
      s" EQ-ARG " XG+  i XG-INT  s"  cells + @ " XG+  i SPEC-FAC-NAME@ XG+  s" -BIND " XG+
   loop
   s" EQ-OUT @ " XG+  SPEC-OUT$ XG+  s" -BIND " XG+
   SPEC-NAME$ XG+  s"  ; " XG+
   XG-EVAL
   XG-RESET
   s" ' " XG+  SPEC-NAME$ XG+  s" -RUN " XG+  sl XG-INT  s"  EQ-XT! " XG+
   XG-EVAL ;

\ register the just-parsed equation as a composable op (no-op for a non-composable one).
: EQ-REGISTER ( -- )
   EQ-COMPOSABLE? 0= if exit then
   EQ-N @ EQ-CAP >= if E-EXT-CAP throw then
   EQ-N @ >EQ-SLOT {: s:eq-slot :}
   SPEC-NAME$ s EQ-NAME!
   SP-FAC-N @ s EQ-SLOT>N cells EQ-K-A + !
   EQ-OUT-DIMS {: rows:n cols:n :}
   rows s EQ-SLOT>N cells EQ-ROWS-A + !
   cols s EQ-SLOT>N cells EQ-COLS-A + !
   SP-FAC-N @ 0 ?do  s i EQ-FAC-DIMS!  loop
   EQ-N @ EQ-GEN-RUN
   EQ-N @ 1+ EQ-N ! ;

public

\ SPEC: NAME <output>[<free>] = <factors> [*] +SUM <contraction> ;  - see file head.
\ Top-level-interpret-only (parses the stream + generates checked words through the
\ XG-EVAL boundary), like EXTENT:/TENSOR:/SUMTYPE. Derives (1) the checked golden
\ words <NAME>-EL + <NAME>, and leaves the parsed structure queryable for the (2)
\ dataflow and (3) shape-obligation records below.
: SPEC: ( -- )
   parse-name SP-NAME!
   SP-COLLECT
   SP-PARSE
   SP-VALIDATE
   SP-EMIT-EL
   SP-EMIT-OUTER
   EQ-REGISTER ;

\ ---- testing / dry-run seams (SPEC: parses the live stream, so it cannot be
\ wrapped for a catch; these string/candidate entries make the derivation testable
\ in-process, mirroring maki/cad.f's MODEL-CAND:). --------------------------------
\ SPEC-CHECK$: parse + validate a spec BODY string (everything after the name, no
\ trailing `;`) and throw a named E-SPEC-* on any malformed spec. No code generated.
: SPEC-CHECK$ ( ptr u8 n -- )  s" cand" SP-NAME!  SP-LOAD$  SP-PARSE  SP-VALIDATE ;
\ SPEC-CAND: NAME <spec> ; - like SPEC: but leaves the element CANDIDATE text in the
\ codegen buffer instead of compiling it, so a test can score the checker verdict of
\ the derived accessor loop (SPEC-CAND$ CHECK-QUIET-CANDIDATE!). Catches the extent
\ flip a valid-but-transposed spec produces.
: SPEC-CAND: ( -- )
   parse-name SP-NAME!  SP-COLLECT  SP-PARSE  SP-VALIDATE
   XG-RESET  SP-EL-CORE ;
: SPEC-CAND$ ( -- ptr u8 n )  XG$ ;

\ ---- derivation (3): PROMOTE shape obligations - the extent MAGNITUDES the output
\ shape and the contraction span impose. Integration boundary: a PROMOTE gate in
\ maki/cad.f (alongside PROMOTE-OK?/PROMOTE-NPOL) would read these; no such gate
\ exists yet (scout: PROMOTE consumes verdicts, not shapes), so this is the
\ self-contained record PROMOTE will consume when that gate lands.
: SPEC-FREE-EXTENT@ ( n -- n ) {: i:n :}  i SPEC-FREE@ SP-EXT-SLOT XR-VAL@ ;
: SPEC-CT-EXTENT@   ( n -- n ) {: i:n :}  i SPEC-CT@   SP-EXT-SLOT XR-VAL@ ;

;package
