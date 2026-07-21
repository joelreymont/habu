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
\ (the candidate-B flip protection). Past parse time, every public dataflow/shape/registry
\ accessor bounds-checks its index against the live count BEFORE any addressing or cast: an
\ out-of-domain free/contraction/factor/factor-member/equation-factor index is the named
\ E-SPEC-RANGE reject, never an out-of-arena read or a forged slot. maki -> habu only; maki
\ owns -5007, -5018..-5019, -5036..-5039.

require maki/extent-tensor.f
require lib/string.f                 \ STR=, ASCII-UPPER: token compares + index-var -> extent-name fold
require lib/adt/option.f             \ option<>: index-variable and tensor lookups return present/absent
require lib/type/deftype.f           \ DEFTYPE: the factor-index (SP-FI) table index is its own type

-5036 constant E-SPEC-SYNTAX    \ malformed spec grammar (brackets, =, keywords, terminator)
-5037 constant E-SPEC-EXTENT    \ an index variable's extent #<UPPER> is not declared
-5038 constant E-SPEC-UNBOUND   \ a factor index is neither a free (output) nor a contraction index
-5039 constant E-SPEC-TENSOR    \ a tensor/gather name is not declared, or has the wrong kind
-5019 constant E-SPEC-ARITY     \ factor index count != tensor rank, or free/contraction arity > 2
-5018 constant E-CAD-GRAD       \ training requested for a forward-only equation (gather adjoint = scatter-add, not expressible)
-5007 constant E-SPEC-RANGE     \ a public dataflow/shape/registry accessor index is outside its live domain

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
variable SP-BATCH-N   \ BTC-2: how many LEADING free indices are batch (free-role) axes
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

\ owner guards: a free/contraction index must lie in [0, live count) before any addressing.
: SP-FREE-BOUNDS ( n -- ) {: i:n :}  i 0 <  i SP-FREE-N @ >=  or if E-SPEC-RANGE throw then ;
: SP-CT-BOUNDS   ( n -- ) {: i:n :}  i 0 <  i SP-CT-N   @ >=  or if E-SPEC-RANGE throw then ;

public
: SPEC-FREE-N ( -- n )  SP-FREE-N @ ;
: SPEC-CT-N   ( -- n )  SP-CT-N @ ;
: SPEC-FREE@ ( n -- ptr u8 n ) {: i:n :}  i SP-FREE-BOUNDS  SP-FREE i SP-IDX-SLOT  i cells SP-FREE-L + @ ;
: SPEC-CT@   ( n -- ptr u8 n ) {: i:n :}  i SP-CT-BOUNDS    SP-CT   i SP-IDX-SLOT  i cells SP-CT-L   + @ ;
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
\ owner guard: a factor-index (SP-FI) row must lie in [0, live SP-FI-N) before any addressing.
\ SPEC-FAC-IDX@/GATHER@ always cross to an in-window row; this also fails closed on a directly
\ forged >SP-FI passed straight to the public SP-FI-VAR@/GATH@ readers.
: SP-FI-BOUNDS ( sp-fi -- ) {: fi:sp-fi :}  fi SP-FI>N 0 <  fi SP-FI>N SP-FI-N @ >=  or if E-SPEC-RANGE throw then ;

public
: SP-FI-VAR@  ( sp-fi -- ptr u8 n ) {: fi:sp-fi :}  fi SP-FI-BOUNDS  fi SP-FI-VAR-SLOT   fi SP-FI>N cells SP-FI-VARL + @ ;
: SP-FI-GATH@ ( sp-fi -- ptr u8 n ) {: fi:sp-fi :}  fi SP-FI-BOUNDS  fi SP-FI-GATH-SLOT  fi SP-FI>N cells SP-FI-GATHL + @ ;
private

\ owner guards for the factor domain: a factor number in [0, SP-FAC-N), and a factor-member
\ index k in [0, that factor's rank). SP-FAC-CNT@ is the raw rank read (f already bounded).
: SP-FAC-CNT@ ( n -- n )  cells SP-FAC-CNT + @ ;
: SP-FAC-BOUNDS ( n -- ) {: f:n :}  f 0 <  f SP-FAC-N @ >=  or if E-SPEC-RANGE throw then ;
: SP-FAC-IDX-BOUNDS ( n n -- ) {: f:n k:n :}
   f SP-FAC-BOUNDS  k 0 <  k f SP-FAC-CNT@ >=  or if E-SPEC-RANGE throw then ;

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
: SPEC-FAC-NAME@ ( n -- ptr u8 n ) {: i:n :}  i SP-FAC-BOUNDS  i SP-FAC-T-SLOT  i cells SP-FAC-TL + @ ;
: SPEC-FAC-RANK@ ( n -- n ) {: f:n :}  f SP-FAC-BOUNDS  f SP-FAC-CNT@ ;
: SPEC-FAC-IDX@ ( n n -- ptr u8 n ) {: f:n k:n :}   \ k-th factor index's variable
   f k SP-FAC-IDX-BOUNDS  f SP-FAC-OFF@ SP-FI>N k + >SP-FI SP-FI-VAR@ ;
: SPEC-FAC-GATHER@ ( n n -- ptr u8 n ) {: f:n k:n :}  \ k-th factor index's gather ("" if none)
   f k SP-FAC-IDX-BOUNDS  f SP-FAC-OFF@ SP-FI>N k + >SP-FI SP-FI-GATH@ ;
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
variable SP-PLUS?     \ an elementwise-add token (+) appeared between output-shaped terms
variable SP-EW?       \ elementwise/broadcast mode: output-shaped terms, NO contraction (no +SUM/Σ)
: SP-PARSE-FACTORS ( -- )   \ terms separated by optional product (*) or elementwise-add (+) tokens; stops at +SUM or end
   begin
      SP-PEEK s" *" STR= if SP-NEXT 2drop  -1 SP-STAR? ! then
      SP-PEEK s" +" STR= if SP-NEXT 2drop  -1 SP-PLUS? ! then
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
: SP-PARSE-REDUCTION ( -- )   \ a trailing +SUM, a prefix Σ already consumed, or (neither) elementwise mode
   SP-PEEK s" +SUM" STR= if
      SP-PREFIX? @ if E-SPEC-SYNTAX throw then
      SP-PARSE-SUM exit
   then
   SP-PREFIX? @ if exit then     \ prefix Σ already supplied the reduction (contraction mode)
   -1 SP-EW? ! ;                 \ no reduction at all: elementwise/broadcast mode
: SP-PARSE ( -- )
   0 SP-FREE-N !  0 SP-CT-N !  0 SP-FAC-N !  0 SP-FI-N !
   0 SP-STAR? !  0 SP-PREFIX? !  0 SP-PLUS? !  0 SP-EW? !
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

\ is index var (a,u) one of factor f's indices? (shared by batch validation + adjoints)
: IDX-IN-FAC? ( ptr u8 n n -- bool ) {: a:ptr u:n f:n :}
   f SPEC-FAC-RANK@ 0 ?do  a u  f i SPEC-FAC-IDX@ STR= if true unloop exit then  loop  false ;

\ ---- batch (free-role) index classification (BTC-2, docs/batch-sequence-design.md §4.3).
\ A batch/head index is a FREE-EXTENT: role riding EVERY factor and the output; the GGEMM
\ is REPLICATED over it. Convention: batch indices are the LEADING free indices, so
\ `S[b h i j]` splits into batch (b,h) then GEMM (i,j). SP-CLASSIFY-BATCH counts the
\ leading free indices whose extent is a free role (stops at the first plain one); a free
\ role AFTER a plain index is a mis-ordering SP-VALIDATE-CT rejects.
: SP-CLASSIFY-BATCH ( -- )
   0 SP-BATCH-N !
   SP-FREE-N @ 0 ?do
      i SPEC-FREE@ SP-EXT-SLOT XR-FREE? 0= if leave then
      SP-BATCH-N @ 1+ SP-BATCH-N !
   loop ;

public
\ dataflow record (2): the batched-contraction replication axes - the free extents the
\ contraction is replicated over (docs/tma-gather.md:29-45). SPEC-BATCH@ i (i < SPEC-BATCH-N)
\ is a leading free index; SPEC-BATCH-EXTENT@ its replication magnitude (PROMOTE leg 3).
: SPEC-BATCH-N ( -- n )  SP-BATCH-N @ ;
: SPEC-BATCH@ ( n -- ptr u8 n )  SPEC-FREE@ ;
: SPEC-BATCH-EXTENT@ ( n -- n ) {: i:n :}  i SPEC-FREE@ SP-EXT-SLOT XR-VAL@ ;
private

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
\ the elementwise/broadcast element (dot habu-spec-broadcast-forms): output-shaped
\ terms combined by + (elementwise add) or * (elementwise mul), NO contraction loop.
\ Each term reuses SP-EMIT-FACTOR, so a factor whose index list is a proper suffix of
\ the output's reads through the SAME extent-typed accessor at its trailing indices - a
\ row (1xC) or scalar broadcast falls out of the shorter index list, and a wrong-extent
\ operand is the same generated-accessor checker reject the contraction form produces.
: SP-EW-COMB ( -- )  SP-PLUS? @ if s" f+ " XG+ else s" f* " XG+ then ;
: SP-EW-EL-CORE ( -- )
   SPEC-NAME$ XG+  s" -EL ( " XG+  SP-EMIT-EL-SIG  s" -- r ) " XG+
   SP-EMIT-PROJ
   SP-FAC-N @ 0 ?do  i SP-EMIT-FACTOR  loop
   SP-FAC-N @ 1 - 0 ?do  SP-EW-COMB  loop ;
: SP-EMIT-EW-EL ( -- )
   XG-RESET  s" : " XG+  SP-EW-EL-CORE  s" ; " XG+  XG-EVAL ;
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

\ ---- batched free-extent outer (BTC-2). A batch/head (free, non-contracted) index rides
\ EVERY factor and the output, so the GGEMM is the SAME contraction REPLICATED over it. The
\ leading SP-BATCH-N free indices are the batch axes; the trailing SP-GEMM-N are the GEMM
\ output axes. habu has only i/j loop counters, so the outer loops split into two words that
\ each stay within two counters: NAME-GEMM (takes the batch indices as ix args, loops the
\ GEMM axes, calls NAME-EL + stores) and NAME (loops the batch axes, calls NAME-GEMM). The
\ element NAME-EL is UNCHANGED - SP-EMIT-EL already takes every free index as a projected
\ arg (SP-EMIT-EL-SIG / SP-EMIT-PROJ) and loops the contraction, so it needs no batch case.
: SP-GEMM-N ( -- n )  SP-FREE-N @ SP-BATCH-N @ - ;   \ GEMM (non-batch) output-index count
\ open / close ?do loops for free positions [lo,hi), outer..inner in declared order.
: SP-EMIT-RANGE-LOOPS ( n n -- ) {: lo:n hi:n :}  hi lo ?do  i SPEC-FREE@ SP-EXT$ XG+  s"  0 ?do " XG+  loop ;
: SP-EMIT-RANGE-CLOSE ( n n -- ) {: lo:n hi:n :}  hi lo ?do  s" loop " XG+  loop ;
\ inject positions [lo,hi) as ix<ext> from the loop counter at nesting depth (pos-lo).
: SP-EMIT-RANGE-INJECT ( n n -- ) {: lo:n hi:n :}
   hi lo ?do  i lo -  hi lo -  SP-EMIT-COUNTER  i SPEC-FREE@ SP-EMIT-INJ  loop ;
\ inject the batch indices from the plain-cell locals g0..g{B-1} that NAME-GEMM binds.
: SP-EMIT-BATCH-INJECT ( -- )
   SP-BATCH-N @ 0 ?do  s" g" XG+  i XG-INT  s"  " XG+  i SPEC-FREE@ SP-EMIT-INJ  loop ;
\ the full free-index tuple for one NAME-EL call / OUT store inside NAME-GEMM.
: SP-EMIT-BATCHED-TUPLE ( -- )  SP-EMIT-BATCH-INJECT  SP-BATCH-N @ SP-FREE-N @ SP-EMIT-RANGE-INJECT ;
\ project NAME-GEMM's batch ix args to plain locals g0..g{B-1} (high->low; stack top is last).
: SP-EMIT-BATCH-PROJ ( -- )
   SP-BATCH-N @ 0 ?do  SP-BATCH-N @ 1 - i -  {: k:n :}  s" IX>N {: g" XG+  k XG-INT  s" :n :} " XG+  loop ;
: SP-EMIT-GEMM-SIG ( -- )   \ the batch ix args NAME-GEMM takes
   SP-BATCH-N @ 0 ?do  s" ix<" XG+  i SPEC-FREE@ SP-EXT-SLOT XR-TAIL@ XG+  s" > " XG+  loop ;
: SP-EMIT-BATCHED-OUTER ( -- )
   XG-RESET
   s" : " XG+  SPEC-NAME$ XG+  s" -GEMM ( " XG+  SP-EMIT-GEMM-SIG  s" -- ) " XG+
   SP-EMIT-BATCH-PROJ
   SP-BATCH-N @ SP-FREE-N @ SP-EMIT-RANGE-LOOPS
   SP-EMIT-BATCHED-TUPLE  SPEC-NAME$ XG+  s" -EL " XG+
   SP-EMIT-BATCHED-TUPLE  SPEC-OUT$ XG+   s" ! " XG+
   SP-BATCH-N @ SP-FREE-N @ SP-EMIT-RANGE-CLOSE
   s" ; " XG+
   XG-EVAL
   XG-RESET
   s" : " XG+  SPEC-NAME$ XG+  s"  ( -- ) " XG+
   0 SP-BATCH-N @ SP-EMIT-RANGE-LOOPS
   0 SP-BATCH-N @ SP-EMIT-RANGE-INJECT  SPEC-NAME$ XG+  s" -GEMM " XG+
   0 SP-BATCH-N @ SP-EMIT-RANGE-CLOSE
   s" ; " XG+
   XG-EVAL ;

\ ---- contraction-legality witness (BTC-2 / BTC-7). SPEC: generates, per contraction
\ equation, a word whose SIGNATURE marks each contraction axis a reduction axis
\ (redx<ct-ext>). Compiling it IS the load-time check: a contraction over a FREE (batch)
\ extent is redx<free>, which the checker rejects at SIG-END-PARAM (exit 70 class), making
\ the cross-sequence leak a type error - not a runtime bug. A plain (inner) contraction
\ extent yields redx<inner>, which compiles silently. Body >RED per axis (swap-threaded for
\ the 2-axis case) so the redx outputs match the declared order.
: SP-EMIT-RSUM-SIG ( -- )   \ ( ix<ct0-tail> [ix<ct1-tail>] -- redx<ct0-tail> [redx<ct1-tail>] )
   SP-CT-N @ 0 ?do  s" ix<" XG+  i SPEC-CT@ SP-EXT-SLOT XR-TAIL@ XG+  s" > " XG+  loop
   s" -- " XG+
   SP-CT-N @ 0 ?do  s" redx<" XG+  i SPEC-CT@ SP-EXT-SLOT XR-TAIL@ XG+  s" > " XG+  loop ;
: SP-RSUM-CORE ( -- )
   SPEC-NAME$ XG+  s" -RSUM ( " XG+  SP-EMIT-RSUM-SIG  s" ) " XG+
   s" >RED " XG+
   SP-CT-N @ 2 = if s" swap >RED swap " XG+ then ;
: SP-EMIT-RSUM ( -- )  XG-RESET  s" : " XG+  SP-RSUM-CORE  s" ; " XG+  XG-EVAL ;

\ SP-EMIT-BODY dispatches the element and outer on the parsed mode; SP-CAND-CORE the
\ checker-candidate element text. A contraction form additionally emits SP-EMIT-RSUM (the
\ free-contraction guard) and, when it carries batch axes, the batched outer.
: SP-EMIT-BODY ( -- )
   SP-EW? @ if
      SP-EMIT-EW-EL  SP-EMIT-OUTER
   else
      SP-EMIT-EL
      SP-BATCH-N @ 0 > if SP-EMIT-BATCHED-OUTER else SP-EMIT-OUTER then
      SP-EMIT-RSUM
   then ;
: SP-CAND-CORE ( -- )  SP-EW? @ if SP-EW-EL-CORE else SP-EL-CORE then ;

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
\ a batch (free-role) index rides EVERY factor (the replication structure the batched
\ derivation depends on): a factor missing it would broadcast, not replicate.
: SP-CHK-BATCH-FAC ( n -- ) {: bi:n :}
   bi SPEC-FREE@ {: va:ptr vu:n :}
   SP-FAC-N @ 0 ?do  va vu i IDX-IN-FAC? 0= if E-SPEC-ARITY throw then  loop ;
: SP-VALIDATE-CT ( -- )   \ contraction form: OUT[batch.. gemm..] = factors [*] +SUM ct
   SP-PLUS? @ if E-SPEC-SYNTAX throw then                          \ + is elementwise-only, never in a contraction
   SP-GEMM-N 2 > if E-SPEC-ARITY throw then                        \ 0..2 GEMM (output) axes (0 = rank-0 full-reduction output)
   SP-GEMM-N 0= SP-BATCH-N @ 0<> and if E-SPEC-ARITY throw then    \ a rank-0 GEMM under a batch axis (per-batch scalar) is stage-2, out of grammar
   SP-BATCH-N @ 2 > if E-SPEC-ARITY throw then                     \ 0..2 batch (replication) axes
   \ a free role after a plain output index is a mis-ordered batch axis (batch must lead).
   SP-FREE-N @ SP-BATCH-N @ ?do  i SPEC-FREE@ SP-EXT-SLOT XR-FREE? if E-SPEC-ARITY throw then  loop
   SP-BATCH-N @ 0 ?do  i SP-CHK-BATCH-FAC  loop
   SP-CT-N @ 1 < SP-CT-N @ 2 > or if E-SPEC-ARITY throw then       \ 1..2 contraction indices
   SP-FAC-N @ 0= if E-SPEC-SYNTAX throw then
   SP-FAC-N @ 1 > SP-STAR? @ 0= and if E-SPEC-SYNTAX throw then    \ >1 factor requires *
   SP-FAC-N @ 1 = SP-STAR? @ and    if E-SPEC-SYNTAX throw then    \ 1 factor forbids *
   SPEC-OUT$ SP-FREE-N @ SP-CHK-DATA                              \ output: data tensor, rank == free count
   SP-FREE-N @ 0 ?do  i SPEC-FREE@ SP-CHK-VAR-EXT  loop
   SP-CT-N @ 0 ?do  i SPEC-CT@ SP-CHK-VAR-EXT  loop
   SP-FAC-N @ 0 ?do  i SP-CHK-FACTOR  loop ;

\ ---- elementwise/broadcast validation (dot habu-spec-broadcast-forms). A factor's
\ index list must be a SUFFIX of the output's free list: same-shape (full suffix),
\ row broadcast (drop the leading index -> a 1xC bias), scalar (empty -> a 1x1 scale).
\ The scalar 1x1 case is a rank-0 factor tensor, which the extent-tensor accessor
\ generator cannot emit (TENSOR: ( ) -> undefined x0); such a factor fails closed here
\ at SP-CHK-DATA (rank mismatch, E-SPEC-ARITY). The long-term fix is rank-0 accessors
\ in maki/extent-tensor.f, after which the scalar form falls out of this same suffix
\ machinery with no grammar change. A NON-suffix index list (a column Rx1 broadcast, or
\ any leading-index drop) is the named E-SPEC-ARITY reject.
: SP-CHK-EW-SUFFIX ( n -- ) {: f:n :}   \ factor f's index vars are the trailing free vars, in order
   SP-FREE-N @ f SPEC-FAC-RANK@ -  {: base:n :}
   f SPEC-FAC-RANK@ 0 ?do
      f i SPEC-FAC-IDX@  base i + SPEC-FREE@  STR= 0= if E-SPEC-ARITY throw then
   loop ;
: SP-CHK-EW-FACTOR ( n -- ) {: f:n :}
   f SPEC-FAC-NAME@  f SPEC-FAC-RANK@  SP-CHK-DATA                 \ declared data tensor, rank == index count
   f SPEC-FAC-RANK@ SP-FREE-N @ > if E-SPEC-ARITY throw then       \ a suffix is no longer than the output
   f SP-CHK-EW-SUFFIX
   f SPEC-FAC-RANK@ 0 ?do
      f i SPEC-FAC-GATHER@ nip 0 > if E-SPEC-TENSOR throw then     \ a broadcast term carries no gather
   loop ;
: SP-VALIDATE-EW ( -- )   \ elementwise form: OUT[free] = terms combined by a single + or *
   SP-BATCH-N @ 0<> if E-SPEC-ARITY throw then                     \ batch/free roles are contraction-only (BTC-2)
   SP-FREE-N @ 1 < SP-FREE-N @ 2 > or if E-SPEC-ARITY throw then   \ output 1..2 free indices
   SP-CT-N @ 0<> if E-SPEC-SYNTAX throw then                       \ elementwise: no contraction indices
   SP-FAC-N @ 0= if E-SPEC-SYNTAX throw then
   SP-FAC-N @ 1 > if
      SP-STAR? @ 0<> SP-PLUS? @ 0<> and if E-SPEC-SYNTAX throw then   \ + and * cannot mix in one expression
      SP-STAR? @ 0<> SP-PLUS? @ 0<> or 0= if E-SPEC-SYNTAX throw then \ >1 term needs a combiner
   else
      SP-STAR? @ 0<> SP-PLUS? @ 0<> or if E-SPEC-SYNTAX throw then    \ a single term takes no combiner
   then
   SPEC-OUT$ SP-FREE-N @ SP-CHK-DATA                              \ output: data tensor, rank == free count
   SP-FREE-N @ 0 ?do  i SPEC-FREE@ SP-CHK-VAR-EXT  loop
   SP-FAC-N @ 0 ?do  i SP-CHK-EW-FACTOR  loop ;
: SP-VALIDATE ( -- )  SP-CLASSIFY-BATCH  SP-EW? @ if SP-VALIDATE-EW else SP-VALIDATE-CT then ;

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
128 constant EQ-CAP                    \ max registered equations (a composable one also registers its 1..K derived adjoints)
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
\ ---- stage-2 adjoint records (docs/model-unified.md "Derived adjoints"). Per forward
\ slot: the eq-slot of each factor's derived adjoint equation (EQ-ADJ-A, slot-major) and
\ a differentiability flag (EQ-DIFF-A: 1 = adjoints derived and trainable, 0 = forward-only,
\ e.g. a gather whose scatter-add adjoint the multiply-then-sum grammar cannot state). ----
create EQ-ADJ-A  EQ-CAP EQ-FCAP * cells allot \ [fwd-slot][k] -> adjoint eq-slot raw (-1 = none)
create EQ-DIFF-A EQ-CAP cells allot           \ per forward slot: 1 = trainable, 0 = forward-only
variable EQ-N

: EQ-ADJ! ( n eq-slot n -- ) {: v:n s:eq-slot k:n :}  v  s EQ-SLOT>N EQ-FCAP * k + cells EQ-ADJ-A + ! ;
: EQ-DIFF-SET! ( n eq-slot -- ) {: v:n s:eq-slot :}  v  s EQ-SLOT>N cells EQ-DIFF-A + ! ;

: EQ-NAME-PTR ( eq-slot -- ptr a )  EQ-SLOT>N EQ-NAME-CAP *  EQ-NAMES + ;

\ owner guard for the equation-factor domain: a factor index k must lie in the equation's
\ LIVE factor window [0, EQ-K@) before any column addressing. EQ-K@ never exceeds EQ-FCAP
\ (the physical column allocation), so this keeps every read inside the slot's live columns.
: EQ-FAC-BOUNDS ( eq-slot n -- ) {: s:eq-slot k:n :}
   k 0 <  k  s EQ-SLOT>N cells EQ-K-A + @  >=  or if E-SPEC-RANGE throw then ;

public
: EQ-NAME@ ( eq-slot -- ptr u8 n ) {: s:eq-slot :}  s EQ-NAME-PTR  s EQ-SLOT>N cells EQ-NLEN + @ ;
: EQ-K@    ( eq-slot -- n )  EQ-SLOT>N cells EQ-K-A    + @ ;
: EQ-ROWS@ ( eq-slot -- n )  EQ-SLOT>N cells EQ-ROWS-A + @ ;
: EQ-COLS@ ( eq-slot -- n )  EQ-SLOT>N cells EQ-COLS-A + @ ;
: EQ-FROW@ ( eq-slot n -- n ) {: s:eq-slot k:n :}  s k EQ-FAC-BOUNDS  s EQ-SLOT>N EQ-FCAP * k + cells EQ-FROW-A + @ ;
: EQ-FCOL@ ( eq-slot n -- n ) {: s:eq-slot k:n :}  s k EQ-FAC-BOUNDS  s EQ-SLOT>N EQ-FCAP * k + cells EQ-FCOL-A + @ ;
\ stage-2 adjoint queries (the reverse transform, maki/backward.f, reads them).
: EQ-ADJ@ ( eq-slot n -- eq-slot ) {: s:eq-slot k:n :}   \ factor k's derived adjoint equation
   s k EQ-FAC-BOUNDS  s EQ-SLOT>N EQ-FCAP * k + cells EQ-ADJ-A + @ >EQ-SLOT ;
: EQ-DIFF? ( eq-slot -- bool )       EQ-SLOT>N cells EQ-DIFF-A + @ 0<> ;  \ trainable (not forward-only)

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

\ a factor is stage-1 plain when it is rank 0..2 and carries no gather index (rank 0 is a
\ scalar operand - a 1x1 broadcast, dot habu-rank-0-tensor - which maps to a 1x1 op-registry cell).
: EQ-FAC-PLAIN? ( n -- bool ) {: f:n :}
   f SPEC-FAC-RANK@ 2 > if false exit then
   f SPEC-FAC-RANK@ 0 ?do  f i SPEC-FAC-GATHER@ nip 0 > if false unloop exit then  loop
   true ;
: EQ-COMPOSABLE? ( -- bool )
   SP-FREE-N @ 2 > if false exit then                 \ 0..2 free indices (0 = rank-0 full-reduction output)
   SP-FAC-N @  dup 1 < swap 3 > or if false exit then
   SP-FAC-N @ 0 ?do  i EQ-FAC-PLAIN? 0= if false unloop exit then  loop
   true ;

\ output (rows,cols) from the free extents; a single free index is a rows x 1 column;
\ a rank-0 (empty free list) full-reduction output is a 1x1 scalar.
: EQ-OUT-DIMS ( -- n n )
   SP-FREE-N @ 0= if 1 1 exit then
   0 EQ-FREE-EXT   SP-FREE-N @ 2 = if 1 EQ-FREE-EXT else 1 then ;
\ store factor f's (rows,cols) into the registry row; a rank-1 factor is rows x 1, a rank-0
\ scalar factor is 1x1.
: EQ-FAC-DIMS! ( eq-slot n -- ) {: s:eq-slot f:n :}
   f SPEC-FAC-RANK@ 0= if 1 else f 0 EQ-FAC-EXT then
      s EQ-SLOT>N EQ-FCAP * f + cells EQ-FROW-A + !
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
   0 s EQ-DIFF-SET!                              \ forward-only until EQ-ADJ-DERIVE attaches adjoints
   EQ-FCAP 0 ?do  -1 s i EQ-ADJ!  loop
   SP-FAC-N @ s EQ-SLOT>N cells EQ-K-A + !
   EQ-OUT-DIMS {: rows:n cols:n :}
   rows s EQ-SLOT>N cells EQ-ROWS-A + !
   cols s EQ-SLOT>N cells EQ-COLS-A + !
   SP-FAC-N @ 0 ?do  s i EQ-FAC-DIMS!  loop
   EQ-N @ EQ-GEN-RUN
   EQ-N @ 1+ EQ-N ! ;

\ ---- derived adjoints (docs/model-unified.md stage 2) ------------------------------
\ The adjoint of an einsum is ANOTHER einsum: for O[free] = F0 F1 ... * +SUM ct, the
\ gradient w.r.t. factor Fj is the equation whose OUTPUT indices are Fj's indices, whose
\ FACTORS are dO (the forward output tensor, carrying the free indices) plus every OTHER
\ Fi, and whose CONTRACTION indices are every forward index NOT among Fj's indices. Those
\ adjoint equations are built as ORDINARY equation SOURCE and run through the SAME parser +
\ emitter + registry (SP-PARSE / SP-VALIDATE / SP-EMIT-EL / SP-EMIT-OUTER / EQ-REGISTER) -
\ no second einsum interpreter. The forward output tensor and each factor tensor are reused
\ by NAME (dQ has Q's extents, dO has O's extents), so no new TENSOR: declaration is needed:
\ the generated RUN word rebinds them per execution from the executor transfer cells.
\
\ A gather factor's adjoint is a scatter-add the multiply-then-sum grammar cannot state, so
\ a gather equation stays forward-only (EQ-DIFF? = 0) and asking for its adjoint is the named
\ E-CAD-GRAD reject - never a wrong gradient. The scatter-add primitive is a follow-up dot.
128 constant ADJ-SRC-CAP
create ADJB-BUF   ADJ-SRC-CAP allot  variable ADJB-U
create EQ-ADJ-SRC  EQ-FCAP ADJ-SRC-CAP * allot   \ per factor, the built adjoint BODY string
create EQ-ADJ-SRCL EQ-FCAP cells allot
create EQ-FWD-NM  40 allot  variable EQ-FWD-NM-U   \ forward name (SP-PARSE clobbers SP-NAME)
create EQ-FWD-SRC SP-SRC-CAP allot  variable EQ-FWD-SRC-U   \ forward body, to restore the queryable state

: ADJB-RESET ( -- )  0 ADJB-U ! ;
: ADJB-C ( n -- ) {: c:n :}
   ADJB-U @ ADJ-SRC-CAP >= if E-SPEC-SYNTAX throw then
   c ADJB-BUF ADJB-U @ + c!  ADJB-U @ 1 + ADJB-U ! ;
: ADJB+ ( ptr u8 n -- ) {: a:ptr u:n :}  u 0 ?do  a i + c@ ADJB-C  loop ;

\ a factor's index-variable list (space separated) and the whole factor `NAME[ i i ]`.
: ADJB-IDXS ( n -- ) {: f:n :}  f SPEC-FAC-RANK@ 0 ?do  f i SPEC-FAC-IDX@ ADJB+  s"  " ADJB+  loop ;
: ADJB-FAC  ( n -- ) {: f:n :}  f SPEC-FAC-NAME@ ADJB+  s" [ " ADJB+  f ADJB-IDXS  s" ] " ADJB+ ;

\ append one contraction index for the adjoint of factor j: every forward index NOT in Fj.
: ADJB-CT-IDX ( ptr u8 n n -- ) {: a:ptr u:n j:n :}
   a u j IDX-IN-FAC? 0= if  a u ADJB+  s"  " ADJB+  then ;
: ADJB-CT ( n -- ) {: j:n :}
   SP-FREE-N @ SP-CT-N @ +  j SPEC-FAC-RANK@ -  0= if exit then   \ Fj spans every forward index: elementwise adjoint, no +SUM (rank-0-output forward)
   s" +SUM " ADJB+
   SP-FREE-N @ 0 ?do  i SPEC-FREE@ j ADJB-CT-IDX  loop
   SP-CT-N   @ 0 ?do  i SPEC-CT@   j ADJB-CT-IDX  loop ;

\ build the adjoint BODY for factor j into EQ-ADJ-SRC[j]: Fj[ Fj-idx ] = O[ free ] * Fi... +SUM ct
: EQ-ADJ-BODY ( n -- ) {: j:n :}
   ADJB-RESET
   j SPEC-FAC-NAME@ ADJB+  s" [ " ADJB+  j ADJB-IDXS  s" ] = " ADJB+
   SPEC-OUT$ ADJB+  s" [ " ADJB+
   SP-FREE-N @ 0 ?do  i SPEC-FREE@ ADJB+  s"  " ADJB+  loop
   s" ] " ADJB+
   SP-FAC-N @ 0 ?do  i j <> if  s" * " ADJB+  i ADJB-FAC  then  loop
   j ADJB-CT
   ADJB-U @ {: u:n :}
   ADJB-BUF  EQ-ADJ-SRC j ADJ-SRC-CAP * +  u  BYTE-COPY
   u EQ-ADJ-SRCL j cells + ! ;
: EQ-ADJ-SRC-BUILD ( -- )  SP-FAC-N @ 0 ?do  i EQ-ADJ-BODY  loop ;

\ does any factor carry a gather index? (its adjoint is a scatter-add, not expressible)
: FAC-HAS-GATHER? ( n -- bool ) {: f:n :}
   f SPEC-FAC-RANK@ 0 ?do  f i SPEC-FAC-GATHER@ nip 0 > if true unloop exit then  loop  false ;
: EQ-HAS-GATHER? ( -- bool )
   SP-FAC-N @ 0 ?do  i FAC-HAS-GATHER? if true unloop exit then  loop  false ;

\ factor a's index list is a trailing SUFFIX of factor b's (same vars, same order, tail-aligned).
: FAC-SUFFIX? ( a b -- bool ) {: fa:n fb:n :}
   fa SPEC-FAC-RANK@  fb SPEC-FAC-RANK@  {: ra:n rb:n :}
   ra rb > if false exit then
   rb ra -  {: base:n :}
   ra 0 ?do  fa i SPEC-FAC-IDX@  fb base i + SPEC-FAC-IDX@  STR= 0= if false unloop exit then  loop
   true ;
\ the empty-contraction (ct=0) adjoint of factor j - Fj spans every forward index - is a legal
\ derived form ONLY as a full-reduction (empty free list) whose other factors are suffixes of Fj:
\ dFj[Fj-idx] = dS[] * the others, an elementwise/broadcast equation. A free axis (an outer-product
\ adjoint) or a non-suffix co-factor is out of grammar - forward-only, fail closed, never mis-derive.
: ADJ-CT0-OK? ( n -- bool ) {: j:n :}
   SP-FREE-N @ 0<> if false exit then
   SP-FAC-N @ 0 ?do  i j <> if  i j FAC-SUFFIX? 0= if false unloop exit then  then  loop  true ;

\ every factor's adjoint lands within the grammar: its GEMM-free axes = Fj's rank minus the
\ batch axes it also carries (1..2), its contraction = (all forward indices) - Fj's rank. Batch
\ axes RIDE ALONG (they appear in Fj and dO, never contracted), so for the non-batched case
\ (SP-BATCH-N=0) this is the old rk-1..2 / ct-1..2 rule EXCEPT the ct=0 full-reduction case: Fj
\ spans every index, so its adjoint is the elementwise dFj = dS[] * the others (ADJ-CT0-OK?).
: EQ-ADJ-DERIVABLE? ( -- bool )
   SP-FREE-N @ SP-CT-N @ +  {: total:n :}
   SP-FAC-N @ 0 ?do
      i SPEC-FAC-RANK@  {: rk:n :}
      rk SP-BATCH-N @ -  {: gf:n :}
      gf 1 < gf 2 > or if false unloop exit then
      total rk -  {: ct:n :}
      ct 0= if
         i ADJ-CT0-OK? 0= if false unloop exit then
      else
         ct 2 > if false unloop exit then
      then
   loop  true ;

: EQ-FWD-NM! ( -- )  SPEC-NAME$ {: a:ptr u:n :}  a EQ-FWD-NM u BYTE-COPY  u EQ-FWD-NM-U ! ;
\ set SP-NAME to <fwd>-ADJ<j> for the derived adjoint equation (reuses ADJB as scratch).
: EQ-ADJ-NAME! ( n -- ) {: j:n :}
   ADJB-RESET
   EQ-FWD-NM EQ-FWD-NM-U @ ADJB+  s" -ADJ" ADJB+  [char] 0 j + ADJB-C
   ADJB-BUF ADJB-U @ SP-NAME! ;

\ ---- elementwise/broadcast adjoints (dot habu-spec-broadcast-forms). The adjoint of an
\ elementwise form is ANOTHER SPEC equation, so it rides the same parse+emit+register
\ pipeline as the contraction adjoints. For O[free] = t0 (+|*) t1 ..., factor Tj's gradient:
\   ADD (+): dTj = dO reduced over Tj's broadcast axes  (same-shape -> a copy; a row-broadcast
\            1xC bias -> the column-sum contraction dB[n] = O[m n] +SUM m).
\   MUL (*): dTj = (dO * every OTHER factor) reduced over Tj's broadcast axes  (product rule;
\            same-shape hadamard -> dA[m n] = O[m n] * B[m n]).
\ dO is carried by the forward OUTPUT tensor name (reused, as the contraction path does). The
\ reduced axes are the free vars NOT among Tj's suffix indices, appended as a trailing +SUM.
: EQ-FAC-SAMESHAPE? ( n -- bool ) {: f:n :}  f SPEC-FAC-RANK@ SP-FREE-N @ = ;
: EQ-EW-ADJ-CT ( n -- ) {: j:n :}   \ reduce over the free vars NOT in factor j (its broadcast axes)
   j EQ-FAC-SAMESHAPE? if exit then                 \ same-shape term: no broadcast axis, no reduction
   s" +SUM " ADJB+
   SP-FREE-N @ 0 ?do
      i SPEC-FREE@ j IDX-IN-FAC? 0= if  i SPEC-FREE@ ADJB+  s"  " ADJB+  then
   loop ;
: EQ-EW-ADJ-BODY ( n -- ) {: j:n :}   \ build factor j's adjoint BODY into EQ-ADJ-SRC[j]
   ADJB-RESET
   j SPEC-FAC-NAME@ ADJB+  s" [ " ADJB+  j ADJB-IDXS  s" ] = " ADJB+   \ Tj[ tj-idx ] =
   SPEC-OUT$ ADJB+  s" [ " ADJB+                                        \ dO carried by the output tensor
   SP-FREE-N @ 0 ?do  i SPEC-FREE@ ADJB+  s"  " ADJB+  loop
   s" ] " ADJB+
   SP-PLUS? @ 0= if                                                     \ MUL: product rule -> * every OTHER factor
      SP-FAC-N @ 0 ?do  i j <> if  s" * " ADJB+  i ADJB-FAC  then  loop
   then
   j EQ-EW-ADJ-CT
   ADJB-U @ {: u:n :}
   ADJB-BUF  EQ-ADJ-SRC j ADJ-SRC-CAP * +  u  BYTE-COPY
   u EQ-ADJ-SRCL j cells + ! ;
: EQ-EW-ADJ-SRC-BUILD ( -- )  SP-FAC-N @ 0 ?do  i EQ-EW-ADJ-BODY  loop ;
\ ADD adjoints are always in-grammar for a 1..2 free output (copy or one column-sum). A MUL
\ adjoint keeps K factors (dO + the K-1 others), so it stays composable only for K <= 3.
: EQ-EW-ADJ-DERIVABLE? ( -- bool )
   SP-PLUS? @ if true exit then
   SP-FAC-N @ 3 <= ;
: EQ-EW-ADJ-DERIVE ( -- )
   EQ-N @ 1- >EQ-SLOT {: fwd:eq-slot :}
   EQ-EW-ADJ-DERIVABLE? 0= if exit then             \ adjoint out of grammar: leave forward-only (EQ-DIFF 0)
   EQ-FWD-NM!
   SP-SRC EQ-FWD-SRC SP-SRC-U @ BYTE-COPY  SP-SRC-U @ EQ-FWD-SRC-U !   \ save the forward body
   SP-FAC-N @ {: kk:n :}
   EQ-EW-ADJ-SRC-BUILD                              \ build ALL bodies before the first SP-PARSE clobbers state
   kk 0 ?do
      i EQ-ADJ-NAME!
      EQ-ADJ-SRC i ADJ-SRC-CAP * +  EQ-ADJ-SRCL i cells + @  SP-LOAD$
      SP-PARSE  SP-VALIDATE  SP-EMIT-BODY
      EQ-N @ {: before:n :}
      EQ-REGISTER
      EQ-N @ before = if E-SPEC-ARITY throw then    \ derived adjoint failed to register (grammar bug)
      EQ-N @ 1-  fwd i EQ-ADJ!
   loop
   -1 fwd EQ-DIFF-SET!
   EQ-FWD-NM EQ-FWD-NM-U @ SP-NAME!
   EQ-FWD-SRC EQ-FWD-SRC-U @ SP-LOAD$  SP-PARSE ;   \ restore the forward queryable dataflow

\ ---- batched adjoint (BTC-2). The adjoint of a batched contraction is the batched
\ TRANSPOSED contraction with the SAME batch extents riding along: for
\ S[b h i j] = Σk Q[b h i k] · K[b h j k], factor Q's gradient is
\ dQ[b h i k] = dS[b h i j] · K[b h j k] +SUM j - another batched equation whose batch
\ axes (b,h) are free on both sides, so they are NEVER summed (batch isolation is
\ structural). EQ-ADJ-BODY builds exactly that source (it excludes Fj's own indices from
\ the contraction, and batch indices are Fj's indices), so the reuse is total; the batched
\ SP-EMIT-BODY generates the dFj words. Batched equations are not composable (rank>2
\ factors do not map to the 2D op registry, spec.f EQ-COMPOSABLE?), so this generates the
\ <fwd>-ADJj words for gradcheck WITHOUT registering them - the same "generate, don't
\ register" split the design records for stage-2 composite-index work.
: EQ-BATCHED-ADJ-DERIVE ( -- )
   EQ-HAS-GATHER? if exit then                     \ gather adjoint = scatter-add: forward-only
   EQ-ADJ-DERIVABLE? 0= if exit then               \ adjoint outside the grammar: forward-only
   EQ-FWD-NM!
   SP-SRC EQ-FWD-SRC SP-SRC-U @ BYTE-COPY  SP-SRC-U @ EQ-FWD-SRC-U !
   SP-FAC-N @ {: kk:n :}
   EQ-ADJ-SRC-BUILD                                \ build ALL bodies before the first SP-PARSE clobbers state
   kk 0 ?do
      i EQ-ADJ-NAME!
      EQ-ADJ-SRC i ADJ-SRC-CAP * +  EQ-ADJ-SRCL i cells + @  SP-LOAD$
      SP-PARSE  SP-VALIDATE  SP-EMIT-BODY
   loop
   EQ-FWD-NM EQ-FWD-NM-U @ SP-NAME!
   EQ-FWD-SRC EQ-FWD-SRC-U @ SP-LOAD$  SP-PARSE  SP-CLASSIFY-BATCH ;   \ restore forward dataflow + batch record

\ derive + register the adjoint equations for the equation SPEC: just registered. Composable
\ equations are gather-free (EQ-COMPOSABLE?); a non-composable, gather, or out-of-grammar
\ adjoint leaves the equation forward-only (EQ-DIFF? = 0). Runs AFTER EQ-REGISTER: the forward
\ parse state is still intact for the source build; each adjoint's SP-PARSE then reuses it.
: EQ-ADJ-DERIVE ( -- )
   SP-BATCH-N @ 0 > if EQ-BATCHED-ADJ-DERIVE exit then   \ batched: derive dFj words (not registered)
   EQ-COMPOSABLE? 0= if exit then                 \ not registered: no slot to attach adjoints to
   SP-EW? @ if EQ-EW-ADJ-DERIVE exit then          \ elementwise/broadcast form: its own adjoint rule
   EQ-N @ 1- >EQ-SLOT {: fwd:eq-slot :}
   EQ-HAS-GATHER? if exit then                     \ scatter-add adjoint: forward-only
   EQ-ADJ-DERIVABLE? 0= if exit then               \ adjoint outside the grammar: forward-only
   EQ-FWD-NM!
   SP-SRC EQ-FWD-SRC SP-SRC-U @ BYTE-COPY  SP-SRC-U @ EQ-FWD-SRC-U !   \ save the forward body
   SP-FAC-N @ {: kk:n :}
   EQ-ADJ-SRC-BUILD                                \ build ALL bodies before the first SP-PARSE clobbers state
   kk 0 ?do
      i EQ-ADJ-NAME!
      EQ-ADJ-SRC i ADJ-SRC-CAP * +  EQ-ADJ-SRCL i cells + @  SP-LOAD$
      SP-PARSE  SP-VALIDATE  SP-EMIT-EL  SP-EMIT-OUTER
      EQ-N @ {: before:n :}
      EQ-REGISTER
      EQ-N @ before = if E-SPEC-ARITY throw then   \ derived adjoint failed to register (grammar bug)
      EQ-N @ 1-  fwd i EQ-ADJ!
   loop
   -1 fwd EQ-DIFF-SET!
   \ the adjoint SP-PARSEs clobbered the forward's queryable dataflow (SPEC-* readers), so
   \ restore it by re-parsing the saved forward body + name.
   EQ-FWD-NM EQ-FWD-NM-U @ SP-NAME!
   EQ-FWD-SRC EQ-FWD-SRC-U @ SP-LOAD$  SP-PARSE ;

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
   SP-EMIT-BODY
   EQ-REGISTER
   EQ-ADJ-DERIVE ;

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
   XG-RESET  SP-CAND-CORE ;
: SPEC-CAND$ ( -- ptr u8 n )  XG$ ;
\ SPEC-RCAND: NAME <spec> ; - like SPEC-CAND: but leaves the contraction-legality witness
\ (SP-RSUM-CORE) candidate text, so a test can score the BTC-7 free-contraction reject SPEC:
\ generates: an inner contraction extent scores -1 (accept), a free (batch) one scores 0.
: SPEC-RCAND: ( -- )
   parse-name SP-NAME!  SP-COLLECT  SP-PARSE  SP-VALIDATE
   XG-RESET  SP-RSUM-CORE ;
: SPEC-RCAND$ ( -- ptr u8 n )  XG$ ;

\ SPEC-ADJ-CHECK$: the training gate as a string seam. Parse + validate a spec BODY, then
\ attempt to derive its adjoints: a gather equation is the named E-CAD-GRAD reject (its
\ scatter-add adjoint is not expressible), an out-of-grammar adjoint is E-SPEC-ARITY, and a
\ differentiable equation validates every derived adjoint through the same pipeline (no code
\ generated). This proves "never a wrong gradient" without needing a captured graph.
\ elementwise arm of the training gate: an out-of-grammar elementwise adjoint (e.g. a MUL
\ with too many factors) is the named E-CAD-GRAD reject; otherwise every derived elementwise
\ adjoint re-parses + re-validates through the same pipeline (no code generated).
: SP-EW-ADJ-CHECK ( -- )
   EQ-EW-ADJ-DERIVABLE? 0= if E-CAD-GRAD throw then
   SP-FAC-N @ {: kk:n :}
   EQ-EW-ADJ-SRC-BUILD
   kk 0 ?do
      EQ-ADJ-SRC i ADJ-SRC-CAP * +  EQ-ADJ-SRCL i cells + @  SP-LOAD$  SP-PARSE  SP-VALIDATE
   loop ;
: SPEC-ADJ-CHECK$ ( ptr u8 n -- )
   s" cand" SP-NAME!  SP-LOAD$  SP-PARSE  SP-VALIDATE
   SP-EW? @ if SP-EW-ADJ-CHECK exit then
   EQ-HAS-GATHER? if E-CAD-GRAD throw then
   EQ-ADJ-DERIVABLE? 0= if E-SPEC-ARITY throw then
   SP-FAC-N @ {: kk:n :}
   EQ-ADJ-SRC-BUILD
   kk 0 ?do
      EQ-ADJ-SRC i ADJ-SRC-CAP * +  EQ-ADJ-SRCL i cells + @  SP-LOAD$  SP-PARSE  SP-VALIDATE
   loop ;

\ ---- derivation (3): PROMOTE shape obligations - the extent MAGNITUDES the output
\ shape and the contraction span impose. Integration boundary: a PROMOTE gate in
\ maki/cad.f (alongside PROMOTE-OK?/PROMOTE-NPOL) would read these; no such gate
\ exists yet (scout: PROMOTE consumes verdicts, not shapes), so this is the
\ self-contained record PROMOTE will consume when that gate lands.
: SPEC-FREE-EXTENT@ ( n -- n ) {: i:n :}  i SPEC-FREE@ SP-EXT-SLOT XR-VAL@ ;
: SPEC-CT-EXTENT@   ( n -- n ) {: i:n :}  i SPEC-CT@   SP-EXT-SLOT XR-VAL@ ;

;package
