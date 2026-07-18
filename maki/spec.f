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
\   - The product combiner is `*` (multiply all factors) and the reduction is
\     `+SUM <index>` (sum-contract over the index). `+SUM` is the ASCII spelling
\     of the schematic `+Σ` (rejected: the multi-byte Σ is fragile in byte-oriented
\     Forth source and hard to type).
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
require lib/adt/option.f             \ option<>: index-variable and tensor lookups return present/absent
require lib/type/value-nominal.f     \ NOMINAL:: the factor-index (SP-FI) table index is its own type

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
NOMINAL: SP-FI
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
   SP-SRC SP-POS @ + c@ {: c:n :}  c SP-WS? 0=  c SP-DELIM? 0= and ;
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
: SP-PARSE-FACTOR ( -- )   \ NAME [ index... ]
   SP-WORD {: na:ptr nu:n :}
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
: SP-PARSE-FACTORS ( -- )   \ factors until * or +SUM
   begin
      SP-PEEK dup 0= if 2drop exit then
      2dup s" *" STR= if 2drop exit then
      2dup s" +SUM" STR= if 2drop exit then
      2drop  SP-PARSE-FACTOR
   again ;
variable SP-STAR?
: SP-PARSE-SUM ( -- )   \ '+SUM' peeked; consume it + contraction vars to end
   SP-NEXT 2drop
   begin
      SP-PEEK dup 0= if 2drop exit then
      2drop  SP-WORD SP-CT+
   again ;
: SP-PARSE-REDUCTION ( -- )
   0 SP-STAR? !
   SP-PEEK dup 0= if 2drop exit then
   2dup s" *" STR= if 2drop  SP-NEXT 2drop  -1 SP-STAR? !
      SP-PEEK dup 0= if 2drop exit then
      2dup s" +SUM" STR= if 2drop SP-PARSE-SUM exit then
      2drop E-SPEC-SYNTAX throw
   then
   2dup s" +SUM" STR= if 2drop SP-PARSE-SUM exit then
   2drop E-SPEC-SYNTAX throw ;
: SP-PARSE ( -- )
   0 SP-FREE-N !  0 SP-CT-N !  0 SP-FAC-N !  0 SP-FI-N !
   0 SP-POS !  0 SP-PB? !
   SP-WORD SP-OUT!
   s" [" SP-EXPECT   SP-PARSE-FREE   s" ]" SP-EXPECT
   s" =" SP-EXPECT
   SP-PARSE-FACTORS
   SP-PARSE-REDUCTION
   SP-NEXT dup 0= 0= if 2drop E-SPEC-SYNTAX throw then 2drop ;

\ ---- index-variable -> extent surface (#<UPPER>) and role resolution ----------
create SP-EXT-BUF 16 allot
variable SP-EXT-U
: SP-UC ( n -- n ) {: c:n :}  c 97 >= c 122 <= and if c 32 - else c then ;
: SP-EXT$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}   \ index var -> #<UPPER> surface name
   0 SP-EXT-U !
   [char] # SP-EXT-BUF c!  1 SP-EXT-U !
   u 0 ?do  a i + c@ SP-UC  SP-EXT-BUF SP-EXT-U @ + c!  SP-EXT-U @ 1 + SP-EXT-U !  loop
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
   SP-EMIT-OUTER ;

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
