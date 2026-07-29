\ checker-model-cases.f - the Habu side of the checker parity gate.
\
\ The module lives in `package CHECKER-MODEL-CASES`. It asks the SHIPPED
\ checker, `src/core/checker.f`, the questions the frozen tables in
\ `package CHECKER-MODEL-PROOF` ask, and asks them in two ways:
\
\   - structurally, by reading the checker's own source through the shared
\     source lexer. The concrete type registry `CT-INIT` and the control-flow
\     dispatch `CF-TOK?` are tables written as code, and this file walks each
\     body token by token against the frozen rows. That is how a type or a
\     control spelling ADDED to the checker and not to the model is caught: the
\     walk runs out of frozen rows, or finds a row it was not expecting, before
\     Rocq is asked anything at all;
\   - behaviourally, by handing each shared program vector to the real
\     `CHECK-QUIET-CANDIDATE!` and demanding the row's one verdict. The same
\     row becomes a Rocq obligation in
\     `test/compiler/checker-model-obligations.f`, so neither side carries a
\     copy of what the program is or of what it must answer.
\
\ Why a structural read and not a runtime probe. Nothing the checker exposes
\ reports "the whole list of concrete types you registered" or "the whole list
\ of control spellings you dispatch". A probe can only ask about a name it
\ already knows, so it can never notice a name nobody wrote down. The source IS
\ the list, and the shared lexer is the only honest way to read it: it drops
\ comments, and it hands back a string literal's payload rather than its bytes
\ as code, so a type name hidden in a comment or a control spelling that only
\ appears inside a string cannot satisfy a row.
\
\ Consumer: `test/compiler/checker-model-proof.f`.

require lib/prelude.f
require lib/errors.f
require lib/string.f
require lib/fmt.f
require lib/test.f
require lib/test/outcome.f
require test/checker-assert.f
require test/compiler/ir-id-source.f
require test/compiler/checker-model-schema.f

package CHECKER-MODEL-CASES
using CHECKER-MODEL-PROOF
public

\ The vectors' prelude. These are the Habu realisation of the model's `wStep1`
\ .. `wKeepAny` word effects and of its `ltok` linear con: one named word per
\ effect, each certifying on its own. Nothing pins them to the model directly,
\ because the vectors already do - change either side and the verdicts move
\ apart. `deflinear` writes a row into the checker's concrete type table, which
\ is one global table and not a wordlist, so the type name it mints is reachable
\ unqualified from any signature however the declaration is scoped; the name is
\ spelled distinctly for that reason.
deflinear cmltok

\ The sum family the MATCH depth vectors eliminate. It is the model's `fmres`:
\ two variants in declaration order, each carrying one `n` of payload. A family
\ is registered in one global type registry, like a linear type, so the name is
\ reachable unqualified from a candidate signature however the declaration is
\ scoped; it is spelled distinctly for that reason.
SUMTYPE cmres 0
  VARIANT cmok  n ;VARIANT
  VARIANT cmerr n ;VARIANT
;SUMTYPE

\ A second sum family whose two variants carry DIFFERENT payloads. It is the
\ model's `fmbool`, and the construct vectors use it for the one question a
\ single-payload family cannot ask: whether the payload a construct consumes
\ comes from the variant or merely from the family. A step that read the family
\ would answer the same for both variants; these two answer differently.
SUMTYPE cmbres 0
  VARIANT cmbf bool ;VARIANT
  VARIANT cmbn n ;VARIANT
;SUMTYPE

\ The two families the scrutinee-pop vectors need, and the only ones here whose
\ bundle is more than two cells. Each variant carries TWO cells, so the bundle is
\ three: two payload slots and the tag. `cmtwin` is `cmwide` again under another
\ name - same variant count, same payloads, same width - so nothing but the
\ family identity distinguishes the two, which is what those vectors are about.
\ They are the model's `fmwide` and `fmtwin`.
SUMTYPE cmwide 0
  VARIANT cmwa n n ;VARIANT
  VARIANT cmwb n n ;VARIANT
;SUMTYPE

SUMTYPE cmtwin 0
  VARIANT cmta n n ;VARIANT
  VARIANT cmtb n n ;VARIANT
;SUMTYPE

: STEP1 ( i64 -- i64 ) ;
: MK-CELL ( -- cell ) 0 ;
: MK-BOOL ( -- bool ) 0 0< ;
: DUP1 ( i64 -- i64 i64 ) dup ;
: DROP1 ( i64 -- ) drop ;
: MK-N ( -- n ) 0 ;
: DROP-N ( n -- ) drop ;
: DUP-POLY ( a -- a a ) dup ;
: DROP-POLY ( a -- ) drop ;
: KEEP-POLY ( a -- a ) ;

\ An ORDINARY word that moves its argument to the return row, which `>r` also
\ does. The two are not interchangeable and that is the point: `>r` is its own
\ rule and takes ONE linear snapshot around the whole transfer, while a call
\ runs the conservation check inside the data-row step, before the return rows
\ move - so at the moment the check runs the value is on neither row. This is
\ the model's `wToRAsWord`.
: TO-R-WORD ( a | -- | a ) >r ;

private

variable CUR-K
variable END-K
variable HITS

\ ---- walking one definition body --------------------------------------------
\ A cursor over the word tokens of a body span. Comment tokens are stepped over
\ by the shared reader, so a comment can neither break a run apart nor stand in
\ for one of its tokens.

: SPAN-OPEN ( ptr u8 n -- ) {: a:ptr u:n :}
   a u COMPILER-ID-SRC:BODY-SPAN END-K ! CUR-K ! ;

: W-MORE? ( -- bool )
   begin CUR-K @ END-K @ < while
      CUR-K @ COMPILER-ID-SRC:WORD-TOKEN? if true exit then
      CUR-K @ 1+ CUR-K !
   repeat
   false ;

: W-TAKE ( -- n )
   W-MORE? 0= if E-CMP-ROW throw then
   CUR-K @ dup 1+ CUR-K ! ;

: W-TOK$ ( -- ptr u8 n )
   W-TAKE COMPILER-ID-SRC:TOKEN$ ;

: OPENER$ ( -- ptr u8 n )
   S\" s\"" ;

\ ---- the concrete type vocabulary -------------------------------------------
\ `CT-INIT` is a run of `s" <name>" <code> <class> <width> <sign> CT-SET` rows.
\ Six tokens per row, thirty rows, nothing else - and the walk asserts the
\ nothing else by demanding the body is exhausted when the rows are.

: WORD-LIT ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u COMPILER-ID-SRC:CONST@ ;

: CLASS-CODE-FOR ( ptr u8 n -- n ) {: wa:ptr wu:n :}
   CLASSES 0 ?do
      i CLS-WORD$ wa wu STR= if i CLS-CODE@ unloop exit then
   loop
   E-CMP-VOCAB throw ;

: SIGN-CODE-FOR ( ptr u8 n -- n ) {: wa:ptr wu:n :}
   SIGNS 0 ?do
      i SGN-WORD$ wa wu STR= if i SGN-CODE@ unloop exit then
   loop
   E-CMP-VOCAB throw ;

: WIDTH$ ( n -- ptr u8 n ) {: w:n :}
   SB-RESET w FMT:SB-INT SB$ ;

: VOCAB-SOURCE-ROW ( n -- ) {: k:n :}
   s" the type registry writes this row's name through a string literal" T-LABEL
   W-TOK$ OPENER$ T$=
   s" and that literal holds exactly the type name the frozen row gives" T-LABEL
   CUR-K @ 1- COMPILER-ID-SRC:TOKEN-CONTENT$ k VOC-NAME$ T$=
   s" it registers that name under the code word this row names" T-LABEL
   W-TOK$ k VOC-CODEW$ T$=
   s" at the class this row names" T-LABEL
   W-TOK$ k VOC-CLASSW$ T$=
   s" at the width this row names" T-LABEL
   W-TOK$ k VOC-WIDTH@ WIDTH$ T$=
   s" with the sign this row names" T-LABEL
   W-TOK$ k VOC-SIGNW$ T$=
   s" and closes the row with the registry's own setter" T-LABEL
   W-TOK$ VOCAB-SET-WORD$ T$= ;

: VOCAB-CODE-ROW ( n -- ) {: k:n :}
   s" the code word this row names is declared exactly once" T-LABEL
   k VOC-CODEW$ COMPILER-ID-SRC:CONSTS 1 T=
   s" and carries the code the frozen table gives it" T-LABEL
   k VOC-CODEW$ WORD-LIT k VOC-CODE@ T=
   s" the class word this row names carries the frozen class" T-LABEL
   k VOC-CLASSW$ WORD-LIT k VOC-CLASSW$ CLASS-CODE-FOR T=
   s" the sign word this row names carries the frozen sign" T-LABEL
   k VOC-SIGNW$ WORD-LIT k VOC-SIGNW$ SIGN-CODE-FOR T= ;

: VOCAB-CLASS-ROW ( n -- ) {: k:n :}
   s" every class word the vocabulary names is declared exactly once" T-LABEL
   k CLS-WORD$ COMPILER-ID-SRC:CONSTS 1 T=
   s" and carries the number the frozen table gives it" T-LABEL
   k CLS-WORD$ WORD-LIT k CLS-CODE@ T= ;

: VOCAB-SIGN-ROW ( n -- ) {: k:n :}
   s" every sign word the vocabulary names is declared exactly once" T-LABEL
   k SGN-WORD$ COMPILER-ID-SRC:CONSTS 1 T=
   s" and carries the number the frozen table gives it" T-LABEL
   k SGN-WORD$ WORD-LIT k SGN-CODE@ T= ;

public

: VOCAB-PHASE ( -- )
   VOCAB-WORD$ SPAN-OPEN
   VOCAB 0 ?do i VOCAB-SOURCE-ROW loop
   s" the type registry holds nothing the frozen table does not name" T-LABEL
   W-MORE? 0= TTRUE
   VOCAB 0 ?do i VOCAB-CODE-ROW loop
   CLASSES 0 ?do i VOCAB-CLASS-ROW loop
   SIGNS 0 ?do i VOCAB-SIGN-ROW loop
   s" the whole family of concrete type codes is the frozen table plus its ceiling" T-LABEL
   CODE-PREFIX$ COMPILER-ID-SRC:CONSTS-PREFIXED VOCAB 1+ T=
   s" the whole family of sign codes is the frozen table" T-LABEL
   SIGN-PREFIX$ COMPILER-ID-SRC:CONSTS-PREFIXED SIGNS T=
   s" a declared linear type takes the code just past the frozen table" T-LABEL
   VOCAB-CEIL-WORD$ WORD-LIT VOCAB 1+ T= ;

private

\ ---- the term tags -----------------------------------------------------------

: TAG-PREFIX-COUNT ( ptr u8 n -- n ) {: pa:ptr pu:n :}
   0 HITS !
   TAGS 0 ?do
      i TAG-WORD$ pa pu STARTS-WITH? if HITS @ 1+ HITS ! then
   loop
   HITS @ ;

: TAG-ROW-CK ( n -- ) {: k:n :}
   s" every term tag the frozen table names is declared exactly once" T-LABEL
   k TAG-WORD$ COMPILER-ID-SRC:CONSTS 1 T=
   s" and carries the code the frozen table gives it" T-LABEL
   k TAG-WORD$ WORD-LIT k TAG-CODE@ T= ;

public

: TAG-PHASE ( -- )
   TAGS 0 ?do i TAG-ROW-CK loop
   s" the checker declares no term tag the frozen table has not heard of" T-LABEL
   s" T-" TAG-PREFIX-COUNT
   s" T-" COMPILER-ID-SRC:CONSTS-PREFIXED T=
   s" and no row-cell tag either" T-LABEL
   s" S-" TAG-PREFIX-COUNT
   s" S-" COMPILER-ID-SRC:CONSTS-PREFIXED T= ;

private

\ ---- the control-flow dispatch table -----------------------------------------
\ `CF-TOK?` opens with its locals group and then runs one
\ `a u s" <spelling>" CORE-STR= IF <handler> RES-TRUE EXIT THEN` row per
\ spelling, in the order it tests them, and ends with `RES-FALSE`. The handler
\ is whatever stands between the test and `RES-TRUE`, so a row that latches a
\ diagnostic instead of running a control word is written out rather than
\ special-cased.

: CONTROL-HEAD ( -- )
   s" the control dispatch opens by naming its two token operands" T-LABEL
   W-TOK$ s" {:" T$=
   W-TOK$ s" a:ptr" T$=
   W-TOK$ s" u:n" T$=
   W-TOK$ s" :}" T$= ;

: +HANDLER-TOKEN ( ptr u8 n -- ) {: a:ptr u:n :}
   SB$ nip 0 > if s"  " SB-APPEND then
   a u SB-APPEND ;

: HANDLER$ ( -- ptr u8 n )
   SB-RESET
   begin
      W-TOK$ {: a:ptr u:n :}
      a u s" RES-TRUE" STR= 0=
   while
      a u +HANDLER-TOKEN
   repeat
   SB$ ;

: CONTROL-ROW ( n -- ) {: k:n :}
   s" the control dispatch tests this row's spelling against both operands" T-LABEL
   W-TOK$ s" a" T$=
   W-TOK$ s" u" T$=
   s" through a string literal, so a spelling in a comment cannot stand in" T-LABEL
   W-TOK$ OPENER$ T$=
   s" and the literal holds exactly the spelling the frozen row names" T-LABEL
   CUR-K @ 1- COMPILER-ID-SRC:TOKEN-CONTENT$ k CFT-SPELL$ T$=
   s" compared with the engine's own token comparison" T-LABEL
   W-TOK$ CONTROL-TEST-WORD$ T$=
   s" and, on a match, running the handler the frozen row names" T-LABEL
   W-TOK$ s" IF" T$=
   HANDLER$ k CFT-HANDLER$ T$=
   s" then answering that the token was consumed and leaving" T-LABEL
   W-TOK$ s" EXIT" T$=
   W-TOK$ s" THEN" T$= ;

public

: CONTROL-PHASE ( -- )
   CONTROL-WORD$ SPAN-OPEN
   CONTROL-HEAD
   CONTROLS 0 ?do i CONTROL-ROW loop
   s" a token none of the frozen rows names is not a control token" T-LABEL
   W-TOK$ s" RES-FALSE" T$=
   s" and the dispatch holds nothing the frozen table does not name" T-LABEL
   W-MORE? 0= TTRUE ;

private

\ ---- the control frame kinds -------------------------------------------------
\ The kind number a construct writes is read out of the construct's own body, so
\ a kind moved from one construct to another is a row that no longer matches
\ rather than a number that still exists somewhere in the file.

\ How many tokens a frozen run is: its space-separated fields. The match below
\ needs this because the shared reader's run matcher walks forward from a start
\ index with no end, so without a length a run that BEGAN inside the body could
\ be satisfied by tokens after it - which is exactly the "the kind is written
\ somewhere in the file" reading this row is meant to replace.
: RUN-TOKENS ( ptr u8 n -- n ) {: a:ptr u:n :}
   0 HITS !
   u 0 ?do
      a i + c@ $20 <> if
         i 0 = if HITS @ 1+ HITS ! else
            a i 1- + c@ $20 = if HITS @ 1+ HITS ! then
         then
      then
   loop
   HITS @ ;

: RUNS-IN-BODY ( ptr u8 n ptr u8 n -- n ) {: oa:ptr ou:n ra:ptr ru:n :}
   ra ru RUN-TOKENS {: want:n :}
   0 HITS !
   oa ou COMPILER-ID-SRC:BODY-SPAN {: b:n e:n :}
   e b ?do
      i COMPILER-ID-SRC:WORD-TOKEN? i want + e <= and if
         ra ru i COMPILER-ID-SRC:RUN-AT? if HITS @ 1+ HITS ! then
      then
   loop
   HITS @ ;

: FRAME-ROW ( n -- ) {: k:n :}
   SB-RESET s" the construct that opens frame kind " SB-APPEND
   k FRK-KIND@ FMT:SB-INT s"  writes that kind, once, itself" SB-APPEND SB$ T-LABEL
   k FRK-OWNER$ k FRK-RUN$ RUNS-IN-BODY 1 T= ;

public

: FRAME-PHASE ( -- )
   FRAMES 0 ?do i FRAME-ROW loop ;

private

\ ---- the shared program vectors ----------------------------------------------
\ `CHECK-QUIET-CANDIDATE!` answers -1 certified, 1 unresolvable, 0 refused. The
\ row stores the model's three-way verdict, so the mapping is written once here
\ and an unresolvable can never be read as a refusal.

: VERDICT-OF ( n -- n ) {: answer:n :}
   answer -1 = if V-CERT exit then
   answer 1 = if V-UNCK exit then
   V-REJECT ;

: VECTOR-ROW ( n -- ) {: k:n :}
   SB-RESET s" the shipped checker answers " SB-APPEND
   k VEC-VERD@ VERDICT-NAME$ SB-APPEND s"  for " SB-APPEND
   k VEC-NAME$ SB-APPEND SB$ T-LABEL
   k VEC-SRC$ CHECK-QUIET-CANDIDATE! VERDICT-OF k VEC-VERD@ T= ;

public

: VECTOR-PHASE ( -- )
   VECTORS 0 ?do i VECTOR-ROW loop ;

\ ---- the whole Habu side -----------------------------------------------------

: HABU-SIDE ( -- )
   CHECKER-FILE$ COMPILER-ID-SRC:SCAN-FILE
   VOCAB-PHASE
   TAG-PHASE
   CONTROL-PHASE
   FRAME-PHASE
   VECTOR-PHASE ;

;using
;package
