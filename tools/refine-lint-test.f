\ refine-lint-test.f - checked fixtures for the TRUSTED mint confinement lint.
\ Red fixture: a scratch source file outside the tree containing an
\ out-of-owner-file mint call must fire with file:line; green: the real tree
\ scan reports 0 findings. Every mint name in this file lives inside a string
\ literal so the lint's own string-body exclusion keeps the tree scan green.
\ Run: bin/hb --load tools/refine-lint-test.f

require lib/date.f
require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require tools/lint/text.f
require tools/lint/token.f
require tools/lint/lib.f
require tools/trust-lint-core.f
require tools/refine-lint-core.f

package RFL-TEST

4096 constant OUT-CAP
18 constant IR-PUBLIC-PKG#
9 constant IR-KIND#
2 constant IR-RAW-FORM#
57 constant DEF-FORM#

variable ROOT-U
variable FILE-U
variable MAN-U

create ROOT-BUF FS-PATH-CAP allot
create FILE-BUF FS-PATH-CAP allot
create MAN-BUF FS-PATH-CAP allot
create OUT-BUF OUT-CAP allot
create RAW-NAME-BUF 64 allot

: ROOT$ ( -- ptr u8 n ) ROOT-BUF ROOT-U @ ;
: FILE$ ( -- ptr u8 n ) FILE-BUF FILE-U @ ;
: MAN$ ( -- ptr u8 n ) MAN-BUF MAN-U @ ;

: ROOT! ( ptr u8 n -- ) {: a:ptr u:n :}
   a ROOT-BUF u BYTE-COPY
   u ROOT-U ! ;

: DQ ( -- ) 34 SB-APPEND-C ;
: IR-NL ( -- ) 10 SB-APPEND-C ;

: SETUP ( -- )
   RFL:BUFFERS
   RFL:RESET
   RFL:INVENTORY
   RFL:FINDINGS 0 T= ;                             \ seed/manifest cross-check is clean

: SHAPE ( -- )
   s" n -- CAD-KIND:rows" RFL:MINT-SHAPE? TTRUE
   s" n -- MIR:input-slot" RFL:MINT-SHAPE? TTRUE
   s" ptr n -- ptr CAD-KIND:dim" RFL:MINT-SHAPE? TTRUE
   s" CAD-KIND:rows -- n" RFL:MINT-SHAPE? TFALSE       \ projection direction stays per-site policy
   s" n -- tensor" RFL:MINT-SHAPE? TFALSE              \ bare-nominal mints are seed-only
   s" n n -- matrix<space-global,f32,m,q>" RFL:MINT-SHAPE? TFALSE
   s" ptr u8 n -- bool" RFL:MINT-SHAPE? TFALSE
   s" -- CAD-KIND:dim" RFL:MINT-SHAPE? TFALSE          \ no raw input to refine
   s" n -- rows:" RFL:MINT-SHAPE? TFALSE               \ edge colons are not family tokens
   s" n -- :rows" RFL:MINT-SHAPE? TFALSE ;

: DETECT ( -- )
   s" 7 ROWS-REFINE drop" RFL:COUNT-STR 1 T=
   s" 7 MAKI:ROWS-REFINE drop" RFL:COUNT-STR 1 T=      \ qualified reference is still a reference
   s" 7 rows-refine drop" RFL:COUNT-STR 1 T=           \ the dictionary is case-insensitive
   s" 1 RAW>NODE 2 RAW>SLOT" RFL:COUNT-STR 2 T=
   s" 1 RAW>ANODE 2 RAW>ASTREAM 3 RAW>AEVENT" RFL:COUNT-STR 3 T=
   s" 3 N>LBTK drop" RFL:COUNT-STR 1 T= ;              \ seed-only mint without a manifest row

: STR-CONTENT$ ( -- ptr u8 n )
   SB-RESET
   s" : X ( -- ) s" SB-APPEND DQ
   s"  has ROWS-REFINE within" SB-APPEND DQ
   s"  2drop ;" SB-APPEND
   SB$ ;

: ESC-CONTENT$ ( -- ptr u8 n )
   SB-RESET
   s" S\" SB-APPEND DQ
   s"  has ROWS-REFINE bytes" SB-APPEND DQ
   s"  drop" SB-APPEND
   SB$ ;

: NO-FALSE-POSITIVE ( -- )
   s" \ prose mentioning ROWS-REFINE in a line comment" RFL:COUNT-STR 0 T=
   s" : F ( n -- n ) dup ; ( ROWS-REFINE in a paren comment )" RFL:COUNT-STR 0 T=
   STR-CONTENT$ RFL:COUNT-STR 0 T=                \ s" string bodies are excluded
   ESC-CONTENT$ RFL:COUNT-STR 0 T=                \ S\" escaped-string bodies too
   s" MY-ROWS-REFINE drop" RFL:COUNT-STR 0 T=          \ whole-token matching only
   s" ROWS-REFINED drop" RFL:COUNT-STR 0 T=
   s" :ROWS-REFINE drop" RFL:COUNT-STR 0 T= ;          \ edge colon is not a qualifier

: IR-PUBLIC-PKG$ ( n -- ptr u8 n )
   case
      0 of s" IR-ID" endof
      1 of s" IR" endof
      2 of s" HIR" endof
      3 of s" SIR" endof
      4 of s" LIR" endof
      5 of s" A64IR" endof
      6 of s" GPU-RIR" endof
      7 of s" GPU-KIR" endof
      8 of s" GPU-GIR" endof
      9 of s" GPU-PTXIR2" endof
      10 of s" GPU-IR" endof
      11 of s" IR-SOURCE" endof
      12 of s" IR-TYPE" endof
      13 of s" IR-ATTR" endof
      14 of s" IR-SCHEMA" endof
      15 of s" IR-BUILD" endof
      16 of s" IR-VERIFY" endof
      17 of s" IR-CODEC" endof
      E-TBL-BOUNDS throw
   endcase ;

: IR-KIND$ ( n -- ptr u8 n )
   case
      0 of s" SOURCE" endof
      1 of s" FUN" endof
      2 of s" BLOCK" endof
      3 of s" OP" endof
      4 of s" VALUE" endof
      5 of s" TYPE" endof
      6 of s" ATTR" endof
      7 of s" SYMBOL" endof
      8 of s" SPAN" endof
      E-TBL-BOUNDS throw
   endcase ;

: IR-API$ ( n n -- ptr u8 n ) {: kind:n form:n :}
   SB-RESET
   form
   case
      0 of s" MINT-" SB-APPEND kind IR-KIND$ SB-APPEND endof
      1 of kind IR-KIND$ SB-APPEND s" >N" SB-APPEND endof
      2 of s" PACK-" SB-APPEND kind IR-KIND$ SB-APPEND endof
      3 of kind IR-KIND$ SB-APPEND s" -OWNER" SB-APPEND endof
      4 of kind IR-KIND$ SB-APPEND s" -LOCAL" SB-APPEND endof
      5 of kind IR-KIND$ SB-APPEND s" -CHECK" SB-APPEND endof
      E-TBL-BOUNDS throw
   endcase
   SB$ ;

: DEF-FORM$ ( n -- ptr u8 n )
   case
      0 of s" :" endof
      1 of s" +:" endof
      2 of s" CHECKED:" endof
      3 of s" TRUSTED:" endof
      4 of s" KERNEL:" endof
      5 of s" CAST:" endof
      6 of s" MODEL:" endof
      7 of s" SUMTYPE" endof
      8 of s" PRODUCT" endof
      9 of s" ENUM" endof
      10 of s" STRUCTURE" endof
      11 of s" VALUE-RECORD" endof
      12 of s" BEGIN-STRUCTURE" endof
      13 of s" constant" endof
      14 of s" 2constant" endof
      15 of s" fconstant" endof
      16 of s" variable" endof
      17 of s" 2variable" endof
      18 of s" fvariable" endof
      19 of s" create" endof
      20 of s" value" endof
      21 of s" defer" endof
      22 of s" LAYOUT-BUFFER" endof
      23 of s" DEFER-LAYOUT-BUFFER" endof
      24 of s" TYPED-BUFFER" endof
      25 of s" TYPED-VARIABLE" endof
      26 of s" PTR-VARIABLE" endof
      27 of s" PTR-FIELD:" endof
      28 of s" CFIELD:" endof
      29 of s" +FIELD" endof
      30 of s" NEWTYPE" endof
      31 of s" DEFTYPE" endof
      32 of s" DEFLINEAR" endof
      33 of s" ENUM+" endof
      34 of s" ENUM4+" endof
      35 of s" BUFFER:" endof
      36 of s" BUFFER" endof
      37 of s" BUFFER-E" endof
      38 of s" CODEGEN:BUFFER" endof
      39 of s" CODEGEN:BUFFER-E" endof
      40 of s" TASK" endof
      41 of s" +USER" endof
      42 of s" FACILITY" endof
      43 of s" TASK:TASK" endof
      44 of s" TASK:+USER" endof
      45 of s" TASK:FACILITY" endof
      46 of s" TR-FILES:" endof
      47 of s" GE-FILES:" endof
      48 of s" IOP:" endof
      49 of s" CONST" endof
      50 of s" ARR" endof
      51 of s" EXTENT:" endof
      52 of s" FREE-EXTENT:" endof
      53 of s" EXTPROD:" endof
      54 of s" TENSOR:" endof
      55 of s" ITENSOR:" endof
      56 of s" SPEC:" endof
      E-TBL-BOUNDS throw
   endcase ;

: DEF-KIND ( n -- n ) {: k:n :}
   k 7 < if LINT-DEF:COLON exit then
   k 13 < if k 5 - exit then
   LINT-DEF:DATA ;

: DEF-CLOSE$ ( n -- ptr u8 n )
   case
      LINT-DEF:COLON of s" ;" endof
      LINT-DEF:SUMTYPE of s" ;SUMTYPE" endof
      LINT-DEF:PRODUCT of s" ;PRODUCT" endof
      LINT-DEF:ENUM of s" ;ENUM" endof
      LINT-DEF:STRUCTURE of s" ;STRUCTURE" endof
      LINT-DEF:VALUE-RECORD of s" END-VALUE-RECORD" endof
      LINT-DEF:LOW-STRUCTURE of s" END-STRUCTURE" endof
      E-TBL-BOUNDS throw
   endcase ;

: FORM$ ( ptr u8 n ptr u8 n n bool -- ptr u8 n )
   {: pa:ptr pu:n na:ptr nu:n form:n pub:bool :}
   SB-RESET
   s" package " SB-APPEND pa pu SB-APPEND
   pub if s"  public " else s"  private " then SB-APPEND
   form DEF-FORM$ SB-APPEND
   32 SB-APPEND-C na nu SB-APPEND
   form DEF-KIND dup LINT-DEF:DATA <> if
      32 SB-APPEND-C DEF-CLOSE$ SB-APPEND
   else
      drop
   then
   s"  ;package" SB-APPEND
   SB$ ;

: PRIVATE-FORM$ ( ptr u8 n ptr u8 n n -- ptr u8 n )
   LINT-FALSE FORM$ ;

: PUBLIC-FORM$ ( ptr u8 n ptr u8 n n -- ptr u8 n )
   LINT-TRUE FORM$ ;

: PUBLIC-MUTATION$ ( ptr u8 n ptr u8 n -- ptr u8 n )
   {: pa:ptr pu:n na:ptr nu:n :}
   SB-RESET
   s" package " SB-APPEND pa pu SB-APPEND
   s"  public : " SB-APPEND na nu SB-APPEND
   s"  ( -- ) ; ;package" SB-APPEND
   SB$ ;

: PRIVATE-MUTATION$ ( ptr u8 n ptr u8 n -- ptr u8 n )
   {: pa:ptr pu:n na:ptr nu:n :}
   SB-RESET
   s" package " SB-APPEND pa pu SB-APPEND
   s"  private : " SB-APPEND na nu SB-APPEND
   s"  ( -- ) ; ;package" SB-APPEND
   SB$ ;

: EXPORT-MUTATION$ ( ptr u8 n ptr u8 n -- ptr u8 n )
   {: pa:ptr pu:n na:ptr nu:n :}
   SB-RESET
   s" package " SB-APPEND pa pu SB-APPEND
   s"  private EXPORT " SB-APPEND na nu SB-APPEND
   s"  ;package" SB-APPEND
   SB$ ;

: QUALIFIED-MUTATION$ ( ptr u8 n ptr u8 n -- ptr u8 n )
   {: pa:ptr pu:n na:ptr nu:n :}
   SB-RESET
   s" : " SB-APPEND pa pu SB-APPEND
   s" :" SB-APPEND na nu SB-APPEND
   s"  ( -- ) ;" SB-APPEND
   SB$ ;

: RAW-PRIVATE-FAILS ( ptr u8 n -- )
   {: na:ptr nu:n :}
   na RAW-NAME-BUF nu BYTE-COPY
   s" test/compiler-ir-mutation.f"
      s" IR" RAW-NAME-BUF nu PRIVATE-MUTATION$ RFL:COUNT-STR-AT 1 T= ;

: RAW-TABLE-CASE ( ptr u8 n -- ) {: na:ptr nu:n :}
   na RAW-NAME-BUF nu BYTE-COPY
   RAW-NAME-BUF nu RFL:RAW-NAME? TTRUE
   RAW-NAME-BUF nu RAW-PRIVATE-FAILS
   s" src/compiler/ir/id.f"
      s" IR-ID" RAW-NAME-BUF nu 5 PRIVATE-FORM$
      RFL:COUNT-STR-AT 0 T=
   s" src/compiler/ir/id.f"
      s" IR-ID" RAW-NAME-BUF nu 0 PRIVATE-FORM$
      RFL:COUNT-STR-AT 1 T=
   s" src/compiler/ir/id.f"
      s" IR-ID" RAW-NAME-BUF nu 3 PRIVATE-FORM$
      RFL:COUNT-STR-AT 1 T=
   s" src/compiler/ir/id.f"
      s" IR-ID" RAW-NAME-BUF nu 16 PRIVATE-FORM$
      RFL:COUNT-STR-AT 1 T=
   s" test/compiler-ir-mutation.f"
      s" IR-ID" RAW-NAME-BUF nu 5 PRIVATE-FORM$
      RFL:COUNT-STR-AT 1 T=
   s" src/compiler/ir/id.f"
      s" IR" RAW-NAME-BUF nu 5 PRIVATE-FORM$
      RFL:COUNT-STR-AT 1 T=
   s" src/compiler/ir/id.f"
      s" IR-ID" RAW-NAME-BUF nu 5 PUBLIC-FORM$
      RFL:COUNT-STR-AT 1 T=
   s" src/compiler/ir/id.f"
      s" OTHER" RAW-NAME-BUF nu QUALIFIED-MUTATION$
      RFL:COUNT-STR-AT 0 T=
   s" src/compiler/ir/id.f"
      s" OTHER" RAW-NAME-BUF nu 0 PRIVATE-FORM$
      RFL:COUNT-STR-AT 0 T=
   s" src/compiler/ir/id.f"
      s" IR-ID" RAW-NAME-BUF nu EXPORT-MUTATION$
      RFL:COUNT-STR-AT 1 T= ;

: RAW-TABLE-COVERAGE ( -- )
   RFL:RAW-NAME-COUNT 26 T=
   s" MINT-KEY" RAW-TABLE-CASE
   s" KEY>N" RAW-TABLE-CASE
   s" MINT-MODULE" RAW-TABLE-CASE
   s" MODULE>N" RAW-TABLE-CASE
   s" MINT-COUNT" RAW-TABLE-CASE
   s" COUNT>N" RAW-TABLE-CASE
   s" MINT-POOL-OFF" RAW-TABLE-CASE
   s" POOL-OFF>N" RAW-TABLE-CASE
   IR-KIND# 0 ?do
      IR-RAW-FORM# 0 ?do
         j i IR-API$ RAW-TABLE-CASE
      loop
   loop
   s" PACK-SOURCE" RFL:RAW-NAME? TFALSE
   s" SOURCE-OWNER" RFL:RAW-NAME? TFALSE
   s" SOURCE-LOCAL" RFL:RAW-NAME? TFALSE
   s" SOURCE-CHECK" RFL:RAW-NAME? TFALSE
   s" MINT-MODULE-X" RFL:RAW-NAME? TFALSE
   s" X-MODULE>N" RFL:RAW-NAME? TFALSE
   s" PACK-MODULE" RFL:RAW-NAME? TFALSE ;

: PRIVATE-FORM-MUTATIONS ( -- )
   LINT-DEF:FORM-COUNT DEF-FORM# T=
   IR-PUBLIC-PKG# 0 ?do
      DEF-FORM# 0 ?do
         s" test/compiler-ir-mutation.f"
            j IR-PUBLIC-PKG$ s" MINT-MODULE" i PRIVATE-FORM$
            RFL:COUNT-STR-AT 1 T=
      loop
   loop ;

: OWNER-FORM-MUTATIONS ( -- )
   IR-PUBLIC-PKG# 0 ?do
      DEF-FORM# 0 ?do
         s" src/compiler/ir/id.f"
            j IR-PUBLIC-PKG$ s" MINT-MODULE" i PRIVATE-FORM$
            RFL:COUNT-STR-AT
            j 0= i 5 = and if 0 else 1 then T=
      loop
   loop ;

: EXPORT-MUTATIONS ( -- )
   IR-PUBLIC-PKG# 0 ?do
      s" test/compiler-ir-mutation.f"
         i IR-PUBLIC-PKG$ s" MINT-MODULE" EXPORT-MUTATION$
         RFL:COUNT-STR-AT 1 T=
      s" test/compiler-ir-mutation.f"
         i IR-PUBLIC-PKG$ s" GLOBAL:COUNT>N" EXPORT-MUTATION$
         RFL:COUNT-STR-AT 1 T=
   loop
   s" test/compiler-ir-mutation.f" s" EXPORT IR:COUNT>N"
      RFL:COUNT-STR-AT 0 T=
   s" test/compiler-ir-mutation.f"
      s" package IR private : RESOLVE ( n -- n ) COUNT>N ; ;package"
      RFL:COUNT-STR-AT 0 T= ;

: PUBLICATION-MUTATIONS ( -- )
   IR-PUBLIC-PKG# 0 ?do
      s" test/compiler-ir-mutation.f"
         i IR-PUBLIC-PKG$ s" MINT-MODULE" PUBLIC-MUTATION$
         RFL:COUNT-STR-AT 1 T=
      s" test/compiler-ir-mutation.f"
         i IR-PUBLIC-PKG$ s" MINT-MODULE" QUALIFIED-MUTATION$
         RFL:COUNT-STR-AT 1 T=
   loop
   s" test/compiler-ir-mutation.f"
      s" IR-RAW" s" MINT-MODULE" QUALIFIED-MUTATION$
      RFL:COUNT-STR-AT 1 T=
   s" test/compiler-ir-mutation.f" s" package OTHER public : MINT-MODULE ( -- ) ; ;package"
      RFL:COUNT-STR-AT 0 T=
   s" test/compiler-ir-mutation.f" s" : OTHER:MINT-MODULE ( -- ) ;"
      RFL:COUNT-STR-AT 0 T=
   s" test/compiler-ir-mutation.f" s" : IR:MINT-MODULE-X ( -- ) ;"
      RFL:COUNT-STR-AT 0 T=
   s" test/compiler-ir-mutation.f" s" : IR:MINT-MODULE:EXTRA ( -- ) ;"
      RFL:COUNT-STR-AT 0 T=
   s" test/compiler-ir-mutation.f"
      s" package IR public : PACK-SOURCE ( -- ) ; ;package"
      RFL:COUNT-STR-AT 0 T=
   s" src/compiler/ir/id.f"
      s" package IR-ID private CAST: MINT-MODULE ( n -- IR-ID:ir-module-id ) ; ;package"
      RFL:COUNT-STR-AT 0 T=
   s" src/compiler/ir/id.f"
      s" package IR-ID public CAST: MINT-MODULE ( n -- IR-ID:ir-module-id ) ; ;package"
      RFL:COUNT-STR-AT 1 T=
   s" src/compiler/ir/id.f"
      s" package IR-ID private CAST: EXTRA ( n -- IR-ID:ir-module-id ) ; ;package"
      RFL:COUNT-STR-AT 1 T=
   s" src/compiler/ir/id.f"
      s" package IR-ID public CAST: EXTRA ( n -- IR-ID:ir-module-id ) ; ;package"
      RFL:COUNT-STR-AT 1 T= ;

: IR-RAW-STRING$ ( -- ptr u8 n )
   SB-RESET
   s" : X ( -- ) s" SB-APPEND DQ
   s" package IR-RAW" SB-APPEND DQ
   s" 2drop ;" SB-APPEND
   SB$ ;

: IR-RAW-SPLIT$ ( -- ptr u8 n )
   SB-RESET
   s" package" SB-APPEND IR-NL
   s" IR-RAW" SB-APPEND IR-NL
   s" ;package" SB-APPEND
   SB$ ;

: IR-RAW-MULTILINE-PAREN$ ( bool -- ptr u8 n ) {: hostile:bool :}
   SB-RESET
   s" package IR-RAW" SB-APPEND IR-NL
   s" ( multiline" SB-APPEND IR-NL
   s" public MINT-MODULE" SB-APPEND IR-NL
   s" )" SB-APPEND IR-NL
   hostile if s" public" SB-APPEND IR-NL then
   s" ;package" SB-APPEND
   SB$ ;

: IR-RAW-MULTILINE-STRING$ ( bool -- ptr u8 n ) {: hostile:bool :}
   SB-RESET
   s" package IR-RAW s" SB-APPEND DQ
   s"  multiline" SB-APPEND IR-NL
   s" public MINT-MODULE" SB-APPEND DQ
   s"  2drop" SB-APPEND IR-NL
   hostile if s" public" SB-APPEND IR-NL then
   s" ;package" SB-APPEND
   SB$ ;

: IR-AUTHORITY-CONFINEMENT ( -- )
   s" legacy package authority" T-LABEL
   \ The deleted lexical authority package cannot be reintroduced anywhere.
   s" src/compiler/ir/id.f" s" package IR-RAW ;package" RFL:COUNT-STR-AT 1 T=
   s" src/compiler/ir/arena.f" s" package IR-RAW ;package" RFL:COUNT-STR-AT 1 T=
   s" src/compiler/ir/codec.f" s" package IR-RAW ;package" RFL:COUNT-STR-AT 1 T=
   s" test/compiler/ir-id.f" s" package IR-RAW ;package" RFL:COUNT-STR-AT 1 T=
   s" src/compiler/ir/source.f" s" package IR-RAW ;package" RFL:COUNT-STR-AT 1 T=
   s" test/other.f" s" PaCkAgE ir-raw ;package" RFL:COUNT-STR-AT 1 T=
   s" test/other.f" IR-RAW-SPLIT$ RFL:COUNT-STR-AT 1 T=
   \ Duplicates are separate authority violations.
   s" test/other.f" s" package IR-RAW ;package package IR-RAW ;package"
      RFL:COUNT-STR-AT 2 T=
   \ One legacy opener is one finding; mode tokens do not create extra findings.
   s" src/compiler/ir/id.f" s" package IR-RAW public ;package"
      RFL:COUNT-STR-AT 1 T=
   s" src/compiler/ir/id.f" s" package IR-RAW PuBlIc public ;package"
      RFL:COUNT-STR-AT 1 T=
   s" src/compiler/ir/id.f" s" package IR-RAW ;package public"
      RFL:COUNT-STR-AT 2 T=
   s" multiline legacy authority" T-LABEL
   \ Whole-source lexical state spans newlines; inert bodies add no finding.
   s" src/compiler/ir/id.f" LINT-FALSE IR-RAW-MULTILINE-PAREN$
      RFL:COUNT-STR-AT 1 T=
   s" src/compiler/ir/id.f" LINT-TRUE IR-RAW-MULTILINE-PAREN$
      RFL:COUNT-STR-AT 1 T=
   s" src/compiler/ir/id.f" LINT-FALSE IR-RAW-MULTILINE-STRING$
      RFL:COUNT-STR-AT 1 T=
   s" src/compiler/ir/id.f" LINT-TRUE IR-RAW-MULTILINE-STRING$
      RFL:COUNT-STR-AT 1 T=
   s" inert legacy authority text" T-LABEL
   \ Comments, strings, near names, reordering, and the wrong opener role do not fire.
   s" test/other.f" s" \ package IR-RAW" RFL:COUNT-STR-AT 0 T=
   s" test/other.f" IR-RAW-STRING$ RFL:COUNT-STR-AT 0 T=
   s" test/other.f" s" package IR-RAWER" RFL:COUNT-STR-AT 1 T=
   s" test/other.f" s" IR-RAW package OTHER ;package" RFL:COUNT-STR-AT 0 T=
   s" test/other.f" s" using IR-RAW" RFL:COUNT-STR-AT 0 T=
   s" src/compiler/ir/id.f" s" package IR-RAW PUBLIC-X ;package"
      RFL:COUNT-STR-AT 1 T=
   s" incomplete legacy authority" T-LABEL
   \ File boundaries fail closed on incomplete scope syntax.
   s" src/compiler/ir/id.f" s" package" RFL:COUNT-STR-AT 1 T=
   s" src/compiler/ir/id.f" s" package IR-RAW" RFL:COUNT-STR-AT 2 T=
   s" test/other.f" s" package IR-RAW" RFL:COUNT-STR-AT 2 T=
   s" src/compiler/ir/id.f" s" package IR-RAW ;package"
      RFL:COUNT-STR-AT 1 T= ;

: PACKAGE-SYNTAX ( -- )
   s" test/compiler-ir-mutation.f" s" package package"
      RFL:COUNT-STR-AT 1 T=
   s" test/compiler-ir-mutation.f" s" package ;package"
      RFL:COUNT-STR-AT 1 T=
   s" test/compiler-ir-mutation.f" s" package IR:BAD"
      RFL:COUNT-STR-AT 1 T=
   s" test/compiler-ir-mutation.f" s" package IR package HIR ;package"
      RFL:COUNT-STR-AT 1 T=
   s" test/compiler-ir-mutation.f" s" ;package"
      RFL:COUNT-STR-AT 1 T=
   s" test/compiler-ir-mutation.f" s" public private"
      RFL:COUNT-STR-AT 2 T=
   s" test/compiler-ir-mutation.f" s" package OTHER"
      RFL:COUNT-STR-AT 1 T=
   s" test/compiler-ir-mutation.f" s" package IR"
      RFL:COUNT-STR-AT 1 T=
   s" test/compiler-ir-mutation.f" s" package"
      RFL:COUNT-STR-AT 1 T=
   \ A standalone `(` after the definer is a comment, so the definer lacks its
   \ atomic name operand; a word merely NAMED with a leading paren (habu1.f's
   \ `(CMP)`) IS that operand, so the paren-named definition is well-formed.
   s" test/compiler-ir-mutation.f" s" package IR private : ( n -- ) ; ;package"
      RFL:COUNT-STR-AT 1 T=
   s" test/compiler-ir-mutation.f" s" package IR private : (COMMENT-NAME) ; ;package"
      RFL:COUNT-STR-AT 0 T=
   s" test/compiler-ir-mutation.f" s" package IR ( unterminated"
      RFL:COUNT-STR-AT 1 T= ;

: CONFINE-POLICY ( -- )
   \ owner file is allowed
   s" maki/tensor.f" s" 1 ROWS-REFINE drop" RFL:COUNT-STR-AT 0 T=
   \ a file cited by the mint's TRUSTED.md Tests cell is allowed
   s" maki/model-ir-test.f" s" 0 RAW>SLOT drop" RFL:COUNT-STR-AT 0 T=
   s" maki/async-dag.f" s" 0 RAW>ANODE drop" RFL:COUNT-STR-AT 0 T=
   s" maki/async-dag-test.f" s" 0 RAW>ASTREAM drop" RFL:COUNT-STR-AT 0 T=
   \ another mint's owner is not this mint's boundary
   s" maki/tensor.f" s" 0 RAW>SLOT drop" RFL:COUNT-STR-AT 1 T=
   s" maki/eval/eval.f" s" 0 RAW>AEVENT drop" RFL:COUNT-STR-AT 1 T=
   \ any other tree file is a finding
   s" maki/eval/eval.f" s" 0 RAW>SLOT drop" RFL:COUNT-STR-AT 1 T= ;

: ALLOWLIST ( -- )
   s" maki/eval/eval.f" s" 1 ROWS-REFINE drop" RFL:COUNT-STR-AT 1 T=
   s" ROWS-REFINE" s" maki/eval/eval.f" RFL:ALLOW+
   s" maki/eval/eval.f" s" 1 ROWS-REFINE drop" RFL:COUNT-STR-AT 0 T=
   \ the allow entry is mint-specific
   s" maki/eval/eval.f" s" 1 COLS-REFINE drop" RFL:COUNT-STR-AT 1 T=
   \ reset clears the allowlist
   RFL:RESET
   RFL:INVENTORY
   s" maki/eval/eval.f" s" 1 ROWS-REFINE drop" RFL:COUNT-STR-AT 1 T= ;

: RED-PREPARE ( -- )
   CLEANUP-RESET
   s" habu-refine-lint" TMPDIR-MKDIR ROOT!
   ROOT$ CLEANUP-DIR+
   ROOT$ s" mint.f" FILE-BUF JOIN-PATH FILE-U !
   FILE$ CLEANUP+
   FILE$ s" : FORGE ( n -- n ) ROWS-REFINE ROWS-RAW ;" WRITE-ALL
   ROOT$ s" TRUSTED.md" MAN-BUF JOIN-PATH MAN-U !
   MAN$ CLEANUP+ ;

: RED-SCAN ( -- ptr u8 n )                 \ captured finding output; RFL:FINDINGS holds count
   RFL:CLEAR-FINDINGS
   OUT-BUF OUT-CAP LINT-OUT-BUFFER!
   FILE$ RFL:SCAN-FILE
   LINT-OUT$
   LINT-OUT-BUFFER-OFF ;

: RED ( -- )
   RED-SCAN {: oa:ptr ou:n :}
   RFL:FINDINGS 1 T=                               \ the mint fires; the projection does not
   oa ou s" REFINE-CONFINE " LINT-CONTAINS? TTRUE
   oa ou s" mint.f:1: " LINT-CONTAINS? TTRUE
   oa ou s" ROWS-REFINE" LINT-CONTAINS? TTRUE
   oa ou s" ` referenced outside owner maki/tensor.f" LINT-CONTAINS? TTRUE
   RFL:CLEAR-FINDINGS ;

\ ---- synthetic-manifest coverage for the anti-rot ratchet -------------------

: LF ( -- ) 10 SB-APPEND-C ;

: MAN-HEADER ( -- )
   s" | Word | Effect | Reason | Tests | Site | Last audited |" SB-APPEND LF
   s" |------|--------|--------|-------|------|--------------|" SB-APPEND LF ;

: SHAPE-MAN$ ( -- ptr u8 n )
   SB-RESET MAN-HEADER
   s" | FAKE-MINT | `n -- CAD-KIND:fake` | test | `maki/fake-test.f` | maki/fake.f | 2026-07-13 |" SB-APPEND LF
   SB$ ;

: MAN-LOAD ( ptr u8 n -- ) {: a:ptr u:n :}
   MAN$ a u WRITE-ALL
   ROOT$ TRUST-LINT-ROOT!
   TRUST-LINT-RESET
   TL-SCAN-MANIFEST ;

: NEW-MINT-RED ( -- )
   SHAPE-MAN$ MAN-LOAD
   RFL:REPORT-OFF
   RFL:CLEAR-FINDINGS
   RFL:SHAPE-SCAN
   RFL:FINDINGS 1 T=                               \ an unseeded mint-shaped row is a finding
   RFL:CLEAR-FINDINGS
   RFL:REPORT-ON ;

\ ---- source-derived anti-rot ratchet ----------------------------------------
\ Seed 1 is ROWS-REFINE (owner maki/tensor.f). STALE-SEED liveness now comes from
\ the owner-source declaration, not a manifest row; confinement is name-and-path
\ based, so it is identical for both declarer forms.

: FORM-CONFINED ( ptr u8 n -- ) {: ca:ptr cu:n :}
   \ this declarer form keeps the seed live (no STALE-SEED) ...
   RFL:CLEAR-FINDINGS
   ca cu 1 RFL:STALE-IF-DEAD
   RFL:FINDINGS 0 T=
   \ ... and its raw->nominal forge stays module-private: an out-of-owner
   \ reference is a finding (red), owner + <owner-stem>-test.f are the greens.
   s" maki/eval/eval.f"   s" 1 ROWS-REFINE drop" RFL:COUNT-STR-AT 1 T=
   s" maki/tensor.f"      s" 1 ROWS-REFINE drop" RFL:COUNT-STR-AT 0 T=
   s" maki/tensor-test.f" s" 1 ROWS-REFINE drop" RFL:COUNT-STR-AT 0 T= ;

: DRIFT-RED ( -- )
   RFL:REPORT-OFF
   \ STALE-SEED, source-derived (red-first): a seed whose owner source no longer
   \ declares it (plain `:` colon def, no CAST:/TRUSTED:) trips the ratchet.
   RFL:CLEAR-FINDINGS
   s" : ROWS-REFINE ( n -- CAD-KIND:rows ) ROWS-RAW ;" 1 RFL:STALE-IF-DEAD
   RFL:FINDINGS 1 T=
   \ both declarer forms keep it live and confined (REFINE-CONFINE red-first each):
   s" TRUSTED: ROWS-REFINE ( n -- CAD-KIND:rows ) ;" FORM-CONFINED
   s" CAST: ROWS-REFINE ( n -- CAD-KIND:rows ) ;"    FORM-CONFINED
   \ the manifest Tests cell is no longer consulted: MINT-PATH's old row cited
   \ lib/nominal/nominal-test.f, which is not <owner-stem>-test.f, so absent an
   \ RFL:ALLOW+ entry it is now a finding - the semantics genuinely changed.
   s" lib/nominal/nominal-test.f" s" 1 MINT-PATH drop" RFL:COUNT-STR-AT 1 T=
   RFL:CLEAR-FINDINGS
   RFL:REPORT-ON ;

: RESTORE ( -- )
   RFL:RESET
   RFL:INVENTORY                                \ back to the real TRUSTED.md
   RFL:FINDINGS 0 T=
   CLEANUP-RUN ;

: LIVE ( -- )
   \ the real tree (maki/ lib/ src/ tools/) must be confined -> returns clean
   RFL:RUN ;

public

: MAIN ( -- )
   T-RESET
   SETUP
   SHAPE
   DETECT
   NO-FALSE-POSITIVE
   RAW-TABLE-COVERAGE
   PRIVATE-FORM-MUTATIONS
   OWNER-FORM-MUTATIONS
   EXPORT-MUTATIONS
   PUBLICATION-MUTATIONS
   IR-AUTHORITY-CONFINEMENT
   PACKAGE-SYNTAX
   CONFINE-POLICY
   ALLOWLIST
   RED-PREPARE
   RED
   NEW-MINT-RED
   DRIFT-RED
   RESTORE
   LIVE
   T-REPORT ;

;package

RFL-TEST:MAIN
