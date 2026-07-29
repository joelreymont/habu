\ checker-model-schema.f - the shared frozen description of what the checker's
\ two models are models OF.
\
\ The module lives in `package CHECKER-MODEL-PROOF`. Its subject is
\ `src/core/checker.f` and the two machine-checked models of it,
\ `formal/Common/Effects.v` (types, stacks, unification, straight-line
\ composition) and `formal/Common/Control.v` (branches, loops, `case`, `MATCH`,
\ early return, the throw edge, quotations, locals, linear-once conservation).
\
\ It holds data and nothing else. Six tables:
\
\   1. The concrete type vocabulary. One row per type `CT-INIT` registers:
\      the surface name, the `CC-` code word, the class word, the width in
\      bits, the sign word, and the `Effects.con` constructor that stands for
\      it. `test/compiler/checker-model-cases.f` walks `CT-INIT`'s body through
\      the shared source lexer and asks the shipped checker whether that is
\      what it registers, and
\      `test/compiler/checker-model-obligations.f` turns the very same rows into
\      Rocq obligations about `con_code`, `con_cls`, `con_width` and `con_sgn`.
\      Neither side carries a copy. This is the table docs/effects.md was
\      measurably wrong about: it listed eight names where the checker
\      registers thirty.
\
\   2. The class and sign vocabularies the rows above name, each pinned to its
\      literal in the checker and to its `Effects.cls` / `Effects.sgn`
\      constructor. `CT-NONE` has no constructor: it is the absence of a class,
\      and the model has no constructor for an absence.
\
\   3. The term tags. `T-CON` .. `T-PARAM` and `S-ROW` / `S-PUSH` are the
\      checker's whole term and stack vocabulary, and each names the
\      `Effects.ty` or `Effects.stack` constructor that stands for it. `T-ATOM`
\      names none: the rigid host identity is a declared omission in Effects.v's
\      own header, so the row records the omission rather than hiding it.
\
\   4. The control-flow dispatch table. One row per spelling `CF-TOK?`
\      recognises, in the order it tests them, with the handler it runs and the
\      `Control.tok` constructor that stands for it. `do` and `?do` are two
\      spellings of one handler and one constructor, which the table says
\      rather than assumes.
\
\   5. The control frame kinds. One row per kind the checker pushes or
\      mutates, with the exact source run that writes the kind number and a
\      model token sequence that must leave a frame of that kind on top.
\
\   6. The shared program vectors. One row is one small checked definition: the
\      Habu source the real checker is asked to certify, the model
\      configuration and token list the same program is written as, and the ONE
\      verdict both must answer. The two encodings are necessarily different -
\      one is text for a token scanner, the other is an already-scanned token
\      list - but they live in one row and the verdict is written once, so a
\      row cannot be satisfied by editing only one side. The last eighteen rows
\      hold six decisions nothing else here reaches: the widening lattice, the
\      control-frame ceiling, `MATCH`'s own depth guard, the per-step linear
\      conservation count, `construct`, and `MATCH`'s scrutinee pop. Before they
\      existed, halving the ceiling, lowering or deleting the depth guard, making
\      the conservation count a no-op, letting any two same-class types stand in
\      for each other, dropping the unterminated-`construct` test at the
\      definition boundary, or letting the scrutinee pop take a bundle of any
\      family of the right width each left the whole gate green.
\
\ Where the two sides are not literally the same shape, and why that is sound:
\
\   - the checker answers -1 certified / 0 refused / 1 unresolvable and the
\     model answers `VCert` / `VReject` / `VUncheckable`. The row stores the
\     model's three-way verdict and the cases file maps the checker's answer
\     into it, so an unresolvable can never be read as a refusal;
\   - a type's class and sign are numbers in the checker and constructors in the
\     model. The class and sign tables carry both, so a row names the word the
\     checker writes and the constructor the model writes, and the numbers are
\     compared through those two tables rather than transcribed per row;
\   - the vectors need a prelude of named words with known effects. Those are
\     the model's `wStep1` .. `wKeepAny` definitions and the Habu definitions in
\     `test/compiler/checker-model-cases.f`; nothing pins them to each other
\     directly, because the vectors already do: change either and the verdicts
\     move apart.
\
\ Consumers: `test/compiler/checker-model-cases.f`,
\ `test/compiler/checker-model-obligations.f`.

require lib/errors.f
require lib/string.f
require lib/fmt.f

package CHECKER-MODEL-PROOF
private

$2000 constant POOL-CAP
$200 constant STR-MAX
$40 constant ROW-MAX

create POOL POOL-CAP allot
create SOFF STR-MAX cells allot
create SLEN STR-MAX cells allot

variable POOL-U
variable STR-N

\ ---- the shared string pool --------------------------------------------------
\ Every text below is interned once and referred to by handle, so a table row is
\ a run of cells and the data blocks stay readable as data.

: STR+ ( ptr u8 n -- n ) {: a:ptr u:n :}
   STR-N @ STR-MAX >= if E-CMP-POOL throw then
   POOL-U @ u + POOL-CAP > if E-CMP-POOL throw then
   a POOL POOL-U @ + u BYTE-COPY
   POOL-U @ STR-N @ cells SOFF + !
   u STR-N @ cells SLEN + !
   STR-N @ POOL-U @ u + POOL-U !
   STR-N @ 1+ STR-N ! ;

public

: STR$ ( n -- ptr u8 n ) {: h:n :}
   h 0 < h STR-N @ >= or if E-CMP-ROW throw then
   POOL h cells SOFF + @ + h cells SLEN + @ ;

private

\ ---- column storage ----------------------------------------------------------

create VOC-NAME ROW-MAX cells allot
create VOC-CODEW ROW-MAX cells allot
create VOC-CLASSW ROW-MAX cells allot
create VOC-WIDTH ROW-MAX cells allot
create VOC-SIGNW ROW-MAX cells allot
create VOC-CON ROW-MAX cells allot

create CLS-WORD ROW-MAX cells allot
create CLS-CODE ROW-MAX cells allot
create CLS-CON ROW-MAX cells allot

create SGN-WORD ROW-MAX cells allot
create SGN-CODE ROW-MAX cells allot
create SGN-CON ROW-MAX cells allot

create TAG-WORD ROW-MAX cells allot
create TAG-CODE ROW-MAX cells allot
create TAG-CON ROW-MAX cells allot
create TAG-PAT ROW-MAX cells allot
create TAG-SORT ROW-MAX cells allot

create CFT-SPELL ROW-MAX cells allot
create CFT-HANDLER ROW-MAX cells allot
create CFT-CON ROW-MAX cells allot

create FRK-KIND ROW-MAX cells allot
create FRK-OWNER ROW-MAX cells allot
create FRK-RUN ROW-MAX cells allot
create FRK-CFG ROW-MAX cells allot
create FRK-TOKS ROW-MAX cells allot

create VEC-NAME ROW-MAX cells allot
create VEC-SRC ROW-MAX cells allot
create VEC-CFG ROW-MAX cells allot
create VEC-TOKS ROW-MAX cells allot
create VEC-VERD ROW-MAX cells allot

variable VOC-N
variable CLS-N
variable SGN-N
variable TAG-N
variable CFT-N
variable FRK-N
variable VEC-N

: COL! ( n ptr a n -- ) {: v:n col:ptr i:n :}
   i ROW-MAX >= if E-CMP-POOL throw then
   v col i cells + ! ;

: COL@ ( ptr a n n -- n ) {: col:ptr i:n cnt:n :}
   i 0 < i cnt >= or if E-CMP-ROW throw then
   col i cells + @ ;

\ ---- 1. the concrete type vocabulary (`CT-INIT`, src/core/checker.f) ---------
\ Read a row as: the surface name the checker registers, the code word that
\ names its code, the class word, the width in bits, the sign word, and the
\ `Effects.con` constructor. The code itself is the row's position plus one,
\ because `CT-INIT` registers a dense run starting at 1 - which the cases file
\ checks against the checker's own `CC-` constants rather than assuming.

: VOC+ ( n n n n n n -- )
   {: name:n codew:n classw:n width:n signw:n con:n :}
   name VOC-NAME VOC-N @ COL!
   codew VOC-CODEW VOC-N @ COL!
   classw VOC-CLASSW VOC-N @ COL!
   width VOC-WIDTH VOC-N @ COL!
   signw VOC-SIGNW VOC-N @ COL!
   con VOC-CON VOC-N @ COL!
   VOC-N @ 1+ VOC-N ! ;

: VOC-ROW ( ptr u8 n ptr u8 n ptr u8 n n ptr u8 n ptr u8 n -- )
   {: na:ptr nu:n ca:ptr cu:n la:ptr lu:n width:n sa:ptr su:n oa:ptr ou:n :}
   na nu STR+ ca cu STR+ la lu STR+ width sa su STR+ oa ou STR+ VOC+ ;

: BUILD-VOCAB ( -- )
   s" n"      s" CC-N"      s" CT-INT"    64 s" CS-GENERIC"  s" CN"      VOC-ROW
   s" f"      s" CC-F"      s" CT-BOOL"    1 s" CS-NONE"     s" CF"      VOC-ROW
   s" r"      s" CC-R"      s" CT-FLOAT"  64 s" CS-NONE"     s" CR"      VOC-ROW
   s" i64"    s" CC-I64"    s" CT-INT"    64 s" CS-SIGNED"   s" CI64"    VOC-ROW
   s" u8"     s" CC-U8"     s" CT-INT"     8 s" CS-UNSIGNED" s" CU8"     VOC-ROW
   s" u32"    s" CC-U32"    s" CT-INT"    32 s" CS-UNSIGNED" s" CU32"    VOC-ROW
   s" cell"   s" CC-CELL"   s" CT-INT"    64 s" CS-GENERIC"  s" CCell"   VOC-ROW
   s" char"   s" CC-CHAR"   s" CT-INT"     8 s" CS-UNSIGNED" s" CChar"   VOC-ROW
   s" str"    s" CC-STR"    s" CT-OBJ"     0 s" CS-NONE"     s" CStr"    VOC-ROW
   s" addr"   s" CC-ADDR"   s" CT-INT"    64 s" CS-ADDR"     s" CAddr"   VOC-ROW
   s" bool"   s" CC-BOOL"   s" CT-BOOL"    1 s" CS-NONE"     s" CBool"   VOC-ROW
   s" idx"    s" CC-IDX"    s" CT-ROLE"   64 s" CS-NONE"     s" CIdx"    VOC-ROW
   s" len"    s" CC-LEN"    s" CT-ROLE"   64 s" CS-NONE"     s" CLen"    VOC-ROW
   s" count"  s" CC-COUNT"  s" CT-ROLE"   64 s" CS-NONE"     s" CCount"  VOC-ROW
   s" off"    s" CC-OFF"    s" CT-ROLE"   64 s" CS-NONE"     s" COff"    VOC-ROW
   s" fd"     s" CC-FD"     s" CT-ROLE"   64 s" CS-NONE"     s" CFd"     VOC-ROW
   s" rc"     s" CC-RC"     s" CT-ROLE"   64 s" CS-NONE"     s" CRc"     VOC-ROW
   s" pid"    s" CC-PID"    s" CT-ROLE"   64 s" CS-NONE"     s" CPid"    VOC-ROW
   s" ms"     s" CC-MS"     s" CT-ROLE"   64 s" CS-NONE"     s" CMs"     VOC-ROW
   s" ns"     s" CC-NS"     s" CT-ROLE"   64 s" CS-NONE"     s" CNs"     VOC-ROW
   s" tok"    s" CC-TOK"    s" CT-ROLE"   64 s" CS-NONE"     s" CTok"    VOC-ROW
   s" reg"    s" CC-REG"    s" CT-ROLE"   64 s" CS-NONE"     s" CReg"    VOC-ROW
   s" label"  s" CC-LABEL"  s" CT-ROLE"   64 s" CS-NONE"     s" CLabel"  VOC-ROW
   s" va"     s" CC-VA"     s" CT-ROLE"   64 s" CS-NONE"     s" CVa"     VOC-ROW
   s" symidx" s" CC-SYMIDX" s" CT-ROLE"   64 s" CS-NONE"     s" CSymidx" VOC-ROW
   s" asm"    s" CC-ASM"    s" CT-ROLE"   64 s" CS-NONE"     s" CAsm"    VOC-ROW
   s" img"    s" CC-IMG"    s" CT-ROLE"   64 s" CS-NONE"     s" CImg"    VOC-ROW
   s" snap"   s" CC-SNAP"   s" CT-ROLE"   64 s" CS-NONE"     s" CSnap"   VOC-ROW
   s" f32"    s" CC-F32"    s" CT-FLOAT"  32 s" CS-NONE"     s" CF32"    VOC-ROW
   s" u16"    s" CC-U16"    s" CT-INT"    16 s" CS-UNSIGNED" s" CU16"    VOC-ROW ;

\ ---- 2. the class and sign vocabularies --------------------------------------
\ `CT-NONE` is the absence of a class and `Effects.cls` has no constructor for
\ an absence, so its row carries an empty constructor and the obligations file
\ writes no obligation for it. Every other row is bound both ways.

: CLS-ROW ( ptr u8 n n ptr u8 n -- ) {: wa:ptr wu:n code:n ca:ptr cu:n :}
   wa wu STR+ CLS-WORD CLS-N @ COL!
   code CLS-CODE CLS-N @ COL!
   ca cu STR+ CLS-CON CLS-N @ COL!
   CLS-N @ 1+ CLS-N ! ;

: BUILD-CLASSES ( -- )
   s" CT-NONE"   0 s" "          CLS-ROW
   s" CT-INT"    1 s" ClsInt"    CLS-ROW
   s" CT-ROLE"   2 s" ClsRole"   CLS-ROW
   s" CT-BOOL"   3 s" ClsBool"   CLS-ROW
   s" CT-FLOAT"  4 s" ClsFloat"  CLS-ROW
   s" CT-OBJ"    5 s" ClsObj"    CLS-ROW
   s" CT-LINEAR" 6 s" ClsLinear" CLS-ROW ;

: SGN-ROW ( ptr u8 n n ptr u8 n -- ) {: wa:ptr wu:n code:n ca:ptr cu:n :}
   wa wu STR+ SGN-WORD SGN-N @ COL!
   code SGN-CODE SGN-N @ COL!
   ca cu STR+ SGN-CON SGN-N @ COL!
   SGN-N @ 1+ SGN-N ! ;

: BUILD-SIGNS ( -- )
   s" CS-NONE"     0 s" SgNone"     SGN-ROW
   s" CS-GENERIC"  1 s" SgGeneric"  SGN-ROW
   s" CS-SIGNED"   2 s" SgSigned"   SGN-ROW
   s" CS-UNSIGNED" 3 s" SgUnsigned" SGN-ROW
   s" CS-ADDR"     4 s" SgAddr"     SGN-ROW ;

\ ---- 3. the term tags --------------------------------------------------------
\ A row names the checker's tag word and its code, the model constructor that
\ stands for it, that constructor's pattern with one hole per argument, and
\ which model type it belongs to - `Effects.ty` for a term, `Effects.stack` for
\ a row cell.
\
\ `T-ATOM` is the rigid host identity. Effects.v's header names it as a
\ deliberate omission from the modelled fragment, so its row carries an empty
\ constructor and an empty sort: the gate then holds the checker to exactly
\ eight tags and the model to exactly the seven that are modelled, and a ninth
\ tag on either side is a new row somebody has to write here first.

: TAG-ROW ( ptr u8 n n ptr u8 n ptr u8 n ptr u8 n -- )
   {: wa:ptr wu:n code:n ca:ptr cu:n pa:ptr pu:n sa:ptr su:n :}
   wa wu STR+ TAG-WORD TAG-N @ COL!
   code TAG-CODE TAG-N @ COL!
   ca cu STR+ TAG-CON TAG-N @ COL!
   pa pu STR+ TAG-PAT TAG-N @ COL!
   sa su STR+ TAG-SORT TAG-N @ COL!
   TAG-N @ 1+ TAG-N ! ;

: BUILD-TAGS ( -- )
   s" T-CON"   0 s" TCon"  s" TCon _"       s" ty"    TAG-ROW
   s" T-VAR"   1 s" TVar"  s" TVar _"       s" ty"    TAG-ROW
   s" T-PTR"   2 s" TPtr"  s" TPtr _"       s" ty"    TAG-ROW
   s" S-ROW"   3 s" SRow"  s" SRow _"       s" stack" TAG-ROW
   s" S-PUSH"  4 s" SPush" s" SPush _ _"    s" stack" TAG-ROW
   s" T-QUOT"  5 s" TQuot" s" TQuot _ _ _"  s" ty"    TAG-ROW
   s" T-ATOM"  6 s" "      s" "             s" "      TAG-ROW
   s" T-PARAM" 7 s" TFam"  s" TFam _ _ _"   s" ty"    TAG-ROW ;

\ ---- 4. the control-flow dispatch table (`CF-TOK?`) --------------------------
\ In the order `CF-TOK?` tests them. The handler column is the token run that
\ stands between the test and `RES-TRUE`, so the `;match` row - which latches a
\ diagnostic and hard-rejects rather than running a control word - is written
\ out rather than special-cased.

: CFT-ROW ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: sa:ptr su:n ha:ptr hu:n ca:ptr cu:n :}
   sa su STR+ CFT-SPELL CFT-N @ COL!
   ha hu STR+ CFT-HANDLER CFT-N @ COL!
   ca cu STR+ CFT-CON CFT-N @ COL!
   CFT-N @ 1+ CFT-N ! ;

: BUILD-CONTROL ( -- )
   s" [:"      s" CF-QUOT"                        s" TOpenQ"     CFT-ROW
   s" ;]"      s" CF-SEMIQ"                       s" TCloseQ"    CFT-ROW
   s" if"      s" CF-IF"                          s" TIf"        CFT-ROW
   s" else"    s" CF-ELSE"                        s" TElse"      CFT-ROW
   s" then"    s" CF-THEN"                        s" TThen"      CFT-ROW
   s" case"    s" CF-CASE"                        s" TCase"      CFT-ROW
   s" of"      s" CF-OF"                          s" TOf"        CFT-ROW
   s" endof"   s" CF-ENDOF-DISPATCH"              s" TEndof"     CFT-ROW
   s" endcase" s" CF-ENDCASE"                     s" TEndcase"   CFT-ROW
   s" ;match"  s" MD-STRAY MDIAG! CF-FAIL"        s" TSemiMatch" CFT-ROW
   s" begin"   s" CF-BEGIN"                       s" TBegin"     CFT-ROW
   s" until"   s" CF-UNTIL"                       s" TUntil"     CFT-ROW
   s" again"   s" CF-AGAIN"                       s" TAgain"     CFT-ROW
   s" while"   s" CF-WHILE"                       s" TWhile"     CFT-ROW
   s" repeat"  s" CF-REPEAT"                      s" TRepeat"    CFT-ROW
   s" do"      s" CF-DO"                          s" TDo"        CFT-ROW
   s" ?do"     s" CF-DO"                          s" TDo"        CFT-ROW
   s" loop"    s" CF-LOOP"                        s" TLoop"      CFT-ROW
   s" +loop"   s" CF-+LOOP"                       s" TPlusLoop"  CFT-ROW
   s" i"       s" CF-I"                           s" TI"         CFT-ROW
   s" j"       s" CF-J"                           s" TJ"         CFT-ROW
   s" exit"    s" CF-EXIT"                        s" TExit"      CFT-ROW
   s" leave"   s" CF-LEAVE"                       s" TLeave"     CFT-ROW
   s" unloop"  s" CF-UNLOOP"                      s" TUnloop"    CFT-ROW
   s" recurse" s" CF-RECURSE"                     s" TRecurse"   CFT-ROW ;

\ The tokens the model knows that `CF-TOK?` does not dispatch: an ordinary call
\ and a call to a word with recorded control flags, `throw` and `die` (named in
\ `THROW-CUR?` / `DEAD-CUR?`), `match` and its family and variant tokens
\ (intercepted by `MATCH-TOK` while `MM` is non-zero), `construct` (intercepted
\ by `CONSTRUCT-TOK` while `CONM` is non-zero, and reusing the same family and
\ variant tokens for its two operands), `execute` and `catch`
\ (`RSEXEC` / `RSCATCH`), the locals binder and a local reference, and the two
\ return-stack transfers. The obligations file writes the whole constructor set
\ out as one exhaustive match, so a constructor added to `Control.tok` and not
\ named here fails to compile.

$10 constant OFF-MAX
create OFF-CON OFF-MAX cells allot
variable OFF-N

: OFF-ROW ( ptr u8 n -- ) {: a:ptr u:n :}
   a u STR+ OFF-CON OFF-N @ COL!
   OFF-N @ 1+ OFF-N ! ;

: BUILD-OFF-TABLE ( -- )
   s" TCall _"           OFF-ROW
   s" TCallCtl _ _ _"    OFF-ROW
   s" TThrow"            OFF-ROW
   s" TDie"              OFF-ROW
   s" TMatch"            OFF-ROW
   s" TFamTok _"         OFF-ROW
   s" TVarTok _"         OFF-ROW
   s" TConstruct"        OFF-ROW
   s" TExec"             OFF-ROW
   s" TCatch"            OFF-ROW
   s" TLocals _"         OFF-ROW
   s" TLocRef _"         OFF-ROW
   s" TToR"              OFF-ROW
   s" TFromR"            OFF-ROW ;

\ ---- 5. the control frame kinds ----------------------------------------------
\ The owner column is the checker word that writes the kind and the run column
\ is the exact token run inside it that writes it - a push for a kind a
\ construct OPENS, a field store for a kind a construct MUTATES into. The
\ configuration and token columns are a model program that must leave a frame of
\ that kind on top of the model's frame stack, so the number is bound at both
\ ends rather than only written down twice.

: FRK-ROW ( n ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- )
   {: kind:n oa:ptr ou:n ra:ptr ru:n ca:ptr cu:n ta:ptr tu:n :}
   kind FRK-KIND FRK-N @ COL!
   oa ou STR+ FRK-OWNER FRK-N @ COL!
   ra ru STR+ FRK-RUN FRK-N @ COL!
   ca cu STR+ FRK-CFG FRK-N @ COL!
   ta tu STR+ FRK-TOKS FRK-N @ COL!
   FRK-N @ 1+ FRK-N ! ;

: BUILD-FRAMES ( -- )
   1  s" CF-IF"         s" 1 DCUR @ 0 RCUR @ 0 CF-PUSH"
      s" sig [i64] [i64]"  s" [TCall wMkBool; TIf]"                       FRK-ROW
   2  s" CF-ELSE"       s" 2 CF-TOP CF.KND !"
      s" sig [i64] [i64]"  s" [TCall wMkBool; TIf; TElse]"                FRK-ROW
   3  s" CF-BEGIN"      s" 3 DCUR @ 0 RCUR @ 0 CF-PUSH"
      s" sig [i64] [i64]"  s" [TBegin]"                                   FRK-ROW
   4  s" CF-WHILE"      s" 4 CF-TOP CF.KND !"
      s" sig [i64] [i64]"  s" [TBegin; TCall wMkBool; TWhile]"            FRK-ROW
   5  s" CF-DO"         s" 5 DCUR @ 0 RCUR @ 0 CF-PUSH"
      s" sig [] []"        s" [TCall wMkN; TCall wMkN; TDo]"              FRK-ROW
   6  s" CF-QUOT"       s" 6 DCUR @ BROW @ RCUR @ RBROW @ CF-PUSH"
      s" sig [i64] [i64]"  s" [TOpenQ]"                                   FRK-ROW
   7  s" CF-CASE"       s" 7 DCUR @ 0 RCUR @ 0 CF-PUSH"
      s" sig [i64] [i64]"  s" [TCase]"                                    FRK-ROW
   8  s" CF-OF"         s" 8 CF@A 0 CF@RA 0 CF-PUSH"
      s" sig [] []"        s" [TCall wMkN; TCase; TCall wMkN; TOf]"       FRK-ROW
   9  s" MATCH-FAM-TOK" s" 9 DCUR @ 0 RCUR @ 0 CF-PUSH"
      s" sig_fam [fam0 100] [nt]" s" [TMatch; TFamTok fmres]"             FRK-ROW
   10 s" MATCH-OF-TOK"  s" 10 r MF.BASE @ 0 r MF.RBASE @ 0 CF-PUSH"
      s" sig_fam [fam0 100] [nt]"
      s" [TMatch; TFamTok fmres; TVarTok 0; TOf]"                         FRK-ROW ;

\ ---- 6. the shared program vectors -------------------------------------------
\ One verdict per row, written once. `V-CERT`, `V-UNCK` and `V-REJECT` are the
\ model's three outcomes; the cases file maps the checker's -1 / 1 / 0 into
\ them, so an unresolvable is never read as a refusal.

public

0 constant V-CERT
1 constant V-UNCK
2 constant V-REJECT

: VERDICT-NAME$ ( n -- ptr u8 n )
   case
      V-CERT of s" VCert" endof
      V-UNCK of s" VUncheckable" endof
      V-REJECT of s" VReject" endof
      E-CMP-ROW throw
   endcase ;

private

: VEC-ROW ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n n -- )
   {: na:ptr nu:n sa:ptr su:n ca:ptr cu:n ta:ptr tu:n verd:n :}
   na nu STR+ VEC-NAME VEC-N @ COL!
   sa su STR+ VEC-SRC VEC-N @ COL!
   ca cu STR+ VEC-CFG VEC-N @ COL!
   ta tu STR+ VEC-TOKS VEC-N @ COL!
   verd VEC-VERD VEC-N @ COL!
   VEC-N @ 1+ VEC-N ! ;

\ Two rows about the widening lattice, which nothing else here reaches. The
\ definition boundary joins at `UK-COERCE`, so `INT-WIDENS?` decides both: a
\ narrow unsigned integer reaches a wider generic one and certifies, while two
\ distinct nominal roles never meet however they are declared. A checker that
\ let same-class types stand in for each other would certify the second row.
: BUILD-WIDENING-VECTORS ( -- )
   s" int_widens_into_the_declared_output"
      s" CMV11 ( u8 -- cell )"
      s" sig [u8] [cellt]" s" []" V-CERT VEC-ROW
   s" a_role_never_reaches_a_sibling_role"
      s" CMV12 ( idx -- len )"
      s" sig [idxt] [lent]" s" []" V-REJECT VEC-ROW ;

\ The control-frame ceiling. `CF-PUSH` (src/core/checker.f) turns the frame
\ after the last one into an UNRESOLVABLE instead of pushing it, so the
\ sharpest pair of programs is two runs of `begin` differing only in length: at
\ the ceiling the definition is merely unbalanced and is refused, one deeper it
\ stops being checkable at all and the verdict changes class. Both texts are
\ BUILT from the ceiling below rather than spelled out, so a row cannot drift
\ from the number it is about.

32 constant FRAME-CEIL

: +OPENERS ( n -- ) {: opens:n :}
   opens 0 ?do s"  begin" SB-APPEND loop ;

: OPENERS$ ( ptr u8 n n -- ptr u8 n ) {: a:ptr u:n opens:n :}
   SB-RESET a u SB-APPEND opens +OPENERS SB$ ;

: MODEL-OPENERS$ ( n -- ptr u8 n ) {: opens:n :}
   SB-RESET s" (repeat TBegin " SB-APPEND
   opens FMT:SB-INT
   s" )" SB-APPEND SB$ ;

: FRAME-CAP-ROW ( ptr u8 n ptr u8 n n n -- )
   {: na:ptr nu:n sa:ptr su:n opens:n verd:n :}
   na nu STR+ VEC-NAME VEC-N @ COL!
   sa su opens OPENERS$ STR+ VEC-SRC VEC-N @ COL!
   s" sig [i64] [i64]" STR+ VEC-CFG VEC-N @ COL!
   opens MODEL-OPENERS$ STR+ VEC-TOKS VEC-N @ COL!
   verd VEC-VERD VEC-N @ COL!
   VEC-N @ 1+ VEC-N ! ;

: BUILD-FRAME-CAP-VECTORS ( -- )
   s" frames_at_the_ceiling_are_still_checked"
      s" CMV13 ( i64 -- i64 )" FRAME-CEIL V-REJECT FRAME-CAP-ROW
   s" one_frame_past_the_ceiling_is_unresolvable"
      s" CMV14 ( i64 -- i64 )" FRAME-CEIL 1+ V-UNCK FRAME-CAP-ROW ;

\ `MATCH`'s OWN depth guard, which is a different rule from the ceiling above.
\ A match form opens two frames of its own - one for the form, one for the
\ branch - and `MATCH-FAM-TOK` (src/core/checker.f) refuses outright, as a HARD
\ reject, once the frame stack is deeper than the number below, precisely so a
\ match can never install one of its two frames and then overflow on the other.
\ That number is therefore the deepest stack at which a match still fits, and
\ the sharpest pair of programs is a match at exactly that depth against one a
\ single frame deeper:
\
\   - at the depth below, the form takes the last two slots the ceiling has, so
\     the very next opener is one too many and the definition stops being
\     checkable;
\   - one deeper, the guard refuses before any of that, and a hard reject
\     outranks an uncheckable, so the verdict changes class.
\
\ Together they pin the number itself: lower the guard and the first row's
\ verdict becomes a refusal, raise it or remove it and the second row's becomes
\ an uncheckable. Both texts are BUILT from the number rather than spelled out.
\ Before these rows existed, lowering the guard left the whole gate green.
\
\ The number is DERIVED from the ceiling rather than written down a second
\ time, because that is the relation the checker's two literals stand in: the
\ match guard is the frame ceiling less the frames a match form opens. A
\ ceiling moved on one side only is then a row that no longer holds, rather
\ than two independent numbers that happen to disagree.

2 constant MATCH-FRAMES               \ the form's frame, and one branch's
FRAME-CEIL MATCH-FRAMES - constant MATCH-DEPTH-MAX

: MATCH-SRC$ ( ptr u8 n n ptr u8 n -- ptr u8 n ) {: ha:ptr hu:n opens:n ta:ptr tu:n :}
   SB-RESET ha hu SB-APPEND opens +OPENERS s"  " SB-APPEND ta tu SB-APPEND SB$ ;

: MATCH-TOKS$ ( n ptr u8 n -- ptr u8 n ) {: opens:n ta:ptr tu:n :}
   SB-RESET s" (opens " SB-APPEND opens FMT:SB-INT
   s"  ++ " SB-APPEND ta tu SB-APPEND s" )" SB-APPEND SB$ ;

: MATCH-DEPTH-ROW ( ptr u8 n ptr u8 n n ptr u8 n ptr u8 n n -- )
   {: na:ptr nu:n sa:ptr su:n opens:n ha:ptr hu:n ma:ptr mu:n verd:n :}
   na nu STR+ VEC-NAME VEC-N @ COL!
   sa su opens ha hu MATCH-SRC$ STR+ VEC-SRC VEC-N @ COL!
   s" sig_fam [fam0 100] [nt]" STR+ VEC-CFG VEC-N @ COL!
   opens ma mu MATCH-TOKS$ STR+ VEC-TOKS VEC-N @ COL!
   verd VEC-VERD VEC-N @ COL!
   VEC-N @ 1+ VEC-N ! ;

: BUILD-MATCH-DEPTH-VECTORS ( -- )
   s" a_match_at_the_deepest_frame_that_fits_takes_both_frames"
      s" CMV15 ( cmres -- n )" MATCH-DEPTH-MAX
      s" MATCH cmres cmok OF begin"
      s" [TMatch; TFamTok fmres; TVarTok 0; TOf; TBegin]"
      V-UNCK MATCH-DEPTH-ROW
   s" a_match_one_frame_deeper_is_refused_before_the_overflow"
      s" CMV16 ( cmres -- n )" MATCH-DEPTH-MAX 1+
      s" MATCH cmres cmok OF"
      s" [TMatch; TFamTok fmres; TVarTok 0; TOf]"
      V-REJECT MATCH-DEPTH-ROW ;

\ Two rows about the per-step linear conservation count, which the three
\ linear rows above do NOT reach: those are all decided by the deferred-taint
\ rule, which rejects a polymorphic copy or drop the moment the variable it
\ laundered resolves linear, and they answer the same either way if the count
\ check itself stops deciding anything. The count is over the data row AND the
\ return row together, so the case that only the count can decide is a value
\ that is on NEITHER row at the moment the check runs. `>r` never produces one -
\ it is its own rule and snapshots the whole transfer - but an ORDINARY word
\ declared with the same effect does, because a call checks the count inside the
\ data-row step, before the return rows move. The second row is the control: the
\ same word and the same tokens with nothing linear in play certify, so what the
\ first row records is the linear, not the transfer.
: BUILD-LINEAR-TRANSFER-VECTORS ( -- )
   s" a_linear_on_neither_row_when_the_step_is_checked"
      s" CMV17 ( cmltok -- cmltok ) CHECKER-MODEL-CASES:TO-R-WORD r>"
      s" sig [ltok] [ltok]" s" [TCall wToRAsWord; TFromR]" V-REJECT VEC-ROW
   s" the_same_transfer_with_nothing_linear_certifies"
      s" CMV18 ( i64 -- i64 ) CHECKER-MODEL-CASES:TO-R-WORD r>"
      s" sig [i64] [i64]" s" [TCall wToRAsWord; TFromR]" V-CERT VEC-ROW ;

\ Eight rows about `construct`, which nothing else here reaches. `construct` is
\ a three-token form and a small state machine, not a word call, so the rules
\ that decide it are the capture, the truncation test at the definition
\ boundary, and the inline step that turns a variant payload into the family's
\ bundle.
\
\ The first pair is the step: the same form certifies with its payload on the
\ row and is refused without it, so a step that consumed nothing would fail the
\ second row. The second pair is the CAPTURE, and it is the sharpest thing here
\ because the two verdicts differ in CLASS. `construct` takes its two operands
\ whatever they spell (`CONSTRUCT-TOK`, src/core/checker.f), so an operand that
\ names no variant is a REFUSAL; the very same token outside the form is just a
\ word the checker has never heard of, which is merely uncheckable. A checker
\ that let the trailing operand fall through to the ordinary word lookup would
\ answer uncheckable for both. The third pair is the payload's OWNER: two
\ variants of one family differing only in what they carry, so the same input
\ certifies for one and is refused for the other. The last row is the round
\ trip: `construct` and `MATCH` are each other's inverse, so building a variant
\ and immediately matching it returns the payload that was put in, and that row
\ is the one that fails if either half stops agreeing with the other about what
\ a bundle is.
\
\ Measured, by mutating the shipped checker, rebuilding the fixpoint and rerunning
\ this gate. Dropping the `CONM` clause from `CHECK`'s open-form test turns
\ exactly the unterminated row red and nothing else. Deleting the inline step
\ from `CONSTRUCT-TOK` turns the first, the captured, the payload-owner and the
\ round-trip rows red. Making the family operand close the form instead of
\ opening the variant slot - so the third token is no longer captured - turns
\ every row here red except the uncheckable control. Each mutation was restored
\ byte-for-byte afterwards.
\
\ Scope note, and it is why the gate runs where it does. `TFAM-CONSTRUCT-FAM`
\ (src/core/type-family.f) resolves a construct family in the ACTIVE package
\ only, so these programs certify only when the checker is asked from inside the
\ package that declared the family. `test/compiler/checker-model-proof.f` runs
\ the whole gate inside `package CHECKER-MODEL-CASES` for that reason.
: BUILD-CONSTRUCT-VECTORS ( -- )
   s" construct_builds_the_bundle_from_the_variant_payload"
      s" CMV19 ( n -- cmres ) construct cmres cmok"
      s" sig_fam [nt] [fam0 100]"
      s" [TConstruct; TFamTok fmres; TVarTok 0]" V-CERT VEC-ROW
   s" construct_without_its_payload_underflows"
      s" CMV20 ( -- cmres ) construct cmres cmok"
      s" sig_fam [] [fam0 100]"
      s" [TConstruct; TFamTok fmres; TVarTok 0]" V-REJECT VEC-ROW
   s" an_unterminated_construct_is_refused_at_the_boundary"
      s" CMV21 ( cmres -- cmres ) construct cmres"
      s" sig_fam [fam0 100] [fam0 100]"
      s" [TConstruct; TFamTok fmres]" V-REJECT VEC-ROW
   s" a_construct_operand_is_captured_whatever_it_spells"
      s" CMV22 ( cmres -- cmres ) construct cmres CMNOVAR"
      s" sig_fam [fam0 100] [fam0 100]"
      s" [TConstruct; TFamTok fmres; TVarTok 9]" V-REJECT VEC-ROW
   s" the_same_operand_outside_the_form_is_only_uncheckable"
      s" CMV23 ( cmres -- cmres ) CMNOVAR"
      s" sig_fam [fam0 100] [fam0 100]"
      s" [TVarTok 9]" V-UNCK VEC-ROW
   s" the_payload_is_the_variants_and_not_the_familys"
      s" CMV24 ( n -- cmbres ) construct cmbres cmbn"
      s" sig_fam [nt] [fam0 102]"
      s" [TConstruct; TFamTok fmbool; TVarTok 1]" V-CERT VEC-ROW
   s" a_sibling_variant_of_the_same_family_wants_its_own_payload"
      s" CMV25 ( n -- cmbres ) construct cmbres cmbf"
      s" sig_fam [nt] [fam0 102]"
      s" [TConstruct; TFamTok fmbool; TVarTok 0]" V-REJECT VEC-ROW
   s" construct_then_match_returns_the_payload_it_was_given"
      s" CMV26 ( n -- n ) construct cmres cmok MATCH cmres cmok OF ENDOF cmerr OF ENDOF ;MATCH"
      s" sig_fam [nt] [nt]"
      s" [TConstruct; TFamTok fmres; TVarTok 0; TMatch; TFamTok fmres; TVarTok 0; TOf; TEndof; TVarTok 1; TOf; TEndof; TSemiMatch]"
      V-CERT VEC-ROW ;

\ Two rows about `MATCH`'s SCRUTINEE POP, which is the walk that takes the
\ scrutinee's whole width-expanded bundle off the row before any branch starts
\ (`MATCH-SCRUT?`, src/core/checker.f). Nothing else here reaches it, because
\ every other family in this file is two cells wide - one payload slot and the
\ tag - and two decisions only show up above that.
\
\ The families the two rows use are `cmwide`, whose variants carry TWO cells
\ each so its bundle is three, and `cmtwin`, which has the same width, the same
\ variant count and the same payloads and only a different identity. The two
\ programs are then the same text with the family token and the variant names
\ changed, and their verdicts differ:
\
\   - the first pops a three-cell bundle whole and certifies, so a walk that
\     stopped short would leave the rest of the bundle on the row and fail it;
\   - the second names a family of exactly the same width, and is REFUSED. A
\     pop that compared cell counts and not the family id would certify it,
\     which is why same-width is the sharp case rather than merely a different
\     family.
\
\ Both families are named in the signature because a `MATCH` family token
\ resolves the way a signature type name does (`TFAM-MATCH-FAM`,
\ src/core/type-family.f); carrying `cmtwin` on the row underneath is how the
\ second row gets to name it at all.
\
\ Measured, by mutating the shipped checker, rebuilding the fixpoint and
\ rerunning this gate; the checker was restored byte-for-byte after each.
\
\   - dropping the family test from `MATCH-SCRUT-CELL?` turns the second row
\     from a refusal into a certification, and it is the ONLY row that moves;
\   - walking a three-cell bundle as two turns the first row from a
\     certification into a refusal, and it is the only row that moves;
\   - walking EVERY bundle as two never reaches this gate at all: the fixpoint
\     self-check refuses the build, because `FIND-EXECUTABLE-IN-PATH`
\     (lib/process-env.f) matches an `option` whose payload is itself a
\     multi-cell layout and stops certifying. The shipped library depends on
\     this walk, which is why the second mutation above has to name a width
\     nothing in `lib/` uses.
: BUILD-SCRUTINEE-VECTORS ( -- )
   s" a_multi_cell_bundle_is_popped_whole"
      s" CMV27 ( cmtwin cmwide -- cmtwin n ) MATCH cmwide cmwa OF CHECKER-MODEL-CASES:DROP-N ENDOF cmwb OF CHECKER-MODEL-CASES:DROP-N ENDOF ;MATCH"
      s" sig_fam [fam0 104; fam0 103] [fam0 104; nt]"
      s" [TMatch; TFamTok fmwide; TVarTok 0; TOf; TCall wDropN; TEndof; TVarTok 1; TOf; TCall wDropN; TEndof; TSemiMatch]"
      V-CERT VEC-ROW
   s" a_same_width_bundle_of_another_family_is_refused"
      s" CMV28 ( cmtwin cmwide -- cmtwin n ) MATCH cmtwin cmta OF CHECKER-MODEL-CASES:DROP-N ENDOF cmtb OF CHECKER-MODEL-CASES:DROP-N ENDOF ;MATCH"
      s" sig_fam [fam0 104; fam0 103] [fam0 104; nt]"
      s" [TMatch; TFamTok fmtwin; TVarTok 0; TOf; TCall wDropN; TEndof; TVarTok 1; TOf; TCall wDropN; TEndof; TSemiMatch]"
      V-REJECT VEC-ROW ;

: BUILD-VECTORS ( -- )
   s" straight_line"
      s" CMV1 ( i64 -- i64 ) CHECKER-MODEL-CASES:STEP1"
      s" sig [i64] [i64]" s" [TCall wStep1]" V-CERT VEC-ROW
   s" arity_mismatch"
      s" CMV2 ( i64 -- i64 ) CHECKER-MODEL-CASES:DUP1"
      s" sig [i64] [i64]" s" [TCall wDup1]" V-REJECT VEC-ROW
   s" branch_arms_join"
      s" CMV3 ( i64 -- i64 ) CHECKER-MODEL-CASES:MK-BOOL if CHECKER-MODEL-CASES:STEP1 else CHECKER-MODEL-CASES:STEP1 then"
      s" sig [i64] [i64]"
      s" [TCall wMkBool; TIf; TCall wStep1; TElse; TCall wStep1; TThen]"
      V-CERT VEC-ROW
   s" branch_arms_disagree"
      s" CMV4 ( i64 -- i64 ) CHECKER-MODEL-CASES:MK-BOOL if CHECKER-MODEL-CASES:STEP1 else CHECKER-MODEL-CASES:DROP1 CHECKER-MODEL-CASES:MK-CELL then"
      s" sig [i64] [i64]"
      s" [TCall wMkBool; TIf; TCall wStep1; TElse; TCall wDrop1; TCall wMkCell; TThen]"
      V-REJECT VEC-ROW
   s" loop_body_neutral"
      s" CMV5 ( i64 -- i64 ) begin CHECKER-MODEL-CASES:MK-BOOL until"
      s" sig [i64] [i64]" s" [TBegin; TCall wMkBool; TUntil]" V-CERT VEC-ROW
   s" loop_body_not_neutral"
      s" CMV6 ( i64 -- i64 ) begin CHECKER-MODEL-CASES:DUP1 CHECKER-MODEL-CASES:MK-BOOL until"
      s" sig [i64] [i64]"
      s" [TBegin; TCall wDup1; TCall wMkBool; TUntil]" V-REJECT VEC-ROW
   s" linear_kept_once"
      s" CMV7 ( cmltok -- cmltok ) CHECKER-MODEL-CASES:KEEP-POLY"
      s" sig [ltok] [ltok]" s" [TCall wKeepAny]" V-CERT VEC-ROW
   s" linear_copied_after_use"
      s" CMV8 ( cmltok -- cmltok cmltok ) CHECKER-MODEL-CASES:DUP-POLY"
      s" sig [ltok] [ltok; ltok]" s" [TCall wDupAny]" V-REJECT VEC-ROW
   s" linear_dropped_after_use"
      s" CMV9 ( cmltok -- ) CHECKER-MODEL-CASES:DROP-POLY"
      s" sig [ltok] []" s" [TCall wDropAny]" V-REJECT VEC-ROW
   s" unclosed_frame"
      s" CMV10 ( i64 -- i64 ) CHECKER-MODEL-CASES:MK-BOOL if CHECKER-MODEL-CASES:STEP1"
      s" sig [i64] [i64]" s" [TCall wMkBool; TIf; TCall wStep1]"
      V-REJECT VEC-ROW
   BUILD-WIDENING-VECTORS
   BUILD-FRAME-CAP-VECTORS
   BUILD-MATCH-DEPTH-VECTORS
   BUILD-LINEAR-TRANSFER-VECTORS
   BUILD-CONSTRUCT-VECTORS
   BUILD-SCRUTINEE-VECTORS ;

: BUILD-ALL ( -- )
   0 POOL-U !  0 STR-N !
   0 VOC-N !  0 CLS-N !  0 SGN-N !  0 TAG-N !
   0 CFT-N !  0 OFF-N !  0 FRK-N !  0 VEC-N !
   BUILD-VOCAB
   BUILD-CLASSES
   BUILD-SIGNS
   BUILD-TAGS
   BUILD-CONTROL
   BUILD-OFF-TABLE
   BUILD-FRAMES
   BUILD-VECTORS ;

BUILD-ALL

public

\ ---- what the gate reads -----------------------------------------------------

: VOCAB ( -- n )        VOC-N @ ;
: VOC-NAME$ ( n -- ptr u8 n )   VOC-NAME swap VOC-N @ COL@ STR$ ;
: VOC-CODEW$ ( n -- ptr u8 n )  VOC-CODEW swap VOC-N @ COL@ STR$ ;
: VOC-CLASSW$ ( n -- ptr u8 n ) VOC-CLASSW swap VOC-N @ COL@ STR$ ;
: VOC-WIDTH@ ( n -- n )         VOC-WIDTH swap VOC-N @ COL@ ;
: VOC-SIGNW$ ( n -- ptr u8 n )  VOC-SIGNW swap VOC-N @ COL@ STR$ ;
: VOC-CON$ ( n -- ptr u8 n )    VOC-CON swap VOC-N @ COL@ STR$ ;
: VOC-CODE@ ( n -- n ) {: i:n :}   i 0 < i VOC-N @ >= or if E-CMP-ROW throw then i 1+ ;

: CLASSES ( -- n )      CLS-N @ ;
: CLS-WORD$ ( n -- ptr u8 n )   CLS-WORD swap CLS-N @ COL@ STR$ ;
: CLS-CODE@ ( n -- n )          CLS-CODE swap CLS-N @ COL@ ;
: CLS-CON$ ( n -- ptr u8 n )    CLS-CON swap CLS-N @ COL@ STR$ ;

: SIGNS ( -- n )        SGN-N @ ;
: SGN-WORD$ ( n -- ptr u8 n )   SGN-WORD swap SGN-N @ COL@ STR$ ;
: SGN-CODE@ ( n -- n )          SGN-CODE swap SGN-N @ COL@ ;
: SGN-CON$ ( n -- ptr u8 n )    SGN-CON swap SGN-N @ COL@ STR$ ;

: TAGS ( -- n )         TAG-N @ ;
: TAG-WORD$ ( n -- ptr u8 n )   TAG-WORD swap TAG-N @ COL@ STR$ ;
: TAG-CODE@ ( n -- n )          TAG-CODE swap TAG-N @ COL@ ;
: TAG-CON$ ( n -- ptr u8 n )    TAG-CON swap TAG-N @ COL@ STR$ ;
: TAG-PAT$ ( n -- ptr u8 n )    TAG-PAT swap TAG-N @ COL@ STR$ ;
: TAG-SORT$ ( n -- ptr u8 n )   TAG-SORT swap TAG-N @ COL@ STR$ ;

: CONTROLS ( -- n )     CFT-N @ ;
: CFT-SPELL$ ( n -- ptr u8 n )   CFT-SPELL swap CFT-N @ COL@ STR$ ;
: CFT-HANDLER$ ( n -- ptr u8 n ) CFT-HANDLER swap CFT-N @ COL@ STR$ ;
: CFT-CON$ ( n -- ptr u8 n )     CFT-CON swap CFT-N @ COL@ STR$ ;

: OFF-CONS ( -- n )     OFF-N @ ;
: OFF-CON$ ( n -- ptr u8 n )     OFF-CON swap OFF-N @ COL@ STR$ ;

: FRAMES ( -- n )       FRK-N @ ;
: FRK-KIND@ ( n -- n )           FRK-KIND swap FRK-N @ COL@ ;
: FRK-OWNER$ ( n -- ptr u8 n )   FRK-OWNER swap FRK-N @ COL@ STR$ ;
: FRK-RUN$ ( n -- ptr u8 n )     FRK-RUN swap FRK-N @ COL@ STR$ ;
: FRK-CFG$ ( n -- ptr u8 n )     FRK-CFG swap FRK-N @ COL@ STR$ ;
: FRK-TOKS$ ( n -- ptr u8 n )    FRK-TOKS swap FRK-N @ COL@ STR$ ;

: VECTORS ( -- n )      VEC-N @ ;
: VEC-NAME$ ( n -- ptr u8 n )    VEC-NAME swap VEC-N @ COL@ STR$ ;
: VEC-SRC$ ( n -- ptr u8 n )     VEC-SRC swap VEC-N @ COL@ STR$ ;
: VEC-CFG$ ( n -- ptr u8 n )     VEC-CFG swap VEC-N @ COL@ STR$ ;
: VEC-TOKS$ ( n -- ptr u8 n )    VEC-TOKS swap VEC-N @ COL@ STR$ ;
: VEC-VERD@ ( n -- n )           VEC-VERD swap VEC-N @ COL@ ;

\ ---- the files this gate is about --------------------------------------------

: CHECKER-FILE$ ( -- ptr u8 n )
   s" src/core/checker.f" ;

2 constant MODELS

: MODEL-FILE$ ( n -- ptr u8 n )
   case
      0 of s" formal/Common/Effects.v" endof
      1 of s" formal/Common/Control.v" endof
      E-CMP-ROW throw
   endcase ;

: MODEL-PREFIX$ ( n -- ptr u8 n )
   case
      0 of s" Habu.Common.Effects." endof
      1 of s" Habu.Common.Control." endof
      E-CMP-ROW throw
   endcase ;

\ The two words whose bodies ARE the tables above: the concrete type registry
\ and the control-flow dispatch. The cases file walks each body token by token,
\ so a row added, removed, reordered, or edited in place moves the walk.
: VOCAB-WORD$ ( -- ptr u8 n )
   s" CT-INIT" ;

: CONTROL-WORD$ ( -- ptr u8 n )
   s" CF-TOK?" ;

: VOCAB-SET-WORD$ ( -- ptr u8 n )
   s" CT-SET" ;

: CONTROL-TEST-WORD$ ( -- ptr u8 n )
   s" CORE-STR=" ;

\ `CC-MAX` is the code a declared linear type takes next (`CT-ADD-LINEAR`), so
\ it is the ceiling of the whole concrete vocabulary and the model's `con_max`.
: VOCAB-CEIL-WORD$ ( -- ptr u8 n )
   s" CC-MAX" ;

\ Naming families the gate holds to an exact size, so a member added without a
\ row here is counted rather than missed.
: CODE-PREFIX$ ( -- ptr u8 n )
   s" CC-" ;

: SIGN-PREFIX$ ( -- ptr u8 n )
   s" CS-" ;

;package
