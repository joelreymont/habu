\ ir-intern-schema.f - the shared frozen description of the interning contract.
\
\ The module lives in `package COMPILER-INTERN-PROOF`. Its subject is the one
\ behaviour three production interners share - `IR-SYM` (src/compiler/ir/symbol.f),
\ `IR-TYPE` (src/compiler/ir/type.f) and `IR-ATTR` (src/compiler/ir/attr.f) - and
\ the machine-checked model of that behaviour in `formal/Common/Interning.v`.
\
\ It holds data and nothing else. Three tables:
\
\   1. The intern sequences. A sequence names a table kind, a committed ceiling,
\      and an ordered list of keys with the answer each key must receive: an
\      ordinal, or the exact throw code that must reject it. These rows are the
\      one copy. `test/compiler/ir-intern-cases.f` runs each row through the real
\      `IR-SYM` / `IR-TYPE` / `IR-ATTR` words, and
\      `test/compiler/ir-intern-obligations.f` turns the very same row into a Rocq
\      obligation about `Habu.Common.Interning`. Neither side carries a copy of
\      the vectors, so weakening a row asks both sides a weaker question and
\      deleting one stops both sides asking.
\
\   2. The scan-predicate field lists. What "the same key" means is decided by
\      which stored cells the scan predicate compares, in order. Adding or
\      dropping one silently changes identity, so the compared list of each
\      predicate is frozen here and read back structurally out of the source.
\
\   3. The frozen guard bodies and the words whose write ordering is checked.
\      A capacity check that runs after the first arena push leaves a row behind
\      on a rejected intern; the model's `ceiling_write_order_counterexample`
\      exhibits exactly that mutation. The reference guards are what make the
\      model's `refs_ok` hypothesis true of the shipped code.
\
\ Where the two sides are not literally the same shape, and why that is sound:
\
\   - A Habu type key is `(kind, width, sign)` or `(kind, space, pointee)` in wire
\     codes; the model's key is `TInt a b` or `TPtr a b` over plain numbers. What
\     the shared row holds is the ABSTRACT key - a constructor and two numbers -
\     and each side maps it injectively into its own vocabulary. The model's
\     theorems are about when two keys are the same key, so an injective encoding
\     on each side is exactly what the comparison needs; identical wire codes are
\     not.
\   - A Habu attribute integer key is one number. It rides the same abstract
\     constructor as an integer type, because the model is generic in the key type
\     and both tables are fixed-cell tables under one ceiling.
\   - Habu rejects with a named throw and the model rejects with `None`. The row
\     records the throw code, so the Habu diagnostic and the Rocq decision stay
\     bound to each other row by row rather than only agreeing that something
\     failed.
\
\ Consumers: `test/compiler/ir-intern-cases.f`,
\ `test/compiler/ir-intern-obligations.f`.

require lib/errors.f
require lib/string.f
require src/compiler/ir/symbol.f
require src/compiler/ir/type.f
require src/compiler/ir/attr.f

package COMPILER-INTERN-PROOF
public

\ ---- the three tables the model covers ---------------------------------------

0 constant KIND-SYM
1 constant KIND-TYPE
2 constant KIND-ATTR
3 constant KIND-COUNT

\ ---- what each intern sequence is there to show ------------------------------
\ Every role below must be covered by at least one sequence. The coverage check
\ in the cases file is what stops a sequence being quietly deleted: the table has
\ no digest, so the roles are the freeze.

0 constant ROLE-DUPLICATE   \ interning a key twice answers one ordinal and moves no count
1 constant ROLE-ORDER-A     \ one insertion order of a key set
2 constant ROLE-ORDER-B     \ the other insertion order of that same key set
3 constant ROLE-CEILING     \ a full table refuses a new key and still answers a duplicate
4 constant ROLE-COLLISION   \ two different byte strings forced through one content filter
5 constant ROLE-FORWARD     \ a key referring to an ordinal the table has not reached
6 constant ROLE-SELF        \ a key referring to the ordinal it would itself receive
7 constant ROLE-COUNT

\ The role's name, for the label a failing row reports and for the name the
\ generated Rocq obligation carries.
: ROLE-NAME$ ( n -- ptr u8 n )
   case
      0 of s" duplicate" endof
      1 of s" order_a" endof
      2 of s" order_b" endof
      3 of s" ceiling" endof
      4 of s" collision" endof
      5 of s" forward_ref" endof
      6 of s" self_ref" endof
      E-CIN-ROW throw
   endcase ;

\ ---- abstract key constructors -----------------------------------------------

0 constant CTOR-INT
1 constant CTOR-PTR

\ ---- the symbol keys ---------------------------------------------------------

7 constant SYM-KEYS

: SYM-KEY$ ( n -- ptr u8 n )
   case
      0 of s" main" endof
      1 of s" helper" endof
      2 of s" VGA" endof
      3 of s" HRA" endof
      4 of s" a" endof
      5 of s" b" endof
      6 of s" c" endof
      E-CIN-ROW throw
   endcase ;

\ ---- the type keys -----------------------------------------------------------
\ Each row is (constructor, first argument, second argument). For an integer the
\ arguments are the width and sign positions below; for a pointer they are the
\ space position and the pointee's module-local ordinal.

4 constant TY-KEYS

: TY-CTOR ( n -- n )
   case
      0 of CTOR-INT endof
      1 of CTOR-INT endof
      2 of CTOR-PTR endof
      3 of CTOR-PTR endof
      E-CIN-ROW throw
   endcase ;

: TY-ARG-A ( n -- n )
   case
      0 of 1 endof                       \ width position 1
      1 of 2 endof                       \ width position 2
      2 of 0 endof                       \ space position 0
      3 of 0 endof                       \ space position 0
      E-CIN-ROW throw
   endcase ;

: TY-ARG-B ( n -- n )
   case
      0 of 1 endof                       \ sign position 1
      1 of 1 endof                       \ sign position 1
      2 of 0 endof                       \ pointee ordinal 0
      3 of 1 endof                       \ pointee ordinal 1
      E-CIN-ROW throw
   endcase ;

\ The enum member each argument position names. The positions are the frozen
\ numbering; these three words are the only place they become Habu members, and
\ the obligation writer emits the same positions as the model's numbers.

: WIDTH-AT ( n -- IR-TYPE:width )
   case
      0 of IR--TYPE-WIDTH:W1 endof
      1 of IR--TYPE-WIDTH:W8 endof
      2 of IR--TYPE-WIDTH:W16 endof
      3 of IR--TYPE-WIDTH:W32 endof
      4 of IR--TYPE-WIDTH:W64 endof
      E-CIN-ROW throw
   endcase ;

: SIGN-AT ( n -- IR-TYPE:sign )
   case
      0 of IR--TYPE-SIGN:UNSIGNED endof
      1 of IR--TYPE-SIGN:SIGNED endof
      E-CIN-ROW throw
   endcase ;

: SPACE-AT ( n -- IR-TYPE:space )
   case
      0 of IR--TYPE-SPACE:GENERIC endof
      1 of IR--TYPE-SPACE:GLOBAL endof
      E-CIN-ROW throw
   endcase ;

\ ---- the attribute keys ------------------------------------------------------

3 constant AT-KEYS

: AT-KEY ( n -- n )
   case
      0 of 7 endof
      1 of 9 endof
      2 of 11 endof
      E-CIN-ROW throw
   endcase ;

private

\ ---- storage -----------------------------------------------------------------

$40 constant STEP-CAP
$10 constant SCN-CAP

create STEP-KEY STEP-CAP cells allot
create STEP-ORD STEP-CAP cells allot
create STEP-CLASS STEP-CAP cells allot

create SCN-ROLE SCN-CAP cells allot
create SCN-KIND SCN-CAP cells allot
create SCN-CEIL SCN-CAP cells allot
create SCN-BASE SCN-CAP cells allot
create SCN-LEN SCN-CAP cells allot
create SCN-FINAL SCN-CAP cells allot

variable STEP-N
variable SCN-N
variable OPEN-BASE

: STEP-RANGE ( n -- ) {: i:n :}
   i 0 < i STEP-N @ >= or if E-CIN-ROW throw then ;

: SCN-RANGE ( n -- ) {: i:n :}
   i 0 < i SCN-N @ >= or if E-CIN-ROW throw then ;

\ ---- table builders ----------------------------------------------------------

\ One step: the key this sequence presents next, the ordinal it must answer, and
\ the throw code that must reject it. An accepted step carries class 0 and its
\ ordinal; a rejected step carries its code and the unusable ordinal -1.
: STEP+ ( n n n -- ) {: keyi:n ord:n class:n :}
   STEP-N @ STEP-CAP >= if E-CIN-ROW throw then
   keyi STEP-KEY STEP-N @ cells + !
   ord STEP-ORD STEP-N @ cells + !
   class STEP-CLASS STEP-N @ cells + !
   STEP-N @ 1+ STEP-N ! ;

: OK+ ( n n -- ) {: keyi:n ord:n :}
   keyi ord 0 STEP+ ;

: NO+ ( n n -- ) {: keyi:n class:n :}
   keyi -1 class STEP+ ;

: SEQ ( -- )
   STEP-N @ OPEN-BASE ! ;

: ;SEQ ( n n n n -- ) {: role:n kind:n ceiling:n final:n :}
   SCN-N @ SCN-CAP >= if E-CIN-ROW throw then
   role SCN-ROLE SCN-N @ cells + !
   kind SCN-KIND SCN-N @ cells + !
   ceiling SCN-CEIL SCN-N @ cells + !
   OPEN-BASE @ SCN-BASE SCN-N @ cells + !
   STEP-N @ OPEN-BASE @ - SCN-LEN SCN-N @ cells + !
   final SCN-FINAL SCN-N @ cells + !
   SCN-N @ 1+ SCN-N ! ;

\ ---- the intern sequences ----------------------------------------------------
\ Read a block as: open, present these keys in this order, close with the role,
\ the table kind, the committed ceiling, and the row count the table must hold
\ when the sequence ends.

: SYM-SEQUENCES ( -- )
   SEQ  0 0 OK+  1 1 OK+  0 0 OK+  1 1 OK+
      ROLE-DUPLICATE KIND-SYM 8 2 ;SEQ

   SEQ  0 0 OK+  1 1 OK+
      ROLE-ORDER-A KIND-SYM 8 2 ;SEQ

   SEQ  1 0 OK+  0 1 OK+
      ROLE-ORDER-B KIND-SYM 8 2 ;SEQ

   SEQ  4 0 OK+  5 1 OK+  6 E-IR-SYM-CAP NO+  4 0 OK+
      ROLE-CEILING KIND-SYM 2 2 ;SEQ

   SEQ  2 0 OK+  3 1 OK+  2 0 OK+
      ROLE-COLLISION KIND-SYM 8 2 ;SEQ ;

: TY-SEQUENCES ( -- )
   SEQ  0 0 OK+  1 1 OK+  2 2 OK+
      ROLE-ORDER-A KIND-TYPE 8 3 ;SEQ

   SEQ  1 0 OK+  0 1 OK+  3 2 OK+
      ROLE-ORDER-B KIND-TYPE 8 3 ;SEQ

   SEQ  2 E-IR-TYPE-BOUND NO+
      ROLE-FORWARD KIND-TYPE 8 0 ;SEQ

   SEQ  0 0 OK+  3 E-IR-TYPE-BOUND NO+
      ROLE-SELF KIND-TYPE 8 1 ;SEQ ;

: AT-SEQUENCES ( -- )
   SEQ  0 0 OK+  1 1 OK+  0 0 OK+
      ROLE-DUPLICATE KIND-ATTR 8 2 ;SEQ

   SEQ  0 0 OK+  1 1 OK+  2 E-IR-ATTR-CAP NO+  0 0 OK+
      ROLE-CEILING KIND-ATTR 2 2 ;SEQ ;

: BUILD-SEQUENCES ( -- )
   0 STEP-N !
   0 SCN-N !
   SYM-SEQUENCES
   TY-SEQUENCES
   AT-SEQUENCES ;

BUILD-SEQUENCES

public

: SCENARIOS ( -- n )       SCN-N @ ;
: STEPS ( -- n )           STEP-N @ ;

: SCN-ROLE@ ( n -- n )     dup SCN-RANGE cells SCN-ROLE + @ ;
: SCN-KIND@ ( n -- n )     dup SCN-RANGE cells SCN-KIND + @ ;
: SCN-CEIL@ ( n -- n )     dup SCN-RANGE cells SCN-CEIL + @ ;
: SCN-BASE@ ( n -- n )     dup SCN-RANGE cells SCN-BASE + @ ;
: SCN-LEN@ ( n -- n )      dup SCN-RANGE cells SCN-LEN + @ ;
: SCN-FINAL@ ( n -- n )    dup SCN-RANGE cells SCN-FINAL + @ ;

: STEP-KEY@ ( n -- n )     dup STEP-RANGE cells STEP-KEY + @ ;
: STEP-ORD@ ( n -- n )     dup STEP-RANGE cells STEP-ORD + @ ;
: STEP-CLASS@ ( n -- n )   dup STEP-RANGE cells STEP-CLASS + @ ;

\ ---- the scan-predicate field lists ------------------------------------------
\ One row per production scan predicate: the file it lives in, the word, the
\ ordered list of stored cells it compares with the operator each comparison
\ uses, and the token its body ends on - the step that actually decides identity.
\ A predicate reads a stored cell as `<offset> RC@`, so the reader collects the
\ token before every `RC@` together with the operator three tokens on, and an
\ added, dropped, reordered, or no-longer-compared field changes the row.

3 constant PRED-COUNT

: PRED-FILE$ ( n -- ptr u8 n )
   case
      0 of s" src/compiler/ir/symbol.f" endof
      1 of s" src/compiler/ir/type.f" endof
      2 of s" src/compiler/ir/attr.f" endof
      E-CIN-ROW throw
   endcase ;

: PRED-WORD$ ( n -- ptr u8 n )
   case
      0 of s" ROW-MATCH?" endof
      1 of s" ROW4-MATCH?" endof
      2 of s" ROW5-MATCH?" endof
      E-CIN-ROW throw
   endcase ;

: PRED-FIELDS$ ( n -- ptr u8 n )
   case
      0 of s" OFF-FLT <> OFF-LEN <>" endof
      1 of s" OFF-KIND <> OFF-A <> OFF-B <> OFF-C =" endof
      2 of s" OFF-KIND <> OFF-A <> OFF-B <> OFF-C <> OFF-D =" endof
      E-CIN-ROW throw
   endcase ;

\ The symbol predicate ends on the byte-compare verify, which is the step that
\ keeps a filter collision from merging two symbols. The two fixed-cell
\ predicates end on the last field comparison itself.
: PRED-VERIFY$ ( n -- ptr u8 n )
   case
      0 of s" BYTES-EQ" endof
      1 of s" =" endof
      2 of s" =" endof
      E-CIN-ROW throw
   endcase ;

\ ---- the words whose write ordering is checked -------------------------------
\ Every capacity check must run before the first arena push. The push may be
\ several calls deep - `IR-SYM:INTERN` pushes through `POOL-ADD` and `ROW-ADD` -
\ so the reader classifies the file's own definitions first and then reads these
\ four bodies against that classification.

4 constant ORDER-COUNT

: ORDER-FILE$ ( n -- ptr u8 n )
   case
      0 of s" src/compiler/ir/symbol.f" endof
      1 of s" src/compiler/ir/type.f" endof
      2 of s" src/compiler/ir/type.f" endof
      3 of s" src/compiler/ir/attr.f" endof
      E-CIN-ROW throw
   endcase ;

: ORDER-WORD$ ( n -- ptr u8 n )
   case
      0 of s" INTERN" endof
      1 of s" INTERN4" endof
      2 of s" FN-END" endof
      3 of s" INTERN5" endof
      E-CIN-ROW throw
   endcase ;

\ The token that writes a cell into an arena, and the token that reads a table's
\ committed capacity. A definition that carries either, directly or through a
\ word it calls, is a writer or a capacity check.
: PUSH-TOKEN$ ( -- ptr u8 n )
   s" IR-ARENA:PUSH" ;

: CAP-TOKEN$ ( -- ptr u8 n )
   s" HC-CAP" ;

\ ---- the reference guards ----------------------------------------------------
\ The model assumes `refs_ok`: every ordinal a key mentions is strictly below the
\ live count. These are the bodies that make it true of the shipped code, frozen
\ whole because each is small and each is entirely guard.

4 constant GUARD-COUNT

: GUARD-WORD$ ( n -- ptr u8 n )
   case
      0 of s" ID-CK" endof
      1 of s" ID-CK-N" endof
      2 of s" STG-REF-CK" endof
      3 of s" STG-VALIDATE" endof
      E-CIN-ROW throw
   endcase ;

: GUARD-BODY$ ( n -- ptr u8 n )
   case
      0 of s" {: r:IR-ARENA:arena id:IR-ID:ir-type-id :} r RHDR-CK r HC-SERIAL LCELL@ r CNT id ID-CK-N" endof
      1 of s" {: hs:n cnt:n id:IR-ID:ir-type-id :} hs id ID-OWNER-SERIAL SERIAL-CK id IR-ID:TYPE-LOCAL dup cnt >= if E-IR-TYPE-BOUND throw then" endof
      2 of s" {: hs:n cnt:n os:n loc:n :} os hs <> if E-IR-TYPE-OWNER throw then loc 0 < loc cnt >= or if E-IR-TYPE-BOUND throw then" endof
      3 of s" {: hs:n cnt:n :} STG-PN @ 0 ?do hs cnt i PO@ i PL@ STG-REF-CK loop STG-RN @ 0 ?do hs cnt i RO@ i RL@ STG-REF-CK loop" endof
      E-CIN-ROW throw
   endcase ;

\ The pointer constructor checks the pointee it was handed, not some other
\ identity, and it does so before it interns.
: PTEE-CHECK$ ( -- ptr u8 n )
   s" r ptee ID-CK" ;

\ The staged-element check is handed the table's live row count as its bound.
: STAGE-CHECK$ ( -- ptr u8 n )
   s" key KEY-SERIAL r CNT STG-VALIDATE" ;

: TYPE-FILE$ ( -- ptr u8 n )
   s" src/compiler/ir/type.f" ;

: MODEL-FILE$ ( -- ptr u8 n )
   s" formal/Common/Interning.v" ;

;package
