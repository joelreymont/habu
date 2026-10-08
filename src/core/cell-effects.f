\ cell-effects.f - effects for words needed before the checker starts.

s" CELL" s" -- n" TRUST
s" CELL-WIDTH-CHECK" s" --" TRUST
s" CHECKER-CAPTURE-PREPARE" s" --" TRUST
s" CHECKER-STORAGE-PREPARE" s" --" TRUST

\ src/core/util.f's public constants. util.f loads before the checker, so a
\ from-source prefix boot gives them no effect of their own and a checked body
\ that names one is E-UNDEFINED there (test/cold-naming-test.f). Rows rather
\ than TRUST declarations, for the reason NULL-PTR's is one.
PRIM: PATH-CAP PE-N PE-OUT PRIM;
PRIM: E-PATH-RANGE PE-N PE-OUT PRIM;
PRIM: SCOPE-FIND-AMBIGUOUS PE-N PE-OUT PRIM;

s" PTR-VARIABLE" s" --" TRUST
s" PERSISTED-PTR-VARIABLE" s" --" TRUST
s" PTR-U8-TABLE" s" n --" TRUST
s" PERSISTED-PTR-U8-TABLE-VARIABLE" s" --" TRUST
s" RESERVED-PTR-U8-CELL" s" n --" TRUST
\ NULL-PTR is the language's null: a pointer of any pointee, and the address OF
\ nothing. `src/core/pointer-storage.f` defines it, and this file loads after
\ it, so THIS row is the record that answers - which is what lets the pointee
\ carry the base-address kind the definition's own signature cannot spell (dot
\ habu-fence-a-base-c6c1d71d). TVK-NULL keeps every honest use: storing a null
\ into a declared pointer cell, comparing one, subtracting one, testing it with
\ 0= - each binds the pointee INSIDE a `ptr`, where the kind is permissive, and
\ each means this one literal address. What it refuses is a value read THROUGH
\ it: `NULL-PTR + @` taken as a nominal identity or as an address certified and
\ forged one. That pointee arm is the whole difference from `data-base`, whose
\ TVK-DBASE reaches an arbitrary DATA word and is fenced at every depth. A row
\ rather than a TRUST declaration, so the effect is stated once, by the checker's
\ own constructors, and one trusted seam fewer stands behind the null.
PRIM: NULL-PTR PE-PTR-A-NULL PE-OUT PRIM;
s" LBUF-CAPTURE-PREPARE" s" --" TRUST

\ These variable accessors are emitted before checking starts. Their concrete
\ storage effects let the multi-error operations below check normally.
PRIM: MULTI-ERR PE-PTR-N PE-OUT PRIM;
PRIM: MULTI-ERR-N PE-PTR-N PE-OUT PRIM;

\ src/habu/verify-source.f counts its learned definers and its `parses:` rows
\ in these checker cells, which the rollback frame rewinds (src/core/checker.f),
\ and names them in checked bodies loaded after the hook.
PRIM: VERIFY-DEFINER-N PE-PTR-N PE-OUT PRIM;
PRIM: VERIFY-PARSES-N PE-PTR-N PE-OUT PRIM;

s" SIG-RAW-MODE" s" -- ptr n" TRUST

\ Enter a fresh collection; END returns its reject count and clears the mode.
\ The floor is read from this source owner's private effect arena.
TRUSTED: MULTI-ERR-BEGIN ( -- )
   UEND @ CHECKER-EFFECT-AUTHORITY:RECOVERY-START
   -1 MULTI-ERR !  0 MULTI-ERR-N ! ;


: MULTI-ERR-END ( -- n )
   MULTI-ERR-N @  0 MULTI-ERR ! ;

\ `parses: W n` and `parses-through: W n ( E1 E2 )`, at top level after W's
\ definition. Each reads its row by parse-name, as the source pre-verifier reads
\ it (src/habu/verify-source.f PARSES-ROW), has the checker check it
\ (src/core/checker.f CHECKER-PARSES-ROW) and keeps nothing: the shape is the
\ pre-verifier's contract alone. The keyword is the token the engine read last
\ when the word starts (CK-TKA-OFF), where a row with no target is named.
\ UNSAFE-TOK? bars both from checked bodies, so they run from top level only.
\ They load after the hook so that this checker records them and INTRINSIC tags
\ each as the engine word the pre-pass knows it by, and as a word that reads
\ the source after it (CTL-PARSES, INTRINSIC-CTL): a word src/core/checker.f
\ defines is recorded by the checker the build kept, whose identities its
\ handover copies, and that checker may know no such word.
TRUSTED: parses: ( -- )
   data-base CK-TKA-OFF + @  data-base CK-TKL-OFF + @
   parse-name parse-name RES-FALSE CHECKER-PARSES-ROW drop drop drop ;
INTRINSIC-PARSES INTRINSIC

TRUSTED: parses-through: ( -- )
   data-base CK-TKA-OFF + @  data-base CK-TKL-OFF + @
   parse-name parse-name PARSES-LIST-LOAD CHECKER-PARSES-ROW drop drop drop ;
INTRINSIC-PARSES-THROUGH INTRINSIC

\ `names: W`, at top level after W's definition: W looks its string operand up
\ as a name, so a string literal before a call of W is a use of the word it
\ names. Read as the source pre-verifier reads it (src/habu/verify-source.f
\ NAMES-ROW), checked by the checker, which marks W's symbol
\ (src/core/checker.f CHECKER-NAMES-ROW), barred from checked bodies and
\ tagged as the two above are.
TRUSTED: names: ( -- )
   data-base CK-TKA-OFF + @  data-base CK-TKL-OFF + @
   parse-name CHECKER-NAMES-ROW ;
INTRINSIC-NAMES INTRINSIC

\ Read finalized numeric call facts without exposing the unification graph.
package CHECKER-CALLS

\ The header's first cell holds the row arena's address, so it is DECLARED
\ storage and the two counts are allotted behind it (dot
\ habu-refuse-a-ptr-5ad2734e). A `create`d cell is raw storage: fetching an
\ address out of one is the launder the checker refuses, and `0 ptr-field` on it
\ was exactly that. The counts are numbers, read through an explicit cell view of
\ the declared head - the same record, the same compiled add-and-load, since both
\ views are type-level only.
PTR-VARIABLE STATE 0 , 0 ,

\ The owner can record calls before this checked header is loaded. Transfer
\ that allocation with its rows, then clear the retired header so capture has
\ only one live store to prepare. Reinstalling the current header is harmless.
: INSTALL ( -- )
   CWIN-STATE {: prior:ptr :}
   prior STATE = if exit then
   prior @ STATE !
   prior CELL + BYTE-VIEW CELL-VIEW @ STATE CELL + BYTE-VIEW CELL-VIEW !
   prior 2 CELL * + BYTE-VIEW CELL-VIEW @ STATE 2 CELL * + BYTE-VIEW CELL-VIEW !
   NULL-PTR prior !
   0 prior CELL + BYTE-VIEW CELL-VIEW !
   0 prior 2 CELL * + BYTE-VIEW CELL-VIEW !
   [: STATE ;] is CWIN-STATE ;
INSTALL

: FIELD ( n n -- ptr n ) {: row:n field:n :}
   STATE @ row 4 * field + CELL * + CELL-VIEW ;

: FIND ( n n -- n n ) {: ord:n kind:n :}
   STATE CELL + BYTE-VIEW CELL-VIEW @ 0 ?do
      i 0 FIELD @ ord = i 3 FIELD @ kind = and if
         i 1 FIELD @ i 2 FIELD @ unloop exit
      then
   loop
   -1 -1 ;

get-current prot-wid-add
public

: CELLS ( n -- n n ) 3 FIND ;
: GLUE ( n -- n n ) 4 FIND ;
: MATCH-PAYLOAD ( n -- n n ) -3 FIND ;
: QUOT-IN ( n n -- n n ) 2 * 5 + FIND ;
: QUOT-OUT ( n n -- n n ) 2 * 6 + FIND ;

get-current prot-wid-add
;package
