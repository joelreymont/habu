\ checker-assert.f - shared test support for asserting what the CHECKER and the type
\ registry actually recorded.
\
\ Two surfaces live here. CHECK-QUIET-CANDIDATE! asks the checker to certify a candidate
\ definition and answers -1 accepted / 0 refused / 1 unresolvable, with diagnostics muted.
\ Package REFLECT reads a declared family's recorded shape back out of the live type
\ registry, through the read-only accessors the checker publishes for public-signature
\ tooling (src/core/checker.f, src/core/type-family.f).
\
\ WHY REFLECT EXISTS. Moving a declaration to another front end or another form can change
\ the recorded kind, case order, width or payload slots without changing anything a suite
\ can otherwise observe: MATCH dispatches on case NAME, so an ordinal projection is blind
\ to a case-order change, and two declaration forms of one family are width- and
\ MATCH-identical. Reading the registry is the only way a suite can see those properties,
\ so every migrated family pins them here.
\
\ TWO RULES THIS SET ENFORCES SO A PIN CANNOT LIE.
\
\ 1. A family is identified by its tail PLUS the constructor package its variants carry -
\    the (package, tail) pair that owns family identity. A tail alone is NOT unique: eight
\    packages declare a family whose tail is `id-result`, so a tail-only lookup silently
\    pins whichever one loaded first and reports green about a family the suite never
\    touched. FAMS answers how many registered families match an identity, so a suite
\    asserts FAMS = 1 and finds out when a tail becomes ambiguous instead of guessing.
\
\ 2. Every reader refuses the -1 sentinel before it reads. FAM-ID answers -1 for an
\    identity that is not registered, and a missing family must report a wrong number
\    (-1, or `<missing>` for a name) rather than read a registry row that is not there.
\
\ test/checker-assert-test.f is this file's own suite: it declares hostile fixtures - two
\ families sharing one tail under different constructor packages - and proves the
\ ambiguity is counted, a wrong constructor package resolves nothing, and every reader
\ answers the sentinel instead of a neighbouring row.

require lib/prelude.f
require lib/string.f

\ Recursive CHECK-CANDIDATE! plus DIAG-QUIET state cannot certify itself.
\ Retirement owner: habu-primitive-effect-axiom-1119f176.
TRUSTED: CHECK-QUIET-CANDIDATE! ( ptr u8 n -- n )
   1 DIAG-QUIET +!
   CHECK-CANDIDATE!
   -1 DIAG-QUIET +! ;

package REFLECT
private

: FAM-CTOR? ( n ptr u8 n -- bool ) {: fam:n pa:ptr pu:n :}
   fam TFAM-VAR-COUNT@ 0 <= if false exit then
   fam TFAM-VAR-START@ SUMV-CTOR-PKG$ pa pu STR= ;

: FAM-HIT? ( n ptr u8 n ptr u8 n -- bool ) {: fam:n ta:ptr tu:n pa:ptr pu:n :}
   fam TFAM-NAME$ ta tu STR= fam pa pu FAM-CTOR? and ;

: FAM-ID ( ptr u8 n ptr u8 n -- n ) {: ta:ptr tu:n pa:ptr pu:n :}   \ family id, or -1
   TFAM-N@ 0 ?do
      i ta tu pa pu FAM-HIT? if i unloop exit then
   loop -1 ;

: LIVE-VARS ( n -- n ) {: fam:n :}
   fam 0 < if -1 exit then
   fam TFAM-VAR-COUNT@ ;

: LIVE-VAR ( n n -- n ) {: fam:n k:n :}            \ the family's k-th variant row, or -1
   k 0 < if -1 exit then
   fam LIVE-VARS k <= if -1 exit then
   fam TFAM-VAR-START@ k + ;

: LIVE-FLDS ( n n -- n ) {: fam:n var:n :}         \ committed field rows this owner holds
   fam 0 < if -1 exit then
   0
   TYPE-FIELD:COUNT 0 ?do
      i TYPE-FIELD:FAMILY@ fam = i TYPE-FIELD:VARIANT@ var = and if 1+ then
   loop ;

: LIVE-SLOT ( n n ptr u8 n -- n ) {: fam:n var:n na:ptr nu:n :}
   fam 0 < if -1 exit then
   fam var na nu TYPE-FIELD:FIND 0= if drop -1 exit then
   TYPE-FIELD:SLOT@ ;

: LIVE-CELLS ( n n ptr u8 n -- n ) {: fam:n var:n na:ptr nu:n :}
   fam 0 < if -1 exit then
   fam var na nu TYPE-FIELD:FIND 0= if drop -1 exit then
   TYPE-FIELD:CELLS@ ;

public

\ ---- family identity ----------------------------------------------------------
\ FAMS is the uniqueness assertion: 1 is the only healthy answer. 0 means the identity
\ resolves nothing (a renamed family, or a wrong constructor package); more than 1 means
\ the identity is ambiguous and no pin under it can be trusted.
: FAMS ( ptr u8 n ptr u8 n -- n ) {: ta:ptr tu:n pa:ptr pu:n :}
   0
   TFAM-N@ 0 ?do
      i ta tu pa pu FAM-HIT? if 1+ then
   loop ;

\ ---- whole-family readers (-1 when the identity resolves nothing) -------------
: KIND ( ptr u8 n ptr u8 n -- n )
   FAM-ID {: fam:n :}   fam 0 < if -1 exit then  fam TFAM-KIND@ ;
: ARITY ( ptr u8 n ptr u8 n -- n )
   FAM-ID {: fam:n :}   fam 0 < if -1 exit then  fam TFAM-ARITY@ ;
: WIDTH ( ptr u8 n ptr u8 n -- n )
   FAM-ID {: fam:n :}   fam 0 < if -1 exit then  fam TFAM-WIDTH@ ;
: VIS ( ptr u8 n ptr u8 n -- n )                   \ 1 public, 0 private, -1 missing
   FAM-ID {: fam:n :}
   fam 0 < if -1 exit then
   fam TFAM-PUBLIC? if 1 else 0 then ;
: VARS ( ptr u8 n ptr u8 n -- n )   FAM-ID LIVE-VARS ;

\ ---- per-case readers (case order is what these pin) --------------------------
: ARM$ ( ptr u8 n ptr u8 n n -- ptr u8 n ) {: ta:ptr tu:n pa:ptr pu:n k:n :}
   ta tu pa pu FAM-ID k LIVE-VAR {: var:n :}
   var 0 < if s" <missing>" exit then
   var SUMV-NAME$ ;
: ARM-CTOR$ ( ptr u8 n ptr u8 n n -- ptr u8 n ) {: ta:ptr tu:n pa:ptr pu:n k:n :}
   ta tu pa pu FAM-ID k LIVE-VAR {: var:n :}
   var 0 < if s" <missing>" exit then
   var SUMV-CTOR-PKG$ ;
\ A case index that does not exist must answer the sentinel, NOT fall through to the
\ record path: TYPE-FIELD:NO-VARIANT is itself -1, so passing an unresolved case straight
\ down would silently count the family's NO-VARIANT rows and report a real number for a
\ case that was never declared. Both case readers therefore refuse `var` first.
: ARM-FLDS ( ptr u8 n ptr u8 n n -- n ) {: ta:ptr tu:n pa:ptr pu:n k:n :}
   ta tu pa pu FAM-ID {: fam:n :}
   fam k LIVE-VAR {: var:n :}
   var 0 < if -1 exit then
   fam var LIVE-FLDS ;
: ARM-SLOT ( ptr u8 n ptr u8 n n ptr u8 n -- n ) {: ta:ptr tu:n pa:ptr pu:n k:n na:ptr nu:n :}
   ta tu pa pu FAM-ID {: fam:n :}
   fam k LIVE-VAR {: var:n :}
   var 0 < if -1 exit then
   fam var na nu LIVE-SLOT ;
: ARM-CELLS ( ptr u8 n ptr u8 n n ptr u8 n -- n ) {: ta:ptr tu:n pa:ptr pu:n k:n na:ptr nu:n :}
   ta tu pa pu FAM-ID {: fam:n :}
   fam k LIVE-VAR {: var:n :}
   var 0 < if -1 exit then
   fam var na nu LIVE-CELLS ;

\ ---- record readers: a record's fields hang off NO-VARIANT --------------------
: FLDS ( ptr u8 n ptr u8 n -- n )
   FAM-ID {: fam:n :}   fam TYPE-FIELD:NO-VARIANT LIVE-FLDS ;
: SLOT ( ptr u8 n ptr u8 n ptr u8 n -- n ) {: ta:ptr tu:n pa:ptr pu:n na:ptr nu:n :}
   ta tu pa pu FAM-ID {: fam:n :}
   fam TYPE-FIELD:NO-VARIANT na nu LIVE-SLOT ;
: CELLS ( ptr u8 n ptr u8 n ptr u8 n -- n ) {: ta:ptr tu:n pa:ptr pu:n na:ptr nu:n :}
   ta tu pa pu FAM-ID {: fam:n :}
   fam TYPE-FIELD:NO-VARIANT na nu LIVE-CELLS ;

;package
