\ structure-make.f: a product family's sealed MAKE / UNMAKE (package
\ STRUCTURE-MAKE), built by sumtype.f's constructor machinery, and its `DERIVE
\ addr` accessor surface. Loads just before structure-decl.f; ;STRUCTURE for a
\ family with fields calls GENERATE. A product bundle is its field cells in slot
\ order, so MAKE and UNMAKE are casts the checker certifies, with no TRUST,
\ TRUSTED: or set-check.

using SCHEMA-REG
using TFAM
using TYPE-DECL

7190 constant E-SM-FAM        \ family id is not a live, public, product-kind family
7191 constant E-SM-EMPTY      \ product family declares no fields
s" family cannot own generated make/unmake"
s" report this declaration as a Habu defect: no declaration text should reach this state"
E-SM-FAM DECL-REJECT:EXPLAIN
s" a constructed family needs at least one field"
s" give the `STRUCTURE` at least one `FIELD name type`"
E-SM-EMPTY DECL-REJECT:EXPLAIN

package TYPE-DECL
private
package STRUCTURE-MAKE

: SM-FIELD-SCHEMA@ ( n n n -- n ) {: idx:n fam:n fs:n :}
   fam fs idx + DECL-EVENT:FIELD-SCHEMA@ ;

\ The field schemas are still provisional: they are read through DECL-EVENT,
\ within the open declaration, so no uncommitted row is globally readable.
: SM-EMIT-ROWS ( n n n -- )
   {: fam:n fs:n fc:n :}
   SCHEMA-ROOT-N@ {: rstart:n :}
   0 BEGIN dup fc < WHILE
      dup fam fs SM-FIELD-SCHEMA@ SCHEMA-ROOT@ SCHEMA-ROOT+ drop
      1 +
   REPEAT drop
   SUMV-N@ {: vstart:n :}
   fam s" make"   0 rstart fc  fam TFAM-SLOTS@ SUMV-ADD drop
   fam s" unmake" 1 rstart fc  fam TFAM-SLOTS@ SUMV-ADD drop
   fam vstart 2 TFAM-VAR-RANGE!
   fam vstart 2 TDECL-CTOR-PUBLISH ;

\ Validation writes no registry. Visibility is not a condition: a private product
\ publishes the same two words into its package's private wordlist.
: SM-REQUIRE-FAMILY ( n -- ) {: fam:n :}
   fam PF-FAM-LIVE? 0= IF E-SM-FAM throw THEN
   fam TFAM-PRODUCT? 0= IF E-SM-FAM throw THEN ;

: SM-REQUIRE-READABLE ( n n -- ) {: fam:n fc:n :}   \ every field row is live in this transaction
   0 BEGIN dup fc < WHILE
      dup fam fam TFAM-FLD-START@ SM-FIELD-SCHEMA@ drop
      1 +
   REPEAT drop ;

: SM-REQUIRE-UNGENERATED ( n -- ) {: fam:n :}
   fam s" make"   SUMV-FIND IF drop E-TFAM-DUP throw THEN drop
   fam s" unmake" SUMV-FIND IF drop E-TFAM-DUP throw THEN drop ;

public

\ A public family gets FAMILY:MAKE / FAMILY:UNMAKE in its constructor namespace;
\ a private or OPAQUE one gets FAMILY-MAKE / FAMILY-UNMAKE in the declaring
\ package's private wordlist (type-family.f TF-CTOR-PRIV$, TFAM-GEN-PUBLIC?).
\ Every reject comes before any registry write. A replayed declaration (verify-source.f) registers the checked effects only: no code
\ and no dictionary entries.
: GENERATE ( n -- ) {: fam:n :}
   fam SM-REQUIRE-FAMILY
   fam TFAM-FLD-COUNT@ {: fc:n :}
   fc 1 < IF E-SM-EMPTY throw THEN
   fam SM-REQUIRE-UNGENERATED
   fam fc SM-REQUIRE-READABLE
   fam  fam TFAM-FLD-START@  fc  SM-EMIT-ROWS
   DECL-REPLAY:RP-ACTIVE? IF fam TDECL-CTOR-REPLAY EXIT THEN
   fam TDECL-PROD-WORDS ;

private

\ The address-surface participant, for `DERIVE addr`. Each accessor's parameter
\ is a committed field id, so this work runs in the reversible commit, after
\ DECL-EVENT's commit and inside the dictionary owner's savepoint: a reject
\ truncates every accessor.
\ One armed family in one cell: declarations never nest.
-1 constant SM-NO-FAMILY
variable SM-ARMED   SM-NO-FAMILY SM-ARMED !

: SM-ARMED-FAM ( -- n ) SM-ARMED @ ;
: SM-DISARM ( -- ) SM-NO-FAMILY SM-ARMED ! ;

\ Visibility is not a condition: it picks the spelling and the wordlist.
: SM-ADDR-OK? ( n -- bool ) {: fam:n :}
   fam PF-FAM-LIVE? 0= IF 0 0= 0= EXIT THEN
   fam TFAM-PRODUCT? 0= IF 0 0= 0= EXIT THEN
   fam TFAM-DERIVE-ADDR? fam TFAM-DERIVE-INIT? or 0= IF 0 0= 0= EXIT THEN
   fam TFAM-FLD-COUNT@ 0 > ;

: SM-ADDR-REQUIRE ( n -- ) {: fam:n :}
   fam SM-ADDR-OK? 0= IF E-SM-FAM throw THEN ;

public

: PART-SNAPSHOT ( -- ) SM-DISARM ;

\ Re-proves the gate ARM checked, so a family made ineligible since fails before
\ any word is made.
: PART-PREPARE ( -- )
   SM-ARMED-FAM {: fam:n :}
   fam SM-NO-FAMILY <> IF fam SM-ADDR-REQUIRE THEN ;

\ Replay registers the accessors' checked effects and emits no code, so a tool
\ that pre-scans a source sees the record's accessors.
: PART-COMMIT ( -- )
   SM-ARMED-FAM {: fam:n :}
   fam SM-NO-FAMILY = IF EXIT THEN
   fam SM-ADDR-REQUIRE
   DECL-REPLAY:RP-ACTIVE? IF
      fam TFAM-DERIVE-ADDR? IF fam TDECL-ADDR-REPLAY THEN
      fam TFAM-DERIVE-INIT? IF fam TDECL-INIT-REPLAY THEN
      EXIT
   THEN
   fam TFAM-DERIVE-ADDR? IF fam TDECL-ADDR-WORDS THEN
   fam TFAM-DERIVE-INIT? IF fam TDECL-INIT-WORDS THEN ;

: PART-ROLLBACK ( -- ) SM-DISARM ;

: PART-RELEASE ( -- ) SM-DISARM ;

\ Legal only for a family that passes the gate, so a refused family fails at
\ ARM, not at commit.
: ARM ( n -- ) {: fam:n :}
   fam SM-ADDR-REQUIRE
   fam SM-ARMED ! ;

;package
;package

;using
;using
;using
