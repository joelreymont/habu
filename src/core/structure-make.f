\ structure-make.f: a product family's sealed MAKE / UNMAKE (package
\ STRUCTURE-MAKE), built by sumtype.f's constructor machinery, and its `DERIVE
\ addr` accessor surface. Loads just before structure-decl.f; ;STRUCTURE for a
\ family with fields calls GENERATE. A product bundle is its field cells in slot
\ order, so MAKE and UNMAKE are casts the checker certifies, with no TRUST,
\ TRUSTED: or set-check.

using SCHEMA-REG
using TFAM
using TYPE-DECL

package TYPE-DECL
private
package STRUCTURE-MAKE

: SM-FIELD-SCHEMA@ ( n n -- n ) {: idx:n fs:n :}
   fs idx + PF-PENDING-SCH@ ;

\ A field's bundle roots: its schema, or for a buffer one `n` per cell, the cells
\ MAKE and UNMAKE carry its bytes in.
: SM-FIELD-ROOTS+ ( n n -- ) {: idx:n fs:n :}
   fs idx + PF-ROW {: r:^ :}
   r PF.FLAGS @ PF-FLAG-BUFFER = IF
      r PF.CELLS @ 0 ?do CC-N SCHEMA-CON SCHEMA-ROOT+ drop loop
      EXIT
   THEN
   idx fs SM-FIELD-SCHEMA@ SCHEMA-ROOT@ SCHEMA-ROOT+ drop ;

\ The field rows are still provisional, at PF-COMMIT-N or above, where a
\ committed reader dies 76.
: SM-EMIT-ROWS ( n n n -- )
   {: fam:n fs:n fc:n :}
   SCHEMA-ROOT-N@ {: rstart:n :}
   0 BEGIN dup fc < WHILE
      dup fs SM-FIELD-ROOTS+
      1 +
   REPEAT drop
   SCHEMA-ROOT-N@ rstart - {: rn:n :}
   SUMV-N@ {: vstart:n :}
   fam s" make"   0 rstart rn  fam TFAM-SLOTS@ SUMV-ADD drop
   fam s" unmake" 1 rstart rn  fam TFAM-SLOTS@ SUMV-ADD drop
   fam vstart 2 TFAM-VAR-RANGE!
   fam vstart 2 TDECL-CTOR-PUBLISH ;

public

\ A public family gets FAMILY:MAKE / FAMILY:UNMAKE in its constructor namespace;
\ a private or OPAQUE one gets FAMILY-MAKE / FAMILY-UNMAKE in the declaring
\ package's private wordlist (type-family.f TF-CTOR-PRIV$, TFAM-GEN-PUBLIC?).
\ Its caller runs it only for a structure with fields. A replayed declaration
\ (verify-source.f) registers the checked effects only: no code and no
\ dictionary entries.
: GENERATE ( n -- ) {: fam:n :}
   fam TFAM-FLD-COUNT@ {: fc:n :}
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

public

: PART-SNAPSHOT ( -- ) SM-DISARM ;


\ Replay registers the accessors' checked effects and emits no code, so a tool
\ that pre-scans a source sees the record's accessors.
: PART-COMMIT ( -- )
   SM-ARMED-FAM {: fam:n :}
   fam SM-NO-FAMILY = IF EXIT THEN
   DECL-REPLAY:RP-ACTIVE? IF
      fam TFAM-DERIVE-ADDR? IF fam TDECL-ADDR-REPLAY THEN
      fam TFAM-DERIVE-INIT? IF fam TDECL-INIT-REPLAY THEN
      EXIT
   THEN
   fam TFAM-DERIVE-ADDR? IF fam TDECL-ADDR-WORDS THEN
   fam TFAM-DERIVE-INIT? IF fam TDECL-INIT-WORDS THEN ;

: PART-ROLLBACK ( -- ) SM-DISARM ;

: PART-RELEASE ( -- ) SM-DISARM ;

\ The structure declaration arms only a family with fields that derives addr or
\ init, and rejects any other first.
: ARM ( n -- ) SM-ARMED ! ;

;package
;package

;using
;using
;using
