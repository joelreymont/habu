\ Loaded after the real source-checker handoff by native-window-owner-child.f.
\ The two payloads come from ordinary STRUCTURE declarations at one base.
s" src/habu/layout.f" provided
s" src/core/checker-owner-abi.f" provided
require src/habu/xref.f
require src/core/generated-declaration-dictionary.f
require src/core/generated-declaration-protection.f

package TFAM
using SCHEMA-REG
using STRUCTURE-DECL
using ENUM-DECL

$10000 constant RI-CAP
$40000 constant RI-STATE-CAP
create RI-A RI-CAP allot
create RI-B RI-CAP allot
create RI-BAD RI-CAP allot
create RI-STATE RI-STATE-CAP allot
create RI-CELLS 32 cells allot
variable RI-A-U variable RI-B-U
variable RI-CURSOR variable RI-COMPARE variable RI-USED

: RI-EQ ( n n -- ) <> IF 79 throw THEN ;

TRUSTED: RI-BUILD-A ( -- )
   CHECKER-SCOPE-START REG-AOT-MARK
   s" ri-same" s" 0 FIELD value n ;STRUCTURE" SD-REPLAY
   s" ri-colour" s" red green ;ENUM" ED-REPLAY
   s" ri-nested" s" 0 FIELD value ri-same ;STRUCTURE" SD-REPLAY
   s" ri-param" s" 1 FIELD value a ;STRUCTURE" SD-REPLAY
   s" ri-pointer" s" 0 FIELD value ptr n ;STRUCTURE" SD-REPLAY
   s" ri-choice" s" 0 VARIANT value FIELD item n ;VARIANT VARIANT empty ;VARIANT ;ENUM" ED-REPLAY
   s" ri-wide" s" 0 FIELD left n FIELD right r ;STRUCTURE" SD-REPLAY
   s" ri-wide-holder" s" 0 FIELD value ri-wide ;STRUCTURE" SD-REPLAY
   s" ri-packed" s" POLICY packed-tag red green ;ENUM" ED-REPLAY
   s" ri-quoted" s" 1 VARIANT call [ a -- a | n -- n ] ;VARIANT VARIANT plain n ;VARIANT" CHECKER-DEFSUM
   REG-AOT-CLOSE RI-A RI-CAP REG-AOT-SAVE RI-A-U !
   CHECKER-SCOPE-DONE ;

TRUSTED: RI-BUILD-B ( -- )
   CHECKER-SCOPE-START REG-AOT-MARK
   s" ri-same" s" 0 FIELD value r ;STRUCTURE" SD-REPLAY
   s" ri-colour" s" red green ;ENUM" ED-REPLAY
   s" ri-nested" s" 0 FIELD value ri-same ;STRUCTURE" SD-REPLAY
   s" ri-param" s" 1 FIELD value a ;STRUCTURE" SD-REPLAY
   s" ri-pointer" s" 0 FIELD value ptr n ;STRUCTURE" SD-REPLAY
   s" ri-choice" s" 0 VARIANT value FIELD item n ;VARIANT VARIANT empty ;VARIANT ;ENUM" ED-REPLAY
   s" ri-wide" s" 0 FIELD left n FIELD right r ;STRUCTURE" SD-REPLAY
   s" ri-wide-holder" s" 0 FIELD value ri-wide ;STRUCTURE" SD-REPLAY
   s" ri-packed" s" POLICY packed-tag red green ;ENUM" ED-REPLAY
   s" ri-quoted" s" 1 VARIANT call [ a -- a | n -- n ] ;VARIANT VARIANT plain n ;VARIANT" CHECKER-DEFSUM
   REG-AOT-CLOSE RI-B RI-CAP REG-AOT-SAVE RI-B-U !
   CHECKER-SCOPE-DONE ;

TRUSTED: RI-BYTES ( ptr u8 n -- ) {: src:ptr bytes:n :}
   bytes RI-STATE-CAP RI-CURSOR @ - > IF 79 throw THEN
   bytes 0 ?do
      RI-COMPARE @ IF
         src i + c@ RI-STATE RI-CURSOR @ i + + c@ RI-EQ
      ELSE src i + c@ RI-STATE RI-CURSOR @ i + + c! THEN
   loop
   bytes RI-CURSOR +! ;

\ Compare every published store byte, count and both lookup indices. This
\ includes constructor associations and the runtime fields ignored by identity.
TRUSTED: RI-STATE-BYTES ( -- )
   0 RI-CURSOR !
   REG-AOT-N 0 ?do
      i REG-AOT-COUNT RI-CELLS i cells + !
      i REG-AOT-BASE-PTR i REG-AOT-COUNT i REG-AOT-WIDTH * RI-BYTES
   loop
   RI-CELLS REG-AOT-N cells RI-BYTES
   TFX-READY @ RI-CELLS ! TFX-HI @ RI-CELLS CELL + !
   TFX-CAP @ RI-CELLS 2 cells + ! TFX-SLOTS RI-CELLS 3 cells + !
   SVX-GEN @ RI-CELLS 4 cells + ! SVX-HI @ RI-CELLS 5 cells + !
   RI-CELLS 6 cells RI-BYTES
   TFX-BASE BYTE-VIEW TFX-SLOTS cells RI-BYTES
   SYM-N @ 1 ?do i SVX@ RI-CELLS ! RI-CELLS CELL RI-BYTES loop ;

TRUSTED: RI-SAVE-STATE ( -- )
   TFX-ENSURE SVX-ENSURE
   0 RI-COMPARE ! RI-STATE-BYTES RI-CURSOR @ RI-USED ! ;

TRUSTED: RI-SAME-STATE ( -- )
   -1 RI-COMPARE ! RI-STATE-BYTES RI-CURSOR @ RI-USED @ RI-EQ ;

TRUSTED: RI-REFUSED ( ptr u8 n -- )
   [: REG-AOT-INSTALL ;] catch
   76 RI-EQ 2drop
   RI-SAME-STATE
   REG-AOT-MEMO-U @ 0 RI-EQ
   REG-AOT-MEMO @ NULL-PTR = 0= IF 79 throw THEN ;

TRUSTED: RI-COPY-A ( -- ) RI-A RI-BAD RI-A-U @ USIGS-COPY ;

TRUSTED: RI-COUNTS-AGREE ( -- )
   RI-A-U @ RI-B-U @ RI-EQ
   REG-AOT-N 0 ?do
      RI-A i REG-AOT-ROW@ RI-B i REG-AOT-ROW@
      {: ab:n ac:n au:n bb:n bc:n bu:n :}
      ab bb RI-EQ ac bc RI-EQ au bu RI-EQ
   loop
   \ Every store is nonempty: families, kinds, variants, fields, layouts,
   \ strings, schemas, and roots all come from actual declarations.
   REG-AOT-N 0 ?do RI-A i REG-AOT-ROW@ drop nip 0 > 0= IF 79 throw THEN loop
   RI-A 2 REG-AOT-ROW@ drop nip 0 > 0= IF 79 throw THEN
   RI-A 3 REG-AOT-ROW@ drop nip 0 > 0= IF 79 throw THEN
   RI-A 6 REG-AOT-ROW@ drop nip 0 > 0= IF 79 throw THEN ;

TRUSTED: RI-FRESH-REFUSALS ( -- )
   RI-SAVE-STATE
   RI-COPY-A
   \ Corrupt the final store, after valid family, field and schema sections.
   RI-BAD 7 REG-AOT-ROW * 24 + + dup REG-AOT-U64@ 1+ swap REG-AOT-U64!
   RI-BAD RI-A-U @ RI-REFUSED
   RI-COPY-A
   $7FFFFFFFFFFFFFFF RI-BAD 7 REG-AOT-ROW * 16 + + REG-AOT-U64!
   RI-BAD RI-A-U @ RI-REFUSED
   RI-COPY-A
   -1 RI-BAD 6 REG-AOT-ROW * 8 + + REG-AOT-U64!
   RI-BAD RI-A-U @ RI-REFUSED ;

\ These preserve the outer table and corrupt the final semantic records instead.
\ The private install path must refuse before any family row or index appears.
TRUSTED: RI-FIRST ( n -- ptr n ) {: store:n :}
   RI-BAD store REG-AOT-ROW@ drop drop {: first:n :}
   RI-BAD RI-A-U @ store first REG-AOT-ITEM ;

TRUSTED: RI-SEMANTIC-REFUSALS ( -- )
   RI-SAVE-STATE
   RI-COPY-A
   $7FFFFFFFFFFFFFFF 0 RI-FIRST TF.NAME-U !
   RI-BAD RI-A-U @ RI-REFUSED
   RI-COPY-A
   $7FFFFFFFFFFFFFFF 3 RI-FIRST PF.SCH !
   RI-BAD RI-A-U @ RI-REFUSED
   RI-COPY-A
   $7FFFFFFFFFFFFFFF 7 RI-FIRST !
   RI-BAD RI-A-U @ RI-REFUSED
   RI-COPY-A
   0 6 RI-FIRST !
   RI-BAD RI-A-U @ RI-REFUSED
   RI-COPY-A
   CC-MAX 6 RI-FIRST CELL + !
   RI-BAD RI-A-U @ RI-REFUSED
   RI-COPY-A
   1 6 RI-FIRST 2 cells + !
   RI-BAD RI-A-U @ RI-REFUSED
   RI-COPY-A
   SCH-PTR 6 RI-FIRST !
   RI-BAD 6 REG-AOT-ROW@ drop drop 6 RI-FIRST CELL + !
   RI-BAD RI-A-U @ RI-REFUSED
   RI-COPY-A
   $7FFFFFFFFFFFFFFF 2 RI-FIRST SV.TAG !
   RI-BAD RI-A-U @ RI-REFUSED
   RI-COPY-A
   $7FFFFFFFFFFFFFFF 3 RI-FIRST PF.CELLS !
   RI-BAD RI-A-U @ RI-REFUSED
   RI-COPY-A
   SCH-PARAM 6 RI-FIRST ! 0 6 RI-FIRST CELL + !
   RI-BAD RI-A-U @ RI-REFUSED
   RI-COPY-A
   SCH-APP 6 RI-FIRST !
   RI-BAD 0 REG-AOT-ROW@ drop drop 6 RI-FIRST CELL + !
   RI-BAD RI-A-U @ RI-REFUSED
   RI-COPY-A
   2 0 RI-FIRST TF.SLOTS ! 2 3 RI-FIRST PF.CELLS ! 16 3 RI-FIRST PF.BYTES !
   RI-BAD RI-A-U @ RI-REFUSED
   RI-COPY-A
   16 3 RI-FIRST PF.BYTES !
   RI-BAD RI-A-U @ RI-REFUSED
   RI-COPY-A
   PK-LAYOUT 1 RI-FIRST !
   RI-BAD RI-A-U @ RI-REFUSED
   RI-COPY-A
   99 4 RI-FIRST LAY.SIZE !
   RI-BAD RI-A-U @ RI-REFUSED ;

\ Imported constructors deliberately lose source symbol IDs; family tail links
\ are rebuilt in the destination. Neither field may defeat duplicate loading.
TRUSTED: RI-CANONICAL-REUSE ( -- )
   RI-COPY-A
   $123 RI-BAD REG-AOT-HDR + TF.TAILNEXT !
   RI-A 0 REG-AOT-ROW@ nip nip
   RI-A 1 REG-AOT-ROW@ nip nip +
   RI-BAD REG-AOT-HDR + + SV.CTOR-SYM $456 swap !
   RI-BAD RI-A-U @ REG-AOT-LOAD RI-SAME-STATE ;

\ Keep the v8 table and carried prefix valid while its final nonempty store
\ claims a fresh append and every earlier nonempty store is already installed.
TRUSTED: RI-MIXED-STATE ( -- )
   RI-COPY-A
   RI-BAD 7 REG-AOT-ROW@ {: base:n count:n bytes:n :}
   count 7 REG-AOT-WIDTH * {: extra:n :}
   extra RI-CAP RI-A-U @ - > IF 79 throw THEN
   RI-A RI-A-U @ extra - + RI-BAD RI-A-U @ + extra USIGS-COPY
   base count + RI-BAD 7 REG-AOT-ROW * 8 + + REG-AOT-U64!
   bytes extra + RI-BAD 7 REG-AOT-ROW * 24 + + REG-AOT-U64!
   RI-BAD RI-A-U @ extra + RI-REFUSED
   REG-AOT-ERROR-A @ REG-AOT-ERROR-U @
   s" tfam: seeded registry stores disagree about installation" CORE-STR= 0= IF 79 throw THEN ;

TRUSTED: RI-REUSE-REFUSALS ( -- )
   RI-SAVE-STATE
   RI-A RI-A-U @ REG-AOT-LOAD RI-SAME-STATE
   RI-CANONICAL-REUSE
   RI-B RI-B-U @ RI-REFUSED
   RI-MIXED-STATE
   RI-COPY-A
   RI-BAD 8 + dup REG-AOT-U64@ 3 + swap REG-AOT-U64!
   RI-BAD RI-A-U @ RI-REFUSED
   RI-A RI-A-U @ REG-AOT-LOAD RI-SAME-STATE ;

\ A fresh delta must bind the prefix contents, not only its eight counts.
TRUSTED: RI-PREFIX-IDENTITY ( -- )
   CHECKER-SCOPE-START
   s" ri-prefix" s" 0 FIELD value n ;STRUCTURE" SD-REPLAY
   REG-AOT-MARK REG-AOT-CLOSE
   RI-B RI-CAP REG-AOT-SAVE RI-B-U !
   RI-B-U @ 0 > 0= IF 79 throw THEN
   REG-AOT-N 0 ?do RI-B i REG-AOT-ROW@ drop nip 0 RI-EQ loop
   s" ri-suffix" s" 0 FIELD value ri-prefix ;STRUCTURE" SD-REPLAY
   REG-AOT-CLOSE RI-A RI-CAP REG-AOT-SAVE RI-A-U !
   CHECKER-SCOPE-DONE
   CHECKER-SCOPE-START
   s" ri-prefix" s" 0 FIELD value r ;STRUCTURE" SD-REPLAY
   REG-AOT-N 0 ?do RI-A i REG-AOT-ROW@ drop drop i REG-AOT-COUNT RI-EQ loop
   RI-SAVE-STATE RI-A RI-A-U @ RI-REFUSED
   RI-B RI-B-U @ RI-REFUSED
   REG-AOT-ERROR-A @ REG-AOT-ERROR-U @
   s" tfam: a seeded type registry differs from its captured prefix" CORE-STR= 0= IF 79 throw THEN
   CHECKER-SCOPE-DONE
   CHECKER-SCOPE-START
   s" ri-prefix" s" 0 FIELD value n ;STRUCTURE" SD-REPLAY
   RI-SAVE-STATE RI-B RI-B-U @ REG-AOT-LOAD RI-SAME-STATE
   RI-A RI-A-U @ REG-AOT-LOAD
   CHECKER-SCOPE-DONE ;

public

TRUSTED: RI-RUN ( -- )
   RI-PREFIX-IDENTITY
   RI-BUILD-A RI-BUILD-B RI-COUNTS-AGREE
   RI-FRESH-REFUSALS RI-SEMANTIC-REFUSALS
   RI-A RI-A-U @ REG-AOT-LOAD
   RI-REUSE-REFUSALS
   s" registry identity and refusal atomicity: ok" type cr ;

;package
TFAM:RI-RUN
