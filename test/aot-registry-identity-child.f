\ Loaded after the real source-checker handoff by native-window-owner-child.f.
\ The two payloads come from ordinary STRUCTURE declarations at one base.
s" src/habu/layout.f" provided
s" src/core/checker-owner-abi.f" provided
require src/habu/xref.f
require src/core/generated-declaration-dictionary.f
require src/core/generated-declaration-protection.f

package TFAM

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
   s" ri-same" s" 0 FIELD value n ;STRUCTURE" STRUCTURE-DECL:SD-REPLAY
   s" ri-colour" s" red green ;ENUM" ENUM-DECL:ED-REPLAY
   REG-AOT-CLOSE RI-A RI-CAP REG-AOT-SAVE RI-A-U !
   CHECKER-SCOPE-DONE ;

TRUSTED: RI-BUILD-B ( -- )
   CHECKER-SCOPE-START REG-AOT-MARK
   s" ri-same" s" 0 FIELD value r ;STRUCTURE" STRUCTURE-DECL:SD-REPLAY
   s" ri-colour" s" red green ;ENUM" ENUM-DECL:ED-REPLAY
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
   RI-SAME-STATE ;

TRUSTED: RI-COPY-A ( -- ) RI-A RI-BAD RI-A-U @ USIGS-COPY ;

TRUSTED: RI-COUNTS-AGREE ( -- )
   RI-A-U @ RI-B-U @ RI-EQ
   REG-AOT-N 0 ?do
      RI-A i REG-AOT-ROW@ RI-B i REG-AOT-ROW@
      {: ab:n ac:n au:n bb:n bc:n bu:n :}
      ab bb RI-EQ ac bc RI-EQ au bu RI-EQ
   loop
   \ Fields and schema nodes must actually occur in this fixture.
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

\ Imported constructors deliberately lose source symbol IDs; family tail links
\ are rebuilt in the destination. Neither field may defeat duplicate loading.
TRUSTED: RI-CANONICAL-REUSE ( -- )
   RI-COPY-A
   $123 RI-BAD REG-AOT-HDR + TF.TAILNEXT !
   RI-A 0 REG-AOT-ROW@ nip nip
   RI-A 1 REG-AOT-ROW@ nip nip +
   RI-BAD REG-AOT-HDR + + SV.CTOR-SYM $456 swap !
   RI-BAD RI-A-U @ REG-AOT-LOAD RI-SAME-STATE ;

TRUSTED: RI-REUSE-REFUSALS ( -- )
   RI-SAVE-STATE
   RI-A RI-A-U @ REG-AOT-LOAD RI-SAME-STATE
   RI-CANONICAL-REUSE
   RI-B RI-B-U @ RI-REFUSED
   RI-COPY-A
   \ One late nonempty store claims it is fresh; the others are installed.
   RI-BAD 6 REG-AOT-ROW * 8 + + {: row:ptr :}
   row REG-AOT-U64@ row 8 + REG-AOT-U64@ + row REG-AOT-U64!
   RI-BAD RI-A-U @ RI-REFUSED
   RI-COPY-A
   RI-BAD 8 + dup REG-AOT-U64@ 3 + swap REG-AOT-U64!
   RI-BAD RI-A-U @ RI-REFUSED
   RI-A RI-A-U @ REG-AOT-LOAD RI-SAME-STATE ;

public

TRUSTED: RI-RUN ( -- )
   RI-BUILD-A RI-BUILD-B RI-COUNTS-AGREE
   RI-FRESH-REFUSALS
   RI-A RI-A-U @ REG-AOT-LOAD
   RI-REUSE-REFUSALS
   s" registry identity and refusal atomicity: ok" type cr ;

;package
TFAM:RI-RUN
