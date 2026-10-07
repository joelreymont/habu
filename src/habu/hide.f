\ hide.f - the executable spec of tools/bootstrap.sh's BOOT-* index words.
\
\ tools/bootstrap.sh's boot-hide prologue finds the record it lowers the
\ dictionary to with BOOT-* words it emits as text, for the gforth stage0 and
\ the sealed native stages alike, and lowers it with its own top-level
\ `seed-ndict!`. These are their BFR-* twins - the record walk, the case-folded
\ name match and the earlier-of-two-markers index - and
\ tools/bootstrap-codegen-test.f includes this file to drive them against the
\ running dictionary. Nothing else loads it.

0 constant BFR-START-SLOT
2 constant BFR-FLAGS-SLOT
3 constant BFR-NAME-SLOT

-1 constant BFR-NOT-FOUND
24 constant BFR-INLINE-OFF

\ THE RECORD VIEW. A dictionary record is addressed by an integer: dbase@ plus
\ an index times DREC. A pointer CAST: is declared only in a package's private
\ section, so the one refinement, N>REC, a record by index, is package
\ BFR-PRELUDE's. A long name is read through its record's pointer field.
package BFR-PRELUDE

private

CAST: N>REC ( n -- ptr n )

public

: REC ( n -- ptr n )
   DREC * dbase@ + N>REC ;

: LONG-NAME ( ptr n -- ptr u8 )
   BFR-NAME-SLOT ptr-field @ ;

;package

: BFR-CELL@ ( ptr n n -- n )
   cells + @ ;

: BFR-START ( ptr n -- n )
   BFR-START-SLOT BFR-CELL@ ;

: BFR-FLAGS ( ptr n -- n )
   BFR-FLAGS-SLOT BFR-CELL@ ;

: BFR-NAME-LEN ( ptr n -- n )
   BFR-FLAGS DNAME-LEN-MASK and ;

: BFR-EXT? ( ptr n -- bool )
   BFR-FLAGS DNAME-EXT and 0= 0= ;

: BFR-INLINE-NAME ( ptr n -- ptr u8 )
   BFR-INLINE-OFF + BYTE-VIEW ;

: BFR-NAME-A ( ptr n -- ptr u8 )
   dup BFR-EXT? if BFR-PRELUDE:LONG-NAME exit then
   BFR-INLINE-NAME ;

: BFR-NAME$ ( ptr n -- ptr u8 n )
   dup BFR-NAME-A swap BFR-NAME-LEN ;

: BFR-FOLD-C ( n -- n )
   dup $41 < if exit then
   dup $5A > if exit then
   $20 or ;

PTR-VARIABLE BFR-A
PTR-VARIABLE BFR-B
variable BFR-U
variable BFR-V
PTR-VARIABLE BFR-SN
variable BFR-SU

: BFR-BYTE@ ( ptr u8 n -- u8 )
   + c@ ;
s" BFR-BYTE@" s" ptr u8 n -- u8" TRUST

: BFR-STR=CI ( ptr u8 n ptr u8 n -- bool )
   BFR-V ! BFR-B ! BFR-U ! BFR-A !
   BFR-U @ BFR-V @ <> if 0 0= 0= exit then
   0 begin dup BFR-U @ < while
      dup BFR-A @ swap BFR-BYTE@ BFR-FOLD-C
      over BFR-B @ swap BFR-BYTE@ BFR-FOLD-C <> if drop 0 0= 0= exit then
      1+
   repeat drop
   0 0= ;

: BFR-MATCH? ( ptr n ptr u8 n -- bool )
   BFR-U ! BFR-A !
   BFR-NAME$ BFR-A @ BFR-U @ BFR-STR=CI ;

: BFR-FIND-FIRST-INDEX ( ptr u8 n -- n )
   BFR-SU ! BFR-SN !
   0 begin dup ndict@ < while
      dup BFR-PRELUDE:REC BFR-SN @ BFR-SU @ BFR-MATCH? if exit then
      1+
   repeat drop
   BFR-NOT-FOUND ;

: BFR-REQUIRE-INDEX ( n -- n )
   dup 0 >= if exit then
   s" build-fixpoint: hide word not found" 76 die ;

: BFR-MIN-FOUND ( n n -- n ) {: a:n b:n :}
   a BFR-NOT-FOUND = if b exit then
   b BFR-NOT-FOUND = if a exit then
   a b < if a else b then ;

: BFR-MARKER-INDEX ( ptr u8 n ptr u8 n -- n ) {: a:ptr u:n b:ptr v:n :}
   a u BFR-FIND-FIRST-INDEX
   b v BFR-FIND-FIRST-INDEX
   BFR-MIN-FOUND BFR-REQUIRE-INDEX ;
