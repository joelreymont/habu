\ hide.f - bootstrap dictionary marker lookup shared with the recovery test.
\ The launcher appends this source before it clears and lowers the dictionary.

$2 constant BOOT-XREF-FLAGS-SLOT
$3 constant BOOT-XREF-NAME-SLOT
package BOOT-PRELUDE
private
CAST: N>REC ( n -- ptr n )
public
: REC ( n -- ptr n )
   DREC * dbase@ + N>REC ;
: LONG-NAME ( ptr n -- ptr u8 )
   BOOT-XREF-NAME-SLOT ptr-field @ ;
;package
: BOOT-XREF-CELL@ ( ptr n n -- n )
   cells + @ ;
: BOOT-XREF-FLAGS ( ptr n -- n )
   BOOT-XREF-FLAGS-SLOT BOOT-XREF-CELL@ ;
: BOOT-XREF-NAME-LEN ( ptr n -- n )
   BOOT-XREF-FLAGS DNAME-LEN-MASK and ;
: BOOT-XREF-EXT? ( ptr n -- bool )
   BOOT-XREF-FLAGS DNAME-EXT and 0= 0= ;
: BOOT-XREF-INLINE-NAME ( ptr n -- ptr u8 )
   $18 + byte-view ;
: BOOT-XREF-NAME-A ( ptr n -- ptr u8 ) {: rec:ptr :}
   rec BOOT-XREF-EXT? if rec BOOT-PRELUDE:LONG-NAME exit then
   rec BOOT-XREF-INLINE-NAME ;
: BOOT-XREF-NAME$ ( ptr n -- ptr u8 n ) {: rec:ptr :}
   rec BOOT-XREF-NAME-A
   rec BOOT-XREF-NAME-LEN ;
: BOOT-XREF-FOLD-C ( n -- n ) {: c:n :}
   c $41 < if c exit then
   c $5A > if c exit then
   c $20 or ;
: BOOT-XREF-STR=CI ( ptr u8 n ptr u8 n -- bool ) {: a:ptr u:n b:ptr v:n :}
   u v <> if 0 0= 0= exit then
   0 begin dup u < while
      dup a + c@ BOOT-XREF-FOLD-C
      over b + c@ BOOT-XREF-FOLD-C <> if drop 0 0= 0= exit then
      1+
   repeat drop
   0 0= ;
: BOOT-XREF-MATCH? ( ptr n ptr u8 n -- bool ) {: rec:ptr name:ptr u:n :}
   rec BOOT-XREF-NAME$ name u BOOT-XREF-STR=CI ;
: BOOT-XREF-FIND-INDEX ( ptr u8 n -- n ) {: name:ptr u:n :}
   0
   begin dup ndict@ < while
      dup BOOT-PRELUDE:REC name u BOOT-XREF-MATCH? if exit then
      1+
   repeat drop
   -1 ;
: BOOT-MIN-FOUND ( n n -- n ) {: a:n b:n :}
   a 0 < if b exit then
   b 0 < if a exit then
   a b < if a else b then ;
: BOOT-XREF-MARKER-INDEX ( ptr u8 n ptr u8 n -- n ) {: a:ptr u:n b:ptr v:n :}
   a u BOOT-XREF-FIND-INDEX  b v BOOT-XREF-FIND-INDEX  BOOT-MIN-FOUND
   dup 0 < if s" bootstrap: hide marker not found" 76 die then ;
