\ hide.f - refresh prelude dictionary truncation.
\
\ Loaded before the native refresh source reloads common engine files. The words
\ intentionally use a BFR prefix so they can be defined in old engines that lack
\ xref.f, then hide themselves by truncating back to the requested marker.

0 constant BFR-START-SLOT
2 constant BFR-FLAGS-SLOT
3 constant BFR-NAME-SLOT

-1 constant BFR-NOT-FOUND
24 constant BFR-INLINE-OFF

TRUSTED: BFR-N>REC ( n -- ptr a ) ;
TRUSTED: BFR-A>U8 ( ptr a -- ptr u8 ) ;
TRUSTED: BFR-N>U8 ( n -- ptr u8 ) ;
TRUSTED: BFR-USIG-END-PTR ( -- ptr a ) USIGS UEND @ + ;
TRUSTED: BFR-UEND! ( n -- ) UEND ! ;
TRUSTED: BFR-NDICT! ( n -- ) ndict! ;
TRUSTED: BFR-CHECK-OFF ( -- ) 0 set-check ;

: BFR-REC-ADDR ( n -- n )
   DREC * dbase@ + ;

: BFR-REC ( n -- ptr a )
   BFR-REC-ADDR BFR-N>REC ;

: BFR-CELL@ ( ptr a n -- n )
   cells + @ ;

: BFR-PTR@ ( ptr a n -- ptr u8 )
   BFR-CELL@ BFR-N>U8 ;

: BFR-START ( ptr a -- n )
   BFR-START-SLOT BFR-CELL@ ;

: BFR-FLAGS ( ptr a -- n )
   BFR-FLAGS-SLOT BFR-CELL@ ;

: BFR-NAME-LEN ( ptr a -- n )
   BFR-FLAGS DNAME-LEN-MASK and ;

: BFR-EXT? ( ptr a -- bool )
   BFR-FLAGS DNAME-EXT and 0= 0= ;

: BFR-INLINE-NAME ( ptr a -- ptr u8 )
   BFR-INLINE-OFF + BFR-A>U8 ;

: BFR-NAME-A ( ptr a -- ptr u8 ) {: rec:ptr :}
   rec BFR-EXT? if rec BFR-NAME-SLOT BFR-PTR@ exit then
   rec BFR-INLINE-NAME ;

: BFR-NAME$ ( ptr a -- ptr u8 n ) {: rec:ptr :}
   rec BFR-NAME-A
   rec BFR-NAME-LEN ;

: BFR-FOLD-C ( n -- n ) {: c:n :}
   c $41 < if c exit then
   c $5A > if c exit then
   c $20 or ;

: BFR-STR=CI ( ptr u8 n ptr u8 n -- bool ) {: a:ptr u:n b:ptr v:n :}
   u v <> if 0 0= 0= exit then
   0 begin dup u < while
      dup a + c@ BFR-FOLD-C
      over b + c@ BFR-FOLD-C <> if drop 0 0= 0= exit then
      1+
   repeat drop
   0 0= ;

: BFR-MATCH? ( ptr a ptr u8 n -- bool ) {: rec:ptr name:ptr u:n :}
   rec BFR-NAME$ name u BFR-STR=CI ;

: BFR-USIG-TERM ( -- )
   0 BFR-USIG-END-PTR ! ;

: BFR-USIGS-RESET ( -- )
   0 BFR-UEND!
   BFR-USIG-TERM ;

: BFR-FIND-FIRST-INDEX ( ptr u8 n -- n ) {: name:ptr u:n :}
   0 begin dup ndict@ < while
      dup BFR-REC name u BFR-MATCH? if exit then
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

: BFR-HIDE-DICT-FROM-EARLIEST ( ptr u8 n ptr u8 n -- )
   BFR-MARKER-INDEX BFR-NDICT! ;
