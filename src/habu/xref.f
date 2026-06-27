\ xref.f - live dictionary inspection words.
\
\ Load after src/habu/layout.f. These words inspect the running image dictionary
\ through dbase@/ndict@ and are intended for the REPL/debug path.

0 constant XREF-START-SLOT
1 constant XREF-LEN-SLOT
2 constant XREF-FLAGS-SLOT
3 constant XREF-NAME-SLOT
5 constant XREF-WORDLIST-SLOT

32 constant XREF-SP

TRUSTED: XREF-N>REC ( n -- ptr a ) ;
TRUSTED: XREF-A>U8 ( ptr a -- ptr u8 ) ;
TRUSTED: XREF-N>U8 ( n -- ptr u8 ) ;

: XREF-TRUE ( -- bool )
   0 0= ;

: XREF-FALSE ( -- bool )
   XREF-TRUE 0= ;

: XREF-NULL ( -- ptr a )
   0 XREF-N>REC ;

: XREF-REC-ADDR ( n -- n )
   DREC * dbase@ + ;

: XREF-REC ( n -- ptr a )
   XREF-REC-ADDR XREF-N>REC ;

: LATEST ( -- ptr a )
   ndict@ dup 0 <= if drop XREF-NULL exit then
   1- XREF-REC ;

: XREF-FOUND? ( ptr a -- bool )
   XREF-NULL <> ;

: XREF-CELL@ ( ptr a n -- n )
   cells + @ ;

: XREF-PTR@ ( ptr a n -- ptr u8 )
   XREF-CELL@ XREF-N>U8 ;

: XREF-START ( ptr a -- n )
   XREF-START-SLOT XREF-CELL@ ;

: XREF-LEN ( ptr a -- n )
   XREF-LEN-SLOT XREF-CELL@ ;

: XREF-FLAGS ( ptr a -- n )
   XREF-FLAGS-SLOT XREF-CELL@ ;

: XREF-WORDLIST ( ptr a -- n )
   XREF-WORDLIST-SLOT XREF-CELL@ ;

: XREF-NAME-LEN ( ptr a -- n )
   XREF-FLAGS DNAME-LEN-MASK and ;

: XREF-EXT? ( ptr a -- bool )
   XREF-FLAGS DNAME-EXT and 0= 0= ;

: XREF-INLINE-NAME ( ptr a -- ptr u8 )
   24 + XREF-A>U8 ;

: XREF-NAME-A ( ptr a -- ptr u8 ) {: rec :}
   rec XREF-EXT? if rec XREF-NAME-SLOT XREF-PTR@ exit then
   rec XREF-INLINE-NAME ;

: XREF-NAME$ ( ptr a -- ptr u8 n ) {: rec :}
   rec XREF-NAME-A
   rec XREF-NAME-LEN ;

: XREF-FOLD-C ( n -- n ) {: c :}
   c 65 < if c exit then
   c 90 > if c exit then
   c 32 or ;

: XREF-STR=CI ( ptr u8 n ptr u8 n -- bool ) {: a u b v :}
   u v <> if XREF-FALSE exit then
   0 begin dup u < while
      dup a + c@ XREF-FOLD-C
      over b + c@ XREF-FOLD-C <> if drop XREF-FALSE exit then
      1+
   repeat drop
   XREF-TRUE ;

: XREF-MATCH? ( ptr a ptr u8 n -- bool ) {: rec name u :}
   rec XREF-NAME$ name u XREF-STR=CI ;

: XREF-FIND ( ptr u8 n -- ptr a ) {: name u :}
   ndict@ 1-
   begin dup 0 >= while
      dup XREF-REC name u XREF-MATCH? if XREF-REC exit then
      1-
   repeat drop
   XREF-NULL ;

: XREF-NAME. ( ptr a -- )
   XREF-NAME$ type ;

: XREF-N. ( ptr u8 n n -- ) {: label:ptr labelu value :}
   label labelu type
   XREF-SP emit
   value . ;

: XREF. ( ptr a -- ) {: rec :}
   rec XREF-FOUND? 0= if s" xref: not found" type cr exit then
   s" name " type rec XREF-NAME. cr
   s" start" rec XREF-START XREF-N.
   s" len" rec XREF-LEN XREF-N.
   s" flags" rec XREF-FLAGS XREF-N.
   s" wordlist" rec XREF-WORDLIST XREF-N. ;

: XREF ( -- )
   parse-name XREF-FIND XREF. ;

: SEE ( -- )
   XREF ;

: WORDS ( -- )
   0 begin dup ndict@ < while
      dup XREF-REC XREF-NAME. space
      1+
   repeat drop cr ;
