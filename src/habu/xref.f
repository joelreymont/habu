\ xref.f - live dictionary inspection words.
\
\ Load after src/habu/layout.f. These words inspect the running image dictionary
\ through dbase@/ndict@ and are intended for the REPL/debug path.

0 constant XREF-START-SLOT
1 constant XREF-LEN-SLOT
2 constant XREF-FLAGS-SLOT
3 constant XREF-NAME-SLOT
5 constant XREF-WORDLIST-SLOT
-1 constant XREF-NAMESPACE-WL
-2 constant XREF-RETIRED-WL

32 constant XREF-SP

TRUSTED: XREF-N>REC ( n -- ptr a ) ;
TRUSTED: XREF-A>U8 ( ptr a -- ptr u8 ) ;
TRUSTED: XREF-N>U8 ( n -- ptr u8 ) ;
TRUSTED: XREF-REC+ ( ptr a n -- ptr a )
   + ;

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

: XREF-RETIRED? ( ptr a -- bool )
   XREF-WORDLIST XREF-RETIRED-WL = ;

: XREF-NAME-LEN ( ptr a -- n )
   XREF-FLAGS DNAME-LEN-MASK and ;

: XREF-EXT? ( ptr a -- bool )
   XREF-FLAGS DNAME-EXT and 0= 0= ;

: XREF-INLINE-NAME ( ptr a -- ptr u8 )
   $18 XREF-REC+ XREF-A>U8 ;

: XREF-NAME-A ( ptr a -- ptr u8 )
   dup XREF-EXT? if XREF-NAME-SLOT XREF-PTR@ exit then
   XREF-INLINE-NAME ;

: XREF-NAME$ ( ptr a -- ptr u8 n )
   dup XREF-NAME-A swap XREF-NAME-LEN ;

: XREF-FOLD-C ( n -- n )
   dup $41 < if exit then
   dup $5A > if exit then
   $20 or ;

variable XREF-A
variable XREF-B
variable XREF-U
variable XREF-V
variable XREF-SN
variable XREF-SU
variable XREF-FN
variable XREF-FU
variable XREF-WID
variable XREF-IDX
variable XREF-NV

: XREF-A-FIELD ( -- ptr ptr u8 )
   XREF-A 0 ptr-field ;

: XREF-B-FIELD ( -- ptr ptr u8 )
   XREF-B 0 ptr-field ;

: XREF-SN-FIELD ( -- ptr ptr u8 )
   XREF-SN 0 ptr-field ;

: XREF-FN-FIELD ( -- ptr ptr u8 )
   XREF-FN 0 ptr-field ;

: XREF-A@ ( -- ptr u8 )
   XREF-A-FIELD @ ;

: XREF-B@ ( -- ptr u8 )
   XREF-B-FIELD @ ;

: XREF-SN@ ( -- ptr u8 )
   XREF-SN-FIELD @ ;

: XREF-FN@ ( -- ptr u8 )
   XREF-FN-FIELD @ ;

: XREF-A! ( ptr u8 -- )
   XREF-A-FIELD ! ;

: XREF-B! ( ptr u8 -- )
   XREF-B-FIELD ! ;

: XREF-SN! ( ptr u8 -- )
   XREF-SN-FIELD ! ;

: XREF-FN! ( ptr u8 -- )
   XREF-FN-FIELD ! ;

: XREF-STR=CI ( ptr u8 n ptr u8 n -- bool )
   XREF-V ! XREF-B! XREF-U ! XREF-A!
   XREF-U @ XREF-V @ <> if XREF-FALSE exit then
   0 begin dup XREF-U @ < while
      dup XREF-A@ swap ZBYTE@ XREF-FOLD-C
      over XREF-B@ swap ZBYTE@ XREF-FOLD-C <> if drop XREF-FALSE exit then
      1+
   repeat drop
   XREF-TRUE ;

: XREF-MATCH? ( ptr a ptr u8 n -- bool )
   XREF-U ! XREF-A!
   XREF-NAME$ XREF-A@ XREF-U @ XREF-STR=CI ;

: XREF-FIND-WL ( ptr u8 n n -- ptr a )
   XREF-WID ! XREF-FU ! XREF-FN!
   ndict@ 1-
   begin dup 0 >= while
      dup XREF-REC XREF-WORDLIST XREF-WID @ = if
         dup XREF-REC XREF-FN@ XREF-FU @ XREF-MATCH? if XREF-REC exit then
      then
      1-
   repeat drop
   XREF-NULL ;

: XREF-FIND-WL-INDEX ( ptr u8 n n -- n )
   XREF-WID ! XREF-FU ! XREF-FN!
   ndict@ 1-
   begin dup 0 >= while
      dup XREF-REC XREF-WORDLIST XREF-WID @ = if
         dup XREF-REC XREF-FN@ XREF-FU @ XREF-MATCH? if exit then
      then
      1-
   repeat drop
   -1 ;

variable XREF-QI
variable XREF-QWID

: XREF-QUAL-INDEX ( ptr u8 n -- n )
   XREF-SU ! XREF-SN!
   -1 XREF-QI !
   0 begin dup XREF-SU @ < while
      XREF-SN@ over ZBYTE@ $3A = if
         XREF-QI @ 0 >= if drop -2 exit then
         dup XREF-QI !
      then
      1+
   repeat drop
   XREF-QI @ dup 0 < if exit then
   dup 0= if drop -1 exit then
   dup XREF-SU @ 1- = if drop -1 exit then ;

: XREF-FIND-QUALIFIED ( ptr u8 n n -- ptr a )
   XREF-IDX ! XREF-SU ! XREF-SN!
   XREF-SN@ XREF-IDX @ XREF-NAMESPACE-WL XREF-FIND-WL
   dup XREF-FOUND? 0= if drop XREF-NULL exit then
   XREF-START XREF-QWID !
   XREF-SN@ XREF-IDX @ 1 + ZPTR+  XREF-SU @ XREF-IDX @ - 1-  XREF-QWID @  XREF-FIND-WL ;

: XREF-FIND-QUALIFIED-INDEX ( ptr u8 n n -- n )
   XREF-IDX ! XREF-SU ! XREF-SN!
   XREF-SN@ XREF-IDX @ XREF-NAMESPACE-WL XREF-FIND-WL
   dup XREF-FOUND? 0= if drop -1 exit then
   XREF-START XREF-QWID !
   XREF-SN@ XREF-IDX @ 1 + ZPTR+  XREF-SU @ XREF-IDX @ - 1-  XREF-QWID @  XREF-FIND-WL-INDEX ;

: XREF-FIND ( ptr u8 n -- ptr a )
   XREF-QUAL-INDEX
   dup -2 = if drop XREF-NULL exit then
   dup 0 >= if XREF-SN@ XREF-SU @ rot XREF-FIND-QUALIFIED exit then
   drop XREF-SN@ XREF-SU @ 0 XREF-FIND-WL ;

: XREF-FIND-INDEX ( ptr u8 n -- n )
   XREF-QUAL-INDEX
   dup -2 = if drop -1 exit then
   dup 0 >= if XREF-SN@ XREF-SU @ rot XREF-FIND-QUALIFIED-INDEX exit then
   drop XREF-SN@ XREF-SU @ 0 XREF-FIND-WL-INDEX ;

: XREF-FIND-CURRENT-INDEX ( ptr u8 n -- n )
   get-current XREF-FIND-WL-INDEX ;

: XREF-FIND-TARGET-INDEX ( ptr u8 n -- n )
   XREF-QUAL-INDEX
   dup -2 = if drop -1 exit then
   dup 0 >= if XREF-SN@ XREF-SU @ rot XREF-FIND-QUALIFIED-INDEX exit then
   drop XREF-SN@ XREF-SU @ XREF-FIND-CURRENT-INDEX ;

: XREF-REQUIRE-INDEX ( n -- n )
   dup 0 >= if exit then
   s" xref: word not found" 76 die ;

: XREF-REQUIRE-UNDEFINE ( n -- n )
   dup 0 >= if exit then
   s" undefine: word not found" 70 die ;

TRUSTED: XREF-PATCH32 ( n ptr a -- )
   patch32 ;

: XREF-RETIRE ( ptr a -- )
   dup XREF-WORDLIST-SLOT cells XREF-REC+
   dup XREF-RETIRED-WL swap XREF-PATCH32
   $4 XREF-REC+ -1 swap XREF-PATCH32
   drop ;

: XREF-RETIRE-WL ( ptr u8 n n -- )
   XREF-WID ! XREF-FU ! XREF-FN!
   ndict@ 1-
   begin dup 0 >= while
      dup XREF-REC XREF-WORDLIST XREF-WID @ = if
         dup XREF-REC XREF-FN@ XREF-FU @ XREF-MATCH? if
            dup XREF-REC XREF-RETIRE
         then
      then
      1-
   repeat drop ;

: UNDEFINE-NAME ( ptr u8 n -- )
   XREF-SU ! XREF-SN!
   XREF-SN@ XREF-SU @ XREF-FIND-TARGET-INDEX XREF-REQUIRE-UNDEFINE XREF-REC XREF-WORDLIST XREF-WID !
   XREF-SN@ XREF-SU @ XREF-WID @ XREF-RETIRE-WL
   XREF-SN@ XREF-SU @ CHECKER-UNDEFINE ;

: UNDEFINE-FOUND ( ptr u8 n n -- )
   XREF-IDX ! XREF-SU ! XREF-SN!
   XREF-IDX @ XREF-REC XREF-WORDLIST
   XREF-SN@ XREF-SU @ rot XREF-RETIRE-WL
   XREF-SN@ XREF-SU @ CHECKER-UNDEFINE ;

: UNDEFINE-IF-DEFINED ( ptr u8 n -- )
   XREF-SU ! XREF-SN!
   XREF-SN@ XREF-SU @ XREF-FIND-INDEX
   dup 0 >= if XREF-SN@ XREF-SU @ rot UNDEFINE-FOUND else drop then ;

: HIDE-DEFS-FROM ( ptr u8 n -- )
   XREF-SU ! XREF-SN!
   XREF-SN@ XREF-SU @ CHECKER-USIGS-TRUNCATE-FROM
   XREF-SN@ XREF-SU @ XREF-FIND-INDEX XREF-REQUIRE-INDEX ndict! ;

: XREF-NAME. ( ptr a -- )
   XREF-NAME$ type ;

: XREF-N. ( ptr u8 n n -- )
   XREF-NV ! XREF-FU ! XREF-FN!
   XREF-FN@ XREF-FU @ type
   XREF-SP emit
   XREF-NV @ . ;

: XREF. ( ptr a -- )
   dup XREF-FOUND? 0= if drop s" xref: not found" type cr exit then
   s" name " type dup XREF-NAME. cr
   dup s" start" rot XREF-START XREF-N.
   dup s" len" rot XREF-LEN XREF-N.
   dup s" flags" rot XREF-FLAGS XREF-N.
   s" wordlist" rot XREF-WORDLIST XREF-N. ;

: XREF ( -- )
   parse-name XREF-FIND XREF. ;

: SEE ( -- )
   XREF ;

: undefine ( -- )
   parse-name dup 0= if s" undefine: missing name" 70 die then
   UNDEFINE-NAME ;

: WORDS ( -- )
   0 begin dup ndict@ < while
      dup XREF-REC dup XREF-RETIRED? if drop else XREF-NAME. space then
      1+
   repeat drop cr ;
