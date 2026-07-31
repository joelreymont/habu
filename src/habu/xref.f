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
: XREF-REC+ ( ptr a n -- ptr a )
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

\ DNAME-WIDE (dot habu-tfam-12-interpret-10b385b1): the record's recorded stack
\ effect carries a wider-than-cell layout value, so the engine's interpret
\ dispatch and interpret ' fail closed before such a bundle can land on the
\ untyped interpret stack (scalar dup/drop/swap silently corrupt it). Marking
\ is the engine prim `wide-mark` (habu1.f BWIDEMARK - the dict region is
\ read-only at runtime, so the write needs the engine's mprotect bracket);
\ monotonic, no unmark. This is the read-side introspection query.
: XREF-WIDE? ( ptr a -- bool )
   XREF-FLAGS DNAME-WIDE and 0= 0= ;

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

PTR-VARIABLE XREF-A
PTR-VARIABLE XREF-B
variable XREF-U
variable XREF-V
PTR-VARIABLE XREF-FN
variable XREF-FU
variable XREF-WID
variable XREF-NV

: XREF-A@ ( -- ptr u8 )
   XREF-A @ ;

: XREF-B@ ( -- ptr u8 )
   XREF-B @ ;

: XREF-FN@ ( -- ptr u8 )
   XREF-FN @ ;

: XREF-A! ( ptr u8 -- )
   XREF-A ! ;

: XREF-B! ( ptr u8 -- )
   XREF-B ! ;

: XREF-FN! ( ptr u8 -- )
   XREF-FN ! ;

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

: XREF-FIND-WL ( ptr u8 n n -- ptr a )
   XREF-FIND-WL-INDEX
   dup 0 >= IF XREF-REC EXIT THEN
   drop XREF-NULL ;

package XREF

public

0 constant FOUND
1 constant NONE
2 constant MALFORMED

private

: KIND@ ( ptr a -- n )
   XREF-FLAGS DNAME-MIN-IN-MASK and 52 rshift ;

: RESULT ( n -- n n )
   dup 0 >= IF FOUND ELSE NONE THEN ;

: QUAL-WID ( ptr u8 n -- n bool )
   XREF-NAMESPACE-WL XREF-FIND-WL-INDEX
   dup 0 < IF drop 0 XREF-FALSE EXIT THEN
   XREF-REC
   dup KIND@
   dup NAMESPACE:KIND-PACKAGE =
   swap NAMESPACE:KIND-TYPE = or 0= IF
      drop 0 XREF-FALSE
      EXIT
   THEN
   XREF-START XREF-TRUE ;

public

: NAMESPACE? ( ptr u8 n -- bool )
   QUAL-WID nip ;

: RESOLVE ( ptr u8 n n -- n n ) {: a:ptr u:n bare-wid:n :}
   a u QNAME:SPLIT
   {: qa:ptr qu:n ta:ptr tu:n kind:n :}
   kind QNAME:MALFORMED = IF -1 MALFORMED EXIT THEN
   kind QNAME:BARE = IF
      ta tu bare-wid XREF-FIND-WL-INDEX RESULT
      EXIT
   THEN
   qa qu QUAL-WID 0= IF drop -1 NONE EXIT THEN
   {: qwid:n :}
   ta tu qwid XREF-FIND-WL-INDEX RESULT ;

: GUARD ( n n -- n n )
   dup MALFORMED = IF 2drop QNAME:E-SYNTAX throw THEN ;

: REQUIRE ( n n ptr u8 n n -- n )
   {: idx:n status:n msg:ptr msgu:n code:n :}
   status MALFORMED = IF QNAME:E-SYNTAX throw THEN
   status FOUND = IF idx EXIT THEN
   msg msgu code die ;

;package

: XREF-FIND ( ptr u8 n -- ptr a )
   0 XREF:RESOLVE XREF:GUARD
   dup XREF:FOUND <> IF 2drop XREF-NULL EXIT THEN
   drop XREF-REC ;

: XREF-REQUIRE-INDEX ( n n -- n )
   s" xref: word not found" 76 XREF:REQUIRE ;

: XREF-REQUIRE-UNDEFINE ( n n -- n )
   s" undefine: word not found" 70 XREF:REQUIRE ;

package GENERATED-DECL-NAME-PREFLIGHT

private

$7FFFFFFFFFFFFFFF constant COUNT-MAX

public

: CHECK ( ptr u8 n -- ) {: a:ptr u:n :}
   a u TFAM-CTOR-WORD? 0= IF
      s" xref: generated declaration visibility mismatch" 76 die
   THEN
   a u XREF-FIND XREF-FOUND? IF
      s" xref: generated declaration already exists" 76 die
   THEN ;

: DICTIONARY-RECORDS ( ptr u8 n n -- n ) {: a:ptr u:n words:n :}
   words 0 <= words COUNT-MAX >= or IF
      s" xref: generated declaration word count overflow" 76 die
   THEN
   words 1 + ;

private

: INSTALL ( -- )
   [: CHECK ;] is TDECL-NAME-PREFLIGHT-XT
   [: XREF:NAMESPACE? ;] is TDECL-NS-EXISTS-XT ;

INSTALL

;package

TRUSTED: XREF-PATCH32 ( n ptr a -- )
   patch32 ;

package XREF

private

: NS-MATCH? ( n ptr u8 n -- bool ) {: i:n a:ptr u:n :}
   i XREF-REC dup XREF-WORDLIST XREF-NAMESPACE-WL <> IF
      drop XREF-FALSE EXIT
   THEN
   a u XREF-MATCH? ;

: FIND-NS ( ptr u8 n -- ptr a n ) {: a:ptr u:n :}
   XREF-NULL 0
   0 BEGIN dup ndict@ < WHILE
      dup a u NS-MATCH? IF
         dup XREF-REC 2swap swap drop 1+ rot
      THEN
      1+
   REPEAT
   drop ;

: WID-VALID? ( n -- bool )
   dup FIRST-DYNAMIC-WID >= swap OWNER-WID-LIMIT < and ;

: SET-TYPE ( ptr a -- )
   dup XREF-LEN-SLOT cells XREF-REC+ 0 swap XREF-PATCH32
   dup XREF-FLAGS NAMESPACE:KIND-TYPE 52 lshift or 32 rshift
   swap XREF-FLAGS-SLOT cells 4 + XREF-REC+ XREF-PATCH32 ;

public

: FINALIZE-NAMESPACE ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u FIND-NS {: rec:ptr count:n :}
   count 1 <> IF s" xref: namespace identity is not unique" 76 die THEN
   rec KIND@ NAMESPACE:KIND-PACKAGE <> IF
      s" xref: namespace is not a package" 76 die
   THEN
   rec XREF-START {: pub:n :}
   rec XREF-LEN {: pri:n :}
   pub WID-VALID? pri WID-VALID? and pub pri <> and 0= IF
      s" xref: invalid package roles" 76 die
   THEN
   rec SET-TYPE
   pub ;

;package

: XREF-RETIRE ( ptr a -- )
   dup XREF-WORDLIST-SLOT cells XREF-REC+
   dup XREF-RETIRED-WL swap XREF-PATCH32
   $4 XREF-REC+ -1 swap XREF-PATCH32
   drop ;

: XREF-RETIRE-INDEX ( n -- )
   XREF-REC XREF-RETIRE ;

: UNDEFINE-NAME ( ptr u8 n -- )
   {: a:ptr u:n :}
   a u get-current XREF:RESOLVE XREF-REQUIRE-UNDEFINE {: idx:n :}
   a u CHECKER-UNDEFINE
   idx XREF-RETIRE-INDEX ;

: UNDEFINE-IF-DEFINED ( ptr u8 n -- )
   {: a:ptr u:n :}
   a u 0 XREF:RESOLVE XREF:GUARD
   dup XREF:NONE = IF 2drop EXIT THEN
   drop {: idx:n :}
   a u CHECKER-UNDEFINE
   idx XREF-RETIRE-INDEX ;

\ Sealed-dictionary truncation guard (TFAM 2b-iii). Once the friend latch is
\ sealed (SEAL-FRIEND, end of cold prefix), a dictionary FORGET/HIDE that lowers
\ ndict below the seal-time watermark would retire engine definitions and (FORGET)
\ rewind CP into engine code. Reject it fail-closed with HB-ERROR:SEAL-VIOLATION. The
\ latch and watermark live in the sealed friend band; friend/cold-load (latch 0)
\ and post-seal user marks (index >= watermark) pass unchanged.
TRUSTED: SEAL-LATCH@ ( -- n ) data-base FRIEND-LATCH-CELL + @ ;
TRUSTED: SEAL-NDICT@ ( -- n ) data-base SEAL-NDICT-CELL + @ ;

: SEAL-DICT-GUARD ( n -- n )
   SEAL-LATCH@ 0= if exit then
   dup SEAL-NDICT@ < if
      s" seal: cannot FORGET/HIDE sealed engine definitions" HB-ERROR:SEAL-VIOLATION die
   then ;

: HIDE-DEFS-FROM ( ptr u8 n -- )
   {: a:ptr u:n :}
   a u 0 XREF:RESOLVE XREF-REQUIRE-INDEX SEAL-DICT-GUARD {: idx:n :}
   a u CHECKER-USIGS-TRUNCATE-FROM-RAW
   idx ndict! ;

variable XREF-FORGET-CP

: FORGET-DEFS-FROM ( ptr u8 n -- )
   {: a:ptr u:n :}
   a u 0 XREF:RESOLVE XREF-REQUIRE-INDEX SEAL-DICT-GUARD {: idx:n :}
   idx XREF-REC XREF-START XREF-FORGET-CP !
   a u CHECKER-USIGS-TRUNCATE-FROM-RAW
   idx ndict!
   XREF-FORGET-CP @ cp! ;

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
   parse-name 0 XREF:RESOLVE XREF:GUARD
   dup XREF:NONE = IF 2drop XREF-NULL XREF. EXIT THEN
   drop XREF-REC XREF. ;

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

\ TFAM 2b-iii: freeze the dictionary-truncation watermark (baseline capture).
\ xref.f is the last BASE prefix file, but src/os/script-argv.f still loads
\ after it, so the cold-prefix assembler appends a second SEAL-CAPTURE token at
\ the true engine-prefix end (habu2.f EMIT-SEAL-CAPTURE-TOKEN) - re-running the
\ capture is monotonic and only ever raises the watermark. This baseline keeps
\ contexts that load the base files without the cold-prefix assembler sealed up
\ to here. The FORGET/HIDE guards above reject truncation below the watermark.
SEAL-CAPTURE
