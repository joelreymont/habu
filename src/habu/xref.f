\ xref.f - live dictionary inspection words.
\
\ Load after src/habu/layout.f. These words inspect the running image dictionary
\ through dbase@/ndict@ and are intended for the REPL/debug path.

0 constant XREF-START-SLOT
1 constant XREF-LEN-SLOT
2 constant XREF-FLAGS-SLOT
3 constant XREF-NAME-SLOT
5 constant XREF-WORDLIST-SLOT
\ The two non-wordlist values a record's wordlist cell can carry. They are
\ src/habu/layout.f's, not this file's: the engine's hash index is keyed on the
\ same cell, and XREF-RETIRE below is the one writer that changes it after a
\ record is already in that index - see the DICT-WL comment there for what the
\ lookup does about it.
DICT-WL:NAMESPACE constant XREF-NAMESPACE-WL
DICT-WL:RETIRED constant XREF-RETIRED-WL

32 constant XREF-SP

\ Xref casts expose mixed dictionary records and their inline/long name bytes.
\ Retirement: habu-builder-trust-rows-c5d41af6.
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
PTR-VARIABLE XREF-SN
variable XREF-SU
PTR-VARIABLE XREF-FN
variable XREF-FU
variable XREF-WID
variable XREF-IDX
variable XREF-NV

: XREF-A@ ( -- ptr u8 )
   XREF-A @ ;

: XREF-B@ ( -- ptr u8 )
   XREF-B @ ;

: XREF-SN@ ( -- ptr u8 )
   XREF-SN @ ;

: XREF-FN@ ( -- ptr u8 )
   XREF-FN @ ;

: XREF-A! ( ptr u8 -- )
   XREF-A ! ;

: XREF-B! ( ptr u8 -- )
   XREF-B ! ;

: XREF-SN! ( ptr u8 -- )
   XREF-SN ! ;

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

package PKG-AUTH
private

0 constant MODE-NONE
1 constant MODE-PRI
2 constant MODE-PUB

: NAME-OUT ( ptr a n -- ptr u8 n n bool ) {: rec:ptr mode:n :}
   rec XREF-NAME$ dup 0= if
      2drop s" " MODE-NONE XREF-FALSE exit
   then
   mode XREF-TRUE ;

\ A package record is live only when its address is an exact DREC slot below
\ ndict@. This rejects stale snapshots, unaligned pointers, and foreign data
\ before any record field is read.
: REC-LIVE? ( n -- bool ) {: addr:n :}
   addr dbase@ < if XREF-FALSE exit then
   addr dbase@ -
   dup DREC mod 0 <> if drop XREF-FALSE exit then
   DREC / ndict@ < ;

\ Return the one authenticated package context. The global context is the exact
\ all-zero tuple. A package context must name a live namespace record whose two
\ WIDs equal the protected engine cells; get-current selects its visibility.
: LIVE-PKG ( n n n n -- ptr u8 n n bool )
   {: recn:n pub:n pri:n cur:n :}
   recn 0= if
      pub 0= pri 0= and cur 0= and if
         s" " MODE-NONE XREF-TRUE
      else
         s" " MODE-NONE XREF-FALSE
      then
      exit
   then
   pub 0= pri 0= or pub pri = or if
      s" " MODE-NONE XREF-FALSE exit
   then
   recn REC-LIVE? 0= if
      s" " MODE-NONE XREF-FALSE exit
   then
   recn XREF-N>REC
   dup XREF-WORDLIST XREF-NAMESPACE-WL <> if
      drop s" " MODE-NONE XREF-FALSE exit
   then
   dup XREF-START pub <> if
      drop s" " MODE-NONE XREF-FALSE exit
   then
   dup XREF-LEN pri <> if
      drop s" " MODE-NONE XREF-FALSE exit
   then
   cur pub = if MODE-PUB NAME-OUT exit then
   cur pri = if MODE-PRI NAME-OUT exit then
   drop s" " MODE-NONE XREF-FALSE ;

: LIVE ( -- ptr u8 n n bool )
   data-base PKG-REC-CELL + @
   data-base PKG-PUB-CELL + @
   data-base PKG-PRI-CELL + @
   get-current
   LIVE-PKG ;

: INSTALL ( -- )
   [: LIVE ;] is PKG-LIVE-XT ;
INSTALL

;package

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

: PROT-WID-CTOR-ADD ( ptr u8 n -- ) {: a:ptr u:n :}
   a u TFAM-CTOR-WORD? 0= IF s" xref: protected-WID constructor mismatch" 76 die THEN
   a u XREF-FIND dup XREF-FOUND? 0= IF
      drop s" xref: protected-WID constructor not found" 76 die
   THEN
   XREF-WORDLIST prot-wid-add ;

using TYPE-DECL
: PROT-WID-CTOR-INSTALL ( -- ) [: PROT-WID-CTOR-ADD ;] is TYPE-DECL:TDECL-PROT-WID-XT ;
PROT-WID-CTOR-INSTALL
-1 TDECL-PROT-WID-ARMED !
;using

package GENERATED-DECL-NAME-PREFLIGHT

private

$7FFFFFFFFFFFFFFF constant COUNT-MAX

: NAMESPACE-EXISTS? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u XREF-QUAL-INDEX {: split:n :}
   split 0 < IF s" xref: generated declaration is not qualified" 76 die THEN
   XREF-SN@ split XREF-NAMESPACE-WL XREF-FIND-WL XREF-FOUND? ;

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
   a u NAMESPACE-EXISTS? IF words EXIT THEN
   words 1 + ;

: NEW-WORDLIST? ( ptr u8 n -- bool )
   NAMESPACE-EXISTS? 0= ;

private

: INSTALL ( -- )
   [: CHECK ;] is TYPE-DECL:TDECL-NAME-PREFLIGHT-XT ;

INSTALL
get-current prot-wid-add

;package

\ Explicit undefine patches raw wordlist/status cells in a live dictionary record.
\ Retirement: habu-builder-trust-rows-c5d41af6.
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

\ A qualified token names PACKAGE:TAIL, but its dictionary record stores TAIL
\ in the package wordlist. Retire from that resolved record identity; the
\ original spelling remains the checker-side symbol identity below.
: XREF-RETIRE-INDEX ( n -- )
   XREF-REC dup XREF-NAME$ rot XREF-WORDLIST XREF-RETIRE-WL ;

: UNDEFINE-NAME ( ptr u8 n -- )
   XREF-SU ! XREF-SN!
   XREF-SN@ XREF-SU @ XREF-FIND-TARGET-INDEX XREF-REQUIRE-UNDEFINE XREF-IDX !
   XREF-SN@ XREF-SU @ CHECKER-UNDEFINE   \ guarded checker mutation completes before retirement
   XREF-IDX @ XREF-RETIRE-INDEX ;

: UNDEFINE-FOUND ( ptr u8 n n -- )
   XREF-IDX ! XREF-SU ! XREF-SN!
   XREF-SN@ XREF-SU @ CHECKER-UNDEFINE
   XREF-IDX @ XREF-RETIRE-INDEX ;

: UNDEFINE-IF-DEFINED ( ptr u8 n -- )
   XREF-SU ! XREF-SN!
   XREF-SN@ XREF-SU @ XREF-FIND-INDEX
   dup 0 >= if XREF-SN@ XREF-SU @ rot UNDEFINE-FOUND else drop then ;

\ Sealed-dictionary truncation guard (TFAM 2b-iii). Once the friend latch is
\ sealed (SEAL-FRIEND, end of cold prefix), a dictionary FORGET/HIDE that lowers
\ ndict below the seal-time watermark would retire engine definitions and (FORGET)
\ rewind CP into engine code. Reject it fail-closed with ENGINE-ERROR:SEAL-VIOLATION. The
\ latch and watermark live in the sealed friend band; friend/cold-load (latch 0)
\ and post-seal user marks (index >= watermark) pass unchanged.
TRUSTED: SEAL-LATCH@ ( -- n ) data-base FRIEND-LATCH-CELL + @ ;
TRUSTED: SEAL-NDICT@ ( -- n ) data-base SEAL-NDICT-CELL + @ ;

: SEAL-DICT-GUARD ( n -- n )
   SEAL-LATCH@ 0= if exit then
   dup SEAL-NDICT@ < if
      s" seal: cannot FORGET/HIDE sealed engine definitions" ENGINE-ERROR:SEAL-VIOLATION die
   then ;

: HIDE-DEFS-FROM ( ptr u8 n -- )
   XREF-SU ! XREF-SN!
   XREF-SN@ XREF-SU @ XREF-FIND-INDEX XREF-REQUIRE-INDEX SEAL-DICT-GUARD {: idx:n :}
   XREF-SN@ XREF-SU @ CHECKER-USIGS-TRUNCATE-FROM-RAW
   idx ndict! ;

\ ---- the notice a code reclamation owes whatever is keyed to code -------------
\ WHY THERE IS AN EVENT HERE AT ALL. The engine compiles every definition into
\ one bump pointer, and a FORGET moves that pointer BACK: the bytes above it are
\ free again and the next definition the engine compiles is written over them.
\ Anything that wrote a fact down against a CODE ADDRESS - what the routine
\ there destroys, what its body is - is then describing bytes that stopped being
\ its routine's, and the address itself will never say so. The bytes may even be
\ rewritten to a different routine at exactly the same address, which is why "a
\ later publication would notice the collision" is not a defence either.
\ src/compiler/native/clobber.f and src/compiler/native/inline.f keep exactly
\ such facts, and src/compiler/native/publish.f decides where a routine lands
\ from the same pointer.
\
\ SO LOWERING THE CODE POINTER IS AN EVENT AND NOT A STORE. Every checked word
\ that reclaims code space does it through TRUNCATE below, and everything
\ holding an address-keyed fact registers once and is told the floor BEFORE the
\ space is released. That is what makes the lifetime of such a fact a
\ consequence of the lifetime of the code it describes rather than of a
\ pointer's happening to move one way.
\
\ THE WATCHERS RUN FIRST AND THE POINTER MOVES AFTER, so there is no instant at
\ which the pointer says a slot is free while a live row still claims to
\ describe what used to be there.
\
\ AND A WATCHER MUST BE TOTAL. It is told in the middle of a FORGET - the
\ dictionary records are already retired and the checker's signatures already
\ truncated - so there is nothing to roll back to and nothing it could
\ meaningfully refuse. It may only drop what it holds. That is the contract
\ src/core/declaration-transaction.f states for its own release phase, for the
\ same reason.
package CODE-RECLAIM

private

\ One slot per file that keeps an address-keyed fact. Three files do today -
\ src/compiler/native/publish.f, which remembers where the routine it published
\ last ends, and the two records a call site reads, clobber.f and inline.f - and
\ the table is fixed because this runs while the engine is compiling and has
\ nowhere to allocate from. A fourth registration would be a source change, so a
\ fifth is a defect in that change rather than a condition a program can reach:
\ it dies here instead of being refused into a caller that has no answer for it.
4 constant WATCH-MAX

create WATCH-TBL WATCH-MAX cells allot
variable WATCH-N
0 WATCH-N !

\ A watcher cell holds an execution token, so the store has to be `xt!` rather
\ than `!`: the cell address is computed at run time, and a snapshot image that
\ kept a raw token would dispatch into the writing run's JIT region. This is the
\ same declaration src/core/declaration-transaction.f's callback stores make.
TRUSTED: WATCH-AT ( n -- ptr [ n -- ] )
   cells WATCH-TBL + ;

\ ---- which bytes a reclamation is allowed to be about ------------------------
\ A FLOOR IS A STATEMENT ABOUT CODE ORDER. The bytes from it upwards are handed
\ back and rewritten, so no routine a surviving record still points at may begin
\ there. The record array, though, is in DEFINITION order, and the two orders
\ agree only while every routine is written in the order its record was made.
\ Two things in this system separate them, and both are ordinary:
\
\   - a republication (src/compiler/native/publish.f) writes a word's new
\     routine at the top of the arena and leaves its record where it was, so an
\     EARLY record can point at LATE code;
\   - an `EXPORT` alias publishes a second record for a word that already
\     exists, so a LATE record can point at EARLY code - the alias record's
\     start is the original's routine, which everything defined between them
\     sits above.
\
\ So a floor read off ONE record's start is reading the wrong order, and the
\ answer is wrong in both directions: too high and retired routines are never
\ given back, too low and a surviving word's routine is. FORGET-DEFS-FROM read
\ exactly that, and forgetting an alias handed back the code of the two live
\ words underneath it - the address-keyed records then correctly dropped their
\ rows, and calling either word ran whatever was compiled next.
\
\ THE FLOOR IS THEREFORE A ROUTINE'S OWN START, CHOSEN BY ADDRESS. Below it must
\ lie every surviving routine; at it must begin a routine the sweep retires. A
\ retired routine's start is the one address that can be named without knowing
\ how long the routine below it is - the recorded length is the span the inliner
\ copies and not the whole emission, so measuring the last survivor's END would
\ be measuring the wrong number. What that costs is a routine's worth of space
\ when the sweep's lowest retirement is not the first thing above the survivors:
\ the bytes stay claimed, which is the harmless direction.

\ The first address of the engine's code arena. Below it are the dictionary
\ records themselves and, lower still, the engine's own loaded text where a
\ primitive's record points - neither is a slot this pointer ever handed out.
: ARENA-LO ( -- n )
   dbase@ DICT-SIZE + ;

\ Where a record's routine begins, or 0 for a record that has no routine in the
\ arena: a package namespace row carries a wordlist id in that cell and a
\ primitive row carries an address in the engine's text.
: REC-CODE ( n -- n ) {: k:n :}
   k XREF-REC XREF-START {: s:n :}
   s ARENA-LO < if 0 exit then
   s cp@ < if s exit then
   0 ;

variable LIVE-HI
variable FLOOR-A

: HIGHER ( n n -- n ) {: hi:n s:n :}
   s hi > if s exit then
   hi ;

\ The highest address any routine below the cut begins at. A record that is
\ already RETIRED still counts: `undefine` retires the name and leaves the
\ routine where it is, and a caller compiled before that retirement still
\ branches into it.
: LIVE-SCAN ( n -- ) {: cut:n :}
   0 LIVE-HI !
   cut 0 ?do
      LIVE-HI @ i REC-CODE HIGHER LIVE-HI !
   loop ;

: MIN-ABOVE-LIVE ( n n -- n ) {: floor:n s:n :}
   s LIVE-HI @ <= if floor exit then
   s floor < if s exit then
   floor ;

public

\ A floor above the free code slot. There is nothing above the slot to reclaim,
\ so a caller asking for one is not truncating - it is moving the pointer the
\ other way with a notice nobody can act on, which is the one shape that would
\ make this word mean two things. Refused by name, and public because it is a
\ refusal a caller can reach.
7178 constant E-FLOOR

\ A floor with a surviving routine at or above it. The bytes from a floor
\ upwards are given back, so this is a caller asking for code that is still
\ somebody's to be rewritten. It is refused before any watcher is told and
\ before the pointer moves, so a caller that gets it has lost nothing.
7179 constant E-LIVE

\ Be told the floor of every code reclamation from here on. Registration is
\ one-way and unordered: a watcher only drops what it holds, so no watcher can
\ observe another one's work and none of them can disagree.
: WATCH ( [ n -- ] -- )
   WATCH-N @ WATCH-MAX >= if
      s" code-reclaim: more watchers than the table holds" 76 die
   then
   WATCH-N @ WATCH-AT xt!
   WATCH-N @ 1+ WATCH-N ! ;

\ The floor of a sweep that retires every record from CUT upwards: the lowest
\ address a retired routine begins at that is above every surviving routine's
\ start. When the sweep retires no routine that sits above the survivors there
\ is nothing to give back and the answer is the free slot itself.
: FLOOR-FROM ( n -- n ) {: cut:n :}
   cut LIVE-SCAN
   cp@ FLOOR-A !
   ndict@ cut ?do
      FLOOR-A @ i REC-CODE MIN-ABOVE-LIVE FLOOR-A !
   loop
   FLOOR-A @ ;

\ Reclaim the code space above this address. Every watcher is told the floor,
\ and only then is the pointer moved: a watcher drops what it holds at or above
\ the floor, so the bytes are released with nothing left claiming to describe
\ them.
\
\ AND THE FLOOR IS HELD AGAINST THE RECORDS THAT SURVIVE IT rather than taken on
\ the caller's word. Every caller computes its floor from something else - this
\ file from the records a FORGET retires, src/core/generated-declaration-
\ dictionary.f from the free slot its transaction saved - and a floor is only
\ ever correct relative to what the dictionary still points at, which is a
\ question this word can ask and they cannot answer for each other. The scan
\ runs before the watchers, so a refusal here leaves the arena exactly as it was.
: TRUNCATE ( n -- )
   {: floor:n :}
   floor cp@ > if E-FLOOR throw then
   ndict@ LIVE-SCAN
   floor LIVE-HI @ <= if E-LIVE throw then
   WATCH-N @ 0 ?do
      floor i WATCH-AT @ execute
   loop
   floor cp! ;

\ How many watchers are registered, which is what a test measures a registration
\ against.
: WATCHERS ( -- n )
   WATCH-N @ ;

private

get-current prot-wid-add

public
get-current prot-wid-add

;package

variable XREF-FORGET-CP

: FORGET-DEFS-FROM ( ptr u8 n -- )
   XREF-SU ! XREF-SN!
   XREF-SN@ XREF-SU @ XREF-FIND-INDEX XREF-REQUIRE-INDEX SEAL-DICT-GUARD {: idx:n :}
   idx CODE-RECLAIM:FLOOR-FROM XREF-FORGET-CP !
   XREF-SN@ XREF-SU @ CHECKER-USIGS-TRUNCATE-FROM-RAW
   idx ndict!
   XREF-FORGET-CP @ CODE-RECLAIM:TRUNCATE ;

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

\ The installed provider holds direct code references. Retire every source-level
\ rebinding seam and mutable provider cell before the engine-prefix seal.
undefine PKG-LIVE-XT
undefine CHECKER-PKG-LIVE-DEFAULT
undefine CHECKER-PKG-BOOT-LIVE
undefine CHECKER-PKG-MIRROR
undefine CHECKER-PKG-CONTEXT
undefine CHECKER-PKG-CONTEXT-REJECT
undefine CHECKER-VERIFY-PKG-DEPTH
undefine VPKG-NAME
undefine VPKG-U
undefine VPKG-MODE
undefine VPKG-SAVE
undefine VPKG-RESTORE
undefine TFAM-PKG-XT
undefine TFAM-PKG$*
package PKG-AUTH
undefine INSTALL
undefine LIVE
undefine LIVE-PKG
undefine REC-LIVE?
undefine NAME-OUT
undefine MODE-PUB
undefine MODE-PRI
undefine MODE-NONE
private
get-current prot-wid-add
public
get-current prot-wid-add
;package

\ TFAM 2b-iii: freeze the dictionary-truncation watermark (baseline capture).
\ xref.f is the last BASE prefix file, but src/os/script-argv.f still loads
\ after it, so the cold-prefix assembler appends a second SEAL-CAPTURE token at
\ the true engine-prefix end (habu2.f EMIT-SEAL-CAPTURE-TOKEN) - re-running the
\ capture is monotonic and only ever raises the watermark. This baseline keeps
\ contexts that load the base files without the cold-prefix assembler sealed up
\ to here. The FORGET/HIDE guards above reject truncation below the watermark.
SEAL-CAPTURE
