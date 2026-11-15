\ xref.f - live dictionary inspection words for the REPL and debug path, read
\ through dbase@/ndict@; load after boot-cells.f, wl.f and layout.f. It requires
\ none: the arm64 cold prefix loads it before it records the files it loaded,
\ so a require would load one twice. A
\ cold-prefix row (boot-files.f BOOT-FILES) loaded after check-hook.f with no
\ `0 set-check` span, so the engine build checks it. tools/check.f refuses it
\ alone: it defines `undefine` (E-RESERVED-DEFINITION) and every engine already
\ holds its words (E-DUPLICATE-DEFINITION).

\ The cold prefix loads CODE-SPAN and DOES-CLAUSE before XREF.

0 constant XREF-START-SLOT
1 constant XREF-LEN-SLOT
2 constant XREF-FLAGS-SLOT
3 constant XREF-NAME-SLOT
DICT-WORDLIST-SLOT constant XREF-WORDLIST-SLOT   \ src/core/util.f
\ Two of the non-wordlist values a record's wordlist cell can carry: the live
\ package marker BOOT:NAMESPACE-WL answers and RETIRED, src/habu/wl.f's
\ WL:RETIRED. The engine's hash index is keyed on the same cell, and
\ XREF:RETIRE below and the checker overlay's record-wid! are the two writers
\ that change it after a record is already in that index - see the
\ WL:RETIRED comment there for what the lookup does about it.
WL:RETIRED constant XREF-RETIRED-WL

32 constant XREF-SP

: XREF-A>U8 ( ptr n -- ptr u8 ) BYTE-VIEW ;

: XREF-REC+ ( ptr n n -- ptr n )
   + ;

: XREF-TRUE ( -- bool )
   0 0= ;

: XREF-FALSE ( -- bool )
   XREF-TRUE 0= ;

: XREF-NULL ( -- ptr n )
   NULL-PTR ;

: XREF-REC-ADDR ( n -- n )
   DREC * dbase@ + ;

\ Every reader in this file reads a record's cells through this word, and so
\ through src/core/checker.f DICT-CELL@, which rejects XREF-NULL.
: XREF-CELL@ ( ptr n n -- n )
   DICT-CELL@ ;

package XREF
private
\ A dictionary record's address and a record cell's name address are integers.
CAST: N>REC ( n -- ptr n )
CAST: N>U8 ( n -- ptr u8 )
public
: REC ( n -- ptr n ) XREF-REC-ADDR N>REC ;
: PTR@ ( ptr n n -- ptr u8 ) XREF-CELL@ N>U8 ;
;package

: XREF-REC ( n -- ptr n ) XREF:REC ;

\ The dictionary index of a record address an engine cell holds as an integer.
\ n must be a live record's address; 0 and unaligned addresses are not
\ refused here (REC-LIVE? tests an address).
: XREF-ADDR>INDEX ( n -- n ) dbase@ - DREC / ;

\ The record at such an address, under the same precondition.
: XREF-ADDR>REC ( n -- ptr n ) XREF-ADDR>INDEX XREF-REC ;

: LATEST ( -- ptr n )
   ndict@ dup 0 <= if drop XREF-NULL exit then
   1- XREF-REC ;

: XREF-FOUND? ( ptr n -- bool )
   XREF-NULL <> ;

: XREF-PTR@ ( ptr n n -- ptr u8 ) XREF:PTR@ ;

: XREF-START ( ptr n -- n )
   XREF-START-SLOT XREF-CELL@ ;

: XREF-RAW-LEN ( ptr n -- n )
   XREF-LEN-SLOT XREF-CELL@ ;

: XREF-FLAGS ( ptr n -- n )
   XREF-FLAGS-SLOT XREF-CELL@ ;

: XREF-WORDLIST ( ptr n -- n )
   XREF-WORDLIST-SLOT XREF-CELL@ ;

\ A namespace row has no code, so it reuses a word's two code-span cells for its
\ two wordlist ids: the public wid where a word keeps its start, the private wid
\ where a word keeps its length. A private wid of 0 means the package has opened
\ no private section.
\ Reading them as XREF-START / XREF-RAW-LEN says the opposite of what is meant.
: XREF-PKG-PUBLIC ( ptr n -- n )
   XREF-START-SLOT XREF-CELL@ ;

: XREF-PKG-PRIVATE ( ptr n -- n )
   XREF-LEN-SLOT XREF-CELL@ ;

\ The private wid of the package the string names, 0 for no such package or
\ no private section.
: XREF-PRIVATE-WL ( ptr u8 n -- n )
   BOOT:NAMESPACE-WL CHECKER-RESOLVE:WL-PROBE {: rec:ptr :}
   rec NULL-PTR = if 0 exit then
   rec XREF-PKG-PRIVATE ;

package XREF
public
\ Every marker is negative. RETIRED marks a retired word, whose row keeps code;
\ RETIRED-NAMESPACE a retired package row; any other negative value is a live
\ package row's marker.
\ Whether a wordlist cell marks a package row, live or retired: its start and
\ length cells hold wids, which no code span reader may take for a span.
: PACKAGE-WL? ( n -- bool ) {: wid:n :}
   wid 0 <  wid XREF-RETIRED-WL <>  and ;

\ Whether a wordlist cell marks a retired row, a word's or a package's.
: RETIRED-WL? ( n -- bool ) {: wid:n :}
   wid XREF-RETIRED-WL =  wid WL:RETIRED-NAMESPACE =  or ;
;package

\ Whether `wid` is protected, as habu1.f EMIT-PROTWID reads it: the OWNER-API wordlists by
\ rule, any other wid below PROT-WID-MAX by its bit at PROT-BITS-OFF.
: XREF-WID-PROTECTED? ( n -- bool ) {: wid:n :}
   wid OWNER-API-PUB-WID = wid OWNER-API-PRI-WID = or if XREF-TRUE exit then
   wid 0 < wid PROT-WID-MAX < 0= or if XREF-FALSE exit then
   wid 6 rshift 8 * data-base PROT-BITS-OFF + + @
   wid 63 and rshift 1 and 0= 0= ;

\ Keep the historical body length distinct from the exact code span. A package
\ row's length cell, live or retired, holds a wordlist id, so it is answered raw
\ rather than sent through CODE-SPAN; a package reader names that cell
\ XREF-PKG-PRIVATE. A raw length is only for record serialization.
: XREF-LEN ( ptr n -- n ) {: rec:ptr :}
   rec XREF-WORDLIST XREF:PACKAGE-WL? if rec XREF-RAW-LEN exit then
   rec XREF-RAW-LEN dup CODE-SPAN:CHECK CODE-SPAN:BODY ;

: XREF-CODE-BYTES ( ptr n -- n ) {: rec:ptr :}
   rec XREF-WORDLIST XREF:PACKAGE-WL? if 0 exit then
   rec XREF-RAW-LEN CODE-SPAN:BYTES ;

: XREF-RETIRED? ( ptr n -- bool )
   XREF-WORDLIST XREF:RETIRED-WL? ;

: XREF-NAME-LEN ( ptr n -- n )
   XREF-FLAGS DNAME-LEN-MASK and ;

: XREF-EXT? ( ptr n -- bool )
   XREF-FLAGS DNAME-EXT and 0= 0= ;

: XREF-INLINE-NAME ( ptr n -- ptr u8 )
   $18 XREF-REC+ XREF-A>U8 ;

: XREF-NAME-A ( ptr n -- ptr u8 )
   dup XREF-EXT? if XREF-NAME-SLOT XREF-PTR@ exit then
   XREF-INLINE-NAME ;

: XREF-NAME$ ( ptr n -- ptr u8 n )
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

: XREF-MATCH? ( ptr n ptr u8 n -- bool )
   XREF-U ! XREF-A!
   XREF-NAME$ XREF-A@ XREF-U @ XREF-STR=CI ;

package PKG-AUTH
private

: NAME-OUT ( ptr n -- ptr u8 n bool ) {: rec:ptr :}
   rec XREF-NAME$ dup 0= if
      2drop s" " XREF-FALSE exit
   then
   XREF-TRUE ;

\ A package record is live only when its address is an exact DREC slot below
\ ndict@. This rejects stale snapshots, unaligned pointers, and foreign data
\ before any record field is read.
: REC-LIVE? ( n -- bool ) {: addr:n :}
   addr dbase@ < if XREF-FALSE exit then
   addr dbase@ -
   dup DREC mod 0 <> if drop XREF-FALSE exit then
   DREC / ndict@ < ;

\ Return the one authenticated open package: the empty name at top level, the
\ exact all-zero tuple. A package must name a live namespace record whose two
\ WIDs equal the protected engine cells. Which of them current is, if either,
\ is the checker's question (src/core/checker.f CHECKER-RESOLVE:AUTHORITY).
: LIVE-PKG ( n n n -- ptr u8 n bool )
   {: recn:n pub:n pri:n :}
   recn 0= if
      s" " pub 0= pri 0= and exit
   then
   pub 0= pri 0= or pub pri = or if
      s" " XREF-FALSE exit
   then
   recn REC-LIVE? 0= if
      s" " XREF-FALSE exit
   then
   recn XREF-ADDR>REC
   dup XREF-WORDLIST BOOT:NAMESPACE-WL <> if
      drop s" " XREF-FALSE exit
   then
   dup XREF-PKG-PUBLIC pub <> if
      drop s" " XREF-FALSE exit
   then
   dup XREF-PKG-PRIVATE pri <> if
      drop s" " XREF-FALSE exit
   then
   NAME-OUT ;

: LIVE ( -- ptr u8 n bool )
   data-base PKG-REC-CELL + @
   data-base PKG-PUB-CELL + @
   data-base PKG-PRI-CELL + @
   LIVE-PKG ;

\ The package whose public wordlist wid is: the live namespace row that
\ publishes it.
: NS-PUB? ( ptr n n -- bool ) {: rec:ptr wid:n :}
   rec XREF-WORDLIST BOOT:NAMESPACE-WL =  rec XREF-PKG-PUBLIC wid =  and ;

: WID-PKG ( n -- ptr u8 n bool ) {: wid:n :}
   ndict@ 1-
   begin dup 0 >= while
      dup XREF-REC wid NS-PUB? if XREF-REC XREF-NAME$ XREF-TRUE exit then
      1-
   repeat drop
   s" " XREF-FALSE ;

\ The package the engine's using slot n imports (layout.f USE-WIDS-OFF).
: USE-SLOT ( n -- ptr u8 n bool )
   cells USE-WIDS-OFF + data-base + @ WID-PKG ;

: INSTALL ( -- )
   [: LIVE ;] is PKG-LIVE-XT
   [: USE-SLOT ;] is USE-SLOT-XT ;
INSTALL

;package

: XREF-FIND-WL ( ptr u8 n n -- ptr n ) {: a:ptr u:n wid:n :}
   \ A retired row stays indexed under the wordlist it left, so WL:RETIRED takes
   \ the descending scan.
   wid 0 >= wid BOOT:NAMESPACE-WL = or if
      a u wid CHECKER-RESOLVE:WL-PROBE exit
   then
   u XREF-FU !  a XREF-FN!
   ndict@ 1-
   begin dup 0 >= while
      dup XREF-REC XREF-WORDLIST wid = if
         dup XREF-REC XREF-FN@ XREF-FU @ XREF-MATCH? if XREF-REC exit then
      then
      1-
   repeat drop
   XREF-NULL ;

\ A miss in the global wordlist answers the walk of WL:PRIMITIVES, the
\ built-ins' wordlist (kernel-x64.f FIND-HELPER, does the same).
: XREF-FIND-WL-INDEX ( ptr u8 n n -- n ) {: a:ptr u:n wid:n :}
   u XREF-FU !  a XREF-FN!
   ndict@ 1-
   begin dup 0 >= while
      dup XREF-REC XREF-WORDLIST wid = if
         dup XREF-REC XREF-FN@ XREF-FU @ XREF-MATCH? if exit then
      then
      1-
   repeat drop
   wid BOOT:GLOBAL-WL = if a u WL:PRIMITIVES recurse exit then
   -1 ;

variable XREF-QI

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

: XREF-FIND-QUALIFIED ( ptr u8 n n -- ptr n )
   XREF-IDX ! XREF-SU ! XREF-SN!
   XREF-SN@ XREF-IDX @ BOOT:NAMESPACE-WL XREF-FIND-WL
   dup XREF-FOUND? 0= if drop XREF-NULL exit then
   XREF-PKG-PUBLIC {: qwid:n :}
   XREF-SN@ XREF-IDX @ 1 + ZPTR+  XREF-SU @ XREF-IDX @ - 1-  qwid  XREF-FIND-WL ;

: XREF-FIND-QUALIFIED-INDEX ( ptr u8 n n -- n )
   XREF-IDX ! XREF-SU ! XREF-SN!
   XREF-SN@ XREF-IDX @ BOOT:NAMESPACE-WL XREF-FIND-WL
   dup XREF-FOUND? 0= if drop -1 exit then
   XREF-PKG-PUBLIC {: qwid:n :}
   XREF-SN@ XREF-IDX @ 1 + ZPTR+  XREF-SU @ XREF-IDX @ - 1-  qwid  XREF-FIND-WL-INDEX ;

: XREF-FIND ( ptr u8 n -- ptr n )
   XREF-QUAL-INDEX
   dup -2 = if drop XREF-NULL exit then
   dup 0 >= if XREF-SN@ XREF-SU @ rot XREF-FIND-QUALIFIED exit then
   drop XREF-SN@ XREF-SU @ BOOT:GLOBAL-WL
   XREF-FIND-WL ;
\ The operand names a word: a string literal before a call is a use of it
\ (src/core/cell-effects.f names:), here and for XREF-FIND-INDEX below.
names: XREF-FIND

: XREF-FIND-INDEX ( ptr u8 n -- n )
   XREF-QUAL-INDEX
   dup -2 = if drop -1 exit then
   dup 0 >= if XREF-SN@ XREF-SU @ rot XREF-FIND-QUALIFIED-INDEX exit then
   drop XREF-SN@ XREF-SU @ BOOT:GLOBAL-WL
   XREF-FIND-WL-INDEX ;
names: XREF-FIND-INDEX

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

\ RETIRE and SEAL rewrite a write-protected record cell through patch32, which
\ this package's row in src/habu/prims.f (EPPRIM: XREF patch32) types for it.
package XREF
private

: RETIRE-RAW ( ptr n -- ) {: rec:ptr :}
   rec XREF-WORDLIST-SLOT cells + {: wl:ptr :}
   XREF-RETIRED-WL wl patch32
   -1 wl 4 + patch32 ;

private

CAST: GRID-RETIRE-XT ( n -- [ ptr n -- ] )

: GRID-RETIRE-OWNER ( ptr n ptr u8 -- )
   {: rec:ptr owner:ptr :}
   owner NULL-PTR = IF EXIT THEN
   owner CHECKER-OWNER-ABI:GRID-RETIRE-OFF + CELL-VIEW @ {: action:n :}
   action 0 <> IF rec action GRID-RETIRE-XT execute THEN ;

: GRID-RETIRE ( ptr n -- ) {: rec:ptr :}
   NCOMP-DISPATCH:TARGET-DECL-A @
      {: target:ptr :}
   NCOMP-DISPATCH:DECL-A @
      {: source:ptr :}
   rec target GRID-RETIRE-OWNER
   source target <> IF rec source GRID-RETIRE-OWNER THEN ;

public

\ A build holds the retirement and the record count then (CHECK:HOLD-RETIRED)
\ before the grid action drops the record's checker link, so a link replays it
\ before the first record the compile made after it (the linker's LINK-OBJECT).
: RETIRE ( ptr n -- ) {: rec:ptr :}
   rec CHECKER-RESOLVE:REC>INDEX  rec XREF-WORDLIST  CHECK:HOLD-RETIRED
   rec GRID-RETIRE
   rec RETIRE-RAW ;

\ DNAME-SEALED is bit 13 of the flags cell's upper half, rewritten whole with
\ its other bits kept.
: SEAL ( ptr n -- ) {: rec:ptr :}
   rec XREF-FLAGS DNAME-SEALED or 32 rshift
   rec XREF-FLAGS-SLOT cells + 4 + patch32 ;

: SEALED? ( ptr n -- bool ) XREF-FLAGS DNAME-SEALED and 0= 0= ;

private
\ Whether rec is a live namespace row whose public or private wid is wid.
: NS-WID? ( ptr n n -- bool ) {: rec:ptr wid:n :}
   rec XREF-WORDLIST BOOT:NAMESPACE-WL =
   rec XREF-PKG-PUBLIC wid =  rec XREF-PKG-PRIVATE wid =  or  and ;

\ Whether rec is named and enters at xt.
: NAMED-AT? ( ptr n n -- bool ) {: rec:ptr xt:n :}
   rec XREF-START xt =  rec XREF-NAME-LEN 0 >  and ;

public
\ The oldest named record entering at xt, -1 for none: an alias shares its
\ source's entry, and a stripped private original has no name.
: ENTRY-REC ( n -- n )
   {: xt:n :}
   ndict@ 0 ?do
      i XREF-REC xt NAMED-AT? if i unloop exit then
   loop
   -1 ;

\ The live namespace row whose public or private wordlist is wid. Package
\ wids count up from FIRST-DYNAMIC-WID, so a lower wid answers at once.
: NS-ROW ( n -- ptr n bool ) {: wid:n :}
   wid FIRST-DYNAMIC-WID < if XREF-NULL XREF-FALSE exit then
   ndict@ 1-
   begin dup 0 >= while
      dup XREF-REC wid NS-WID? if XREF-REC XREF-TRUE exit then
      1-
   repeat drop
   XREF-NULL XREF-FALSE ;

\ Whether wid is the public or private wordlist of a live package whose record
\ is SEALED?.
: WID-SEALED? ( n -- bool )
   NS-ROW if SEALED? else drop XREF-FALSE then ;

;package

\ A rejection's name between backticks.
: XREF-EMIT-TICKS ( ptr u8 n -- ) {: a:ptr u:n :}
   96 REJECTION:EMIT1  a u REJECTION:EMIT-STR  96 REJECTION:EMIT1 ;

\ The scope a lookup of the name a u searched: a qualified name its package's
\ public words, a bare one wid.
: XREF-EMIT-SCOPE ( ptr u8 n n -- ) {: a:ptr u:n wid:n :}
   a u XREF-QUAL-INDEX {: q:n :}
   q -2 = if s" none: the name holds more than one `:`" REJECTION:EMIT-STR exit then
   q 0 >= if
      s" the public words of package " REJECTION:EMIT-STR  a q XREF-EMIT-TICKS exit
   then
   wid BOOT:GLOBAL-WL = if s" the global words" REJECTION:EMIT-STR exit then
   wid XREF:NS-ROW 0= if drop s" the current wordlist" REJECTION:EMIT-STR exit then
   {: row:ptr :}
   row XREF-PKG-PUBLIC wid = if s" the public words of package "
   else s" the private words of package " then REJECTION:EMIT-STR
   row XREF-NAME$ XREF-EMIT-TICKS ;

\ A call of the word w wu, which takes a name: `s" MY-WORD" W`.
: XREF-EMIT-CALL ( ptr u8 n -- ) {: w:ptr wu:n :}
   96 REJECTION:EMIT1  115 REJECTION:EMIT1  34 REJECTION:EMIT1
   s"  MY-WORD" REJECTION:EMIT-STR  34 REJECTION:EMIT1  32 REJECTION:EMIT1
   w wu REJECTION:EMIT-STR  96 REJECTION:EMIT1 ;

package GENERATED-DECL-NAME-PREFLIGHT

private

\ a u is qualified: DICTIONARY-RECORDS asks NAMESPACED? first.
: NAMESPACE-EXISTS? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u XREF-QUAL-INDEX {: split:n :}
   XREF-SN@ split BOOT:NAMESPACE-WL XREF-FIND-WL XREF-FOUND? ;

\ A generated declaration is qualified exactly when it reserves a constructor
\ namespace; a private family's bare words (TF-CTOR-PRIV$) allocate no wordlist.
: NAMESPACED? ( ptr u8 n -- bool )
   XREF-QUAL-INDEX 0 >= ;

\ A generated name in a sealed package's namespace (its row SEALED?) is the
\ declaration's error, whether or not the package holds it. A bare name lands
\ in the declaring package's private wordlist, and a sealed package cannot be
\ opened, so no bare name reaches one.
: CHECK ( ptr u8 n -- ) {: a:ptr u:n :}
   a u XREF-QUAL-INDEX {: q:n :}
   q 0 < IF EXIT THEN
   a q BOOT:NAMESPACE-WL CHECKER-RESOLVE:WL-PROBE {: ns:ptr :}
   ns XREF-FOUND? 0= IF EXIT THEN
   ns XREF:SEALED? 0= IF EXIT THEN
   a u s" would change a sealed package"
      s" declare it in a package of your own: a sealed package takes no new words"
      TYPE-DECL:E-TDECL-NAME TYPE-DECL:TDECL-THROW-AT ;

public

\ words, the count a declaration generates, is at least 1.
: DICTIONARY-RECORDS ( ptr u8 n n -- n ) {: a:ptr u:n words:n :}
   a u NAMESPACED? 0= IF words EXIT THEN
   a u NAMESPACE-EXISTS? IF words EXIT THEN
   words 1 + ;

private

: INSTALL ( -- )
   [: CHECK ;] is TYPE-DECL:TDECL-NAME-PREFLIGHT-XT ;

INSTALL
seal-package

;package

: XREF-RETIRE-WL ( ptr u8 n n -- ) {: a:ptr u:n wid:n :}
   u XREF-FU !  a XREF-FN!
   ndict@ 1-
   begin dup 0 >= while
      dup XREF-REC XREF-WORDLIST wid = if
         dup XREF-REC XREF-FN@ XREF-FU @ XREF-MATCH? if
            dup XREF-REC XREF:RETIRE
         then
      then
      1-
   repeat drop ;

\ A does> definer's clause is a record of its own (habu2.f DOES-REC): published
\ one slot above the definer, in its wordlist, under its name with `;does`
\ added, and starting inside its body. Record k+1 is k's clause when it is all
\ four; the body test is what tells the clause from a word someone named X;does.
: XREF-DOES-COMPANION? ( n -- bool ) {: k:n :}
   k 1+ ndict@ >= if XREF-FALSE exit then
   k XREF-REC {: parent:ptr :}
   k 1+ XREF-REC {: clause:ptr :}
   parent XREF-WORDLIST clause XREF-WORDLIST <> if XREF-FALSE exit then
   parent XREF-NAME$ {: name:ptr len:n :}
   clause XREF-NAME$ {: derived:ptr dlen:n :}
   DOES-CLAUSE:SUFFIX$ {: suffix:ptr sufu:n :}
   dlen len sufu + <> if XREF-FALSE exit then
   derived len name len XREF-STR=CI 0= if XREF-FALSE exit then
   derived len + sufu suffix sufu XREF-STR=CI 0= if XREF-FALSE exit then
   parent XREF-START {: first:n :}
   clause XREF-START {: entry:n :}
   entry first <= if XREF-FALSE exit then
   entry clause XREF-CODE-BYTES +  first parent XREF-CODE-BYTES +  <= ;

\ The checker overlay asks this same test of a live definer a replayed
\ `undefine` retires (src/core/checker.f CHECKER-OVERLAY CLAUSE-OF); a replayed
\ definer's clause, codeless, is paired where the overlay publishes it.
: DOES-COMPANION-INSTALL ( -- ) [: XREF-DOES-COMPANION? ;] is DOES-COMPANION?-XT ;
DOES-COMPANION-INSTALL

\ A qualified token names PACKAGE:TAIL, but its dictionary record stores TAIL
\ in the package wordlist. Retire from that resolved record identity; the
\ original spelling remains the checker-side symbol identity below.
\ A definer's clause goes with it, as a record rather than by name. The AOT seed
\ finds the clause under the definer's derived name, a redefinition publishes a
\ new clause under that same name, and a wordlist holding two live rows for one
\ name answers the older (habu1.f WLFIND), so a clause left live would go on
\ answering for the new definer's words. Retiring moves no code: a word the old
\ definer made still branches to the old clause.
: XREF-RETIRE-INDEX ( n -- ) {: k:n :}
   k XREF-DOES-COMPANION? if k 1+ XREF-REC XREF:RETIRE then
   k XREF-REC dup XREF-NAME$ rot XREF-WORDLIST XREF-RETIRE-WL ;

\ While a checker overlay is open (src/habu/layout.f REPLAY-SCOPE) the engine
\ undefines nothing live, as it defines nothing (src/habu/habu1.f
\ OVERLAY-EMIT:GUARD,): replay-close gives back only the records the overlay
\ retired and logged, so a record retired here would stay retired after the
\ pair and the live word would be gone. The refusal names the word before
\ anything changes and throws ENGINE-ERROR:OVERLAY-OPEN, which ends a top-level
\ load with that code. Both loops run these words; a replayed `undefine` never
\ reaches them, since the checker's source pass retires through the overlay
\ (src/core/checker.f CHECKER-OVERLAY:RETIRE).
: XREF-UNDEFINE-GUARD ( ptr u8 n -- ) {: a:ptr u:n :}
   data-base REPLAY-SCOPE:LATCH + @ 0= if exit then
   2 s" E-OVERLAY-OPEN habu: undefine while a checker replay is open: " write drop
   2 a u write drop
   2 S\" \n" write drop
   ENGINE-ERROR:OVERLAY-OPEN throw ;

: XREF-REQUIRE-UNDEFINE ( n ptr u8 n -- n ) {: idx:n a:ptr u:n :}
   idx 0 < if
      s" E-UNDEFINED" REJECTION:OPEN
      a u XREF-EMIT-TICKS
      s"  is not defined here, so `undefine` cannot retire it" REJECTION:EMIT-STR
      a u REJECTION:AT  s" not found" REJECTION:EMIT-STR
      s" scope" REJECTION:NOTE  a u get-current XREF-EMIT-SCOPE
      REJECTION:HELP
      s" name a word this scope defines, or drop the `undefine` line"
      REJECTION:EMIT-STR
      CHECKER-REJECT-RC REJECTION:STOP
   then
   idx ;

: UNDEFINE-NAME ( ptr u8 n -- )
   XREF-SU ! XREF-SN!
   XREF-SN@ XREF-SU @ XREF-UNDEFINE-GUARD
   XREF-SN@ XREF-SU @ XREF-FIND-TARGET-INDEX
   XREF-SN@ XREF-SU @ XREF-REQUIRE-UNDEFINE XREF-IDX !
   XREF-SN@ XREF-SU @ CHECKER-UNDEFINE   \ guarded checker mutation completes before retirement
   XREF-IDX @ XREF-RETIRE-INDEX ;

: UNDEFINE-FOUND ( ptr u8 n n -- )
   XREF-IDX ! XREF-SU ! XREF-SN!
   XREF-SN@ XREF-SU @ XREF-UNDEFINE-GUARD
   XREF-SN@ XREF-SU @ CHECKER-UNDEFINE
   XREF-IDX @ XREF-RETIRE-INDEX ;

: UNDEFINE-IF-DEFINED ( ptr u8 n -- )
   XREF-SU ! XREF-SN!
   XREF-SN@ XREF-SU @ XREF-FIND-INDEX
   dup 0 >= if XREF-SN@ XREF-SU @ rot UNDEFINE-FOUND else drop then ;

\ The seal watermark of the dictionary being loaded: while a build writes an
\ image, the image's, where a header store goes (habu2.f ROUTE-HEADER), 0 until
\ the product seals; else hb's.
: SEAL-NDICT@ ( -- n )
   data-base BOOT:IMAGE-BASE-CELL + @ {: image:n :}
   image 0 <> if NULL-PTR BYTE-VIEW image + SEAL-NDICT-CELL + CELL-VIEW @ exit then
   data-base SEAL-NDICT-CELL + @ ;

\ idx is the record FORGET-DEFS-FROM or HIDE-DEFS-FROM, the word w wu, found
\ for the name a u, -1 for none; a name that is not defined is rejected.
: XREF-REQUIRE-DEFINED ( n ptr u8 n ptr u8 n -- n )
   {: idx:n a:ptr u:n w:ptr wu:n :}
   idx 0 < if
      s" E-UNDEFINED" REJECTION:OPEN
      a u XREF-EMIT-TICKS  s"  is not defined, so " REJECTION:EMIT-STR
      w wu XREF-EMIT-TICKS  s"  has no word to start from" REJECTION:EMIT-STR
      s" scope" REJECTION:NOTE  a u BOOT:GLOBAL-WL XREF-EMIT-SCOPE
      REJECTION:HELP  s" pass a word this program defined, as in " REJECTION:EMIT-STR
      w wu XREF-EMIT-CALL
      CHECKER-REJECT-RC REJECTION:STOP
   then
   idx ;

\ Sealed-dictionary truncation guard: once the namespace watermark is set, a
\ FORGET/HIDE lowering ndict below it would retire engine definitions and
\ (FORGET) rewind CP into engine code, so the word w wu given the engine word
\ a u at index idx is rejected. A zero watermark is the open build namespace;
\ post-seal marks at or above the watermark pass. The floor is hb's own, in a
\ build too.
: SEAL-DICT-GUARD ( n ptr u8 n ptr u8 n -- n )
   {: idx:n a:ptr u:n w:ptr wu:n :}
   data-base SEAL-NDICT-CELL + @ {: floor:n :}
   floor 0= 0=  idx floor <  and if
      s" E-SEAL-VIOLATION" REJECTION:OPEN
      a u XREF-EMIT-TICKS  s"  is an engine definition, which " REJECTION:EMIT-STR
      w wu XREF-EMIT-TICKS  s"  cannot remove" REJECTION:EMIT-STR
      s" record" REJECTION:NOTE  idx REJECTION:EMIT-U
      s" sealed" REJECTION:NOTE  s" records below " REJECTION:EMIT-STR
      floor REJECTION:EMIT-U  s"  belong to the engine" REJECTION:EMIT-STR
      REJECTION:HELP  s" pass a word this program defined, as in " REJECTION:EMIT-STR
      w wu XREF-EMIT-CALL
      CHECKER-REJECT-RC REJECTION:STOP
   then
   idx ;

\ A snapshot builder truncates a refresh tail with HIDE-DEFS-FROM and retires
\ one word with `undefine NAME`; it never replays baked sources to hide one.
: HIDE-DEFS-FROM ( ptr u8 n -- )
   XREF-SU ! XREF-SN!
   XREF-SN@ XREF-SU @ XREF-FIND-INDEX
   XREF-SN@ XREF-SU @ s" HIDE-DEFS-FROM" XREF-REQUIRE-DEFINED
   XREF-SN@ XREF-SU @ s" HIDE-DEFS-FROM" SEAL-DICT-GUARD {: idx:n :}
   XREF-SN@ XREF-SU @ CHECKER-USIGS-TRUNCATE-FROM-RAW
   idx ndict! ;

\ FORGET retires a suffix of dictionary records and gives back the corresponding
\ code. Live XREF records decide the safe floor; relocation metadata over the
\ released span is cleared before the code pointer moves.
\ A selected record is borrowed from the resolver at SELECT. The slot alone is
\ never identity: lowering and re-exposing the count issues another occurrence.
\ These references stay in this process and carry no checker effect or code lease.
\ The resolver's primitive rows belong to DEF-OCC; the checker also selects
\ occurrences when it binds an exported contract (src/habu/prims.f).
package DEF-OCC
private
: NONCALLABLE ( ptr n -- ) {: rec:ptr :}
   s" E-NONCALLABLE" REJECTION:OPEN
   s" `DEF-OCC:CALLABLE` was given the package `" REJECTION:EMIT-STR
   rec XREF-NAME$ REJECTION:EMIT-STR
   s" `, which has no code" REJECTION:EMIT-STR
   REJECTION:HELP
   s" pass the slot and occurrence `DEF-OCC:SELECT` answered for a word"
   REJECTION:EMIT-STR
   CHECKER-REJECT-RC REJECTION:STOP ;

public
: SELECT ( ptr n -- n n ) def-occ-select ;
: RESOLVE ( n n -- ptr n ) def-occ-resolve ;

: CALLABLE ( n n -- n )
   RESOLVE dup XREF-WORDLIST BOOT:NAMESPACE-WL = if NONCALLABLE then
   XREF-START ;

;package

package CODE-RECLAIM

private

\ The raw engine primitive edits relocation metadata. Its CODE-RECLAIM row
\ (src/habu/prims.f) admits this checked word, which TRUNCATE calls with its
\ proved [start,start+len) span; no other checked caller reaches it.
: CLEAR-MAPS ( n n -- )
   reloc-maps-clear ;

\ A FLOOR IS A STATEMENT ABOUT CODE ORDER: the bytes from it upwards are handed
\ back and rewritten, so no routine a surviving record points at may begin
\ there. Records are in DEFINITION order, and an `EXPORT` alias can publish a
\ late record for an earlier routine, so a floor read off one record's start
\ can be too low and give back a surviving routine.
\
\ THE FLOOR IS A RETIRED RECORD'S OWN START, CHOSEN BY ADDRESS: every
\ surviving routine lies below it. A start is the one address nameable without
\ knowing the length of the routine below it (the recorded length excludes a
\ trailing return). The cost, when the lowest retirement is not the first
\ routine above the survivors, is a routine's worth of bytes left claimed.

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

\ A record's lowest arena byte: its out-of-line name copy (layout.f DNAME-EXT),
\ else its routine (REC-CODE), for an alias its source's, which it never wrote.
: REC-LOW ( n -- n ) {: k:n :}
   k REC-CODE {: s:n :}
   k XREF-REC {: rec:ptr :}
   rec XREF-EXT? 0= if s exit then
   rec XREF-NAME-SLOT XREF-CELL@ {: a:n :}
   a ARENA-LO <  a cp@ < 0=  or if s exit then
   a ;

\ The highest address any routine below the cut begins at. A record that is
\ already RETIRED still counts: `undefine` retires the name and leaves the
\ routine where it is, and a caller compiled before that retirement still
\ branches into it.
\ The scans below carry their running values on the stack, never in a cell: a
\ routine's start is an arena address, which moves with every launch, and a
\ cell holding one would carry it into a captured image.
: LIVE-SCAN ( n -- n ) {: cut:n :}
   0
   cut 0 ?do
      i REC-CODE max
   loop ;

: MIN-ABOVE-LIVE ( n n n -- n ) {: live:n floor:n s:n :}
   s live <= if floor exit then
   s floor min ;

public

\ A floor above the free code slot: there is nothing above the slot to reclaim,
\ and the request would move the pointer the other way. Refused by name; public
\ because a caller can reach it.
7178 constant E-FLOOR

\ A floor with a surviving routine at or above it: code still somebody's would
\ be rewritten. Refused before the pointer moves.
7179 constant E-LIVE

\ The floor of a sweep that retires every record from CUT upwards: the lowest
\ REC-LOW of a retired record above every surviving routine's start, else cp@.
: FLOOR-FROM ( n -- n ) {: cut:n :}
   cut LIVE-SCAN {: live:n :}
   cp@
   ndict@ cut ?do
      live swap i REC-LOW MIN-ABOVE-LIVE
   loop ;

\ Reclaim the code space above floor: both relocation maps are cleared over
\ [floor, old CP), then the pointer moves. The floor is held against the
\ surviving records, not taken on the caller's word: each caller computes it
\ from something else (FORGET from the records it retires,
\ src/core/generated-declaration-dictionary.f from its saved free slot), and
\ only this word can ask what the dictionary still points at. The scan runs
\ before mutation, so a refusal leaves the arena as it was.
: TRUNCATE ( n -- )
   {: floor:n :}
   floor cp@ > if E-FLOOR throw then
   floor ndict@ LIVE-SCAN <= if E-LIVE throw then
   floor cp@ over - CLEAR-MAPS
   floor cp! ;

seal-package

;package

: FORGET-DEFS-FROM ( ptr u8 n -- )
   XREF-SU ! XREF-SN!
   XREF-SN@ XREF-SU @ XREF-FIND-INDEX
   XREF-SN@ XREF-SU @ s" FORGET-DEFS-FROM" XREF-REQUIRE-DEFINED
   XREF-SN@ XREF-SU @ s" FORGET-DEFS-FROM" SEAL-DICT-GUARD {: idx:n :}
   idx CODE-RECLAIM:FLOOR-FROM {: floor:n :}
   XREF-SN@ XREF-SU @ CHECKER-USIGS-TRUNCATE-FROM-RAW
   idx ndict!
   floor CODE-RECLAIM:TRUNCATE ;

: XREF-NAME. ( ptr n -- )
   XREF-NAME$ type ;

: XREF-N. ( ptr u8 n n -- )
   XREF-NV ! XREF-FU ! XREF-FN!
   XREF-FN@ XREF-FU @ type
   XREF-SP emit
   XREF-NV @ . ;

: XREF. ( ptr n -- )
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

\ `undefine`, the source token t tu, has no name after it. It is written
\ through its help, and the caller stops it.
: UNDEFINE-NO-NAME ( ptr u8 n -- ) {: t:ptr tu:n :}
   t tu s" the name of the word to retire"
   t tu s" no name follows" REJECTION:MISSING-OPERAND
   s" write the word after it, as in `undefine NAME`" REJECTION:EMIT-STR ;

\ The `undefine` token is kept at entry: parse-name moves the token cells.
: undefine ( -- )
   data-base TKA-CELL + 0 ptr-field @  data-base TKL-CELL + @
   {: ta:ptr tu:n :}
   parse-name dup 0= if
      ta tu UNDEFINE-NO-NAME  CHECKER-REJECT-RC REJECTION:STOP
   then
   UNDEFINE-NAME ;

: WORDS ( -- )
   0 begin dup ndict@ < while
      dup XREF-REC dup XREF-RETIRED? if drop else XREF-NAME. space then
      1+
   repeat drop cr ;

\ The installed provider holds direct code references. Retire every source-level
\ rebinding seam and mutable provider cell before the engine-prefix seal.
undefine PKG-LIVE-XT
undefine USE-SLOT-XT
undefine USE-SLOT-BOOT
undefine CWIN-STATE
undefine CALL-FREEZE-XT
undefine CALL-FREEZE-INSTALL
undefine U-DRAIN-XT
undefine U-DRAIN-INSTALL
undefine TAIL-CAPTURE-XT
undefine TAIL-CAPTURE-DEFAULT
undefine TAIL-CAPTURE-INSTALL
undefine DOES-COMPANION?-XT
undefine DOES-COMPANION-INSTALL
undefine CHECKER-PKG-LIVE-DEFAULT
undefine CHECKER-PKG-BOOT-LIVE
undefine CHECKER-PKG-MIRROR
undefine CHECKER-PKG-CONTEXT
undefine CHECKER-RESOLVE:AUTHORITY
undefine CHECKER-PKG-CONTEXT-REJECT
undefine CHECKER-VERIFY-PKG-DEPTH
undefine TFAM-PKG-XT
undefine TFAM-PKG$*
package PKG-AUTH
undefine INSTALL
undefine LIVE
undefine LIVE-PKG
undefine REC-LIVE?
undefine NAME-OUT
seal-package
;package

\ The declaration owner holds CHECKER-RESYNC's entry by its xt, which keeps the
\ name through the seal where SLOT and SLOTS lose theirs: the product still
\ resolves SCOPE in the package's private wordlist. Retire it so the owner is
\ the only way in. MIRROR is public for the checker overlay's direct calls,
\ compiled above; retire it too.
package CHECKER-RESYNC
undefine SCOPE
public
undefine MIRROR
;package

\ The checker overlay's public words are the checker's own calls, compiled
\ above in src/core/checker.f. Retire their names so no source reaches the
\ engine's replay writers through them.
package CHECKER-OVERLAY
public
undefine OPEN-SCOPE
undefine CLOSE-SCOPE
undefine MARKS
undefine ROLLBACK
undefine HIDE
undefine UNHIDE
undefine SCOPE-BYTES
undefine SAVE-SCOPE
undefine RESTORE-SCOPE
undefine FILE-DONE
undefine PUBLISH
undefine CLAUSE
undefine OPEN-PACKAGE
undefine SECTION
undefine CLOSE-PACKAGE
undefine OPEN-USING
undefine CLOSE-USING
undefine RETIRE
;package
