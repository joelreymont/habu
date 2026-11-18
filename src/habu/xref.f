split-str
\ xref.f - live dictionary inspection words for the REPL and debug path, read
\ through dbase@/ndict@; load after boot-cells.f, wl.f and layout.f. It requires
\ none: the arm64 cold prefix loads it before it records the files it loaded,
\ so a require would load one twice. A cold-prefix row (boot-files.f
\ BOOT-FILES) loaded after check-hook.f with no `0 set-check` span, so the
\ engine build checks it. tools/check.f refuses it alone: every engine already
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

: XREF-A>U8 ( ^n -- ^u8 ) BYTE-VIEW ;

: XREF-REC+ ( ^n n -- ^n )
   + ;

: XREF-TRUE ( -- bool )
   0 0= ;

: XREF-FALSE ( -- bool )
   XREF-TRUE 0= ;

: XREF-NULL ( -- ^n )
   NULL-PTR ;

: XREF-REC-ADDR ( n -- n )
   DREC * dbase@ + ;

package XREF
private
EXPORT CHECK:DICT-CELL@
global
\ Every reader in this file reads a record's cells through this word, which
\ rejects XREF-NULL.
: XREF-CELL@ ( ^n n -- n ) DICT-CELL@ ;
private
\ A dictionary record's address and a record cell's name address are integers.
CAST: N>REC ( n -- ^n )
CAST: N>U8 ( n -- ^u8 )
public
: REC ( n -- ^n ) XREF-REC-ADDR N>REC ;
: PTR@ ( ^n n -- ^u8 ) XREF-CELL@ N>U8 ;
;package

: XREF-REC ( n -- ^n ) XREF:REC ;

\ The dictionary index of a record address an engine cell holds as an integer.
\ n must be a live record's address; 0 and unaligned addresses are not
\ refused here (REC-LIVE? tests an address).
: XREF-ADDR>INDEX ( n -- n ) dbase@ - DREC / ;

\ The record at such an address, under the same precondition.
: XREF-ADDR>REC ( n -- ^n ) XREF-ADDR>INDEX XREF-REC ;

: LATEST ( -- ^n )
   ndict@ dup 0 <= if drop XREF-NULL exit then
   1- XREF-REC ;

: XREF-FOUND? ( ^n -- bool )
   XREF-NULL <> ;

: XREF-PTR@ ( ^n n -- ^u8 ) XREF:PTR@ ;

: XREF-START ( ^n -- n )
   XREF-START-SLOT XREF-CELL@ ;

: XREF-RAW-LEN ( ^n -- n )
   XREF-LEN-SLOT XREF-CELL@ ;

: XREF-FLAGS ( ^n -- n )
   XREF-FLAGS-SLOT XREF-CELL@ ;

: XREF-WORDLIST ( ^n -- n )
   XREF-WORDLIST-SLOT XREF-CELL@ ;

\ A namespace row has no code, so it reuses a word's two code-span cells for its
\ two wordlist ids: the public wid where a word keeps its start, the private wid
\ where a word keeps its length. A private wid of 0 means the package has opened
\ no private section.
\ Reading them as XREF-START / XREF-RAW-LEN says the opposite of what is meant.
: XREF-PKG-PUBLIC ( ^n -- n )
   XREF-START-SLOT XREF-CELL@ ;

: XREF-PKG-PRIVATE ( ^n -- n )
   XREF-LEN-SLOT XREF-CELL@ ;

\ The private wid of the package the string names, 0 for no such package or
\ no private section.
: XREF-PRIVATE-WL ( str -- n )
   BOOT:NAMESPACE-WL CHECKER-RESOLVE:WL-PROBE {: rec:^ :}
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

\ The OWNER-API wordlists are the protected ones.
: XREF-WID-PROTECTED? ( n -- bool ) {: wid:n :}
   wid OWNER-API-PUB-WID = wid OWNER-API-PRI-WID = or if XREF-TRUE exit then
   XREF-FALSE ;

\ Keep the historical body length distinct from the exact code span. A package
\ row's length cell, live or retired, holds a wordlist id, so it is answered raw
\ rather than sent through CODE-SPAN; a package reader names that cell
\ XREF-PKG-PRIVATE. A raw length is only for record serialization.
: XREF-LEN ( ^n -- n ) {: rec:^ :}
   rec XREF-WORDLIST XREF:PACKAGE-WL? if rec XREF-RAW-LEN exit then
   rec XREF-RAW-LEN dup CODE-SPAN:CHECK CODE-SPAN:BODY ;

: XREF-CODE-BYTES ( ^n -- n ) {: rec:^ :}
   rec XREF-WORDLIST XREF:PACKAGE-WL? if 0 exit then
   rec XREF-RAW-LEN CODE-SPAN:BYTES ;

: XREF-RETIRED? ( ^n -- bool )
   XREF-WORDLIST XREF:RETIRED-WL? ;

: XREF-NAME-LEN ( ^n -- n )
   XREF-FLAGS DNAME-LEN-MASK and ;

: XREF-EXT? ( ^n -- bool )
   XREF-FLAGS DNAME-EXT and 0= 0= ;

: XREF-INLINE-NAME ( ^n -- ^u8 )
   $18 XREF-REC+ XREF-A>U8 ;

: XREF-NAME-A ( ^n -- ^u8 )
   dup XREF-EXT? if XREF-NAME-SLOT XREF-PTR@ exit then
   XREF-INLINE-NAME ;

: XREF-NAME$ ( ^n -- ^u8 n )
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

: XREF-A@ ( -- ^u8 )
   XREF-A @ ;

: XREF-B@ ( -- ^u8 )
   XREF-B @ ;

: XREF-SN@ ( -- ^u8 )
   XREF-SN @ ;

: XREF-FN@ ( -- ^u8 )
   XREF-FN @ ;

: XREF-A! ( ^u8 -- )
   XREF-A ! ;

: XREF-B! ( ^u8 -- )
   XREF-B ! ;

: XREF-SN! ( ^u8 -- )
   XREF-SN ! ;

: XREF-FN! ( ^u8 -- )
   XREF-FN ! ;

: XREF-STR=CI ( str str -- bool )
   XREF-V ! XREF-B! XREF-U ! XREF-A!
   XREF-U @ XREF-V @ <> if XREF-FALSE exit then
   0 begin dup XREF-U @ < while
      dup XREF-A@ swap ZBYTE@ XREF-FOLD-C
      over XREF-B@ swap ZBYTE@ XREF-FOLD-C <> if drop XREF-FALSE exit then
      1+
   repeat drop
   XREF-TRUE ;

: XREF-MATCH? ( ^n str -- bool )
   XREF-U ! XREF-A!
   XREF-NAME$ XREF-A@ XREF-U @ XREF-STR=CI ;

package PKG-AUTH
private

: NAME-OUT ( ^n -- str bool ) {: rec:^ :}
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
: LIVE-PKG ( n n n -- str bool )
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

: LIVE ( -- ^u8 n bool )
   data-base PKG-REC-CELL + @
   data-base PKG-PUB-CELL + @
   data-base PKG-PRI-CELL + @
   LIVE-PKG ;

\ The package whose public wordlist wid is: the live namespace row that
\ publishes it.
: NS-PUB? ( ^n n -- bool ) {: rec:^ wid:n :}
   rec XREF-WORDLIST BOOT:NAMESPACE-WL =  rec XREF-PKG-PUBLIC wid =  and ;

: WID-PKG ( n -- str bool ) {: wid:n :}
   ndict@ 1-
   begin dup 0 >= while
      dup XREF-REC wid NS-PUB? if XREF-REC XREF-NAME$ XREF-TRUE exit then
      1-
   repeat drop
   s" " XREF-FALSE ;

\ The package the engine's using slot n imports (layout.f USE-WIDS-OFF).
: USE-SLOT ( n -- str bool )
   cells USE-WIDS-OFF + data-base + @ WID-PKG ;

EXPORT CHECK:PKG-LIVE-XT
EXPORT CHECK:USE-SLOT-XT
: INSTALL ( -- )
   [: LIVE ;] is PKG-LIVE-XT
   [: USE-SLOT ;] is USE-SLOT-XT ;
INSTALL

;package

: XREF-FIND-WL ( str n -- ^n ) {: a:^ u:n wid:n :}
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
: XREF-FIND-WL-INDEX ( str n -- n ) {: a:^ u:n wid:n :}
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

: XREF-QUAL-INDEX ( str -- n )
   XREF-SU ! XREF-SN!
   -1 XREF-QI !
   0 begin dup XREF-SU @ < while
      XREF-SN@ over ZBYTE@ $3A = if
         dup 0= if drop -1 exit then
         XREF-QI @ 0 >= if drop -2 exit then
         dup XREF-QI !
      then
      1+
   repeat drop
   XREF-QI @ dup 0 < if exit then
   dup XREF-SU @ 1- = if drop -1 exit then ;

: XREF-FIND-QUALIFIED ( str n -- ^n )
   XREF-IDX ! XREF-SU ! XREF-SN!
   XREF-SN@ XREF-IDX @ BOOT:NAMESPACE-WL XREF-FIND-WL
   dup XREF-FOUND? 0= if drop XREF-NULL exit then
   XREF-PKG-PUBLIC {: qwid:n :}
   XREF-SN@ XREF-IDX @ 1 + ZPTR+  XREF-SU @ XREF-IDX @ - 1-  qwid  XREF-FIND-WL ;

: XREF-FIND ( ^u8 n -- ^n )
   XREF-QUAL-INDEX
   dup -2 = if drop XREF-NULL exit then
   dup 0 >= if XREF-SN@ XREF-SU @ rot XREF-FIND-QUALIFIED exit then
   drop XREF-SN@ XREF-SU @ BOOT:GLOBAL-WL
   XREF-FIND-WL ;
names: XREF-FIND

: XREF-FIND-CURRENT-INDEX ( str -- n )
   get-current XREF-FIND-WL-INDEX ;

package XREF
private
EXPORT CHECKER-RESOLVE:HEAD-ROW

\ A qualified head resolves as the checker's does, a part of the open package
\ first, so `undefine` finds the word the checker's guard then judges.
: XREF-FIND-TARGET-INDEX ( str -- n ) {: a:^ u:n :}
   a u XREF-QUAL-INDEX {: q:n :}
   q -2 = if -1 exit then
   q 0 < if a u XREF-FIND-CURRENT-INDEX exit then
   a q HEAD-ROW {: ns:^ :}
   ns XREF-FOUND? 0= if -1 exit then
   a q 1 + ZPTR+  u q - 1-  ns XREF-PKG-PUBLIC  XREF-FIND-WL-INDEX ;
;package

: XREF-REQUIRE-INDEX ( n -- n )
   dup 0 >= if exit then
   s" xref: word not found" EXIT-STATUS:INTERNAL die ;

\ RETIRE and SEAL rewrite a write-protected record cell through patch32, which
\ this package's row in src/habu/prims.f (EPPRIM: XREF patch32) types for it.
package XREF
private

: RETIRE-RAW ( ^n -- ) {: rec:^ :}
   rec XREF-WORDLIST-SLOT cells + {: wl:^ :}
   XREF-RETIRED-WL wl patch32
   -1 wl 4 + patch32 ;

EXPORT CHECK:HOLD-RETIRED
EXPORT CHECKER-RESOLVE:REC>INDEX

public

\ A build holds the retirement and the record count then (CHECK:HOLD-RETIRED),
\ so a link replays it before the first record the compile made after it (the
\ linker's LINK-OBJECT).
: RETIRE ( ^n -- ) {: rec:^ :}
   rec REC>INDEX  rec XREF-WORDLIST  HOLD-RETIRED
   rec RETIRE-RAW ;

\ DNAME-SEALED is bit 13 of the flags cell's upper half, rewritten whole with
\ its other bits kept.
: SEAL ( ^n -- ) {: rec:^ :}
   rec XREF-FLAGS DNAME-SEALED or 32 rshift
   rec XREF-FLAGS-SLOT cells + 4 + patch32 ;

: SEALED? ( ^n -- bool ) XREF-FLAGS DNAME-SEALED and 0= 0= ;

private
\ Whether rec is a live namespace row whose public or private wid is wid.
: NS-WID? ( ^n n -- bool ) {: rec:^ wid:n :}
   rec XREF-WORDLIST BOOT:NAMESPACE-WL =
   rec XREF-PKG-PUBLIC wid =  rec XREF-PKG-PRIVATE wid =  or  and ;

\ Whether rec is named and enters at xt.
: NAMED-AT? ( ^n n -- bool ) {: rec:^ xt:n :}
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
: NS-ROW ( n -- ^n bool ) {: wid:n :}
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

private
: PKG-OPEN-SEALED? ( -- bool )
   data-base PKG-PUB-CELL + @ 0= if XREF-FALSE exit then
   data-base PKG-REC-CELL + @ XREF-ADDR>REC SEALED? ;

EXPORT BOOT:TYPES-WL

: PKG-WID-SEALED? ( n -- bool ) {: wid:n :}
   wid FIRST-DYNAMIC-WID < if XREF-FALSE exit then
   wid BOOT:GLOBAL-WL =  wid TYPES-WL =  or if XREF-FALSE exit then
   data-base PKG-PUB-CELL + @ 0= 0= if
      wid data-base PKG-PUB-CELL + @ =  wid data-base PKG-PRI-CELL + @ =  or if
         PKG-OPEN-SEALED? exit
      then
   then
   wid WID-SEALED? ;

public
\ Whether a definition into wordlist wid is refused: wid is a sealed package's,
\ the open package is sealed, or an open part's owner is sealed and wid is not
\ the part's.
: SEALED-TARGET? ( n -- bool ) {: wid:n :}
   wid PKG-WID-SEALED?  PKG-OPEN-SEALED?  or if XREF-TRUE exit then
   data-base ENCLOSING:REC + @ {: owner:n :}
   owner 0= if XREF-FALSE exit then
   owner XREF-ADDR>REC SEALED? 0= if XREF-FALSE exit then
   wid data-base PKG-PUB-CELL + @ <>  wid data-base PKG-PRI-CELL + @ <>  and ;

;package

\ A rejection's name between backticks.
: XREF-EMIT-TICKS ( str -- ) {: a:^ u:n :}
   96 REJECTION:EMIT1  a u REJECTION:EMIT-STR  96 REJECTION:EMIT1 ;

\ The scope a lookup of the name a u searched: a qualified name its package's
\ public words, a bare one wid.
: XREF-EMIT-SCOPE ( str n -- ) {: a:^ u:n wid:n :}
   a u XREF-QUAL-INDEX {: q:n :}
   q -2 = if s" none: the name holds more than one `:`" REJECTION:EMIT-STR exit then
   q 0 >= if
      s" the public words of package " REJECTION:EMIT-STR  a q XREF-EMIT-TICKS exit
   then
   wid BOOT:GLOBAL-WL = if s" the global words" REJECTION:EMIT-STR exit then
   wid XREF:NS-ROW 0= if drop s" the current wordlist" REJECTION:EMIT-STR exit then
   {: row:^ :}
   row XREF-PKG-PUBLIC wid = if s" the public words of package "
   else s" the private words of package " then REJECTION:EMIT-STR
   row XREF-NAME$ XREF-EMIT-TICKS ;

package GENERATED-DECL-NAME-PREFLIGHT

private
EXPORT CHECKER-RESOLVE:HEAD-ROW
EXPORT CHECKER-RESOLVE:SOURCE-SEALED?
EXPORT TYPE-DECL:TDECL-THROW-AT

: SEALED-STOP ( str -- )
   s" " s" " TYPE-DECL:E-TDECL-SEALED TDECL-THROW-AT ;

\ A family or generated name is the declaration's error where SEALED-TARGET?
\ refuses it, or in a package the verified source sealed. A head resolves as the
\ definition's does: a part is asked by its path.
: CHECK ( ^u8 n -- ) {: a:^ u:n :}
   a u XREF-QUAL-INDEX {: q:n :}
   q 0 < IF get-current XREF:SEALED-TARGET? IF a u SEALED-STOP THEN EXIT THEN
   a q HEAD-ROW {: ns:^ :}
   ns XREF-FOUND? IF ns XREF-NAME$ ELSE a q THEN SOURCE-SEALED? IF a u SEALED-STOP THEN
   ns XREF-FOUND? IF ns XREF-PKG-PUBLIC ELSE 0 THEN XREF:SEALED-TARGET? IF a u SEALED-STOP THEN ;

EXPORT TYPE-DECL:TDECL-NAME-PREFLIGHT-XT

: INSTALL ( -- )
   [: CHECK ;] is TDECL-NAME-PREFLIGHT-XT ;

INSTALL
seal-package

;package

: XREF-RETIRE-WL ( str n -- ) {: a:^ u:n wid:n :}
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
   k XREF-REC {: parent:^ :}
   k 1+ XREF-REC {: clause:^ :}
   parent XREF-WORDLIST clause XREF-WORDLIST <> if XREF-FALSE exit then
   parent XREF-NAME$ {: name:^ len:n :}
   clause XREF-NAME$ {: derived:^ dlen:n :}
   DOES-CLAUSE:SUFFIX$ {: suffix:^ sufu:n :}
   dlen len sufu + <> if XREF-FALSE exit then
   derived len name len XREF-STR=CI 0= if XREF-FALSE exit then
   derived len + sufu suffix sufu XREF-STR=CI 0= if XREF-FALSE exit then
   parent XREF-START {: first:n :}
   clause XREF-START {: entry:n :}
   entry first <= if XREF-FALSE exit then
   entry clause XREF-CODE-BYTES +  first parent XREF-CODE-BYTES +  <= ;

package XREF
private
EXPORT CHECK:CHECKER-UNDEFINE
EXPORT CHECK:REJECT-QUALIFIED
EXPORT CHECK:DOES-COMPANION?-XT
\ The checker overlay asks this same test of a live definer a replayed
\ `undefine` retires (src/core/checker.f CHECKER-OVERLAY CLAUSE-OF); a replayed
\ definer's clause, codeless, is paired where the overlay publishes it.
: DOES-COMPANION-INSTALL ( -- ) [: XREF-DOES-COMPANION? ;] is DOES-COMPANION?-XT ;
DOES-COMPANION-INSTALL
;package

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
\ pair and the live word would be gone. The rejection names the word before
\ anything changes. Both loops run these words; a replayed `undefine` never
\ reaches them, since the checker's source pass retires through the overlay
\ (src/core/checker.f CHECKER-OVERLAY:RETIRE).
: XREF-UNDEFINE-GUARD ( str -- ) {: a:^ u:n :}
   data-base REPLAY-SCOPE:LATCH + @ 0= if exit then
   s" E-OVERLAY-OPEN" REJECTION:OPEN
   s" `undefine` " REJECTION:EMIT-STR  a u XREF-EMIT-TICKS
   s"  while a checker replay is open" REJECTION:EMIT-STR
   a u REJECTION:AT  s" replay still open" REJECTION:EMIT-STR
   REJECTION:HELP
   s" run `undefine` after the `CHECK:SCOPED` quotation returns"
   REJECTION:EMIT-STR
   EXIT-STATUS:REJECTED REJECTION:STOP ;

: XREF-REQUIRE-UNDEFINE ( n str -- n ) {: idx:n a:^ u:n :}
   idx 0 < if
      s" E-UNDEFINED" REJECTION:OPEN
      a u XREF-EMIT-TICKS
      s"  is not defined here, so `undefine` cannot retire it" REJECTION:EMIT-STR
      a u REJECTION:AT  s" not found" REJECTION:EMIT-STR
      s" scope" REJECTION:NOTE  a u get-current XREF-EMIT-SCOPE
      REJECTION:HELP
      s" name a word this scope defines, or drop the `undefine` line"
      REJECTION:EMIT-STR
      EXIT-STATUS:REJECTED REJECTION:STOP
   then
   idx ;

package XREF
public
\ The index `undefine` would retire for a u, negative for none; a
\ malformed name is rejected.
: UNDEFINE-FIND ( str -- n ) {: a:^ u:n :}
   a u XREF-QUAL-INDEX -2 = if a u a u REJECT-QUALIFIED then
   a u XREF-FIND-TARGET-INDEX ;

\ The index `undefine` retires for a u, rejecting one this scope does not define.
: UNDEFINE-INDEX ( str -- n ) {: a:^ u:n :}
   a u UNDEFINE-FIND  a u XREF-REQUIRE-UNDEFINE ;

: UNDEFINE-NAME ( str -- )
   XREF-SU ! XREF-SN!
   XREF-SN@ XREF-SU @ XREF-UNDEFINE-GUARD
   XREF-SN@ XREF-SU @ UNDEFINE-INDEX XREF-IDX !
   XREF-SN@ XREF-SU @ CHECKER-UNDEFINE   \ guarded checker mutation completes before retirement
   XREF-IDX @ XREF-RETIRE-INDEX ;
;package

\ The seal watermark of the dictionary being loaded: while a build writes an
\ image, the image's, where a header store goes (habu2.f ROUTE-HEADER), 0 until
\ the product seals; else hb's.
: SEAL-NDICT@ ( -- n )
   data-base BOOT:IMAGE-BASE-CELL + @ {: image:n :}
   image 0 <> if NULL-PTR BYTE-VIEW image + SEAL-NDICT-CELL + CELL-VIEW @ exit then
   data-base SEAL-NDICT-CELL + @ ;

\ A selected record is borrowed from the resolver at SELECT. The slot alone is
\ never identity: lowering and re-exposing the count issues another occurrence.
\ These references stay in this process and carry no checker effect or code lease.
\ The resolver's primitive rows belong to DEF-OCC; the checker also selects
\ occurrences when it binds an exported contract (src/habu/prims.f).
package DEF-OCC
private
: NONCALLABLE ( ^n -- ) {: rec:^ :}
   s" E-NONCALLABLE" REJECTION:OPEN
   s" `DEF-OCC:CALLABLE` was given the package `" REJECTION:EMIT-STR
   rec XREF-NAME$ REJECTION:EMIT-STR
   s" `, which has no code" REJECTION:EMIT-STR
   REJECTION:HELP
   s" pass the slot and occurrence `DEF-OCC:SELECT` answered for a word"
   REJECTION:EMIT-STR
   EXIT-STATUS:REJECTED REJECTION:STOP ;

public
: SELECT ( ^n -- n n ) def-occ-select ;
: RESOLVE ( n n -- ^n ) def-occ-resolve ;

: CALLABLE ( n n -- n )
   RESOLVE dup XREF-WORDLIST BOOT:NAMESPACE-WL = if NONCALLABLE then
   XREF-START ;

;package

package CODE-RECLAIM

private

\ A FLOOR IS A STATEMENT ABOUT CODE ORDER: the bytes from it upwards are handed
\ back and rewritten, so no routine a surviving record points at may begin
\ there. THE FLOOR IS A RETIRED RECORD'S OWN START, CHOSEN BY ADDRESS: a
\ build's records, its aliases included, name its own routines, which lie above
\ every survivor's, or kernel text below the arena. A start is the one address
\ nameable without knowing the length of the routine below it (the recorded
\ length excludes a trailing return). The cost, when the lowest retirement is
\ not the first routine above the survivors, is a routine's worth of bytes left
\ claimed. No floor is kept in a cell: an arena address moves with every
\ launch, and a cell would carry it into a captured image.

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
   k XREF-REC {: rec:^ :}
   rec XREF-EXT? 0= if s exit then
   rec XREF-NAME-SLOT XREF-CELL@ {: a:n :}
   a ARENA-LO <  a cp@ < 0=  or if s exit then
   a ;

\ The lower of FLOOR and a record's REC-LOW S, where 0 is no arena byte.
: LOWER ( n n -- n ) {: floor:n s:n :}
   s 0 = if floor exit then
   s floor min ;

\ The caller's floor lies at or below cp@ and above every routine a surviving
\ record points at: FLOOR-FROM's, or a mark taken before the code above it.
: TRUNCATE ( n -- )
   {: floor:n :}
   floor cp@ over - reloc-maps-clear
   floor cp! ;

public

\ The floor of a sweep that retires every record from CUT, a build's
\ first, upwards: the lowest REC-LOW among them, else cp@.
: FLOOR-FROM ( n -- n ) {: cut:n :}
   cp@
   ndict@ cut ?do
      i REC-LOW LOWER
   loop ;

;package

: XREF-NAME. ( ^n -- )
   XREF-NAME$ type ;

: XREF-N. ( str n -- )
   XREF-NV ! XREF-FU ! XREF-FN!
   XREF-FN@ XREF-FU @ type
   XREF-SP emit
   XREF-NV @ . ;

: XREF. ( ^n -- )
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
: UNDEFINE-NO-NAME ( str -- ) {: t:^ tu:n :}
   t tu s" the name of the word to retire"
   t tu s" no name follows" REJECTION:MISSING-OPERAND
   s" write the word after it, as in `undefine NAME`" REJECTION:EMIT-STR ;

\ The `undefine` token is kept at entry: parse-name moves the token cells.
: undefine ( -- )
   data-base TKA-CELL + 0 ptr-field @  data-base TKL-CELL + @
   {: ta:^ tu:n :}
   parse-name dup 0= if
      ta tu UNDEFINE-NO-NAME  EXIT-STATUS:REJECTED REJECTION:STOP
   then
   XREF:UNDEFINE-NAME ;

: WORDS ( -- )
   0 begin dup ndict@ < while
      dup XREF-REC dup XREF-RETIRED? if drop else XREF-NAME. space then
      1+
   repeat drop cr ;

\ The installed provider holds direct code references. Retire every source-level
\ rebinding seam and mutable provider cell before the engine-prefix seal.
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
\ the only way in.
package CHECKER-RESYNC
undefine SCOPE
;package
