\ publish.f - making a sealed emission an ordinary word of the running engine.
\ One concern: the engine boundary between machine code the chain emitted and a
\ dictionary record every caller already reaches.
\
\ The only door is the sealed emission: REPUBLISH takes a NAME and nothing else,
\ and every byte is read back out of A64EMIT, which refuses before a run is
\ sealed and before the validator has accepted that module's allocation.
\
\ Two phases. Everything that can refuse refuses before a byte of code, a map
\ bit or a record cell has moved, and nothing in the commit phase can throw.
\
\ The write window is one engine primitive and cannot be a loop here: flipping
\ the code region writable removes execute permission from the page the running
\ interpreter is on, so everything between the two flips has to be engine text.
\
\ No two publications claim one code slot. The pointer goes back only through
\ CODE-RECLAIM, which tells every address-keyed record its floor, so a rewind
\ nobody announced becomes E-NPUB-SLOT at the next publication.
\
\ A branch inside the region keeps its distance wherever the region is mapped; a
\ branch out of it does not, and has to be recorded where the branch is BUILT.

require lib/prelude.f
require lib/errors.f
require lib/string.f
require lib/vector.f
require src/compiler/native/branch.f
require src/compiler/native/clobber.f
require src/compiler/native/dict.f
require src/compiler/native/emit.f
require src/compiler/native/trap.f

package NPUB

private

\ ---- the two engine facts this seam depends on -------------------------------
$4000 constant CODE-RESERVE

4 constant INSN-BYTES

\ ---- the three primitives that write -----------------------------------------
TRUSTED: CODE-WINDOW ( ptr u8 n n -- )
   code-publish ;

\ One call site recorded as reaching outside the region. The window above has
\ already cleared every bit of the span, so this only ever sets.
TRUSTED: RELOC-EXTERNAL ( n -- )
   callmap-set ;

\ One address chain recorded, at the word its first move-wide sits in.
TRUSTED: RELOC-ADDR ( n -- )
   addrmap-set ;

\ The record's two cells, written whole and in one order: length, then the start
\ address with a store-release.
TRUSTED: RETARGET-REC ( n n n -- )
   xref-retarget ;

\ OR'd into the record's flags; the engine's own publish tail pokes the same
\ byte into the same field (habu2.f EM-REC-WIDE-PUBLISH).
TRUSTED: MIN-IN-REC ( n n -- )
   min-in-mark ;

\ ---- the replacement log -----------------------------------------------------
128 constant ROWS-SEED

\ A row's spelling is NAME-CELLS cells wide whatever the name's length, so the
\ width and the length this log refuses past cannot drift.
8 constant NAME-CELLS
NAME-CELLS cells constant NAME-MAX

\ ---- and the one ceiling that is left -----------------------------------------
REGION INSN-BYTES / constant ROWS-CEIL

create LOG-NAMES VEC-HEADER-CELLS cells allot
create LOG-LENS VEC-HEADER-CELLS cells allot
create LOG-WIDS VEC-HEADER-CELLS cells allot
create LOG-OLD-START VEC-HEADER-CELLS cells allot
create LOG-OLD-LEN VEC-HEADER-CELLS cells allot
create LOG-NEW-START VEC-HEADER-CELLS cells allot
create LOG-NEW-LEN VEC-HEADER-CELLS cells allot

\ ---- when the storage is taken, and why not at load ---------------------------
\ The mappings are NOT taken while this file loads: a mapping base is process
\ local, and an AOT capture copies this file's DATA into another engine.
\ The guard asks the LAST column, so a part-way allocation cannot look finished.
: LOG-INIT ( -- )
   LOG-NEW-LEN VEC-CAP@ COUNT>N 0= 0= if exit then
   LOG-NAMES ROWS-SEED NAME-CELLS * VEC-COUNT VEC-INIT
   LOG-LENS ROWS-SEED VEC-COUNT VEC-INIT
   LOG-WIDS ROWS-SEED VEC-COUNT VEC-INIT
   LOG-OLD-START ROWS-SEED VEC-COUNT VEC-INIT
   LOG-OLD-LEN ROWS-SEED VEC-COUNT VEC-INIT
   LOG-NEW-START ROWS-SEED VEC-COUNT VEC-INIT
   LOG-NEW-LEN ROWS-SEED VEC-COUNT VEC-INIT ;

: ROWS# ( -- n )
   LOG-NEW-START VEC-LEN@ LEN>N ;

\ Read raw: these are the readers on a path whose length is the population. The
\ four columns a lookup ANSWERS with keep the checked accessor.
: NAME-AT ( n -- ptr u8 ) {: k:n :}
   LOG-NAMES VEC-DATA@ k NAME-MAX * + BYTE-VIEW ;

: LEN-AT ( n -- n ) {: k:n :}
   LOG-LENS VEC-DATA@ k cells + @ ;

: WID-AT ( n -- n ) {: k:n :}
   LOG-WIDS VEC-DATA@ k cells + @ ;

: NEW-START-AT ( n -- n ) {: k:n :}
   LOG-NEW-START VEC-DATA@ k cells + @ ;

: LOG-NAME$ ( n -- ptr u8 n ) {: k:n :}
   k NAME-AT  k LEN-AT ;

: LOG-ROW? ( n ptr u8 n n -- bool ) {: k:n a:ptr u:n wid:n :}
   k WID-AT wid <> if false exit then
   k LOG-NAME$ a u STR= ;

\ The FIRST row a name has carries what the old emitter produced for it. A name
\ is republished again only after a FORGET, whose floor drops the earlier row.
: LOG-FIND ( ptr u8 n n -- n ) {: a:ptr u:n wid:n :}
   -1
   ROWS# 0 ?do
      i a u wid LOG-ROW? if drop i leave then
   loop ;

: LOG-OK ( ptr u8 n n -- n ) {: a:ptr u:n wid:n :}
   a u wid LOG-FIND dup 0 < if E-NPUB-LOG throw then ;

\ Room is taken in front because an allocation can fail and the append runs in
\ the commit phase, which may not throw. Taking room changes no row.
: LOG-ROOM ( -- )
   LOG-INIT
   ROWS# 1+ {: need:n :}
   need ROWS-CEIL > if E-NPUB-CAP throw then
   LOG-NAMES need NAME-CELLS * VEC-COUNT VEC-ENSURE
   LOG-LENS need VEC-COUNT VEC-ENSURE
   LOG-WIDS need VEC-COUNT VEC-ENSURE
   LOG-OLD-START need VEC-COUNT VEC-ENSURE
   LOG-OLD-LEN need VEC-COUNT VEC-ENSURE
   LOG-NEW-START need VEC-COUNT VEC-ENSURE
   LOG-NEW-LEN need VEC-COUNT VEC-ENSURE ;

\ Asked in the validation phase, so the append below cannot answer anything but
\ yes; the throws inside it are the backstop and not the path.
: LOG-CK ( n -- ) {: u:n :}
   u NAME-MAX > if E-NPUB-CAP throw then
   LOG-ROOM ;

\ The bytes are written first and the length raised after, so a row is never
\ counted before the column it is counted by holds it.
: NAME+ ( ptr u8 n n -- ) {: a:ptr u:n k:n :}
   a  k NAME-AT  u STR-LEN BYTE-COPY-LEN
   k 1+ NAME-CELLS * VEC-LEN LOG-NAMES VEC-LEN! ;

\ The address column is pushed LAST: it is what ROWS# counts and what the
\ reclamation cut reads, so a row becomes one only when every other column holds it.
: LOG+ ( ptr u8 n n n n n n -- )
   {: a:ptr u:n wid:n os:n ol:n ns:n nl:n :}
   u NAME-MAX > if E-NPUB-CAP throw then
   LOG-ROOM
   a u ROWS# NAME+
   u   LOG-LENS VEC-PUSH-N drop
   wid LOG-WIDS VEC-PUSH-N drop
   os LOG-OLD-START VEC-PUSH-N drop
   ol LOG-OLD-LEN VEC-PUSH-N drop
   nl LOG-NEW-LEN VEC-PUSH-N drop
   ns LOG-NEW-START VEC-PUSH-N drop ;

\ ---- a row's lifetime is the lifetime of the code it describes ----------------
\ A row is never dropped to make space, but a row across a RECLAMATION does not
\ preserve evidence, it manufactures it. The rows that go are a SUFFIX.
: LOG-TRUNC-TO ( n -- ) {: k:n :}
   k NAME-CELLS * VEC-LEN LOG-NAMES VEC-LEN!
   k VEC-LEN LOG-LENS VEC-LEN!
   k VEC-LEN LOG-WIDS VEC-LEN!
   k VEC-LEN LOG-OLD-START VEC-LEN!
   k VEC-LEN LOG-OLD-LEN VEC-LEN!
   k VEC-LEN LOG-NEW-LEN VEC-LEN!
   k VEC-LEN LOG-NEW-START VEC-LEN! ;

: LOG-FLOOR-ROW ( n -- n ) {: floor:n :}
   ROWS#
   ROWS# 0 ?do
      i NEW-START-AT floor >= if drop i leave then
   loop ;

\ A row below the floor means this log is not the sequence the cut rests on: a
\ defect here with no correct answer to give, and a watcher may not throw.
: LOG-ORDER-CK ( n n -- ) {: floor:n k:n :}
   ROWS# k ?do
      i NEW-START-AT floor < if
         s" npub: logged republications out of publication order" 76 die
      then
   loop ;

: LOG-DROP-FROM ( n -- ) {: floor:n :}
   floor LOG-FLOOR-ROW {: k:n :}
   floor k LOG-ORDER-CK
   k LOG-TRUNC-TO ;

\ ---- which names may be republished -----------------------------------------
: NAME-REC ( ptr u8 n n -- n ) {: a:ptr u:n wid:n :}
   a u wid XREF-FIND-WL-INDEX dup 0 < if E-NPUB-NAME throw then ;

: WORDLIST-CK ( n -- ) {: idx:n :}
   idx XREF-REC XREF-WORDLIST {: wid:n :}
   wid XREF-RETIRED-WL = if E-NPUB-NAME throw then
   wid XREF-NAMESPACE-WL = if E-NPUB-NAME throw then ;

: FLAGS-CK ( n -- ) {: idx:n :}
   idx XREF-REC XREF-FLAGS {: f:n :}
   f DNAME-INT and 0<> if E-NPUB-NAME throw then
   f DNAME-IMM and 0<> if E-NPUB-NAME throw then ;

: TARGET ( ptr u8 n n -- n ) {: a:ptr u:n wid:n :}
   a u wid NAME-REC {: idx:n :}
   idx WORDLIST-CK
   idx FLAGS-CK
   idx ;

\ ---- the emission, measured --------------------------------------------------
: SIZE-CK ( -- n )
   A64EMIT:SIZE {: n:n :}
   A64EMIT:INSNS 0 <= if E-NPUB-SIZE throw then
   n INSN-BYTES < if E-NPUB-SIZE throw then
   n INSN-BYTES mod 0<> if E-NPUB-SIZE throw then
   n ;

: OFFSET-CK ( n n -- n ) {: off:n size:n :}
   off 0 < if E-NPUB-OFFSET throw then
   off INSN-BYTES + size > if E-NPUB-OFFSET throw then
   off INSN-BYTES mod 0<> if E-NPUB-OFFSET throw then
   off ;

\ ---- claiming the code space -------------------------------------------------
: CODE-CEILING ( -- n )
   dbase@ REGION + CODE-RESERVE - ;

: ROOM-CK ( n -- n ) {: size:n :}
   cp@ {: fn:n :}
   fn size + CODE-CEILING > if E-NPUB-ROOM throw then
   fn ;

\ ---- the slot is above every slot this seam has already claimed ---------------
variable CLAIMED
0 CLAIMED !

: SLOT-CK ( n -- ) {: fn:n :}
   fn CLAIMED @ < if E-NPUB-SLOT throw then ;

: CLAIM ( n n -- ) {: fn:n size:n :}
   fn size + CLAIMED ! ;

\ A routine starting below the floor is untouched, so this lowers the line to
\ the floor rather than to nothing.
: RECLAIMED ( n -- ) {: floor:n :}
   floor LOG-DROP-FROM
   floor CLAIMED @ < if floor CLAIMED ! then ;

\ ---- the routine's branches and the slot it is written at --------------------
: PLACE-CK ( n -- ) {: fn:n :}
   A64EMIT:PLACED? 0= if exit then
   A64EMIT:PLACEMENT fn <> if E-NPUB-PLACE throw then ;

\ ---- what the routine branches to, read off the instructions ------------------
: INSN-ADDR ( n n n -- n ) {: fn:n size:n k:n :}
   k A64EMIT:MAP-OFFSET@ size OFFSET-CK fn + ;

\ Control that reaches the trap never comes back, so no register it writes can
\ be read by a caller; charging them would make every such caller save its file.
: ENDS-PROCESS? ( n -- bool ) {: t:n :}
   NTRAP:ROUTINE$ NDICT:CALL-TARGET {: e:n :}
   e 0= if false exit then
   t e = ;

: TARGET-CK ( n n n n -- ) {: t:n fn:n g:n f:n :}
   t fn = if exit then
   t ENDS-PROCESS? if exit then
   t A64EFF:GPR-ALL NCLOB:GPR-CLOB A64EFF:GPRS-N
   g invert and 0<> if E-NPUB-CLOBBER throw then
   t A64EFF:FPR-ALL NCLOB:FPR-CLOB A64EFF:FPRS-N
   f invert and 0<> if E-NPUB-CLOBBER throw then ;

\ ---- and a branch that LEAVES the routine is a call site too -------------------
: LEAVES? ( n n n -- bool ) {: fn:n size:n t:n :}
   t fn < if true exit then
   t fn size + >= ;

: BRANCH-TARGET-CK ( n n n n n -- ) {: fn:n size:n at:n g:n f:n :}
   at A64EMIT:WORD@ {: w:n :}
   w NBR:BL? if
      fn size at INSN-ADDR  w  NBR:BL-TARGET  fn g f TARGET-CK exit
   then
   w NBR:B? 0= if exit then
   fn size at INSN-ADDR  w  NBR:B-TARGET {: t:n :}
   fn size t LEAVES? 0= if exit then
   t fn g f TARGET-CK ;

: BRANCH-CK ( n n -- ) {: fn:n size:n :}
   A64EMIT:GPR-CLOBBER A64EFF:GPRS-N {: g:n :}
   A64EMIT:FPR-CLOBBER A64EFF:FPRS-N {: f:n :}
   A64EMIT:INSNS 0 ?do
      fn size i g f BRANCH-TARGET-CK
   loop ;

\ ---- the source map describes the bytes the window will copy ------------------
: MAP-CK ( n -- ) {: size:n :}
   A64EMIT:INSNS 0 ?do
      i A64EMIT:MAP-OFFSET@ size OFFSET-CK  i INSN-BYTES * <> if
         E-NPUB-OFFSET throw
      then
   loop ;

\ ---- the relocation record of every call the emission carries -----------------
: EXTERNAL? ( n -- bool ) {: t:n :}
   t dbase@ < if true exit then
   t dbase@ REGION + >= ;

\ The engine's loader refuses an image whose recorded site does not hold a
\ branch-with-link, so a tail branch OUT of the region is refused instead
\ (habu-relocate-a-tail-96d571af). One that stays inside needs no record.
: TAIL-RELOC-CK ( n n -- ) {: fn:n size:n :}
   A64EMIT:INSNS 0 ?do
      i A64EMIT:WORD@ {: w:n :}
      w NBR:B? if
         fn size i INSN-ADDR  w  NBR:B-TARGET {: t:n :}
         fn size t LEAVES? t EXTERNAL? and if E-NPUB-RELOC throw then
      then
   loop ;

\ Runs after the window, which cleared the whole span, so this only ever sets.
: RELOC-CALLS ( n -- ) {: fn:n :}
   A64EMIT:INSNS 0 ?do
      i A64EMIT:WORD@ NBR:BL? if
         fn i INSN-BYTES * +  i A64EMIT:WORD@  NBR:BL-TARGET EXTERNAL? if
            fn i INSN-BYTES * + RELOC-EXTERNAL
         then
      then
   loop ;

\ The sites are not searched for and the bytes are never decoded: emit.f
\ recorded each one while it wrote the instructions.
: RELOC-ADDRS ( n -- ) {: fn:n :}
   A64EMIT:ADDR-SITES 0 ?do
      fn  i A64EMIT:ADDR-SITE@ INSN-BYTES * +  RELOC-ADDR
   loop ;

\ ---- how much of the routine a caller may copy --------------------------------
\ The engine stores a length EXCLUDING the trailing return, because that is the
\ span its inliner copies. A routine that does not end in a return has none to
\ leave out, so its recorded length is the whole emission.
: RECORDED-LEN ( n -- n ) {: size:n :}
   A64EMIT:TRAILING-RETURN? 0= if size exit then
   size INSN-BYTES - ;

public

\ Every refusal this publication can make, ending before the first byte moves:
\ everything that does not depend on how the record was reached.
: VALIDATE-EMISSION ( n -- n ) {: size:n :}
   size ROOM-CK {: fn:n :}
   fn PLACE-CK
   fn SLOT-CK
   fn  A64EMIT:GPR-CLOBBER  A64EMIT:FPR-CLOBBER  NCLOB:RECORD-CK
   fn size BRANCH-CK
   fn size TAIL-RELOC-CK
   fn ;

: VALIDATE ( ptr u8 n n n -- n n ) {: a:ptr u:n wid:n size:n :}
   size MAP-CK
   a u wid TARGET {: idx:n :}
   size VALIDATE-EMISSION {: fn:n :}
   u LOG-CK
   idx fn ;

\ Bytes first, then each call's relocation record, then LAST the record cell
\ that makes the routine reachable, written with a release.
: COMMIT ( n n n -- ) {: idx:n fn:n size:n :}
   A64EMIT:BYTES fn size CODE-WINDOW
   fn RELOC-CALLS
   fn RELOC-ADDRS
   fn  size RECORDED-LEN  idx RETARGET-REC
   fn  A64EMIT:GPR-CLOBBER  A64EMIT:FPR-CLOBBER  NCLOB:RECORD
   fn size CLAIM ;

: REPUBLISH ( ptr u8 n n -- ) {: a:ptr u:n wid:n :}
   SIZE-CK {: size:n :}
   a u wid size VALIDATE {: idx:n fn:n :}
   idx XREF-REC XREF-START {: os:n :}
   idx XREF-REC XREF-LEN {: ol:n :}
   idx fn size COMMIT
   a u wid os ol fn  size RECORDED-LEN  LOG+ ;

\ ---- committing a publication the engine withheld ----------------------------
: HELD-IDX ( -- n )
   ndict@ ;

\ A lower index is a word that IS published and a higher one is a slot no `:`
\ has written.
: HELD-CK ( n -- ) {: idx:n :}
   idx HELD-IDX <> if E-NPUB-HELD throw then
   idx WORDLIST-CK
   idx FLAGS-CK ;

\ The two the engine's publish tail applies after ndict++ (EM-REC-WIDE-PUBLISH),
\ read from the same two latches.
: HELD-FACTS ( n -- ) {: idx:n :}
   REC-WIDE-PUBLISH
   REC-MIN-IN@ {: mi:n :}
   mi 0<> if idx mi MIN-IN-REC then ;

\ `:` is the sole constructor of a dictionary record in this system, so the
\ chain COMMITS the record the engine already built rather than building one.
\ The held record is always the slot the count points at, so this takes no index.
: HELD-PROVE ( -- n n n )
   SIZE-CK {: size:n :}
   size MAP-CK
   HELD-IDX {: idx:n :}
   idx HELD-CK
   idx XREF-REC XREF-NAME$ nip {: u:n :}
   size VALIDATE-EMISSION {: fn:n :}
   u LOG-CK
   idx fn size ;

: COMMIT-HELD ( -- )
   HELD-PROVE {: idx:n fn:n size:n :}
   idx XREF-REC XREF-NAME$ {: a:ptr u:n :}
   idx XREF-REC XREF-WORDLIST {: wid:n :}
   idx fn size COMMIT
   ndict@ 1+ ndict!
   idx HELD-FACTS
   a u wid 0 0 fn  size RECORDED-LEN  LOG+ ;

\ One word and not a copy of one, so the two answers cannot drift apart; a
\ committed question would cost a code slot and a row that may not be evicted.
: VALIDATE-HELD ( -- )
   HELD-PROVE 2drop drop ;

\ The engine's own free code slot, which is what REPUBLISH claims. Asking is
\ free: a caller that asks and never publishes has moved no pointer.
: NEXT-SLOT ( -- n )
   cp@ ;

\ A caller deciding whether it may leave through a callee at all asks here
\ BEFORE it commits to the shape; TAIL-RELOC-CK is the fail-closed backstop.
: IN-REGION? ( n -- bool )
   EXTERNAL? 0= ;

: REPUBLISHED ( -- n )
   ROWS# ;

: REPUBLISHED? ( ptr u8 n n -- bool ) {: a:ptr u:n wid:n :}
   a u wid LOG-FIND 0 >= ;

\ The only surviving measurement of what the old emitter produced for that name.
: OLD-START ( ptr u8 n n -- n ) {: a:ptr u:n wid:n :}
   LOG-OLD-START  a u wid LOG-OK VEC-IDX VEC-N@ ;

: OLD-LEN ( ptr u8 n n -- n ) {: a:ptr u:n wid:n :}
   LOG-OLD-LEN  a u wid LOG-OK VEC-IDX VEC-N@ ;

: NEW-START ( ptr u8 n n -- n ) {: a:ptr u:n wid:n :}
   LOG-NEW-START  a u wid LOG-OK VEC-IDX VEC-N@ ;

: NEW-LEN ( ptr u8 n n -- n ) {: a:ptr u:n wid:n :}
   LOG-NEW-LEN  a u wid LOG-OK VEC-IDX VEC-N@ ;

\ So the line a claimed slot is held against follows the pointer down when the
\ space above it is really given back.
\
\ PUBLIC BECAUSE A CAPTURED CHAIN HAS TO RE-RUN IT: the slot is CODE-RECLAIM's,
\ below the capture window, so a seeded engine would hold every published line
\ against an address the reclaim already gave back. Named on
\ tools/aot-chain-capture.f's boot-run list, which LFIND resolves.
: WATCH-INSTALL ( -- )
   [: RECLAIMED ;] CODE-RECLAIM:WATCH ;

WATCH-INSTALL

private
get-current prot-wid-add

public
get-current prot-wid-add

;package
