\ effect-store-census.f - what the checker's effect store is actually made of.
\
\ WHY IT EXISTS. The store was 7.5MB of growth for 6,799 words - about 1.1KB per
\ word for signatures carrying a handful of small integers - and no argument
\ about WHY could be settled without a byte-for-byte account of what was in
\ there. This walks a window of the store and says where every byte of it went.
\ It is the acceptance instrument for dot habu-the-effect-store-45bdc561 and it
\ stays in the tree because the next question about the store deserves the same
\ answer rather than another one-off probe.
\
\ IT WALKS THE GRAPH, NOT SPANS, and that is the whole design. A record used to
\ own the contiguous bytes between itself and the next record, so a census could
\ subtract two offsets. Since node interning landed, a record's rows may be
\ nodes an older record wrote, so "the bytes of record R" is a question about
\ REACHABILITY: a node belongs to the first record that reaches it, and every
\ later reader of it is a SHARE. The walk therefore carries a visited set, and
\ the accounting identity it publishes - window = headers + node bytes, orphan
\ zero - is what proves the walk saw everything exactly once.
\
\ IT READS THE LAYOUT FROM ITS OWNER. Every record and node offset, every tag,
\ and both record and node sizes are asked of src/core/checker.f through the
\ named boundary below rather than restated here. A census that carried its own
\ copy of the layout would agree with the store only until somebody moved a
\ field, and would then report confidently wrong numbers - which is exactly the
\ failure a measurement instrument must not have.
\
\ THE DISTINCT-SHAPE COUNT IS THIS FILE'S OWN ANSWER, computed bottom-up from the
\ stored fields without consulting the checker's interner at all. That makes it a
\ differential rather than an echo: over the WHOLE store NODES and SHAPES must
\ come out equal, because a second copy of a shape is precisely what the interner
\ exists to prevent. Over a partial window they need not, since a shape the
\ window reuses may live in a node below its base. Before interning, the whole
\ store held 84 nodes for every shape.
\
\ Run it over a load:
\   bin/hb-host --load tools/effect-store-census-run.f -- src/compiler/native/migrate.f
\ or drive it in-process: MARK, load, RUN, then read the counters.

require lib/errors.f
require lib/string.f
require lib/memory.f

package EFF-CENSUS

\ ---- the read boundary onto the checker's private store -----------------------
\ Read-only: offsets in, cells and bytes out, no store word and no mutation. The
\ same shape test/engine-suite.f's TG-* shims use, and for the same reason - the
\ store is checker-internal and its names are stripped past the seal, so a tool
\ reaches it as compiled calls from named one-line boundaries.
TRUSTED: STORE-END ( -- n ) UEND @ ;
TRUSTED: CELL-AT ( n -- n ) USIGS-CELL-AT @ ;
TRUSTED: BYTE-AT ( n -- n ) USIGS swap + c@ ;
TRUSTED: REC-BYTES ( -- n ) EFF-REC ;
TRUSTED: NODE-BYTES ( -- n ) EFF-NODE ;
TRUSTED: R-DIN ( -- n ) ER-DIN-OFF ;
TRUSTED: R-DOUT ( -- n ) ER-DOUT-OFF ;
TRUSTED: R-RIN ( -- n ) ER-RIN-OFF ;
TRUSTED: R-ROUT ( -- n ) ER-ROUT-OFF ;
TRUSTED: R-HASR ( -- n ) ER-HASR-OFF ;
TRUSTED: R-SYMPREV ( -- n ) ER-SYMPREV-OFF ;
TRUSTED: N-TAG ( -- n ) EN-TAG-OFF ;
TRUSTED: N-A ( -- n ) EN-A-OFF ;
TRUSTED: N-B ( -- n ) EN-B-OFF ;
TRUSTED: N-C ( -- n ) EN-C-OFF ;
TRUSTED: N-D ( -- n ) EN-D-OFF ;
TRUSTED: N-E ( -- n ) EN-E-OFF ;
TRUSTED: N-F ( -- n ) EN-F-OFF ;
TRUSTED: N-G ( -- n ) EN-G-OFF ;
TRUSTED: N-H ( -- n ) EN-H-OFF ;
TRUSTED: T-PTR ( -- n ) EN-PTR ;
TRUSTED: T-PUSH ( -- n ) EN-PUSH ;
TRUSTED: T-QUOT ( -- n ) EN-QUOT ;
TRUSTED: T-ATOM ( -- n ) EN-ATOM ;
TRUSTED: T-PARAM ( -- n ) EN-PARAM ;

\ ---- the counters the walk fills ----------------------------------------------
variable WINDOW-V   variable RECS-V     variable SHADOW-V
variable NODES-V    variable NODEB-V    variable SHARES-V   variable SHAREB-V
variable FINAL-V    variable DUP-V      variable BELOW-V    variable SHAPES-V

\ ---- the visited set: one byte per eight-byte granule of the window -----------
\ Bit 0 marks a node the walk has already charged to a record; bit 1 marks a
\ record another record's symbol chain shadows. Two bits rather than two arrays
\ because a record offset and a node offset share one address space.
1 constant SEEN-BIT
2 constant SHADOW-BIT
PTR-VARIABLE VIS-P
variable BASE-V     variable CUR-V

: GRANULE ( n -- n ) 3 rshift ;

: VIS@ ( n -- n ) GRANULE VIS-P @ swap + c@ ;

: VIS+ ( n n -- ) {: off:n b:n :}
   off VIS@ b or  VIS-P @ off GRANULE +  c! ;

: SEEN? ( n -- bool ) VIS@ SEEN-BIT and 0 <> ;
: SEE ( n -- ) SEEN-BIT VIS+ ;
: SHADOWED? ( n -- bool ) VIS@ SHADOW-BIT and 0 <> ;
: SHADOW ( n -- ) SHADOW-BIT VIS+ ;

\ ---- the shape table: this file's own canonical-shape counter ------------------
\ Keys are 64-bit content hashes folded bottom-up, so two entries collide only by
\ hash accident; the count is a MEASUREMENT and never decides what the store
\ does, which is why a hash key is honest here and would not be in the interner.
$CBF29CE484222325 constant FNV-BASIS
$100000001B3 constant FNV-PRIME
PTR-VARIABLE SHT-P
variable SHT-CAP-V  variable SHT-I  variable H-V

: H0 ( -- ) FNV-BASIS H-V ! ;
: H+ ( n -- ) H-V @ xor FNV-PRIME * H-V ! ;
: H@ ( -- n ) H-V @ ;

: SHT-SLOT ( n -- ptr a ) cells SHT-P @ + ;

: SHT+ ( n -- ) {: k:n :}
   k SHT-CAP-V @ 1 - and SHT-I !
   BEGIN SHT-I @ SHT-SLOT @ 0 <> WHILE
      SHT-I @ SHT-SLOT @ k = IF EXIT THEN
      SHT-I @ 1 + SHT-CAP-V @ 1 - and SHT-I !
   REPEAT
   k SHT-I @ SHT-SLOT !
   SHAPES-V @ 1 + SHAPES-V ! ;

\ ---- allocation: sized from the window, so nothing is silently truncated -------
: POW2-AT-LEAST ( n -- n ) {: need:n :}
   1 BEGIN dup need < WHILE 2 * REPEAT ;

: ALLOC-VIS ( n -- ) {: bytes:n :}
   bytes MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop VIS-P !
   0 BEGIN dup bytes < WHILE
      0 VIS-P @ over + c!
      1 +
   REPEAT drop ;

: ALLOC-SHT ( n -- ) {: cnt:n :}
   cnt MEM:CELLS-ALLOC-COUNT MEM:ALLOC-CELLS SHT-P !
   cnt SHT-CAP-V !
   0 BEGIN dup cnt < WHILE
      0 over SHT-SLOT !
      1 +
   REPEAT drop ;

\ ---- byte accounting ----------------------------------------------------------
: ALIGN8 ( n -- n ) 7 + $FFFFFFFFFFFFFFF8 and ;

variable DUPCUR
: CHARGE ( n -- ) {: b:n :}
   DUPCUR @ 0 <> IF b DUP-V @ + DUP-V ! EXIT THEN
   b FINAL-V @ + FINAL-V ! ;

: TAKE ( n -- ) {: b:n :}
   b NODEB-V @ + NODEB-V !
   b CHARGE ;

: TAG-AT ( n -- n ) N-TAG + CELL-AT ;
: FIELD ( n n -- n ) + CELL-AT ;
: ARG-AT ( n n -- n ) {: p:n i:n :}   \ the i-th arg offset of an EN-PARAM node
   p N-D FIELD i cells + CELL-AT ;

\ WALK ( n -- ) : charge the subterm at `off` to the record being visited, once.
\ A node below the window belongs to an earlier load and is counted as a
\ reference, never as bytes - counting it would make the window's own arithmetic
\ come out negative, which is how the sharing case first announced itself.
: WALK ( n -- ) {: off:n :}
   off 0= IF EXIT THEN
   off BASE-V @ < IF BELOW-V @ 1 + BELOW-V ! EXIT THEN
   off SEEN? IF
      SHARES-V @ 1 + SHARES-V !
      NODE-BYTES SHAREB-V @ + SHAREB-V !
      EXIT
   THEN
   off SEE
   NODES-V @ 1 + NODES-V !
   NODE-BYTES TAKE
   off TAG-AT {: tg:n :}
   tg T-PTR = IF off N-A FIELD RECURSE EXIT THEN
   tg T-PUSH = IF off N-A FIELD RECURSE  off N-B FIELD RECURSE EXIT THEN
   tg T-QUOT = IF
      off N-A FIELD RECURSE  off N-B FIELD RECURSE
      off N-C FIELD RECURSE  off N-D FIELD RECURSE EXIT THEN
   tg T-ATOM = IF off N-B FIELD ALIGN8 TAKE EXIT THEN
   tg T-PARAM = IF
      off N-B FIELD ALIGN8 TAKE
      off N-C FIELD {: argc:n :}
      argc cells TAKE
      0 BEGIN dup argc < WHILE
         off over ARG-AT RECURSE
         1 +
      REPEAT drop
   THEN ;

\ ---- the canonical shape of a subterm -----------------------------------------
: STR-HASH ( n n -- n ) {: a:n u:n :}
   H0
   0 BEGIN dup u < WHILE
      dup a + BYTE-AT H+
      1 +
   REPEAT drop
   H@ ;

: SHAPE ( n -- n ) {: off:n :}
   off 0= IF 0 EXIT THEN
   off TAG-AT {: tg:n :}
   tg T-PTR = IF
      off N-A FIELD RECURSE {: ca:n :}
      H0 tg H+ ca H+ H@ dup SHT+ EXIT THEN
   tg T-PUSH = IF
      off N-A FIELD RECURSE {: ca:n :}
      off N-B FIELD RECURSE {: cb:n :}
      H0 tg H+ ca H+ cb H+ off N-C FIELD H+ H@ dup SHT+ EXIT THEN
   tg T-QUOT = IF
      off N-A FIELD RECURSE {: qa:n :}
      off N-B FIELD RECURSE {: qb:n :}
      off N-C FIELD RECURSE {: qc:n :}
      off N-D FIELD RECURSE {: qd:n :}
      H0 tg H+ qa H+ qb H+ qc H+ qd H+
      off N-E FIELD H+ off N-F FIELD H+ off N-G FIELD H+ off N-H FIELD H+
      H@ dup SHT+ EXIT THEN
   tg T-ATOM = IF
      off N-A FIELD off N-B FIELD STR-HASH {: sh:n :}
      H0 tg H+ sh H+ off N-B FIELD H+ off N-C FIELD H+ H@ dup SHT+ EXIT THEN
   \ The running fold is parked on the RETURN stack across each argument, never in
   \ a variable: the recursion below re-enters this word and would overwrite a
   \ shared accumulator, which is how the count first came out ABOVE the node
   \ count - one node answering with two different shapes.
   tg T-PARAM = IF
      off N-A FIELD off N-B FIELD STR-HASH {: ph:n :}
      off N-C FIELD {: argc:n :}
      H0 tg H+ ph H+ off N-B FIELD H+ argc H+
      off N-E FIELD H+ off N-H FIELD H+
      H@ >r
      0 BEGIN dup argc < WHILE
         off over ARG-AT RECURSE
         r> H-V ! H+ H@ >r
         1 +
      REPEAT drop
      r> dup SHT+ EXIT THEN
   H0 tg H+ off N-A FIELD H+ off N-B FIELD H+ H@ dup SHT+ ;

\ ---- the two passes -----------------------------------------------------------
\ Pass one marks every record another record shadows. A record's ER.SYMPREV is
\ the previous record with the same symbol, so a record named by any successor's
\ back-link is not the newest one for its symbol - which is exactly what newest-
\ wins means, asked of the store's own links rather than of a symbol table.
: MARK-SHADOWED ( -- )
   BASE-V @ CUR-V !
   BEGIN CUR-V @ CELL-AT 0 <> WHILE
      CUR-V @ R-SYMPREV FIELD {: p:n :}
      p 0 <> IF p 1 - BASE-V @ >= IF p 1 - SHADOW THEN THEN
      CUR-V @ CELL-AT CUR-V !
   REPEAT ;

: VISIT-ROWS ( n -- ) {: rec:n :}
   rec R-DIN FIELD WALK    rec R-DIN FIELD SHAPE drop
   rec R-DOUT FIELD WALK   rec R-DOUT FIELD SHAPE drop
   rec R-HASR FIELD 0 <> IF
      rec R-RIN FIELD WALK    rec R-RIN FIELD SHAPE drop
      rec R-ROUT FIELD WALK   rec R-ROUT FIELD SHAPE drop
   THEN ;

: VISIT-RECORDS ( -- )
   BASE-V @ CUR-V !
   BEGIN CUR-V @ CELL-AT 0 <> WHILE
      RECS-V @ 1 + RECS-V !
      CUR-V @ SHADOWED? IF
         -1 DUPCUR !  SHADOW-V @ 1 + SHADOW-V !
      ELSE 0 DUPCUR ! THEN
      REC-BYTES CHARGE
      CUR-V @ VISIT-ROWS
      CUR-V @ CELL-AT CUR-V !
   REPEAT ;

: RESET ( -- )
   0 RECS-V !   0 SHADOW-V !  0 NODES-V !  0 NODEB-V !
   0 SHARES-V ! 0 SHAREB-V !  0 FINAL-V !  0 DUP-V !
   0 BELOW-V !  0 SHAPES-V !  0 WINDOW-V ! ;

public

\ MARK ( -- n ) : the store end to census from. Take it before the load whose
\ cost is the question; everything appended after it is the window.
: MARK ( -- n ) STORE-END ;

: RUN ( n -- ) {: base:n :}
   RESET
   base BASE-V !
   STORE-END base - WINDOW-V !
   STORE-END GRANULE 1 + ALLOC-VIS
   WINDOW-V @ NODE-BYTES / 4 * 64 max POW2-AT-LEAST ALLOC-SHT
   MARK-SHADOWED
   VISIT-RECORDS ;

: WINDOW-BYTES ( -- n ) WINDOW-V @ ;
: RECORDS ( -- n ) RECS-V @ ;
: SHADOWED ( -- n ) SHADOW-V @ ;
: HEADER-BYTES ( -- n ) RECS-V @ REC-BYTES * ;
: NODES ( -- n ) NODES-V @ ;
: NODE-TOTAL-BYTES ( -- n ) NODEB-V @ ;
: SHARES ( -- n ) SHARES-V @ ;
: SHARE-BYTES ( -- n ) SHAREB-V @ ;
: BELOW-WINDOW ( -- n ) BELOW-V @ ;
: FINAL-BYTES ( -- n ) FINAL-V @ ;
: DUP-BYTES ( -- n ) DUP-V @ ;
: SHAPES ( -- n ) SHAPES-V @ ;

\ ORPHAN-BYTES ( -- n ) : the window minus everything the walk accounted for.
\ Zero is the instrument's own proof that it saw the store exactly once; a
\ non-zero answer means the walk and the arena disagree and no other number in
\ the table can be believed.
: ORPHAN-BYTES ( -- n )
   WINDOW-V @ FINAL-V @ - DUP-V @ - ;

: REPORT ( -- )
   s" effect-store-census" type cr
   s" window-bytes " type WINDOW-BYTES . cr
   s" records " type RECORDS . cr
   s" shadowed-records " type SHADOWED . cr
   s" header-bytes " type HEADER-BYTES . cr
   s" nodes " type NODES . cr
   s" node-bytes " type NODE-TOTAL-BYTES . cr
   s" shapes " type SHAPES . cr
   s" shares " type SHARES . cr
   s" share-bytes " type SHARE-BYTES . cr
   s" below-window-refs " type BELOW-WINDOW . cr
   s" final-bytes " type FINAL-BYTES . cr
   s" dup-bytes " type DUP-BYTES . cr
   s" orphan-bytes " type ORPHAN-BYTES . cr ;

;package
