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
\ the accounting identity it publishes - window = bindings + contents + node
\ bytes + dead, orphan zero - is what proves the walk saw everything exactly
\ once. DEAD is what a capture's sweep zeroed in place: the bytes of a retired
\ binding that nothing kept still reaches (src/core/checker.f CHECKER-SWEEP).
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
\ independent rather than an echo: over the WHOLE store NODES and SHAPES must
\ come out equal, because a second copy of a shape is precisely what the interner
\ exists to prevent. Over a partial window they need not, since a shape the
\ window reuses may live in a node below its base. Before interning, the whole
\ store held 84 nodes for every shape.
\
\ Run it over a load:
\   bin/hb --load tools/effect-store-census-run.f -- src/compiler/native/compiler.f
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
TRUSTED: REC-NEXT ( n -- n ) E-PTR E-NEXT@ ;
TRUSTED: BYTE-AT ( n -- n ) USIGS swap + c@ ;
TRUSTED: REC-BYTES ( -- n ) EFF-REC ;
TRUSTED: NODE-BYTES ( -- n ) EFF-NODE ;
TRUSTED: CONTENT-BYTES ( -- n ) EFF-CONTENT ;
TRUSTED: R-CONTENT ( n -- n ) E-PTR ER.CONTENT @ ;
TRUSTED: R-DIN ( n -- n ) E-PTR E-DIN@ ;
TRUSTED: R-DOUT ( n -- n ) E-PTR E-DOUT@ ;
TRUSTED: R-RIN ( n -- n ) E-PTR E-RIN@ ;
TRUSTED: R-ROUT ( n -- n ) E-PTR E-ROUT@ ;
TRUSTED: R-HASR ( n -- n ) E-PTR E-HASR@ ;
TRUSTED: R-SYMPREV ( -- n ) ER-SYMPREV-OFF ;
TRUSTED: R-SYM ( n -- n ) E-PTR ER.SYM @ ;
TRUSTED: SYM-GONE? ( n -- bool ) SYM-RETIRED? ;
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
TRUSTED: T-VAR ( -- n ) EN-VAR ;
TRUSTED: T-ROW ( -- n ) EN-ROW ;

\ ---- the counters the walk fills ----------------------------------------------
variable WINDOW-V   variable RECS-V     variable SHADOW-V
variable CONTENTS-V
variable NODES-V    variable NODEB-V    variable SHARES-V   variable SHAREB-V
variable FINAL-V    variable DUP-V      variable BELOW-V    variable SHAPES-V
variable UNKEYED-V  variable RETIRED-V  variable DEAD-V

\ ---- the visited set: one byte per eight-byte granule of the store ------------
\ Bit 0 marks a node the walk has already charged to a record; bit 1 marks a
\ record another record's symbol chain shadows; bit 2 marks a granule some
\ charged extent covers, which is what tells a zeroed granule nothing reaches
\ from one the walk accounted for. Three bits rather than three arrays because a
\ record offset and a node offset share one address space.
1 constant SEEN-BIT
2 constant SHADOW-BIT
4 constant COVER-BIT
PTR-VARIABLE VIS-P
variable VIS-N-V                         \ the map's length in granules
variable BASE-V     variable CUR-V

: GRANULE ( n -- n ) 3 rshift ;

: VIS@ ( n -- n ) GRANULE VIS-P @ swap + c@ ;

: VIS+ ( n n -- ) {: off:n b:n :}
   off VIS@ b or  VIS-P @ off GRANULE +  c! ;

: SEEN? ( n -- bool ) VIS@ SEEN-BIT and 0 <> ;
: SEE ( n -- ) SEEN-BIT VIS+ ;
: SHADOWED? ( n -- bool ) VIS@ SHADOW-BIT and 0 <> ;
: SHADOW ( n -- ) SHADOW-BIT VIS+ ;
: COVERED? ( n -- bool ) VIS@ COVER-BIT and 0 <> ;

\ COVER ( n n -- ) : the granules of the extent [off, off+bytes) are accounted
\ for. Clamped to the map, so a field that names bytes past the store end cannot
\ write past it: those bytes are charged but lie outside the window, and the
\ orphan count goes negative, which is the report such a store deserves.
: COVER ( n n -- ) {: off:n bytes:n :}
   off GRANULE 0 max
   BEGIN dup off bytes + 7 + GRANULE VIS-N-V @ min < WHILE
      VIS-P @ over + dup c@ COVER-BIT or swap c!
      1 +
   REPEAT drop ;

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

: SHT-SLOT ( n -- ptr n ) cells SHT-P @ + ;

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
   bytes VIS-N-V !
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
: CHARGE ( n n -- ) {: off:n b:n :}
   off b COVER
   DUPCUR @ 0 <> IF b DUP-V @ + DUP-V ! EXIT THEN
   b FINAL-V @ + FINAL-V ! ;

: TAKE ( n n -- ) {: off:n b:n :}
   b NODEB-V @ + NODEB-V !
   off b CHARGE ;

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
   off NODE-BYTES TAKE
   off TAG-AT {: tg:n :}
   tg T-PTR = IF off N-A FIELD RECURSE EXIT THEN
   tg T-PUSH = IF off N-A FIELD RECURSE  off N-B FIELD RECURSE EXIT THEN
   tg T-QUOT = IF
      off N-A FIELD RECURSE  off N-B FIELD RECURSE
      off N-C FIELD RECURSE  off N-D FIELD RECURSE EXIT THEN
   tg T-ATOM = IF off N-A FIELD  off N-B FIELD ALIGN8 TAKE EXIT THEN
   tg T-PARAM = IF
      off N-A FIELD  off N-B FIELD ALIGN8 TAKE
      off N-C FIELD {: argc:n :}
      off N-D FIELD  argc cells TAKE
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
   H0 tg H+ off N-A FIELD H+ off N-B FIELD H+
   \ The storage restriction is independent of the variable's ordinary kind.
   tg T-VAR = tg T-ROW = or IF off N-C FIELD H+ THEN
   H@ dup SHT+ ;

\ ---- the two passes -----------------------------------------------------------
\ Pass one marks every record another record shadows. A record's ER.SYMPREV is
\ the previous record with the same symbol, so a record named by any successor's
\ back-link is not the newest one for its symbol - which is exactly what newest-
\ wins means, asked of the store's own links rather than of a symbol table.
: MARK-SHADOWED ( -- )
   BASE-V @ CUR-V !
   BEGIN CUR-V @ REC-NEXT 0 <> WHILE
      CUR-V @ R-SYMPREV FIELD {: p:n :}
      p 0 <> IF p 1 - BASE-V @ >= IF p 1 - SHADOW THEN THEN
      CUR-V @ REC-NEXT CUR-V !
   REPEAT ;

: VISIT-ROWS ( n -- ) {: rec:n :}
   rec R-DIN WALK    rec R-DIN SHAPE drop
   rec R-DOUT WALK   rec R-DOUT SHAPE drop
   rec R-HASR 0 <> IF
      rec R-RIN WALK    rec R-RIN SHAPE drop
      rec R-ROUT WALK   rec R-ROUT SHAPE drop
   THEN ;

: VISIT-CONTENT ( n -- ) R-CONTENT
   dup BASE-V @ < IF drop EXIT THEN
   dup SEEN? IF drop EXIT THEN
   dup SEE  CONTENT-BYTES CHARGE
   1 CONTENTS-V +! ;

\ A binding keyed on no symbol is a retired one the capture's sweep reduced to
\ its chain link (src/core/checker.f CHECKER-SWEEP), or an anonymous record a
\ root still names, such as a definer's created effect. One still keyed on a
\ retired symbol is a binding the sweep missed. No reader reaches any of them
\ through a name.
: KEY ( n -- ) {: rec:n :}
   rec R-SYM {: sym:n :}
   sym 0= IF 1 UNKEYED-V +! EXIT THEN
   sym SYM-GONE? IF 1 RETIRED-V +! THEN ;

\ A record whose ER.CONTENT is 0 has no content and no rows: the sweep left it
\ only the link to the next record, so there is nothing below it to visit.
: VISIT-RECORDS ( -- )
   BASE-V @ CUR-V !
   BEGIN CUR-V @ REC-NEXT 0 <> WHILE
      RECS-V @ 1 + RECS-V !
      CUR-V @ KEY
      CUR-V @ SHADOWED? IF
         -1 DUPCUR !  SHADOW-V @ 1 + SHADOW-V !
      ELSE 0 DUPCUR ! THEN
      CUR-V @ REC-BYTES CHARGE
      CUR-V @ R-CONTENT 0 <> IF
         CUR-V @ VISIT-CONTENT
         CUR-V @ VISIT-ROWS
      THEN
      CUR-V @ REC-NEXT CUR-V !
   REPEAT ;

\ A granule no charged extent covers and that holds zero is one the sweep
\ zeroed. One that holds anything else is an orphan, and stays one.
: COUNT-DEAD ( -- )
   BASE-V @ BEGIN dup 8 + STORE-END <= WHILE
      dup COVERED? 0= IF
         dup CELL-AT 0= IF DEAD-V @ 8 + DEAD-V ! THEN
      THEN
      8 +
   REPEAT drop ;

: RESET ( -- )
   0 CONTENTS-V !
   0 RECS-V !   0 SHADOW-V !  0 NODES-V !  0 NODEB-V !
   0 SHARES-V ! 0 SHAREB-V !  0 FINAL-V !  0 DUP-V !
   0 BELOW-V !  0 SHAPES-V !  0 WINDOW-V !
   0 UNKEYED-V !  0 RETIRED-V !  0 DEAD-V ! ;

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
   VISIT-RECORDS
   COUNT-DEAD ;

: WINDOW-BYTES ( -- n ) WINDOW-V @ ;
: RECORDS ( -- n ) RECS-V @ ;
: SHADOWED ( -- n ) SHADOW-V @ ;
: HEADER-BYTES ( -- n ) RECS-V @ REC-BYTES * ;
: CONTENTS ( -- n ) CONTENTS-V @ ;
: CONTENT-TOTAL-BYTES ( -- n ) CONTENTS-V @ CONTENT-BYTES * ;
: NODES ( -- n ) NODES-V @ ;
: NODE-TOTAL-BYTES ( -- n ) NODEB-V @ ;
: SHARES ( -- n ) SHARES-V @ ;
: SHARE-BYTES ( -- n ) SHAREB-V @ ;
: BELOW-WINDOW ( -- n ) BELOW-V @ ;
: FINAL-BYTES ( -- n ) FINAL-V @ ;
: DUP-BYTES ( -- n ) DUP-V @ ;
: SHAPES ( -- n ) SHAPES-V @ ;
: UNKEYED ( -- n ) UNKEYED-V @ ;
: RETIRED-KEYED ( -- n ) RETIRED-V @ ;
: DEAD-BYTES ( -- n ) DEAD-V @ ;

\ ORPHAN-BYTES ( -- n ) : the window minus everything the walk accounted for.
\ Zero is the instrument's own proof that it saw the store exactly once; a
\ non-zero answer means the walk and the arena disagree and no other number in
\ the table can be believed.
: ORPHAN-BYTES ( -- n )
   WINDOW-V @ FINAL-V @ - DUP-V @ - DEAD-V @ - ;

: REPORT ( -- )
   s" effect-store-census" type cr
   s" window-bytes " type WINDOW-BYTES . cr
   s" records " type RECORDS . cr
   s" shadowed-records " type SHADOWED . cr
   s" header-bytes " type HEADER-BYTES . cr
   s" contents " type CONTENTS . cr
   s" content-bytes " type CONTENT-TOTAL-BYTES . cr
   s" nodes " type NODES . cr
   s" node-bytes " type NODE-TOTAL-BYTES . cr
   s" shapes " type SHAPES . cr
   s" shares " type SHARES . cr
   s" share-bytes " type SHARE-BYTES . cr
   s" below-window-refs " type BELOW-WINDOW . cr
   s" final-bytes " type FINAL-BYTES . cr
   s" dup-bytes " type DUP-BYTES . cr
   s" dead-bytes " type DEAD-BYTES . cr
   s" orphan-bytes " type ORPHAN-BYTES . cr
   s" unkeyed-bindings " type UNKEYED . cr
   s" retired-symbol-bindings " type RETIRED-KEYED . cr ;

;package
