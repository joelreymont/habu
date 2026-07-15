\ effect.f - the canonical CAD effect-row algebra over the sealed immutable
\ nominal collection (MODEL-CAD-V2-PLAN.md R8; dot habu-define-finite-cad-0bdf52ad).
\
\ A CAD effect ROW is a canonical, sorted-unique set of semantic bindings held as
\ an opaque one-cell handle - there is NO wide by-value PRODUCT and NO small
\ composition bound. Each binding names one finite effect atom, one slot kind, one
\ slot index, and a (possibly empty) stable site path. This file OWNS only the
\ static vocabulary encoding and the row algebra; immutable storage, interning,
\ canonical digest, wire codec, and the transactional builder belong to
\ lib/nominal/ (dot habu-add-immutable-nominal-9290a81f) and are CONSUMED through
\ its public package NOM API, never reimplemented.
\
\ The public row type is NOM's own `NOM:row`. lib/nominal declares its handle
\ families (`NOM:path` / `NOM:binding` / `NOM:row`) in package NOM's PUBLIC
\ section (dot habu-export-public-nom-20170121), so this file names `NOM:row`
\ directly in every signature that carries a published row and consumes/produces
\ it through NOM's public API with no branding cast. There is no separate
\ effect-row brand and no row<->effect-row bridge: a CAD effect row IS a NOM row
\ whose bindings encode the effect vocabulary below, and VALIDATE / CE-SCAN-ROW
\ enforce that encoding whenever a row crosses back in from the wire.
\
\ Encoding. A binding's identity key is (atom, site-path, slot-kind, slot-index).
\ It is projected into NOM's (path, slot) map so equal keys collapse and any
\ difference stays distinct: the path is, root-first,
\   [ site segment ... ]  atom  slot-kind  slot-index
\ and the NOM slot is the fixed sentinel CE-SLOT. Every segment is self-describing
\ - its top byte is a tag (SITE / ATOM / KIND / INDEX), its low 56 bits the value
\ - so a foreign or corrupted path fails classification closed. A LOCAL EMIT builds
\ the three-segment leaf with an empty site path; REMAP prefixes one tagged
\ caller/call-site segment at the root of every path (via NOM:REMAP), preserving
\ the leaf atom/kind/index exactly, so two callees that both bind local slot zero
\ stay distinct once remapped into different sites while replaying the same remap
\ is idempotent. Because the whole key lives in the path and the slot is constant,
\ two bindings with different atoms may share a site/slot (state-write + atomic),
\ one atom may bind many slots (weight + bias parameter reads), and no runtime
\ digest, address, generation, sequence, or allocation-order artifact ever enters
\ identity - NOM compares canonical content.
\
\ Direct duplicate insertion (the exact same binding EMITted twice in one build)
\ is a caller error: FREEZE rejects it with E-CADEFF-DUPLICATE (detected as a
\ collapsed count, transactionally - no handle escapes). UNION, by contrast, is
\ the canonical merge where repeated identical entries are idempotent; it inherits
\ NOM's associative, commutative, deterministic, content-interned semantics.
\
\ Classification reads a row back through its PUBLIC canonical wire (NOM:ENCODE)
\ into a 10-bit atom-presence mask, so ROW-BARRIER? / ROW-DUP-OK? / ROW-CACHEABLE?
\ / ROWS-COMMUTE? fold the effect-types.f truth tables over exactly the atoms a
\ row contains, replay-identically for a decoded or restored row. The classifier
\ scratch is a lazily allocated mmap resource boundary (not a semantic capacity on
\ the row); a row too large for it fails closed with a typed diagnostic.
\
\ Load after lib/nominal/snapshot.f and src/cad/effect-types.f.

require lib/memory.f
require lib/nominal/snapshot.f
require src/cad/effect-types.f

package CAD-EFFECT
private

\ ---- self-describing path segment tags (top byte = tag, low 56 bits = value) --
56 constant CE-TAG-SHIFT
$00FFFFFFFFFFFFFF constant CE-VAL-MASK
1 constant CE-SITE-TAG          \ a caller / call-site ordinal prefixed by REMAP
2 constant CE-ATOM-TAG          \ the finite effect atom code (1..9)
3 constant CE-KIND-TAG          \ the slot-kind code (0..3)
4 constant CE-INDEX-TAG         \ the slot index (>= 0)
0 constant CE-SLOT              \ every effect binding's fixed NOM slot; identity is the path
10 constant CE-ATOM-SPAN        \ atom codes occupy 0..9 -> a 10-bit presence mask
$3FC constant CE-EFFECTFUL-MASK \ bits 2..9: the barrier / non-duplicable atoms

: CE-SEG ( n n -- n ) {: tag:n val:n :}   \ pack a tagged path segment
   tag CE-TAG-SHIFT lshift val or ;
: CE-SEG-TAG ( n -- n )   CE-TAG-SHIFT rshift ;
: CE-SEG-VAL ( n -- n )   CE-VAL-MASK and ;

variable CE-INSERTS             \ direct EMIT count in the live transaction (dup detection)

public

\ ---- transactional builder (thin over NOM's linear nom-builder) ----------------
: NEW ( -- nom-builder )
   NOM:NEW  0 CE-INSERTS ! ;

: EMIT ( nom-builder effect-atom slot-kind n -- nom-builder )
   {: a:effect-atom k:slot-kind idx:n :}
   a ATOM>CODE {: acode:n :}
   acode 1 < if NOM:ROLLBACK E-CADEFF-ATOM throw then      \ pure/invalid atom: abort the tx
   idx 0 < idx CE-VAL-MASK > or if NOM:ROLLBACK E-CADEFF-INDEX throw then   \ out-of-range index: abort
   NOM:ROOT                                                \ build ROOT / atom / kind / index
   CE-ATOM-TAG acode CE-SEG NOM:CONS
   CE-KIND-TAG k KIND>CODE CE-SEG NOM:CONS
   CE-INDEX-TAG idx CE-SEG NOM:CONS
   CE-SLOT NOM:BIND
   NOM:ADD
   CE-INSERTS @ 1+ CE-INSERTS ! ;

: FREEZE ( nom-builder -- NOM:row )
   NOM:FREEZE {: r:NOM:row :}
   CE-INSERTS @ {: n:n :}
   0 CE-INSERTS !
   r NOM:SIZE n < if E-CADEFF-DUPLICATE throw then   \ collapsed count == direct duplicate
   r ;

: ROLLBACK ( nom-builder -- )
   0 CE-INSERTS !  NOM:ROLLBACK ;

\ ---- the unique canonical empty row --------------------------------------------
: PURE ( -- NOM:row )   NEW FREEZE ;

\ ---- canonical composition (NOM alias; one body, published as UNION) -----------
EXPORT NOM:UNION                        \ CAD-EFFECT:UNION  ( NOM:row NOM:row -- NOM:row )

: REMAP ( NOM:row n -- NOM:row ) {: s:n :}
   s 0 < s CE-VAL-MASK > or if E-CADEFF-SITE throw then    \ out-of-range site ordinal
   CE-SITE-TAG s CE-SEG  NOM:REMAP ;

\ ---- canonical identity / row serialization (NOM aliases; one body, two names) -
EXPORT NOM:EQUAL?                       \ CAD-EFFECT:EQUAL?  ( NOM:row NOM:row -- bool )
EXPORT NOM:SIZE                         \ CAD-EFFECT:SIZE    ( NOM:row -- n )
EXPORT NOM:KEY                          \ CAD-EFFECT:KEY     ( NOM:row ptr u8 -- )
EXPORT NOM:ENCODE                       \ CAD-EFFECT:ENCODE  ( NOM:row ptr u8 n -- n )
\ SNAPSHOT/RESTORE are store-wide wire ops (not row-handle ops), also NOM aliases.
EXPORT NOM:SNAPSHOT                     \ CAD-EFFECT:SNAPSHOT  ( ptr u8 n -- n )
EXPORT NOM:RESTORE                      \ CAD-EFFECT:RESTORE   ( ptr u8 n -- n )
: RESET ( -- )                          0 CE-INSERTS !  NOM:RESET ;

private

\ ---- classification scratch: a lazily allocated mmap wire buffer ---------------
$400000 constant CE-WIRE-SEED   \ 4 MiB classifier scratch (resource boundary, not row capacity)
PTR-VARIABLE CE-WIRE-P
variable CE-WIRE-CAP
variable CE-WPOS
variable CE-WLEN
variable CE-MASK

: CE-WIRE-ENSURE ( -- )
   CE-WIRE-CAP @ 0= if
      CE-WIRE-SEED MEM-ALLOC-BYTES drop CE-WIRE-P 0 ptr-field !
      CE-WIRE-SEED CE-WIRE-CAP !
   then ;

: CE-BE64@ ( ptr u8 -- n ) {: p:ptr :}   \ read one big-endian cell
   0  8 0 do  8 lshift  p i + c@ or  loop ;

: CE-WCELL@ ( -- n )                     \ next wire cell, bounds-checked, advances cursor
   CE-WPOS @ 8 + CE-WLEN @ > if E-CADEFF-MALFORMED throw then
   CE-WIRE-P 0 ptr-field @ CE-WPOS @ + CE-BE64@
   CE-WPOS @ 8 + CE-WPOS ! ;

: CE-SEG@ ( n -- n ) {: tag:n :}         \ read next cell, require its tag, return the value
   CE-WCELL@ {: cell:n :}
   cell CE-SEG-TAG tag <> if E-CADEFF-MALFORMED throw then
   cell CE-SEG-VAL ;

\ last decoded binding fields (single-binding introspection for focused tests)
variable CE-B-ATOM  variable CE-B-KIND  variable CE-B-INDEX

: CE-SKIP-SITE ( n -- ) {: ssegs:n :}    \ validate + skip the leading site segments
   0 begin dup ssegs < while
      CE-SITE-TAG CE-SEG@ drop
      1+
   repeat drop ;

: CE-SCAN-BINDING ( -- )
   CE-WCELL@ {: d:n :}                    \ path depth
   d 3 < if E-CADEFF-MALFORMED throw then
   d 3 - CE-SKIP-SITE
   CE-ATOM-TAG CE-SEG@ {: acode:n :}
   CE-KIND-TAG CE-SEG@ {: kcode:n :}
   CE-INDEX-TAG CE-SEG@ {: idx:n :}
   CE-WCELL@ CE-SLOT <> if E-CADEFF-MALFORMED throw then    \ canonical slot only
   acode EFFECT-CODE? 0= if E-CADEFF-MALFORMED throw then
   kcode KIND-CODE? 0= if E-CADEFF-MALFORMED throw then
   idx 0 < if E-CADEFF-MALFORMED throw then
   acode CE-B-ATOM !  kcode CE-B-KIND !  idx CE-B-INDEX !
   1 acode lshift CE-MASK @ or CE-MASK ! ;

: CE-SCAN-ROW ( NOM:row -- n )        \ encode + parse; returns the atom-presence mask
   CE-WIRE-ENSURE
   CE-WIRE-P 0 ptr-field @ CE-WIRE-CAP @ NOM:ENCODE CE-WLEN !
   0 CE-WPOS !  0 CE-MASK !
   CE-WCELL@ {: cnt:n :}
   cnt 0 < if E-CADEFF-MALFORMED throw then
   0 begin dup cnt < while  CE-SCAN-BINDING  1+  repeat drop
   CE-MASK @ ;

\ ---- pairwise commute closure over two atom masks (uses the COMMUTE table) -----
variable CE-CMT-A  variable CE-CMT-B  variable CE-CMT-OK

: CE-CMT-PAIR ( n n -- ) {: a:n b:n :}
   1 a lshift CE-CMT-A @ and 0= if exit then     \ a not present -> vacuous
   1 b lshift CE-CMT-B @ and 0= if exit then      \ b not present -> vacuous
   a b COMMUTE-CODE? 0= if 0 CE-CMT-OK ! then ;

: CE-CMT-ROW ( n -- ) {: a:n :}
   0 begin dup CE-ATOM-SPAN < while
      dup a swap CE-CMT-PAIR
      1+
   repeat drop ;

: CE-MASK-COMMUTE? ( n n -- bool ) {: ma:n mb:n :}
   ma CE-CMT-A !  mb CE-CMT-B !  1 CE-CMT-OK !
   0 begin dup CE-ATOM-SPAN < while  dup CE-CMT-ROW  1+  repeat drop
   CE-CMT-OK @ 1 = ;

public

\ ---- effect-row well-formedness + row-level truth tables ----------------------
: VALIDATE ( NOM:row -- NOM:row )  \ reject unless every binding is a valid effect binding
   dup CE-SCAN-ROW drop ;

: DECODE ( ptr u8 n -- NOM:row )   NOM:DECODE VALIDATE ;

: ROW-BARRIER? ( NOM:row -- bool )   \ any effectful atom present
   CE-SCAN-ROW CE-EFFECTFUL-MASK and 0= 0= ;
: ROW-DUP-OK? ( NOM:row -- bool )    \ only duplicable atoms (empty row => true)
   CE-SCAN-ROW CE-EFFECTFUL-MASK and 0= ;
: ROW-CACHEABLE? ( NOM:row -- bool ) \ only cacheable atoms (empty row => true)
   CE-SCAN-ROW CE-EFFECTFUL-MASK and 0= ;
: ROWS-COMMUTE? ( NOM:row NOM:row -- bool )
   CE-SCAN-ROW {: mb:n :}
   CE-SCAN-ROW {: ma:n :}
   ma mb CE-MASK-COMMUTE? ;

;package
