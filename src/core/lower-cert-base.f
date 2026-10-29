\ lower-cert-base.f — boot-safe lowering-certificate ABI and dispatcher, loaded
\ right after checker.f. Until the full producer installs (layout-valid.f), only
\ definitions with no lowering facts may publish a source-bound empty cert.

package LOWER-CERT

$4841425543455254 constant MAGIC-V
2 constant VERSION-V
10 constant HEADER-N
0 constant MAGIC-I
1 constant VERSION-I
2 constant TOTAL-BYTES-I
3 constant NEEDS-I
\ The NEEDS cell: what lowering needs beyond the plain body. REPLAY: the width
\ facts must be replayed (layout-valid.f NEEDS-P2). INCOMPLETE: an unjudged
\ scan lost a width fact past a rejected token (checker.f SCAN-INCOMPLETE), so
\ the facts cannot be lowered from and tier 0 refuses the certificate.
0 constant NEEDS-NONE
1 constant NEEDS-REPLAY
2 constant NEEDS-INCOMPLETE
4 constant WF-COUNT-I
5 constant BIND-COUNT-I
6 constant FETCH-COUNT-I
7 constant FETCH-DATA-CELLS-I
8 constant BODY-LEN-I
9 constant BODY-HASH-I
\ Width rows are {source-byte-offset, operand-position, width, flags}; fetch
\ rows are {source-byte-offset, absolute descriptor-cell offset, cell count}.
\ Each descriptor starts with a check count; checks are {offset,limit,guards}
\ followed by guard triples {offset,tag,limit}.
4 constant WF-NCELLS
3 constant FETCH-NCELLS
3 constant CHECK-NCELLS
3 constant GUARD-NCELLS
1 constant FETCH-BIT
2 constant STORE-BIT
4 constant XPAD-BIT      \ construct/MATCH extra-pad fact flag
$CBF29CE484222325 constant FNV-OFFSET-V
$100000001B3 constant FNV-PRIME-V

$400 constant BUF-INIT
$0FFFFFFFFFFFFFFF constant MAX-CELLS

create BUF-BOOT BUF-INIT cells allot
\ PERSISTED: the slot holds a data address across a capture, and only marked
\ cells are rebased when the window's DATA moves to the image's base.
\ BUF-ENSURE also grows into process-local mmap, which BUF-SNAP-RESET undoes.
PERSISTED-PTR-VARIABLE BUF-P   BUF-BOOT BUF-P !
variable BUF-CAP BUF-INIT BUF-CAP !
variable BUF-N
\ The kept buffer holds a does> clause's certificate while its definer's own
\ body is checked into BUF (KEEP): the lowering transaction freezes both.
\ PERSISTED for BUF-P's reason, and reset with it by BUF-SNAP-RESET.
create KEPT-BOOT BUF-INIT cells allot
PERSISTED-PTR-VARIABLE KEPT-P   KEPT-BOOT KEPT-P !
variable KEPT-CAP BUF-INIT KEPT-CAP !
variable KEPT-N
variable BODY-LEN
variable BODY-HASH

\ FULL-XT is a defer, as CHECKER-CERT:PRODUCER-XT is: the cell holds an
\ execution token persisted in the DP heap, and only `defer`/`is` declare such a
\ cell to the snapshot address-cell table.
defer FULL-XT ( ptr u8 n n -- )

\ DISPATCH must ask whether the full producer is installed, and a dispatch cell
\ cannot be asked, so the grant is an integer flag, also the single-assignment
\ guard below.
variable FULL-SET   0 FULL-SET !

: BUF ( -- ptr n ) BUF-P @ ;

: GROW-CAP ( n n -- n ) {: need:n cap:n :}
   need 0 <= need MAX-CELLS > or if s" lowering certificate capacity overflow" 76 die then
   cap MAX-CELLS 2 / <= if cap 2 * need max exit then
   need ;

: HASH ( ptr u8 n -- n ) {: a:ptr u:n :}
   FNV-OFFSET-V
   0 begin dup u < while
      a over + c@ rot xor FNV-PRIME-V * swap
      1 +
   repeat drop ;

: BUF-ENSURE ( n -- ) {: need:n :}
   need BUF-CAP @ <= if exit then
   need BUF-CAP @ GROW-CAP {: cap:n :}
   BUF-P @ BUF-CAP @ cells cap cells ARENA-BYTES-GROW BUF-P !
   cap BUF-CAP ! ;

\ Put the certificate buffer back on its boot buffer for the capture; MAKE
\ re-emits the whole certificate per definition, so nothing live is lost.
\ layout-valid.f SNAP-RESET calls this.
: BUF-SNAP-RESET ( -- )
   BUF-BOOT BUF-P !   BUF-INIT BUF-CAP !
   BUF-BOOT 0 BUF-INIT ARENA-CELLS-ZERO
   KEPT-BOOT KEPT-P !   BUF-INIT KEPT-CAP !
   KEPT-BOOT 0 BUF-INIT ARENA-CELLS-ZERO
   0 BUF-N ! 0 KEPT-N ! 0 BODY-LEN ! 0 BODY-HASH ! ;

: BUF, ( n -- )
   BUF-N @ 1 + BUF-ENSURE
   BUF-N @ cells BUF + !
   BUF-N @ 1 + BUF-N ! ;

: SOURCE! ( ptr u8 n -- ) {: a:ptr u:n :}
   u BODY-LEN !
   a u HASH BODY-HASH ! ;

: EMPTY ( ptr u8 n -- )
   SOURCE!
   0 BUF-N !
   HEADER-N BUF-ENSURE
   MAGIC-V BUF,
   VERSION-V BUF,
   HEADER-N cells BUF,
   SCAN-INCOMPLETE @ 0 <> if NEEDS-INCOMPLETE else NEEDS-NONE then BUF,
   0 BUF,  0 BUF,  0 BUF,  0 BUF,
   BODY-LEN @ BUF,
   BODY-HASH @ BUF, ;

: CERT-BYTES ( -- ptr u8 n ) BUF BYTE-VIEW BUF-N @ cells ;

: CERT-CELL@ ( n -- n ) {: idx:n :}
   idx 0 < idx BUF-N @ >= or if s" lowering certificate cell index" 76 die then
   idx cells BUF + @ ;

: FULL-INSTALL ( [ ptr u8 n n -- ] -- )
   FULL-SET @ 0 <> if s" lowering certificate full producer already installed" 76 die then
   is FULL-XT
   1 FULL-SET ! ;

: DISPATCH ( ptr u8 n n -- ) {: a:ptr u:n verdict:n :}
   FULL-SET @ 0 <> if a u verdict FULL-XT exit then
   WF-N@ 0 <> if s" lowering certificate facts before full producer" 76 die then
   a u EMPTY ;

\ One-shot install seam. A quotation is a compile-time construct, so the
\ producer is handed over from inside a definition rather than from the top
\ level; the seal retires this word with the rest of the install surface.
: DISPATCH-INSTALL ( -- ) [: DISPATCH ;] CHECKER-CERT:INSTALL ;

public

: MAGIC ( -- n ) MAGIC-V ;
: VERSION ( -- n ) VERSION-V ;
: HEADER-CELLS ( -- n ) HEADER-N ;
: MAGIC-CELL ( -- n ) MAGIC-I ;
: VERSION-CELL ( -- n ) VERSION-I ;
: TOTAL-BYTES-CELL ( -- n ) TOTAL-BYTES-I ;
: NEEDS-CELL ( -- n ) NEEDS-I ;
: INCOMPLETE-NEEDS ( -- n ) NEEDS-INCOMPLETE ;
: WF-COUNT-CELL ( -- n ) WF-COUNT-I ;
: BIND-COUNT-CELL ( -- n ) BIND-COUNT-I ;
: FETCH-COUNT-CELL ( -- n ) FETCH-COUNT-I ;
: FETCH-DATA-CELLS-CELL ( -- n ) FETCH-DATA-CELLS-I ;
: WF-CELLS ( -- n ) WF-NCELLS ;
: FETCH-CELLS ( -- n ) FETCH-NCELLS ;
: CHECK-CELLS ( -- n ) CHECK-NCELLS ;
: GUARD-CELLS ( -- n ) GUARD-NCELLS ;
: FETCH-FLAG ( -- n ) FETCH-BIT ;
: STORE-FLAG ( -- n ) STORE-BIT ;
: XPAD-FLAG ( -- n ) XPAD-BIT ;
: BODY-LEN-CELL ( -- n ) BODY-LEN-I ;
: BODY-HASH-CELL ( -- n ) BODY-HASH-I ;
: FNV-OFFSET ( -- n ) FNV-OFFSET-V ;
: FNV-PRIME ( -- n ) FNV-PRIME-V ;
: CELL-COUNT ( -- n ) BUF-N @ ;
: CELL@ ( n -- n ) CERT-CELL@ ;
: BYTES ( -- ptr u8 n ) CERT-BYTES ;

\ Keep the certificate just produced and give the producer the other buffer.
\ BUF-N goes to 0, so BYTES read before the next production is an empty,
\ invalid certificate rather than the kept one.
: KEEP ( -- )
   BUF-P @ KEPT-P @ BUF-P ! KEPT-P !
   BUF-CAP @ KEPT-CAP @ BUF-CAP ! KEPT-CAP !
   BUF-N @ KEPT-N !  0 BUF-N ! ;

: KEPT ( -- ptr u8 n ) KEPT-P @ BYTE-VIEW KEPT-N @ cells ;

\ Resolve only the certificate the caller's source produced. Its returned header
\ also carries the byte length, checked by consumers before copying any facts.
: FOR-HASH ( n -- ptr u8 n )
   BODY-HASH @ <> if s" lowering certificate source hash mismatch" 76 die then
   CERT-BYTES ;

DISPATCH-INSTALL

;package

package CHECKER-REG
' LOWER-CERT:FOR-HASH DECLARATIONS CHECKER-FETCH-ABI:CERTIFICATE-OFF + xt!
' LOWER-CERT:BYTES DECLARATIONS CHECKER-OWNER-ABI:CERT-BYTES-OFF + xt!
' LOWER-CERT:KEEP DECLARATIONS CHECKER-OWNER-ABI:CERT-KEEP-OFF + xt!
' LOWER-CERT:KEPT DECLARATIONS CHECKER-OWNER-ABI:CERT-KEPT-OFF + xt!
;package
