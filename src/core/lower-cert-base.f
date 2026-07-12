\ lower-cert-base.f — boot-safe lowering-certificate ABI and dispatcher.
\ Loaded immediately after checker.f. Until the full producer installs, only
\ definitions with no lowering facts may publish a source-bound empty cert.
\ Header cells: magic, version, total bytes, needs-p2, width-fact count,
\ bind-width count, fetch count, descriptor cells, body bytes, body FNV-1a64.
\ Width rows are {source-byte-offset, operand-position, width, flags}; fetch
\ rows are {source-byte-offset, absolute descriptor-cell offset, cell count}.
\ Each descriptor starts with a check count; checks are {offset,limit,guards}
\ followed by guard triples {offset,tag,limit}.

package LOWER-CERT

$4841425543455254 constant MAGIC-V
2 constant VERSION-V
10 constant HEADER-N
0 constant MAGIC-I
1 constant VERSION-I
2 constant TOTAL-BYTES-I
3 constant NEEDS-I
4 constant WF-COUNT-I
5 constant BIND-COUNT-I
6 constant FETCH-COUNT-I
7 constant FETCH-DATA-CELLS-I
8 constant BODY-LEN-I
9 constant BODY-HASH-I
4 constant WF-NCELLS
3 constant FETCH-NCELLS
3 constant CHECK-NCELLS
3 constant GUARD-NCELLS
1 constant FETCH-BIT
2 constant STORE-BIT
$CBF29CE484222325 constant FNV-OFFSET-V
$100000001B3 constant FNV-PRIME-V

$400 constant BUF-INIT
$0FFFFFFFFFFFFFFF constant MAX-CELLS

create BUF-BOOT BUF-INIT cells allot
PTR-VARIABLE BUF-P   BUF-BOOT BUF-P !
variable BUF-CAP BUF-INIT BUF-CAP !
variable BUF-N
variable BODY-LEN
variable BODY-HASH
variable FULL-XT   0 FULL-XT !

: BUF ( -- ptr a ) BUF-P @ ;

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
   0 BUF,  0 BUF,  0 BUF,  0 BUF,  0 BUF,
   BODY-LEN @ BUF,
   BODY-HASH @ BUF, ;

: CERT-BYTES ( -- ptr u8 n ) BUF BUF-N @ cells ;

: CERT-CELL@ ( n -- n ) {: idx:n :}
   idx 0 < idx BUF-N @ >= or if s" lowering certificate cell index" 76 die then
   idx cells BUF + @ ;

: FULL-INSTALL ( n -- )
   FULL-XT @ 0 <> if s" lowering certificate full producer already installed" 76 die then
   FULL-XT ! ;

: DISPATCH ( ptr u8 n n -- ) {: a:ptr u:n verdict:n :}
   FULL-XT @ {: xt:n :}
   xt 0 <> if a u verdict xt CHECKER-CERT-CALL exit then
   WF-N@ 0 <> if s" lowering certificate facts before full producer" 76 die then
   a u EMPTY ;

public

: MAGIC ( -- n ) MAGIC-V ;
: VERSION ( -- n ) VERSION-V ;
: HEADER-CELLS ( -- n ) HEADER-N ;
: MAGIC-CELL ( -- n ) MAGIC-I ;
: VERSION-CELL ( -- n ) VERSION-I ;
: TOTAL-BYTES-CELL ( -- n ) TOTAL-BYTES-I ;
: NEEDS-CELL ( -- n ) NEEDS-I ;
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
: BODY-LEN-CELL ( -- n ) BODY-LEN-I ;
: BODY-HASH-CELL ( -- n ) BODY-HASH-I ;
: FNV-OFFSET ( -- n ) FNV-OFFSET-V ;
: FNV-PRIME ( -- n ) FNV-PRIME-V ;
: CELL-COUNT ( -- n ) BUF-N @ ;
: CELL@ ( n -- n ) CERT-CELL@ ;
: BYTES ( -- ptr u8 n ) CERT-BYTES ;

' DISPATCH CHECKER-CERT:INSTALL

;package
