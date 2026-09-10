require lib/xml/scalar.f

package XML
public
DEFLINEAR XML:reader
ENUM kind start end text comment pi cdata eof ;ENUM

private
CAST: KIND>N ( kind -- n )
CAST: >KIND ( n -- kind )

21 constant HEADER-CELLS
5 constant FRAME-CELLS
8 constant ATTR-CELLS
4 constant NS-CELLS
FRAME-CELLS ATTR-CELLS + NS-CELLS + constant CAP-CELLS
0 constant SOURCE-IDX
1 cells constant SOURCE-LEN
2 cells constant POSITION
3 cells constant CAPACITY
4 cells constant OPEN-DEPTH
5 cells constant NS-COUNT
6 cells constant ATTR-COUNT
7 cells constant ATTR-INDEX
8 cells constant RAW-OFF
9 cells constant RAW-LEN
10 cells constant TOKEN-KIND
11 cells constant NAME-OFF
12 cells constant NAME-LEN
13 cells constant VALUE-OFF
14 cells constant VALUE-LEN
15 cells constant URI-OFF
16 cells constant URI-LEN
17 cells constant PENDING
18 cells constant ROOT-COUNT
19 cells constant FAILED
20 cells constant STORAGE-LEN

1 constant POP-NEXT
2 constant EMPTY-END
-1 constant XML-URI
-2 constant XMLNS-URI

\ The checked initializer validates storage; only linear ownership projection
\ is asserted. Retirement owner: cap:raw-pointer-lifetime. See xml-test.f.
TRUSTED: MINT ( ptr n -- XML:reader ) ;
TRUSTED: STATE ( XML:reader -- XML:reader ptr n ) dup ;
TRUSTED: CONSUME ( XML:reader -- ) drop ;

: CAP-CHECK ( n -- )
   dup 0 < if E-CAPACITY throw then
   MAX-SIZE HEADER-CELLS cells - CAP-CELLS cells / > if
      E-CAPACITY throw
   then ;

public
: STORAGE-BYTES ( n -- n )
   dup CAP-CHECK
   CAP-CELLS * HEADER-CELLS + cells ;

private
: FRAME ( ptr n n -- ptr n )
   {: state index:n :}
   state HEADER-CELLS index FRAME-CELLS * + cells + ;

: ATTRIBUTE ( ptr n n -- ptr n )
   {: state index:n :}
   state HEADER-CELLS state CAPACITY + @ FRAME-CELLS * +
   index ATTR-CELLS * + cells + ;

: NAMESPACE ( ptr n n -- ptr n )
   {: state index:n :}
   state HEADER-CELLS state CAPACITY + @ FRAME-CELLS ATTR-CELLS + * +
   index NS-CELLS * + cells + ;

: SOURCE$ ( ptr n -- ptr u8 n )
   {: state :}
   state SOURCE-IDX ptr-field @ state SOURCE-LEN + @ ;

: AT ( ptr n n -- n )
   {: state index:n :}
   state SOURCE$ index BYTE@ ;

: EOF? ( ptr n -- bool )
   {: state :}
   state POSITION + @ state SOURCE-LEN + @ = ;

: PEEK ( ptr n -- n )
   dup POSITION + @ AT ;

: ADVANCE ( ptr n -- )
   POSITION + 1 swap +! ;

: ADVANCE-N ( ptr n n -- )
   {: state count:n :}
   count state SOURCE-LEN + @ state POSITION + @ - > if
      E-TRUNCATED throw
   then
   count state POSITION + +! ;

: SPAN$ ( ptr n n n -- ptr u8 n )
   {: state start:n size:n :}
   state SOURCE-IDX ptr-field @ start + size ;

: MATCH$ ( ptr n ptr u8 n -- bool )
   {: state value size:n :}
   size state SOURCE-LEN + @ state POSITION + @ - > if false exit then
   state state POSITION + @ size SPAN$ value size BYTES= ;

: REQUIRE-BYTE ( ptr n n -- )
   {: state byte:n :}
   state PEEK byte <> if E-MALFORMED throw then
   state ADVANCE ;

: WHITE? ( ptr n -- bool )
   dup EOF? if drop false exit then
   PEEK XML-SPACE? ;

: SKIP-WHITE ( ptr n -- )
   begin dup WHITE? while dup ADVANCE repeat drop ;

: SCAN-SCALAR ( ptr n -- n )
   {: state :}
   state SOURCE$ state POSITION + @ SCALAR-AT
   state POSITION + ! ;

: URI$ ( ptr n n n -- ptr u8 n )
   {: state start:n size:n :}
   start XML-URI = if s" http://www.w3.org/XML/1998/namespace" exit then
   start XMLNS-URI = if s" http://www.w3.org/2000/xmlns/" exit then
   state start size SPAN$ ;

: CURRENT-FRAME ( ptr n -- ptr n )
   dup OPEN-DEPTH + @ 1- FRAME ;

: STORAGE-CHECK ( ptr n n ptr u8 n -- )
   {: storage cap:n source size:n :}
   storage BYTE-VIEW cap SPAN-CHECK
   storage BYTE-VIEW BYTE-ADDRESS CELL 1- and 0<> if E-STORAGE throw then
   cap 1 STORAGE-BYTES < if E-CAPACITY throw then
   source size SPAN-CHECK
   storage BYTE-VIEW cap source size OVERLAP? if E-ALIAS throw then ;

: INIT-STATE ( ptr n n ptr u8 n -- )
   {: storage cap:n source size:n :}
   HEADER-CELLS 0 ?do 0 storage i cells + ! loop
   source storage SOURCE-IDX ptr-field !
   size storage SOURCE-LEN + !
   cap HEADER-CELLS cells - CAP-CELLS cells / storage CAPACITY + !
   cap storage STORAGE-LEN + !
   -1 storage ATTR-INDEX + !
   XML-KIND:EOF KIND>N storage TOKEN-KIND + ! ;

: BOM ( ptr n -- )
   {: state :}
   state s\" \xFF\xFE" MATCH$ state s\" \xFE\xFF" MATCH$ or if
      E-ENCODING throw
   then
   state s\" <\z" MATCH$ state s\" \z<" MATCH$ or if
      E-ENCODING throw
   then
   state s\" \xEF\xBB\xBF" MATCH$ if state 3 ADVANCE-N then ;

public
: INIT ( ptr n n ptr u8 n -- XML:reader )
   {: storage cap:n source size:n :}
   storage cap source size STORAGE-CHECK
   storage cap source size INIT-STATE
   storage BOM
   storage MINT ;

: CLOSE ( XML:reader -- )
   CONSUME ;

private
: LIVE ( ptr n -- )
   FAILED + @ 0<> if E-STATE throw then ;

: NAMED ( ptr n -- )
   {: state :}
   state LIVE
   state NAME-LEN + @ 0= if E-STATE throw then ;

: ACTIVE-ATTRIBUTE ( ptr n -- ptr n )
   {: state :}
   state LIVE
   state TOKEN-KIND + @ XML-KIND:START KIND>N <> if E-STATE throw then
   state ATTR-INDEX + @ {: index:n :}
   index 0 < index state ATTR-COUNT + @ >= or if E-STATE throw then
   state index ATTRIBUTE ;

: LOCAL-SPAN ( ptr u8 n -- ptr u8 n )
   {: name size:n :}
   size 0 ?do
      name i + c@ $3A = if
         name i 1+ + size i 1+ - unloop exit
      then
   loop
   name size ;

: OUTPUT-CHECK ( ptr n ptr u8 n -- )
   {: state destination cap:n :}
   destination cap SPAN-CHECK
   state SOURCE$ destination cap OVERLAP? if E-ALIAS throw then
   state BYTE-VIEW state STORAGE-LEN + @
   destination cap OVERLAP? if E-ALIAS throw then ;

\ Keep the reader below throwing workers' arguments. Native catch restores
\ stack depth, so public locals could overwrite the reader's saved stack cell.
: RAW-STATE ( ptr n -- off len )
   {: state :}
   state LIVE
   state RAW-OFF + @ >OFF state RAW-LEN + @ >LEN ;

: RAW$-STATE ( ptr n -- ptr u8 n )
   {: state :}
   state LIVE
   state state RAW-OFF + @ state RAW-LEN + @ SPAN$ ;

: NAME$-STATE ( ptr n -- ptr u8 n )
   {: state :}
   state NAMED
   state state NAME-OFF + @ state NAME-LEN + @ SPAN$ ;

: DECODE-ARGS ( XML:reader ptr u8 n -- XML:reader ptr n ptr u8 n )
   {: destination cap:n :}
   STATE destination cap ;

: URI-STATE ( ptr n ptr u8 n -- n )
   {: state destination cap:n :}
   state NAMED
   state destination cap OUTPUT-CHECK
   state state URI-OFF + @ state URI-LEN + @ URI$
   DECODE-ATTR destination cap DECODE-INTO ;

: ATTR-RESET-STATE ( ptr n -- )
   {: state :}
   state LIVE
   state TOKEN-KIND + @ XML-KIND:START KIND>N <> if E-STATE throw then
   -1 state ATTR-INDEX + ! ;

: ATTR-NEXT-STATE ( ptr n -- bool )
   {: state :}
   state LIVE
   state TOKEN-KIND + @ XML-KIND:START KIND>N <> if E-STATE throw then
   state ATTR-INDEX + @ state ATTR-COUNT + @ >= if false exit then
   1 state ATTR-INDEX + +!
   state ATTR-INDEX + @ state ATTR-COUNT + @ < ;

: ATTR-RAW-STATE ( ptr n -- off len )
   ACTIVE-ATTRIBUTE {: attr :}
   attr 4 cells + @ >OFF attr 5 cells + @ >LEN ;

: ATTR-NAME$-STATE ( ptr n -- ptr u8 n )
   {: state :}
   state ACTIVE-ATTRIBUTE {: attr :}
   state attr @ attr CELL + @ SPAN$ ;

: ATTR-VALUE$-STATE ( ptr n -- ptr u8 n )
   {: state :}
   state ACTIVE-ATTRIBUTE {: attr :}
   state attr 2 cells + @ attr 3 cells + @ SPAN$ ;

: ATTR-VALUE-STATE ( ptr n -- off len )
   ACTIVE-ATTRIBUTE {: attr :}
   attr 2 cells + @ >OFF attr 3 cells + @ >LEN ;

: ATTR-URI-STATE ( ptr n ptr u8 n -- n )
   {: state destination cap:n :}
   state ACTIVE-ATTRIBUTE {: attr :}
   state destination cap OUTPUT-CHECK
   state attr 6 cells + @ attr 7 cells + @ URI$
   DECODE-ATTR destination cap DECODE-INTO ;

: ATTR-TEXT-STATE ( ptr n ptr u8 n -- n )
   {: state destination cap:n :}
   state ACTIVE-ATTRIBUTE {: attr :}
   state destination cap OUTPUT-CHECK
   state attr 2 cells + @ attr 3 cells + @ SPAN$
   DECODE-ATTR destination cap DECODE-INTO ;

public
: KIND ( XML:reader -- XML:reader kind )
   STATE dup LIVE TOKEN-KIND + @ >KIND ;

: RAW ( XML:reader -- XML:reader off len )
   STATE RAW-STATE ;

: RAW$ ( XML:reader -- XML:reader ptr u8 n )
   STATE RAW$-STATE ;

: NAME$ ( XML:reader -- XML:reader ptr u8 n )
   STATE NAME$-STATE ;

: LOCAL$ ( XML:reader -- XML:reader ptr u8 n )
   NAME$ LOCAL-SPAN ;

: DEPTH ( XML:reader -- XML:reader n )
   STATE dup LIVE OPEN-DEPTH + @ ;

: URI ( XML:reader ptr u8 n -- XML:reader n )
   DECODE-ARGS URI-STATE ;

: ATTR-RESET ( XML:reader -- XML:reader )
   STATE ATTR-RESET-STATE ;

: ATTR-NEXT ( XML:reader -- XML:reader bool )
   STATE ATTR-NEXT-STATE ;

: ATTR-RAW ( XML:reader -- XML:reader off len )
   STATE ATTR-RAW-STATE ;

: ATTR-NAME$ ( XML:reader -- XML:reader ptr u8 n )
   STATE ATTR-NAME$-STATE ;

: ATTR-LOCAL$ ( XML:reader -- XML:reader ptr u8 n )
   ATTR-NAME$ LOCAL-SPAN ;

: ATTR-VALUE$ ( XML:reader -- XML:reader ptr u8 n )
   STATE ATTR-VALUE$-STATE ;

: ATTR-VALUE ( XML:reader -- XML:reader off len )
   STATE ATTR-VALUE-STATE ;

: ATTR-URI ( XML:reader ptr u8 n -- XML:reader n )
   DECODE-ARGS ATTR-URI-STATE ;

: ATTR-TEXT ( XML:reader ptr u8 n -- XML:reader n )
   DECODE-ARGS ATTR-TEXT-STATE ;

;package
