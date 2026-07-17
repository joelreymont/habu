\ type-field.f - transactional shared field metadata.
\
\ TYPE-FIELD owns the ordered field registry shared by PRODUCT and payload
\ SUMTYPE declarations. Builder calls are package-scoped, fully role typed, and
\ transactional. Published reflection never exposes arena pointers or raw ids.

package TYPE-FIELD
public

7123 constant E-TX
7124 constant E-DRAFT
7125 constant E-FAMILY
7126 constant E-VARIANT
7127 constant E-NAME
7128 constant E-DUP
7129 constant E-SCHEMA
7130 constant E-LAYOUT
7131 constant E-FLAGS
7132 constant E-SOURCE
7133 constant E-ID
7134 constant E-CAPACITY
7135 constant E-RANGE
7136 constant E-VISIBILITY

s" TYPE-FIELD:field-id" CHECKER-DEFTYPE
s" TYPE-FIELD:family-id" CHECKER-DEFTYPE
s" TYPE-FIELD:variant-id" CHECKER-DEFTYPE
s" TYPE-FIELD:schema-id" CHECKER-DEFTYPE
s" TYPE-FIELD:slot" CHECKER-DEFTYPE
s" TYPE-FIELD:cell-count" CHECKER-DEFTYPE
s" TYPE-FIELD:byte-off" CHECKER-DEFTYPE
s" TYPE-FIELD:byte-size" CHECKER-DEFTYPE
s" TYPE-FIELD:alignment" CHECKER-DEFTYPE
s" TYPE-FIELD:field-flags" CHECKER-DEFTYPE
s" TYPE-FIELD:source-id" CHECKER-DEFTYPE
s" TYPE-FIELD:source-off" CHECKER-DEFTYPE
s" TYPE-FIELD:source-len" CHECKER-DEFTYPE
s" TYPE-FIELD:visibility" CHECKER-DEFTYPE
s" TYPE-FIELD:field-count" CHECKER-DEFTYPE
s" TYPE-FIELD:field-tx" CHECKER-DEFLINEAR
s" TYPE-FIELD:field-draft" CHECKER-DEFLINEAR

private

\ Private proof erasure/minting. Every mint follows the validation local to its
\ role; field-id is minted only while scanning committed rows.
TRUSTED: ID>N ( TYPE-FIELD:field-id -- n ) ;
TRUSTED: N>ID ( n -- TYPE-FIELD:field-id ) ;
TRUSTED: FAMILY>N ( TYPE-FIELD:family-id -- n ) ;
TRUSTED: N>FAMILY ( n -- TYPE-FIELD:family-id ) ;
TRUSTED: VARIANT>N ( TYPE-FIELD:variant-id -- n ) ;
TRUSTED: N>VARIANT ( n -- TYPE-FIELD:variant-id ) ;
TRUSTED: SCHEMA>N ( TYPE-FIELD:schema-id -- n ) ;
TRUSTED: N>SCHEMA ( n -- TYPE-FIELD:schema-id ) ;
TRUSTED: SLOT>N ( TYPE-FIELD:slot -- n ) ;
TRUSTED: N>SLOT ( n -- TYPE-FIELD:slot ) ;
TRUSTED: CELLS>N ( TYPE-FIELD:cell-count -- n ) ;
TRUSTED: N>CELLS ( n -- TYPE-FIELD:cell-count ) ;
TRUSTED: BYTE-OFF>N ( TYPE-FIELD:byte-off -- n ) ;
TRUSTED: N>BYTE-OFF ( n -- TYPE-FIELD:byte-off ) ;
TRUSTED: BYTE-SIZE>N ( TYPE-FIELD:byte-size -- n ) ;
TRUSTED: N>BYTE-SIZE ( n -- TYPE-FIELD:byte-size ) ;
TRUSTED: ALIGN>N ( TYPE-FIELD:alignment -- n ) ;
TRUSTED: N>ALIGN ( n -- TYPE-FIELD:alignment ) ;
TRUSTED: FLAGS>N ( TYPE-FIELD:field-flags -- n ) ;
TRUSTED: N>FLAGS ( n -- TYPE-FIELD:field-flags ) ;
TRUSTED: SOURCE-ID>N ( TYPE-FIELD:source-id -- n ) ;
TRUSTED: N>SOURCE-ID ( n -- TYPE-FIELD:source-id ) ;
TRUSTED: SOURCE-OFF>N ( TYPE-FIELD:source-off -- n ) ;
TRUSTED: N>SOURCE-OFF ( n -- TYPE-FIELD:source-off ) ;
TRUSTED: SOURCE-LEN>N ( TYPE-FIELD:source-len -- n ) ;
TRUSTED: N>SOURCE-LEN ( n -- TYPE-FIELD:source-len ) ;
TRUSTED: VIS>N ( TYPE-FIELD:visibility -- n ) ;
TRUSTED: N>VIS ( n -- TYPE-FIELD:visibility ) ;
TRUSTED: COUNT>N ( TYPE-FIELD:field-count -- n ) ;
TRUSTED: N>COUNT ( n -- TYPE-FIELD:field-count ) ;
TRUSTED: TX>N ( TYPE-FIELD:field-tx -- n ) ;
TRUSTED: N>TX ( n -- TYPE-FIELD:field-tx ) ;
TRUSTED: DRAFT>N ( TYPE-FIELD:field-draft -- n ) ;
TRUSTED: N>DRAFT ( n -- TYPE-FIELD:field-draft ) ;

\ Pre-hook registry boundaries.
TRUSTED: RAW-GROW ( ptr a n n -- ptr a ) ARENA-BYTES-GROW ;
TRUSTED: RAW-FAMILY-N ( -- n ) TFAM-N@ ;
TRUSTED: RAW-FAMILY-RESOLVE ( ptr u8 n ptr u8 n -- n bool ) TFAM-RESOLVE ;
TRUSTED: RAW-FAMILY-PKG$ ( n -- ptr u8 n ) TFAM-PKG$ ;
TRUSTED: RAW-FAMILY-VIS@ ( n -- n ) TFAM-VIS@ ;
TRUSTED: RAW-FAMILY-PRODUCT? ( n -- bool ) TFAM-PRODUCT? ;
TRUSTED: RAW-FAMILY-SUM? ( n -- bool ) TFAM-SUM? ;
TRUSTED: RAW-ACTIVE-PKG$ ( -- ptr u8 n ) TFAM-ACTIVE-PKG$ ;
TRUSTED: RAW-VARIANT-N ( -- n ) SUMV-N@ ;
TRUSTED: RAW-VARIANT-FIND ( n ptr u8 n -- n bool ) SUMV-FIND ;
TRUSTED: RAW-VARIANT-FAMILY@ ( n -- n ) SUMV-FAM@ ;
TRUSTED: RAW-SCHEMA-N ( -- n ) SCHEMA-ROOT-N@ ;
TRUSTED: RAW-CANON? ( ptr u8 n -- bool ) TF-CANON? ;
TRUSTED: RAW-KEYWORD? ( ptr u8 n -- bool ) TDECL-KEYWORD? ;
TRUSTED: RAW-PUBLIC ( -- n ) CHECKER-PACKAGE-PUBLIC ;
TRUSTED: RAW-PRIVATE ( -- n ) CHECKER-PACKAGE-PRIVATE ;
TRUSTED: RAW-U8 ( ptr a -- ptr u8 ) ;

$7FFFFFFFFFFFFFFF constant MAX-N
4 constant CAP-INIT
16 constant NAME-INIT

1 constant FLAG-PUBLIC
2 constant FLAG-BYTE
FLAG-PUBLIC FLAG-BYTE or constant FLAG-MASK

: B-FALSE ( -- bool ) 0 0= 0= ;
: B-TRUE ( -- bool ) 0 0= ;

1 constant STAGE-SCHEMA
2 constant STAGE-LAYOUT
4 constant STAGE-SOURCE
STAGE-SCHEMA STAGE-LAYOUT or STAGE-SOURCE or constant STAGE-DONE

\ Field row; rows at [COMMIT-N, ROW-N) are provisional.
0 cells constant ROW.FAMILY-OFF
1 cells constant ROW.HAS-VARIANT-OFF
2 cells constant ROW.VARIANT-OFF
3 cells constant ROW.NAME-OFF-OFF
4 cells constant ROW.NAME-LEN-OFF
5 cells constant ROW.SCHEMA-OFF
6 cells constant ROW.SLOT-OFF
7 cells constant ROW.CELLS-OFF
8 cells constant ROW.BYTE-OFF-OFF
9 cells constant ROW.BYTE-SIZE-OFF
10 cells constant ROW.ALIGN-OFF
11 cells constant ROW.FLAGS-OFF
12 cells constant ROW.SOURCE-ID-OFF
13 cells constant ROW.SOURCE-OFF-OFF
14 cells constant ROW.SOURCE-LEN-OFF
15 cells constant ROW-REC

: ROW.FAMILY ( ptr a -- ptr a ) ROW.FAMILY-OFF + ;
: ROW.HAS-VARIANT ( ptr a -- ptr a ) ROW.HAS-VARIANT-OFF + ;
: ROW.VARIANT ( ptr a -- ptr a ) ROW.VARIANT-OFF + ;
: ROW.NAME-OFF ( ptr a -- ptr a ) ROW.NAME-OFF-OFF + ;
: ROW.NAME-LEN ( ptr a -- ptr a ) ROW.NAME-LEN-OFF + ;
: ROW.SCHEMA ( ptr a -- ptr a ) ROW.SCHEMA-OFF + ;
: ROW.SLOT ( ptr a -- ptr a ) ROW.SLOT-OFF + ;
: ROW.CELLS ( ptr a -- ptr a ) ROW.CELLS-OFF + ;
: ROW.BYTE-OFF ( ptr a -- ptr a ) ROW.BYTE-OFF-OFF + ;
: ROW.BYTE-SIZE ( ptr a -- ptr a ) ROW.BYTE-SIZE-OFF + ;
: ROW.ALIGN ( ptr a -- ptr a ) ROW.ALIGN-OFF + ;
: ROW.FLAGS ( ptr a -- ptr a ) ROW.FLAGS-OFF + ;
: ROW.SOURCE-ID ( ptr a -- ptr a ) ROW.SOURCE-ID-OFF + ;
: ROW.SOURCE-OFF ( ptr a -- ptr a ) ROW.SOURCE-OFF-OFF + ;
: ROW.SOURCE-LEN ( ptr a -- ptr a ) ROW.SOURCE-LEN-OFF + ;

\ Strict-LIFO transaction frame. A provisional row lives at ROW-N beyond
\ COMMIT-N; the owning frame carries its draft handle and stage state.
0 cells constant TX.ROW-OFF
1 cells constant TX.NAME-OFF
2 cells constant TX.FAMILY-OFF
3 cells constant TX.TOKEN-OFF
4 cells constant TX.DRAFT-ROW-OFF
5 cells constant TX.DRAFT-TOKEN-OFF
6 cells constant TX.DRAFT-STATE-OFF
7 cells constant TX-REC

: TX.ROW ( ptr a -- ptr a ) TX.ROW-OFF + ;
: TX.NAME ( ptr a -- ptr a ) TX.NAME-OFF + ;
: TX.FAMILY ( ptr a -- ptr a ) TX.FAMILY-OFF + ;
: TX.TOKEN ( ptr a -- ptr a ) TX.TOKEN-OFF + ;
: TX.DRAFT-ROW ( ptr a -- ptr a ) TX.DRAFT-ROW-OFF + ;
: TX.DRAFT-TOKEN ( ptr a -- ptr a ) TX.DRAFT-TOKEN-OFF + ;
: TX.DRAFT-STATE ( ptr a -- ptr a ) TX.DRAFT-STATE-OFF + ;

: LAYOUT= ( n n -- )
   <> if E-LAYOUT throw then ;

ROW.FAMILY-OFF 0 cells LAYOUT=
ROW.HAS-VARIANT-OFF 1 cells LAYOUT=
ROW.VARIANT-OFF 2 cells LAYOUT=
ROW.NAME-OFF-OFF 3 cells LAYOUT=
ROW.NAME-LEN-OFF 4 cells LAYOUT=
ROW.SCHEMA-OFF 5 cells LAYOUT=
ROW.SLOT-OFF 6 cells LAYOUT=
ROW.CELLS-OFF 7 cells LAYOUT=
ROW.BYTE-OFF-OFF 8 cells LAYOUT=
ROW.BYTE-SIZE-OFF 9 cells LAYOUT=
ROW.ALIGN-OFF 10 cells LAYOUT=
ROW.FLAGS-OFF 11 cells LAYOUT=
ROW.SOURCE-ID-OFF 12 cells LAYOUT=
ROW.SOURCE-OFF-OFF 13 cells LAYOUT=
ROW.SOURCE-LEN-OFF 14 cells LAYOUT=
ROW-REC 15 cells LAYOUT=
TX.ROW-OFF 0 cells LAYOUT=
TX.NAME-OFF 1 cells LAYOUT=
TX.FAMILY-OFF 2 cells LAYOUT=
TX.TOKEN-OFF 3 cells LAYOUT=
TX.DRAFT-ROW-OFF 4 cells LAYOUT=
TX.DRAFT-TOKEN-OFF 5 cells LAYOUT=
TX.DRAFT-STATE-OFF 6 cells LAYOUT=
TX-REC 7 cells LAYOUT=

create ROW-BOOT CAP-INIT ROW-REC * allot
PTR-VARIABLE ROW-P   ROW-BOOT ROW-P !
variable ROW-CAP   CAP-INIT ROW-CAP !
variable ROW-N
variable COMMIT-N

variable DRAFT-SERIAL

create TX-BOOT CAP-INIT TX-REC * allot
PTR-VARIABLE TX-P   TX-BOOT TX-P !
variable TX-CAP   CAP-INIT TX-CAP !
variable TX-DEPTH
variable TX-SERIAL

create NAME-BOOT NAME-INIT allot
PTR-VARIABLE NAME-P   NAME-BOOT NAME-P !
variable NAME-CAP   NAME-INIT NAME-CAP !
variable NAME-N

: NEXT-CAP ( n n n -- n ) {: need:n cap:n stride:n :}
   need 0 < stride 0 <= or if E-CAPACITY throw then
   MAX-N stride / {: limit:n :}
   need limit > if E-CAPACITY throw then
   cap limit 2 / > if need else cap 2 * need max then ;

: ROW-ENSURE ( n -- ) {: need:n :}
   need ROW-CAP @ <= if exit then
   need ROW-CAP @ ROW-REC NEXT-CAP {: cap:n :}
   ROW-P @ ROW-CAP @ ROW-REC * cap ROW-REC * RAW-GROW ROW-P !
   cap ROW-CAP ! ;

: TX-ENSURE ( n -- ) {: need:n :}
   need TX-CAP @ <= if exit then
   need TX-CAP @ TX-REC NEXT-CAP {: cap:n :}
   TX-P @ TX-CAP @ TX-REC * cap TX-REC * RAW-GROW TX-P !
   cap TX-CAP ! ;

: NAME-ENSURE ( n -- ) {: add:n :}
   add 0 < if E-CAPACITY throw then
   NAME-N @ MAX-N add - > if E-CAPACITY throw then
   NAME-N @ add + {: need:n :}
   need NAME-CAP @ <= if exit then
   need NAME-CAP @ 1 NEXT-CAP {: cap:n :}
   NAME-P @ NAME-CAP @ cap RAW-GROW NAME-P !
   cap NAME-CAP ! ;

: ROW-REC@ ( n -- ptr a ) {: id:n :}
   id 0 < id ROW-N @ >= or if E-ID throw then
   id ROW-REC * ROW-P @ + ;

: LIVE-REC@ ( n -- ptr a ) {: id:n :}
   id 0 < id COMMIT-N @ >= or if E-ID throw then
   id ROW-REC * ROW-P @ + ;

: TX-REC@ ( n -- ptr a ) {: id:n :}
   id 0 < id TX-DEPTH @ >= or if E-TX throw then
   id TX-REC * TX-P @ + ;

: NAME-BASE ( -- ptr u8 ) NAME-P @ RAW-U8 ;

: NAME$ ( n n -- ptr u8 n ) {: off:n u:n :}
   off 0 < u 0 < or if E-ID throw then
   off NAME-N @ > NAME-N @ off - u < or if E-ID throw then
   NAME-BASE off + u ;

: COPY-NAME ( ptr u8 n -- n ) {: a:ptr u:n :}
   u NAME-ENSURE
   NAME-N @ {: off:n :}
   0 begin dup u < while
      dup a + c@ over NAME-BASE off + + c!
      1+
   repeat drop
   NAME-N @ u + NAME-N !
   off ;

: ASCII-FOLD ( n -- n ) {: c:n :}
   c 65 >= c 90 <= and if c 32 + else c then ;

: CI= ( ptr u8 n ptr u8 n -- bool ) {: a:ptr au:n b:ptr bu:n :}
   au bu <> if 0 0= 0= exit then
   0 begin dup au < while
      dup a + c@ ASCII-FOLD over b + c@ ASCII-FOLD <> if
         drop 0 0= 0= exit
      then
      1+
   repeat drop 0 0= ;

: FIELD-NAME= ( ptr u8 n ptr a -- bool ) {: a:ptr u:n row:ptr :}
   row ROW.NAME-OFF @ row ROW.NAME-LEN @ NAME$ a u CI= ;

: GENERATED? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" make" CI= if 0 0= exit then
   a u s" unmake" CI= if 0 0= exit then
   a u s" tag" CI= if 0 0= exit then
   a u s" eq" CI= if 0 0= exit then
   a u s" hash" CI= if 0 0= exit then
   a u s" order" CI= if 0 0= exit then
   a u s" encode" CI= if 0 0= exit then
   a u s" decode" CI= ;

: RESERVED? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u RAW-KEYWORD? if 0 0= exit then
   a u s" structure" CI= if 0 0= exit then
   a u s" ;structure" CI= if 0 0= exit then
   a u GENERATED? ;

: REQUIRE-NAME ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0= if E-NAME throw then
   a u RAW-CANON? 0= if E-NAME throw then
   a u RESERVED? if E-NAME throw then ;

: REQUIRE-FAMILY ( n -- ) {: fam:n :}
   fam 0 < fam RAW-FAMILY-N >= or if E-FAMILY throw then ;

: FAMILY-OWNED? ( n -- bool )
   RAW-FAMILY-PKG$ RAW-ACTIVE-PKG$ CORE-STR= ;

: FAMILY-VISIBLE? ( n -- bool ) {: fam:n :}
   fam RAW-FAMILY-VIS@ RAW-PUBLIC = if 0 0= exit then
   fam FAMILY-OWNED? ;

: REQUIRE-VISIBLE ( n -- )
   dup REQUIRE-FAMILY
   FAMILY-VISIBLE? 0= if E-VISIBILITY throw then ;

: REQUIRE-OWNED ( n -- )
   dup REQUIRE-FAMILY
   FAMILY-OWNED? 0= if E-VISIBILITY throw then ;

: REQUIRE-VARIANT ( n n -- ) {: fam:n variant:n :}
   variant 0 < variant RAW-VARIANT-N >= or if E-VARIANT throw then
   variant RAW-VARIANT-FAMILY@ fam <> if E-VARIANT throw then ;

: ROW-OWNER? ( n n bool n -- bool ) {: id:n fam:n has:bool variant:n :}
   id ROW-REC@ {: row:ptr :}
   row ROW.FAMILY @ fam =
   row ROW.HAS-VARIANT @ has if 1 else 0 then = and
   row ROW.VARIANT @ variant = and ;

: ROW-VISIBLE? ( ptr a -- bool ) {: row:ptr :}
   row ROW.FAMILY @ FAMILY-VISIBLE? 0= if 0 0= 0= exit then
   row ROW.FLAGS @ FLAG-PUBLIC and 0 <> if 0 0= exit then
   row ROW.FAMILY @ FAMILY-OWNED? ;

: OWNER-SEEN? ( n bool n -- bool ) {: fam:n has:bool variant:n :}
   0 begin dup ROW-N @ < while
      dup fam has variant ROW-OWNER? if drop 0 0= exit then
      1+
   repeat drop 0 0= 0= ;

: REQUIRE-CONTIGUOUS ( n bool n -- ) {: fam:n has:bool variant:n :}
   fam has variant OWNER-SEEN? 0= if exit then
   ROW-N @ 1 - fam has variant ROW-OWNER? 0= if E-RANGE throw then ;

: REQUIRE-UNIQUE ( n bool n ptr u8 n -- )
   {: fam:n has:bool variant:n a:ptr u:n :}
   0 begin dup ROW-N @ < while
      dup fam has variant ROW-OWNER? if
         dup ROW-REC@ a u rot FIELD-NAME= if drop E-DUP throw then
      then
      1+
   repeat drop ;

: NEXT-SERIAL ( ptr n -- n ) {: cell:ptr :}
   cell @ MAX-N = if E-CAPACITY throw then
   cell @ 1 + dup cell ! ;

: TX-TOP-REC ( -- ptr a )
   TX-DEPTH @ 0= if E-TX throw then
   TX-DEPTH @ 1 - TX-REC@ ;

: TX-TOP? ( n -- bool ) {: token:n :}
   TX-DEPTH @ 0= if 0 0= 0= exit then
   TX-TOP-REC TX.TOKEN @ token = ;

: REQUIRE-TX ( n -- )
   TX-TOP? 0= if E-TX throw then ;

: ROLLBACK-TOP ( -- )
   TX-TOP-REC {: tx:ptr :}
   tx TX.ROW @ ROW-N !
   tx TX.NAME @ NAME-N !
   TX-DEPTH @ 1 - TX-DEPTH ! ;

: ABORT-ALL ( -- )
   begin TX-DEPTH @ 0 > while ROLLBACK-TOP repeat ;

: REQUIRE-DRAFT ( n n -- ptr a ) {: tx-token:n draft-token:n :}
   tx-token REQUIRE-TX
   TX-TOP-REC {: tx:ptr :}
   tx TX.DRAFT-ROW @ 0 < if E-DRAFT throw then
   tx TX.DRAFT-TOKEN @ draft-token <> if E-DRAFT throw then
   tx TX.DRAFT-ROW @ ROW-REC@ ;

: REQUIRE-STAGE-NEW ( n -- ) {: stage:n :}
   TX-TOP-REC TX.DRAFT-STATE @ stage and 0 <> if E-DRAFT throw then ;

: STAGE+ ( n -- ) {: stage:n :}
   TX-TOP-REC TX.DRAFT-STATE dup @ stage or swap ! ;

variable A-TX
variable A-DRAFT
variable A-VAR-F
variable A-VAR
variable A-NAME
variable A-NAME-U
variable A-X
variable A-Y
variable A-Z
variable A-W
variable R-DRAFT

: ABORT-THROW ( n -- ) {: code:n :}
   code 0= if exit then
   ABORT-ALL
   code throw ;

: START-CORE ( -- )
   A-TX @ REQUIRE-TX
   TX-TOP-REC {: tx:ptr :}
   tx TX.FAMILY @ {: fam:n :}
   tx TX.DRAFT-ROW @ 0 >= if E-DRAFT throw then

   \ The caller may have passed parse-name storage. Copy it before any s" or
   \ parser-adjacent validation can reuse the interpreter token buffer.
   A-NAME @ A-NAME-U @ COPY-NAME {: name-off:n :}
   name-off A-NAME-U @ NAME$ {: name:ptr name-u:n :}

   A-VAR-F @ if
      fam RAW-FAMILY-SUM? 0= if E-FAMILY throw then
      fam A-VAR @ REQUIRE-VARIANT
   else
      fam RAW-FAMILY-PRODUCT? 0= if E-FAMILY throw then
   then
   name name-u REQUIRE-NAME
   fam A-VAR-F @ 0= 0= A-VAR @ REQUIRE-CONTIGUOUS
   fam A-VAR-F @ 0= 0= A-VAR @ name name-u REQUIRE-UNIQUE

   ROW-N @ 1 + ROW-ENSURE
   DRAFT-SERIAL NEXT-SERIAL {: token:n :}
   ROW-N @ {: row-id:n :}
   row-id ROW-REC * ROW-P @ + {: row:ptr :}
   fam row ROW.FAMILY !
   A-VAR-F @ row ROW.HAS-VARIANT !
   A-VAR @ row ROW.VARIANT !
   name-off row ROW.NAME-OFF !
   name-u row ROW.NAME-LEN !
   0 row ROW.SCHEMA !
   0 row ROW.SLOT !
   0 row ROW.CELLS !
   0 row ROW.BYTE-OFF !
   0 row ROW.BYTE-SIZE !
   0 row ROW.ALIGN !
   0 row ROW.FLAGS !
   0 row ROW.SOURCE-ID !
   0 row ROW.SOURCE-OFF !
   0 row ROW.SOURCE-LEN !
   ROW-N @ 1 + ROW-N !
   row-id tx TX.DRAFT-ROW !
   token tx TX.DRAFT-TOKEN !
   0 tx TX.DRAFT-STATE !
   token R-DRAFT ! ;

: START-RUN ( n bool n ptr u8 n -- )
   {: tx:n has:bool variant:n a:ptr u:n :}
   tx A-TX !
   has if 1 else 0 then A-VAR-F !
   variant A-VAR !
   a A-NAME !
   u A-NAME-U !
   [: START-CORE ;] catch ABORT-THROW ;

: SCHEMA-CORE ( -- )
   A-TX @ A-DRAFT @ REQUIRE-DRAFT {: row:ptr :}
   STAGE-SCHEMA REQUIRE-STAGE-NEW
   A-X @ 0 < A-X @ RAW-SCHEMA-N >= or if E-SCHEMA throw then
   A-Y @ 0 <= if E-LAYOUT throw then
   A-Y @ dup 1 - and 0 <> if E-LAYOUT throw then
   A-Z @ FLAG-MASK invert and 0 <> if E-FLAGS throw then
   A-X @ row ROW.SCHEMA !
   A-Y @ row ROW.ALIGN !
   A-Z @ row ROW.FLAGS !
   STAGE-SCHEMA STAGE+ ;

: LAYOUT-CORE ( -- )
   A-TX @ A-DRAFT @ REQUIRE-DRAFT {: row:ptr :}
   STAGE-LAYOUT REQUIRE-STAGE-NEW
   A-X @ 0 < A-Y @ 0 < or A-Z @ 0 < or A-W @ 0 < or
      if E-LAYOUT throw then
   A-X @ MAX-N A-Y @ - > if E-LAYOUT throw then
   A-Z @ MAX-N A-W @ - > if E-LAYOUT throw then
   A-X @ row ROW.SLOT !
   A-Y @ row ROW.CELLS !
   A-Z @ row ROW.BYTE-OFF !
   A-W @ row ROW.BYTE-SIZE !
   STAGE-LAYOUT STAGE+ ;

: SOURCE-CORE ( -- )
   A-TX @ A-DRAFT @ REQUIRE-DRAFT {: row:ptr :}
   STAGE-SOURCE REQUIRE-STAGE-NEW
   A-X @ 0 < A-Y @ 0 < or A-Z @ 0 < or if E-SOURCE throw then
   A-Y @ MAX-N A-Z @ - > if E-SOURCE throw then
   A-X @ row ROW.SOURCE-ID !
   A-Y @ row ROW.SOURCE-OFF !
   A-Z @ row ROW.SOURCE-LEN !
   STAGE-SOURCE STAGE+ ;

: ADD-CORE ( -- )
   A-TX @ A-DRAFT @ REQUIRE-DRAFT {: row:ptr :}
   TX-TOP-REC {: tx:ptr :}
   tx TX.DRAFT-STATE @ STAGE-DONE <> if E-DRAFT throw then
   row ROW.BYTE-OFF @ row ROW.ALIGN @ mod 0 <> if E-LAYOUT throw then
   -1 tx TX.DRAFT-ROW !
   0 tx TX.DRAFT-TOKEN !
   0 tx TX.DRAFT-STATE ! ;

: COMMIT-CORE ( -- )
   A-TX @ REQUIRE-TX
   TX-TOP-REC TX.DRAFT-ROW @ 0 >= if E-DRAFT throw then
   TX-DEPTH @ 1 - TX-DEPTH !
   TX-DEPTH @ 0= if ROW-N @ COMMIT-N ! then ;

: ROLLBACK-CORE ( -- )
   A-TX @ REQUIRE-TX
   ROLLBACK-TOP ;

public

\ Role constructors validate raw parser/registry outputs before minting.
: FAMILY ( ptr u8 n -- TYPE-FIELD:family-id ) {: a:ptr u:n :}
   RAW-ACTIVE-PKG$ a u RAW-FAMILY-RESOLVE 0= if E-FAMILY throw then
   N>FAMILY ;

: VARIANT-ID ( TYPE-FIELD:family-id ptr u8 n -- TYPE-FIELD:variant-id )
   {: a:ptr u:n :}
   FAMILY>N {: fam:n :}
   fam REQUIRE-VISIBLE
   fam a u RAW-VARIANT-FIND 0= if E-VARIANT throw then
   N>VARIANT ;

: SCHEMA-ID ( n -- TYPE-FIELD:schema-id ) {: n:n :}
   n 0 < n RAW-SCHEMA-N >= or if E-SCHEMA throw then
   n N>SCHEMA ;

: SLOT-ID ( n -- TYPE-FIELD:slot ) {: n:n :}
   n 0 < if E-LAYOUT throw then n N>SLOT ;

: CELL-COUNT ( n -- TYPE-FIELD:cell-count ) {: n:n :}
   n 0 < if E-LAYOUT throw then n N>CELLS ;

: BYTE-OFF ( n -- TYPE-FIELD:byte-off ) {: n:n :}
   n 0 < if E-LAYOUT throw then n N>BYTE-OFF ;

: BYTE-SIZE ( n -- TYPE-FIELD:byte-size ) {: n:n :}
   n 0 < if E-LAYOUT throw then n N>BYTE-SIZE ;

: ALIGNMENT ( n -- TYPE-FIELD:alignment ) {: n:n :}
   n 0 <= if E-LAYOUT throw then
   n dup 1 - and 0 <> if E-LAYOUT throw then
   n N>ALIGN ;

: SOURCE-ID ( n -- TYPE-FIELD:source-id ) {: n:n :}
   n 0 < if E-SOURCE throw then n N>SOURCE-ID ;

: SOURCE-OFF ( n -- TYPE-FIELD:source-off ) {: n:n :}
   n 0 < if E-SOURCE throw then n N>SOURCE-OFF ;

: SOURCE-LEN ( n -- TYPE-FIELD:source-len ) {: n:n :}
   n 0 < if E-SOURCE throw then n N>SOURCE-LEN ;

: PUBLIC-FLAGS ( -- TYPE-FIELD:field-flags )
   FLAG-PUBLIC N>FLAGS ;

: PRIVATE-FLAGS ( -- TYPE-FIELD:field-flags )
   0 N>FLAGS ;

: PUBLIC-BYTE-FLAGS ( -- TYPE-FIELD:field-flags )
   FLAG-PUBLIC FLAG-BYTE or N>FLAGS ;

: PRIVATE-BYTE-FLAGS ( -- TYPE-FIELD:field-flags )
   FLAG-BYTE N>FLAGS ;

: PUBLIC-VIS ( -- TYPE-FIELD:visibility ) RAW-PUBLIC N>VIS ;
: PRIVATE-VIS ( -- TYPE-FIELD:visibility ) RAW-PRIVATE N>VIS ;
: FIELD-COUNT>N ( TYPE-FIELD:field-count -- n ) COUNT>N ;

: FIELD-ID= ( TYPE-FIELD:field-id TYPE-FIELD:field-id -- bool )
   ID>N swap ID>N = ;
: FAMILY= ( TYPE-FIELD:family-id TYPE-FIELD:family-id -- bool )
   FAMILY>N swap FAMILY>N = ;
: VARIANT= ( TYPE-FIELD:variant-id TYPE-FIELD:variant-id -- bool )
   VARIANT>N swap VARIANT>N = ;
: SCHEMA= ( TYPE-FIELD:schema-id TYPE-FIELD:schema-id -- bool )
   SCHEMA>N swap SCHEMA>N = ;
: SLOT= ( TYPE-FIELD:slot TYPE-FIELD:slot -- bool )
   SLOT>N swap SLOT>N = ;
: CELL-COUNT= ( TYPE-FIELD:cell-count TYPE-FIELD:cell-count -- bool )
   CELLS>N swap CELLS>N = ;
: BYTE-OFF= ( TYPE-FIELD:byte-off TYPE-FIELD:byte-off -- bool )
   BYTE-OFF>N swap BYTE-OFF>N = ;
: BYTE-SIZE= ( TYPE-FIELD:byte-size TYPE-FIELD:byte-size -- bool )
   BYTE-SIZE>N swap BYTE-SIZE>N = ;
: ALIGNMENT= ( TYPE-FIELD:alignment TYPE-FIELD:alignment -- bool )
   ALIGN>N swap ALIGN>N = ;
: FLAGS= ( TYPE-FIELD:field-flags TYPE-FIELD:field-flags -- bool )
   FLAGS>N swap FLAGS>N = ;
: SOURCE-ID= ( TYPE-FIELD:source-id TYPE-FIELD:source-id -- bool )
   SOURCE-ID>N swap SOURCE-ID>N = ;
: SOURCE-OFF= ( TYPE-FIELD:source-off TYPE-FIELD:source-off -- bool )
   SOURCE-OFF>N swap SOURCE-OFF>N = ;
: SOURCE-LEN= ( TYPE-FIELD:source-len TYPE-FIELD:source-len -- bool )
   SOURCE-LEN>N swap SOURCE-LEN>N = ;
: VISIBILITY= ( TYPE-FIELD:visibility TYPE-FIELD:visibility -- bool )
   VIS>N swap VIS>N = ;

\ Scoped builder: OPEN binds the transaction to a family owned by the active
\ package. All following roles are nominal, and every failure aborts the whole
\ nested transaction stack before propagating its named error.
: OPEN ( TYPE-FIELD:family-id -- TYPE-FIELD:field-tx )
   FAMILY>N {: fam:n :}
   fam REQUIRE-OWNED
   TX-DEPTH @ 1 + TX-ENSURE
   TX-SERIAL NEXT-SERIAL {: token:n :}
   TX-DEPTH @ TX-REC * TX-P @ + {: tx:ptr :}
   ROW-N @ tx TX.ROW !
   NAME-N @ tx TX.NAME !
   fam tx TX.FAMILY !
   token tx TX.TOKEN !
   -1 tx TX.DRAFT-ROW !
   0 tx TX.DRAFT-TOKEN !
   0 tx TX.DRAFT-STATE !
   TX-DEPTH @ 1 + TX-DEPTH !
   token N>TX ;

: START ( TYPE-FIELD:field-tx ptr u8 n -- TYPE-FIELD:field-tx TYPE-FIELD:field-draft )
   {: a:ptr u:n :}
   TX>N {: tx:n :}
   tx B-FALSE 0 a u START-RUN
   tx N>TX R-DRAFT @ N>DRAFT ;

: START-VARIANT
   ( TYPE-FIELD:field-tx TYPE-FIELD:variant-id ptr u8 n -- TYPE-FIELD:field-tx TYPE-FIELD:field-draft )
   {: a:ptr u:n :}
   VARIANT>N {: variant:n :}
   TX>N {: tx:n :}
   tx B-TRUE variant a u START-RUN
   tx N>TX R-DRAFT @ N>DRAFT ;

: PARSE-START ( TYPE-FIELD:field-tx -- TYPE-FIELD:field-tx TYPE-FIELD:field-draft )
   parse-name START ;

: PARSE-START-VARIANT
   ( TYPE-FIELD:field-tx TYPE-FIELD:variant-id -- TYPE-FIELD:field-tx TYPE-FIELD:field-draft )
   parse-name START-VARIANT ;

: SCHEMA
   ( TYPE-FIELD:field-tx TYPE-FIELD:field-draft TYPE-FIELD:schema-id TYPE-FIELD:alignment TYPE-FIELD:field-flags -- TYPE-FIELD:field-tx TYPE-FIELD:field-draft )
   FLAGS>N A-Z !
   ALIGN>N A-Y !
   SCHEMA>N A-X !
   DRAFT>N A-DRAFT !
   TX>N A-TX !
   [: SCHEMA-CORE ;] catch ABORT-THROW
   A-TX @ N>TX A-DRAFT @ N>DRAFT ;

: LAYOUT
   ( TYPE-FIELD:field-tx TYPE-FIELD:field-draft TYPE-FIELD:slot TYPE-FIELD:cell-count TYPE-FIELD:byte-off TYPE-FIELD:byte-size -- TYPE-FIELD:field-tx TYPE-FIELD:field-draft )
   BYTE-SIZE>N A-W !
   BYTE-OFF>N A-Z !
   CELLS>N A-Y !
   SLOT>N A-X !
   DRAFT>N A-DRAFT !
   TX>N A-TX !
   [: LAYOUT-CORE ;] catch ABORT-THROW
   A-TX @ N>TX A-DRAFT @ N>DRAFT ;

: SOURCE
   ( TYPE-FIELD:field-tx TYPE-FIELD:field-draft TYPE-FIELD:source-id TYPE-FIELD:source-off TYPE-FIELD:source-len -- TYPE-FIELD:field-tx TYPE-FIELD:field-draft )
   SOURCE-LEN>N A-Z !
   SOURCE-OFF>N A-Y !
   SOURCE-ID>N A-X !
   DRAFT>N A-DRAFT !
   TX>N A-TX !
   [: SOURCE-CORE ;] catch ABORT-THROW
   A-TX @ N>TX A-DRAFT @ N>DRAFT ;

: ADD ( TYPE-FIELD:field-tx TYPE-FIELD:field-draft -- TYPE-FIELD:field-tx )
   DRAFT>N A-DRAFT !
   TX>N A-TX !
   [: ADD-CORE ;] catch ABORT-THROW
   A-TX @ N>TX ;

: COMMIT ( TYPE-FIELD:field-tx -- )
   TX>N A-TX !
   [: COMMIT-CORE ;] catch ABORT-THROW ;

: ROLLBACK ( TYPE-FIELD:field-tx -- )
   TX>N A-TX !
   [: ROLLBACK-CORE ;] catch ABORT-THROW ;

: COUNT ( -- TYPE-FIELD:field-count ) COMMIT-N @ N>COUNT ;

private

: FIND-RAW ( n bool n ptr u8 n -- TYPE-FIELD:field-id )
   {: fam:n has:bool variant:n a:ptr u:n :}
   fam REQUIRE-VISIBLE
   has if fam variant REQUIRE-VARIANT then
   0 begin dup COMMIT-N @ < while
      dup fam has variant ROW-OWNER? if
         dup ROW-REC@ a u rot FIELD-NAME= if
            dup LIVE-REC@ ROW-VISIBLE? 0= if drop E-VISIBILITY throw then
            N>ID exit
         then
      then
      1+
   repeat drop E-ID throw ;

: FIELD-REC ( TYPE-FIELD:field-id -- ptr a )
   ID>N LIVE-REC@ dup ROW-VISIBLE? 0= if drop E-VISIBILITY throw then ;

public

: FIND ( TYPE-FIELD:family-id ptr u8 n -- TYPE-FIELD:field-id )
   {: a:ptr u:n :}
   FAMILY>N B-FALSE 0 a u FIND-RAW ;

: FIND-VARIANT
   ( TYPE-FIELD:family-id TYPE-FIELD:variant-id ptr u8 n -- TYPE-FIELD:field-id )
   {: a:ptr u:n :}
   VARIANT>N {: variant:n :}
   FAMILY>N B-TRUE variant a u FIND-RAW ;

: FAMILY@ ( TYPE-FIELD:field-id -- TYPE-FIELD:family-id )
   FIELD-REC ROW.FAMILY @ N>FAMILY ;

: VARIANT? ( TYPE-FIELD:field-id -- bool )
   FIELD-REC ROW.HAS-VARIANT @ 0= 0= ;

: VARIANT@ ( TYPE-FIELD:field-id -- TYPE-FIELD:variant-id )
   FIELD-REC {: row:ptr :}
   row ROW.HAS-VARIANT @ 0= if E-VARIANT throw then
   row ROW.VARIANT @ N>VARIANT ;

: NAME
   ( TYPE-FIELD:field-id ptr u8 TYPE-FIELD:byte-size -- TYPE-FIELD:byte-size )
   BYTE-SIZE>N {: cap:n :}
   {: dst:ptr :}
   ID>N LIVE-REC@ dup ROW-VISIBLE? 0= if drop E-VISIBILITY throw then
   {: row:ptr :}
   row ROW.NAME-LEN @ {: u:n :}
   cap u < if E-CAPACITY throw then
   row ROW.NAME-OFF @ u NAME$ {: src:ptr su:n :}
   0 begin dup su < while
      dup src + c@ over dst + c!
      1+
   repeat drop
   su N>BYTE-SIZE ;

: SCHEMA@ ( TYPE-FIELD:field-id -- TYPE-FIELD:schema-id )
   FIELD-REC ROW.SCHEMA @ N>SCHEMA ;
: SLOT@ ( TYPE-FIELD:field-id -- TYPE-FIELD:slot )
   FIELD-REC ROW.SLOT @ N>SLOT ;
: CELLS@ ( TYPE-FIELD:field-id -- TYPE-FIELD:cell-count )
   FIELD-REC ROW.CELLS @ N>CELLS ;
: BYTE-OFF@ ( TYPE-FIELD:field-id -- TYPE-FIELD:byte-off )
   FIELD-REC ROW.BYTE-OFF @ N>BYTE-OFF ;
: BYTE-SIZE@ ( TYPE-FIELD:field-id -- TYPE-FIELD:byte-size )
   FIELD-REC ROW.BYTE-SIZE @ N>BYTE-SIZE ;
: ALIGN@ ( TYPE-FIELD:field-id -- TYPE-FIELD:alignment )
   FIELD-REC ROW.ALIGN @ N>ALIGN ;
: FLAGS@ ( TYPE-FIELD:field-id -- TYPE-FIELD:field-flags )
   FIELD-REC ROW.FLAGS @ N>FLAGS ;

: VIS@ ( TYPE-FIELD:field-id -- TYPE-FIELD:visibility )
   FIELD-REC ROW.FLAGS @ FLAG-PUBLIC and 0 <> if
      PUBLIC-VIS
   else
      PRIVATE-VIS
   then ;

: SOURCE@
   ( TYPE-FIELD:field-id -- TYPE-FIELD:source-id TYPE-FIELD:source-off TYPE-FIELD:source-len )
   FIELD-REC {: row:ptr :}
   row ROW.SOURCE-ID @ N>SOURCE-ID
   row ROW.SOURCE-OFF @ N>SOURCE-OFF
   row ROW.SOURCE-LEN @ N>SOURCE-LEN ;

: EACH
   ( R TYPE-FIELD:family-id [ R TYPE-FIELD:field-id -- R ] -- R )
   {: q :} \ typed-local-lint: allow-bare-local - quotation preserves R.
   FAMILY>N {: fam:n :}
   fam REQUIRE-VISIBLE
   0 begin dup COMMIT-N @ < while
      dup fam B-FALSE 0 ROW-OWNER? if
         dup LIVE-REC@ ROW-VISIBLE? if dup >r N>ID q execute r> then
      then
      1+
   repeat drop ;

: EACH-VARIANT
   ( R TYPE-FIELD:family-id TYPE-FIELD:variant-id [ R TYPE-FIELD:field-id -- R ] -- R )
   {: q :} \ typed-local-lint: allow-bare-local - quotation preserves R.
   VARIANT>N {: variant:n :}
   FAMILY>N {: fam:n :}
   fam REQUIRE-VISIBLE
   fam variant REQUIRE-VARIANT
   0 begin dup COMMIT-N @ < while
      dup fam B-TRUE variant ROW-OWNER? if
         dup LIVE-REC@ ROW-VISIBLE? if dup >r N>ID q execute r> then
      then
      1+
   repeat drop ;

\ Both sections are immutable after the cold-prefix friend phase. Qualified
\ public reflection remains callable; package reopening and all private state
\ remain sealed.
private
get-current prot-wid-add
public
get-current prot-wid-add
;package
