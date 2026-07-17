\ type-field.f - transactional shared field metadata.
\
\ TYPE-FIELD owns the single ordered field registry used by the unified
\ STRUCTURE and payload ENUM declarers. Mutators stay private so only the
\ post-hook declaration prefix can publish rows; the public surface is typed,
\ read-only reflection over nominal field-id values.

s" lib/adt/option.f" required

7123 constant E-FIELD-TX
7124 constant E-FIELD-DRAFT
7125 constant E-FIELD-FAMILY
7126 constant E-FIELD-VARIANT
7127 constant E-FIELD-NAME
7128 constant E-FIELD-DUP
7129 constant E-FIELD-SCHEMA
7130 constant E-FIELD-LAYOUT
7131 constant E-FIELD-FLAGS
7132 constant E-FIELD-SOURCE
7133 constant E-FIELD-ID
7134 constant E-FIELD-CAPACITY
7135 constant E-FIELD-RANGE
7136 constant E-FIELD-MARK

1 constant FIELD-PUBLIC
2 constant FIELD-BYTE-ADDRESSABLE
FIELD-PUBLIC FIELD-BYTE-ADDRESSABLE or constant FIELD-FLAG-MASK

package TYPE-FIELD
public

s" TYPE-FIELD:field-id" CHECKER-DEFTYPE

private

DEFLINEAR TYPE-FIELD:field-tx
DEFLINEAR TYPE-FIELD:field-draft

s" TYPE-FIELD:mark" CHECKER-DEFTYPE

\ Audited representation boundaries. Runtime values are scalar arena indices
\ or serials; only these private words mint or erase their nominal roles.
TRUSTED: TX>N ( TYPE-FIELD:field-tx -- n ) ;
TRUSTED: N>TX ( n -- TYPE-FIELD:field-tx ) ;
TRUSTED: DRAFT>N ( TYPE-FIELD:field-draft -- n ) ;
TRUSTED: N>DRAFT ( n -- TYPE-FIELD:field-draft ) ;
TRUSTED: ID>N ( TYPE-FIELD:field-id -- n ) ;
TRUSTED: N>ID ( n -- TYPE-FIELD:field-id ) ;
TRUSTED: MARK>N ( TYPE-FIELD:mark -- n ) ;
TRUSTED: N>MARK ( n -- TYPE-FIELD:mark ) ;

\ The registries and allocator load before the checker hook and deliberately
\ expose no user effects. Keep their use inside this sealed private boundary.
TRUSTED: RAW-GROW ( ptr a n n -- ptr a ) ARENA-BYTES-GROW ;
TRUSTED: RAW-FAMILY-N ( -- n ) TFAM-N@ ;
TRUSTED: RAW-VARIANT-N ( -- n ) SUMV-N@ ;
TRUSTED: RAW-VARIANT-FAMILY@ ( n -- n ) SUMV-FAM@ ;
TRUSTED: RAW-SCHEMA-N ( -- n ) SCHEMA-ROOT-N@ ;
TRUSTED: RAW-CANON? ( ptr u8 n -- bool ) TF-CANON? ;
TRUSTED: RAW-KEYWORD? ( ptr u8 n -- bool ) TDECL-KEYWORD? ;
TRUSTED: RAW-PUBLIC ( -- n ) CHECKER-PACKAGE-PUBLIC ;
TRUSTED: RAW-PRIVATE ( -- n ) CHECKER-PACKAGE-PRIVATE ;

$7FFFFFFFFFFFFFFF constant FIELD-MAX-N
4 constant FIELD-CAP-INIT
16 constant FIELD-NAME-INIT

1 constant DRAFT-SCHEMA
2 constant DRAFT-LAYOUT
4 constant DRAFT-SOURCE
DRAFT-SCHEMA DRAFT-LAYOUT or DRAFT-SOURCE or constant DRAFT-COMPLETE

\ Published row: owner key, copied name, schema/layout, flags, and source.
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

\ A draft is a full row plus its owning transaction, unique token, and stages.
15 cells constant DRAFT.TX-OFF
16 cells constant DRAFT.TOKEN-OFF
17 cells constant DRAFT.STATE-OFF
18 cells constant DRAFT-REC

: DRAFT.TX ( ptr a -- ptr a ) DRAFT.TX-OFF + ;
: DRAFT.TOKEN ( ptr a -- ptr a ) DRAFT.TOKEN-OFF + ;
: DRAFT.STATE ( ptr a -- ptr a ) DRAFT.STATE-OFF + ;

\ Transaction frames make nested commit/rollback strict LIFO.
0 cells constant TX.ROW-OFF
1 cells constant TX.NAME-OFF
2 cells constant TX.DRAFT-OFF
3 cells constant TX.TOKEN-OFF
4 cells constant TX-REC

: TX.ROW ( ptr a -- ptr a ) TX.ROW-OFF + ;
: TX.NAME ( ptr a -- ptr a ) TX.NAME-OFF + ;
: TX.DRAFT ( ptr a -- ptr a ) TX.DRAFT-OFF + ;
: TX.TOKEN ( ptr a -- ptr a ) TX.TOKEN-OFF + ;

\ Marks support later checker rollback integration without exposing pointers.
0 cells constant MARK.ROW-OFF
1 cells constant MARK.NAME-OFF
2 cells constant MARK.DRAFT-OFF
3 cells constant MARK.TX-OFF
4 cells constant MARK.TOKEN-OFF
5 cells constant MARK-REC

: MARK.ROW ( ptr a -- ptr a ) MARK.ROW-OFF + ;
: MARK.NAME ( ptr a -- ptr a ) MARK.NAME-OFF + ;
: MARK.DRAFT ( ptr a -- ptr a ) MARK.DRAFT-OFF + ;
: MARK.TX ( ptr a -- ptr a ) MARK.TX-OFF + ;
: MARK.TOKEN ( ptr a -- ptr a ) MARK.TOKEN-OFF + ;

: FIELD-LAYOUT= ( n n -- )
   <> if E-FIELD-LAYOUT throw then ;

ROW.FAMILY-OFF 0 cells FIELD-LAYOUT=
ROW.HAS-VARIANT-OFF 1 cells FIELD-LAYOUT=
ROW.VARIANT-OFF 2 cells FIELD-LAYOUT=
ROW.NAME-OFF-OFF 3 cells FIELD-LAYOUT=
ROW.NAME-LEN-OFF 4 cells FIELD-LAYOUT=
ROW.SCHEMA-OFF 5 cells FIELD-LAYOUT=
ROW.SLOT-OFF 6 cells FIELD-LAYOUT=
ROW.CELLS-OFF 7 cells FIELD-LAYOUT=
ROW.BYTE-OFF-OFF 8 cells FIELD-LAYOUT=
ROW.BYTE-SIZE-OFF 9 cells FIELD-LAYOUT=
ROW.ALIGN-OFF 10 cells FIELD-LAYOUT=
ROW.FLAGS-OFF 11 cells FIELD-LAYOUT=
ROW.SOURCE-ID-OFF 12 cells FIELD-LAYOUT=
ROW.SOURCE-OFF-OFF 13 cells FIELD-LAYOUT=
ROW.SOURCE-LEN-OFF 14 cells FIELD-LAYOUT=
ROW-REC 15 cells FIELD-LAYOUT=
DRAFT.TX-OFF 15 cells FIELD-LAYOUT=
DRAFT.TOKEN-OFF 16 cells FIELD-LAYOUT=
DRAFT.STATE-OFF 17 cells FIELD-LAYOUT=
DRAFT-REC 18 cells FIELD-LAYOUT=
TX.ROW-OFF 0 cells FIELD-LAYOUT=
TX.NAME-OFF 1 cells FIELD-LAYOUT=
TX.DRAFT-OFF 2 cells FIELD-LAYOUT=
TX.TOKEN-OFF 3 cells FIELD-LAYOUT=
TX-REC 4 cells FIELD-LAYOUT=
MARK.ROW-OFF 0 cells FIELD-LAYOUT=
MARK.NAME-OFF 1 cells FIELD-LAYOUT=
MARK.DRAFT-OFF 2 cells FIELD-LAYOUT=
MARK.TX-OFF 3 cells FIELD-LAYOUT=
MARK.TOKEN-OFF 4 cells FIELD-LAYOUT=
MARK-REC 5 cells FIELD-LAYOUT=

create ROW-BOOT FIELD-CAP-INIT ROW-REC * allot
PTR-VARIABLE ROW-P   ROW-BOOT ROW-P !
variable ROW-CAP   FIELD-CAP-INIT ROW-CAP !
variable ROW-N

create DRAFT-BOOT FIELD-CAP-INIT DRAFT-REC * allot
PTR-VARIABLE DRAFT-P   DRAFT-BOOT DRAFT-P !
variable DRAFT-CAP   FIELD-CAP-INIT DRAFT-CAP !
variable DRAFT-N
variable DRAFT-SERIAL

create TX-BOOT FIELD-CAP-INIT TX-REC * allot
PTR-VARIABLE TX-P   TX-BOOT TX-P !
variable TX-CAP   FIELD-CAP-INIT TX-CAP !
variable TX-DEPTH
variable TX-SERIAL

create MARK-BOOT FIELD-CAP-INIT MARK-REC * allot
PTR-VARIABLE MARK-P   MARK-BOOT MARK-P !
variable MARK-CAP   FIELD-CAP-INIT MARK-CAP !
variable MARK-N
variable MARK-SERIAL

create NAME-BOOT FIELD-NAME-INIT allot
PTR-VARIABLE NAME-P   NAME-BOOT NAME-P !
variable NAME-CAP   FIELD-NAME-INIT NAME-CAP !
variable NAME-N

: NEXT-CAP ( n n n -- n ) {: need:n cap:n stride:n :}
   need 0 < stride 0 <= or if E-FIELD-CAPACITY throw then
   FIELD-MAX-N stride / {: limit:n :}
   need limit > if E-FIELD-CAPACITY throw then
   cap limit 2 / > if need else cap 2 * need max then ;

: ROW-ENSURE ( n -- ) {: need:n :}
   need ROW-CAP @ <= if exit then
   need ROW-CAP @ ROW-REC NEXT-CAP {: cap:n :}
   ROW-P @ ROW-CAP @ ROW-REC * cap ROW-REC * RAW-GROW ROW-P !
   cap ROW-CAP ! ;

: DRAFT-ENSURE ( n -- ) {: need:n :}
   need DRAFT-CAP @ <= if exit then
   need DRAFT-CAP @ DRAFT-REC NEXT-CAP {: cap:n :}
   DRAFT-P @ DRAFT-CAP @ DRAFT-REC * cap DRAFT-REC * RAW-GROW DRAFT-P !
   cap DRAFT-CAP ! ;

: TX-ENSURE ( n -- ) {: need:n :}
   need TX-CAP @ <= if exit then
   need TX-CAP @ TX-REC NEXT-CAP {: cap:n :}
   TX-P @ TX-CAP @ TX-REC * cap TX-REC * RAW-GROW TX-P !
   cap TX-CAP ! ;

: MARK-ENSURE ( n -- ) {: need:n :}
   need MARK-CAP @ <= if exit then
   need MARK-CAP @ MARK-REC NEXT-CAP {: cap:n :}
   MARK-P @ MARK-CAP @ MARK-REC * cap MARK-REC * RAW-GROW MARK-P !
   cap MARK-CAP ! ;

: NAME-ENSURE ( n -- ) {: add:n :}
   add 0 < if E-FIELD-CAPACITY throw then
   NAME-N @ FIELD-MAX-N add - > if E-FIELD-CAPACITY throw then
   NAME-N @ add + {: need:n :}
   need NAME-CAP @ <= if exit then
   need NAME-CAP @ 1 NEXT-CAP {: cap:n :}
   NAME-P @ NAME-CAP @ cap RAW-GROW NAME-P !
   cap NAME-CAP ! ;

: ROW-REC@ ( n -- ptr a ) {: id:n :}
   id 0 < id ROW-N @ >= or if E-FIELD-ID throw then
   id ROW-REC * ROW-P @ + ;

: DRAFT-REC@ ( n -- ptr a ) {: id:n :}
   id 0 < id DRAFT-N @ >= or if E-FIELD-DRAFT throw then
   id DRAFT-REC * DRAFT-P @ + ;

: TX-REC@ ( n -- ptr a ) {: id:n :}
   id 0 < id TX-DEPTH @ >= or if E-FIELD-TX throw then
   id TX-REC * TX-P @ + ;

: MARK-REC@ ( n -- ptr a ) {: id:n :}
   id 0 < id MARK-N @ >= or if E-FIELD-MARK throw then
   id MARK-REC * MARK-P @ + ;

: NAME-BASE ( -- ptr u8 ) NAME-P @ BYTE-VIEW ;

: NAME$ ( n n -- ptr u8 n ) {: off:n u:n :}
   off 0 < u 0 < or if E-FIELD-ID throw then
   off NAME-N @ > NAME-N @ off - u < or if E-FIELD-ID throw then
   NAME-BASE off + u ;

: ASCII-FOLD ( n -- n ) {: c:n :}
   c 65 >= c 90 <= and if c 32 + else c then ;

: FIELD-CI= ( ptr u8 n ptr u8 n -- bool ) {: a:ptr au:n b:ptr bu:n :}
   au bu <> if 0 0= 0= exit then
   0 begin dup au < while
      dup a + c@ ASCII-FOLD over b + c@ ASCII-FOLD <> if
         drop 0 0= 0= exit
      then
      1+
   repeat drop 0 0= ;

: FIELD-NAME= ( ptr u8 n ptr a -- bool ) {: a:ptr u:n row:ptr :}
   row ROW.NAME-OFF @ row ROW.NAME-LEN @ NAME$ a u FIELD-CI= ;

: GENERATED-NAME? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" make" FIELD-CI= if 0 0= exit then
   a u s" unmake" FIELD-CI= if 0 0= exit then
   a u s" tag" FIELD-CI= if 0 0= exit then
   a u s" eq" FIELD-CI= if 0 0= exit then
   a u s" hash" FIELD-CI= if 0 0= exit then
   a u s" order" FIELD-CI= if 0 0= exit then
   a u s" encode" FIELD-CI= if 0 0= exit then
   a u s" decode" FIELD-CI= ;

: FIELD-RESERVED? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u RAW-KEYWORD? if 0 0= exit then
   a u s" structure" FIELD-CI= if 0 0= exit then
   a u s" ;structure" FIELD-CI= if 0 0= exit then
   a u GENERATED-NAME? ;

: REQUIRE-NAME ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0= if E-FIELD-NAME throw then
   a u RAW-CANON? 0= if E-FIELD-NAME throw then
   a u FIELD-RESERVED? if E-FIELD-NAME throw then ;

: REQUIRE-FAMILY ( n -- ) {: fam:n :}
   fam 0 < fam RAW-FAMILY-N >= or if E-FIELD-FAMILY throw then ;

: REQUIRE-OWNER ( n bool n -- ) {: fam:n has:bool variant:n :}
   fam REQUIRE-FAMILY
   has if
      variant 0 < variant RAW-VARIANT-N >= or if E-FIELD-VARIANT throw then
      variant RAW-VARIANT-FAMILY@ fam <> if E-FIELD-VARIANT throw then
   else
      variant 0 <> if E-FIELD-VARIANT throw then
   then ;

: ROW-OWNER? ( n n bool n -- bool ) {: id:n fam:n has:bool variant:n :}
   id ROW-REC@ {: row:ptr :}
   row ROW.FAMILY @ fam =
   row ROW.HAS-VARIANT @ has if 1 else 0 then = and
   row ROW.VARIANT @ variant = and ;

: OWNER-SEEN? ( n bool n -- bool ) {: fam:n has:bool variant:n :}
   0 begin dup ROW-N @ < while
      dup fam has variant ROW-OWNER? if drop 0 0= exit then
      1+
   repeat drop 0 0= 0= ;

: REQUIRE-CONTIGUOUS ( n bool n -- ) {: fam:n has:bool variant:n :}
   fam has variant OWNER-SEEN? 0= if exit then
   ROW-N @ 1 - fam has variant ROW-OWNER? 0= if E-FIELD-RANGE throw then ;

: REQUIRE-UNIQUE ( n bool n ptr u8 n -- )
   {: fam:n has:bool variant:n a:ptr u:n :}
   0 begin dup ROW-N @ < while
      dup fam has variant ROW-OWNER? if
         dup ROW-REC@ a u rot FIELD-NAME= if drop E-FIELD-DUP throw then
      then
      1+
   repeat drop ;

: COPY-NAME ( ptr u8 n -- n ) {: a:ptr u:n :}
   u NAME-ENSURE
   NAME-N @ {: off:n :}
   0 begin dup u < while
      dup a + c@ over NAME-BASE off + + c!
      1+
   repeat drop
   NAME-N @ u + NAME-N !
   off ;

: NEXT-SERIAL ( ptr n -- n ) {: cell:ptr :}
   cell @ FIELD-MAX-N = if E-FIELD-CAPACITY throw then
   cell @ 1 + dup cell ! ;

: TX-TOP-REC ( -- ptr a )
   TX-DEPTH @ 0= if E-FIELD-TX throw then
   TX-DEPTH @ 1 - TX-REC@ ;

: TX-TOP? ( n -- bool ) {: token:n :}
   TX-DEPTH @ 0= if 0 0= 0= exit then
   TX-TOP-REC TX.TOKEN @ token = ;

: REQUIRE-TX ( n -- )
   TX-TOP? 0= if E-FIELD-TX throw then ;

: ROLLBACK-RAW ( n -- ) {: token:n :}
   token REQUIRE-TX
   TX-TOP-REC {: tx:ptr :}
   tx TX.ROW @ ROW-N !
   tx TX.NAME @ NAME-N !
   tx TX.DRAFT @ DRAFT-N !
   TX-DEPTH @ 1 - TX-DEPTH ! ;

: REQUIRE-DRAFT ( n n -- ptr a ) {: tx-token:n draft-token:n :}
   tx-token REQUIRE-TX
   DRAFT-N @ TX-TOP-REC TX.DRAFT @ <= if E-FIELD-DRAFT throw then
   DRAFT-N @ 1 - DRAFT-REC@ {: draft:ptr :}
   draft DRAFT.TX @ tx-token <> if E-FIELD-DRAFT throw then
   draft DRAFT.TOKEN @ draft-token <> if E-FIELD-DRAFT throw then
   draft ;

: REQUIRE-STAGE-NEW ( ptr a n -- ) {: draft:ptr stage:n :}
   draft DRAFT.STATE @ stage and 0 <> if E-FIELD-DRAFT throw then ;

variable ARG-TX
variable ARG-DRAFT
variable ARG-FAMILY
variable ARG-HAS-VARIANT
variable ARG-VARIANT
variable ARG-NAME-A
variable ARG-NAME-U
variable ARG-A
variable ARG-B
variable ARG-C
variable ARG-D
variable RESULT-DRAFT
variable RESULT-ID

: ABORT-THROW ( n -- ) {: code:n :}
   code 0= if exit then
   ARG-TX @ TX-TOP? if ARG-TX @ ROLLBACK-RAW then
   code throw ;

: OPEN ( -- TYPE-FIELD:field-tx )
   TX-DEPTH @ 1 + TX-ENSURE
   TX-SERIAL NEXT-SERIAL {: token:n :}
   TX-DEPTH @ TX-REC * TX-P @ + {: tx:ptr :}
   ROW-N @ tx TX.ROW !
   NAME-N @ tx TX.NAME !
   DRAFT-N @ tx TX.DRAFT !
   token tx TX.TOKEN !
   TX-DEPTH @ 1 + TX-DEPTH !
   token N>TX ;

: START-CORE ( -- )
   ARG-TX @ REQUIRE-TX
   DRAFT-N @ TX-TOP-REC TX.DRAFT @ <> if E-FIELD-DRAFT throw then
   ARG-FAMILY @ ARG-HAS-VARIANT @ 0= 0= ARG-VARIANT @ REQUIRE-OWNER
   ARG-NAME-A @ ARG-NAME-U @ REQUIRE-NAME
   ARG-FAMILY @ ARG-HAS-VARIANT @ 0= 0= ARG-VARIANT @ REQUIRE-CONTIGUOUS
   ARG-FAMILY @ ARG-HAS-VARIANT @ 0= 0= ARG-VARIANT @
      ARG-NAME-A @ ARG-NAME-U @ REQUIRE-UNIQUE
   ARG-NAME-U @ NAME-ENSURE
   DRAFT-N @ 1 + DRAFT-ENSURE
   ARG-NAME-A @ ARG-NAME-U @ COPY-NAME {: name-off:n :}
   DRAFT-SERIAL NEXT-SERIAL {: token:n :}
   DRAFT-N @ DRAFT-REC * DRAFT-P @ + {: draft:ptr :}
   ARG-FAMILY @ draft ROW.FAMILY !
   ARG-HAS-VARIANT @ draft ROW.HAS-VARIANT !
   ARG-VARIANT @ draft ROW.VARIANT !
   name-off draft ROW.NAME-OFF !
   ARG-NAME-U @ draft ROW.NAME-LEN !
   0 draft ROW.SCHEMA !
   0 draft ROW.SLOT !
   0 draft ROW.CELLS !
   0 draft ROW.BYTE-OFF !
   0 draft ROW.BYTE-SIZE !
   0 draft ROW.ALIGN !
   0 draft ROW.FLAGS !
   0 draft ROW.SOURCE-ID !
   0 draft ROW.SOURCE-OFF !
   0 draft ROW.SOURCE-LEN !
   ARG-TX @ draft DRAFT.TX !
   token draft DRAFT.TOKEN !
   0 draft DRAFT.STATE !
   DRAFT-N @ 1 + DRAFT-N !
   token RESULT-DRAFT ! ;

: START ( TYPE-FIELD:field-tx n bool n ptr u8 n -- TYPE-FIELD:field-tx TYPE-FIELD:field-draft )
   {: fam:n has:bool variant:n a:ptr u:n :}
   TX>N ARG-TX !
   fam ARG-FAMILY !
   has if 1 else 0 then ARG-HAS-VARIANT !
   variant ARG-VARIANT !
   a ARG-NAME-A !
   u ARG-NAME-U !
   [: START-CORE ;] catch ABORT-THROW
   ARG-TX @ N>TX RESULT-DRAFT @ N>DRAFT ;

: SCHEMA-CORE ( -- )
   ARG-TX @ ARG-DRAFT @ REQUIRE-DRAFT {: draft:ptr :}
   draft DRAFT-SCHEMA REQUIRE-STAGE-NEW
   ARG-A @ 0 < ARG-A @ RAW-SCHEMA-N >= or if E-FIELD-SCHEMA throw then
   ARG-B @ 0 <= if E-FIELD-LAYOUT throw then
   ARG-B @ dup 1 - and 0 <> if E-FIELD-LAYOUT throw then
   ARG-C @ FIELD-FLAG-MASK invert and 0 <> if E-FIELD-FLAGS throw then
   ARG-A @ draft ROW.SCHEMA !
   ARG-B @ draft ROW.ALIGN !
   ARG-C @ draft ROW.FLAGS !
   draft DRAFT.STATE dup @ DRAFT-SCHEMA or swap ! ;

: SCHEMA ( TYPE-FIELD:field-tx TYPE-FIELD:field-draft n n n -- TYPE-FIELD:field-tx TYPE-FIELD:field-draft )
   {: root:n align:n flags:n :}
   DRAFT>N ARG-DRAFT !
   TX>N ARG-TX !
   root ARG-A !
   align ARG-B !
   flags ARG-C !
   [: SCHEMA-CORE ;] catch ABORT-THROW
   ARG-TX @ N>TX ARG-DRAFT @ N>DRAFT ;

: LAYOUT-CORE ( -- )
   ARG-TX @ ARG-DRAFT @ REQUIRE-DRAFT {: draft:ptr :}
   draft DRAFT-LAYOUT REQUIRE-STAGE-NEW
   ARG-A @ 0 < ARG-B @ 0 < or ARG-C @ 0 < or ARG-D @ 0 < or
      if E-FIELD-LAYOUT throw then
   ARG-A @ FIELD-MAX-N ARG-B @ - > if E-FIELD-LAYOUT throw then
   ARG-C @ FIELD-MAX-N ARG-D @ - > if E-FIELD-LAYOUT throw then
   ARG-A @ draft ROW.SLOT !
   ARG-B @ draft ROW.CELLS !
   ARG-C @ draft ROW.BYTE-OFF !
   ARG-D @ draft ROW.BYTE-SIZE !
   draft DRAFT.STATE dup @ DRAFT-LAYOUT or swap ! ;

: LAYOUT ( TYPE-FIELD:field-tx TYPE-FIELD:field-draft n n n n -- TYPE-FIELD:field-tx TYPE-FIELD:field-draft )
   {: slot:n cells:n byte-off:n byte-size:n :}
   DRAFT>N ARG-DRAFT !
   TX>N ARG-TX !
   slot ARG-A !
   cells ARG-B !
   byte-off ARG-C !
   byte-size ARG-D !
   [: LAYOUT-CORE ;] catch ABORT-THROW
   ARG-TX @ N>TX ARG-DRAFT @ N>DRAFT ;

: SOURCE-CORE ( -- )
   ARG-TX @ ARG-DRAFT @ REQUIRE-DRAFT {: draft:ptr :}
   draft DRAFT-SOURCE REQUIRE-STAGE-NEW
   ARG-A @ 0 < ARG-B @ 0 < or ARG-C @ 0 < or if E-FIELD-SOURCE throw then
   ARG-B @ FIELD-MAX-N ARG-C @ - > if E-FIELD-SOURCE throw then
   ARG-A @ draft ROW.SOURCE-ID !
   ARG-B @ draft ROW.SOURCE-OFF !
   ARG-C @ draft ROW.SOURCE-LEN !
   draft DRAFT.STATE dup @ DRAFT-SOURCE or swap ! ;

: SOURCE ( TYPE-FIELD:field-tx TYPE-FIELD:field-draft n n n -- TYPE-FIELD:field-tx TYPE-FIELD:field-draft )
   {: source:n off:n u:n :}
   DRAFT>N ARG-DRAFT !
   TX>N ARG-TX !
   source ARG-A !
   off ARG-B !
   u ARG-C !
   [: SOURCE-CORE ;] catch ABORT-THROW
   ARG-TX @ N>TX ARG-DRAFT @ N>DRAFT ;

: DRAFT>ROW ( ptr a ptr a -- ) {: draft:ptr row:ptr :}
   draft ROW.FAMILY @ row ROW.FAMILY !
   draft ROW.HAS-VARIANT @ row ROW.HAS-VARIANT !
   draft ROW.VARIANT @ row ROW.VARIANT !
   draft ROW.NAME-OFF @ row ROW.NAME-OFF !
   draft ROW.NAME-LEN @ row ROW.NAME-LEN !
   draft ROW.SCHEMA @ row ROW.SCHEMA !
   draft ROW.SLOT @ row ROW.SLOT !
   draft ROW.CELLS @ row ROW.CELLS !
   draft ROW.BYTE-OFF @ row ROW.BYTE-OFF !
   draft ROW.BYTE-SIZE @ row ROW.BYTE-SIZE !
   draft ROW.ALIGN @ row ROW.ALIGN !
   draft ROW.FLAGS @ row ROW.FLAGS !
   draft ROW.SOURCE-ID @ row ROW.SOURCE-ID !
   draft ROW.SOURCE-OFF @ row ROW.SOURCE-OFF !
   draft ROW.SOURCE-LEN @ row ROW.SOURCE-LEN ! ;

: ADD-CORE ( -- )
   ARG-TX @ ARG-DRAFT @ REQUIRE-DRAFT {: draft:ptr :}
   draft DRAFT.STATE @ DRAFT-COMPLETE <> if E-FIELD-DRAFT throw then
   draft ROW.BYTE-OFF @ draft ROW.ALIGN @ mod 0 <> if E-FIELD-LAYOUT throw then
   ROW-N @ 1 + ROW-ENSURE
   ROW-N @ {: id:n :}
   id ROW-REC * ROW-P @ + {: row:ptr :}
   draft row DRAFT>ROW
   id 1 + ROW-N !
   DRAFT-N @ 1 - DRAFT-N !
   id RESULT-ID ! ;

: ADD ( TYPE-FIELD:field-tx TYPE-FIELD:field-draft -- TYPE-FIELD:field-tx TYPE-FIELD:field-id )
   DRAFT>N ARG-DRAFT !
   TX>N ARG-TX !
   [: ADD-CORE ;] catch ABORT-THROW
   ARG-TX @ N>TX RESULT-ID @ N>ID ;

: COMMIT-CORE ( -- )
   ARG-TX @ REQUIRE-TX
   DRAFT-N @ TX-TOP-REC TX.DRAFT @ <> if E-FIELD-DRAFT throw then
   TX-DEPTH @ 1 - TX-DEPTH ! ;

: COMMIT ( TYPE-FIELD:field-tx -- )
   TX>N ARG-TX !
   [: COMMIT-CORE ;] catch ABORT-THROW ;

: ROLLBACK ( TYPE-FIELD:field-tx -- )
   TX>N ROLLBACK-RAW ;

: MARK ( -- TYPE-FIELD:mark )
   MARK-N @ 1 + MARK-ENSURE
   MARK-SERIAL NEXT-SERIAL {: token:n :}
   MARK-N @ MARK-REC * MARK-P @ + {: mark:ptr :}
   ROW-N @ mark MARK.ROW !
   NAME-N @ mark MARK.NAME !
   DRAFT-N @ mark MARK.DRAFT !
   TX-DEPTH @ mark MARK.TX !
   token mark MARK.TOKEN !
   MARK-N @ 1 + MARK-N !
   token N>MARK ;

: RESTORE ( TYPE-FIELD:mark -- )
   MARK>N {: token:n :}
   MARK-N @ 0= if E-FIELD-MARK throw then
   MARK-N @ 1 - MARK-REC@ {: mark:ptr :}
   mark MARK.TOKEN @ token <> if E-FIELD-MARK throw then
   mark MARK.ROW @ ROW-N @ > if E-FIELD-MARK throw then
   mark MARK.NAME @ NAME-N @ > if E-FIELD-MARK throw then
   mark MARK.DRAFT @ DRAFT-N @ > if E-FIELD-MARK throw then
   mark MARK.TX @ TX-DEPTH @ > if E-FIELD-MARK throw then
   mark MARK.ROW @ ROW-N !
   mark MARK.NAME @ NAME-N !
   mark MARK.DRAFT @ DRAFT-N !
   mark MARK.TX @ TX-DEPTH !
   MARK-N @ 1 - MARK-N ! ;

public

: COUNT ( -- n ) ROW-N @ ;

: FIND ( n bool n ptr u8 n -- option<TYPE-FIELD:field-id> )
   {: fam:n has:bool variant:n a:ptr u:n :}
   fam has variant REQUIRE-OWNER
   0 begin dup ROW-N @ < while
      dup fam has variant ROW-OWNER? if
         dup ROW-REC@ a u rot FIELD-NAME= if N>ID OPTION:SOME exit then
      then
      1+
   repeat drop OPTION:NONE ;

private

: FIELD-REC ( TYPE-FIELD:field-id -- ptr a )
   ID>N ROW-REC@ ;

public

: FAMILY@ ( TYPE-FIELD:field-id -- n ) FIELD-REC ROW.FAMILY @ ;
: VARIANT? ( TYPE-FIELD:field-id -- bool ) FIELD-REC ROW.HAS-VARIANT @ 0= 0= ;
: VARIANT@ ( TYPE-FIELD:field-id -- n )
   FIELD-REC {: row:ptr :}
   row ROW.HAS-VARIANT @ 0= if E-FIELD-VARIANT throw then
   row ROW.VARIANT @ ;

: NAME ( TYPE-FIELD:field-id ptr u8 n -- n ) {: dst:ptr cap:n :}
   cap 0 < if E-FIELD-CAPACITY throw then
   ID>N ROW-REC@ {: row:ptr :}
   row ROW.NAME-LEN @ {: u:n :}
   cap u < if E-FIELD-CAPACITY throw then
   row ROW.NAME-OFF @ u NAME$ {: src:ptr su:n :}
   0 begin dup su < while
      dup src + c@ over dst + c!
      1+
   repeat drop
   su ;

: SCHEMA@ ( TYPE-FIELD:field-id -- n ) FIELD-REC ROW.SCHEMA @ ;
: SLOT@ ( TYPE-FIELD:field-id -- n ) FIELD-REC ROW.SLOT @ ;
: CELLS@ ( TYPE-FIELD:field-id -- n ) FIELD-REC ROW.CELLS @ ;
: BYTE-OFF@ ( TYPE-FIELD:field-id -- n ) FIELD-REC ROW.BYTE-OFF @ ;
: BYTE-SIZE@ ( TYPE-FIELD:field-id -- n ) FIELD-REC ROW.BYTE-SIZE @ ;
: ALIGN@ ( TYPE-FIELD:field-id -- n ) FIELD-REC ROW.ALIGN @ ;
: FLAGS@ ( TYPE-FIELD:field-id -- n ) FIELD-REC ROW.FLAGS @ ;
: VIS@ ( TYPE-FIELD:field-id -- n )
   FLAGS@ FIELD-PUBLIC and 0 <> if
      RAW-PUBLIC
   else
      RAW-PRIVATE
   then ;

: SOURCE@ ( TYPE-FIELD:field-id -- n n n )
   FIELD-REC {: row:ptr :}
   row ROW.SOURCE-ID @
   row ROW.SOURCE-OFF @
   row ROW.SOURCE-LEN @ ;

: EACH ( R n bool n [ R TYPE-FIELD:field-id -- R ] -- R )
   {: fam:n has:bool variant:n q :} \ typed-local-lint: allow-bare-local - quotation bound as ordinary local (docs/forth.md)
   fam has variant REQUIRE-OWNER
   0 begin dup ROW-N @ < while
      dup fam has variant ROW-OWNER? if
         dup >r N>ID q execute r>
      then
      1+
   repeat drop ;

\ Seal both package sections after the complete provider has published.
private
get-current prot-wid-add
public
get-current prot-wid-add
;package
