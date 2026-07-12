\ layout-valid.f — full lowering-certificate producer.
\ Reopens LOWER-CERT after type-family metadata is available and replaces the
\ boot-safe scalar producer with the canonical width/fetch serializer.

package LOWER-CERT

$80 constant FETCH-INIT
$200 constant DATA-INIT
$100 constant ENV-INIT
$100 constant GUARD-INIT
$100 constant TASK-INIT

create DATA-BOOT DATA-INIT cells allot
PTR-VARIABLE DATA-P   DATA-BOOT DATA-P !
variable DATA-CAP DATA-INIT DATA-CAP !
variable DATA-N

create ENV-BOOT ENV-INIT cells allot
PTR-VARIABLE ENV-P   ENV-BOOT ENV-P !
variable ENV-CAP ENV-INIT ENV-CAP !

create GUARD-OFF-BOOT GUARD-INIT cells allot
create GUARD-TAG-BOOT GUARD-INIT cells allot
create GUARD-LIM-BOOT GUARD-INIT cells allot
PTR-VARIABLE GUARD-OFF-P   GUARD-OFF-BOOT GUARD-OFF-P !
PTR-VARIABLE GUARD-TAG-P   GUARD-TAG-BOOT GUARD-TAG-P !
PTR-VARIABLE GUARD-LIM-P   GUARD-LIM-BOOT GUARD-LIM-P !
variable GUARD-CAP     GUARD-INIT GUARD-CAP !
variable GUARD-N

create TASK-KIND-BOOT TASK-INIT cells allot
create TASK-A-BOOT TASK-INIT cells allot
create TASK-B-BOOT TASK-INIT cells allot
create TASK-C-BOOT TASK-INIT cells allot
PTR-VARIABLE TASK-KIND-P   TASK-KIND-BOOT TASK-KIND-P !
PTR-VARIABLE TASK-A-P      TASK-A-BOOT TASK-A-P !
PTR-VARIABLE TASK-B-P      TASK-B-BOOT TASK-B-P !
PTR-VARIABLE TASK-C-P      TASK-C-BOOT TASK-C-P !
variable TASK-CAP      TASK-INIT TASK-CAP !
variable TASK-N

create FETCH-KEY-BOOT FETCH-INIT cells allot
create FETCH-OFF-BOOT FETCH-INIT cells allot
create FETCH-LEN-BOOT FETCH-INIT cells allot
PTR-VARIABLE FETCH-KEY-P   FETCH-KEY-BOOT FETCH-KEY-P !
PTR-VARIABLE FETCH-OFF-P   FETCH-OFF-BOOT FETCH-OFF-P !
PTR-VARIABLE FETCH-LEN-P   FETCH-LEN-BOOT FETCH-LEN-P !
variable FETCH-CAP     FETCH-INIT FETCH-CAP !
variable FETCH-N
variable HAS-VALID

variable CHECKS
variable SUM-I
variable PAY-I
variable FIELD-I

0 constant WALK-TASK
1 constant GUARD-TASK
2 constant UNGUARD-TASK

: DATA ( -- ptr a ) DATA-P @ ;
: ENV ( -- ptr a ) ENV-P @ ;
: GUARD-OFF ( -- ptr a ) GUARD-OFF-P @ ;
: GUARD-TAG ( -- ptr a ) GUARD-TAG-P @ ;
: GUARD-LIM ( -- ptr a ) GUARD-LIM-P @ ;
: TASK-KIND ( -- ptr a ) TASK-KIND-P @ ;
: TASK-A ( -- ptr a ) TASK-A-P @ ;
: TASK-B ( -- ptr a ) TASK-B-P @ ;
: TASK-C ( -- ptr a ) TASK-C-P @ ;
: FETCH-KEY ( -- ptr a ) FETCH-KEY-P @ ;
: FETCH-OFF ( -- ptr a ) FETCH-OFF-P @ ;
: FETCH-LEN ( -- ptr a ) FETCH-LEN-P @ ;
: DATA-ENSURE ( n -- ) {: need:n :}
   need DATA-CAP @ <= if exit then
   need DATA-CAP @ GROW-CAP {: cap:n :}
   DATA-P @ DATA-CAP @ cells cap cells ARENA-BYTES-GROW DATA-P !
   cap DATA-CAP ! ;

: ENV-ENSURE ( n -- ) {: need:n :}
   need ENV-CAP @ <= if exit then
   need ENV-CAP @ GROW-CAP {: cap:n :}
   ENV-P @ ENV-CAP @ cells cap cells ARENA-BYTES-GROW ENV-P !
   cap ENV-CAP ! ;

: GUARD-ENSURE ( n -- ) {: need:n :}
   need GUARD-CAP @ <= if exit then
   need GUARD-CAP @ GROW-CAP {: cap:n :}
   GUARD-OFF-P @ GUARD-CAP @ cells cap cells ARENA-BYTES-GROW GUARD-OFF-P !
   GUARD-TAG-P @ GUARD-CAP @ cells cap cells ARENA-BYTES-GROW GUARD-TAG-P !
   GUARD-LIM-P @ GUARD-CAP @ cells cap cells ARENA-BYTES-GROW GUARD-LIM-P !
   cap GUARD-CAP ! ;

: TASK-ENSURE ( n -- ) {: need:n :}
   need TASK-CAP @ <= if exit then
   need TASK-CAP @ GROW-CAP {: cap:n :}
   TASK-KIND-P @ TASK-CAP @ cells cap cells ARENA-BYTES-GROW TASK-KIND-P !
   TASK-A-P @ TASK-CAP @ cells cap cells ARENA-BYTES-GROW TASK-A-P !
   TASK-B-P @ TASK-CAP @ cells cap cells ARENA-BYTES-GROW TASK-B-P !
   TASK-C-P @ TASK-CAP @ cells cap cells ARENA-BYTES-GROW TASK-C-P !
   cap TASK-CAP ! ;

: FETCH-ENSURE ( n -- ) {: need:n :}
   need FETCH-CAP @ <= if exit then
   need FETCH-CAP @ GROW-CAP {: cap:n :}
   FETCH-KEY-P @ FETCH-CAP @ cells cap cells ARENA-BYTES-GROW FETCH-KEY-P !
   FETCH-OFF-P @ FETCH-CAP @ cells cap cells ARENA-BYTES-GROW FETCH-OFF-P !
   FETCH-LEN-P @ FETCH-CAP @ cells cap cells ARENA-BYTES-GROW FETCH-LEN-P !
   cap FETCH-CAP ! ;

: DATA, ( n -- )
   DATA-N @ 1 + DATA-ENSURE
   DATA-N @ cells DATA + !
   DATA-N @ 1 + DATA-N ! ;

: ENV! ( n n -- ) {: idx:n term:n :}
   idx 0 < if s" lowering certificate env index" 76 die then
   idx 1 + ENV-ENSURE
   term idx cells ENV + ! ;

: ENV@ ( n -- n ) {: idx:n :}
   idx 0 < idx ENV-CAP @ >= or if s" lowering certificate env index" 76 die then
   idx cells ENV + @ ;

: ENV-TERM! ( n -- ) {: term0:n :}
   term0 T-RES {: term:n :}
   term PARAM>ARGC ENV-ENSURE
   0 begin dup term PARAM>ARGC < while
      term over PARAM>ARG over ENV!
      1 +
   repeat drop ;

: SCHEMA-TERM ( n -- n ) {: node:n :}
   node SCHEMA-PARAM? if node SCHEMA-A@ ENV@ exit then
   node SCHEMA-CON? if node SCHEMA-A@ MK-CON exit then
   node SCHEMA-PTR? if node SCHEMA-A@ recurse MK-PTR exit then
   node SCHEMA-APP? if
      PARAM-SCR-N @ {: base:n :}
      0 begin dup node SCHEMA-C@ < while
         node SCHEMA-B@ over + SCHEMA-ROOT@ recurse PARAM-SCR+
         1 +
      repeat drop
      base node SCHEMA-A@ TFAM-NAME$ node SCHEMA-A@ MK-PARAM exit
   then
   s" lowering certificate unsupported schema" 76 die ;

: GUARD-PUSH ( n n n -- ) {: off:n tag:n limit:n :}
   off 0 < if s" lowering certificate negative guard offset" 76 die then
   limit 0 <= if s" lowering certificate empty guard domain" 76 die then
   tag 0 < tag limit >= or if s" lowering certificate guard tag outside domain" 76 die then
   GUARD-N @ 1 + GUARD-ENSURE
   off GUARD-N @ cells GUARD-OFF + !
   tag GUARD-N @ cells GUARD-TAG + !
   limit GUARD-N @ cells GUARD-LIM + !
   GUARD-N @ 1 + GUARD-N ! ;

: GUARD-POP ( -- )
   GUARD-N @ 0= if s" lowering certificate guard underflow" 76 die then
   GUARD-N @ 1 - GUARD-N ! ;

: CHECK, ( n n -- ) {: off:n fam:n :}
   off 0 < if s" lowering certificate negative check offset" 76 die then
   fam TFAM-VAR-COUNT@ {: limit:n :}
   limit 0 <= if s" lowering certificate empty tag domain" 76 die then
   off DATA,  limit DATA,  GUARD-N @ DATA,
   0 begin dup GUARD-N @ < while
      dup cells GUARD-OFF + @ DATA,
      dup cells GUARD-TAG + @ DATA,
      dup cells GUARD-LIM + @ DATA,
      1 +
   repeat drop
   CHECKS @ 1 + CHECKS ! ;

: TASK-PUSH ( n n n n -- ) {: kind:n a:n b:n c:n :}
   TASK-N @ 1 + TASK-ENSURE
   kind TASK-N @ cells TASK-KIND + !
   a TASK-N @ cells TASK-A + !
   b TASK-N @ cells TASK-B + !
   c TASK-N @ cells TASK-C + !
   TASK-N @ 1 + TASK-N ! ;

: TASK-POP ( -- n n n n )
   TASK-N @ 0= if s" lowering certificate task underflow" 76 die then
   TASK-N @ 1 - dup TASK-N ! {: idx:n :}
   idx cells TASK-KIND + @
   idx cells TASK-A + @
   idx cells TASK-B + @
   idx cells TASK-C + @ ;

: VARIANT-OFF ( n n n n -- n ) {: term:n vid:n idx:n base:n :}
   term ENV-TERM!
   base 0 begin dup idx < while
      vid SUMV-SCH-START@ over + SCHEMA-ROOT@ SCHEMA-TERM T-WIDTH
      rot + swap 1 +
   repeat drop ;

: QUEUE-VARIANT ( n n n n -- ) {: term:n off:n tag-off:n vid:n :}
   UNGUARD-TASK 0 0 0 TASK-PUSH
   vid SUMV-SCH-COUNT@ 1 - PAY-I !
   begin PAY-I @ 0 >= while
      term ENV-TERM!
      vid SUMV-SCH-START@ PAY-I @ + SCHEMA-ROOT@ SCHEMA-TERM {: child:n :}
      term vid PAY-I @ off VARIANT-OFF {: child-off:n :}
      WALK-TASK child child-off 0 TASK-PUSH
      PAY-I @ 1 - PAY-I !
   repeat
   GUARD-TASK
   tag-off
   vid SUMV-TAG@
   term PARAM>FAM TFAM-VAR-COUNT@
   TASK-PUSH ;

: QUEUE-SUM ( n n -- ) {: term:n off:n :}
   term ENV-TERM!
   term PARAM>FAM {: fam:n :}
   off fam TFAM-SLOTS@ + {: tag-off:n :}
   tag-off fam CHECK,
   fam TFAM-VAR-COUNT@ 1 - SUM-I !
   begin SUM-I @ 0 >= while
      fam TFAM-VAR-START@ SUM-I @ + {: vid:n :}
      term off tag-off vid QUEUE-VARIANT
      SUM-I @ 1 - SUM-I !
   repeat ;

: QUEUE-PRODUCT ( n n -- ) {: term:n off:n :}
   term ENV-TERM!
   term PARAM>FAM {: fam:n :}
   fam TFAM-FLD-COUNT@ 1 - FIELD-I !
   begin FIELD-I @ 0 >= while
      fam TFAM-FLD-START@ FIELD-I @ + {: field:n :}
      term ENV-TERM!
      field PF-SCH@ SCHEMA-ROOT@ SCHEMA-TERM
      off field PF-SLOT@ + WALK-TASK -rot 0 TASK-PUSH
      FIELD-I @ 1 - FIELD-I !
   repeat ;

: WALK-ONE ( n n -- ) {: term0:n off:n :}
   term0 T-RES {: term:n :}
   term LAYOUT-PARAM? 0= if exit then
   term PARAM>FAM {: fam:n :}
   fam TFAM-PRODUCT? if term off QUEUE-PRODUCT exit then
   fam TFAM-SUM? fam TFAM-ENUM? or if term off QUEUE-SUM exit then
   s" lowering certificate non-layout family" 76 die ;

: WALK ( n -- ) {: term:n :}
   0 TASK-N !
   WALK-TASK term 0 0 TASK-PUSH
   begin TASK-N @ 0 <> while
      TASK-POP {: kind:n a:n b:n c:n :}
      kind WALK-TASK = if a b WALK-ONE else
      kind GUARD-TASK = if a b c GUARD-PUSH else
      kind UNGUARD-TASK = if GUARD-POP else
         s" lowering certificate task kind" 76 die
      then then then
   repeat ;

: FETCH-ADD ( n -- ) {: row:n :}
   FETCH-N @ 1 + FETCH-ENSURE
   DATA-N @ {: start:n :}
   0 DATA,  0 CHECKS !  0 GUARD-N !
   row WF-TERM@ WALK
   CHECKS @ start cells DATA + !
   CHECKS @ 0 <> if -1 HAS-VALID ! then
   row WF-OFF@ FETCH-N @ cells FETCH-KEY + !
   start FETCH-N @ cells FETCH-OFF + !
   DATA-N @ start - FETCH-N @ cells FETCH-LEN + !
   FETCH-N @ 1 + FETCH-N ! ;

: FETCH-BUILD ( -- )
   0 DATA-N !  0 FETCH-N !  0 HAS-VALID !
   0 begin dup WF-N@ < while
      dup WF-FLAGS@ FETCH-FLAG and 0 <> if dup FETCH-ADD then
      1 +
   repeat drop ;

: NEEDS-P2 ( -- n )
   WF-WIDE? HAS-VALID @ 0 <> or if 1 else 0 then ;

: STREAM-BASE ( -- n )
   HEADER-N
   WF-N@ WF-NCELLS * +
   LOCW-HW-N@ +
   FETCH-N @ FETCH-NCELLS * + ;

: TOTAL-CELLS ( -- n ) STREAM-BASE DATA-N @ + ;

: HEADER, ( -- )
   MAGIC-V BUF,
   VERSION-V BUF,
   TOTAL-CELLS cells BUF,
   NEEDS-P2 BUF,
   WF-N@ BUF,
   LOCW-HW-N@ BUF,
   FETCH-N @ BUF,
   DATA-N @ BUF,
   BODY-LEN @ BUF,
   BODY-HASH @ BUF, ;

: WIDTH-FACTS, ( -- )
   0 begin dup WF-N@ < while
      dup WF-OFF@ BUF,
      dup WF-POS@ BUF,
      dup WF-WIDTH@ BUF,
      dup WF-FLAGS@ BUF,
      1 +
   repeat drop ;

: BIND-WIDTHS, ( -- )
   0 begin dup LOCW-HW-N@ < while
      dup LOCW-HW@ BUF,
      1 +
   repeat drop ;

: FETCH-INDEX, ( -- )
   0 begin dup FETCH-N @ < while
      dup cells FETCH-KEY + @ BUF,
      dup cells FETCH-OFF + @ STREAM-BASE + BUF,
      dup cells FETCH-LEN + @ BUF,
      1 +
   repeat drop ;

: FETCH-DATA, ( -- )
   0 begin dup DATA-N @ < while
      dup cells DATA + @ BUF,
      1 +
   repeat drop ;

: MAKE ( ptr u8 n -- )
   2dup SOURCE!
   2drop
   FETCH-BUILD
   0 BUF-N !
   TOTAL-CELLS BUF-ENSURE
   HEADER, WIDTH-FACTS, BIND-WIDTHS, FETCH-INDEX, FETCH-DATA,
   BUF-N @ TOTAL-CELLS <> if s" lowering certificate size mismatch" 76 die then ;

: FULL-PRODUCE ( ptr u8 n n -- ) {: a:ptr u:n verdict:n :}
   MULTI-ERR? if a u EMPTY exit then
   verdict -1 = if a u MAKE exit then
   a u EMPTY ;

' FULL-PRODUCE FULL-INSTALL

;package
