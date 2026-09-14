\ elaborate.f - the straight-line elaborator: it walks one sealed source tape and
\ builds the operations of a colon definition into a module under construction.
\
\ The compile-time value vector holds one entry per CELL, and a glue mask says
\ which cells are one value: bit i means cell i continues the cell below it, so
\ bit 0 is always clear. A rename may only move whole values.
\
\ The return stack is modelled ENTIRELY at compile time: `>r` moves a value id
\ between two vectors, emits no instruction and never touches the engine's
\ return-stack region. That rests on the checker having proved the return row.
\
\ A quotation body is a second FUNCTION of the emission, so the tokens between
\ `[:` and `;]` are taken out of the enclosing body's hands.

require lib/prelude.f
require lib/errors.f
require src/core/quotation-storage.f
require src/compiler/ir/id.f
require src/compiler/ir/context.f
require src/compiler/ir/arena.f
require src/compiler/ir/type.f
require src/compiler/ir/fun.f
require src/compiler/ir/build.f
require src/compiler/ir/source.f
require src/compiler/native/tape.f
require src/compiler/native/hir.f
require src/compiler/native/hir-word.f
require src/compiler/native/string.f
require src/compiler/native/frozen.f
require src/compiler/native/trap.f
require src/compiler/native/family.f

package NELAB
private

\ ---- what one elaboration is working on --------------------------------------
1 TYPED-BUFFER S-CTX IR-CTX:ctx
1 TYPED-BUFFER S-BLD IR-BUILD:builder
1 TYPED-BUFFER S-VW IR-ARENA:view
1 TYPED-BUFFER S-KEY IR-ID:ir-module-key

: CTX ( -- IR-CTX:ctx )              0 S-CTX @ ;
: BLD ( -- IR-BUILD:builder )        0 S-BLD @ ;
: VW ( -- IR-ARENA:view )            0 S-VW @ ;
: MKEY ( -- IR-ID:ir-module-key )    0 S-KEY @ ;

\ ---- the word a body token names ---------------------------------------------
: WSYM ( n -- IR-ID:ir-symbol-id )
   {: ix:n :}
   CTX BLD  VW MKEY ix NTAPE:SPELL@  HIR-WORD:KEY-SYM ;

\ ---- how many cells the access on a tape row moves ---------------------------
: TOK-OFF ( IR-ARENA:view n -- n )
   {: v:IR-ARENA:view ix:n :}
   v MKEY ix NTAPE:SPAN@ IR-SOURCE:SPAN-START ;

: TOK-CELLS ( IR-ARENA:view n -- n )
   TOK-OFF NDICT:MEM-CELLS ;

\ ---- naming the token a refusal was about ------------------------------------
\ One refused spelling's bytes, kept so a caller can name the token.
128 constant RF-CAP                  \ bytes of one refused spelling the record holds

here CELL 1- and CELL swap - CELL 1- and allot
variable RF-AT                       \ the row of the admit in flight, or -1
variable RF-ROW                      \ the row the record was taken for, or -1
variable RF-U                        \ how many of that row's spelling bytes are held
1 TYPED-BUFFER RF-KIND NTAPE:kind
create RF-BUF RF-CAP allot

: RF-RESET ( -- )
   -1 RF-AT !
   -1 RF-ROW ! ;

\ The one place this file asks the word model what a body token means.
: ADMIT-AT ( IR-ARENA:arena n -- HIR:meaning )
   {: r:IR-ARENA:arena ix:n :}
   ix RF-AT !
   VW r ix  ix WSYM  HIR-WORD:ADMIT-TOKEN
   -1 RF-AT ! ;

: RF-TAKE ( -- )
   VW RF-AT @ NTAPE:KIND@ 0 RF-KIND !
   0 RF-U !
   RF-AT @ RF-ROW !
   CTX BLD  VW MKEY RF-AT @ NTAPE:SPELL@  RF-BUF RF-CAP IR-BUILD:SYMBOL-COPY
   RF-U ! ;

\ Taken so that it cannot BECOME the refusal: the record is written before the
\ throw and reads nothing that can fail.
: RF-RECORD ( -- )
   RF-AT @ 0 < if exit then
   [: RF-TAKE ;] catch {: rc:n :}
   rc 0= if exit then
   0 RF-U ! ;

\ A refusal this file makes itself still has to name its token.
: QUOT-REFUSE ( n -- )
   RF-AT !
   RF-RECORD
   E-NELAB-QUOT throw ;

\ ---- the compile-time value vector -------------------------------------------
64 constant VMAX

\ ---- which entries of the vector are cells of ONE value ----------------------

\ ---- and which entries name a quotation body ---------------------------------
\ An entry names no quotation body.
-1 constant VQ-NONE                  \ this entry names no quotation body

here CELL 1- and CELL swap - CELL 1- and allot
variable VN                          \ how many values the vector holds
variable VGLUE                       \ bit i set: vector entries i and i-1 are cells of ONE value
VMAX TYPED-BUFFER VSTK IR-ID:ir-value-id
VMAX TYPED-BUFFER VWIN IR-ID:ir-value-id
create VQ    VMAX cells allot        \ the quotation body entry i names, or VQ-NONE
create VQWIN VMAX cells allot        \ the same for the window a rename consumed
create VQSAV VMAX cells allot        \ and for the entries a call hands over and takes back

\ Measured: the tree's deepest return-stack nest is ten.
16 constant RMAX                     \ measured: the tree's deepest nest is ten
variable RN                          \ how many values the return vector holds
RMAX TYPED-BUFFER RSTK IR-ID:ir-value-id
create RQ RMAX cells allot

: VRESET ( -- )
   0 VN !
   0 RN !
   0 VGLUE !
   VMAX 0 ?do  VQ-NONE i cells VQ + !  loop ;

: VPUSH ( IR-ID:ir-value-id -- )
   {: val:IR-ID:ir-value-id :}
   VN @ VMAX >= if E-NELAB-CAP throw then
   val VN @ VSTK !
   VGLUE @  1 VN @ lshift invert and  VGLUE !
   VQ-NONE VN @ cells VQ + !
   VN @ 1+ VN ! ;

\ Shifting by the word size is reduced modulo it, so a run as wide as the word
\ would come back as a run of ONE.
: VRUN-MASK ( n -- n ) {: n:n :}
   n 0 <= if 0 exit then
   n VMAX >= if -1 exit then
   1 n lshift 1 - ;

: VGLUE-LOW ( n n -- n ) {: mask:n n:n :}
   mask  n VRUN-MASK  and ;

\ ---- what the glue bit SAYS ---------------------------------------------------
\ Bit i is about a BOUNDARY: cell i and the cell below it are one value.
: VGLUE-BIT? ( n -- bool ) {: i:n :}
   i 0 < i VN @ >= or if E-NELAB-UNDER throw then
   VGLUE @  1 i lshift  and 0<> ;

: VGLUE-CLEAR ( n n -- ) {: base:n n:n :}
   VGLUE @  n VRUN-MASK base lshift invert and  VGLUE ! ;

: VGLUE-RUN ( n n -- ) {: base:n mask:n :}
   mask 0= if exit then
   VGLUE @  mask base lshift or  VGLUE ! ;

\ A value may reach FURTHER DOWN than the run being laid, so the mask grows
\ rather than being written over.
: VGLUE-GROW ( n n -- ) {: base:n w:n :}
   w 2 < if exit then
   base 1+  w 1 - VRUN-MASK  VGLUE-RUN ;

: VGLUE-ABOVE? ( n -- bool ) {: base:n :}
   base VN @ >= if false exit then
   VGLUE @ VN @ VGLUE-LOW
   1 base lshift 1 - invert and 0<> ;

: VRUN-DOWN? ( n -- bool ) {: i:n :}
   i 0 <= if false exit then
   i VGLUE-BIT? ;

: VROW-BASE ( n -- n )
   begin dup VRUN-DOWN? while 1- repeat ;

\ ---- what the DEFINITION's own two rows say --------------------------------
variable IN-GLUE
variable OUT-GLUE

\ Every reader of the vector goes through here, so one bound serves them all.
: VAT ( n -- IR-ID:ir-value-id )
   {: i:n :}
   i 0 < i VN @ >= or if E-NELAB-UNDER throw then
   i VSTK @ ;

: VDROP ( n -- )
   {: k:n :}
   k 0 < k VN @ > or if E-NELAB-UNDER throw then
   VN @ k - VN ! ;

: VAT! ( IR-ID:ir-value-id n -- )
   {: val:IR-ID:ir-value-id i:n :}
   i 0 < i VN @ >= or if E-NELAB-UNDER throw then
   val i VSTK ! ;

\ `[:` marks the entry it stages and nothing else writes it.
: VQ@ ( n -- n )
   {: i:n :}
   i 0 < i VN @ >= or if E-NELAB-UNDER throw then
   i cells VQ + @ ;

: VQ! ( n n -- )
   {: k:n i:n :}
   i 0 < i VN @ >= or if E-NELAB-UNDER throw then
   k i cells VQ + ! ;

: VQ-SAVE ( -- )
   VN @ 0 ?do  i cells VQ + @  i cells VQSAV + !  loop ;

: VQ-KEEP ( n -- )
   {: n:n :}
   n 0 ?do  i cells VQSAV + @  i cells VQ + !  loop ;

\ ---- the compile-time RETURN vector ------------------------------------------
: RAT ( n -- IR-ID:ir-value-id )
   {: i:n :}
   i 0 < i RN @ >= or if E-NELAB-UNDER throw then
   i RSTK @ ;

: RQ@ ( n -- n )
   dup 0 < over RN @ >= or if E-NELAB-UNDER throw then
   cells RQ + @ ;

: RPUSH ( IR-ID:ir-value-id n -- )
   {: val:IR-ID:ir-value-id q:n :}
   RN @ RMAX >= if E-NELAB-CAP throw then
   val RN @ RSTK !
   q RN @ cells RQ + !
   RN @ 1+ RN ! ;

: RDROP ( n -- )
   {: k:n :}
   k 0 < k RN @ > or if E-NELAB-UNDER throw then
   RN @ k - RN ! ;

\ SPILL COPIES the parked values onto the top of the data vector rather than
\ moving them, because a seam has to leave the return row as it found it.
: R-VPUSH ( n -- ) {: i:n :}
   i RAT VPUSH
   i RQ@ VN @ 1- VQ! ;

: R-SPILL ( -- )
   RN @ 0 ?do i R-VPUSH loop ;

: R-FILL ( n -- )
   {: k:n :}
   VN @ k < if E-NELAB-UNDER throw then
   0 RN !
   k 0 ?do  VN @ k - i + dup VAT swap VQ@ RPUSH  loop
   k VDROP ;

\ ---- compile-time stack renames ----------------------------------------------
\ The window's own glue, its bit 0 always clear for the reason the vector's is.
variable WGLUE                       \ the window's own glue, its bit 0 always clear
variable WW                          \ how many cells the window holds

: W-CONT? ( n -- bool ) {: k:n :}
   k 0 <= if false exit then
   k WW @ >= if false exit then
   WGLUE @  1 k lshift  and 0<> ;

: W-END ( n -- n )
   1+
   begin dup W-CONT? while 1+ repeat ;

: W-VALUE-BASE ( n -- n ) {: r:n :}
   0 r 0 ?do W-END loop ;

: W-VALUE-CELLS ( n -- n )
   W-VALUE-BASE  dup W-END  swap - ;

variable VRW-P                       \ the cursor, walking one value down at a time
variable VRW-N                       \ values still to find

: VROWS-CELLS ( n -- n ) {: values:n :}
   VN @ VRW-P !
   values VRW-N !
   begin VRW-N @ 0 >  VRW-P @ 0 >  and while
      VRW-P @ 1- VROW-BASE VRW-P !
      VRW-N @ 1- VRW-N !
   repeat
   VRW-N @ 0<> if -1 exit then
   VN @ VRW-P @ - ;

: RENAME-PICK ( n -- )
   {: r:n :}
   r W-VALUE-BASE {: b:n :}
   b W-END b - {: w:n :}
   VN @ {: at:n :}
   w 0 ?do
      b i + VWIN @ VPUSH
      b i + cells VQWIN + @  VN @ 1-  VQ!
   loop
   at w VGLUE-GROW ;

\ A pick names a window value by its DEPTH, zero being the top, and picks are
\ listed bottom first.
: RENAME-VALUE ( n n -- n ) {: in:n depth:n :}
   depth 0 < depth in >= or if E-NELAB-SHAPE throw then
   in 1- depth - ;

: RENAME ( IR-ARENA:arena IR-ARENA:arena IR-ID:ir-symbol-id -- )
   {: p:IR-ARENA:arena r:IR-ARENA:arena sym:IR-ID:ir-symbol-id :}
   r sym HIR-WORD:INPUTS@ {: in:n :}
   r sym HIR-WORD:PICKS {: picks:n :}
   in VROWS-CELLS {: w:n :}
   w 0 < if E-NELAB-UNDER throw then
   VN @ w - {: base:n :}
   w WW !
   VGLUE @ base rshift  w VGLUE-LOW  WGLUE !
   w 0 ?do
      base i + VAT  i VWIN !
      base i + VQ@  i cells VQWIN + !
   loop
   w VDROP
   0 picks 0 ?do
      in  p r sym i HIR-WORD:PICK@  RENAME-VALUE  W-VALUE-CELLS +
   loop
   VN @ + VMAX > if E-NELAB-CAP throw then
   picks 0 ?do
      in  p r sym i HIR-WORD:PICK@  RENAME-VALUE  RENAME-PICK
   loop ;

\ ---- the return-stack transfers ----------------------------------------------
\ The checker has already proved the return row at every join and loop edge, so
\ the depth is a compile-time number.
: RSTACK-CK ( n -- )
   {: base:n :}
   base VGLUE-ABOVE? if E-NELAB-BUNDLE throw then ;

: TO-R ( n -- )
   {: cells:n :}
   cells VN @ > if E-NELAB-UNDER throw then
   VN @ cells - {: base:n :}
   base RSTACK-CK
   RN @ cells + RMAX > if E-NELAB-CAP throw then
   cells 0 ?do  base i + dup VAT swap VQ@ RPUSH  loop
   cells VDROP ;

: FROM-R ( n -- )
   {: cells:n :}
   cells RN @ > if E-NELAB-UNDER throw then
   VN @ cells + VMAX > if E-NELAB-CAP throw then
   RN @ cells - {: base:n :}
   cells 0 ?do  base i + R-VPUSH  loop
   cells RDROP ;

\ A peek is a pop that does not take: the same cells arrive and the return row
\ is unchanged.
: FETCH-R ( n -- )
   {: cells:n :}
   cells RN @ > if E-NELAB-UNDER throw then
   VN @ cells + VMAX > if E-NELAB-CAP throw then
   RN @ cells - {: base:n :}
   cells 0 ?do  base i + R-VPUSH  loop ;

: RSTACK-STEP ( IR-ARENA:arena IR-ID:ir-symbol-id -- )
   {: r:IR-ARENA:arena sym:IR-ID:ir-symbol-id :}
   r sym HIR-WORD:RSTACK-CELLS@ {: cells:n :}
   r sym HIR-WORD:RSTACK@
   MATCH HIR:rmove
      to-r    OF cells TO-R ENDOF
      from-r  OF cells FROM-R ENDOF
      fetch-r OF cells FETCH-R ENDOF
   ;MATCH ;

\ ---- the names a `{: … :}` group binds ---------------------------------------
\ A local or group occupies a source token. Reserve from that count.
variable LMAX
variable LVMAX                       \ physical cells reserved for live locals
64 constant LNAME-CAP                \ bytes one declaration spelling may hold

here CELL 1- and CELL swap - CELL 1- and allot
variable LN                          \ how many locals were declared
variable LG-N                        \ how many groups the pre-pass found
variable LG-OPEN                     \ the group the pre-pass is reading, or -1
variable LG-K0                       \ the first name index of that open group
variable LGB                         \ how many groups the walk has bound
variable LBN                         \ physical local cells live where the walk stands
DYNAMIC-BUFFER LNAME IR-ID:ir-symbol-id
DYNAMIC-BUFFER LVAL IR-ID:ir-value-id
DYNAMIC-BUFFER LQ n
DYNAMIC-BUFFER LOWN n
DYNAMIC-BUFFER LSX n
DYNAMIC-BUFFER LOCAL-TABLES n
11 constant LOCAL-FIELDS

: LOCAL-VALUES-ROOM ( n -- ) {: n:n :}
   n LVMAX @ <= if exit then
   n IR-CTX:SCRATCH-LIMIT 4 cells / > if E-IR-CTX-SCRATCH throw then
   n LVAL-RESERVE
   n LQ-RESERVE
   n LOWN-RESERVE
   n LSX-RESERVE
   n LVMAX ! ;


: LOCALS-ROOM ( n -- ) {: n:n :}
   n IR-CTX:SCRATCH-LIMIT LOCAL-FIELDS cells / > if E-IR-CTX-SCRATCH throw then
   n LNAME-RESERVE
   n LOCAL-VALUES-ROOM
   n LOCAL-FIELDS * LOCAL-TABLES-RESERVE
   n LMAX ! ;

: LOCAL-FIELD ( n -- ptr n ) LMAX @ * LOCAL-TABLES ;
: LCROSS ( -- ptr n ) 0 LOCAL-FIELD ;
: LROW ( -- ptr n ) 1 LOCAL-FIELD ;
: LEND ( -- ptr n ) 2 LOCAL-FIELD ;
: LSLOT ( -- ptr n ) 3 LOCAL-FIELD ;  \ lexical name slot, used by the scope scan
: LG-A ( -- ptr n ) 4 LOCAL-FIELD ;
: LG-B ( -- ptr n ) 5 LOCAL-FIELD ;
: LG-K ( -- ptr n ) 6 LOCAL-FIELD ;
: LG-F ( -- ptr n ) 7 LOCAL-FIELD ;
: LPEND ( -- ptr n ) 8 LOCAL-FIELD ;
: LBASE ( -- ptr n ) 9 LOCAL-FIELD ;
: LWIDTH ( -- ptr n ) 10 LOCAL-FIELD ;
create LBUF LNAME-CAP allot

: LRESET ( -- )
   0 LN !
   0 LG-N !
   -1 LG-OPEN !
   0 LG-K0 !
   0 LGB !
   0 LBN !
   LMAX @ 0 ?do
      0 i cells LCROSS + !
      0 i cells LPEND + !
   loop ;

: LAT ( n -- n )
   dup 0 < over LN @ >= or if E-NELAB-LOCAL throw then ;

: LSAT ( n -- n )
   dup 0 < over LVMAX @ >= or if E-NELAB-LOCAL throw then ;

: LQ@ ( n -- n )     LSAT LQ @ ;
: LQ! ( n n -- )     {: k:n i:n :}  k i LSAT LQ ! ;

: LOWN@ ( n -- n )   LSAT LOWN @ ;
: LOWN! ( n n -- )   {: k:n i:n :}  k i LSAT LOWN ! ;

: LSX@ ( n -- bool ) LSAT LSX @ 0<> ;
: LSX! ( bool n -- )
   {: f:bool i:n :}
   f if 1 else 0 then  i LSAT LSX ! ;

: LROW@ ( n -- n )
   LAT cells LROW + @ ;

: LROW! ( n n -- )
   {: row:n k:n :}
   row k cells LROW + ! ;

: LEND@ ( n -- n )
   LAT cells LEND + @ ;

: LEND! ( n n -- )
   {: row:n k:n :}
   row k cells LEND + ! ;

: LSLOT@ ( n -- n )
   LAT cells LSLOT + @ ;

: LSLOT! ( n n -- )
   {: s:n k:n :}
   s k cells LSLOT + ! ;

\ A name is usable from the row its group's `:}` is on to the row its enclosing
\ structure closes.
: LIVE-AT? ( n n -- bool )
   {: k:n ix:n :}
   k LROW@ {: r:n :}
   r 0 < if false exit then
   r ix < if ix k LEND@ < exit then
   false ;

: LGAT ( n -- n )
   dup 0 < over LG-N @ >= or if E-NELAB-LOCAL throw then ;

: LG-A@ ( n -- n )
   LGAT cells LG-A + @ ;

: LG-B@ ( n -- n )
   LGAT cells LG-B + @ ;

: LG-K@ ( n -- n )
   LGAT cells LG-K + @ ;

: LG-F@ ( n -- n )
   LGAT cells LG-F + @ ;

\ Whether this local's value has to TRAVEL - be handed over at every call and
\ taken back - which is what a call inside its scope makes true.
: LCROSS? ( n -- bool )
   LAT cells LCROSS + @ 0<> ;

: LCROSS+ ( n -- )
   LAT cells LCROSS +  1 swap ! ;

: IN-DECL? ( n -- bool )
   {: ix:n :}
   false
   LG-N @ 0 ?do
      ix i LG-A@ >=  ix i LG-B@ <  and or
   loop ;

\ Local declarations and mentions use the same folded key as dictionary names.
: LOCAL-OF ( n -- n )
   {: ix:n :}
   VW ix NTAPE:KIND@ NTAPE-KIND:NAME NTAPE-KIND:EQ 0= if -1 exit then
   ix WSYM {: sy:IR-ID:ir-symbol-id :}
   -1
   LN @ 0 ?do
      sy i LNAME @ NFROZEN:SAME-SYM?  i ix LIVE-AT?  and if drop i then
   loop ;

\ ---- the definition's memory order -------------------------------------------
1 TYPED-BUFFER S-TOK IR-ID:ir-value-id
variable TOK-LIVE                    \ whether an order has been minted yet
variable TOK-NEED                    \ whether the body has a word that takes one
variable CALL-NEED                   \ whether the body calls anything at all
variable TAIL-NEED                   \ whether the last thing the body does is a call it need not come back from
variable CALL-BACK                   \ whether the body makes a call control comes BACK from
variable TAIL-ENTRY                  \ where the callee it would leave through starts
variable OPJ                         \ general operands taken so far by the open staging

: TOK-RESET ( -- )
   0 TOK-LIVE !
   0 TOK-NEED ! ;

: TOK ( -- IR-ID:ir-value-id )
   0 S-TOK @ ;

: TOK! ( IR-ID:ir-value-id -- )
   0 S-TOK !
   1 TOK-LIVE ! ;

\ ---- staging one operation ---------------------------------------------------
: CELL-TYPE ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-type-id )
   IR--TYPE-WIDTH:W64 IR--TYPE-SIGN:SIGNED IR-BUILD:INTERN-INT ;

\ Every operation this pass stages carries the span of the TOKEN that produced
\ it, so a diagnostic points at the source.
: OPEN ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:view IR-ID:ir-module-key n IR-ID:ir-symbol-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder v:IR-ARENA:view key:IR-ID:ir-module-key
      ix:n op:IR-ID:ir-symbol-id :}
   c b op IR-BUILD:BEGIN-OP
   c b  v key ix NTAPE:SPAN@  IR-BUILD:SET-OP-SPAN ;

\ ---- which positions of a form carry the order --------------------------------
: TOKEN? ( IR-ID:ir-type-id -- bool )
   {: t:IR-ID:ir-type-id :}
   t  CTX BLD HIR:MEM-TYPE  NFROZEN:SAME-TYPE? ;

: TOKEN-CK ( n -- n )
   dup 1 > if E-NELAB-TOKEN throw then ;

\ ---- the two value types, asked of the one authority --------------------------
: VTYPE-OF ( IR-ID:ir-value-id -- IR-ID:ir-type-id )
   {: val:IR-ID:ir-value-id :}
   CTX BLD val IR-BUILD:VALUE-TYPE@ ;

: REAL-T? ( IR-ID:ir-type-id -- bool )
   {: t:IR-ID:ir-type-id :}
   t  CTX BLD HIR:REAL-TYPE  NFROZEN:SAME-TYPE? ;

: CELL-T? ( IR-ID:ir-type-id -- bool )
   {: t:IR-ID:ir-type-id :}
   t  CTX BLD CELL-TYPE  NFROZEN:SAME-TYPE? ;

: REAL-VALUE? ( IR-ID:ir-value-id -- bool )
   VTYPE-OF REAL-T? ;

\ Only data-stack values leave as cell call operands. Hidden values remain
\ typed SSA values and may keep their real type across the call.
: NO-REAL-CK ( -- )
   VN @ 0 ?do
      i VAT REAL-VALUE? if E-NELAB-TYPE throw then
   loop ;

: TOKEN-OPERANDS ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-symbol-id -- n )
   {: c:IR-CTX:ctx b:IR-BUILD:builder op:IR-ID:ir-symbol-id :}
   c b op IR-BUILD:SCHEMA-OPERANDS {: k:n :}
   0
   k 0 ?do
      c b op i IR-BUILD:SCHEMA-OPERAND@ TOKEN? if 1+ then
   loop
   TOKEN-CK ;

: TOKEN-RESULTS ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-symbol-id -- n )
   {: c:IR-CTX:ctx b:IR-BUILD:builder op:IR-ID:ir-symbol-id :}
   c b op IR-BUILD:SCHEMA-RESULTS {: k:n :}
   0
   k 0 ?do
      c b op i IR-BUILD:SCHEMA-RESULT@ TOKEN? if 1+ then
   loop
   TOKEN-CK ;

: OPERANDS+ ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-symbol-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder op:IR-ID:ir-symbol-id :}
   c b op IR-BUILD:SCHEMA-OPERANDS {: k:n :}
   k  c b op TOKEN-OPERANDS  - {: v:n :}
   v VN @ > if E-NELAB-UNDER throw then
   VN @ v - {: base:n :}
   0 OPJ !
   k 0 ?do
      c b op i IR-BUILD:SCHEMA-OPERAND@ TOKEN? if
         c b TOK IR-BUILD:ADD-OPERAND
      else
         c b  base OPJ @ + VAT  IR-BUILD:ADD-OPERAND
         OPJ @ 1+ OPJ !
      then
   loop
   v VDROP ;

: RESULTS+ ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-symbol-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder op:IR-ID:ir-symbol-id :}
   c b op TOKEN-RESULTS drop
   c b op IR-BUILD:SCHEMA-RESULTS {: k:n :}
   k 0 ?do
      c b  c b op i IR-BUILD:SCHEMA-RESULT@  IR-BUILD:ADD-RESULT
   loop ;

: CLOSE ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-symbol-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder op:IR-ID:ir-symbol-id :}
   c b IR-BUILD:END-OP {: id:IR-ID:ir-op-id :}
   c b op IR-BUILD:SCHEMA-RESULTS {: k:n :}
   k 0 ?do
      c b op i IR-BUILD:SCHEMA-RESULT@ TOKEN? if
         c b id i IR-BUILD:OP-RESULT@ TOK!
      else
         c b id i IR-BUILD:OP-RESULT@ VPUSH
      then
   loop ;

\ ---- the two crossings between a cell and a double ----------------------------
\ A crossing computes nothing: the same eight bytes read as the other type.
: CROSS-VALUE ( n IR-ID:ir-value-id HIR:opcode -- IR-ID:ir-value-id )
   {: ix:n v:IR-ID:ir-value-id kop:HIR:opcode :}
   CTX BLD kop HIR:ENSURE-OP {: op:IR-ID:ir-symbol-id :}
   CTX BLD VW MKEY ix op OPEN
   CTX BLD v IR-BUILD:ADD-OPERAND
   CTX BLD  CTX BLD op 0 IR-BUILD:SCHEMA-RESULT@  IR-BUILD:ADD-RESULT
   CTX BLD IR-BUILD:END-OP {: id:IR-ID:ir-op-id :}
   CTX BLD id 0 IR-BUILD:OP-RESULT@ ;

: CROSS1 ( n n HIR:opcode -- )
   {: ix:n k:n kop:HIR:opcode :}
   ix  k VAT  kop CROSS-VALUE  k VAT! ;

\ The crossing a value takes on its way into a form that names cells.
: CELL-CROSS-RUN ( n n n -- )
   {: ix:n base:n k:n :}
   k 0 ?do
      base i + VAT REAL-VALUE? if ix base i + HIR-OPCODE:REALBITS CROSS1 then
   loop ;

: CELL-CROSS ( n n -- )
   {: ix:n n:n :}
   ix 0 n CELL-CROSS-RUN ;

\ Make the value at one vector position answer to the type that position wants.
: COERCE1 ( n n IR-ID:ir-type-id -- )
   {: ix:n k:n want:IR-ID:ir-type-id :}
   k VAT VTYPE-OF {: have:IR-ID:ir-type-id :}
   have want NFROZEN:SAME-TYPE? if exit then
   want REAL-T? have CELL-T? and 0= if E-NELAB-TYPE throw then
   ix k HIR-OPCODE:BITSREAL CROSS1 ;

: COERCE-OPERANDS ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-symbol-id n -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder op:IR-ID:ir-symbol-id ix:n :}
   c b op IR-BUILD:SCHEMA-OPERANDS {: k:n :}
   k  c b op TOKEN-OPERANDS  - {: v:n :}
   v VN @ > if E-NELAB-UNDER throw then
   VN @ v - {: base:n :}
   0 OPJ !
   k 0 ?do
      c b op i IR-BUILD:SCHEMA-OPERAND@ TOKEN? 0= if
         ix  base OPJ @ +  c b op i IR-BUILD:SCHEMA-OPERAND@  COERCE1
         OPJ @ 1+ OPJ !
      then
   loop ;

\ ---- the block-local literal memo ---------------------------------------------
\ One number materialised twice in one straight line is one value.
64 constant LITMAX

create LIT-VAL LITMAX cells allot     \ the number
create LIT-KIND LITMAX cells allot    \ and what the number IS
LITMAX TYPED-BUFFER LIT-ID IR-ID:ir-value-id
variable LIT-N

: LIT-RESET ( -- )
   0 LIT-N ! ;

: LIT-MARK ( -- n )
   LIT-N @ ;

: LIT-RELEASE ( n -- )
   {: m:n :}
   m LIT-N @ > if exit then
   m LIT-N ! ;

: LIT-FIND ( n n -- n )
   {: kind:n val:n :}
   -1
   LIT-N @ 0 ?do
      i cells LIT-VAL + @ val =
      i cells LIT-KIND + @ kind = and if drop i leave then
   loop ;

: LIT-REMEMBER ( n n IR-ID:ir-value-id -- )
   {: kind:n val:n id:IR-ID:ir-value-id :}
   LIT-N @ LITMAX >= if exit then
   val LIT-N @ cells LIT-VAL + !
   kind LIT-N @ cells LIT-KIND + !
   id LIT-N @ LIT-ID !
   LIT-N @ 1+ LIT-N ! ;

\ ---- the things a body token becomes -----------------------------------------
: STAGE-LIT ( n n n -- )
   {: ix:n val:n kind:n :}
   CTX BLD HIR-OPCODE:CONST HIR:ENSURE-OP {: op:IR-ID:ir-symbol-id :}
   CTX BLD VW MKEY ix op OPEN
   CTX BLD op OPERANDS+
   CTX BLD op RESULTS+
   CTX BLD  CTX BLD HIR:KEY-VALUE  CTX BLD val IR-BUILD:INTERN-INT-ATTR
   IR-BUILD:ADD-ATTR
   CTX BLD  CTX BLD HIR:KEY-ADDR  CTX BLD kind HIR:ADDR-ATTR
   IR-BUILD:ADD-ATTR
   CTX BLD op CLOSE ;

: EMIT-KIND-LIT ( n n n -- )
   {: ix:n val:n kind:n :}
   kind val LIT-FIND {: j:n :}
   j 0 >= if j LIT-ID @ VPUSH exit then
   ix val kind STAGE-LIT
   kind val  VN @ 1- VAT  LIT-REMEMBER ;

: EMIT-LIT ( n n -- )
   {: ix:n val:n :}
   ix val HIR:ADDR-NONE EMIT-KIND-LIT ;

\ The memory the definition is entered with, staged at the token's span.
: EMIT-MEM ( n -- )
   {: ix:n :}
   CTX BLD HIR-OPCODE:MEM HIR:ENSURE-OP {: op:IR-ID:ir-symbol-id :}
   CTX BLD VW MKEY ix op OPEN
   CTX BLD op OPERANDS+
   CTX BLD op RESULTS+
   CTX BLD op CLOSE ;

\ An operation that takes an order has to find one, so the order is minted in
\ the entry block before the first operation that needs it.
: TOKEN-READY ( IR-ID:ir-symbol-id -- )
   {: op:IR-ID:ir-symbol-id :}
   CTX BLD op TOKEN-OPERANDS 0= if exit then
   TOK-LIVE @ 0= if E-NELAB-TOKEN throw then ;

: EMIT-OPCODE ( n HIR:opcode -- )
   {: ix:n k:HIR:opcode :}
   CTX BLD k HIR:ENSURE-OP {: op:IR-ID:ir-symbol-id :}
   op TOKEN-READY
   \ STORE writes a cell's bits; a real value crosses only its value operand.
   k HIR-OPCODE:STORE HIR-OPCODE:EQ if
      VN @ 2 < if E-NELAB-UNDER throw then
      ix VN @ 2 - 1 CELL-CROSS-RUN
   then
   CTX BLD op ix COERCE-OPERANDS
   CTX BLD VW MKEY ix op OPEN
   CTX BLD op OPERANDS+
   CTX BLD op RESULTS+
   CTX BLD op CLOSE ;

: EMIT-CONST ( n -- )
   {: ix:n :}
   ix  VW ix NTAPE:LIT@  EMIT-LIT ;

: EMIT-FLIT ( n n -- )
   {: ix:n val:n :}
   CTX BLD HIR-OPCODE:FCONST HIR:ENSURE-OP {: op:IR-ID:ir-symbol-id :}
   CTX BLD op ix COERCE-OPERANDS
   CTX BLD VW MKEY ix op OPEN
   CTX BLD op OPERANDS+
   CTX BLD op RESULTS+
   CTX BLD  CTX BLD HIR:KEY-VALUE  CTX BLD val IR-BUILD:INTERN-INT-ATTR
   IR-BUILD:ADD-ATTR
   CTX BLD op CLOSE ;

: EMIT-FCONST ( n -- )
   {: ix:n :}
   ix  VW ix NTAPE:LIT@  EMIT-FLIT ;

\ ---- a word named by its spelling rather than by a tape row ------------------
: EMIT-OP-SYM ( IR-ARENA:arena n IR-ID:ir-symbol-id -- )
   {: r:IR-ARENA:arena ix:n sy:IR-ID:ir-symbol-id :}
   ix  r sy HIR-WORD:OPCODE@  EMIT-OPCODE ;

: EMIT-OP ( IR-ARENA:arena n -- )
   {: r:IR-ARENA:arena ix:n :}
   r ix  ix WSYM  EMIT-OP-SYM ;

\ A `create`d word's address is a DATA address a snapshot moves with the region,
\ so the literal carries its kind and not just its number.
: EMIT-FIXED-SYM ( IR-ARENA:arena n IR-ID:ir-symbol-id -- )
   {: r:IR-ARENA:arena ix:n sy:IR-ID:ir-symbol-id :}
   ix  r sy HIR-WORD:FIXED-VALUE@  r sy HIR-WORD:FIXED-KIND@  EMIT-KIND-LIT ;

: EMIT-FIXED ( IR-ARENA:arena n -- )
   {: r:IR-ARENA:arena ix:n :}
   r ix  ix WSYM  EMIT-FIXED-SYM ;

\ ---- a string literal ----------------------------------------------------------
$1000 constant SB-CAP
create SB-BUF SB-CAP allot

: STRING-BODY ( n -- ptr u8 n ) {: ix:n :}
   VW MKEY ix NTAPE:SPELL@ {: sy:IR-ID:ir-symbol-id :}
   SB-BUF  CTX BLD sy SB-BUF SB-CAP IR-BUILD:SYMBOL-COPY ;

: EMIT-STRING ( n -- ) {: ix:n :}
   ix STRING-BODY {: a u:n :}
   ix  a u NSTR:INTERN  HIR:ADDR-DATA  EMIT-KIND-LIT
   ix  u  EMIT-LIT ;

: EMIT-CONST-OP-SYM ( IR-ARENA:arena n IR-ID:ir-symbol-id -- )
   {: r:IR-ARENA:arena ix:n sy:IR-ID:ir-symbol-id :}
   ix  r sy HIR-WORD:CONST-VALUE@  EMIT-LIT
   ix  r sy HIR-WORD:CONST-OPCODE@  EMIT-OPCODE ;

: EMIT-CONST-OP ( IR-ARENA:arena n -- )
   {: r:IR-ARENA:arena ix:n :}
   r ix  ix WSYM  EMIT-CONST-OP-SYM ;

\ The outputs are the whole vector, bottom first.
: RETURN-CROSS ( n -- )
   0 swap CELL-CROSS ;

\ The RETURN vector is empty here, and that is asked rather than assumed.
: EMIT-RETURN ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:view IR-ID:ir-module-key n -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder v:IR-ARENA:view key:IR-ID:ir-module-key
      out:n :}
   VN @ out <> if E-NELAB-ARITY throw then
   RN @ 0<> if E-NELAB-JOIN throw then
   VGLUE @ out VGLUE-LOW  OUT-GLUE @ <> if E-NELAB-JOIN throw then
   out RETURN-CROSS
   c b HIR-OPCODE:RETURN HIR:ENSURE-OP {: op:IR-ID:ir-symbol-id :}
   c b v key 0 op OPEN
   out 0 ?do
      c b  i VAT  IR-BUILD:ADD-OPERAND
   loop
   out VDROP
   c b IR-BUILD:END-OP drop ;

\ An empty, physically arity-preserving colon body is an identity over its
\ cells.  The checker still owns whether that identity may change their logical
\ grouping; ordinary empty retypes are rejected before NELAB runs.  A generated
\ product MAKE/UNMAKE is the one current certified consumer: its input and
\ output rows describe the same cells with opposite glue.  Keep every nonempty
\ body on the ordinary strict return check at the frame return.
: EMPTY-FRAME-RESHAPE ( n n n n -- )
   {: lo:n hi:n in:n out:n :}
   lo hi <> if exit then
   in out <> if exit then
   VGLUE @ in VGLUE-LOW IN-GLUE @ <> if E-NELAB-JOIN throw then
   OUT-GLUE @ VGLUE ! ;

\ ---- the open control structures ----------------------------------------------
\ Structures one definition may nest.
32 constant CMAX

here CELL 1- and CELL swap - CELL 1- and allot
variable CS-N
variable CS-LOOPS                   \ active index frames, one bit per CS slot
CMAX TYPED-BUFFER CS-KIND HIR:ctrl
create CS-DEPTH CMAX cells allot
create CS-JOIN CMAX cells allot
create CS-HEAD CMAX cells allot
create CS-ARM CMAX cells allot
create CS-NW CMAX cells allot
create CS-XD CMAX cells allot
create CS-EXIT CMAX cells allot
create CS-END CMAX cells allot       \ whether the arm before this frame's `else` ended
create CS-W CMAX cells allot         \ cells one value of a tag-dispatch frame's subject occupies
create CS-TRAP CMAX cells allot      \ the ordinal a `MATCH` mismatch traps with
create CS-OFIX CMAX cells allot      \ the row of the `of` that opened the arm being read, or -1
create CS-JOINED CMAX cells allot    \ whether any arm of this frame reached its join
create CS-RD CMAX cells allot        \ parked values the return vector held when this structure opened
create CS-ARMR CMAX cells allot      \ parked values the first arm of an `if` left, beside CS-ARM
create CS-XR CMAX cells allot        \ parked values the first `while` of a loop left, beside CS-XD
create CS-LB CMAX cells allot        \ bound locals the walk held when this structure opened
create CS-LARM CMAX cells allot      \ and when the arm being read opened, or -1
create CS-LOOP0 CMAX cells allot     \ loop visibility at the structure's entry
create CS-LOOP1 CMAX cells allot     \ loop visibility at its live branch join
CMAX TYPED-BUFFER CS-IDX IR-ID:ir-value-id
CMAX TYPED-BUFFER CS-LIM IR-ID:ir-value-id

: CS-RESET ( -- )
   0 CS-N !
   0 CS-LOOPS ! ;

: CS-AT ( n -- n )
   dup 0 < over CS-N @ >= or if E-NELAB-CTRL throw then ;

: CS-TOP ( -- n )
   CS-N @ 1- CS-AT ;

\ Opening a structure clears every field the words inside it may write.
: CS-PUSH ( HIR:ctrl n n -- )
   {: k:HIR:ctrl d:n j:n :}
   CS-N @ CMAX >= if E-NELAB-BLOCK throw then
   CS-N @ {: t:n :}
   k t CS-KIND !
   d t cells CS-DEPTH + !
   j t cells CS-JOIN + !
   -1 t cells CS-ARM + !
   0 t cells CS-NW + !
   -1 t cells CS-XD + !
   -1 t cells CS-EXIT + !
   0 t cells CS-END + !
   0 t cells CS-W + !
   0 t cells CS-TRAP + !
   -1 t cells CS-OFIX + !
   0 t cells CS-JOINED + !
   RN @ t cells CS-RD + !
   -1 t cells CS-ARMR + !
   -1 t cells CS-XR + !
   LBN @ t cells CS-LB + !
   -1 t cells CS-LARM + !
   CS-LOOPS @ t cells CS-LOOP0 + !
   0 t cells CS-LOOP1 + !
   t 1+ CS-N ! ;

: CS-POP ( -- )
   CS-N @ 1 < if E-NELAB-CTRL throw then
   CS-N @ 1- CS-N ! ;

\ A `then` over an open `begin` is a closer meeting the wrong opener.
: CS-OPENER-CK ( HIR:ctrl -- n )
   {: want:HIR:ctrl :}
   CS-TOP {: t:n :}
   t CS-KIND @ want HIR-CTRL:EQ 0= if E-NELAB-CTRL throw then
   t ;

: CS-DEPTH@ ( n -- n )    cells CS-DEPTH + @ ;
: CS-JOIN@ ( n -- n )     cells CS-JOIN + @ ;
: CS-HEAD@ ( n -- n )     cells CS-HEAD + @ ;
: CS-ARM@ ( n -- n )      cells CS-ARM + @ ;
: CS-NW@ ( n -- n )       cells CS-NW + @ ;
: CS-XD@ ( n -- n )       cells CS-XD + @ ;
: CS-EXIT@ ( n -- n )     cells CS-EXIT + @ ;
: CS-END@ ( n -- n )      cells CS-END + @ ;
: CS-W@ ( n -- n )        cells CS-W + @ ;
: CS-TRAP@ ( n -- n )     cells CS-TRAP + @ ;
: CS-OFIX@ ( n -- n )     cells CS-OFIX + @ ;
: CS-JOINED? ( n -- bool ) cells CS-JOINED + @ 0<> ;
: CS-RD@ ( n -- n )       cells CS-RD + @ ;
: CS-ARMR@ ( n -- n )     cells CS-ARMR + @ ;
: CS-XR@ ( n -- n )       cells CS-XR + @ ;
: CS-LB@ ( n -- n )       cells CS-LB + @ ;
: CS-LARM@ ( n -- n )     cells CS-LARM + @ ;
: CS-LOOP0@ ( n -- n )    cells CS-LOOP0 + @ ;
: CS-LOOP1@ ( n -- n )    cells CS-LOOP1 + @ ;

: CS-JOIN! ( n n -- )     cells CS-JOIN + ! ;
: CS-ARM! ( n n -- )      cells CS-ARM + ! ;
: CS-ARMR! ( n n -- )     cells CS-ARMR + ! ;
: CS-XD! ( n n -- )       cells CS-XD + ! ;
: CS-XR! ( n n -- )       cells CS-XR + ! ;
: CS-EXIT! ( n n -- )     cells CS-EXIT + ! ;
: CS-END! ( n n -- )      cells CS-END + ! ;
: CS-W! ( n n -- )        cells CS-W + ! ;
: CS-TRAP! ( n n -- )     cells CS-TRAP + ! ;
: CS-OFIX! ( n n -- )     cells CS-OFIX + ! ;
: CS-JOINED+ ( n -- )     cells CS-JOINED +  1 swap ! ;
: CS-LARM! ( n n -- )     cells CS-LARM + ! ;
: CS-LOOP1! ( n n -- )    cells CS-LOOP1 + ! ;

: CS-LOOP-BIT ( n -- n ) 1 swap lshift ;

: CS-LOOPS= ( n -- )
   CS-LOOPS @ <> if E-NELAB-JOIN throw then ;

\ ---- giving a structure's names back ------------------------------------------
: LOC-REST ( n -- )
   {: m:n :}
   LBN @ m < if E-NELAB-LOCAL throw then
   m LBN ! ;

\ A seam that leaves from the middle BORROWS the locals scope: `while` branches
\ out of a loop whose group is still open.
: LOC-BORROW ( n -- n )
   {: m:n :}
   LBN @ {: had:n :}
   m LOC-REST
   had ;

: LOC-RETURN ( n -- )
   {: had:n :}
   had LBN @ < if E-NELAB-LOCAL throw then
   had LBN ! ;

: CS-WHILE+ ( n -- )
   {: t:n :}
   t CS-NW@ 1+  t cells CS-NW + ! ;

: CS-ELSE? ( n -- bool )
   CS-ARM@ 0 >= ;

: CS-ADT? ( n -- bool ) {: t:n :}
   t CS-KIND @ HIR-CTRL:OPEN-MATCH HIR-CTRL:EQ
   t CS-KIND @ HIR-CTRL:OPEN-CASE HIR-CTRL:EQ or ;

: CS-MATCH? ( n -- bool )
   CS-KIND @ HIR-CTRL:OPEN-MATCH HIR-CTRL:EQ ;

: CS-PENDING ( n -- n )
   {: t:n :}
   t CS-ELSE? if t CS-ARM@ exit then
   t CS-JOIN@ ;

\ ---- what is live at a point in the body -------------------------------------
: DO-FRAME-IS? ( n -- bool )
   CS-KIND @ HIR-CTRL:OPEN-DO HIR-CTRL:EQ ;

: DO-OPEN-N ( -- n )
   0
   CS-N @ 0 ?do
      i DO-FRAME-IS? if 1+ then
   loop ;

variable DOK                         \ counted loops the search below has passed

\ Outermost first, which is the order the header's argument list is built in.
: DO-NTH ( n -- n )
   {: k:n :}
   -1
   0 DOK !
   CS-N @ 0 ?do
      i DO-FRAME-IS? if
         DOK @ k = if drop i leave then
         DOK @ 1+ DOK !
      then
   loop
   dup 0 < if E-NELAB-CTRL throw then ;

\ Innermost first, which is the order `i` and `j` search in.
: DO-INNER-NTH ( n -- n )
   {: k:n :}
   DO-OPEN-N k 1+ < if E-NELAB-CTRL throw then
   DO-OPEN-N 1- k - DO-NTH ;

\ Discharged frames still carry SSA values across lexical edges, but cannot
\ supply an index. The checker has already proved each live join agrees.
: DO-ACTIVE-NTH ( n -- n ) {: k:n :}
   -1
   0 DOK !
   CS-N @ 0 ?do
      CS-N @ 1- i - {: t:n :}
      t CS-LOOP-BIT CS-LOOPS @ and 0<> if
         DOK @ k = if drop t leave then
         1 DOK +!
      then
   loop
   dup 0 < if E-NELAB-CTRL throw then ;

: CROSS-N ( -- n )
   CALL-NEED @ 0= if 0 exit then
   DO-OPEN-N ;

: CROSS-DO ( -- n n )
   0 CROSS-N ;

\ The locals a call can reach, which is what decides which cross an edge.
: CROSS-L ( -- n )
   0
   LBN @ 0 ?do  i LSX@ if 1+ then  loop ;

: LOOP-OPERAND+ ( n -- )
   {: t:n :}
   CTX BLD  t CS-IDX @  IR-BUILD:ADD-OPERAND
   CTX BLD  t CS-LIM @  IR-BUILD:ADD-OPERAND ;

: LOOP-OPERANDS+ ( n n -- )
   {: lo:n h:n :}
   h 0 ?do  lo i + DO-NTH LOOP-OPERAND+  loop ;

\ Index first, then limit, because one edge hands the pair over together.
: LOOP-ARG+ ( n -- )
   {: t:n :}
   CTX BLD  CTX BLD CELL-TYPE  IR-BUILD:ADD-BLOCK-ARG  t CS-IDX !
   CTX BLD  CTX BLD CELL-TYPE  IR-BUILD:ADD-BLOCK-ARG  t CS-LIM ! ;

: LOOP-ARGS+ ( n n -- )
   {: lo:n h:n :}
   h 0 ?do  lo i + DO-NTH LOOP-ARG+  loop ;

: LOCAL-CK ( n -- bool )
   {: l:n :}
   l 0= if false exit then
   l CROSS-L <> if E-NELAB-LOCAL throw then
   true ;

\ No crossing local holds a double.
: NO-REAL-LOCAL-CK ( n -- )
   LOCAL-CK 0= if exit then
   LBN @ 0 ?do
      i LSX@ if
         i LVAL @ REAL-VALUE? if E-NELAB-TYPE throw then
      then
   loop ;

: LOCAL-OPERANDS+ ( n -- )
   dup NO-REAL-LOCAL-CK
   LOCAL-CK 0= if exit then
   LBN @ 0 ?do
      i LSX@ if CTX BLD  i LVAL @  IR-BUILD:ADD-OPERAND then
   loop ;

: LOCAL-ARGS+ ( n -- )
   LOCAL-CK 0= if exit then
   LBN @ 0 ?do
      i LSX@ if
         CTX BLD  CTX BLD CELL-TYPE  IR-BUILD:ADD-BLOCK-ARG  i LVAL !
      then
   loop ;

\ ---- the blocks a definition is made of --------------------------------------
variable NB                          \ blocks closed so far; also the open block's ordinal

\ The module ordinal this function's first block takes; a successor names a
\ block of the MODULE and this walk numbers from zero.
variable BBASE                       \ the module ordinal this function's first block takes

\ ---- leaving from the middle of a definition ---------------------------------
variable IN-N                        \ values the definition takes
variable OUT-N                       \ values the definition leaves
variable FR-GIN                      \ what the caller staged for the definition about to be compiled
variable FR-GOUT
variable EXIT-USED                   \ whether the body has an `exit` at all
variable EXIT-ORD                    \ the block every `exit` and the fall-through reach

\ ---- a path that has already ended -------------------------------------------
0 constant PATH-LIVE                 \ the walk is on a path that goes on
2 constant PATH-DEAD                 \ a call that does not come back closed it
variable PATH-END

: PATH-ENDED? ( -- bool )
   PATH-END @ PATH-LIVE <> ;

: PATH-DEAD? ( -- bool )
   PATH-END @ PATH-DEAD = ;

: EXIT-RESET ( -- )
   0 EXIT-USED !
   -1 EXIT-ORD !
   PATH-LIVE PATH-END ! ;

variable BLOCK-LIMIT

\ This walk's own ordinal, raised into the module's.
: BLOCK-ORD ( n -- IR-ID:ir-block-id )
   {: k:n :}
   k 0 < k BLOCK-LIMIT @ >= or if E-NELAB-BLOCK throw then
   MKEY  BBASE @ k +  IR-ID:PACK-BLOCK ;

\ The base is held against the module at every block, so a drift is a refusal.
: CLOSE-HELD ( -- )
   CTX BLD IR-BUILD:END-BLOCK IR-ID:BLOCK-LOCAL {: ord:n :}
   ord  BBASE @ NB @ +  <> if E-NELAB-BLOCK throw then ;

: CLOSE-BLOCK ( -- )
   CLOSE-HELD
   NB @ 1+ {: k:n :}
   k BLOCK-LIMIT @ > if E-NELAB-BLOCK throw then
   k NB ! ;

\ ---- the bodies this definition defers ---------------------------------------
\ Signature parameters and callback-valued results also need rows, so their
\ storage grows independently of the number of source tokens.
7 constant QUOT-FIELDS
DYNAMIC-BUFFER QAT-BUF n
DYNAMIC-BUFFER QLO-BUF n
DYNAMIC-BUFFER QHI-BUF n
DYNAMIC-BUFFER QIN-BUF n
DYNAMIC-BUFFER QOUT-BUF n
DYNAMIC-BUFFER QFUN-BUF n
DYNAMIC-BUFFER QPARENT n

: QUOT-ROOM ( n -- ) {: n:n :}
   n IR-CTX:SCRATCH-LIMIT QUOT-FIELDS cells / > if E-IR-CTX-SCRATCH throw then
   n QAT-BUF-RESERVE
   n QLO-BUF-RESERVE
   n QHI-BUF-RESERVE
   n QIN-BUF-RESERVE
   n QOUT-BUF-RESERVE
   n QFUN-BUF-RESERVE
   n QPARENT-RESERVE ;

-1 constant QNONE                    \ no consumer has said what this body takes and leaves

here CELL 1- and CELL swap - CELL 1- and allot
variable QN                          \ quotation metadata rows in this definition
variable QD                          \ the row of the body the pre-scan has open, or -1
variable QBASE                       \ the ordinal of the function the definition itself is
: QAT ( -- ptr n ) 0 QAT-BUF ;
: QLO ( -- ptr n ) 0 QLO-BUF ;
: QHI ( -- ptr n ) 0 QHI-BUF ;
: QIN ( -- ptr n ) 0 QIN-BUF ;
: QOUT ( -- ptr n ) 0 QOUT-BUF ;
\ This row's quotation is no body of this emission - it is a parameter.
-1 constant QPARAM                   \ this row's quotation is no body of this emission
: QFUN ( -- ptr n ) 0 QFUN-BUF ;

\ ---- which body each token belongs to, and which body is being walked ---------
-1 constant QOWNER-DEF               \ the definition's own function, which is no body
variable QCUR                        \ the body being walked, or QOWNER-DEF

: QROW-CK ( n -- n )
   dup 0 < over QN @ >= or if E-NELAB-QUOT-CAP throw then ;

\ Joined values share an ABI, while each literal retains its own body row.
: QROOT ( n -- n )
   QROW-CK
   begin dup QPARENT @ over <> while QPARENT @ repeat ;

: QAT@ ( n -- n )    QROW-CK cells QAT + @ ;
: QLO@ ( n -- n )    QROW-CK cells QLO + @ ;
: QHI@ ( n -- n )    QROW-CK cells QHI + @ ;
: QFUN@ ( n -- n )   QROW-CK cells QFUN + @ ;
: QIN@ ( n -- n )    QROOT cells QIN + @ ;
: QOUT@ ( n -- n )   QROOT cells QOUT + @ ;

: QFILL ( n n n -- )
   {: k:n in:n out:n :}
   k QIN@ QNONE <> if
      k QIN@ in <>  k QOUT@ out <>  or if k QAT@ QUOT-REFUSE then
      exit
   then
   k QROOT {: root:n :}
   in root cells QIN + !
   out root cells QOUT + ! ;


: QMERGE ( n n -- ) {: a:n b:n :}
   a b = if exit then
   a 0 < b 0 < or if E-NELAB-JOIN throw then
   a QROOT {: ra:n :} b QROOT {: rb:n :}
   ra rb = if exit then
   rb QIN@ QNONE <> if ra rb QIN@ rb QOUT@ QFILL then
   ra rb QPARENT ! ;

\ ---- what type each block argument has ---------------------------------------
\ Block argument positions across the whole function.
: ARG-CAP ( -- n ) BLOCK-LIMIT @ VMAX * ;

here CELL 1- and CELL swap - CELL 1- and allot
DYNAMIC-BUFFER ARG-N-BUF n
: ARG-N ( -- ptr n ) 0 ARG-N-BUF ;
DYNAMIC-BUFFER ARG-G-BUF n
: ARG-G ( -- ptr n ) 0 ARG-G-BUF ;
DYNAMIC-BUFFER ARG-R-BUF n
: ARG-R ( -- ptr n ) 0 ARG-R-BUF ;
DYNAMIC-BUFFER ARG-T IR-ID:ir-type-id
DYNAMIC-BUFFER ARG-Q n
VMAX TYPED-BUFFER XV IR-ID:ir-value-id  \ what the edge being staged really hands over

\ SKELETON has counted the function before any argument rows are written.
: BLOCK-ROOM ( n -- )
   1 max BLOCK-LIMIT !
   BLOCK-LIMIT @ ARG-N-BUF-RESERVE
   BLOCK-LIMIT @ ARG-G-BUF-RESERVE
   BLOCK-LIMIT @ ARG-R-BUF-RESERVE
   ARG-CAP ARG-T-RESERVE
   ARG-CAP ARG-Q-RESERVE
   ;

: ARG-RESET ( -- )
   BLOCK-LIMIT @ 0 ?do
      -1 i cells ARG-N + !  0 i cells ARG-G + !  0 i cells ARG-R + !
   loop ;

: ARG-BLOCK-CK ( n -- n )
   dup 0 < over BLOCK-LIMIT @ >= or if E-NELAB-BLOCK throw then ;

: ARG-STATED? ( n -- bool )
   ARG-BLOCK-CK cells ARG-N + @ 0 >= ;

: ARG-WIDTH@ ( n -- n )
   ARG-BLOCK-CK cells ARG-N + @ ;

: ARG-T@ ( n n -- IR-ID:ir-type-id )
   {: t:n k:n :}
   k 0 < k t ARG-WIDTH@ >= or if E-NELAB-JOIN throw then
   t VMAX * k + ARG-T @ ;

\ Where the values in a block's argument positions BEGIN; a value that is one
\ cell begins and ends at itself.
: ARG-G@ ( n -- n )
   ARG-BLOCK-CK cells ARG-G + @ ;

: ARG-R@ ( n -- n )
   ARG-BLOCK-CK cells ARG-R + @ ;

: ARG-STATE ( n n -- )
   {: t:n n:n :}
   t ARG-STATED? if E-NELAB-JOIN throw then
   n 0 < n VMAX > or if E-NELAB-CAP throw then
   n 0 ?do
      i VAT VTYPE-OF  t VMAX * i +  ARG-T !
      i VQ@ t VMAX * i + ARG-Q !
   loop
   VGLUE @  n VGLUE-LOW  t ARG-BLOCK-CK cells ARG-G + !
   RN @ t ARG-BLOCK-CK cells ARG-R + !
   n t ARG-BLOCK-CK cells ARG-N + ! ;

\ ---- what a function is built from -------------------------------------------
: FUN-STATE! ( IR-BUILD:builder -- )
   {: b:IR-BUILD:builder :}
   b IR-BUILD:BLOCKS BBASE !
   ARG-RESET
   0 LBN ! ;

\ The live vector first, then the counters of every loop the edge crosses, then
\ the crossing locals, then the memory order.
: OPEN-ARGS-H ( n n n n n -- )
   {: ix:n n:n lo:n h:n l:n :}
   NB @ ARG-STATED? 0= if E-NELAB-JOIN throw then
   NB @ ARG-WIDTH@ n <> if E-NELAB-JOIN throw then
   CTX BLD IR-BUILD:BEGIN-BLOCK
   CTX BLD  VW MKEY ix NTAPE:SPAN@  IR-BUILD:SET-BLOCK-SPAN
   VRESET
   LIT-RESET
   n 0 ?do
      CTX BLD  NB @ i ARG-T@  IR-BUILD:ADD-BLOCK-ARG VPUSH
      NB @ VMAX * i + ARG-Q @ i VQ!
   loop
   0 NB @ ARG-G@ VGLUE-RUN
   NB @ ARG-R@ R-FILL
   lo h LOOP-ARGS+
   l LOCAL-ARGS+
   TOK-LIVE @ 0<> if
      CTX BLD  CTX BLD HIR:MEM-TYPE  IR-BUILD:ADD-BLOCK-ARG TOK!
   then ;

: OPEN-ARGS ( n n -- )
   CROSS-DO CROSS-L OPEN-ARGS-H ;

: OPEN-PLAIN ( n -- )
   {: ix:n :}
   CTX BLD IR-BUILD:BEGIN-BLOCK
   CTX BLD  VW MKEY ix NTAPE:SPAN@  IR-BUILD:SET-BLOCK-SPAN ;

\ ---- what one edge really hands over -----------------------------------------
: EDGE-VALUE ( n n IR-ID:ir-type-id -- IR-ID:ir-value-id )
   {: ix:n k:n want:IR-ID:ir-type-id :}
   k VAT {: v:IR-ID:ir-value-id :}
   v VTYPE-OF want NFROZEN:SAME-TYPE? if v exit then
   want REAL-T?  v REAL-VALUE? 0=  and if
      ix v HIR-OPCODE:BITSREAL CROSS-VALUE exit
   then
   want CELL-T?  v REAL-VALUE?  and if
      ix v HIR-OPCODE:REALBITS CROSS-VALUE exit
   then
   E-NELAB-TYPE throw ;

\ The first edge into a block states one type per vector position it hands over.
: EDGE-STAGE ( n n -- )
   {: ix:n t:n :}
   t ARG-STATED? 0= if
      t VN @ ARG-STATE
      VN @ 0 ?do  i VAT  i XV !  loop
      exit
   then
   t ARG-WIDTH@ VN @ <> if E-NELAB-JOIN throw then
   t ARG-R@ RN @ <> if E-NELAB-JOIN throw then
   t ARG-G@  VGLUE @ VN @ VGLUE-LOW  <> if E-NELAB-JOIN throw then
   VN @ 0 ?do
      t VMAX * i + ARG-Q @ i VQ@ QMERGE
      ix i  t i ARG-T@  EDGE-VALUE  i XV !
   loop ;

: TERM-BR-H ( n n n n n -- )
   {: ix:n t:n lo:n h:n l:n :}
   R-SPILL
   ix t EDGE-STAGE
   CTX BLD  CTX BLD HIR-OPCODE:BR HIR:ENSURE-OP  IR-BUILD:BEGIN-OP
   CTX BLD  VW MKEY ix NTAPE:SPAN@  IR-BUILD:SET-OP-SPAN
   VN @ 0 ?do
      CTX BLD  i XV @  IR-BUILD:ADD-OPERAND
   loop
   lo h LOOP-OPERANDS+
   l LOCAL-OPERANDS+
   TOK-LIVE @ 0<> if
      CTX BLD TOK IR-BUILD:ADD-OPERAND
   then
   CTX BLD  t BLOCK-ORD  IR-BUILD:ADD-SUCCESSOR
   CTX BLD IR-BUILD:END-OP drop
   CLOSE-BLOCK
   RN @ VDROP ;

: TERM-BR ( n n -- )
   CROSS-DO CROSS-L TERM-BR-H ;

: TERM-BRZ ( n n n -- )
   {: ix:n z:n o:n :}
   VN @ 1- VAT {: f:IR-ID:ir-value-id :}
   1 VDROP
   CTX BLD  CTX BLD HIR-OPCODE:BRZ HIR:ENSURE-OP  IR-BUILD:BEGIN-OP
   CTX BLD  VW MKEY ix NTAPE:SPAN@  IR-BUILD:SET-OP-SPAN
   CTX BLD f IR-BUILD:ADD-OPERAND
   CTX BLD  z BLOCK-ORD  IR-BUILD:ADD-SUCCESSOR
   CTX BLD  o BLOCK-ORD  IR-BUILD:ADD-SUCCESSOR
   CTX BLD IR-BUILD:END-OP drop
   CLOSE-BLOCK ;

\ A whole block that does nothing but hand the live values on.
: STUB-H ( n n n n n -- )
   {: ix:n t:n lo:n h:n l:n :}
   LIT-MARK {: m:n :}
   ix OPEN-PLAIN
   ix t lo h l TERM-BR-H
   m LIT-RELEASE ;

: STUB ( n n -- )
   CROSS-DO CROSS-L STUB-H ;

\ ---- reading the definition frame --------------------------------------------
: NAME-CK ( IR-ARENA:view n -- )
   NTAPE:KIND@ NTAPE-KIND:NAME NTAPE-KIND:EQ 0= if E-NELAB-SHAPE throw then ;

: MODE-CK ( IR-ARENA:view n NTAPE:mode -- )
   {: v:IR-ARENA:view ix:n want:NTAPE:mode :}
   v ix NTAPE:MODE@ want NTAPE-MODE:EQ 0= if E-NELAB-MODE throw then ;

\ ---- finding the locals groups -----------------------------------------------
: MODELED-AS? ( IR-ARENA:arena n HIR:meaning -- bool )
   {: r:IR-ARENA:arena ix:n m:HIR:meaning :}
   VW ix NTAPE:KIND@ NTAPE-KIND:NAME NTAPE-KIND:EQ 0= if false exit then
   ix WSYM {: sy:IR-ID:ir-symbol-id :}
   r sy HIR-WORD:MODELS? 0= if false exit then
   r sy HIR-WORD:MEANING@ m HIR-MEANING:EQ ;

\ ---- the one name a local may not take ----------------------------------------
: PRE-FRAME? ( IR-ARENA:arena IR-ID:ir-symbol-id -- bool )
   {: r:IR-ARENA:arena sy:IR-ID:ir-symbol-id :}
   r sy HIR-WORD:MODELS? 0= if false exit then
   r sy HIR-WORD:MEANING@ HIR-MEANING:CONTROL HIR-MEANING:EQ 0= if false exit then
   r sy HIR-WORD:CTRL@ HIR-CTRL:CLOSE-QUOT HIR-CTRL:EQ ;

\ The bare name, interned into this module so every mention reaches one symbol.
: DECLARE-LOCAL ( IR-ARENA:arena n -- )
   {: r:IR-ARENA:arena ix:n :}
   LN @ LMAX @ >= if E-NELAB-LOCAL-CAP throw then
   CTX BLD  VW MKEY ix NTAPE:SPELL@  LBUF LNAME-CAP IR-BUILD:SYMBOL-COPY {: u:n :}
   LBUF u HIR-WORD:LOCAL-NAME-LEN {: nu:n :}
   nu 1 < if E-NELAB-LOCAL throw then
   CTX BLD LBUF nu HIR-WORD:KEY-SPELL {: sy:IR-ID:ir-symbol-id :}
   r sy PRE-FRAME? if E-NELAB-LOCAL throw then
   sy LN @ LNAME !
   -1 LN @ LROW!
   -1 LN @ LEND!
   -1 LN @ LSLOT!
   LN @ 1+ LN ! ;

\ ---- the scopes the pre-pass is inside ---------------------------------------
variable SS-N                        \ open structures the pre-pass is inside
variable SS-L                        \ names in scope where the pre-pass stands
create SS-M CMAX cells allot         \ names in scope when each of them opened

: SS-RESET ( -- )
   0 SS-N !
   0 SS-L ! ;

: SS-PUSH ( -- )
   SS-N @ CMAX >= if E-NELAB-BLOCK throw then
   SS-L @  SS-N @ cells SS-M +  !
   SS-N @ 1+ SS-N ! ;

: SS-MARK ( -- n )
   SS-N @ 1 < if 0 exit then
   SS-N @ 1- cells SS-M + @ ;

: SS-REST ( n -- )
   {: ix:n :}
   SS-MARK {: m:n :}
   LN @ 0 ?do
      i LROW@ 0 >=  i LEND@ 0 <  and  i LSLOT@ m >=  and if ix i LEND! then
   loop
   m SS-L ! ;

: SS-CLOSE ( n -- )
   SS-REST
   SS-N @ 1 < if exit then
   SS-N @ 1- SS-N ! ;

\ Whether this row is a mention of a name that is in scope HERE.
: SCOPE-LOCAL? ( n -- bool )
   {: ix:n :}
   VW ix NTAPE:KIND@ NTAPE-KIND:NAME NTAPE-KIND:EQ 0= if false exit then
   ix WSYM {: sy:IR-ID:ir-symbol-id :}
   false
   LN @ 0 ?do
      i LROW@ 0 >=  i LROW@ ix <  and  i LEND@ 0 <  and if
         sy i LNAME @ NFROZEN:SAME-SYM? or
      then
   loop ;

: SCOPE-STEP ( IR-ARENA:arena n -- )
   {: r:IR-ARENA:arena ix:n :}
   ix SCOPE-LOCAL? if exit then
   r ix HIR-MEANING:CONTROL MODELED-AS? 0= if exit then
   r  ix WSYM  HIR-WORD:CTRL@
   MATCH HIR:ctrl
      open-if      OF SS-PUSH ENDOF
      mid-else     OF ix SS-REST ENDOF
      close-if     OF ix SS-CLOSE ENDOF
      open-begin   OF SS-PUSH ENDOF
      mid-while    OF ENDOF
      close-until  OF ix SS-CLOSE ENDOF
      close-repeat OF ix SS-CLOSE ENDOF
      close-again  OF ix SS-CLOSE ENDOF
      open-do      OF SS-PUSH ENDOF
      open-do-skip OF SS-PUSH ENDOF
      close-loop   OF ix SS-CLOSE ENDOF
      index        OF ENDOF
      outer-index  OF ENDOF
      drop-loop    OF ENDOF
      early-leave  OF ENDOF
      early-exit   OF ENDOF
      self-call    OF ENDOF
      open-match   OF SS-PUSH ENDOF
      match-arm    OF SS-PUSH ENDOF
      close-arm    OF ix SS-CLOSE ENDOF
      close-match  OF ix SS-CLOSE ENDOF
      open-case    OF SS-PUSH ENDOF
      close-case   OF ix SS-CLOSE ENDOF
      make-bundle  OF ENDOF
      open-quot    OF SS-PUSH ENDOF
      close-quot   OF ix SS-CLOSE ENDOF
      bind-defer   OF ENDOF
      exec         OF ENDOF
      catch        OF ENDOF
      finally      OF ENDOF
      tick         OF ENDOF
      eval         OF ENDOF
   ;MATCH ;

: GROUP-OPEN ( n -- )
   {: ix:n :}
   LG-N @ LMAX @ >= if E-NELAB-LOCAL-CAP throw then
   LG-N @ {: g:n :}
   g 1+ LG-N !
   ix g cells LG-A + !
   -1 g cells LG-B + !
   0 g cells LG-K + !
   LN @ g cells LG-F + !
   LN @ LG-K0 !
   g LG-OPEN ! ;

: GROUP-CLOSE ( n -- )
   {: ix:n :}
   LG-OPEN @ {: g:n :}
   LG-K0 @ {: k0:n :}
   SS-L @ {: s:n :}
   ix g cells LG-B + !
   LN @ k0 -  g cells LG-K + !
   LN @ k0 ?do
      ix i LROW!
      s i + k0 -  i LSLOT!
   loop
   LN @ k0 -  s +  SS-L !
   -1 LG-OPEN ! ;

: SCAN-STEP ( IR-ARENA:arena n -- )
   {: r:IR-ARENA:arena ix:n :}
   LG-OPEN @ 0 < if
      r ix HIR-MEANING:OPEN-LOCALS MODELED-AS? if ix GROUP-OPEN exit then
      r ix SCOPE-STEP exit
   then
   VW ix NTAPE:KIND@ NTAPE-KIND:NAME NTAPE-KIND:EQ 0= if E-NELAB-LOCAL throw then
   r ix HIR-MEANING:CLOSE-LOCALS MODELED-AS? if ix GROUP-CLOSE exit then
   r ix HIR-MEANING:OPEN-LOCALS MODELED-AS? if E-NELAB-LOCAL throw then
   r ix DECLARE-LOCAL ;

\ A name still in scope when the body ends is in scope for the whole of it.
: LOCALS-SCAN ( IR-ARENA:arena n n -- )
   {: r:IR-ARENA:arena lo:n hi:n :}
   LRESET
   SS-RESET
   hi lo ?do
      r i SCAN-STEP
   loop
   LG-OPEN @ 0 >= if E-NELAB-LOCAL throw then
   LN @ 0 ?do
      i LEND@ 0 < if hi i LEND! then
   loop ;

\ ---- is this row a particular control word? ----------------------------------
: ROW-CTRL? ( IR-ARENA:arena n HIR:ctrl -- bool )
   {: r:IR-ARENA:arena ix:n want:HIR:ctrl :}
   VW ix NTAPE:KIND@ NTAPE-KIND:NAME NTAPE-KIND:EQ 0= if false exit then
   ix WSYM {: sy:IR-ID:ir-symbol-id :}
   r sy HIR-WORD:MODELS? 0= if false exit then
   r sy HIR-WORD:MEANING@ HIR-MEANING:CONTROL HIR-MEANING:EQ 0= if false exit then
   r sy HIR-WORD:CTRL@ want HIR-CTRL:EQ ;

\ ---- how far a body token may be from the definition's first ------------------
\ Body tokens one definition may have.
variable TMAX                       \ token count of the current unit
PTR-VARIABLE TOK-TABLES
10 constant TOK-FIELDS

: TOK-ROOM ( n -- ) {: n:n :}
   n IR-CTX:SCRATCH-LIMIT TOK-FIELDS cells / > if E-IR-CTX-SCRATCH throw then
   n TMAX !
   CTX n TOK-FIELDS * cells IR-CTX:SCRATCH-TAKE drop TOK-TABLES ! ;

: TOK-FIELD ( n -- ptr n )
   TMAX @ * cells TOK-TABLES @ + ;

: TOK-CK ( n -- n )
   dup 0 < over TMAX @ >= or if E-NELAB-BLOCK throw then ;

: QOPENED ( -- ptr n ) 0 TOK-FIELD ;
: QOWN ( -- ptr n ) 1 TOK-FIELD ;

: QUOT-RESET ( -- )
   0 QN !
   -1 QD !
   QOWNER-DEF QCUR !
   TMAX @ 0 ?do
      -1 i cells QOPENED + !
      QOWNER-DEF i cells QOWN + !
   loop ;

: QOPENED@ ( n -- n )
   TOK-CK cells QOPENED + @ ;

: QOWN@ ( n -- n )
   TOK-CK cells QOWN + @ ;

: QINSIDE? ( n -- bool )
   QOWN@ QOWNER-DEF <> ;

128 constant QSPELL-CAP

here CELL 1- and CELL swap - CELL 1- and allot
create QSPELL-BUF QSPELL-CAP allot

: QSPELL ( n -- ptr u8 n )
   {: ix:n :}
   CTX BLD  VW MKEY ix NTAPE:SPELL@  QSPELL-BUF QSPELL-CAP IR-BUILD:SYMBOL-COPY
   QSPELL-BUF swap ;

: PARSE-IMM-TOKEN? ( n -- bool ) {: ix:n :}
   VW ix NTAPE:KIND@ NTAPE-KIND:NAME NTAPE-KIND:EQ 0= if false exit then
   \ Leave oversized names to normal admission, which records their refusal.
   CTX BLD VW MKEY ix NTAPE:SPELL@ IR-BUILD:SYMBOL-LEN
   QSPELL-CAP > if false exit then
   ix QSPELL CHECKER-OWNER:PARSE-IMM? ;

: WALK-SKIP? ( n -- bool )
   dup QOWN@ QCUR @ <> if drop true exit then
   PARSE-IMM-TOKEN? ;

\ ---- where a body's arity comes from -----------------------------------------
: QARG-FILL ( n n -- )
   {: ix:n j:n :}
   VN @ 1- j -  VQ@ {: k:n :}
   k 0 < if exit then
   ix j NDICT:CALL-QUOT-IN
   over NDICT:QUOT-NONE = if
      \ A trusted mint can stop the checker's walk before this call. The
      \ callee's declared simple callback still supplies its ordinary ABI.
      2drop ix QSPELL j NDICT:SPELL-QUOT-DIN
   then {: qi:n qo:n :}
   qi NDICT:QUOT-NONE = if
      \ A polymorphic consumer can store or forward a quotation whose
      \ calling convention is already known from the checked parameter.
      k QIN@ QNONE <> if exit then
      k QAT@ QUOT-REFUSE
   then
   k qi qo 0 max QFILL ;

: QCALL-FILL ( n n -- )
   {: ix:n a:n :}
   a VN @ > if exit then
   a 0 ?do
      ix i QARG-FILL
   loop ;

: QRET1-FILL ( n -- )
   {: j:n :}
   VN @ 1- j -  VQ@ {: k:n :}
   k 0 < if exit then
   0 QSPELL j NDICT:SPELL-QUOT-DOUT {: qi:n qo:n :}
   qi NDICT:QUOT-NONE = if k QAT@ QUOT-REFUSE then
   k qi qo QFILL ;

: QRET-FILL ( n -- )
   {: out:n :}
   out VN @ <> if exit then
   out 0 ?do
      i QRET1-FILL
   loop ;

\ An enclosing walk supplies the calling convention before its child is built.
: QCONSUMED-CK ( n -- )
   dup QIN@ QNONE = if QAT@ QUOT-REFUSE else drop then ;

: QOPEN-ROW ( n -- )
   {: ix:n :}
   QN @ 1+ QUOT-ROOM
   QN @ {: k:n :}
   k k QPARENT !
   ix k cells QAT + !
   ix 1+ k cells QLO + !
   QNONE k cells QIN + !
   QNONE k cells QOUT + !
   QBASE @ k + 1+  k cells QFUN + !
   k ix TOK-CK cells QOPENED + !
   k QD !
   k 1+ QN ! ;

\ A quotation supplied by a parameter, a called word, or a named tick has no
\ body in this emission, but retains the same known calling convention.
: QKNOWN ( n n n n -- )
   {: ix:n qi:n qo:n cellix:n :}
   QN @ 1+ QUOT-ROOM
   QN @ {: k:n :}
   k k QPARENT !
   ix k cells QAT + !
   0 k cells QLO + !
   0 k cells QHI + !
   qi k cells QIN + !
   qo k cells QOUT + !
   QPARAM k cells QFUN + !
   k 1+ QN !
   k cellix VQ! ;

: QOPEN-PARAM ( n n -- )
   {: j:n cellix:n :}
   0 QSPELL j NDICT:SPELL-QUOT-DIN {: qi:n qo:n :}
   qi NDICT:QUOT-NONE = if exit then
   0 qi qo cellix QKNOWN ;

: QRESULTS-FILL ( n n -- ) {: ix:n out:n :}
   out 0 ?do
      ix i NDICT:CALL-QUOT-OUT {: qi:n qo:n :}
      qi NDICT:QUOT-NONE <> if ix qi qo VN @ 1- i - QKNOWN then
   loop ;

: QPARAMS-OPEN ( n -- )
   {: in:n :}
   in 0 ?do
      i  in 1- i -  QOPEN-PARAM
   loop ;

: QCLOSE-ROW ( n -- )
   {: ix:n :}
   QD @ QROW-CK {: k:n :}
   ix k cells QHI + !
   k QAT@ QOWN@ QD ! ;

: QSCAN-STEP ( IR-ARENA:arena n -- )
   {: r:IR-ARENA:arena ix:n :}
   \ The opening token belongs to its enclosing body and records where to
   \ return when this quotation closes. Inner tokens retain their own owner.
   QD @ ix TOK-CK cells QOWN + !
   r ix HIR-CTRL:OPEN-QUOT ROW-CTRL? if
      ix QOPEN-ROW
      exit
   then
   r ix HIR-CTRL:CLOSE-QUOT ROW-CTRL? if
      QD @ 0 < if ix QUOT-REFUSE then
      ix QCLOSE-ROW
   then ;

: QUOT-SCAN ( IR-ARENA:arena n n -- )
   {: r:IR-ARENA:arena lo:n hi:n :}
   QUOT-RESET
   hi lo ?do
      r i QSCAN-STEP
   loop
   QD @ 0 >= if QD @ QAT@ QUOT-REFUSE then ;

\ ---- the three tag-dispatch forms, read before anything else reads a word -----
\ The modes are the engine's own CMM numbers.
0 constant MR-NONE                   \ an ordinary body token
1 constant MR-FAMILY                 \ the family operand of a `MATCH` or a `construct`
2 constant MR-VARIANT                \ the variant operand of one
3 constant MR-DEFER                  \ the deferred word `is` binds to
4 constant MR-TICK                   \ the word named by compile-time tick

0 constant MM-OFF                    \ no operand token is expected
1 constant MM-CON-FAM                \ `construct` has been read; its family is next
2 constant MM-CON-VAR                \ and then its variant
3 constant MM-FAM                    \ `MATCH` has been read; its family is next
4 constant MM-VARIANT                \ a variant token, or the `;MATCH` that ends the form
5 constant MM-OF                     \ the `of` that opens the arm of the variant just read

0 constant MK-MATCH                  \ the open form is a `MATCH`
1 constant MK-CASE                   \ the open form is a `case`

128 constant MTOK-CAP                \ bytes of one operand token this pass can read

here CELL 1- and CELL swap - CELL 1- and allot
: MROLE ( -- ptr n ) 2 TOK-FIELD ;
: MTAG ( -- ptr n ) 3 TOK-FIELD ;
: MPAD ( -- ptr n ) 4 TOK-FIELD ;
: MPAY ( -- ptr n ) 5 TOK-FIELD ;
: MWID ( -- ptr n ) 6 TOK-FIELD ;
: MGLUE ( -- ptr n ) 7 TOK-FIELD ;
: MEND ( -- ptr n ) 8 TOK-FIELD ;
create MTOK MTOK-CAP allot

CMAX constant MSMAX                  \ open tag-dispatch forms, as the control stack's own ceiling
create MS-KIND MSMAX cells allot     \ MK-MATCH or MK-CASE
create MS-FAM MSMAX cells allot      \ the family a `MATCH` frame is over
create MS-WID MSMAX cells allot      \ and the cells one value of its instantiation occupies; a `case` frame's subject is the one cell DO-OPEN-CASE gives it
create MS-ARM MSMAX cells allot      \ whether the pass is inside one of its arms
create MS-OF MSMAX cells allot       \ the row of the `of` that opened the arm read last
variable MSN                         \ how many forms are open
variable MM                          \ which operand token the pass is expecting
variable CB-ROW                      \ the `MATCH` or `construct` row being read
variable CB-FAM                      \ and the family its first operand named
variable MV-ROW                      \ the variant row read last, whose `of` is next

: MATCH-RESET ( -- )
   MM-OFF MM !
   0 MSN !
   -1 CB-ROW !
   -1 MV-ROW !
   TMAX @ 0 ?do
      MR-NONE i cells MROLE + !
      0 i cells MEND + !
   loop ;

: MROLE@ ( n -- n )   TOK-CK cells MROLE + @ ;
: MTAG@ ( n -- n )    TOK-CK cells MTAG + @ ;
: MPAD@ ( n -- n )    TOK-CK cells MPAD + @ ;
: MPAY@ ( n -- n )    TOK-CK cells MPAY + @ ;
: MWID@ ( n -- n )    TOK-CK cells MWID + @ ;
: MGLUE@ ( n -- n )   TOK-CK cells MGLUE + @ ;
: MEND@ ( n -- bool ) TOK-CK cells MEND + @ 0<> ;

: MOPERAND? ( n -- bool )
   MROLE@ MR-NONE <> ;

: MROLE! ( n n -- ) {: ix:n r:n :}
   r ix TOK-CK cells MROLE + ! ;

: MVAR! ( n n -- ) {: ix:n tag:n :}
   tag ix TOK-CK cells MTAG + ! ;

: MARM! ( n n n n -- ) {: ix:n tag:n pads:n pay:n :}
   tag ix TOK-CK cells MTAG + !
   pads ix TOK-CK cells MPAD + !
   pay ix TOK-CK cells MPAY + ! ;

: MMATCH! ( n n n -- ) {: ix:n w:n ord:n :}
   w ix TOK-CK cells MWID + !
   ord ix TOK-CK cells MTAG + ! ;

: MGLUE! ( n n -- ) {: ix:n glue:n :}
   glue ix TOK-CK cells MGLUE + ! ;

: MEND! ( n -- ) {: ix:n :}
   1 ix TOK-CK cells MEND + ! ;

\ ---- the forms this pass has open --------------------------------------------
: MS-AT ( n -- n )
   dup 0 < over MSN @ >= or if E-NELAB-MATCH throw then ;

: MS-TOP ( -- n )
   MSN @ 1- MS-AT ;

: MS-PUSH ( n n n -- ) {: kind:n fam:n w:n :}
   MSN @ MSMAX >= if E-NELAB-BLOCK throw then
   kind MSN @ cells MS-KIND + !
   fam MSN @ cells MS-FAM + !
   w MSN @ cells MS-WID + !
   0 MSN @ cells MS-ARM + !
   -1 MSN @ cells MS-OF + !
   MSN @ 1+ MSN ! ;

: MS-POP ( -- )
   MSN @ 1 < if E-NELAB-MATCH throw then
   MSN @ 1- MSN ! ;

: MS-MATCH? ( n -- bool )   MS-AT cells MS-KIND + @ MK-MATCH = ;
: MS-FAM@ ( n -- n )        MS-AT cells MS-FAM + @ ;
: MS-WID@ ( n -- n )        MS-AT cells MS-WID + @ ;
: MS-ARM? ( n -- bool )     MS-AT cells MS-ARM + @ 0<> ;
: MS-OF@ ( n -- n )         MS-AT cells MS-OF + @ ;

: MS-ARM! ( n n -- ) {: t:n ofix:n :}
   1 t MS-AT cells MS-ARM + !
   ofix t MS-AT cells MS-OF + ! ;

: MS-ARM-END! ( n -- ) {: t:n :}
   0 t MS-AT cells MS-ARM + ! ;

\ ---- one operand token -------------------------------------------------------
: MTOK$ ( n -- ptr u8 n ) {: ix:n :}
   VW ix NTAPE:KIND@ NTAPE-KIND:NAME NTAPE-KIND:EQ 0= if E-NELAB-MATCH throw then
   MTOK  CTX BLD  VW MKEY ix NTAPE:SPELL@  MTOK MTOK-CAP IR-BUILD:SYMBOL-COPY ;

\ ---- the operand steps, one per mode -----------------------------------------
: MSCAN-MATCH-FAM ( n -- ) {: ix:n :}
   ix MTOK$ NFAM:MATCH-FAM {: fam:n ok:bool :}
   ok 0= if E-NELAB-MATCH throw then
   ix MR-FAMILY MROLE!
   ix NDICT:MATCH-CELLS {: w:n :}
   w NDICT:MATCH-NONE = if E-NELAB-MATCH throw then
   w fam NFAM:WIDTH < if E-NELAB-MATCH throw then
   CB-ROW @  w  fam NFAM:NAME$ NTRAP:FAMILY  MMATCH!
   MK-MATCH fam w MS-PUSH
   MM-VARIANT MM ! ;

: MSCAN-VARIANT ( n -- ) {: ix:n :}
   MS-TOP {: t:n :}
   ix MTOK$ t MS-FAM@ NFAM:VARIANT {: vid:n ok:bool :}
   ok 0= if E-NELAB-MATCH throw then
   ix MR-VARIANT MROLE!
   ix  vid NFAM:TAG  MVAR!
   ix MV-ROW !
   MM-OF MM ! ;

\ The variant's row is the arm's, so the arm's own numbers are known here.
: MSCAN-OF ( n -- ) {: ix:n :}
   MV-ROW @ {: vix:n :}
   vix 0 < if E-NELAB-MATCH throw then
   MS-TOP {: t:n :}
   ix NDICT:MATCH-CELLS {: pads:n :}
   pads NDICT:MATCH-NONE = if E-NELAB-MATCH throw then
   t MS-WID@ 1- pads - {: pay:n :}
   pay 0 < if E-NELAB-MATCH throw then
   ix NDICT:MATCH-PAYLOAD {: cells:n glue:n :}
   cells pay <> glue NDICT:GLUE-UNKNOWN = or if E-NELAB-MATCH throw then
   ix  vix MTAG@  pads  pay  MARM!
   ix glue MGLUE!
   t ix MS-ARM!
   -1 MV-ROW !
   MM-OFF MM ! ;

: MSCAN-CON-FAM ( n -- ) {: ix:n :}
   ix MTOK$ NFAM:CON-FAM {: fam:n ok:bool :}
   ok 0= if E-NELAB-MATCH throw then
   ix MR-FAMILY MROLE!
   fam CB-FAM !
   MM-CON-VAR MM ! ;

: MSCAN-CON-VAR ( n -- ) {: ix:n :}
   ix MTOK$ CB-FAM @ NFAM:VARIANT {: vid:n ok:bool :}
   ok 0= if E-NELAB-MATCH throw then
   ix MR-VARIANT MROLE!
   CB-FAM @ vid NFAM:PADS  ix NDICT:CON-PADS +  {: pads:n :}
   CB-ROW @  vid NFAM:TAG  pads  vid NFAM:PAY-CELLS  MARM!
   MM-OFF MM ! ;

\ ---- the keywords the pass reacts to -----------------------------------------
: MSCAN-ENDOF ( -- )
   MS-TOP {: t:n :}
   t MS-ARM? 0= if E-NELAB-MATCH throw then
   t MS-ARM-END!
   t MS-MATCH? if MM-VARIANT MM ! then ;

\ `;MATCH` is the moment the LAST arm becomes known.
: MSCAN-SEMI ( -- )
   MS-TOP {: t:n :}
   t MS-MATCH? 0= if E-NELAB-MATCH throw then
   t MS-ARM? if E-NELAB-MATCH throw then
   t MS-OF@ {: ofix:n :}
   ofix 0 < if E-NELAB-MATCH throw then
   ofix MEND!
   MS-POP
   MM-OFF MM ! ;

: MSCAN-ENDCASE ( -- )
   MS-TOP {: t:n :}
   t MS-MATCH? if E-NELAB-MATCH throw then
   t MS-ARM? if E-NELAB-MATCH throw then
   MS-POP ;

: MSCAN-CASE-OF ( n -- ) {: ix:n :}
   MS-TOP {: t:n :}
   t MS-MATCH? if E-NELAB-MATCH throw then
   t MS-ARM? if E-NELAB-MATCH throw then
   t ix MS-ARM! ;

: MSCAN-OPEN ( IR-ARENA:arena n -- bool ) {: r:IR-ARENA:arena ix:n :}
   r ix HIR-CTRL:OPEN-MATCH ROW-CTRL? if
      ix CB-ROW !  MM-FAM MM !  true exit
   then
   r ix HIR-CTRL:MAKE-BUNDLE ROW-CTRL? if
      ix CB-ROW !  MM-CON-FAM MM !  true exit
   then
   r ix HIR-CTRL:OPEN-CASE ROW-CTRL? if
      MK-CASE 0 1 MS-PUSH  true exit
   then
   false ;

: MSCAN-CLOSE ( IR-ARENA:arena n -- bool ) {: r:IR-ARENA:arena ix:n :}
   r ix HIR-CTRL:CLOSE-ARM ROW-CTRL? if MSCAN-ENDOF true exit then
   r ix HIR-CTRL:CLOSE-MATCH ROW-CTRL? if MSCAN-SEMI true exit then
   r ix HIR-CTRL:CLOSE-CASE ROW-CTRL? if MSCAN-ENDCASE true exit then
   r ix HIR-CTRL:MATCH-ARM ROW-CTRL? if ix MSCAN-CASE-OF true exit then
   false ;

: MSCAN-STEP ( IR-ARENA:arena n -- ) {: r:IR-ARENA:arena ix:n :}
   ix IN-DECL? if exit then
   MM @ MM-FAM = if ix MSCAN-MATCH-FAM exit then
   MM @ MM-VARIANT = if
      r ix HIR-CTRL:CLOSE-MATCH ROW-CTRL? if MSCAN-SEMI exit then
      ix MSCAN-VARIANT exit
   then
   MM @ MM-OF = if
      r ix HIR-CTRL:MATCH-ARM ROW-CTRL? 0= if E-NELAB-MATCH throw then
      ix MSCAN-OF exit
   then
   MM @ MM-CON-FAM = if ix MSCAN-CON-FAM exit then
   MM @ MM-CON-VAR = if ix MSCAN-CON-VAR exit then
   ix LOCAL-OF 0 >= if exit then
   r ix MSCAN-OPEN if exit then
   r ix MSCAN-CLOSE drop ;

\ Walked once, before anything reads the word model for a body word.
: MATCH-SCAN ( IR-ARENA:arena n n -- ) {: r:IR-ARENA:arena lo:n hi:n :}
   MATCH-RESET
   hi lo ?do
      r i MSCAN-STEP
   loop
   MSN @ 0<> if E-NELAB-MATCH throw then
   MM @ MM-OFF <> if E-NELAB-MATCH throw then ;

\ ---- the deferred word `is` names --------------------------------------------
: DSCAN-STEP ( IR-ARENA:arena n n -- )
   {: r:IR-ARENA:arena n:n ix:n :}
   ix IN-DECL? if exit then
   ix LOCAL-OF 0 >= if exit then
   r ix HIR-CTRL:BIND-DEFER ROW-CTRL?
   r ix HIR-CTRL:TICK ROW-CTRL? or 0= if exit then
   ix 1+ {: t:n :}
   t n >= if E-NELAB-DEFER throw then
   VW t NTAPE:KIND@ NTAPE-KIND:NAME NTAPE-KIND:EQ 0= if E-NELAB-DEFER throw then
   r ix HIR-CTRL:TICK ROW-CTRL? if t MR-TICK MROLE!
   else t MR-DEFER MROLE! then ;

: DEFER-SCAN ( IR-ARENA:arena n n -- ) {: r:IR-ARENA:arena lo:n hi:n :}
   hi lo ?do
      r hi i DSCAN-STEP
   loop ;

\ ---- the names the dialect does not model, before anything reads the model ----
\ A name the dialect does not model is resolved through dict.f at the point it
\ is used, in the order the engine resolves the body that wrote it.
: RESOLVE-STEP ( IR-ARENA:arena IR-ARENA:arena n -- )
   {: p:IR-ARENA:arena r:IR-ARENA:arena ix:n :}
   ix MOPERAND? if exit then
   ix IN-DECL? if exit then
   ix LOCAL-OF 0 >= if exit then
   VW ix NTAPE:KIND@ NTAPE-KIND:NAME NTAPE-KIND:EQ 0= if exit then
   ix WSYM {: sy:IR-ID:ir-symbol-id :}
   r sy HIR-WORD:MODELS? if exit then
   CTX BLD r sy HIR-WORD:RESOLVE-FIXED if exit then
   CTX BLD p r sy HIR-WORD:RESOLVE-CALLABLE drop ;

: RESOLVE-SCAN ( IR-ARENA:arena IR-ARENA:arena n n -- )
   {: p:IR-ARENA:arena r:IR-ARENA:arena lo:n hi:n :}
   hi lo ?do
      p r i RESOLVE-STEP
   loop ;

\ ---- which control actions stage a call, and which call ----------------------
: CTRL-CALL? ( HIR:ctrl -- HIR:opcode bool )
   {: k:HIR:ctrl :}
   k HIR-CTRL:SELF-CALL HIR-CTRL:EQ if HIR-OPCODE:CALL true exit then
   k HIR-CTRL:BIND-DEFER HIR-CTRL:EQ if HIR-OPCODE:WORDCALL true exit then
   k HIR-CTRL:EXEC HIR-CTRL:EQ if HIR-OPCODE:WORDCALL true exit then
   k HIR-CTRL:CATCH HIR-CTRL:EQ if HIR-OPCODE:WORDCALL true exit then
   k HIR-CTRL:FINALLY HIR-CTRL:EQ if HIR-OPCODE:WORDCALL true exit then
   k HIR-CTRL:EVAL HIR-CTRL:EQ if HIR-OPCODE:WORDCALL true exit then
   HIR-OPCODE:CALL false ;

: SYM-ORDER? ( IR-ARENA:arena IR-ID:ir-symbol-id -- bool )
   {: r:IR-ARENA:arena sy:IR-ID:ir-symbol-id :}
   r sy HIR-WORD:MODELS? 0= if false exit then
   r sy HIR-WORD:MEANING@ {: m:HIR:meaning :}
   m HIR-MEANING:OP HIR-MEANING:EQ if
      CTX BLD  CTX BLD  r sy HIR-WORD:OPCODE@  HIR:ENSURE-OP  TOKEN-OPERANDS
      0<> exit
   then
   m HIR-MEANING:CONST-OP HIR-MEANING:EQ if
      CTX BLD  CTX BLD  r sy HIR-WORD:CONST-OPCODE@  HIR:ENSURE-OP  TOKEN-OPERANDS
      0<> exit
   then
   false ;

\ ---- does this definition touch memory at all? -------------------------------
: WORD-ORDER? ( IR-ARENA:arena n -- bool )
   {: r:IR-ARENA:arena ix:n :}
   VW ix NTAPE:KIND@ NTAPE-KIND:NAME NTAPE-KIND:EQ 0= if false exit then
   ix WSYM {: sy:IR-ID:ir-symbol-id :}
   r sy HIR-WORD:MODELS? 0= if false exit then
   r sy HIR-WORD:MEANING@ {: m:HIR:meaning :}
   m HIR-MEANING:CALLABLE HIR-MEANING:EQ if
      CTX BLD  CTX BLD  HIR-OPCODE:WORDCALL HIR:ENSURE-OP  TOKEN-OPERANDS
      0<> exit
   then
   m HIR-MEANING:CONTROL HIR-MEANING:EQ if
      r sy HIR-WORD:CTRL@ CTRL-CALL? {: op:HIR:opcode calls:bool :}
      calls 0= if false exit then
      CTX BLD  CTX BLD  op HIR:ENSURE-OP  TOKEN-OPERANDS 0<> exit
   then
   r sy SYM-ORDER? ;

: MEM-SCAN ( IR-ARENA:arena n n -- )
   {: r:IR-ARENA:arena lo:n hi:n :}
   0 TOK-NEED !
   hi lo ?do
      i MOPERAND? 0=  i IN-DECL? 0=  and  i LOCAL-OF 0 <  and if
         r i WORD-ORDER? if 1 TOK-NEED ! then
      then
   loop ;

\ ---- does this definition CALL at all? ---------------------------------------
: QUOTATION-STORE? ( IR-ARENA:arena n -- bool ) {: r:IR-ARENA:arena ix:n :}
   r ix WSYM HIR-WORD:OPCODE@ HIR-OPCODE:STORE HIR-OPCODE:EQ 0= if false exit then
   ix 1 NDICT:CALL-QUOT-IN drop NDICT:QUOT-NONE <> ;

: GUARDED-STORE? ( HIR:opcode -- bool )
   dup HIR-OPCODE:STORE HIR-OPCODE:EQ
   swap HIR-OPCODE:BSTORE HIR-OPCODE:EQ or ;

: WORD-CALL? ( IR-ARENA:arena n -- bool )
   {: r:IR-ARENA:arena ix:n :}
   VW ix NTAPE:KIND@ NTAPE-KIND:NAME NTAPE-KIND:EQ 0= if false exit then
   ix WSYM {: sy:IR-ID:ir-symbol-id :}
   r sy HIR-WORD:MODELS? 0= if false exit then
   r sy HIR-WORD:MEANING@ {: m:HIR:meaning :}
   m HIR-MEANING:CALLABLE HIR-MEANING:EQ if true exit then
   m HIR-MEANING:OP HIR-MEANING:EQ if r sy HIR-WORD:OPCODE@ GUARDED-STORE? exit then
   m HIR-MEANING:CONTROL HIR-MEANING:EQ if
      r sy HIR-WORD:CTRL@ CTRL-CALL? {: op:HIR:opcode calls:bool :}
      calls exit
   then
   false ;

\ ---- which locals a call can reach -------------------------------------------
variable LSN                         \ loops the scan is inside
variable LS-SEQ                      \ identity of the current outer loop

: OPENS-LOOP? ( IR-ARENA:arena n -- bool )
   {: r:IR-ARENA:arena ix:n :}
   r ix HIR-CTRL:OPEN-BEGIN ROW-CTRL?
   r ix HIR-CTRL:OPEN-DO ROW-CTRL? or
   r ix HIR-CTRL:OPEN-DO-SKIP ROW-CTRL? or ;

: CLOSES-LOOP? ( IR-ARENA:arena n -- bool )
   {: r:IR-ARENA:arena ix:n :}
   r ix HIR-CTRL:CLOSE-UNTIL ROW-CTRL?
   r ix HIR-CTRL:CLOSE-REPEAT ROW-CTRL? or
   r ix HIR-CTRL:CLOSE-AGAIN ROW-CTRL? or
   r ix HIR-CTRL:CLOSE-LOOP ROW-CTRL? or ;

: LS-PUSH ( -- )
   LSN @ 0= if 1 LS-SEQ +! then
   LSN @ 1+ LSN ! ;

: LS-POP ( -- )
   LSN @ 1 < if E-NELAB-CTRL throw then
   LSN @ 1- LSN ! ;

\ Before the first call, retain locals read in the current outer loop. A call
\ in that loop can precede those reads on its next iteration. Later reads are
\ already covered by CALL-NEED, so neither loop bitsets nor local masks help.
: LS-CALL+ ( -- )
   LSN @ 0= if exit then
   LN @ 0 ?do
      i cells LPEND + @ LS-SEQ @ = if i LCROSS+ then
   loop ;

: LS-PEND+ ( n -- )
   {: k:n :}
   LSN @ 0= if exit then
   LS-SEQ @ k cells LPEND + ! ;

\ A call marks the whole definition and every loop it is inside.
: CROSS-STEP ( IR-ARENA:arena n -- )
   {: r:IR-ARENA:arena ix:n :}
   ix MOPERAND? if exit then
   ix IN-DECL? if exit then
   ix LOCAL-OF {: k:n :}
   k 0 >= if
      CALL-NEED @ 0<> if k LCROSS+ exit then
      k LS-PEND+ exit
   then
   r ix WORD-CALL? if
      1 CALL-NEED !
      LS-CALL+ exit
   then
   r ix OPENS-LOOP? if LS-PUSH exit then
   r ix CLOSES-LOOP? if LS-POP then ;

: CROSS-SCAN ( IR-ARENA:arena n n -- )
   {: r:IR-ARENA:arena lo:n hi:n :}
   0 CALL-NEED !
   0 LSN !
   0 LS-SEQ !
   hi lo ?do
      r i CROSS-STEP
   loop
   LSN @ 0<> if E-NELAB-CTRL throw then ;

\ ---- the block skeleton ------------------------------------------------------
here CELL 1- and CELL swap - CELL 1- and allot
: JOIN-TAB ( -- ptr n ) 9 TOK-FIELD ;

: JOIN-RESET ( -- )
   TMAX @ 0 ?do
      -1 i cells JOIN-TAB + !
   loop ;

: JOIN-OF ( n -- n )
   TOK-CK cells JOIN-TAB + @ ;

\ A token the skeleton recorded nothing against is a refusal, not a zero.
: JOIN-CK ( n -- n )
   dup 0 < if E-NELAB-CTRL throw then ;

: JOIN! ( n n -- )
   {: ix:n j:n :}
   j ix TOK-CK cells JOIN-TAB + ! ;

: SK-PUSH ( HIR:ctrl n -- )
   {: k:HIR:ctrl ix:n :}
   k 0 ix CS-PUSH ;

: SK-ELSE ( n -- )
   {: ix:n :}
   HIR-CTRL:OPEN-IF CS-OPENER-CK {: t:n :}
   t CS-ELSE? if E-NELAB-CTRL throw then
   PATH-DEAD? if -1 t CS-END! else NB @ 1+ NB ! then
   PATH-LIVE PATH-END !
   t CS-JOIN@ NB @ JOIN!
   ix t CS-ARM! ;

: SK-BOTH-ENDED? ( n bool -- bool )
   {: t:n armend:bool :}
   t CS-ELSE? 0= if false exit then
   armend 0= if false exit then
   t CS-END@ 0<> ;

: SK-CLOSE-IF ( -- )
   HIR-CTRL:OPEN-IF CS-OPENER-CK {: t:n :}
   t CS-PENDING {: key:n :}
   PATH-ENDED? {: armend:bool :}
   armend 0= if NB @ 1+ NB ! then
   t armend SK-BOTH-ENDED? if
      CS-POP
      PATH-DEAD PATH-END !
      exit
   then
   PATH-LIVE PATH-END !
   key NB @ JOIN!
   CS-POP ;

: SK-WHILE ( -- )
   HIR-CTRL:OPEN-BEGIN CS-OPENER-CK {: t:n :}
   PATH-ENDED? if E-NELAB-CTRL throw then
   t CS-WHILE+
   NB @ 2 + NB ! ;

: SK-REPEAT ( -- )
   HIR-CTRL:OPEN-BEGIN CS-OPENER-CK {: t:n :}
   t CS-NW@ 0= if E-NELAB-CTRL throw then
   PATH-ENDED? 0= if NB @ 1+ NB ! then
   t CS-JOIN@ NB @ JOIN!
   CS-POP
   PATH-LIVE PATH-END ! ;

: SK-UNTIL ( -- )
   HIR-CTRL:OPEN-BEGIN CS-OPENER-CK {: t:n :}
   t CS-NW@ 0<> if E-NELAB-CTRL throw then
   NB @ 2 + NB !
   CS-POP ;

\ `again` opens NOTHING: its back edge is unconditional and its loop has no
\ exit edge at all.
: SK-AGAIN ( -- )
   HIR-CTRL:OPEN-BEGIN CS-OPENER-CK {: t:n :}
   t CS-NW@ 0<> if E-NELAB-CTRL throw then
   PATH-ENDED? 0= if NB @ 1+ NB ! then
   CS-POP
   PATH-DEAD PATH-END ! ;

: SK-LEAVE ( -- )
   DO-OPEN-N 0= if E-NELAB-CTRL throw then
   0 DO-INNER-NTH CS-JOINED+
   NB @ 1+ NB !
   PATH-DEAD PATH-END ! ;


: SK-CLOSE-LOOP ( -- )
   HIR-CTRL:OPEN-DO CS-OPENER-CK {: t:n :}
   PATH-ENDED? 0= if NB @ 3 + NB ! t CS-JOINED+ then
   t CS-JOINED? if
      t CS-JOIN@ NB @ JOIN!
      PATH-LIVE PATH-END !
   else PATH-DEAD PATH-END ! then
   CS-POP ;

\ ---- what a tag dispatch counts ----------------------------------------------
: SK-ARM ( n -- ) {: ix:n :}
   CS-N @ 1 < if E-NELAB-CTRL throw then
   CS-TOP {: t:n :}
   t CS-ADT? 0= if E-NELAB-CTRL throw then
   t CS-OFIX@ 0 >= if E-NELAB-CTRL throw then
   NB @ 2 + NB !
   ix t CS-OFIX! ;

: SK-CLOSE-ARM ( -- )
   CS-N @ 1 < if E-NELAB-CTRL throw then
   CS-TOP {: t:n :}
   t CS-ADT? 0= if E-NELAB-CTRL throw then
   t CS-OFIX@ {: ofix:n :}
   ofix 0 < if E-NELAB-CTRL throw then
   PATH-ENDED? 0= if
      NB @ 1+ NB !
      t CS-JOINED+
   then
   PATH-LIVE PATH-END !
   ofix MEND@ 0= if ofix NB @ JOIN! then
   -1 t CS-OFIX! ;

: SK-ADT-JOIN ( n -- ) {: t:n :}
   t CS-JOINED? if
      t CS-JOIN@ NB @ JOIN!
      PATH-LIVE PATH-END !
   else
      PATH-DEAD PATH-END !
   then
   CS-POP ;

: SK-CLOSE-MATCH ( -- )
   HIR-CTRL:OPEN-MATCH CS-OPENER-CK {: t:n :}
   t CS-OFIX@ 0 >= if E-NELAB-CTRL throw then
   t SK-ADT-JOIN ;

: SK-CLOSE-CASE ( -- )
   HIR-CTRL:OPEN-CASE CS-OPENER-CK {: t:n :}
   t CS-OFIX@ 0 >= if E-NELAB-CTRL throw then
   PATH-ENDED? 0= if
      NB @ 1+ NB !
      t CS-JOINED+
   then
   t SK-ADT-JOIN ;

\ A call the definition really MAKES and control does not come back from.
: SK-DEAD-CALL? ( IR-ARENA:arena n -- bool )
   {: r:IR-ARENA:arena ix:n :}
   r ix HIR-MEANING:CALLABLE MODELED-AS? 0= if false exit then
   r  ix WSYM  HIR-WORD:CALLEE-DEAD? ;

: SK-AFTER-END-CK ( IR-ARENA:arena n -- )
   {: r:IR-ARENA:arena ix:n :}
   r ix HIR-MEANING:CONTROL MODELED-AS? 0= if E-NELAB-CTRL throw then
   r  ix WSYM  HIR-WORD:CTRL@ {: k:HIR:ctrl :}
   k HIR-CTRL:CLOSE-IF HIR-CTRL:EQ if exit then
   k HIR-CTRL:MID-ELSE HIR-CTRL:EQ  PATH-DEAD?  and if exit then
   k HIR-CTRL:CLOSE-ARM HIR-CTRL:EQ  PATH-DEAD?  and if exit then
   k HIR-CTRL:CLOSE-CASE HIR-CTRL:EQ  PATH-DEAD?  and if exit then
   k HIR-CTRL:CLOSE-LOOP HIR-CTRL:EQ if exit then
   k HIR-CTRL:CLOSE-REPEAT HIR-CTRL:EQ if exit then
   k HIR-CTRL:CLOSE-AGAIN HIR-CTRL:EQ if exit then
   E-NELAB-CTRL throw ;

: SK-STEP ( IR-ARENA:arena n -- )
   {: r:IR-ARENA:arena ix:n :}
   VW ix NTAPE-MODE:COMPILING MODE-CK
   ix WALK-SKIP? if exit then
   ix MOPERAND? if exit then
   ix IN-DECL? if exit then
   ix LOCAL-OF 0 >= if exit then
   PATH-ENDED? if r ix SK-AFTER-END-CK then
   r ix SK-DEAD-CALL? if
      NB @ 1+ NB !
      PATH-DEAD PATH-END !
      exit
   then
   r ix ADMIT-AT
   HIR-MEANING:CONTROL HIR-MEANING:EQ 0= if exit then
   r  ix WSYM  HIR-WORD:CTRL@
   MATCH HIR:ctrl
      open-if      OF HIR-CTRL:OPEN-IF ix SK-PUSH  NB @ 2 + NB ! ENDOF
      mid-else     OF ix SK-ELSE ENDOF
      close-if     OF SK-CLOSE-IF ENDOF
      open-begin   OF HIR-CTRL:OPEN-BEGIN ix SK-PUSH  NB @ 1+ NB ! ENDOF
      mid-while    OF SK-WHILE ENDOF
      close-until  OF SK-UNTIL ENDOF
      close-repeat OF SK-REPEAT ENDOF
      close-again  OF SK-AGAIN ENDOF
      open-do      OF HIR-CTRL:OPEN-DO ix SK-PUSH  NB @ 1+ NB ! ENDOF
      open-do-skip OF HIR-CTRL:OPEN-DO ix SK-PUSH
                      CS-TOP CS-JOINED+ NB @ 3 + NB ! ENDOF
      close-loop   OF SK-CLOSE-LOOP ENDOF
      index        OF ENDOF
      outer-index  OF ENDOF
      drop-loop    OF ENDOF
      early-leave  OF SK-LEAVE ENDOF
      early-exit   OF NB @ 1+ NB !  1 EXIT-USED !  PATH-DEAD PATH-END ! ENDOF
      self-call    OF ENDOF
      open-match   OF HIR-CTRL:OPEN-MATCH ix SK-PUSH ENDOF
      match-arm    OF ix SK-ARM ENDOF
      close-arm    OF SK-CLOSE-ARM ENDOF
      close-match  OF SK-CLOSE-MATCH ENDOF
      open-case    OF HIR-CTRL:OPEN-CASE ix SK-PUSH ENDOF
      close-case   OF SK-CLOSE-CASE ENDOF
      make-bundle  OF ENDOF
      open-quot    OF ENDOF
      close-quot   OF ix QUOT-REFUSE ENDOF
      bind-defer   OF ENDOF
      exec         OF ENDOF
      catch        OF ENDOF
      finally      OF ENDOF
      tick         OF ENDOF
      eval         OF ENDOF
   ;MATCH ;

\ A structure left open at the end of the body is a refusal.
: SKELETON ( IR-ARENA:arena n n -- )
   {: r:IR-ARENA:arena lo:n hi:n :}
   hi TMAX @ > if E-NELAB-BLOCK throw then
   0 NB !
   JOIN-RESET
   CS-RESET
   EXIT-RESET
   hi lo ?do
      r i SK-STEP
   loop
   CS-N @ 0<> if E-NELAB-CTRL throw then
   PATH-DEAD? {: dead:bool :}
   EXIT-USED @ 0<> if
      dead if NB @ else NB @ 1+ then EXIT-ORD !
   then
   EXIT-USED @ 0<> if EXIT-ORD @ 1+ else NB @ then
   1+ BLOCK-ROOM
   0 NB !
   PATH-LIVE PATH-END !
   CS-RESET ;

\ ---- what each structured control word builds --------------------------------

: DO-OPEN-IF ( n -- )
   {: ix:n :}
   VN @ 1 < if E-NELAB-UNDER throw then
   NB @ {: c:n :}
   ix JOIN-OF JOIN-CK {: j:n :}
   HIR-CTRL:OPEN-IF  VN @ 1-  j  CS-PUSH
   ix  c 1+  c 2 +  TERM-BRZ
   ix j STUB
   ix OPEN-PLAIN ;

: DO-ELSE ( n -- )
   {: ix:n :}
   HIR-CTRL:OPEN-IF CS-OPENER-CK {: t:n :}
   t CS-ELSE? if E-NELAB-CTRL throw then
   t CS-LB@ LOC-REST
   t CS-DEPTH@ {: d:n :}
   t CS-RD@ {: rd:n :}
   t CS-JOIN@ {: e:n :}
   ix JOIN-OF {: j:n :}
   PATH-ENDED? if
      -1 t CS-END!
      0 t CS-ARM!
      0 t CS-ARMR!
      PATH-LIVE PATH-END !
   else
      j JOIN-CK drop
      VN @ t CS-ARM!
      RN @ t CS-ARMR!
      ix j TERM-BR
   then
   NB @ e <> if E-NELAB-CTRL throw then
   CS-LOOPS @ t CS-LOOP1!
   t CS-LOOP0@ CS-LOOPS !
   j t CS-JOIN!
   ix  d rd +  OPEN-ARGS ;

: DO-JOIN-WIDTH ( n -- n )
   {: t:n :}
   t CS-ELSE? 0= if t CS-DEPTH@ exit then
   t CS-END@ 0<> if VN @ exit then
   t CS-ARM@ ;

: DO-JOIN-RD ( n -- n )
   {: t:n :}
   t CS-ELSE? 0= if t CS-RD@ exit then
   t CS-END@ 0<> if RN @ exit then
   t CS-ARMR@ ;


: DO-JOIN-LOOPS ( n bool -- ) {: t:n ended:bool :}
   t CS-ELSE? if
      t CS-END@ 0<> if exit then
      t CS-LOOP1@
   else t CS-LOOP0@ then
   ended if CS-LOOPS ! else CS-LOOPS= then ;

: DO-CLOSE-IF ( n -- )
   {: ix:n :}
   HIR-CTRL:OPEN-IF CS-OPENER-CK {: t:n :}
   t CS-LB@ LOC-REST
   PATH-ENDED? {: armend:bool :}
   t armend DO-JOIN-LOOPS
   t DO-JOIN-WIDTH {: w:n :}
   t DO-JOIN-RD {: rd:n :}
   t CS-JOIN@ {: j:n :}
   armend if
      PATH-LIVE PATH-END !
   else
      VN @ w <> if E-NELAB-JOIN throw then
      RN @ rd <> if E-NELAB-JOIN throw then
      ix j TERM-BR
   then
   t armend SK-BOTH-ENDED? if
      CS-POP
      PATH-DEAD PATH-END !
      exit
   then
   NB @ j <> if E-NELAB-CTRL throw then
   ix  w rd +  OPEN-ARGS
   CS-POP ;

: DO-OPEN-BEGIN ( n -- )
   {: ix:n :}
   NB @ 1+ {: h:n :}
   VN @ {: d:n :}
   HIR-CTRL:OPEN-BEGIN d h CS-PUSH
   ix JOIN-OF CS-TOP CS-EXIT!
   ix h TERM-BR
   ix  d CS-TOP CS-RD@ +  OPEN-ARGS ;

: DO-WHILE ( n -- )
   {: ix:n :}
   HIR-CTRL:OPEN-BEGIN CS-OPENER-CK {: t:n :}
   t CS-EXIT@ JOIN-CK {: j:n :}
   VN @ 1 < if E-NELAB-UNDER throw then
   t CS-NW@ 0<> if
      VN @ 1- t CS-XD@ <> if E-NELAB-JOIN throw then
      RN @ t CS-XR@ <> if E-NELAB-JOIN throw then
      t CS-LOOP1@ CS-LOOPS=
   then
   CS-LOOPS @ t CS-LOOP1!
   NB @ {: c:n :}
   ix  c 1+  c 2 +  TERM-BRZ
   VN @ t CS-XD!
   RN @ t CS-XR!
   t CS-WHILE+
   t CS-LB@ LOC-BORROW {: had:n :}
   ix j STUB
   had LOC-RETURN
   ix OPEN-PLAIN ;

: DO-CLOSE-REPEAT ( n -- )
   {: ix:n :}
   HIR-CTRL:OPEN-BEGIN CS-OPENER-CK {: t:n :}
   t CS-NW@ 0= if E-NELAB-CTRL throw then
   t CS-LB@ LOC-REST
   t CS-DEPTH@ {: d:n :}
   t CS-RD@ {: rd:n :}
   t CS-JOIN@ {: h:n :}
   t CS-EXIT@ JOIN-CK {: j:n :}
   t CS-XD@ {: xd:n :}
   t CS-XR@ {: xr:n :}
   PATH-ENDED? 0= if
      VN @ d <> if E-NELAB-JOIN throw then
      RN @ rd <> if E-NELAB-JOIN throw then
      ix h TERM-BR
   then
   t CS-LOOP1@ CS-LOOPS !
   PATH-LIVE PATH-END !
   NB @ j <> if E-NELAB-CTRL throw then
   ix  xd xr +  OPEN-ARGS
   CS-POP ;

: DO-CLOSE-UNTIL ( n -- )
   {: ix:n :}
   HIR-CTRL:OPEN-BEGIN CS-OPENER-CK {: t:n :}
   t CS-NW@ 0<> if E-NELAB-CTRL throw then
   t CS-LB@ LOC-REST
   t CS-DEPTH@ {: d:n :}
   t CS-RD@ {: rd:n :}
   t CS-JOIN@ {: h:n :}
   VN @ 1 < if E-NELAB-UNDER throw then
   VN @ 1- d <> if E-NELAB-JOIN throw then
   RN @ rd <> if E-NELAB-JOIN throw then
   NB @ {: c:n :}
   ix  c 1+  c 2 +  TERM-BRZ
   ix h STUB
   ix OPEN-PLAIN
   CS-POP ;

\ The body ends by branching back to the header and nothing opens after it.
: DO-AGAIN ( n -- )
   {: ix:n :}
   HIR-CTRL:OPEN-BEGIN CS-OPENER-CK {: t:n :}
   t CS-NW@ 0<> if E-NELAB-CTRL throw then
   t CS-LB@ LOC-REST
   t CS-DEPTH@ {: d:n :}
   t CS-RD@ {: rd:n :}
   t CS-JOIN@ {: h:n :}
   PATH-ENDED? 0= if
      VN @ d <> if E-NELAB-JOIN throw then
      RN @ rd <> if E-NELAB-JOIN throw then
      ix h TERM-BR
   then
   CS-POP
   PATH-DEAD PATH-END ! ;

: HEAD-CROSS-DO ( -- n n )
   CALL-NEED @ 0<> if 0 DO-OPEN-N exit then
   DO-OPEN-N 1- 1 ;

\ The start is on top and the limit under it, where the source left them.
: DO-PAIR ( -- IR-ID:ir-value-id IR-ID:ir-value-id )
   VN @ 2 < if E-NELAB-UNDER throw then
   VN @ 1- VAT {: st:IR-ID:ir-value-id :}
   VN @ 2 - VAT {: lm:IR-ID:ir-value-id :}
   st REAL-VALUE? if E-NELAB-TYPE throw then
   lm REAL-VALUE? if E-NELAB-TYPE throw then
   st lm ;

: DO-ENTER ( IR-ID:ir-value-id IR-ID:ir-value-id n n n n -- )
   {: st:IR-ID:ir-value-id lm:IR-ID:ir-value-id ix:n h:n d:n j:n :}
   HIR-CTRL:OPEN-DO d j CS-PUSH
   CS-TOP CS-LOOP-BIT CS-LOOPS @ or CS-LOOPS !
   h CS-TOP cells CS-HEAD + !
   st CS-TOP CS-IDX !
   lm CS-TOP CS-LIM !
   ix h  HEAD-CROSS-DO CROSS-L  TERM-BR-H
   ix  d CS-TOP CS-RD@ +  HEAD-CROSS-DO CROSS-L  OPEN-ARGS-H ;

: DO-OPEN-DO ( n -- )
   {: ix:n :}
   DO-PAIR {: st:IR-ID:ir-value-id lm:IR-ID:ir-value-id :}
   2 VDROP
   ix JOIN-OF {: j:n :}
   st lm  ix  NB @ 1+  VN @  j  DO-ENTER ;

: DO-OPEN-DO-SKIP ( n -- )
   {: ix:n :}
   DO-PAIR {: st:IR-ID:ir-value-id lm:IR-ID:ir-value-id :}
   ix HIR-OPCODE:SUB EMIT-OPCODE
   VN @ 1- {: d:n :}
   NB @ {: c:n :}
   ix JOIN-OF JOIN-CK {: j:n :}
   ix  c 1+  c 2 +  TERM-BRZ
   ix j STUB
   ix OPEN-PLAIN
   st lm  ix  c 3 +  d  j  DO-ENTER ;

: EXIT-CROSS-DO ( -- n n )
   CALL-NEED @ 0= if 0 0 exit then
   0 DO-OPEN-N 1- ;


: DO-DEAD-LOOP ( n n -- ) {: ix:n t:n :}
   t CS-JOIN@ {: j:n :}
   t CS-DEPTH@ t CS-RD@ + {: n:n :}
   t CS-LOOP0@ CS-LOOPS !
   CS-POP
   j 0 < if exit then
   NB @ j <> if E-NELAB-CTRL throw then
   ix n OPEN-ARGS
   PATH-LIVE PATH-END ! ;

: DO-CLOSE-LOOP ( n -- )
   {: ix:n :}
   HIR-CTRL:OPEN-DO CS-OPENER-CK {: t:n :}
   t CS-LB@ LOC-REST
   PATH-ENDED? if ix t DO-DEAD-LOOP exit then
   t CS-DEPTH@ {: d:n :}
   t CS-RD@ {: rd:n :}
   t CS-JOIN@ {: j:n :}
   t CS-HEAD@ {: h:n :}
   t CS-IDX @ {: iv:IR-ID:ir-value-id :}
   t CS-LIM @ {: lv:IR-ID:ir-value-id :}
   VN @ d <> if E-NELAB-JOIN throw then
   RN @ rd <> if E-NELAB-JOIN throw then
   iv VPUSH
   ix 1 EMIT-LIT
   ix HIR-OPCODE:ADD EMIT-OPCODE
   VN @ 1- VAT {: nx:IR-ID:ir-value-id :}
   lv VPUSH
   ix HIR-OPCODE:LT EMIT-OPCODE
   NB @ {: c:n :}
   ix  c 1+  c 2 +  TERM-BRZ
   ix j  EXIT-CROSS-DO CROSS-L  STUB-H
   ix OPEN-PLAIN
   nx t CS-IDX !
   ix h  HEAD-CROSS-DO CROSS-L  TERM-BR-H
   t CS-LOOP0@ CS-LOOPS !
   CS-POP
   ix  d rd +  OPEN-ARGS
   NB @ j <> if E-NELAB-CTRL throw then ;

: DO-FRAME ( -- n )
   0 DO-INNER-NTH ;

: DO-INDEX ( -- )
   0 DO-ACTIVE-NTH CS-IDX @ VPUSH ;

: DO-OUTER-INDEX ( -- )
   1 DO-ACTIVE-NTH CS-IDX @ VPUSH ;

\ `leave` branches out of the INNERMOST counted loop, to the block that loop's
\ own exit stub already branches to.
: DO-LEAVE ( n -- )
   {: ix:n :}
   DO-FRAME {: t:n :}
   t CS-DEPTH@ {: d:n :}
   t CS-RD@ {: rd:n :}
   t CS-JOIN@ JOIN-CK {: j:n :}
   VN @ d <> if E-NELAB-JOIN throw then
   RN @ rd <> if E-NELAB-JOIN throw then
   t CS-LB@ LOC-BORROW {: had:n :}
   ix j  EXIT-CROSS-DO CROSS-L  TERM-BR-H
   had LOC-RETURN
   PATH-DEAD PATH-END ! ;

\ ---- the three tag-dispatch forms --------------------------------------------

\ A bundle's cells are one value, so a form that would split them is refused.
: BUNDLE-CK ( n n n -- ) {: base:n w:n code:n :}
   base VGLUE-BIT? if code throw then
   w 1 ?do
      base i + VGLUE-BIT? 0= if code throw then
   loop
   base w + VN @ >= if exit then
   base w + VGLUE-BIT? if code throw then ;

: DO-OPEN-MATCH ( n -- ) {: ix:n :}
   ix MWID@ {: w:n :}
   w VN @ > if E-NELAB-UNDER throw then
   VN @ w - {: base:n :}
   base w E-NELAB-MATCH BUNDLE-CK
   HIR-CTRL:OPEN-MATCH base  ix JOIN-OF  CS-PUSH
   w CS-TOP CS-W!
   ix MTAG@ CS-TOP CS-TRAP! ;

: DO-OPEN-CASE ( n -- ) {: ix:n :}
   VN @ 1 < if E-NELAB-UNDER throw then
   VN @ 1- {: base:n :}
   base 1 E-NELAB-MATCH BUNDLE-CK
   HIR-CTRL:OPEN-CASE base  ix JOIN-OF  CS-PUSH
   1 CS-TOP CS-W! ;

: ARM-WIDTH-CK ( n -- ) {: t:n :}
   t CS-MATCH? if t CS-DEPTH@ t CS-W@ + else t CS-DEPTH@ 2 + then
   VN @ <> if E-NELAB-JOIN throw then
   RN @ t CS-RD@ <> if E-NELAB-JOIN throw then ;

: ARM-FLAG ( n n -- ) {: ix:n t:n :}
   t CS-MATCH? if
      VN @ 1- VAT VPUSH
      ix  ix MTAG@  EMIT-LIT
   else
      t CS-DEPTH@ VAT VPUSH
   then
   ix HIR-OPCODE:EQUAL EMIT-OPCODE ;

\ Drop the tag and pads, then restore the instantiated payload's value groups.
: ARM-RESHAPE ( n n -- ) {: ix:n t:n :}
   t CS-MATCH? 0= if 1 VDROP exit then
   ix MPAD@ 1+ {: d:n :}
   d VN @ > if E-NELAB-UNDER throw then
   d VDROP
   t CS-DEPTH@ {: base:n :}
   base VN @ base - VGLUE-CLEAR
   base ix MGLUE@ VGLUE-RUN ;

\ Copy the diagnostic through the same owned literal path as source strings;
\ no emitted trap depends on the build host's ordinal table or helper code.
: TRAP-ARGS ( n n -- ) {: ix:n ord:n :}
   ord NTRAP:MESSAGE {: a:ptr u:n rc:n :}
   ix a u NSTR:INTERN HIR:ADDR-DATA STAGE-LIT
   ix u HIR:ADDR-NONE STAGE-LIT
   ix rc HIR:ADDR-NONE STAGE-LIT ;

\ The block a tag that matched no variant runs into, which is the last arm's
\ mismatch edge.
: MATCH-TRAP ( n n -- ) {: ix:n ord:n :}
   LIT-MARK {: m:n :}
   ix OPEN-PLAIN
   ix ord TRAP-ARGS
   ix HIR-OPCODE:TRAP EMIT-OPCODE
   CLOSE-BLOCK
   m LIT-RELEASE ;

: DO-ARM ( n -- ) {: ix:n :}
   CS-N @ 1 < if E-NELAB-CTRL throw then
   CS-TOP {: t:n :}
   t CS-ADT? 0= if E-NELAB-CTRL throw then
   t CS-OFIX@ 0 >= if E-NELAB-CTRL throw then
   t ARM-WIDTH-CK
   CS-LOOPS @ t cells CS-LOOP0 + !
   ix t ARM-FLAG
   NB @ {: c:n :}
   ix  c 1+  c 2 +  TERM-BRZ
   ix MEND@ if ix t CS-TRAP@ MATCH-TRAP else ix  ix JOIN-OF JOIN-CK  STUB then
   ix OPEN-PLAIN
   ix t ARM-RESHAPE
   LBN @ t CS-LARM!
   ix t CS-OFIX! ;

: DO-CLOSE-ARM ( n -- ) {: ix:n :}
   CS-N @ 1 < if E-NELAB-CTRL throw then
   CS-TOP {: t:n :}
   t CS-ADT? 0= if E-NELAB-CTRL throw then
   t CS-OFIX@ {: ofix:n :}
   ofix 0 < if E-NELAB-CTRL throw then
   t CS-LARM@ {: lm:n :}
   lm 0 < if E-NELAB-CTRL throw then
   lm LOC-REST
   PATH-ENDED? if
      PATH-LIVE PATH-END !
   else
      ix  t CS-JOIN@ JOIN-CK  TERM-BR
      CS-LOOPS @ t CS-LOOP1!
      t CS-JOINED+
   then
   t CS-LOOP0@ CS-LOOPS !
   -1 t CS-OFIX!
   -1 t CS-LARM!
   ofix MEND@ if exit then
   ofix JOIN-OF JOIN-CK {: nx:n :}
   NB @ nx <> if E-NELAB-CTRL throw then
   ix  t CS-DEPTH@ t CS-W@ +  t CS-RD@ +  OPEN-ARGS ;

: ADT-JOIN ( n n -- ) {: ix:n t:n :}
   t CS-JOIN@ {: j:n :}
   t CS-JOINED? 0= if
      j 0 >= if E-NELAB-CTRL throw then
      PATH-DEAD PATH-END !
      exit
   then
   j JOIN-CK drop
   t CS-LOOP1@ CS-LOOPS !
   NB @ j <> if E-NELAB-CTRL throw then
   NB @ ARG-STATED? 0= if E-NELAB-JOIN throw then
   ix  NB @ ARG-WIDTH@  OPEN-ARGS
   PATH-LIVE PATH-END ! ;

: DO-CLOSE-MATCH ( n -- ) {: ix:n :}
   HIR-CTRL:OPEN-MATCH CS-OPENER-CK {: t:n :}
   t CS-OFIX@ 0 >= if E-NELAB-CTRL throw then
   t CS-LB@ LOC-REST
   ix t ADT-JOIN
   CS-POP ;

: DO-CLOSE-CASE ( n -- ) {: ix:n :}
   HIR-CTRL:OPEN-CASE CS-OPENER-CK {: t:n :}
   t CS-OFIX@ 0 >= if E-NELAB-CTRL throw then
   t CS-LB@ LOC-REST
   PATH-ENDED? if
      PATH-LIVE PATH-END !
   else
      \ The default may consume entry values; only its current selector is dropped.
      1 VDROP
      ix  t CS-JOIN@ JOIN-CK  TERM-BR
      CS-LOOPS @ t CS-LOOP1!
      t CS-JOINED+
   then
   ix t ADT-JOIN
   CS-POP ;

: DO-MAKE-BUNDLE ( n -- ) {: ix:n :}
   ix MPAY@ {: k:n :}
   k VN @ > if E-NELAB-UNDER throw then
   VN @ k - {: base:n :}
   ix MPAD@ 0 ?do
      ix 0 EMIT-LIT
   loop
   ix  ix MTAG@  EMIT-LIT
   base  VN @ base -  VGLUE-GROW ;

\ What the operation is handed is the whole live vector, because no register
\ survives a call.
: CALL-LIVE ( n n -- n )
   {: a:n r:n :}
   VN @ a < if E-NELAB-CALL throw then
   TOK-LIVE @ 0= if E-NELAB-CALL throw then
   VN @ a - {: k:n :}
   k r + VMAX > if E-NELAB-CALL throw then
   k ;

: CALL-CROSS-CK ( -- )
   CALL-NEED @ 0= if E-NELAB-CALL throw then ;

\ Only data-stack values use the callee's stack window. Parked values, locals
\ and loop counters stay live in SSA; allocation spills them across the call.
\ A callback may replace an independent row, so hidden values cannot be
\ recovered at fixed offsets below its outputs.
: CALL-CROSS ( n -- )
   VN @ CELL-CROSS ;

: CALL-OPERANDS+ ( -- )
   CALL-CROSS-CK
   NO-REAL-CK
   CTX BLD TOK IR-BUILD:ADD-OPERAND
   VN @ 0 ?do
      CTX BLD  i VAT  IR-BUILD:ADD-OPERAND
   loop ;

: CALL-RESULTS+ ( n -- )
   {: n:n :}
   CTX BLD  CTX BLD HIR:MEM-TYPE  IR-BUILD:ADD-RESULT
   n 0 ?do
      CTX BLD  CTX BLD CELL-TYPE  IR-BUILD:ADD-RESULT
   loop ;

: CALL-KEPT ( n n n -- n ) {: keep:n k:n had:n :}
   keep  had VGLUE-LOW  k 1+ VGLUE-LOW ;

: CALL-CLOSE ( n n n -- )
   {: n:n out:n oglue:n :}
   VGLUE @ {: keep:n :}
   VN @ {: had:n :}
   VQ-SAVE
   CTX BLD IR-BUILD:END-OP {: id:IR-ID:ir-op-id :}
   VN @ VDROP
   CTX BLD id 0 IR-BUILD:OP-RESULT@ TOK!
   n 0 ?do
      CTX BLD id  i 1+  IR-BUILD:OP-RESULT@ VPUSH
   loop
   n out - {: k:n :}
   k VQ-KEEP
   0  keep k had CALL-KEPT  VGLUE-RUN
   k oglue VGLUE-RUN ;

\ ---- a call control does not come back from ----------------------------------
128 constant DN-CAP                  \ bytes of a callee's spelling this asks about
here CELL 1- and CELL swap - CELL 1- and allot
create DN-BUF DN-CAP allot

: DEAD-ORD ( IR-ID:ir-symbol-id -- n )
   {: sy:IR-ID:ir-symbol-id :}
   CTX BLD sy DN-BUF DN-CAP IR-BUILD:SYMBOL-COPY {: u:n :}
   DN-BUF u NTRAP:NO-RETURN ;

\ The ordinal is staged FRESH and not taken off the literal memo.
: DEAD-END ( IR-ARENA:arena n -- )
   {: r:IR-ARENA:arena ix:n :}
   ix  ix WSYM DEAD-ORD TRAP-ARGS
   ix HIR-OPCODE:TRAP EMIT-OPCODE
   CLOSE-BLOCK
   PATH-DEAD PATH-END ! ;

\ A self-call empties the memo without asking.
: DO-SELF-CALL ( n -- )
   {: ix:n :}
   IN-N @ OUT-N @ CALL-LIVE  OUT-N @ + {: back:n :}
   ix CALL-CROSS
   CTX BLD HIR-OPCODE:CALL HIR:ENSURE-OP {: op:IR-ID:ir-symbol-id :}
   CTX BLD VW MKEY ix op OPEN
   CALL-OPERANDS+
   back CALL-RESULTS+
   back OUT-N @ OUT-GLUE @ CALL-CLOSE
   LIT-RESET ;

: WCALL-ATTRS+ ( n n n -- )
   {: entry:n in:n out:n :}
   CTX BLD  CTX BLD HIR:KEY-ENTRY
   CTX BLD entry IR-BUILD:INTERN-INT-ATTR  IR-BUILD:ADD-ATTR
   CTX BLD  CTX BLD HIR:KEY-IN
   CTX BLD in IR-BUILD:INTERN-INT-ATTR  IR-BUILD:ADD-ATTR
   CTX BLD  CTX BLD HIR:KEY-OUT
   CTX BLD out IR-BUILD:INTERN-INT-ATTR  IR-BUILD:ADD-ATTR ;

: STAGE-WCALL ( n n n n n -- )
   {: ix:n entry:n a:n o:n oglue:n :}
   a o CALL-LIVE o + {: back:n :}
   ix CALL-CROSS
   CTX BLD HIR-OPCODE:WORDCALL HIR:ENSURE-OP {: op:IR-ID:ir-symbol-id :}
   CTX BLD VW MKEY ix op OPEN
   CALL-OPERANDS+
   back CALL-RESULTS+
   entry a o WCALL-ATTRS+
   back o oglue CALL-CLOSE
   LIT-RESET ;

: DO-WORD-CALL ( IR-ARENA:arena n -- )
   {: r:IR-ARENA:arena ix:n :}
   ix WSYM {: sy:IR-ID:ir-symbol-id :}
   ix NDICT:CALL-CELLS {: a:n o:n :}
   a 0 < if
      r sy HIR-WORD:CALLEE-IN@ r sy HIR-WORD:CALLEE-OUT@
      r sy HIR-WORD:OUT-GLUE@
   else
      a o ix NDICT:CALL-GLUE nip
   then {: in:n out:n glue:n :}
   ix  r sy HIR-WORD:ENTRY@
   in out glue STAGE-WCALL
   ix out QRESULTS-FILL
   r sy HIR-WORD:CALLEE-DEAD? if r ix DEAD-END then ;

\ Source evaluation has a dynamic stack effect. Only an explicit TRUSTED:
\ boundary can assert its result row; checked callers still reject evaluate.
\ Publish this boundary's live row and receive its declared results through the
\ ordinary native call path, including its register saves and relocations.
: DO-EVAL ( n -- ) {: ix:n :}
   data-base TRUSTED-CELL + @ 0= if E-HIR-UNMODELED throw then
   VN @ 2 < if E-NELAB-UNDER throw then
   QCUR @ QOWNER-DEF = if
      OUT-N @ OUT-GLUE @
   else
      QCUR @ QOUT@ NDICT:GLUE-NONE
   then {: out:n glue:n :}
   out 0 < if ix QUOT-REFUSE then
   s" evaluate" NDICT:CALL-TARGET {: entry:n :}
   entry 0= if E-HIR-UNMODELED throw then
   ix entry VN @ out glue STAGE-WCALL ;

\ ---- what `[:` stages ---------------------------------------------------------
: DO-QUOT ( n -- )
   {: ix:n :}
   ix QOPENED@ {: k:n :}
   k 0 < if ix QUOT-REFUSE then
   CTX BLD HIR-OPCODE:QUOT HIR:ENSURE-OP {: op:IR-ID:ir-symbol-id :}
   CTX BLD VW MKEY ix op OPEN
   CTX BLD op RESULTS+
   CTX BLD  CTX BLD HIR:KEY-FUN
   CTX BLD  k QFUN@  IR-BUILD:INTERN-INT-ATTR  IR-BUILD:ADD-ATTR
   CTX BLD op CLOSE
   k  VN @ 1-  VQ! ;

\ A tick carries a real code address, so snapshot/AOT relocation records it.
\ If the target has a known effect, preserve its quotation calling convention.
: DO-TICK ( n -- ) {: ix:n :}
   ix 1+ QSPELL {: a:ptr u:n :}
   a u NDICT:CALL-TARGET {: entry:n :}
   entry 0= if ix QUOT-REFUSE then
   ix entry HIR:ADDR-CODE EMIT-KIND-LIT
   a u NDICT:SPELL-ARITY {: qi:n qo:n :}
   qi NDICT:ARITY-NONE = if exit then
   qo NDICT:ARITY-NONE = if exit then
   ix qi qo VN @ 1- QKNOWN ;

\ ---- binding a quotation to a deferred word -----------------------------------
: DO-IS ( n -- )
   {: ix:n :}
   VN @ 1 < if E-NELAB-UNDER throw then
   ix 1+ QSPELL {: a u:n :}
   a u NDICT:SPELL-DEFER-CELL {: cell:n :}
   cell 0= if E-NELAB-DEFER throw then
   a u NDICT:SPELL-ARITY {: din:n dout:n :}
   din NDICT:ARITY-NONE = if E-NELAB-DEFER throw then
   dout NDICT:ARITY-NONE = if E-NELAB-DEFER throw then
   s" xt!" NDICT:CALL-TARGET {: entry:n :}
   entry 0= if E-NELAB-DEFER throw then
   VN @ 1- VQ@ {: k:n :}
   k 0 >= if k din dout QFILL then
   ix cell HIR:ADDR-DATA EMIT-KIND-LIT
   ix entry 2 0 NDICT:GLUE-NONE STAGE-WCALL ;

\ ---- entering the routine a quotation names ----------------------------------
: DO-EXEC ( IR-ARENA:arena n -- )
   {: r:IR-ARENA:arena ix:n :}
   VN @ 1 < if E-NELAB-UNDER throw then
   VN @ 1- VQ@ {: k:n :}
   ix NDICT:EXEC-CELLS {: cin:n cout:n :}
   cin 0 < if
      k 0 < if ix QUOT-REFUSE then
      k QIN@ k QOUT@
   else cin cout then {: in:n out:n :}
   in 0 < if ix QUOT-REFUSE then
   k 0 >= if
      k QIN@ QNONE = if k in out 0 max QFILL then
   then
   s" execute" NDICT:CALL-TARGET {: entry:n :}
   entry 0= if ix QUOT-REFUSE then
   ix NDICT:CALL-CELLS drop 0 < if NDICT:GLUE-NONE
   else ix NDICT:CALL-GLUE nip then {: glue:n :}
   glue NDICT:GLUE-UNKNOWN = if E-NELAB-BUNDLE throw then
   ix entry  in 1+  out 0 max  glue  STAGE-WCALL
   in 0 >= out NDICT:ARITY-NONE = and if r ix DEAD-END then ;

\ ---- running a quotation and coming back either way --------------------------
\ `catch` stages ONE call and opens no scope of its own.
: DO-CATCH ( n -- )
   {: ix:n :}
   VN @ 1 < if E-NELAB-UNDER throw then
   VN @ 1- VQ@ {: k:n :}
   ix NDICT:CATCH-CELLS {: win:n back:n :}
   win NDICT:CATCH-NONE = if ix QUOT-REFUSE then
   back NDICT:CATCH-NONE <> if back win <> if ix QUOT-REFUSE then then
   VN @ 1- win < if E-NELAB-UNDER throw then
   s" catch" NDICT:CALL-TARGET {: entry:n :}
   entry 0= if ix QUOT-REFUSE then
   k 0 >= if k win win QFILL then
   \ RSCATCH requires the returned row to match the input row. Keep that
   \ window's value boundaries; the appended result code is a separate cell.
   VGLUE @ VN @ 1- win - rshift win VGLUE-LOW {: glue:n :}
   ix entry  win 1+  win 1+  glue  STAGE-WCALL ;


\ Both quotations are native calls. The cleanup has a certified empty window;
\ the body window and its outputs come from the same per-site checker table.
: DO-FINALLY ( IR-ARENA:arena n -- ) {: r:IR-ARENA:arena ix:n :}
   VN @ 2 < if E-NELAB-UNDER throw then
   ix NDICT:FINALLY-CELLS {: in:n out:n cleanup-out:n :}
   in 0 < if ix QUOT-REFUSE then
   VN @ 2 - VQ@ {: body:n :}
   VN @ 1- VQ@ {: cleanup:n :}
   body 0 >= if body in out 0 max QFILL then
   cleanup 0 >= if cleanup 0 0 QFILL then
   s" finally" NDICT:CALL-TARGET {: entry:n :}
   entry 0= if ix QUOT-REFUSE then
   \ Returning products and wide families need the checker's value boundaries.
   \ Dead outputs need no grouping. Trusted bodies can carry only quotation
   \ arity, without ordinary call rows; handle those exactly as execute does.
   out 0 < ix NDICT:CALL-CELLS drop 0 < or if NDICT:GLUE-NONE
   else ix NDICT:CALL-GLUE nip then {: glue:n :}
   glue NDICT:GLUE-UNKNOWN = if E-NELAB-BUNDLE throw then
   ix entry in 2 + out 0 max glue STAGE-WCALL
   out 0 < cleanup-out 0 < or if r ix DEAD-END then ;

\ ---- a call that BUILDS a value of a wide instantiation ------------------------
: CON-PADS-PUSH ( n n -- ) {: ix:n x:n :}
   x 0 ?do  ix 0 EMIT-LIT  loop ;

\ What came back is ONE value: the call answered its DECLARED bundle.
: CON-BUNDLE-GLUE ( n -- ) {: w:n :}
   w VN @ > if E-NELAB-UNDER throw then
   VN @ w -  w  VGLUE-GROW ;

: DO-CALL ( IR-ARENA:arena n -- )
   {: r:IR-ARENA:arena ix:n :}
   ix NDICT:CALL-CELLS drop {: in:n :}
   in 0 < if r ix WSYM HIR-WORD:CALLEE-IN@ else in then
   ix swap QCALL-FILL
   ix NDICT:CON-PADS {: x:n :}
   ix x CON-PADS-PUSH
   r ix DO-WORD-CALL
   x 0= if exit then
   r  ix WSYM  HIR-WORD:CALLEE-OUT@  x +  CON-BUNDLE-GLUE ;

\ Source stores use the engine's protected-span boundary. A raw HIR store
\ alone cannot enforce the same sealed-memory rule as interpreted ! and c!.
: DO-STORE ( n -- ) {: ix:n :}
   ix QSPELL NDICT:CALL-TARGET {: entry:n :}
   entry 0= if E-HIR-UNMODELED throw then
   ix entry 2 0 NDICT:GLUE-NONE STAGE-WCALL ;

\ ---- moving a whole value between memory and the vector ----------------------
: WIDE-ADDR ( n IR-ID:ir-value-id n -- )
   {: ix:n base:IR-ID:ir-value-id k:n :}
   base VPUSH
   k 0= if exit then
   ix k cells EMIT-LIT
   ix HIR-OPCODE:ADD EMIT-OPCODE ;

\ `@` at width W: the address comes off the vector and W cells go back on.
: WIDE-LOAD ( n n -- ) {: ix:n w:n :}
   VN @ 1 < if E-NELAB-UNDER throw then
   VN @ 1- VAT {: base:IR-ID:ir-value-id :}
   1 VDROP
   VN @ {: bot:n :}
   w 0 ?do
      ix base i WIDE-ADDR
      ix HIR-OPCODE:LOAD EMIT-OPCODE
   loop
   VN @ bot - w <> if E-NELAB-BUNDLE throw then
   bot w VGLUE-GROW ;

: WIDE-STORE ( n n -- ) {: ix:n w:n :}
   VN @ w 1+ < if E-NELAB-UNDER throw then
   VN @ w 1+ - {: bot:n :}
   bot w E-NELAB-BUNDLE BUNDLE-CK
   \ Keep the address on the vector so every guarded call preserves it.
   w 0 ?do
      bot i + VAT VPUSH
      ix bot w + VAT i WIDE-ADDR
      ix DO-STORE
   loop
   VN @ bot w + 1+ <> if E-NELAB-BUNDLE throw then
   w 1+ VDROP ;

: DO-QUOTATION-STORE ( n -- ) {: ix:n :}
   s" QUOTATION-STORAGE:STORE" NDICT:CALL-TARGET {: entry:n :}
   entry 0= if E-HIR-UNMODELED throw then
   ix entry 2 0 NDICT:GLUE-NONE STAGE-WCALL ;

: DO-OP ( IR-ARENA:arena n -- )
   {: r:IR-ARENA:arena ix:n :}
   ix NDICT:CALL-CELLS drop {: a:n :}
   a 0 >= if ix a QCALL-FILL then
   VW ix TOK-CELLS {: w:n :}
   w 1 = if
      r ix QUOTATION-STORE? if ix DO-QUOTATION-STORE exit then
      r ix WSYM HIR-WORD:OPCODE@ {: k:HIR:opcode :}
      k GUARDED-STORE? if ix DO-STORE else r ix EMIT-OP then
      exit
   then
   w 1 < if E-NELAB-BUNDLE throw then
   r  ix WSYM  HIR-WORD:OPCODE@ {: k:HIR:opcode :}
   k HIR-OPCODE:LOAD HIR-OPCODE:EQ if ix w WIDE-LOAD exit then
   k HIR-OPCODE:STORE HIR-OPCODE:EQ if ix w WIDE-STORE exit then
   E-NELAB-BUNDLE throw ;

: DO-UNLOOP ( -- )
   0 DO-ACTIVE-NTH CS-LOOP-BIT invert CS-LOOPS @ and CS-LOOPS ! ;

\ EXIT returns from the current function, including a quotation sibling.
: DO-EXIT ( n -- )
   {: ix:n :}
   QCUR @ QOWNER-DEF = if OUT-N @ else QCUR @ QOUT@ then
   VN @ <> if E-NELAB-ARITY throw then
   EXIT-ORD @ 0 < if E-NELAB-CTRL throw then
   ix EXIT-ORD @ 0 0 0 TERM-BR-H
   PATH-DEAD PATH-END ! ;

\ ---- binding and reading the locals ------------------------------------------
: BIND-CROSS-ONE ( n n -- )
   {: ix:n s:n :}
   s LSX@ 0= if exit then
   s LVAL @ REAL-VALUE? 0= if exit then
   ix  s LVAL @  HIR-OPCODE:REALBITS CROSS-VALUE  s LVAL ! ;

: LOCAL-BIND-CROSS ( n n n -- )
   {: ix:n from:n k:n :}
   k 0 ?do
      ix  from i +  BIND-CROSS-ONE
   loop ;

\ The value, the quotation row it names, and which declaration owns the slot.
: BIND-ONE ( n n n -- )
   {: base:n d:n s:n :}
   base VAT  s LVAL !
   base VQ@  s LQ!
   d s LOWN!
   d LCROSS?  s LSX! ;


\ Lexical slots count names; the live vector holds every cell of each name.
: BIND-VALUE ( n n n -- n n )
   {: base:n d:n s:n :}
   base 1+
   begin dup VN @ < if dup VGLUE-BIT? else false then while 1+ repeat
   base - {: w:n :}
   s d cells LBASE + !
   w d cells LWIDTH + !
   w 0 ?do  base i + d s i + BIND-ONE  loop
   base w + s w + ;


: DO-CLOSE-LOCALS ( n -- )
   {: ix:n :}
   LGB @ LG-N @ >= if E-NELAB-LOCAL throw then
   ix LGB @ LG-B@ <> if E-NELAB-LOCAL throw then
   LGB @ LG-K@ {: k:n :}
   LGB @ LG-F@ {: d0:n :}
   LBN @ {: from:n :}
   k VROWS-CELLS {: w:n :}
   w 0 < if E-NELAB-UNDER throw then
   from w + LOCAL-VALUES-ROOM
   VN @ w - from
   k 0 ?do
      d0 i + swap BIND-VALUE
   loop
   2drop
   w VDROP
   ix from w LOCAL-BIND-CROSS
   from w + LBN !
   LGB @ 1+ LGB ! ;

: LOCAL-READ? ( n -- bool )
   {: ix:n :}
   ix LOCAL-OF {: k:n :}
   k 0 < if false exit then
   k cells LBASE + @ {: s:n :}
   k cells LWIDTH + @ {: w:n :}
   s 0 <  s w + LBN @ >  or if E-NELAB-LOCAL throw then
   VN @ {: base:n :}
   w 0 ?do
      s i + {: at:n :}
      at LOWN@ k <> if E-NELAB-LOCAL throw then
      at LSAT LVAL @ VPUSH
      at LQ@ VN @ 1- VQ!
   loop
   base w VGLUE-GROW
   true ;

: DO-CONTROL ( IR-ARENA:arena n -- )
   {: r:IR-ARENA:arena ix:n :}
   r  ix WSYM  HIR-WORD:CTRL@
   MATCH HIR:ctrl
      open-if      OF ix DO-OPEN-IF ENDOF
      mid-else     OF ix DO-ELSE ENDOF
      close-if     OF ix DO-CLOSE-IF ENDOF
      open-begin   OF ix DO-OPEN-BEGIN ENDOF
      mid-while    OF ix DO-WHILE ENDOF
      close-until  OF ix DO-CLOSE-UNTIL ENDOF
      close-repeat OF ix DO-CLOSE-REPEAT ENDOF
      close-again  OF ix DO-AGAIN ENDOF
      open-do      OF ix DO-OPEN-DO ENDOF
      open-do-skip OF ix DO-OPEN-DO-SKIP ENDOF
      close-loop   OF ix DO-CLOSE-LOOP ENDOF
      index        OF DO-INDEX ENDOF
      outer-index  OF DO-OUTER-INDEX ENDOF
      drop-loop    OF DO-UNLOOP ENDOF
      early-leave  OF ix DO-LEAVE ENDOF
      early-exit   OF ix DO-EXIT ENDOF
      self-call    OF ix DO-SELF-CALL ENDOF
      open-match   OF ix DO-OPEN-MATCH ENDOF
      match-arm    OF ix DO-ARM ENDOF
      close-arm    OF ix DO-CLOSE-ARM ENDOF
      close-match  OF ix DO-CLOSE-MATCH ENDOF
      open-case    OF ix DO-OPEN-CASE ENDOF
      close-case   OF ix DO-CLOSE-CASE ENDOF
      make-bundle  OF ix DO-MAKE-BUNDLE ENDOF
      open-quot    OF ix DO-QUOT ENDOF
      close-quot   OF ix QUOT-REFUSE ENDOF
      bind-defer   OF ix DO-IS ENDOF
      exec         OF r ix DO-EXEC ENDOF
      catch        OF ix DO-CATCH ENDOF
      finally      OF r ix DO-FINALLY ENDOF
      tick         OF ix DO-TICK ENDOF
      eval         OF ix DO-EVAL ENDOF
   ;MATCH ;

\ ---- the walk ----------------------------------------------------------------
variable IX                          \ the body token the walk stands on

: AFTER-END-CK ( IR-ARENA:arena n -- )
   {: r:IR-ARENA:arena ix:n :}
   r ix HIR-MEANING:CONTROL MODELED-AS? 0= if E-NELAB-CTRL throw then
   r  ix WSYM  HIR-WORD:CTRL@ {: k:HIR:ctrl :}
   k HIR-CTRL:CLOSE-IF HIR-CTRL:EQ if exit then
   k HIR-CTRL:MID-ELSE HIR-CTRL:EQ  PATH-DEAD?  and if exit then
   k HIR-CTRL:CLOSE-ARM HIR-CTRL:EQ  PATH-DEAD?  and if exit then
   k HIR-CTRL:CLOSE-CASE HIR-CTRL:EQ  PATH-DEAD?  and if exit then
   k HIR-CTRL:CLOSE-LOOP HIR-CTRL:EQ if exit then
   k HIR-CTRL:CLOSE-REPEAT HIR-CTRL:EQ if exit then
   k HIR-CTRL:CLOSE-AGAIN HIR-CTRL:EQ if exit then
   E-NELAB-CTRL throw ;

: STEP ( IR-ARENA:arena IR-ARENA:arena n -- )
   {: p:IR-ARENA:arena r:IR-ARENA:arena ix:n :}
   VW ix NTAPE-MODE:COMPILING MODE-CK
   ix WALK-SKIP? if exit then
   ix MOPERAND? if exit then
   PATH-ENDED? if r ix AFTER-END-CK then
   ix IN-DECL? if exit then
   ix LOCAL-READ? if exit then
   r ix ADMIT-AT
   MATCH HIR:meaning
      literal      OF ix EMIT-CONST ENDOF
      real-literal OF ix EMIT-FCONST ENDOF
      string-literal OF ix EMIT-STRING ENDOF
      op           OF r ix DO-OP ENDOF
      const-op     OF r ix EMIT-CONST-OP ENDOF
      fixed        OF r ix EMIT-FIXED ENDOF
      callable     OF r ix DO-CALL ENDOF
      control      OF r ix DO-CONTROL ENDOF
      rename       OF p r  ix WSYM  RENAME ENDOF
      rstack       OF r  ix WSYM  RSTACK-STEP ENDOF
      open-locals  OF E-NELAB-LOCAL throw ENDOF
      close-locals OF ix DO-CLOSE-LOCALS ENDOF
      unmodeled    OF E-HIR-UNMODELED throw ENDOF
   ;MATCH ;

\ Every row after the name, to the end of the tape.
: WALK ( IR-ARENA:arena IR-ARENA:arena n n -- )
   {: p:IR-ARENA:arena r:IR-ARENA:arena lo:n hi:n :}
   lo IX !
   begin
      IX @ hi <
   while
      p r IX @ STEP
      IX @ 1+ IX !
   repeat ;

\ ---- the two walks of the body, with the record around them ------------------
: SK-KEEP ( IR-ARENA:arena n n -- IR-ARENA:arena n n )
   {: r:IR-ARENA:arena lo:n hi:n :}
   r lo hi SKELETON
   r lo hi ;

: SKELETON-TRY ( IR-ARENA:arena n n -- )
   [: SK-KEEP ;] catch {: rc:n :}
   2drop drop
   rc 0= if exit then
   RF-RECORD
   rc throw ;

: WALK-KEEP ( IR-ARENA:arena IR-ARENA:arena n n -- IR-ARENA:arena IR-ARENA:arena n n )
   {: p:IR-ARENA:arena r:IR-ARENA:arena lo:n hi:n :}
   p r lo hi WALK
   p r lo hi ;

: WALK-TRY ( IR-ARENA:arena IR-ARENA:arena n n -- )
   [: WALK-KEEP ;] catch {: rc:n :}
   2drop 2drop
   rc 0= if exit then
   RF-RECORD
   rc throw ;

\ ---- opening the function ----------------------------------------------------
: SIGNATURE ( IR-CTX:ctx IR-BUILD:builder n n -- IR-ID:ir-type-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder in:n out:n :}
   c b CELL-TYPE {: t:IR-ID:ir-type-id :}
   IR-TYPE:FN-BEGIN
   in 0 ?do t IR-TYPE:FN-PARAM loop
   out 0 ?do t IR-TYPE:FN-RESULT loop
   c b IR-BUILD:INTERN-CODE-REF ;

: ARITY-CK ( n n -- )
   {: in:n out:n :}
   in 0 < out 0 < or if E-NELAB-ARITY throw then
   in VMAX > out VMAX > or if E-NELAB-ARITY throw then ;

\ ---- the name a quotation's own function carries ------------------------------
128 constant QNAME-CAP

here CELL 1- and CELL swap - CELL 1- and allot
create QNAME-BUF QNAME-CAP allot
variable QNAME-U
variable FUN-KIND
0 constant FUN-COLON
1 constant FUN-DOES-PARENT
2 constant FUN-DOES-CLAUSE

: QNAME+ ( ptr u8 n -- )
   {: a u:n :}
   QNAME-U @ u + QNAME-CAP > if E-NELAB-QUOT-CAP throw then
   a  QNAME-BUF QNAME-U @ +  u BYTE-COPY
   QNAME-U @ u + QNAME-U ! ;

variable QNAME-P                     \ the place value the digit loop is on

: QNAME-DIGIT ( n -- )
   {: d:n :}
   QNAME-U @ 1+ QNAME-CAP > if E-NELAB-QUOT-CAP throw then
   QNAME-BUF QNAME-U @ +  48 d +  swap c!
   QNAME-U @ 1+ QNAME-U ! ;

: QNAME-DIGITS ( n -- )
   {: k:n :}
   1 QNAME-P !
   begin  k  QNAME-P @ 10 *  >=  while
      QNAME-P @ 10 * QNAME-P !
   repeat
   begin
      k QNAME-P @ /  10 mod  QNAME-DIGIT
      QNAME-P @ 10 / QNAME-P !
      QNAME-P @ 0=
   until ;

: QNAME ( n -- ptr u8 n )
   {: k:n :}
   0 QNAME-U !
   0 QSPELL QNAME+
   FUN-KIND @ FUN-DOES-CLAUSE = if s" ;does" QNAME+ then
   s" [:" QNAME+
   k QNAME-DIGITS
   QNAME-BUF QNAME-U @ ;

: DOES-NAME ( -- ptr u8 n )
   0 QNAME-U !
   0 QSPELL QNAME+
   s" ;does" QNAME+
   QNAME-BUF QNAME-U @ ;

: OPEN-FUN ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:view IR-ID:ir-module-key n n -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder v:IR-ARENA:view key:IR-ID:ir-module-key
      in:n out:n :}
   c b  v key 0 NTAPE:SPELL@  IR-BUILD:BEGIN-FUN
   c b  c b in out SIGNATURE  IR-BUILD:SET-SIGNATURE
   c b IR--FUN-LINKAGE:DEFINED IR-BUILD:SET-LINKAGE
   c b IR--FUN-VISIBILITY:EXPORTED IR-BUILD:SET-VISIBILITY
   c b IR--FUN-CONVENTION:HABU IR-BUILD:SET-CONVENTION
   c b  v key 0 NTAPE:SPAN@  IR-BUILD:SET-FUN-SPAN ;

: OPEN-DOES-FUN ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:view IR-ID:ir-module-key n n n -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder v:IR-ARENA:view key:IR-ID:ir-module-key
      at:n in:n out:n :}
   c b  c b DOES-NAME IR-BUILD:INTERN-SYMBOL  IR-BUILD:BEGIN-FUN
   c b  c b in out SIGNATURE  IR-BUILD:SET-SIGNATURE
   c b IR--FUN-LINKAGE:DEFINED IR-BUILD:SET-LINKAGE
   c b IR--FUN-VISIBILITY:HIDDEN IR-BUILD:SET-VISIBILITY
   c b IR--FUN-CONVENTION:HABU IR-BUILD:SET-CONVENTION
   c b  v key at NTAPE:SPAN@  IR-BUILD:SET-FUN-SPAN ;

: OPEN-BLOCK ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:view IR-ID:ir-module-key n n -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder v:IR-ARENA:view key:IR-ID:ir-module-key
      at:n in:n :}
   c b IR-BUILD:BEGIN-BLOCK
   c b  v key at NTAPE:SPAN@  IR-BUILD:SET-BLOCK-SPAN
   VRESET
   LIT-RESET
   in 0 ?do
      c b  c b CELL-TYPE  IR-BUILD:ADD-BLOCK-ARG VPUSH
   loop
   0 IN-GLUE @ VGLUE-RUN
   in QPARAMS-OPEN ;

\ ---- one quotation body, as a function of its own -----------------------------
: QOPEN-FUN ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:view IR-ID:ir-module-key n -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder v:IR-ARENA:view key:IR-ID:ir-module-key
      k:n :}
   k QIN@ {: in:n :}
   k QOUT@ {: out:n :}
   in out ARITY-CK
   c b  c b  k QNAME  IR-BUILD:INTERN-SYMBOL  IR-BUILD:BEGIN-FUN
   c b  c b in out SIGNATURE  IR-BUILD:SET-SIGNATURE
   c b IR--FUN-LINKAGE:DEFINED IR-BUILD:SET-LINKAGE
   c b IR--FUN-VISIBILITY:HIDDEN IR-BUILD:SET-VISIBILITY
   c b IR--FUN-CONVENTION:HABU IR-BUILD:SET-CONVENTION
   c b  v key k QAT@ NTAPE:SPAN@  IR-BUILD:SET-FUN-SPAN ;

: QOPEN-BLOCK ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:view IR-ID:ir-module-key n -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder v:IR-ARENA:view key:IR-ID:ir-module-key
      k:n :}
   k QIN@ {: in:n :}
   c b IR-BUILD:BEGIN-BLOCK
   c b  v key k QAT@ NTAPE:SPAN@  IR-BUILD:SET-BLOCK-SPAN
   VRESET
   LIT-RESET
   in 0 ?do
      c b  c b CELL-TYPE  IR-BUILD:ADD-BLOCK-ARG VPUSH
   loop ;

: QEMIT-RETURN ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:view IR-ID:ir-module-key n -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder v:IR-ARENA:view key:IR-ID:ir-module-key
      k:n :}
   k QOUT@ {: out:n :}
   VN @ out <> if k QAT@ QUOT-REFUSE then
   RN @ 0<> if k QAT@ QUOT-REFUSE then
   out RETURN-CROSS
   c b HIR-OPCODE:RETURN HIR:ENSURE-OP {: op:IR-ID:ir-symbol-id :}
   c b v key  k QHI@  op OPEN
   out 0 ?do
      c b  VN @ out - i + VAT  IR-BUILD:ADD-OPERAND
   loop
   out VDROP
   c b IR-BUILD:END-OP drop ;

\ Every `hir.quot` staged during the enclosing walk named the ordinal this body
\ is about to take, which is what the assertion holds.
: QBUILD ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:view IR-ID:ir-module-key IR-ARENA:arena IR-ARENA:arena n -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder v:IR-ARENA:view key:IR-ID:ir-module-key
      p:IR-ARENA:arena r:IR-ARENA:arena k:n :}
   k QCONSUMED-CK
   b IR-BUILD:FUNS  k QFUN@  <> if k QAT@ QUOT-REFUSE then
   k QLO@ {: lo:n :}
   k QHI@ {: hi:n :}
   LBN @ {: lb:n :}
   k QCUR !
   r lo hi SKELETON-TRY
   b FUN-STATE!
   c b v key k QOPEN-FUN
   c b v key k QOPEN-BLOCK
   TOK-NEED @ 0<> if k QAT@ EMIT-MEM then
   PATH-LIVE PATH-END !
   p r lo hi WALK-TRY
   CS-N @ 0<> if E-NELAB-CTRL throw then
   PATH-DEAD? {: dead:bool :}
   EXIT-USED @ 0<> if
      dead 0= if
         VN @ k QOUT@ <> if k QAT@ QUOT-REFUSE then
         k QHI@ EXIT-ORD @ 0 0 0 TERM-BR-H
      then
      NB @ EXIT-ORD @ <> if E-NELAB-CTRL throw then
      k QHI@ k QOUT@ 0 0 0 OPEN-ARGS-H
   then
   dead EXIT-USED @ 0= and 0= if
      c b v key k QEMIT-RETURN
      CLOSE-HELD
   then
   c b IR-BUILD:END-FUN drop
   lb LBN !
   QOWNER-DEF QCUR ! ;

: QBUILD-ALL ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:view IR-ID:ir-module-key IR-ARENA:arena IR-ARENA:arena -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder v:IR-ARENA:view key:IR-ID:ir-module-key
      p:IR-ARENA:arena r:IR-ARENA:arena :}
   QN @ 0 ?do
      i QFUN@ QPARAM <> if c b v key p r i QBUILD then
   loop ;

\ A `{: … :}` group inside a quotation body is refused.
: QLOCALS-CK ( -- )
   LG-N @ 0 ?do
      i LG-B@ QINSIDE? if i LG-B@ QUOT-REFUSE then
   loop ;

: NAME-READ ( IR-ARENA:view -- )
   {: v:IR-ARENA:view :}
   v 0 NAME-CK
   v 0 NTAPE-MODE:INTERPRETING MODE-CK ;

public

: HAS-QUOTATIONS? ( -- bool )
   QN @ 0<> ;


: CALLED? ( -- bool )
   CALL-NEED @ 0<> ;

: CALLS-BACK? ( -- bool )
   CALL-BACK @ 0<> ;

: TAIL-CALLED? ( -- bool )
   TAIL-NEED @ 0<> ;

: TAIL-ENTRY@ ( -- n )
   TAIL-ENTRY @ ;

\ ---- when the last call need not be come back from ---------------------------
\ Whether the callee this definition would leave through may be one.
: TAIL-CALLEE? ( IR-ARENA:arena n -- bool )
   {: r:IR-ARENA:arena ix:n :}
   ix WSYM {: sy:IR-ID:ir-symbol-id :}
   r sy HIR-WORD:MEANING@ HIR-MEANING:CALLABLE HIR-MEANING:EQ 0= if false exit then
   r sy HIR-WORD:CALLEE-IN@ IN-N @ <> if false exit then
   r sy HIR-WORD:CALLEE-OUT@ OUT-N @ = ;

: BACK-CALL? ( IR-ARENA:arena n -- bool )
   {: r:IR-ARENA:arena ix:n :}
   ix MOPERAND? if false exit then
   r ix WORD-CALL? 0= if false exit then
   r ix HIR-MEANING:CALLABLE MODELED-AS? 0= if true exit then
   r  ix WSYM  HIR-WORD:CALLEE-DEAD? 0= ;

: BACK-SCAN ( IR-ARENA:arena n n -- )
   {: r:IR-ARENA:arena lo:n hi:n :}
   0 CALL-BACK !
   hi lo ?do
      r i BACK-CALL? if 1 CALL-BACK ! leave then
   loop ;

: TAIL-SCAN ( IR-ARENA:arena n n -- )
   {: r:IR-ARENA:arena lo:n hi:n :}
   0 TAIL-NEED !
   0 TAIL-ENTRY !
   r lo hi BACK-SCAN
   IN-N @ OUT-N @ <> if exit then
   IN-N @ 0= if exit then
   EXIT-USED @ 0<> if exit then
   NB @ 0<> if exit then
   hi lo <= if exit then
   r hi 1- WORD-CALL? 0= if exit then
   r hi 1- TAIL-CALLEE? 0= if exit then
   1 TAIL-NEED !
   r  hi 1- WSYM  HIR-WORD:ENTRY@ TAIL-ENTRY !
   0 CALL-BACK !
   hi 1- lo ?do
      r i BACK-CALL? if 1 CALL-BACK ! leave then
   loop ;

\ A definition whose own rows cannot be segmented is not compiled.
: FRAME-GLUE! ( n n -- )
   {: gin:n gout:n :}
   gin FR-GIN !
   gout FR-GOUT ! ;

private

variable FUN-GIN
variable FUN-GOUT
variable DOES-AT
variable DOES-FUN
PTR-VARIABLE DOES-SIG
variable DOES-SIG-U
variable DOES-PATCH

public

: CAPTURE-PREPARE ( -- )
   0 QN !
   0 LMAX !
   0 LVMAX !
   0 BLOCK-LIMIT !
   NULL-PTR TOK-TABLES ! 0 TMAX !
   NULL-PTR DOES-SIG !  0 DOES-SIG-U !
   0 TAIL-ENTRY !
   0 DOES-PATCH ! ;

private

: UNIT ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:view -- n )
   {: c:IR-CTX:ctx b:IR-BUILD:builder v:IR-ARENA:view :}
   c 0 S-CTX !
   b 0 S-BLD !
   v 0 S-VW !
   b IR-BUILD:MODULE-KEY 0 S-KEY !
   v NTAPE:TOKENS {: n:n :}
   n 1 < if E-NELAB-SHAPE throw then
   n TOK-ROOM
   n LOCALS-ROOM
   v NAME-READ
   n ;

: STAGE-FUN-ADDR ( n n -- )
   {: ix:n fun:n :}
   CTX BLD HIR-OPCODE:QUOT HIR:ENSURE-OP {: op:IR-ID:ir-symbol-id :}
   CTX BLD VW MKEY ix op OPEN
   CTX BLD op RESULTS+
   CTX BLD  CTX BLD HIR:KEY-FUN
   CTX BLD fun IR-BUILD:INTERN-INT-ATTR IR-BUILD:ADD-ATTR
   CTX BLD op CLOSE
   VQ-NONE VN @ 1- VQ! ;

: STAGE-DOES-PATCH ( -- )
   DOES-AT @ DOES-FUN @ STAGE-FUN-ADDR
   DOES-AT @  DOES-SIG @ DOES-SIG-U @ NSTR:INTERN  HIR:ADDR-DATA STAGE-LIT
   DOES-AT @ DOES-SIG-U @ HIR:ADDR-NONE STAGE-LIT
   DOES-AT @ DOES-PATCH @ 3 0 NDICT:GLUE-NONE STAGE-WCALL
   1 CALL-NEED ! ;

: BEFORE-RETURN ( -- )
   FUN-KIND @ FUN-DOES-PARENT = if STAGE-DOES-PATCH then ;

: SCAN-FUN ( IR-ARENA:arena IR-ARENA:arena n n -- )
   {: p:IR-ARENA:arena r:IR-ARENA:arena lo:n hi:n :}
   r lo hi QUOT-SCAN
   FUN-KIND @ FUN-DOES-PARENT = if QBASE @ QN @ + 1+ DOES-FUN ! then
   r lo hi LOCALS-SCAN
   QLOCALS-CK
   r lo hi MATCH-SCAN
   r lo hi DEFER-SCAN
   p r lo hi RESOLVE-SCAN
   r lo hi MEM-SCAN
   r lo hi CROSS-SCAN ;

: OPEN-FUN-BODY ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:view IR-ID:ir-module-key n n -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder v:IR-ARENA:view key:IR-ID:ir-module-key
      in:n out:n :}
   FUN-KIND @ FUN-DOES-CLAUSE = if
      c b v key DOES-AT @ in out OPEN-DOES-FUN
      c b v key DOES-AT @ in OPEN-BLOCK
      exit
   then
   c b v key in out OPEN-FUN
   c b v key 0 in OPEN-BLOCK ;

: BUILD-FUN ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:view IR-ARENA:arena IR-ARENA:arena n n n n -- IR-ID:ir-fun-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder v:IR-ARENA:view p:IR-ARENA:arena
      r:IR-ARENA:arena lo:n hi:n in:n out:n :}
   TOK-RESET
   in IN-N !
   out OUT-N !
   FUN-GIN @ IN-GLUE !  FUN-GOUT @ OUT-GLUE !
   IN-GLUE @ NDICT:GLUE-UNKNOWN = if E-NELAB-BUNDLE throw then
   OUT-GLUE @ NDICT:GLUE-UNKNOWN = if E-NELAB-BUNDLE throw then
   b IR-BUILD:FUNS QBASE !
   p r lo hi SCAN-FUN
   r lo hi SKELETON-TRY
   b FUN-STATE!
   b IR-BUILD:MODULE-KEY {: key:IR-ID:ir-module-key :}
   c b v key in out OPEN-FUN-BODY
   TOK-NEED @ 0<> if lo EMIT-MEM then
   PATH-LIVE PATH-END !
   p r lo hi WALK-TRY
   FUN-KIND @ FUN-COLON = if lo hi in out EMPTY-FRAME-RESHAPE then
   CS-N @ 0<> if E-NELAB-CTRL throw then
   PATH-DEAD? {: dead:bool :}
   EXIT-USED @ 0<> if
      dead 0= if
         VN @ out <> if E-NELAB-ARITY throw then
         BEFORE-RETURN
         0 EXIT-ORD @ 0 0 0 TERM-BR-H
      then
      NB @ EXIT-ORD @ <> if E-NELAB-CTRL throw then
      0 out 0 0 0 OPEN-ARGS-H
   then
   dead  EXIT-USED @ 0=  and 0= if
      EXIT-USED @ 0= if BEFORE-RETURN then
      out QRET-FILL
      c b v key out EMIT-RETURN
      CLOSE-HELD
   then
   r lo hi TAIL-SCAN
   FUN-KIND @ FUN-DOES-PARENT = if 0 TAIL-NEED ! 1 CALL-NEED ! then
   c b IR-BUILD:END-FUN {: f:IR-ID:ir-fun-id :}
   c b v key p r QBUILD-ALL
   f ;

public

: COLON ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:view IR-ARENA:arena IR-ARENA:arena n n -- IR-ID:ir-fun-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder v:IR-ARENA:view p:IR-ARENA:arena
      r:IR-ARENA:arena in:n out:n :}
   RF-RESET
   in out ARITY-CK
   c b v UNIT {: n:n :}
   FUN-COLON FUN-KIND !
   FR-GIN @ FUN-GIN !  FR-GOUT @ FUN-GOUT !
   0 FR-GIN ! 0 FR-GOUT !
   c b v p r 1 n in out BUILD-FUN ;

: DOES ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:view IR-ARENA:arena IR-ARENA:arena n n n n n n n ptr u8 n -- IR-ID:ir-fun-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder v:IR-ARENA:view p:IR-ARENA:arena
      r:IR-ARENA:arena in:n out:n at:n din:n dout:n dgin:n dgout:n sig:ptr sigu:n :}
   RF-RESET
   in out ARITY-CK
   din dout ARITY-CK
   c b v UNIT {: n:n :}
   at 1 < at n >= or if E-NELAB-SHAPE throw then
   s" does-patch" NDICT:CALL-TARGET dup 0= if E-HIR-UNMODELED throw then DOES-PATCH !
   at DOES-AT !  sig DOES-SIG !  sigu DOES-SIG-U !
   FUN-DOES-PARENT FUN-KIND !
   FR-GIN @ FUN-GIN !  FR-GOUT @ FUN-GOUT !
   0 FR-GIN ! 0 FR-GOUT !
   c b v p r 1 at in out BUILD-FUN {: f:IR-ID:ir-fun-id :}
   b IR-BUILD:FUNS DOES-FUN @ <> if E-NELAB-SHAPE throw then
   FUN-DOES-CLAUSE FUN-KIND !
   dgin FUN-GIN !  dgout FUN-GOUT !
   c b v p r at 1+ n din dout BUILD-FUN drop
   1 CALL-NEED !  0 TAIL-NEED !
   f ;

: DOES-FUNCTION ( -- n )
   DOES-FUN @ ;

\ ---- what the last elaboration refused ---------------------------------------

\ The row the refusal was about; every reader of the record asks this first.
: REFUSED-ROW ( -- n )
   RF-ROW @ ;

: REFUSED-KIND? ( NTAPE:kind -- bool )
   {: k:NTAPE:kind :}
   RF-ROW @ 0 < if false exit then
   0 RF-KIND @ k NTAPE-KIND:EQ ;

: REFUSED$ ( -- ptr u8 n )
   RF-ROW @ 0 < if RF-BUF 0 exit then
   RF-BUF RF-U @ ;

: REFUSED-CAP ( -- n )
   RF-CAP ;

: REFUSED-RESET ( -- )
   RF-RESET ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;package
