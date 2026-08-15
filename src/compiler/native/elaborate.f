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
require src/compiler/ir/id.f
require src/compiler/ir/context.f
require src/compiler/ir/arena.f
require src/compiler/ir/type.f
require src/compiler/ir/fun.f
require src/compiler/ir/build.f
require src/compiler/ir/source.f
require src/compiler/native/tape.f
require src/compiler/native/clobber.f
require src/compiler/native/hir.f
require src/compiler/native/hir-word.f
require src/compiler/native/string.f
require src/compiler/native/inline.f
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

: RPUSH ( IR-ID:ir-value-id -- )
   {: val:IR-ID:ir-value-id :}
   RN @ RMAX >= if E-NELAB-CAP throw then
   val RN @ RSTK !
   RN @ 1+ RN ! ;

: RDROP ( n -- )
   {: k:n :}
   k 0 < k RN @ > or if E-NELAB-UNDER throw then
   RN @ k - RN ! ;

\ SPILL COPIES the parked values onto the top of the data vector rather than
\ moving them, because a seam has to leave the return row as it found it.
: R-SPILL ( -- )
   RN @ 0 ?do  i RAT VPUSH  loop ;

: R-FILL ( n -- )
   {: k:n :}
   VN @ k < if E-NELAB-UNDER throw then
   0 RN !
   k 0 ?do  VN @ k - i + VAT RPUSH  loop
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
   base VGLUE-ABOVE? if E-NELAB-BUNDLE throw then
   VN @ base ?do  i VQ@ VQ-NONE <> if E-NELAB-BUNDLE throw then  loop ;

: TO-R ( n -- )
   {: cells:n :}
   cells VN @ > if E-NELAB-UNDER throw then
   VN @ cells - {: base:n :}
   base RSTACK-CK
   RN @ cells + RMAX > if E-NELAB-CAP throw then
   cells 0 ?do  base i + VAT RPUSH  loop
   cells VDROP ;

: FROM-R ( n -- )
   {: cells:n :}
   cells RN @ > if E-NELAB-UNDER throw then
   VN @ cells + VMAX > if E-NELAB-CAP throw then
   RN @ cells - {: base:n :}
   cells 0 ?do  base i + RAT VPUSH  loop
   cells RDROP ;

\ A peek is a pop that does not take: the same cells arrive and the return row
\ is unchanged.
: FETCH-R ( n -- )
   {: cells:n :}
   cells RN @ > if E-NELAB-UNDER throw then
   VN @ cells + VMAX > if E-NELAB-CAP throw then
   RN @ cells - {: base:n :}
   cells 0 ?do  base i + RAT VPUSH  loop ;

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
\ Locals, and groups, one definition may declare.
16 constant LMAX                     \ locals, and groups, one definition may declare
64 constant LNAME-CAP                \ bytes one declaration spelling may hold

here CELL 1- and CELL swap - CELL 1- and allot
variable LN                          \ how many locals were declared
variable LG-N                        \ how many groups the pre-pass found
variable LG-OPEN                     \ the group the pre-pass is reading, or -1
variable LG-K0                       \ the first name index of that open group
variable LGB                         \ how many groups the walk has bound
variable LBN                         \ how many slots are live where the walk stands
LMAX TYPED-BUFFER LNAME IR-ID:ir-symbol-id
LMAX TYPED-BUFFER LVAL IR-ID:ir-value-id
create LCROSS LMAX cells allot       \ whether a call can reach a mention of this local
\ The quotation row a local's value names is carried on the NAME and not on the
\ value, because a value may be renamed and a name may not.
create LQ LMAX cells allot           \ the quotation row this slot's value names, or VQ-NONE
create LROW LMAX cells allot         \ the row the group declaring this local closes on
create LEND LMAX cells allot         \ and the row it goes out of scope on
create LSLOT LMAX cells allot        \ the slot it takes while it is in scope, or -1
create LOWN LMAX cells allot         \ which declaration is in this slot
create LSX LMAX cells allot          \ and whether that one has to travel
create LG-A LMAX cells allot         \ the row each group's `{:` is on
create LG-B LMAX cells allot         \ the row its `:}` is on, or -1 while it is open
create LG-K LMAX cells allot         \ how many names it declares
create LG-F LMAX cells allot         \ the first declaration index it declares
create LBUF LNAME-CAP allot

: LRESET ( -- )
   0 LN !
   0 LG-N !
   -1 LG-OPEN !
   0 LG-K0 !
   0 LGB !
   0 LBN !
   LMAX 0 ?do
      0 i cells LCROSS + !
      VQ-NONE i cells LQ + !
      -1 i cells LOWN + !
      0 i cells LSX + !
   loop ;

: LAT ( n -- n )
   dup 0 < over LN @ >= or if E-NELAB-LOCAL throw then ;

: LSAT ( n -- n )
   dup 0 < over LMAX >= or if E-NELAB-LOCAL throw then ;

: LQ@ ( n -- n )     LSAT cells LQ + @ ;
: LQ! ( n n -- )     {: k:n i:n :}  k i LSAT cells LQ + ! ;

: LOWN@ ( n -- n )   LSAT cells LOWN + @ ;
: LOWN! ( n n -- )   {: k:n i:n :}  k i LSAT cells LOWN + ! ;

: LSX@ ( n -- bool ) LSAT cells LSX + @ 0<> ;
: LSX! ( bool n -- )
   {: f:bool i:n :}
   f if 1 else 0 then  i LSAT cells LSX + ! ;

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

\ Whether ONE of this body's calls keeps no register for the caller.
variable CALL-BARE

\ Whether this local's value has to TRAVEL - be handed over at every call and
\ taken back - which is what a call inside its scope makes true.
: LCROSS? ( n -- bool )
   LAT cells LCROSS + @ 0<>
   CALL-BARE @ 0<> and ;

: LCROSS+ ( n -- )
   LAT cells LCROSS +  1 swap ! ;

: IN-DECL? ( n -- bool )
   {: ix:n :}
   false
   LG-N @ 0 ?do
      ix i LG-A@ >=  ix i LG-B@ <  and or
   loop ;

\ The comparison is against the declared name, not the spelling of the mention.
: LOCAL-OF ( n -- n )
   {: ix:n :}
   VW ix NTAPE:KIND@ NTAPE-KIND:NAME NTAPE-KIND:EQ 0= if -1 exit then
   VW MKEY ix NTAPE:SPELL@ {: sy:IR-ID:ir-symbol-id :}
   -1
   LN @ 0 ?do
      sy i LNAME @ NFROZEN:SAME-SYM?  i ix LIVE-AT?  and if drop i leave then
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

\ No double anywhere on the vector where a value leaves as a cell.
: NO-REAL-CK ( -- )
   VN @ 0 ?do
      i VAT REAL-VALUE? if E-NELAB-TYPE throw then
   loop
   RN @ 0 ?do
      i RAT REAL-VALUE? if E-NELAB-TYPE throw then
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
   CTX BLD kop HIR:OPCODE {: op:IR-ID:ir-symbol-id :}
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

\ The parked values as cells, for the same reason and by the same route.
: R-CROSS ( n -- )
   {: ix:n :}
   RN @ 0 ?do
      i RAT REAL-VALUE? if
         ix  i RAT  HIR-OPCODE:REALBITS CROSS-VALUE  i RSTK !
      then
   loop ;

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

\ ---- and what a call does to it ----------------------------------------------
\ A call may destroy the register a memo row's value lives in, so the memo is
\ emptied at every call.
: LIT-CALL-BARRIER ( n -- )
   {: entry:n :}
   entry NCLOB:KNOWN? if exit then
   LIT-RESET ;

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
   CTX BLD HIR-OPCODE:CONST HIR:OPCODE {: op:IR-ID:ir-symbol-id :}
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
   CTX BLD HIR-OPCODE:MEM HIR:OPCODE {: op:IR-ID:ir-symbol-id :}
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
   CTX BLD k HIR:OPCODE {: op:IR-ID:ir-symbol-id :}
   op TOKEN-READY
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
   CTX BLD HIR-OPCODE:FCONST HIR:OPCODE {: op:IR-ID:ir-symbol-id :}
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
   ix STRING-BODY {: a u:n :} \ typed-local-lint: allow-bare-local - a keeps the ptr u8 byte-span role
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
   c b HIR-OPCODE:RETURN HIR:OPCODE {: op:IR-ID:ir-symbol-id :}
   c b v key 0 op OPEN
   out 0 ?do
      c b  i VAT  IR-BUILD:ADD-OPERAND
   loop
   out VDROP
   c b IR-BUILD:END-OP drop ;

\ ---- the open control structures ----------------------------------------------
\ Structures one definition may nest.
32 constant CMAX

here CELL 1- and CELL swap - CELL 1- and allot
variable CS-N
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
CMAX TYPED-BUFFER CS-IDX IR-ID:ir-value-id
CMAX TYPED-BUFFER CS-LIM IR-ID:ir-value-id

: CS-RESET ( -- )
   0 CS-N ! ;

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
1 constant PATH-EXIT                 \ an `exit` closed the arm; only its `then` may follow
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

\ This walk's own ordinal, raised into the module's.
: BLOCK-ORD ( n -- IR-ID:ir-block-id )
   {: k:n :}
   k 0 < k NFROZEN:BMAX >= or if E-NELAB-BLOCK throw then
   MKEY  BBASE @ k +  IR-ID:PACK-BLOCK ;

\ The base is held against the module at every block, so a drift is a refusal.
: CLOSE-HELD ( -- )
   CTX BLD IR-BUILD:END-BLOCK IR-ID:BLOCK-LOCAL {: ord:n :}
   ord  BBASE @ NB @ +  <> if E-NELAB-BLOCK throw then ;

: CLOSE-BLOCK ( -- )
   CLOSE-HELD
   NB @ 1+ {: k:n :}
   k NFROZEN:BMAX > if E-NELAB-BLOCK throw then
   k NB ! ;

\ ---- what type each block argument has ---------------------------------------
\ Block argument positions across the whole function.
NFROZEN:BMAX VMAX * constant ARG-CAP

here CELL 1- and CELL swap - CELL 1- and allot
create ARG-N NFROZEN:BMAX cells allot   \ vector positions stated for this block, or -1
create ARG-G NFROZEN:BMAX cells allot   \ where the values in those positions begin, as the boundary mask above
create ARG-R NFROZEN:BMAX cells allot   \ how many of the TOP positions are parked return values
ARG-CAP TYPED-BUFFER ARG-T IR-ID:ir-type-id  \ the type each of those positions has
VMAX TYPED-BUFFER XV IR-ID:ir-value-id  \ what the edge being staged really hands over

: ARG-RESET ( -- )
   NFROZEN:BMAX 0 ?do
      -1 i cells ARG-N + !  0 i cells ARG-G + !  0 i cells ARG-R + !
   loop ;

: ARG-BLOCK-CK ( n -- n )
   dup 0 < over NFROZEN:BMAX >= or if E-NELAB-BLOCK throw then ;

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
      ix i  t i ARG-T@  EDGE-VALUE  i XV !
   loop ;

: TERM-BR-H ( n n n n n -- )
   {: ix:n t:n lo:n h:n l:n :}
   R-SPILL
   ix t EDGE-STAGE
   CTX BLD  CTX BLD HIR-OPCODE:BR HIR:OPCODE  IR-BUILD:BEGIN-OP
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
   CTX BLD  CTX BLD HIR-OPCODE:BRZ HIR:OPCODE  IR-BUILD:BEGIN-OP
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

: IN-SCOPE-NOW? ( n -- bool )
   {: k:n :}
   k LG-K0 @ >= if true exit then
   k LROW@ 0 < if false exit then
   k LEND@ 0 < ;

\ The duplicate this file refuses is a LIVE one, and only a live one.
: DUP-LOCAL? ( IR-ID:ir-symbol-id -- bool )
   {: sy:IR-ID:ir-symbol-id :}
   false
   LN @ 0 ?do
      i IN-SCOPE-NOW? if  sy i LNAME @ NFROZEN:SAME-SYM? or  then
   loop ;

\ ---- the one name a local may not take ----------------------------------------
: PRE-FRAME? ( IR-ARENA:arena IR-ID:ir-symbol-id -- bool )
   {: r:IR-ARENA:arena sy:IR-ID:ir-symbol-id :}
   r sy HIR-WORD:MODELS? 0= if false exit then
   r sy HIR-WORD:MEANING@ HIR-MEANING:CONTROL HIR-MEANING:EQ 0= if false exit then
   r sy HIR-WORD:CTRL@ HIR-CTRL:CLOSE-QUOT HIR-CTRL:EQ ;

\ The bare name, interned into this module so every mention reaches one symbol.
: DECLARE-LOCAL ( IR-ARENA:arena n -- )
   {: r:IR-ARENA:arena ix:n :}
   LN @ LMAX >= if E-NELAB-LOCAL-CAP throw then
   CTX BLD  VW MKEY ix NTAPE:SPELL@  LBUF LNAME-CAP IR-BUILD:SYMBOL-COPY {: u:n :}
   LBUF u HIR-WORD:LOCAL-NAME-LEN {: nu:n :}
   nu 1 < if E-NELAB-LOCAL throw then
   CTX BLD LBUF nu IR-BUILD:INTERN-SYMBOL {: sy:IR-ID:ir-symbol-id :}
   r  CTX BLD sy HIR-WORD:KEY-SYM  PRE-FRAME? if E-NELAB-LOCAL throw then
   sy DUP-LOCAL? if E-NELAB-LOCAL throw then
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
   VW MKEY ix NTAPE:SPELL@ {: sy:IR-ID:ir-symbol-id :}
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
   ;MATCH ;

: GROUP-OPEN ( n -- )
   {: ix:n :}
   LG-N @ LMAX >= if E-NELAB-LOCAL-CAP throw then
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
: LOCALS-SCAN ( IR-ARENA:arena n -- )
   {: r:IR-ARENA:arena n:n :}
   LRESET
   SS-RESET
   n 1 ?do
      r i SCAN-STEP
   loop
   LG-OPEN @ 0 >= if E-NELAB-LOCAL throw then
   LN @ 0 ?do
      i LEND@ 0 < if n i LEND! then
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
256 constant TMAX                    \ body tokens one definition may have

: TOK-CK ( n -- n )
   dup 0 < over TMAX >= or if E-NELAB-BLOCK throw then ;

\ ---- the bodies this definition defers ---------------------------------------
\ Quotation bodies one definition may hold; the definition's own function is
\ not one of them.
NFROZEN:FMAX 1- constant QMAX        \ quotation bodies one definition may hold

-1 constant QNONE                    \ no consumer has said what this body takes and leaves

here CELL 1- and CELL swap - CELL 1- and allot
variable QN                          \ how many bodies this definition holds
variable QD                          \ the row of the body the pre-scan has open, or -1
variable QBASE                       \ the ordinal of the function the definition itself is
create QAT   QMAX cells allot        \ the `[:` each body was opened at
create QLO   QMAX cells allot        \ its first body token
create QHI   QMAX cells allot        \ its `;]`, which is one past its last body token
create QIN   QMAX cells allot        \ what the body takes, in cells, or QNONE
create QOUT  QMAX cells allot        \ and what it leaves
create QOPENED TMAX cells allot      \ this token opens body k, or -1
\ This row's quotation is no body of this emission - it is a parameter.
-1 constant QPARAM                   \ this row's quotation is no body of this emission
create QFUN  QMAX cells allot        \ the function of this emission a row's body is

\ ---- which body each token belongs to, and which body is being walked ---------
-1 constant QOWNER-DEF               \ the definition's own function, which is no body
create QOWN TMAX cells allot         \ the body each token belongs to, or QOWNER-DEF
variable QCUR                        \ the body being walked, or QOWNER-DEF

: QUOT-RESET ( -- )
   0 QN !
   -1 QD !
   QOWNER-DEF QCUR !
   TMAX 0 ?do
      -1 i cells QOPENED + !
      QOWNER-DEF i cells QOWN + !
   loop ;

: QROW-CK ( n -- n )
   dup 0 < over QN @ >= or if E-NELAB-QUOT-CAP throw then ;

: QAT@ ( n -- n )    QROW-CK cells QAT + @ ;
: QLO@ ( n -- n )    QROW-CK cells QLO + @ ;
: QHI@ ( n -- n )    QROW-CK cells QHI + @ ;
: QFUN@ ( n -- n )   QROW-CK cells QFUN + @ ;
: QIN@ ( n -- n )    QROW-CK cells QIN + @ ;
: QOUT@ ( n -- n )   QROW-CK cells QOUT + @ ;

: QOPENED@ ( n -- n )
   TOK-CK cells QOPENED + @ ;

: QOWN@ ( n -- n )
   TOK-CK cells QOWN + @ ;

: QSKIP? ( n -- bool )
   QOWN@ QCUR @ <> ;

: QINSIDE? ( n -- bool )
   QOWN@ QOWNER-DEF <> ;

: QFILL ( n n n -- )
   {: k:n in:n out:n :}
   k QIN@ QNONE <> if
      k QIN@ in <>  k QOUT@ out <>  or if k QAT@ QUOT-REFUSE then
      exit
   then
   in k cells QIN + !
   out k cells QOUT + ! ;

128 constant QSPELL-CAP

here CELL 1- and CELL swap - CELL 1- and allot
create QSPELL-BUF QSPELL-CAP allot

: QSPELL ( n -- ptr u8 n )
   {: ix:n :}
   CTX BLD  VW MKEY ix NTAPE:SPELL@  QSPELL-BUF QSPELL-CAP IR-BUILD:SYMBOL-COPY
   QSPELL-BUF swap ;

\ ---- where a body's arity comes from -----------------------------------------
: QARG-FILL ( n n -- )
   {: ix:n j:n :}
   VN @ 1- j -  VQ@ {: k:n :}
   k 0 < if exit then
   ix QSPELL j NDICT:SPELL-QUOT-DIN {: qi:n qo:n :}
   qi NDICT:QUOT-NONE = if k QAT@ QUOT-REFUSE then
   k qi qo QFILL ;

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

\ Every body this definition holds has to have been told what it takes.
: QCONSUMED-CK ( -- )
   QN @ 0 ?do
      i QFUN@ QPARAM <>  i QIN@ QNONE =  and if i QAT@ QUOT-REFUSE then
   loop ;

: QOPEN-ROW ( n -- )
   {: ix:n :}
   QN @ QMAX >= if E-NELAB-QUOT-CAP throw then
   QN @ {: k:n :}
   ix k cells QAT + !
   ix 1+ k cells QLO + !
   QNONE k cells QIN + !
   QNONE k cells QOUT + !
   QBASE @ k + 1+  k cells QFUN + !
   k ix TOK-CK cells QOPENED + !
   k QD !
   k 1+ QN ! ;

\ ---- a quotation this definition was HANDED ----------------------------------
: QOPEN-PARAM ( n n -- )
   {: j:n cellix:n :}
   0 QSPELL j NDICT:SPELL-QUOT-DIN {: qi:n qo:n :}
   qi NDICT:QUOT-NONE = if exit then
   QN @ QMAX >= if E-NELAB-QUOT-CAP throw then
   QN @ {: k:n :}
   0 k cells QAT + !
   0 k cells QLO + !
   0 k cells QHI + !
   qi k cells QIN + !
   qo k cells QOUT + !
   QPARAM k cells QFUN + !
   k 1+ QN !
   k cellix VQ! ;

: QPARAMS-OPEN ( n -- )
   {: in:n :}
   in 0 ?do
      i  in 1- i -  QOPEN-PARAM
   loop ;

: QCLOSE-ROW ( n -- )
   {: ix:n :}
   QD @ QROW-CK {: k:n :}
   ix k cells QHI + !
   ix  k QLO@  ?do
      k i TOK-CK cells QOWN + !
   loop
   k ix TOK-CK cells QOWN + !
   -1 QD ! ;

: QSCAN-STEP ( IR-ARENA:arena n -- )
   {: r:IR-ARENA:arena ix:n :}
   r ix HIR-CTRL:OPEN-QUOT ROW-CTRL? if
      QD @ 0 >= if ix QUOT-REFUSE then
      ix QOPEN-ROW
      exit
   then
   r ix HIR-CTRL:CLOSE-QUOT ROW-CTRL? if
      QD @ 0 < if ix QUOT-REFUSE then
      ix QCLOSE-ROW
   then ;

: QUOT-SCAN ( IR-ARENA:arena n -- )
   {: r:IR-ARENA:arena n:n :}
   QUOT-RESET
   n 1 ?do
      r i QSCAN-STEP
   loop
   QD @ 0 >= if QD @ QAT@ QUOT-REFUSE then ;

\ ---- the three tag-dispatch forms, read before anything else reads a word -----
\ The modes are the engine's own CMM numbers.
0 constant MR-NONE                   \ an ordinary body token
1 constant MR-FAMILY                 \ the family operand of a `MATCH` or a `construct`
2 constant MR-VARIANT                \ the variant operand of one
3 constant MR-DEFER                  \ the deferred word `is` binds to

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
create MROLE TMAX cells allot        \ which operand, if any, this row is
create MTAG TMAX cells allot         \ an arm's variant tag, a `MATCH` row's trap ordinal, a `construct` row's tag
create MPAD TMAX cells allot         \ the zero pads an arm drops, or a `construct` pushes
create MPAY TMAX cells allot         \ the payload CELLS an arm keeps or a `construct` consumes - and, on a variant row, its payload FIELDS, which is the count the arm's `of` holds those cells against
create MWID TMAX cells allot         \ a `MATCH` row's bundle width in cells, as the instantiation really occupies it
create MFLD TMAX cells allot         \ how many FIELDS an arm's payload has, beside the CELLS it keeps
create MEND TMAX cells allot         \ whether this `of` opens the LAST arm of its `MATCH`
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
   TMAX 0 ?do
      MR-NONE i cells MROLE + !
      0 i cells MEND + !
   loop ;

: MROLE@ ( n -- n )   TOK-CK cells MROLE + @ ;
: MTAG@ ( n -- n )    TOK-CK cells MTAG + @ ;
: MPAD@ ( n -- n )    TOK-CK cells MPAD + @ ;
: MPAY@ ( n -- n )    TOK-CK cells MPAY + @ ;
: MWID@ ( n -- n )    TOK-CK cells MWID + @ ;
: MFLD@ ( n -- n )    TOK-CK cells MFLD + @ ;
: MEND@ ( n -- bool ) TOK-CK cells MEND + @ 0<> ;

: MOPERAND? ( n -- bool )
   MROLE@ MR-NONE <> ;

: MROLE! ( n n -- ) {: ix:n r:n :}
   r ix TOK-CK cells MROLE + ! ;

: MVAR! ( n n n -- ) {: ix:n tag:n terms:n :}
   tag ix TOK-CK cells MTAG + !
   terms ix TOK-CK cells MPAY + ! ;

: MARM! ( n n n n -- ) {: ix:n tag:n pads:n pay:n :}
   tag ix TOK-CK cells MTAG + !
   pads ix TOK-CK cells MPAD + !
   pay ix TOK-CK cells MPAY + ! ;

: MMATCH! ( n n n -- ) {: ix:n w:n ord:n :}
   w ix TOK-CK cells MWID + !
   ord ix TOK-CK cells MTAG + ! ;

: MFLD! ( n n -- ) {: ix:n fields:n :}
   fields ix TOK-CK cells MFLD + ! ;

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
   ix  vid NFAM:TAG  vid NFAM:PAY-TERMS  MVAR!
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
   ix  vix MTAG@  pads  pay  MARM!
   ix  vix MPAY@  MFLD!
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
: MATCH-SCAN ( IR-ARENA:arena n -- ) {: r:IR-ARENA:arena n:n :}
   MATCH-RESET
   n 1 ?do
      r i MSCAN-STEP
   loop
   MSN @ 0<> if E-NELAB-MATCH throw then
   MM @ MM-OFF <> if E-NELAB-MATCH throw then ;

\ ---- the deferred word `is` names --------------------------------------------
: DSCAN-STEP ( IR-ARENA:arena n n -- )
   {: r:IR-ARENA:arena n:n ix:n :}
   ix IN-DECL? if exit then
   ix LOCAL-OF 0 >= if exit then
   r ix HIR-CTRL:BIND-DEFER ROW-CTRL? 0= if exit then
   ix 1+ {: t:n :}
   t n >= if E-NELAB-DEFER throw then
   VW t NTAPE:KIND@ NTAPE-KIND:NAME NTAPE-KIND:EQ 0= if E-NELAB-DEFER throw then
   t MR-DEFER MROLE! ;

: DEFER-SCAN ( IR-ARENA:arena n -- ) {: r:IR-ARENA:arena n:n :}
   n 1 ?do
      r n i DSCAN-STEP
   loop ;

\ ---- the names the dialect does not model, before anything reads the model ----
\ A name the dialect does not model is resolved through dict.f at the point it
\ is used, in the order the engine resolves the body that wrote it.
: RESOLVE-STEP ( IR-ARENA:arena n -- )
   {: r:IR-ARENA:arena ix:n :}
   ix MOPERAND? if exit then
   ix IN-DECL? if exit then
   ix LOCAL-OF 0 >= if exit then
   VW ix NTAPE:KIND@ NTAPE-KIND:NAME NTAPE-KIND:EQ 0= if exit then
   ix WSYM {: sy:IR-ID:ir-symbol-id :}
   r sy HIR-WORD:MODELS? if exit then
   CTX BLD r sy HIR-WORD:RESOLVE-FIXED if exit then
   CTX BLD r sy HIR-WORD:RESOLVE-CALLABLE drop ;

: RESOLVE-SCAN ( IR-ARENA:arena n -- )
   {: r:IR-ARENA:arena n:n :}
   n 1 ?do
      r i RESOLVE-STEP
   loop ;

\ ---- which calls are COPIED instead of made ----------------------------------
here CELL 1- and CELL swap - CELL 1- and allot
create INL-TAB TMAX cells allot      \ whether the call on this body token is copied

: INL-RESET ( -- )
   TMAX 0 ?do
      0 i cells INL-TAB + !
   loop ;

: INL-AT? ( n -- bool )
   TOK-CK cells INL-TAB + @ 0<> ;

: INL+ ( n -- )
   1 swap TOK-CK cells INL-TAB + ! ;

: INL-SYM ( n n -- IR-ID:ir-symbol-id )
   {: entry:n k:n :}
   CTX BLD  entry k NINL:SPELL$  HIR-WORD:KEY-SPELL ;

public

\ What a copy stages for a token of one meaning.
ENUM staging DERIVE eq
   call
   op
   const-op
   fixed
   rename
;ENUM

private

\ The one table: every meaning is answered here and nowhere else.
: SPLICE-STAGING ( HIR:meaning -- staging )
   MATCH HIR:meaning
      literal      OF NELAB-STAGING:CALL ENDOF
      real-literal OF NELAB-STAGING:CALL ENDOF
      string-literal OF NELAB-STAGING:CALL ENDOF
      op           OF NELAB-STAGING:OP ENDOF
      const-op     OF NELAB-STAGING:CONST-OP ENDOF
      fixed        OF NELAB-STAGING:FIXED ENDOF
      rename       OF NELAB-STAGING:RENAME ENDOF
      rstack       OF NELAB-STAGING:CALL ENDOF
      callable     OF NELAB-STAGING:CALL ENDOF
      control      OF NELAB-STAGING:CALL ENDOF
      open-locals  OF NELAB-STAGING:CALL ENDOF
      close-locals OF NELAB-STAGING:CALL ENDOF
      unmodeled    OF NELAB-STAGING:CALL ENDOF
   ;MATCH ;

: SPLICE-MEANING? ( HIR:meaning -- bool )
   SPLICE-STAGING NELAB-STAGING:CALL NELAB-STAGING:EQ 0= ;

: REC-NAME? ( n n -- bool )
   {: entry:n k:n :}
   entry k NINL:KIND@ NTAPE-KIND:NAME NTAPE-KIND:EQ ;

: REC-TOKEN? ( IR-ARENA:arena n n -- bool )
   {: r:IR-ARENA:arena entry:n k:n :}
   entry k NINL:KIND@ {: kd:NTAPE:kind :}
   kd NTAPE-KIND:INT-LITERAL NTAPE-KIND:EQ if true exit then
   kd NTAPE-KIND:REAL-LITERAL NTAPE-KIND:EQ if true exit then
   kd NTAPE-KIND:NAME NTAPE-KIND:EQ 0= if false exit then
   entry k INL-SYM {: sy:IR-ID:ir-symbol-id :}
   r sy HIR-WORD:MODELS? 0= if false exit then
   r sy HIR-WORD:MEANING@ SPLICE-MEANING? ;

: REC-BODY? ( IR-ARENA:arena n -- bool )
   {: r:IR-ARENA:arena entry:n :}
   true
   entry NINL:TOKENS 0 ?do
      r entry i REC-TOKEN? 0= if drop false leave then
   loop ;

\ ---- which control actions stage a call, and which call ----------------------
: CTRL-CALL? ( HIR:ctrl -- HIR:opcode bool )
   {: k:HIR:ctrl :}
   k HIR-CTRL:SELF-CALL HIR-CTRL:EQ if HIR-OPCODE:CALL true exit then
   k HIR-CTRL:BIND-DEFER HIR-CTRL:EQ if HIR-OPCODE:WORDCALL true exit then
   k HIR-CTRL:EXEC HIR-CTRL:EQ if HIR-OPCODE:WORDCALL true exit then
   k HIR-CTRL:CATCH HIR-CTRL:EQ if HIR-OPCODE:WORDCALL true exit then
   HIR-OPCODE:CALL false ;

: SYM-ORDER? ( IR-ARENA:arena IR-ID:ir-symbol-id -- bool )
   {: r:IR-ARENA:arena sy:IR-ID:ir-symbol-id :}
   r sy HIR-WORD:MODELS? 0= if false exit then
   r sy HIR-WORD:MEANING@ {: m:HIR:meaning :}
   m HIR-MEANING:OP HIR-MEANING:EQ if
      CTX BLD  CTX BLD  r sy HIR-WORD:OPCODE@  HIR:OPCODE  TOKEN-OPERANDS
      0<> exit
   then
   m HIR-MEANING:CONST-OP HIR-MEANING:EQ if
      CTX BLD  CTX BLD  r sy HIR-WORD:CONST-OPCODE@  HIR:OPCODE  TOKEN-OPERANDS
      0<> exit
   then
   false ;

: REC-TOKEN-ORDER? ( IR-ARENA:arena n n -- bool )
   {: r:IR-ARENA:arena entry:n k:n :}
   entry k REC-NAME? 0= if false exit then
   r  entry k INL-SYM  SYM-ORDER? ;

: REC-BODY-ORDER? ( IR-ARENA:arena n -- bool )
   {: r:IR-ARENA:arena entry:n :}
   false
   entry NINL:TOKENS 0 ?do
      r entry i REC-TOKEN-ORDER? or
   loop ;

\ Whether the callee named on this token has a body that may be copied here.
: CALLEE-COPY? ( IR-ARENA:arena n -- bool )
   {: r:IR-ARENA:arena ix:n :}
   ix WSYM {: sy:IR-ID:ir-symbol-id :}
   r sy HIR-WORD:CALLEE-DEAD? if false exit then
   r sy HIR-WORD:ENTRY@ {: entry:n :}
   entry NINL:KNOWN? 0= if false exit then
   entry NINL:IN@  r sy HIR-WORD:CALLEE-IN@  <> if E-NELAB-INLINE throw then
   entry NINL:OUT@ r sy HIR-WORD:CALLEE-OUT@ <> if E-NELAB-INLINE throw then
   r entry REC-BODY? ;

: INL-STEP ( IR-ARENA:arena n -- )
   {: r:IR-ARENA:arena ix:n :}
   r ix HIR-MEANING:CALLABLE MODELED-AS? 0= if exit then
   r ix CALLEE-COPY? if ix INL+ then ;

: INLINE-SCAN ( IR-ARENA:arena n -- )
   {: r:IR-ARENA:arena n:n :}
   INL-RESET
   n 1 ?do
      i MOPERAND? 0=  i IN-DECL? 0=  and  i LOCAL-OF 0 <  and if r i INL-STEP then
   loop ;

\ ---- does this definition touch memory at all? -------------------------------
: WORD-ORDER? ( IR-ARENA:arena n -- bool )
   {: r:IR-ARENA:arena ix:n :}
   VW ix NTAPE:KIND@ NTAPE-KIND:NAME NTAPE-KIND:EQ 0= if false exit then
   ix WSYM {: sy:IR-ID:ir-symbol-id :}
   r sy HIR-WORD:MODELS? 0= if false exit then
   r sy HIR-WORD:MEANING@ {: m:HIR:meaning :}
   m HIR-MEANING:CALLABLE HIR-MEANING:EQ if
      ix INL-AT? if r  r sy HIR-WORD:ENTRY@  REC-BODY-ORDER? exit then
      CTX BLD  CTX BLD  HIR-OPCODE:WORDCALL HIR:OPCODE  TOKEN-OPERANDS
      0<> exit
   then
   m HIR-MEANING:CONTROL HIR-MEANING:EQ if
      r sy HIR-WORD:CTRL@ CTRL-CALL? {: op:HIR:opcode calls:bool :}
      calls 0= if false exit then
      CTX BLD  CTX BLD  op HIR:OPCODE  TOKEN-OPERANDS 0<> exit
   then
   r sy SYM-ORDER? ;

: MEM-SCAN ( IR-ARENA:arena n -- )
   {: r:IR-ARENA:arena n:n :}
   0 TOK-NEED !
   n 1 ?do
      i MOPERAND? 0=  i IN-DECL? 0=  and  i LOCAL-OF 0 <  and if
         r i WORD-ORDER? if 1 TOK-NEED ! then
      then
   loop ;

\ ---- does this definition CALL at all? ---------------------------------------
: WORD-CALL? ( IR-ARENA:arena n -- bool )
   {: r:IR-ARENA:arena ix:n :}
   VW ix NTAPE:KIND@ NTAPE-KIND:NAME NTAPE-KIND:EQ 0= if false exit then
   ix WSYM {: sy:IR-ID:ir-symbol-id :}
   r sy HIR-WORD:MODELS? 0= if false exit then
   r sy HIR-WORD:MEANING@ {: m:HIR:meaning :}
   m HIR-MEANING:CALLABLE HIR-MEANING:EQ if ix INL-AT? 0= exit then
   m HIR-MEANING:CONTROL HIR-MEANING:EQ if
      r sy HIR-WORD:CTRL@ CTRL-CALL? {: op:HIR:opcode calls:bool :}
      calls exit
   then
   false ;

\ ---- and does the call leave the caller anything? ----------------------------
\ Whether the call keeps any register for the caller.
: CALL-KEEPS? ( IR-ARENA:arena n -- bool )
   {: r:IR-ARENA:arena ix:n :}
   ix WSYM {: sy:IR-ID:ir-symbol-id :}
   r sy HIR-WORD:MEANING@ HIR-MEANING:CALLABLE HIR-MEANING:EQ 0= if false exit then
   r sy HIR-WORD:ENTRY@ NCLOB:KNOWN? ;

\ ---- which locals a call can reach -------------------------------------------
32 constant LSMAX                    \ loops one definition may nest, as CMAX does

here CELL 1- and CELL swap - CELL 1- and allot
variable LSN                         \ loops the scan is inside
create LS-CALL LSMAX cells allot     \ whether a call has been met inside this loop
create LS-PEND LSMAX cells allot     \ locals mentioned in it before any call was met

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
   LSN @ LSMAX >= if E-NELAB-BLOCK throw then
   0 LSN @ cells LS-CALL + !
   0 LSN @ cells LS-PEND + !
   LSN @ 1+ LSN ! ;

: LS-POP ( -- )
   LSN @ 1 < if E-NELAB-CTRL throw then
   LSN @ 1- LSN !
   LSN @ cells LS-CALL + @ 0= if exit then
   LSN @ cells LS-PEND + @ {: m:n :}
   LN @ 0 ?do
      m 1 i lshift and 0<> if i LCROSS+ then
   loop ;

: LS-CALL+ ( -- )
   LSN @ 0 ?do  1 i cells LS-CALL + !  loop ;

: LS-PEND+ ( n -- )
   {: k:n :}
   LSN @ 0 ?do
      i cells LS-PEND + @  1 k lshift or  i cells LS-PEND + !
   loop ;

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
      r ix CALL-KEEPS? 0= if 1 CALL-BARE ! then
      LS-CALL+ exit
   then
   r ix OPENS-LOOP? if LS-PUSH exit then
   r ix CLOSES-LOOP? if LS-POP then ;

: CROSS-SCAN ( IR-ARENA:arena n -- )
   {: r:IR-ARENA:arena n:n :}
   0 CALL-NEED !
   0 CALL-BARE !
   0 LSN !
   n 1 ?do
      r i CROSS-STEP
   loop
   LSN @ 0<> if E-NELAB-CTRL throw then ;

\ ---- the block skeleton ------------------------------------------------------
here CELL 1- and CELL swap - CELL 1- and allot
create JOIN-TAB TMAX cells allot

: JOIN-RESET ( -- )
   TMAX 0 ?do
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
   PATH-END @ PATH-EXIT = if E-NELAB-CTRL throw then
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
   NB @ 1+ NB !
   t CS-JOIN@ NB @ JOIN!
   CS-POP ;

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
   NB @ 1+ NB !
   CS-POP
   PATH-DEAD PATH-END ! ;

: SK-LEAVE ( -- )
   DO-OPEN-N 0= if E-NELAB-CTRL throw then
   NB @ 1+ NB !
   PATH-DEAD PATH-END ! ;

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
   ix INL-AT? if false exit then
   r  ix WSYM  HIR-WORD:CALLEE-DEAD? ;

: SK-AFTER-END-CK ( IR-ARENA:arena n -- )
   {: r:IR-ARENA:arena ix:n :}
   r ix HIR-MEANING:CONTROL MODELED-AS? 0= if E-NELAB-CTRL throw then
   r  ix WSYM  HIR-WORD:CTRL@ {: k:HIR:ctrl :}
   k HIR-CTRL:CLOSE-IF HIR-CTRL:EQ if exit then
   k HIR-CTRL:MID-ELSE HIR-CTRL:EQ  PATH-DEAD?  and if exit then
   k HIR-CTRL:CLOSE-ARM HIR-CTRL:EQ  PATH-DEAD?  and if exit then
   k HIR-CTRL:CLOSE-CASE HIR-CTRL:EQ  PATH-DEAD?  and if exit then
   E-NELAB-CTRL throw ;

: SK-STEP ( IR-ARENA:arena n -- )
   {: r:IR-ARENA:arena ix:n :}
   VW ix NTAPE-MODE:COMPILING MODE-CK
   ix QSKIP? if exit then
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
      open-do-skip OF HIR-CTRL:OPEN-DO ix SK-PUSH  NB @ 3 + NB ! ENDOF
      close-loop   OF HIR-CTRL:OPEN-DO CS-OPENER-CK CS-JOIN@
                      NB @ 3 + NB !  NB @ JOIN!  CS-POP ENDOF
      index        OF ENDOF
      outer-index  OF ENDOF
      drop-loop    OF ENDOF
      early-leave  OF SK-LEAVE ENDOF
      early-exit   OF NB @ 1+ NB !  1 EXIT-USED !  PATH-EXIT PATH-END ! ENDOF
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
   ;MATCH ;

\ A structure left open at the end of the body is a refusal.
: SKELETON ( IR-ARENA:arena n n -- )
   {: r:IR-ARENA:arena lo:n hi:n :}
   hi TMAX > if E-NELAB-BLOCK throw then
   0 NB !
   JOIN-RESET
   CS-RESET
   EXIT-RESET
   hi lo ?do
      r i SK-STEP
   loop
   CS-N @ 0<> if E-NELAB-CTRL throw then
   PATH-END @ PATH-EXIT = if E-NELAB-CTRL throw then
   PATH-DEAD? {: dead:bool :}
   EXIT-USED @ 0<> if
      dead if NB @ else NB @ 1+ then EXIT-ORD !
   then
   EXIT-USED @ 0<> if EXIT-ORD @ 1+ else NB @ then
   NFROZEN:BMAX > if E-NELAB-BLOCK throw then
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

: DO-CLOSE-IF ( n -- )
   {: ix:n :}
   HIR-CTRL:OPEN-IF CS-OPENER-CK {: t:n :}
   t CS-LB@ LOC-REST
   PATH-ENDED? {: armend:bool :}
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
   then
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
   VN @ d <> if E-NELAB-JOIN throw then
   RN @ rd <> if E-NELAB-JOIN throw then
   ix h TERM-BR
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
   VN @ d <> if E-NELAB-JOIN throw then
   RN @ rd <> if E-NELAB-JOIN throw then
   ix h TERM-BR
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
   h CS-TOP cells CS-HEAD + !
   st CS-TOP CS-IDX !
   lm CS-TOP CS-LIM !
   ix h  HEAD-CROSS-DO CROSS-L  TERM-BR-H
   ix  d CS-TOP CS-RD@ +  HEAD-CROSS-DO CROSS-L  OPEN-ARGS-H ;

: DO-OPEN-DO ( n -- )
   {: ix:n :}
   DO-PAIR {: st:IR-ID:ir-value-id lm:IR-ID:ir-value-id :}
   2 VDROP
   ix JOIN-OF JOIN-CK {: j:n :}
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

: DO-CLOSE-LOOP ( n -- )
   {: ix:n :}
   HIR-CTRL:OPEN-DO CS-OPENER-CK {: t:n :}
   t CS-LB@ LOC-REST
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
   CS-POP
   ix  d rd +  OPEN-ARGS
   NB @ j <> if E-NELAB-CTRL throw then ;

: DO-FRAME ( -- n )
   0 DO-INNER-NTH ;

: DO-INDEX ( -- )
   DO-FRAME CS-IDX @ VPUSH ;

: DO-OUTER-INDEX ( -- )
   1 DO-INNER-NTH CS-IDX @ VPUSH ;

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

\ A `MATCH` arm drops the tag and this many pads, which leaves the payload.
: ARM-GLUE ( n n n -- ) {: base:n k:n fields:n :}
   base k VGLUE-CLEAR
   k 2 < if exit then
   fields k = if exit then
   fields 1 = if base k VGLUE-GROW exit then
   E-NELAB-MATCH throw ;

: ARM-RESHAPE ( n n -- ) {: ix:n t:n :}
   t CS-MATCH? 0= if 1 VDROP exit then
   ix MPAD@ 1+ {: d:n :}
   d VN @ > if E-NELAB-UNDER throw then
   d VDROP
   t CS-DEPTH@ {: base:n :}
   base  VN @ base -  ix MFLD@  ARM-GLUE ;

\ The block a tag that matched no variant runs into, which is the last arm's
\ mismatch edge.
: MATCH-TRAP ( n n -- ) {: ix:n ord:n :}
   LIT-MARK {: m:n :}
   ix OPEN-PLAIN
   ix ord HIR:ADDR-NONE STAGE-LIT
   ix HIR-OPCODE:TRAP EMIT-OPCODE
   CLOSE-BLOCK
   m LIT-RELEASE ;

: DO-ARM ( n -- ) {: ix:n :}
   CS-N @ 1 < if E-NELAB-CTRL throw then
   CS-TOP {: t:n :}
   t CS-ADT? 0= if E-NELAB-CTRL throw then
   t CS-OFIX@ 0 >= if E-NELAB-CTRL throw then
   t ARM-WIDTH-CK
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
      t CS-JOINED+
   then
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
      VN @ t CS-DEPTH@ 1+ < if E-NELAB-UNDER throw then
      1 VDROP
      ix  t CS-JOIN@ JOIN-CK  TERM-BR
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

\ Every value this call site hands over goes into a DATA-STACK SLOT.
: CALL-CROSS ( n -- )
   {: ix:n :}
   ix VN @ CELL-CROSS
   ix R-CROSS ;

: R-OPERANDS+ ( -- )
   RN @ 0 ?do
      CTX BLD  i RAT  IR-BUILD:ADD-OPERAND
   loop ;

: CALL-OPERANDS+ ( -- )
   CALL-CROSS-CK
   NO-REAL-CK
   CTX BLD TOK IR-BUILD:ADD-OPERAND
   CROSS-DO LOOP-OPERANDS+
   CROSS-L LOCAL-OPERANDS+
   R-OPERANDS+
   VN @ 0 ?do
      CTX BLD  i VAT  IR-BUILD:ADD-OPERAND
   loop ;

: CROSS-RESULTS ( -- n )
   CROSS-N 2 *  CROSS-L +  RN @ + ;

: CALL-RESULTS+ ( n -- )
   {: n:n :}
   CTX BLD  CTX BLD HIR:MEM-TYPE  IR-BUILD:ADD-RESULT
   n CROSS-RESULTS +  0 ?do
      CTX BLD  CTX BLD CELL-TYPE  IR-BUILD:ADD-RESULT
   loop ;

: LOOP-RESULT@ ( IR-ID:ir-op-id n n -- )
   {: id:IR-ID:ir-op-id k:n t:n :}
   CTX BLD id  k 2 * 1+   IR-BUILD:OP-RESULT@  t CS-IDX !
   CTX BLD id  k 2 * 2 +  IR-BUILD:OP-RESULT@  t CS-LIM ! ;

: LOOP-RESULTS@ ( IR-ID:ir-op-id n n -- )
   {: id:IR-ID:ir-op-id lo:n h:n :}
   h 0 ?do  id i  lo i + DO-NTH  LOOP-RESULT@  loop ;

variable LRK                         \ crossing locals the walk below has taken back

: R-RESULTS@ ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   RN @ 0 ?do
      CTX BLD id  CROSS-N 2 * CROSS-L + i + 1+  IR-BUILD:OP-RESULT@  i RSTK !
   loop ;

: LOCAL-RESULTS@ ( IR-ID:ir-op-id n -- )
   {: id:IR-ID:ir-op-id l:n :}
   l LOCAL-CK 0= if exit then
   0 LRK !
   LBN @ 0 ?do
      i LSX@ if
         CTX BLD id  CROSS-N 2 * LRK @ + 1+  IR-BUILD:OP-RESULT@  i LVAL !
         LRK @ 1+ LRK !
      then
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
   id CROSS-DO LOOP-RESULTS@
   id CROSS-L LOCAL-RESULTS@
   id R-RESULTS@
   n 0 ?do
      CTX BLD id  i 1+ CROSS-RESULTS +  IR-BUILD:OP-RESULT@ VPUSH
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
   ix  ix WSYM DEAD-ORD  HIR:ADDR-NONE STAGE-LIT
   ix HIR-OPCODE:TRAP EMIT-OPCODE
   CLOSE-BLOCK
   PATH-DEAD PATH-END ! ;

\ A self-call empties the memo without asking.
: DO-SELF-CALL ( n -- )
   {: ix:n :}
   IN-N @ OUT-N @ CALL-LIVE  OUT-N @ + {: back:n :}
   ix CALL-CROSS
   CTX BLD HIR-OPCODE:CALL HIR:OPCODE {: op:IR-ID:ir-symbol-id :}
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
   CTX BLD HIR-OPCODE:WORDCALL HIR:OPCODE {: op:IR-ID:ir-symbol-id :}
   CTX BLD VW MKEY ix op OPEN
   CALL-OPERANDS+
   back CALL-RESULTS+
   entry a o WCALL-ATTRS+
   back o oglue CALL-CLOSE
   entry LIT-CALL-BARRIER ;

: DO-WORD-CALL ( IR-ARENA:arena n -- )
   {: r:IR-ARENA:arena ix:n :}
   ix WSYM {: sy:IR-ID:ir-symbol-id :}
   ix  r sy HIR-WORD:ENTRY@
   r sy HIR-WORD:CALLEE-IN@  r sy HIR-WORD:CALLEE-OUT@
   r sy HIR-WORD:OUT-GLUE@  STAGE-WCALL
   r sy HIR-WORD:CALLEE-DEAD? if r ix DEAD-END then ;

\ ---- copying a callee's body in instead of calling it ------------------------
\ A copied call leaves the same values a made one would.
: INLINE-NAME ( IR-ARENA:arena IR-ARENA:arena n IR-ID:ir-symbol-id -- )
   {: p:IR-ARENA:arena r:IR-ARENA:arena ix:n sy:IR-ID:ir-symbol-id :}
   r sy HIR-WORD:ADMIT SPLICE-STAGING
   MATCH staging
      call     OF E-NELAB-INLINE throw ENDOF
      op       OF r ix sy EMIT-OP-SYM ENDOF
      const-op OF r ix sy EMIT-CONST-OP-SYM ENDOF
      fixed    OF r ix sy EMIT-FIXED-SYM ENDOF
      rename   OF p r sy RENAME ENDOF
   ;MATCH ;

: INLINE-TOKEN ( IR-ARENA:arena IR-ARENA:arena n n n -- )
   {: p:IR-ARENA:arena r:IR-ARENA:arena ix:n entry:n k:n :}
   entry k NINL:KIND@
   MATCH NTAPE:kind
      name           OF p r ix  entry k INL-SYM  INLINE-NAME ENDOF
      int-literal    OF ix  entry k NINL:LIT@  EMIT-LIT ENDOF
      real-literal   OF ix  entry k NINL:LIT@  EMIT-FLIT ENDOF
      char-literal   OF E-NELAB-INLINE throw ENDOF
      string-literal OF E-NELAB-INLINE throw ENDOF
   ;MATCH ;

: DO-INLINE ( IR-ARENA:arena IR-ARENA:arena n -- )
   {: p:IR-ARENA:arena r:IR-ARENA:arena ix:n :}
   ix WSYM {: sy:IR-ID:ir-symbol-id :}
   r sy HIR-WORD:ENTRY@ {: entry:n :}
   entry NINL:IN@ {: a:n :}
   entry NINL:OUT@ {: o:n :}
   VN @ a < if E-NELAB-UNDER throw then
   VN @ a - {: base:n :}
   base o + VMAX > if E-NELAB-CAP throw then
   ix base a CELL-CROSS-RUN
   entry NINL:TOKENS 0 ?do
      p r ix entry i INLINE-TOKEN
      VN @ base < if E-NELAB-INLINE throw then
   loop
   VN @ base o + <> if E-NELAB-INLINE throw then
   ix base o CELL-CROSS-RUN
   base  r sy HIR-WORD:OUT-GLUE@  VGLUE-RUN ;

\ ---- what `[:` stages ---------------------------------------------------------
: DO-QUOT ( n -- )
   {: ix:n :}
   ix QOPENED@ {: k:n :}
   k 0 < if ix QUOT-REFUSE then
   CTX BLD HIR-OPCODE:QUOT HIR:OPCODE {: op:IR-ID:ir-symbol-id :}
   CTX BLD VW MKEY ix op OPEN
   CTX BLD op RESULTS+
   CTX BLD  CTX BLD HIR:KEY-FUN
   CTX BLD  k QFUN@  IR-BUILD:INTERN-INT-ATTR  IR-BUILD:ADD-ATTR
   CTX BLD op CLOSE
   k  VN @ 1-  VQ! ;

\ ---- binding a quotation to a deferred word -----------------------------------
: DO-IS ( n -- )
   {: ix:n :}
   VN @ 1 < if E-NELAB-UNDER throw then
   ix 1+ QSPELL {: a u:n :} \ typed-local-lint: allow-bare-local - a keeps the ptr u8 byte-span role
   a u NDICT:SPELL-DEFER-CELL {: cell:n :}
   cell 0= if E-NELAB-DEFER throw then
   a u NDICT:SPELL-ARITY {: din:n dout:n :}
   din NDICT:ARITY-NONE = if E-NELAB-DEFER throw then
   dout NDICT:ARITY-NONE = if E-NELAB-DEFER throw then
   s" xt!" NDICT:CALL-TARGET {: entry:n :}
   entry 0= if E-NELAB-DEFER throw then
   VN @ 1- VQ@ {: k:n :}
   k 0 >= if k din dout QFILL then
   ix cell EMIT-LIT
   ix entry 2 0 NDICT:GLUE-NONE STAGE-WCALL ;

\ ---- entering the routine a quotation names ----------------------------------
: DO-EXEC ( n -- )
   {: ix:n :}
   VN @ 1 < if E-NELAB-UNDER throw then
   VN @ 1- VQ@ {: k:n :}
   k 0 < if ix QUOT-REFUSE then
   k QIN@ {: qin:n :}
   k QOUT@ {: qout:n :}
   qin QNONE = if ix QUOT-REFUSE then
   s" execute" NDICT:CALL-TARGET {: entry:n :}
   entry 0= if ix QUOT-REFUSE then
   ix entry  qin 1+  qout  NDICT:GLUE-NONE  STAGE-WCALL ;

\ ---- running a quotation and coming back either way --------------------------
\ `catch` stages ONE call and opens no scope of its own.
: DO-CATCH ( n -- )
   {: ix:n :}
   VN @ 1 < if E-NELAB-UNDER throw then
   VN @ 1- VQ@ {: k:n :}
   k 0 < if ix QUOT-REFUSE then
   ix NDICT:CATCH-CELLS {: win:n back:n :}
   win NDICT:CATCH-NONE = if ix QUOT-REFUSE then
   back NDICT:CATCH-NONE = if ix QUOT-REFUSE then
   back win <> if ix QUOT-REFUSE then
   VN @ 1- win < if E-NELAB-UNDER throw then
   s" catch" NDICT:CALL-TARGET {: entry:n :}
   entry 0= if ix QUOT-REFUSE then
   k win win QFILL
   ix entry  win 1+  win 1+  NDICT:GLUE-NONE  STAGE-WCALL ;

\ ---- a call that BUILDS a value of a wide instantiation ------------------------
: CON-PADS-PUSH ( n n -- ) {: ix:n x:n :}
   x 0 ?do  ix 0 EMIT-LIT  loop ;

\ What came back is ONE value: the call answered its DECLARED bundle.
: CON-BUNDLE-GLUE ( n -- ) {: w:n :}
   w VN @ > if E-NELAB-UNDER throw then
   VN @ w -  w  VGLUE-GROW ;

: DO-CALL ( IR-ARENA:arena IR-ARENA:arena n -- )
   {: p:IR-ARENA:arena r:IR-ARENA:arena ix:n :}
   ix  r  ix WSYM  HIR-WORD:CALLEE-IN@  QCALL-FILL
   ix NDICT:CON-PADS {: x:n :}
   ix x CON-PADS-PUSH
   ix INL-AT? if p r ix DO-INLINE else r ix DO-WORD-CALL then
   x 0= if exit then
   r  ix WSYM  HIR-WORD:CALLEE-OUT@  x +  CON-BUNDLE-GLUE ;

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
   VN @ 1 < if E-NELAB-UNDER throw then
   VN @ 1- VAT {: addr:IR-ID:ir-value-id :}
   1 VDROP
   w VN @ > if E-NELAB-UNDER throw then
   VN @ w - {: bot:n :}
   bot w E-NELAB-BUNDLE BUNDLE-CK
   w 0 ?do
      bot i + VAT VPUSH
      ix addr i WIDE-ADDR
      ix HIR-OPCODE:STORE EMIT-OPCODE
   loop
   VN @ bot w + <> if E-NELAB-BUNDLE throw then
   w VDROP ;

: DO-OP ( IR-ARENA:arena n -- )
   {: r:IR-ARENA:arena ix:n :}
   VW ix TOK-CELLS {: w:n :}
   w 1 = if r ix EMIT-OP exit then
   w 1 < if E-NELAB-BUNDLE throw then
   r  ix WSYM  HIR-WORD:OPCODE@ {: k:HIR:opcode :}
   k HIR-OPCODE:LOAD HIR-OPCODE:EQ if ix w WIDE-LOAD exit then
   k HIR-OPCODE:STORE HIR-OPCODE:EQ if ix w WIDE-STORE exit then
   E-NELAB-BUNDLE throw ;

: DO-UNLOOP ( -- )
   DO-FRAME drop ;

\ The vector has to hold exactly the values the definition declares it leaves.
: DO-EXIT ( n -- )
   {: ix:n :}
   CS-N @ 1 < if E-NELAB-CTRL throw then
   CS-TOP CS-KIND @ HIR-CTRL:OPEN-IF HIR-CTRL:EQ 0= if E-NELAB-CTRL throw then
   VN @ OUT-N @ <> if E-NELAB-ARITY throw then
   EXIT-ORD @ 0 < if E-NELAB-CTRL throw then
   ix EXIT-ORD @ 0 0 0 TERM-BR-H
   PATH-EXIT PATH-END ! ;

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
   d LSLOT@ s <> if E-NELAB-LOCAL throw then
   base VAT  s LVAL !
   base VQ@  s LQ!
   d s LOWN!
   d LCROSS?  s LSX! ;

: DO-CLOSE-LOCALS ( n -- )
   {: ix:n :}
   LGB @ LG-N @ >= if E-NELAB-LOCAL throw then
   ix LGB @ LG-B@ <> if E-NELAB-LOCAL throw then
   LGB @ LG-K@ {: k:n :}
   LGB @ LG-F@ {: d0:n :}
   LBN @ {: from:n :}
   from k +  LMAX > if E-NELAB-LOCAL-CAP throw then
   k VN @ > if E-NELAB-UNDER throw then
   VN @ k - {: base:n :}
   k 0 ?do
      base i +  d0 i +  from i +  BIND-ONE
   loop
   k VDROP
   ix from k LOCAL-BIND-CROSS
   from k + LBN !
   LGB @ 1+ LGB ! ;

: LOCAL-READ? ( n -- bool )
   {: ix:n :}
   ix LOCAL-OF {: k:n :}
   k 0 < if false exit then
   k LSLOT@ {: s:n :}
   s 0 <  s LBN @ >=  or if E-NELAB-LOCAL throw then
   s LOWN@ k <> if E-NELAB-LOCAL throw then
   s LSAT LVAL @ VPUSH
   s LQ@  VN @ 1-  VQ!
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
      exec         OF ix DO-EXEC ENDOF
      catch        OF ix DO-CATCH ENDOF
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
   E-NELAB-CTRL throw ;

: STEP ( IR-ARENA:arena IR-ARENA:arena n -- )
   {: p:IR-ARENA:arena r:IR-ARENA:arena ix:n :}
   VW ix NTAPE-MODE:COMPILING MODE-CK
   ix QSKIP? if exit then
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
      callable     OF p r ix DO-CALL ENDOF
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

: QNAME+ ( ptr u8 n -- )
   {: a u:n :} \ typed-local-lint: allow-bare-local - a keeps the ptr u8 byte-span role
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
   s" [:" QNAME+
   k QNAME-DIGITS
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

: OPEN-BLOCK ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:view IR-ID:ir-module-key n -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder v:IR-ARENA:view key:IR-ID:ir-module-key
      in:n :}
   c b IR-BUILD:BEGIN-BLOCK
   c b  v key 0 NTAPE:SPAN@  IR-BUILD:SET-BLOCK-SPAN
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
   c b HIR-OPCODE:RETURN HIR:OPCODE {: op:IR-ID:ir-symbol-id :}
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
   PATH-END @ PATH-EXIT = if E-NELAB-CTRL throw then
   EXIT-USED @ 0<> if k QAT@ QUOT-REFUSE then
   PATH-DEAD? if k QAT@ QUOT-REFUSE then
   c b v key k QEMIT-RETURN
   CLOSE-HELD
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

EXPORT SPLICE-STAGING
EXPORT SPLICE-MEANING?

\ The rule a caller asks before it copies a token, read off the one table.
: SPLICEABLE? ( IR-ARENA:view IR-ARENA:arena n IR-ID:ir-symbol-id -- bool )
   {: v:IR-ARENA:view r:IR-ARENA:arena ix:n sy:IR-ID:ir-symbol-id :}
   v ix NTAPE:KIND@ {: kd:NTAPE:kind :}
   kd NTAPE-KIND:INT-LITERAL NTAPE-KIND:EQ if true exit then
   kd NTAPE-KIND:REAL-LITERAL NTAPE-KIND:EQ if true exit then
   kd NTAPE-KIND:NAME NTAPE-KIND:EQ 0= if false exit then
   r sy HIR-WORD:MODELS? 0= if false exit then
   v ix TOK-CELLS 1 <> if false exit then
   r sy HIR-WORD:MEANING@ SPLICE-MEANING? ;

\ ---- what a recorder has to be told about a call -----------------------------

: COPIED? ( n -- bool )
   INL-AT? ;

\ The address the row it was copied from is keyed by.
: COPIED-ENTRY ( IR-ARENA:arena n -- n )
   {: r:IR-ARENA:arena ix:n :}
   ix INL-AT? 0= if E-NELAB-INLINE throw then
   r  ix WSYM  HIR-WORD:ENTRY@ ;

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

: BACK-SCAN ( IR-ARENA:arena n -- )
   {: r:IR-ARENA:arena n:n :}
   0 CALL-BACK !
   n 1 ?do
      r i BACK-CALL? if 1 CALL-BACK ! leave then
   loop ;

: TAIL-SCAN ( IR-ARENA:arena n -- )
   {: r:IR-ARENA:arena n:n :}
   0 TAIL-NEED !
   0 TAIL-ENTRY !
   r n BACK-SCAN
   IN-N @ OUT-N @ <> if exit then
   IN-N @ 0= if exit then
   EXIT-USED @ 0<> if exit then
   NB @ 0<> if exit then
   n 2 < if exit then
   r n 1- WORD-CALL? 0= if exit then
   r n 1- TAIL-CALLEE? 0= if exit then
   1 TAIL-NEED !
   r  n 1- WSYM  HIR-WORD:ENTRY@ TAIL-ENTRY !
   0 CALL-BACK !
   n 1- 1 ?do
      r i BACK-CALL? if 1 CALL-BACK ! leave then
   loop ;

\ A definition whose own rows cannot be segmented is not compiled.
: FRAME-GLUE! ( n n -- )
   {: gin:n gout:n :}
   gin FR-GIN !
   gout FR-GOUT ! ;

: COLON ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:view IR-ARENA:arena IR-ARENA:arena n n -- IR-ID:ir-fun-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder v:IR-ARENA:view p:IR-ARENA:arena
      r:IR-ARENA:arena in:n out:n :}
   RF-RESET
   in out ARITY-CK
   b IR-BUILD:MODULE-KEY {: key:IR-ID:ir-module-key :}
   c 0 S-CTX !
   b 0 S-BLD !
   v 0 S-VW !
   key 0 S-KEY !
   v NTAPE:TOKENS {: n:n :}
   n 1 < if E-NELAB-SHAPE throw then
   v NAME-READ
   TOK-RESET
   in IN-N !
   out OUT-N !
   FR-GIN @ IN-GLUE !  FR-GOUT @ OUT-GLUE !
   0 FR-GIN !  0 FR-GOUT !
   IN-GLUE @ NDICT:GLUE-UNKNOWN = if E-NELAB-BUNDLE throw then
   OUT-GLUE @ NDICT:GLUE-UNKNOWN = if E-NELAB-BUNDLE throw then
   b IR-BUILD:FUNS QBASE !
   r n QUOT-SCAN
   r n LOCALS-SCAN
   QLOCALS-CK
   r n MATCH-SCAN
   r n DEFER-SCAN
   r n RESOLVE-SCAN
   r n INLINE-SCAN
   r n MEM-SCAN
   r n CROSS-SCAN
   r 1 n SKELETON-TRY
   b FUN-STATE!
   c b v key in out OPEN-FUN
   c b v key in OPEN-BLOCK
   TOK-NEED @ 0<> if 0 EMIT-MEM then
   PATH-LIVE PATH-END !
   p r 1 n WALK-TRY
   CS-N @ 0<> if E-NELAB-CTRL throw then
   PATH-END @ PATH-EXIT = if E-NELAB-CTRL throw then
   PATH-DEAD? {: dead:bool :}
   EXIT-USED @ 0<> if
      dead 0= if
         VN @ out <> if E-NELAB-ARITY throw then
         0 EXIT-ORD @ 0 0 0 TERM-BR-H
      then
      NB @ EXIT-ORD @ <> if E-NELAB-CTRL throw then
      0 out 0 0 0 OPEN-ARGS-H
   then
   dead  EXIT-USED @ 0=  and 0= if
      out QRET-FILL
      c b v key out EMIT-RETURN
      CLOSE-HELD
   then
   r n TAIL-SCAN
   QCONSUMED-CK
   c b IR-BUILD:END-FUN {: f:IR-ID:ir-fun-id :}
   c b v key p r QBUILD-ALL
   f ;

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
