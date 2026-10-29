\ fun.f - the append-only function and block store: one row per function, one
\ row per basic block, and a cell pool of function attribute windows. Functions
\ and blocks share a package because each row reads the other table's live count.
\ Identities are IR-ID's ir-fun-id and ir-block-id; operation meaning is the
\ schema table's.

require src/lib/prelude.f
require src/lib/errors.f
require src/compiler/target.f
require src/compiler/binding.f
require src/compiler/ir/id.f
require src/compiler/ir/context.f
require src/compiler/ir/arena.f
require src/compiler/ir/source.f
require src/compiler/ir/symbol.f
require src/compiler/ir/type.f
require src/compiler/ir/attr.f
require src/compiler/ir/schema.f
require src/compiler/ir/op.f

package IR-FUN
public

\ The wire codes below persist it.
ENUM linkage DERIVE eq
   defined
   replaceable
   imported
;ENUM

ENUM visibility DERIVE eq
   hidden
   exported
;ENUM

ENUM convention DERIVE eq
   habu
   c-abi
   kernel
   wasm
;ENUM

private

\ One-way projections of IR-ID identities onto serials; nothing here re-mints.
CAST: KEY-SERIAL ( IR-ID:ir-module-key -- n )
CAST: MID-SERIAL ( IR-ID:ir-module-id -- n )

\ ---- layout ------------------------------------------------------------------
$464E5031 constant FNP-MAGIC         \ "FNP1": the attribute-pool header tag
$464E5231 constant FNR-MAGIC         \ "FNR1": the function-table header tag
$424C5231 constant BLR-MAGIC         \ "BLR1": the block-table header tag

0 constant HC-MAGIC
1 constant HC-SERIAL
2 constant HC-CAP
3 constant HDR-CELLS

0 constant OFF-SYM                   \ the symbol name
1 constant OFF-SIG                   \ the signature type
2 constant OFF-BST                   \ first-block
3 constant OFF-BN                    \ block-count
4 constant OFF-ATST                  \ the attribute window
5 constant OFF-ATN
6 constant OFF-SRC                   \ the source span
7 constant OFF-SBEG
8 constant OFF-SLEN
9 constant OFF-LNK                   \ linkage
10 constant OFF-VIS                  \ the visibility half of the same question
11 constant OFF-CC                   \ the calling convention
12 constant FNROW-CELLS

0 constant OFF-PAR                   \ the parent function
1 constant OFF-AGST                  \ the argument window
2 constant OFF-AGN
3 constant OFF-OPST                  \ the operation window
4 constant OFF-OPN
5 constant OFF-TERM                  \ the terminator operation
6 constant OFF-BSRC                  \ the source span
7 constant OFF-BBEG
8 constant OFF-BLEN
9 constant BROW-CELLS

public
$FFFFFFFF HDR-CELLS - FNROW-CELLS / constant FNROW-CAP-MAX
$FFFFFFFF HDR-CELLS - BROW-CELLS / constant BLK-CAP-MAX
$FFFFFFFF HDR-CELLS - constant POOL-CAP-MAX
private
32 constant ATTR-MAX                 \ committed staged attributes per function

\ ---- stable wire codes -------------------------------------------------------
\ One injective code per family; a code may be added but never renumbered
\ without a schema bump.
0 constant LK-DEFINED
1 constant LK-REPLACEABLE
2 constant LK-IMPORTED
0 constant VS-HIDDEN
1 constant VS-EXPORTED
0 constant CV-HABU
1 constant CV-C
2 constant CV-KERNEL
3 constant CV-WASM

: LNK-CODE ( IR-FUN:linkage -- n )
   MATCH linkage
      defined     OF LK-DEFINED ENDOF
      replaceable OF LK-REPLACEABLE ENDOF
      imported    OF LK-IMPORTED ENDOF
   ;MATCH ;

: VIS-CODE ( IR-FUN:visibility -- n )
   MATCH visibility
      hidden   OF VS-HIDDEN ENDOF
      exported OF VS-EXPORTED ENDOF
   ;MATCH ;

: CC-CODE ( IR-FUN:convention -- n )
   MATCH convention
      habu   OF CV-HABU ENDOF
      c-abi  OF CV-C ENDOF
      kernel OF CV-KERNEL ENDOF
      wasm   OF CV-WASM ENDOF
   ;MATCH ;

\ A stored code outside the family's vocabulary is a corrupted or forged row, so
\ each decoder rejects it named instead of reading as some other member.
: N>LNK ( n -- IR-FUN:linkage )
   case
      LK-DEFINED     of IR--FUN-LINKAGE:DEFINED endof
      LK-REPLACEABLE of IR--FUN-LINKAGE:REPLACEABLE endof
      LK-IMPORTED    of IR--FUN-LINKAGE:IMPORTED endof
      E-IR-FUN-STATE throw
   endcase ;

: N>VIS ( n -- IR-FUN:visibility )
   case
      VS-HIDDEN   of IR--FUN-VISIBILITY:HIDDEN endof
      VS-EXPORTED of IR--FUN-VISIBILITY:EXPORTED endof
      E-IR-FUN-STATE throw
   endcase ;

: N>CC ( n -- IR-FUN:convention )
   case
      CV-HABU   of IR--FUN-CONVENTION:HABU endof
      CV-C      of IR--FUN-CONVENTION:C-ABI endof
      CV-KERNEL of IR--FUN-CONVENTION:KERNEL endof
      CV-WASM   of IR--FUN-CONVENTION:WASM endof
      E-IR-FUN-STATE throw
   endcase ;

\ ---- cell access -------------------------------------------------------------
\ Each store is resolved once at the public word into a reader, which refuses
\ the wrong live/frozen state itself: fnp the attribute pool, fnr the function
\ table, blr the blocks.

\ ---- headers and shape -------------------------------------------------------
: PSHAPE-CK ( n -- )
   HDR-CELLS < if E-IR-FUN-STATE throw then ;

: FNSHAPE-CK ( n -- )
   dup HDR-CELLS < if E-IR-FUN-STATE throw then
   HDR-CELLS - FNROW-CELLS mod 0 <> if E-IR-FUN-STATE throw then ;

: BSHAPE-CK ( n -- )
   dup HDR-CELLS < if E-IR-FUN-STATE throw then
   HDR-CELLS - BROW-CELLS mod 0 <> if E-IR-FUN-STATE throw then ;

: MAGIC-CK ( n n -- )
   <> if E-IR-FUN-STATE throw then ;

: PHDR-CK ( IR-ARENA:reader -- )
   {: fnp:IR-ARENA:reader :}
   fnp IR-ARENA:RD-SIZE PSHAPE-CK
   FNP-MAGIC fnp HC-MAGIC IR-ARENA:RD@ MAGIC-CK ;

: FNHDR-CK ( IR-ARENA:reader -- )
   {: fnr:IR-ARENA:reader :}
   fnr IR-ARENA:RD-SIZE FNSHAPE-CK
   FNR-MAGIC fnr HC-MAGIC IR-ARENA:RD@ MAGIC-CK ;

: BHDR-CK ( IR-ARENA:reader -- )
   {: blr:IR-ARENA:reader :}
   blr IR-ARENA:RD-SIZE BSHAPE-CK
   BLR-MAGIC blr HC-MAGIC IR-ARENA:RD@ MAGIC-CK ;

: FNCNT ( IR-ARENA:reader -- n )
   dup FNHDR-CK IR-ARENA:RD-SIZE HDR-CELLS - FNROW-CELLS / ;

: BCNT ( IR-ARENA:reader -- n )
   dup BHDR-CK IR-ARENA:RD-SIZE HDR-CELLS - BROW-CELLS / ;

: PCELLS ( IR-ARENA:reader -- n )
   dup PHDR-CK IR-ARENA:RD-SIZE HDR-CELLS - ;

\ ---- ownership ---------------------------------------------------------------
: SERIAL-CK ( n n -- )
   <> if E-IR-FUN-OWNER throw then ;

\ A cross-module trio rejects before a row window is trusted against the pool.
: TRIO-CK ( IR-ARENA:reader IR-ARENA:reader IR-ARENA:reader -- )
   {: fnp:IR-ARENA:reader fnr:IR-ARENA:reader blr:IR-ARENA:reader :}
   fnp PHDR-CK
   fnr FNHDR-CK
   blr BHDR-CK
   fnp HC-SERIAL IR-ARENA:RD@ fnr HC-SERIAL IR-ARENA:RD@ SERIAL-CK
   blr HC-SERIAL IR-ARENA:RD@ fnr HC-SERIAL IR-ARENA:RD@ SERIAL-CK ;

: FNKEY-CK ( IR-ARENA:reader IR-ID:ir-module-key -- )
   {: fnr:IR-ARENA:reader key:IR-ID:ir-module-key :}
   fnr FNHDR-CK
   fnr HC-SERIAL IR-ARENA:RD@ key KEY-SERIAL SERIAL-CK ;

: BKEY-CK ( IR-ARENA:reader IR-ID:ir-module-key -- )
   {: blr:IR-ARENA:reader key:IR-ID:ir-module-key :}
   blr BHDR-CK
   blr HC-SERIAL IR-ARENA:RD@ key KEY-SERIAL SERIAL-CK ;

: BFN-CK ( IR-ARENA:reader IR-ARENA:reader IR-ID:ir-module-key -- )
   {: blr:IR-ARENA:reader fnr:IR-ARENA:reader key:IR-ID:ir-module-key :}
   blr key BKEY-CK
   fnr key FNKEY-CK ;

\ ---- identity projections ----------------------------------------------------
: SYM-ORD ( IR-ID:ir-symbol-id -- n )
   IR-ID:SYMBOL-LOCAL ;

: SYM-OWNER ( IR-ID:ir-symbol-id -- n )
   IR-ID:SYMBOL-OWNER MID-SERIAL ;

: TYP-OWNER ( IR-ID:ir-type-id -- n )
   IR-ID:TYPE-OWNER MID-SERIAL ;

: ATTR-OWNER ( IR-ID:ir-attr-id -- n )
   IR-ID:ATTR-OWNER MID-SERIAL ;

: SRC-OWNER ( IR-ID:ir-source-id -- n )
   IR-ID:SOURCE-OWNER MID-SERIAL ;

\ Non-negative only: the owning table revalidates the id it is handed.
: ORD-OK ( n -- n )
   dup 0 < if E-IR-FUN-STATE throw then ;

: LEN-OK ( n -- n )
   dup 0 < if E-IR-FUN-STATE throw then ;

: STEP-CK ( n n -- )
   <> if E-IR-FUN-WINDOW throw then ;

: ROW-ORD ( n n IR-ID:ir-fun-id -- n )
   {: hs:n cnt:n id:IR-ID:ir-fun-id :}
   id IR-ID:FUN-OWNER MID-SERIAL hs SERIAL-CK
   id IR-ID:FUN-LOCAL
   dup 0 < over cnt >= or if E-IR-FUN-BOUND throw then ;

: BLK-ORD ( n n IR-ID:ir-block-id -- n )
   {: hs:n cnt:n id:IR-ID:ir-block-id :}
   id IR-ID:BLOCK-OWNER MID-SERIAL hs SERIAL-CK
   id IR-ID:BLOCK-LOCAL
   dup 0 < over cnt >= or if E-IR-FUN-BOUND throw then ;

: FNROW-AT ( IR-ARENA:reader IR-ID:ir-fun-id -- n )
   {: fnr:IR-ARENA:reader id:IR-ID:ir-fun-id :}
   fnr HC-SERIAL IR-ARENA:RD@ fnr FNCNT id ROW-ORD ;

: BROW-AT ( IR-ARENA:reader IR-ID:ir-block-id -- n )
   {: blr:IR-ARENA:reader id:IR-ID:ir-block-id :}
   blr HC-SERIAL IR-ARENA:RD@ blr BCNT id BLK-ORD ;

\ ---- row and pool addressing -------------------------------------------------
: FNROW-CELL ( n n -- n )
   swap FNROW-CELLS * HDR-CELLS + + ;

: BROW-CELL ( n n -- n )
   swap BROW-CELLS * HDR-CELLS + + ;

: FNC@ ( IR-ARENA:reader n n -- n )
   FNROW-CELL IR-ARENA:RD@ ;

: BC@ ( IR-ARENA:reader n n -- n )
   BROW-CELL IR-ARENA:RD@ ;

\ ---- the window tilings ------------------------------------------------------
\ A block's operation windows tile IR-OP's operation table, so each operation has
\ exactly one parent block with no back-reference; BEGIN-BLOCK captures the
\ operation count, so an operation appended outside a block leaves a gap the
\ tiling rejects. Block arguments are IR-OP values whose rows name their block
\ and position; the argument run need not tile the value table.
\ The upper bound belongs to the block table; readers that hold it check it.
: FNBK-END ( IR-ARENA:reader n -- n )
   {: fnr:IR-ARENA:reader l:n :}
   fnr l OFF-BST FNC@ LEN-OK  fnr l OFF-BN FNC@ LEN-OK + ;

: BTILE-CK ( IR-ARENA:reader n -- )
   {: fnr:IR-ARENA:reader l:n :}
   l 0= if 0 else fnr l 1- FNBK-END then
   fnr l OFF-BST FNC@ STEP-CK ;

\ The upper bound belongs to IR-OP's operation table.
: BOP-END ( IR-ARENA:reader n -- n )
   {: blr:IR-ARENA:reader l:n :}
   blr l OFF-OPST BC@ LEN-OK  blr l OFF-OPN BC@ LEN-OK + ;

: OTILE-CK ( IR-ARENA:reader n -- )
   {: blr:IR-ARENA:reader l:n :}
   l 0= if 0 else blr l 1- BOP-END then
   blr l OFF-OPST BC@ STEP-CK ;

\ ---- the staged function and block -------------------------------------------
\ The block stage nests inside the function stage, so an orphan block is
\ unrepresentable. Any end consumes its stage.
0 constant MODE-NONE
1 constant MODE-OPEN
-1 constant UNSET

align
0 CWORK-VARIABLE FSTG-MODE n
0 CWORK-VARIABLE FSTG-SYM n
0 CWORK-VARIABLE FSTG-SYMO n
0 CWORK-VARIABLE FSTG-SIG n
0 CWORK-VARIABLE FSTG-SIGO n
0 CWORK-VARIABLE FSTG-BST n
0 CWORK-VARIABLE FSTG-SRC n
0 CWORK-VARIABLE FSTG-SRCO n
0 CWORK-VARIABLE FSTG-SBEG n
0 CWORK-VARIABLE FSTG-SLEN n
0 CWORK-VARIABLE FSTG-LNK n
0 CWORK-VARIABLE FSTG-VIS n
0 CWORK-VARIABLE FSTG-CC n
0 CWORK-VARIABLE FSTG-ATN n
ATTR-MAX CWORK-SPAN FSTG-AT n
ATTR-MAX CWORK-SPAN FSTG-ATO n

0 CWORK-VARIABLE BSTG-MODE n
0 CWORK-VARIABLE BSTG-OPST n
0 CWORK-VARIABLE BSTG-AGST n
0 CWORK-VARIABLE BSTG-AGN n
0 CWORK-VARIABLE BSTG-SRC n
0 CWORK-VARIABLE BSTG-SRCO n
0 CWORK-VARIABLE BSTG-SBEG n
0 CWORK-VARIABLE BSTG-SLEN n

: AT@ ( n -- n )
   cells FSTG-AT + @ ;

: AT! ( n n -- )
   cells FSTG-AT + ! ;

: ATO@ ( n -- n )
   cells FSTG-ATO + @ ;

: ATO! ( n n -- )
   cells FSTG-ATO + ! ;

: FSTG-RESET ( -- )
   UNSET FSTG-SIG !  0 FSTG-SIGO !
   UNSET FSTG-SRC !  0 FSTG-SRCO !  0 FSTG-SBEG !  0 FSTG-SLEN !
   UNSET FSTG-LNK !  UNSET FSTG-VIS !  UNSET FSTG-CC !
   0 FSTG-ATN ! ;

: BSTG-RESET ( -- )
   0 BSTG-AGN !  UNSET BSTG-AGST !
   UNSET BSTG-SRC !  0 BSTG-SRCO !  0 BSTG-SBEG !  0 BSTG-SLEN ! ;

: FSTG-OPEN-CK ( -- )
   FSTG-MODE @ MODE-OPEN <> if E-IR-FUN-STAGE throw then ;

: BSTG-OPEN-CK ( -- )
   BSTG-MODE @ MODE-OPEN <> if E-IR-FUN-STAGE throw then ;

: FSTG-TAKE ( -- )
   FSTG-MODE @ {: have:n :}
   MODE-NONE FSTG-MODE !
   have MODE-OPEN <> if E-IR-FUN-STAGE throw then ;

: BSTG-TAKE ( -- )
   BSTG-MODE @ {: have:n :}
   MODE-NONE BSTG-MODE !
   have MODE-OPEN <> if E-IR-FUN-STAGE throw then ;

: ONCE-CK ( n -- )
   UNSET <> if E-IR-FUN-STAGE throw then ;

: ATTR+ ( IR-ID:ir-attr-id -- )
   {: a:IR-ID:ir-attr-id :}
   FSTG-ATN @ ATTR-MAX >= if E-IR-FUN-CAP throw then
   FSTG-ATN @ {: n:n :}
   a IR-ID:ATTR-LOCAL n AT!
   a ATTR-OWNER n ATO!
   n 1+ FSTG-ATN ! ;

\ ---- staged validation -------------------------------------------------------
\ Every check runs before the first IR-ARENA:PUSH; ADD-BLOCK-ARG is the one
\ earlier mutation.
: OWNED-CK ( n n -- )
   {: hs:n owner:n :}
   owner hs SERIAL-CK ;

: FNFIELDS-CK ( -- )
   FSTG-SIG @ UNSET = if E-IR-FUN-FIELD throw then
   FSTG-SRC @ UNSET = if E-IR-FUN-FIELD throw then
   FSTG-LNK @ UNSET = if E-IR-FUN-FIELD throw then
   FSTG-VIS @ UNSET = if E-IR-FUN-FIELD throw then
   FSTG-CC @ UNSET = if E-IR-FUN-FIELD throw then ;

: BFIELDS-CK ( -- )
   BSTG-SRC @ UNSET = if E-IR-FUN-FIELD throw then ;

\ One module's function table defines a name at most once.
: DUP-CK ( IR-ARENA:reader -- )
   {: fnr:IR-ARENA:reader :}
   fnr FNCNT 0 ?do
      fnr i OFF-SYM FNC@ FSTG-SYM @ = if E-IR-FUN-DUP throw then
   loop ;

: SIG-CK ( IR-ARENA:arena IR-ID:ir-module-key -- )
   {: tr:IR-ARENA:arena key:IR-ID:ir-module-key :}
   key KEY-SERIAL FSTG-SIGO @ OWNED-CK
   tr  key FSTG-SIG @ IR-ID:PACK-TYPE  IR-TYPE:KIND@
   IR--TYPE-KIND:CODE-REF IR--TYPE-KIND:EQ
   0= if E-IR-FUN-SIG throw then ;

: ATTRS-CK ( IR-ARENA:arena IR-ID:ir-module-key -- )
   {: ar:IR-ARENA:arena key:IR-ID:ir-module-key :}
   FSTG-ATN @ 0 ?do
      key KEY-SERIAL i ATO@ OWNED-CK
      ar  key i AT@ IR-ID:PACK-ATTR  IR-ATTR:KIND@ drop
   loop ;

: FNSPAN-CK ( IR-ARENA:arena IR-ID:ir-module-key -- )
   {: sa:IR-ARENA:arena key:IR-ID:ir-module-key :}
   key KEY-SERIAL FSTG-SRCO @ OWNED-CK
   sa  key FSTG-SRC @ IR-ID:PACK-SOURCE FSTG-SBEG @ FSTG-SLEN @ IR--SOURCE-SPAN:MAKE
   IR-SOURCE:SPAN-CK ;

: BSPAN-CK ( IR-ARENA:arena IR-ID:ir-module-key -- )
   {: sa:IR-ARENA:arena key:IR-ID:ir-module-key :}
   key KEY-SERIAL BSTG-SRCO @ OWNED-CK
   sa  key BSTG-SRC @ IR-ID:PACK-SOURCE BSTG-SBEG @ BSTG-SLEN @ IR--SOURCE-SPAN:MAKE
   IR-SOURCE:SPAN-CK ;

\ An imported function has no body and must be exported; a replaceable one must
\ be exported; a function with a body needs an entry block.
: BODY-CK ( n -- )
   {: bn:n :}
   FSTG-LNK @ LK-IMPORTED = if
      bn 0 <> if E-IR-FUN-LINKAGE throw then
      FSTG-VIS @ VS-EXPORTED <> if E-IR-FUN-LINKAGE throw then
      exit
   then
   bn 1 < if E-IR-FUN-LINKAGE throw then
   FSTG-LNK @ LK-REPLACEABLE = FSTG-VIS @ VS-EXPORTED <> and
   if E-IR-FUN-LINKAGE throw then ;

\ Habu and C are native, kernel is PTX, Wasm has its own Habu-cell convention.
: NATIVE? ( CTARGET:arch -- bool )
   {: a:CTARGET:arch :}
   a CTARGET-ARCH:AARCH64 CTARGET-ARCH:EQ
   a CTARGET-ARCH:A32 CTARGET-ARCH:EQ or
   a CTARGET-ARCH:THUMB2 CTARGET-ARCH:EQ or
   a CTARGET-ARCH:C66X CTARGET-ARCH:EQ or
   a CTARGET-ARCH:X86-64 CTARGET-ARCH:EQ or ;

: TARGET-CK ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c IR-CTX:BINDING@ CBIND:TARGET@ CTARGET:ARCH@ {: a:CTARGET:arch :}
   FSTG-CC @ case
      CV-HABU   of a NATIVE? endof
      CV-C      of a NATIVE? endof
      CV-KERNEL of a CTARGET-ARCH:PTX CTARGET-ARCH:EQ endof
      CV-WASM   of a CTARGET-ARCH:WASM CTARGET-ARCH:EQ endof
      false swap
   endcase
   0= if E-IR-FUN-TARGET throw then ;

\ Makes the stored block window and each block's parent field agree.
: PARENTS-CK ( IR-ARENA:reader n n n -- )
   {: blr:IR-ARENA:reader l:n bst:n bn:n :}
   bn 0 ?do
      blr bst i + OFF-PAR BC@ l <> if E-IR-FUN-PARENT throw then
   loop ;

\ ---- block-side staged validation --------------------------------------------
\ Exactly one terminator, and it is the last operation.
: TERM-AT-CK ( IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key n n n -- )
   {: r:IR-ARENA:arena qr:IR-ARENA:arena key:IR-ID:ir-module-key opst:n i:n last:n :}
   qr  r key  key opst i + IR-ID:PACK-OP  IR-OP:OPCODE@  IR-SCHEMA:TERMINATOR?
   {: t:bool :}
   t if
      i last <> if E-IR-FUN-TERM throw then
      exit
   then
   i last = if E-IR-FUN-TERM throw then ;

: TERM-CK ( IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key n n -- )
   {: r:IR-ARENA:arena qr:IR-ARENA:arena key:IR-ID:ir-module-key opst:n opn:n :}
   opn 1 < if E-IR-FUN-TERM throw then
   opn 0 ?do
      r qr key opst i opn 1- TERM-AT-CK
   loop ;

\ Two blocks cannot share an argument; an argument minted after the block's
\ first operation breaks the run and lands here.
: ARGS-CK ( IR-ARENA:arena IR-ID:ir-module-key n n n -- )
   {: v:IR-ARENA:arena key:IR-ID:ir-module-key l:n agst:n agn:n :}
   agn 0 ?do
      key agst i + IR-ID:PACK-VALUE {: id:IR-ID:ir-value-id :}
      v id IR-OP:VALUE-KIND@ IR--OP-DEF--KIND:BLK-ARG IR--OP-DEF--KIND:EQ
      0= if E-IR-FUN-ARG throw then
      v key id IR-OP:VALUE-BLOCK@ IR-ID:BLOCK-LOCAL l <> if E-IR-FUN-ARG throw then
      v id IR-OP:VALUE-ARG@ i <> if E-IR-FUN-ARG throw then
   loop ;

\ ---- room and append ---------------------------------------------------------
: FNROOM-CK ( IR-ARENA:reader IR-ARENA:reader -- )
   {: fnp:IR-ARENA:reader fnr:IR-ARENA:reader :}
   fnr FNCNT fnr HC-CAP IR-ARENA:RD@ >= if E-IR-FUN-CAP throw then
   fnp PCELLS FSTG-ATN @ + fnp HC-CAP IR-ARENA:RD@ > if E-IR-FUN-CAP throw then ;

\ Both the pool and the row are reserved before either is written, after the
\ room check so this store's capacity error comes first. Open readers survive a
\ RESERVE: they re-read the row's pointer and count on every call.
: FNROOM-TAKE ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena -- )
   {: c:IR-CTX:ctx p:IR-ARENA:arena f:IR-ARENA:arena :}
   c p FSTG-ATN @ IR-ARENA:RESERVE
   c f FNROW-CELLS IR-ARENA:RESERVE ;

: BROOM-TAKE ( IR-CTX:ctx IR-ARENA:arena -- )
   {: c:IR-CTX:ctx b:IR-ARENA:arena :}
   c b BROW-CELLS IR-ARENA:RESERVE ;

: BROOM-CK ( IR-ARENA:reader -- )
   {: blr:IR-ARENA:reader :}
   blr BCNT blr HC-CAP IR-ARENA:RD@ >= if E-IR-FUN-CAP throw then ;

: CELL+ ( IR-CTX:ctx IR-ARENA:arena n -- )
   IR-ARENA:PUSH drop ;

: ATTRS-ADD ( IR-CTX:ctx IR-ARENA:arena -- )
   {: c:IR-CTX:ctx p:IR-ARENA:arena :}
   FSTG-ATN @ 0 ?do
      c p i AT@ CELL+
   loop ;

: FNROW-ADD ( IR-CTX:ctx IR-ARENA:arena n n n -- )
   {: c:IR-CTX:ctx f:IR-ARENA:arena atst:n bst:n bn:n :}
   c f FSTG-SYM @ CELL+
   c f FSTG-SIG @ CELL+
   c f bst CELL+   c f bn CELL+
   c f atst CELL+  c f FSTG-ATN @ CELL+
   c f FSTG-SRC @ CELL+
   c f FSTG-SBEG @ CELL+
   c f FSTG-SLEN @ CELL+
   c f FSTG-LNK @ CELL+
   c f FSTG-VIS @ CELL+
   c f FSTG-CC @ CELL+ ;

: BROW-ADD ( IR-CTX:ctx IR-ARENA:arena n n n n -- )
   {: c:IR-CTX:ctx b:IR-ARENA:arena par:n agst:n opst:n opn:n :}
   c b par CELL+
   c b agst CELL+  c b BSTG-AGN @ CELL+
   c b opst CELL+  c b opn CELL+
   c b opst opn + 1- CELL+
   c b BSTG-SRC @ CELL+
   c b BSTG-SBEG @ CELL+
   c b BSTG-SLEN @ CELL+ ;

\ ---- creation checks ---------------------------------------------------------
: FNROW-CAP-OK ( n -- )
   dup 1 < over FNROW-CAP-MAX > or if E-IR-FUN-CAP throw then
   drop ;

: BLK-CAP-OK ( n -- )
   dup 0 < over BLK-CAP-MAX > or if E-IR-FUN-CAP throw then
   drop ;

: POOL-CAP-OK ( n -- )
   dup 0 < over POOL-CAP-MAX > or if E-IR-FUN-CAP throw then
   drop ;

public

\ ---- creation ----------------------------------------------------------------
\ All three headers are bound to key's module serial; the stores die with the
\ owning context.
: NEW ( IR-CTX:ctx IR-ID:ir-module-key n n n -- IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena )
   {: c:IR-CTX:ctx key:IR-ID:ir-module-key fcap:n bcap:n pcap:n :}
   fcap FNROW-CAP-OK
   bcap BLK-CAP-OK
   pcap POOL-CAP-OK
   c pcap HDR-CELLS + IR-ARENA:NEW {: p:IR-ARENA:arena :}
   c p HDR-CELLS IR-ARENA:RESERVE
   c p FNP-MAGIC CELL+
   c p key KEY-SERIAL CELL+
   c p pcap CELL+
   c fcap FNROW-CELLS * HDR-CELLS + IR-ARENA:NEW {: f:IR-ARENA:arena :}
   c f HDR-CELLS IR-ARENA:RESERVE
   c f FNR-MAGIC CELL+
   c f key KEY-SERIAL CELL+
   c f fcap CELL+
   c bcap BROW-CELLS * HDR-CELLS + IR-ARENA:NEW {: b:IR-ARENA:arena :}
   c b HDR-CELLS IR-ARENA:RESERVE
   c b BLR-MAGIC CELL+
   c b key KEY-SERIAL CELL+
   c b bcap CELL+
   p f b ;

\ ---- the function builder ----------------------------------------------------
\ The block count captured here is revalidated at the end as the start of the
\ block window, so blocks of an abandoned function are not absorbed.
: BEGIN-FUN ( IR-ARENA:arena IR-ID:ir-symbol-id -- )
   {: b:IR-ARENA:arena sym:IR-ID:ir-symbol-id :}
   FSTG-MODE @ MODE-NONE <> if E-IR-FUN-STAGE throw then
   b IR-ARENA:OPEN-LIVE BCNT {: bst:n :}
   MODE-OPEN FSTG-MODE !
   FSTG-RESET
   sym SYM-ORD FSTG-SYM !
   sym SYM-OWNER FSTG-SYMO !
   bst FSTG-BST ! ;

: ABANDON-FUN ( -- )
   FSTG-TAKE ;

: SET-SIGNATURE ( IR-ID:ir-type-id -- )
   {: t:IR-ID:ir-type-id :}
   FSTG-OPEN-CK
   FSTG-SIG @ ONCE-CK
   t IR-ID:TYPE-LOCAL FSTG-SIG !
   t TYP-OWNER FSTG-SIGO ! ;

: SET-LINKAGE ( IR-FUN:linkage -- )
   FSTG-OPEN-CK
   FSTG-LNK @ ONCE-CK
   LNK-CODE FSTG-LNK ! ;

: SET-VISIBILITY ( IR-FUN:visibility -- )
   FSTG-OPEN-CK
   FSTG-VIS @ ONCE-CK
   VIS-CODE FSTG-VIS ! ;

: SET-CONVENTION ( IR-FUN:convention -- )
   FSTG-OPEN-CK
   FSTG-CC @ ONCE-CK
   CC-CODE FSTG-CC ! ;

: ADD-FUN-ATTR ( IR-ID:ir-attr-id -- )
   FSTG-OPEN-CK
   ATTR+ ;

: SET-FUN-SPAN ( IR-SOURCE:span -- )
   IR--SOURCE-SPAN:UNMAKE
   {: src:IR-ID:ir-source-id st:n ln:n :}
   FSTG-OPEN-CK
   FSTG-SRC @ ONCE-CK
   src IR-ID:SOURCE-LOCAL FSTG-SRC !
   src SRC-OWNER FSTG-SRCO !
   st FSTG-SBEG !
   ln FSTG-SLEN ! ;

\ The stage is consumed whatever the outcome; all checks precede the first write.
: END-FUN ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena -- IR-ID:ir-fun-id )
   {: c:IR-CTX:ctx p:IR-ARENA:arena f:IR-ARENA:arena b:IR-ARENA:arena key:IR-ID:ir-module-key sr:IR-ARENA:arena tr:IR-ARENA:arena ar:IR-ARENA:arena sa:IR-ARENA:arena :}
   FSTG-TAKE
   BSTG-MODE @ MODE-NONE <> if E-IR-FUN-STAGE throw then
   p IR-ARENA:OPEN-LIVE {: fnp:IR-ARENA:reader :}
   f IR-ARENA:OPEN-LIVE {: fnr:IR-ARENA:reader :}
   b IR-ARENA:OPEN-LIVE {: blr:IR-ARENA:reader :}
   fnp fnr blr TRIO-CK
   fnr key FNKEY-CK
   FNFIELDS-CK
   key KEY-SERIAL FSTG-SYMO @ OWNED-CK
   sr  key FSTG-SYM @ IR-ID:PACK-SYMBOL  IR-SYM:LEN@ drop
   fnr DUP-CK
   tr key SIG-CK
   ar key ATTRS-CK
   sa key FNSPAN-CK
   fnr FNCNT {: l:n :}
   l 0= if 0 else fnr l 1- FNBK-END then FSTG-BST @ STEP-CK
   blr BCNT FSTG-BST @ - {: bn:n :}
   bn 0 < if E-IR-FUN-WINDOW throw then
   bn BODY-CK
   c TARGET-CK
   blr l FSTG-BST @ bn PARENTS-CK
   fnp fnr FNROOM-CK
   c p f FNROOM-TAKE
   fnp PCELLS {: atst:n :}
   c p ATTRS-ADD
   c f atst FSTG-BST @ bn FNROW-ADD
   key l IR-ID:PACK-FUN ;

\ ---- the block builder -------------------------------------------------------
: BEGIN-BLOCK ( IR-ARENA:arena -- )
   {: r:IR-ARENA:arena :}
   FSTG-OPEN-CK
   BSTG-MODE @ MODE-NONE <> if E-IR-FUN-STAGE throw then
   r IR-OP:OPS {: opst:n :}
   MODE-OPEN BSTG-MODE !
   BSTG-RESET
   opst BSTG-OPST ! ;

: ABANDON-BLOCK ( -- )
   BSTG-TAKE ;

\ The one mutation before a block's checks: a value left by a rejected END-BLOCK
\ is discarded by the builder's ABORT, and the freeze verifier rejects a value
\ with no block.
: ADD-BLOCK-ARG ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key IR-ID:ir-type-id -- IR-ID:ir-value-id )
   {: c:IR-CTX:ctx v:IR-ARENA:arena tr:IR-ARENA:arena b:IR-ARENA:arena key:IR-ID:ir-module-key t:IR-ID:ir-type-id :}
   BSTG-OPEN-CK
   b IR-ARENA:OPEN-LIVE {: blr:IR-ARENA:reader :}
   blr key BKEY-CK
   c v tr key  key blr BCNT IR-ID:PACK-BLOCK  t BSTG-AGN @ IR-OP:MINT-ARG
   {: val:IR-ID:ir-value-id :}
   BSTG-AGN @ 0= if val IR-ID:VALUE-LOCAL BSTG-AGST ! then
   BSTG-AGN @ 1+ BSTG-AGN !
   val ;

: SET-BLOCK-SPAN ( IR-SOURCE:span -- )
   IR--SOURCE-SPAN:UNMAKE
   {: src:IR-ID:ir-source-id st:n ln:n :}
   BSTG-OPEN-CK
   BSTG-SRC @ ONCE-CK
   src IR-ID:SOURCE-LOCAL BSTG-SRC !
   src SRC-OWNER BSTG-SRCO !
   st BSTG-SBEG !
   ln BSTG-SLEN ! ;

: END-BLOCK ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena -- IR-ID:ir-block-id )
   {: c:IR-CTX:ctx b:IR-ARENA:arena f:IR-ARENA:arena key:IR-ID:ir-module-key v:IR-ARENA:arena r:IR-ARENA:arena qr:IR-ARENA:arena sa:IR-ARENA:arena :}
   BSTG-TAKE
   FSTG-OPEN-CK
   b IR-ARENA:OPEN-LIVE {: blr:IR-ARENA:reader :}
   f IR-ARENA:OPEN-LIVE {: fnr:IR-ARENA:reader :}
   blr fnr key BFN-CK
   BFIELDS-CK
   sa key BSPAN-CK
   blr BCNT {: l:n :}
   l 0= if 0 else blr l 1- BOP-END then BSTG-OPST @ STEP-CK
   r IR-OP:OPS BSTG-OPST @ - {: opn:n :}
   opn 0 < if E-IR-FUN-WINDOW throw then
   r qr key BSTG-OPST @ opn TERM-CK
   BSTG-AGN @ 0= if 0 else BSTG-AGST @ ORD-OK then {: agst:n :}
   agst BSTG-AGN @ + v IR-OP:VALUES > if E-IR-FUN-BOUND throw then
   v key l agst BSTG-AGN @ ARGS-CK
   blr BROOM-CK
   c b BROOM-TAKE
   c b fnr FNCNT agst BSTG-OPST @ opn BROW-ADD
   key l IR-ID:PACK-BLOCK ;

\ ---- table readers -----------------------------------------------------------
: FUNS ( IR-ARENA:arena -- n )
   IR-ARENA:OPEN-LIVE FNCNT ;

: BLOCKS ( IR-ARENA:arena -- n )
   IR-ARENA:OPEN-LIVE BCNT ;

: ATTR-CELLS ( IR-ARENA:arena -- n )
   IR-ARENA:OPEN-LIVE PCELLS ;

private

: FNFLD ( IR-ARENA:reader IR-ID:ir-fun-id n -- n )
   {: fnr:IR-ARENA:reader id:IR-ID:ir-fun-id off:n :}
   fnr  fnr id FNROW-AT  off FNC@ ;

: BFLD ( IR-ARENA:reader IR-ID:ir-block-id n -- n )
   {: blr:IR-ARENA:reader id:IR-ID:ir-block-id off:n :}
   blr  blr id BROW-AT  off BC@ ;

public

\ ---- function readers --------------------------------------------------------
: SYMBOL@ ( IR-ARENA:arena IR-ID:ir-module-key IR-ID:ir-fun-id -- IR-ID:ir-symbol-id )
   {: f:IR-ARENA:arena key:IR-ID:ir-module-key id:IR-ID:ir-fun-id :}
   f IR-ARENA:OPEN-LIVE {: fnr:IR-ARENA:reader :}
   fnr key FNKEY-CK
   key fnr id OFF-SYM FNFLD ORD-OK IR-ID:PACK-SYMBOL ;

\ The consumer revalidates the span with IR-SOURCE:SPAN-CK.
: SPAN@ ( IR-ARENA:arena IR-ID:ir-module-key IR-ID:ir-fun-id -- IR-SOURCE:span )
   {: f:IR-ARENA:arena key:IR-ID:ir-module-key id:IR-ID:ir-fun-id :}
   f IR-ARENA:OPEN-LIVE {: fnr:IR-ARENA:reader :}
   fnr key FNKEY-CK
   fnr id FNROW-AT {: l:n :}
   key fnr l OFF-SRC FNC@ ORD-OK IR-ID:PACK-SOURCE
   fnr l OFF-SBEG FNC@ LEN-OK
   fnr l OFF-SLEN FNC@ LEN-OK
   IR--SOURCE-SPAN:MAKE ;

: BLOCK-COUNT ( IR-ARENA:arena IR-ID:ir-fun-id -- n )
   {: f:IR-ARENA:arena id:IR-ID:ir-fun-id :}
   f IR-ARENA:OPEN-LIVE id OFF-BN FNFLD LEN-OK ;

\ ---- block readers -----------------------------------------------------------
\ Predecessor and successor counts are not stored: IR-VERIFY derives them at
\ freeze, since a count is wrong while any branch is unwritten.
: PARENT@ ( IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key IR-ID:ir-block-id -- IR-ID:ir-fun-id )
   {: b:IR-ARENA:arena f:IR-ARENA:arena key:IR-ID:ir-module-key id:IR-ID:ir-block-id :}
   b IR-ARENA:OPEN-LIVE {: blr:IR-ARENA:reader :}
   f IR-ARENA:OPEN-LIVE {: fnr:IR-ARENA:reader :}
   blr fnr key BFN-CK
   blr id OFF-PAR BFLD ORD-OK {: par:n :}
   par fnr FNCNT >= if E-IR-FUN-BOUND throw then
   key par IR-ID:PACK-FUN ;

: ARG-COUNT ( IR-ARENA:arena IR-ID:ir-block-id -- n )
   {: b:IR-ARENA:arena id:IR-ID:ir-block-id :}
   b IR-ARENA:OPEN-LIVE id OFF-AGN BFLD LEN-OK ;

: ARG@ ( IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key IR-ID:ir-block-id n -- IR-ID:ir-value-id )
   {: b:IR-ARENA:arena v:IR-ARENA:arena key:IR-ID:ir-module-key id:IR-ID:ir-block-id i:n :}
   b IR-ARENA:OPEN-LIVE {: blr:IR-ARENA:reader :}
   blr key BKEY-CK
   blr id BROW-AT {: l:n :}
   blr l OFF-AGN BC@ LEN-OK {: ln:n :}
   i 0 < i ln >= or if E-IR-FUN-BOUND throw then
   blr l OFF-AGST BC@ LEN-OK i + {: ord:n :}
   ord v IR-OP:VALUES >= if E-IR-FUN-BOUND throw then
   key ord IR-ID:PACK-VALUE {: val:IR-ID:ir-value-id :}
   v val IR-OP:VALUE-KIND@ IR--OP-DEF--KIND:BLK-ARG IR--OP-DEF--KIND:EQ
   0= if E-IR-FUN-ARG throw then
   v key val IR-OP:VALUE-BLOCK@ IR-ID:BLOCK-LOCAL l <> if E-IR-FUN-ARG throw then
   v val IR-OP:VALUE-ARG@ i <> if E-IR-FUN-ARG throw then
   val ;

: OP-COUNT ( IR-ARENA:arena IR-ID:ir-block-id -- n )
   {: b:IR-ARENA:arena id:IR-ID:ir-block-id :}
   b IR-ARENA:OPEN-LIVE id OFF-OPN BFLD LEN-OK ;

: OP@ ( IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key IR-ID:ir-block-id n -- IR-ID:ir-op-id )
   {: b:IR-ARENA:arena r:IR-ARENA:arena key:IR-ID:ir-module-key id:IR-ID:ir-block-id i:n :}
   b IR-ARENA:OPEN-LIVE {: blr:IR-ARENA:reader :}
   blr key BKEY-CK
   blr id BROW-AT {: l:n :}
   blr l OTILE-CK
   blr l OFF-OPN BC@ LEN-OK {: ln:n :}
   i 0 < i ln >= or if E-IR-FUN-BOUND throw then
   blr l OFF-OPST BC@ LEN-OK i + {: ord:n :}
   ord r IR-OP:OPS >= if E-IR-FUN-BOUND throw then
   key ord IR-ID:PACK-OP ;

: TERMINATOR@ ( IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key IR-ID:ir-block-id -- IR-ID:ir-op-id )
   {: b:IR-ARENA:arena r:IR-ARENA:arena key:IR-ID:ir-module-key id:IR-ID:ir-block-id :}
   b IR-ARENA:OPEN-LIVE {: blr:IR-ARENA:reader :}
   blr key BKEY-CK
   blr id BROW-AT {: l:n :}
   blr l OTILE-CK
   blr l BOP-END 1- {: want:n :}
   blr l OFF-TERM BC@ ORD-OK want <> if E-IR-FUN-TERM throw then
   want r IR-OP:OPS >= if E-IR-FUN-BOUND throw then
   key want IR-ID:PACK-OP ;

: BLOCK-SPAN@ ( IR-ARENA:arena IR-ID:ir-module-key IR-ID:ir-block-id -- IR-SOURCE:span )
   {: b:IR-ARENA:arena key:IR-ID:ir-module-key id:IR-ID:ir-block-id :}
   b IR-ARENA:OPEN-LIVE {: blr:IR-ARENA:reader :}
   blr key BKEY-CK
   blr id BROW-AT {: l:n :}
   key blr l OFF-BSRC BC@ ORD-OK IR-ID:PACK-SOURCE
   blr l OFF-BBEG BC@ LEN-OK
   blr l OFF-BLEN BC@ LEN-OK
   IR--SOURCE-SPAN:MAKE ;

\ ---- frozen readers ----------------------------------------------------------
: RFUNS ( IR-ARENA:reader -- n )
   IR-ARENA:FROZEN-READER FNCNT ;

: FFUNS ( IR-ARENA:view -- n )
   IR-ARENA:OPEN RFUNS ;

: RBLOCKS ( IR-ARENA:reader -- n )
   IR-ARENA:FROZEN-READER BCNT ;

: FBLOCKS ( IR-ARENA:view -- n )
   IR-ARENA:OPEN RBLOCKS ;

: FSYMBOL@ ( IR-ARENA:view IR-ID:ir-module-key IR-ID:ir-fun-id -- IR-ID:ir-symbol-id )
   {: f:IR-ARENA:view key:IR-ID:ir-module-key id:IR-ID:ir-fun-id :}
   f IR-ARENA:OPEN {: fnr:IR-ARENA:reader :}
   fnr key FNKEY-CK
   key fnr id OFF-SYM FNFLD ORD-OK IR-ID:PACK-SYMBOL ;

: RSIGNATURE@ ( IR-ARENA:reader IR-ID:ir-module-key IR-ID:ir-fun-id -- IR-ID:ir-type-id )
   {: f:IR-ARENA:reader key:IR-ID:ir-module-key id:IR-ID:ir-fun-id :}
   f IR-ARENA:FROZEN-READER {: fnr:IR-ARENA:reader :}
   fnr key FNKEY-CK
   key fnr id OFF-SIG FNFLD ORD-OK IR-ID:PACK-TYPE ;

: FSIGNATURE@ ( IR-ARENA:view IR-ID:ir-module-key IR-ID:ir-fun-id -- IR-ID:ir-type-id )
   {: f:IR-ARENA:view key:IR-ID:ir-module-key id:IR-ID:ir-fun-id :}
   f IR-ARENA:OPEN key id RSIGNATURE@ ;

: FLINKAGE@ ( IR-ARENA:view IR-ID:ir-fun-id -- IR-FUN:linkage )
   {: f:IR-ARENA:view id:IR-ID:ir-fun-id :}
   f IR-ARENA:OPEN id OFF-LNK FNFLD N>LNK ;

: FVISIBILITY@ ( IR-ARENA:view IR-ID:ir-fun-id -- IR-FUN:visibility )
   {: f:IR-ARENA:view id:IR-ID:ir-fun-id :}
   f IR-ARENA:OPEN id OFF-VIS FNFLD N>VIS ;

: FCONVENTION@ ( IR-ARENA:view IR-ID:ir-fun-id -- IR-FUN:convention )
   {: f:IR-ARENA:view id:IR-ID:ir-fun-id :}
   f IR-ARENA:OPEN id OFF-CC FNFLD N>CC ;

: FSPAN@ ( IR-ARENA:view IR-ID:ir-module-key IR-ID:ir-fun-id -- IR-SOURCE:span )
   {: f:IR-ARENA:view key:IR-ID:ir-module-key id:IR-ID:ir-fun-id :}
   f IR-ARENA:OPEN {: fnr:IR-ARENA:reader :}
   fnr key FNKEY-CK
   fnr id FNROW-AT {: l:n :}
   key fnr l OFF-SRC FNC@ ORD-OK IR-ID:PACK-SOURCE
   fnr l OFF-SBEG FNC@ LEN-OK
   fnr l OFF-SLEN FNC@ LEN-OK
   IR--SOURCE-SPAN:MAKE ;

: RBLOCK-COUNT ( IR-ARENA:reader IR-ID:ir-fun-id -- n )
   {: f:IR-ARENA:reader id:IR-ID:ir-fun-id :}
   f IR-ARENA:FROZEN-READER id OFF-BN FNFLD LEN-OK ;

: FBLOCK-COUNT ( IR-ARENA:view IR-ID:ir-fun-id -- n )
   {: f:IR-ARENA:view id:IR-ID:ir-fun-id :}
   f IR-ARENA:OPEN id RBLOCK-COUNT ;

\ The block is asked whether this function is its parent, so a window reaching
\ another function's blocks is refused.
: RBLOCK@ ( IR-ARENA:reader IR-ARENA:reader IR-ID:ir-module-key IR-ID:ir-fun-id n -- IR-ID:ir-block-id )
   {: f:IR-ARENA:reader b:IR-ARENA:reader key:IR-ID:ir-module-key id:IR-ID:ir-fun-id i:n :}
   b IR-ARENA:FROZEN-READER {: blr:IR-ARENA:reader :}
   f IR-ARENA:FROZEN-READER {: fnr:IR-ARENA:reader :}
   blr fnr key BFN-CK
   fnr id FNROW-AT {: l:n :}
   fnr l BTILE-CK
   fnr l OFF-BN FNC@ LEN-OK {: ln:n :}
   i 0 < i ln >= or if E-IR-FUN-BOUND throw then
   fnr l OFF-BST FNC@ LEN-OK i + {: ord:n :}
   ord blr BCNT >= if E-IR-FUN-BOUND throw then
   blr ord OFF-PAR BC@ l <> if E-IR-FUN-PARENT throw then
   key ord IR-ID:PACK-BLOCK ;

: FBLOCK@ ( IR-ARENA:view IR-ARENA:view IR-ID:ir-module-key IR-ID:ir-fun-id n -- IR-ID:ir-block-id )
   {: f:IR-ARENA:view b:IR-ARENA:view key:IR-ID:ir-module-key id:IR-ID:ir-fun-id i:n :}
   b IR-ARENA:OPEN {: blr:IR-ARENA:reader :}
   f IR-ARENA:OPEN blr key id i RBLOCK@ ;

: RARG-COUNT ( IR-ARENA:reader IR-ID:ir-block-id -- n )
   {: b:IR-ARENA:reader id:IR-ID:ir-block-id :}
   b IR-ARENA:FROZEN-READER id OFF-AGN BFLD LEN-OK ;

: FARG-COUNT ( IR-ARENA:view IR-ID:ir-block-id -- n )
   {: b:IR-ARENA:view id:IR-ID:ir-block-id :}
   b IR-ARENA:OPEN id RARG-COUNT ;

private

\ Admits the block before the public wrappers open its companion view.
: ARG-ORD ( IR-ARENA:reader IR-ID:ir-module-key IR-ID:ir-block-id n -- n n )
   {: blr:IR-ARENA:reader key:IR-ID:ir-module-key id:IR-ID:ir-block-id i:n :}
   blr key BKEY-CK
   blr id BROW-AT {: l:n :}
   blr l OFF-AGN BC@ LEN-OK {: ln:n :}
   i 0 < i ln >= or if E-IR-FUN-BOUND throw then
   l blr l OFF-AGST BC@ LEN-OK i + ;

: ARG-VALUE ( IR-ARENA:reader IR-ID:ir-module-key n n n -- IR-ID:ir-value-id )
   {: v:IR-ARENA:reader key:IR-ID:ir-module-key l:n ord:n i:n :}
   ord v IR-OP:RVALUES >= if E-IR-FUN-BOUND throw then
   key ord IR-ID:PACK-VALUE {: val:IR-ID:ir-value-id :}
   v val IR-OP:RVALUE-KIND@ IR--OP-DEF--KIND:BLK-ARG IR--OP-DEF--KIND:EQ
   0= if E-IR-FUN-ARG throw then
   v key val IR-OP:RVALUE-BLOCK@ IR-ID:BLOCK-LOCAL l <> if E-IR-FUN-ARG throw then
   v val IR-OP:RVALUE-ARG@ i <> if E-IR-FUN-ARG throw then
   val ;

public

: RARG@ ( IR-ARENA:reader IR-ARENA:reader IR-ID:ir-module-key IR-ID:ir-block-id n -- IR-ID:ir-value-id )
   {: b:IR-ARENA:reader v:IR-ARENA:reader key:IR-ID:ir-module-key id:IR-ID:ir-block-id i:n :}
   b IR-ARENA:FROZEN-READER key id i ARG-ORD {: l:n ord:n :}
   v key l ord i ARG-VALUE ;

: FARG@ ( IR-ARENA:view IR-ARENA:view IR-ID:ir-module-key IR-ID:ir-block-id n -- IR-ID:ir-value-id )
   {: b:IR-ARENA:view v:IR-ARENA:view key:IR-ID:ir-module-key id:IR-ID:ir-block-id i:n :}
   b IR-ARENA:OPEN key id i ARG-ORD {: l:n ord:n :}
   v IR-ARENA:OPEN key l ord i ARG-VALUE ;

: ROP-COUNT ( IR-ARENA:reader IR-ID:ir-block-id -- n )
   {: b:IR-ARENA:reader id:IR-ID:ir-block-id :}
   b IR-ARENA:FROZEN-READER id OFF-OPN BFLD LEN-OK ;

: FOP-COUNT ( IR-ARENA:view IR-ID:ir-block-id -- n )
   {: b:IR-ARENA:view id:IR-ID:ir-block-id :}
   b IR-ARENA:OPEN id ROP-COUNT ;

private

: OP-ORD ( IR-ARENA:reader IR-ID:ir-module-key IR-ID:ir-block-id n -- n )
   {: blr:IR-ARENA:reader key:IR-ID:ir-module-key id:IR-ID:ir-block-id i:n :}
   blr key BKEY-CK
   blr id BROW-AT {: l:n :}
   blr l OTILE-CK
   blr l OFF-OPN BC@ LEN-OK {: ln:n :}
   i 0 < i ln >= or if E-IR-FUN-BOUND throw then
   blr l OFF-OPST BC@ LEN-OK i + ;

: TERM-ORD ( IR-ARENA:reader IR-ID:ir-module-key IR-ID:ir-block-id -- n )
   {: blr:IR-ARENA:reader key:IR-ID:ir-module-key id:IR-ID:ir-block-id :}
   blr key BKEY-CK
   blr id BROW-AT {: l:n :}
   blr l OTILE-CK
   blr l BOP-END 1- {: want:n :}
   blr l OFF-TERM BC@ ORD-OK want <> if E-IR-FUN-TERM throw then
   want ;

: OP-ID ( IR-ARENA:reader IR-ID:ir-module-key n -- IR-ID:ir-op-id )
   {: r:IR-ARENA:reader key:IR-ID:ir-module-key ord:n :}
   ord r IR-OP:ROPS >= if E-IR-FUN-BOUND throw then
   key ord IR-ID:PACK-OP ;

public

: ROP@ ( IR-ARENA:reader IR-ARENA:reader IR-ID:ir-module-key IR-ID:ir-block-id n -- IR-ID:ir-op-id )
   {: b:IR-ARENA:reader r:IR-ARENA:reader key:IR-ID:ir-module-key id:IR-ID:ir-block-id i:n :}
   b IR-ARENA:FROZEN-READER key id i OP-ORD {: ord:n :}
   r key ord OP-ID ;

: FOP@ ( IR-ARENA:view IR-ARENA:view IR-ID:ir-module-key IR-ID:ir-block-id n -- IR-ID:ir-op-id )
   {: b:IR-ARENA:view r:IR-ARENA:view key:IR-ID:ir-module-key id:IR-ID:ir-block-id i:n :}
   b IR-ARENA:OPEN key id i OP-ORD {: ord:n :}
   r IR-ARENA:OPEN key ord OP-ID ;

: RTERMINATOR@ ( IR-ARENA:reader IR-ARENA:reader IR-ID:ir-module-key IR-ID:ir-block-id -- IR-ID:ir-op-id )
   {: b:IR-ARENA:reader r:IR-ARENA:reader key:IR-ID:ir-module-key id:IR-ID:ir-block-id :}
   b IR-ARENA:FROZEN-READER key id TERM-ORD {: ord:n :}
   r key ord OP-ID ;

: FTERMINATOR@ ( IR-ARENA:view IR-ARENA:view IR-ID:ir-module-key IR-ID:ir-block-id -- IR-ID:ir-op-id )
   {: b:IR-ARENA:view r:IR-ARENA:view key:IR-ID:ir-module-key id:IR-ID:ir-block-id :}
   b IR-ARENA:OPEN key id TERM-ORD {: ord:n :}
   r IR-ARENA:OPEN key ord OP-ID ;

: FBLOCK-SPAN@ ( IR-ARENA:view IR-ID:ir-module-key IR-ID:ir-block-id -- IR-SOURCE:span )
   {: b:IR-ARENA:view key:IR-ID:ir-module-key id:IR-ID:ir-block-id :}
   b IR-ARENA:OPEN {: blr:IR-ARENA:reader :}
   blr key BKEY-CK
   blr id BROW-AT {: l:n :}
   key blr l OFF-BSRC BC@ ORD-OK IR-ID:PACK-SOURCE
   blr l OFF-BBEG BC@ LEN-OK
   blr l OFF-BLEN BC@ LEN-OK
   IR--SOURCE-SPAN:MAKE ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;package
