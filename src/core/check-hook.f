\ check-hook.f - default native source checker hook and certificate checkpoint.

package LOWER-CERT-HOOK

70 constant CHECK-RC

: PREFLIGHT ( ptr u8 n ptr u8 n bool -- )
   drop
   {: ba:ptr bu:n ta:ptr tu:n :}
   ta tu NEUTRAL-PARSE-IMM? if exit then
   ba bu ta tu CHECKER-PREFLIGHT:CHECK-TARGET! 0 <> if
      s" checker: preflight did not reject unmodeled immediate" 76 die
   then
   CHECK-RC throw ;

\ ---- a duplicate only the checker sees ----------------------------------------
\ A replay entry (CHECKER-DEFPRODUCT with GENERATED-DECL-CTOR:REPLAY-LEGACY,
\ STRUCTURE-DECL:SD-REPLAY, ...) publishes a checked effect without defining the
\ word, so a later definition passes the engine's wall (habu2.f C-REJECT-DUP-DEF)
\ and checker.f CHECK-REC-ADMIT refuses it with $4E and no message. The hook asks
\ CHECKER-CERT-DUP? (which interns nothing) first and writes what the wall writes.
\ The source tools classify $4E themselves (check-all-errors-core.f DUP-RC).
$4E constant DUP-RC

\ This file loads before src/habu/layout.f, so the pending definition's two
\ cells are spelled here as data-base offsets, as src/core/include.f spells the
\ SRCLOC pair. layout.f reserves them as PEND-CELL and PENDTKA-CELL; the
\ cursor, INP-CELL, is src/core/layout-buffer.f STGT-INP-CELL. The path pair,
\ SRC-PATH and SRC-PATHLEN-OFF, and the file's bytes, SRC-OWNED, are this
\ package's from src/core/checker.f.
STGT-INP-CELL RESERVED-PTR-U8-CELL SRC-CURSOR
$3688 constant PEND-OFF
$2850 RESERVED-PTR-U8-CELL SRC-NAME

\ The name as written, which is what the wall prints: the first token of the
\ source the engine hands the hook.
: FIRST-TOKEN ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   0 begin dup u < if a over + c@ 32 > else 0 0= 0= then while 1 + repeat
   a swap ;

\ The line of a byte of the open file, counted from its first owned byte.
: LINE-AT ( ptr u8 -- n )
   SRC-OWNED @ swap ERROR-TEXT:LINE ;

\ The line of the definition's name, where the wall places a duplicate. At a
\ colon definition's closing `;` the definition is pending (PEND-CELL) and its
\ opener kept where its name starts (PENDTKA-CELL); create, variable and
\ constant run the hook with nothing pending (habu2.f C-DEFHOOK), PENDTKA-CELL
\ still at the last colon definition's name and the cursor just past their own.
\ A name not between the file's first owned byte and the cursor is not counted
\ to, since the count would read outside the file: it is placed at the cursor.
: NAME-LINE ( -- n )
   SRC-OWNED @ SRC-NAME @ SRC-CURSOR @ {: o:ptr nm:ptr p:ptr :}
   data-base PEND-OFF + @ 0 <>  nm o - 0 >= and  p nm - 0 >= and
   if nm LINE-AT else p LINE-AT then ;

\ ` at <path>` while a source file is open, then `:<line>` under
\ SRCLOC:FILE-BUFFER?, as the tail writes it; nothing on stdin or at the REPL.
: AT-SOURCE ( -- )
   data-base SRC-PATHLEN-OFF + @ {: u:n :}
   u 0= if exit then
   s"  at " ERROR-TEXT:TYPE
   SRC-PATH @ u ERROR-TEXT:TYPE
   SRCLOC:FILE-BUFFER? 0= if exit then
   s" :" ERROR-TEXT:TYPE
   NAME-LINE ERROR-TEXT:DIGITS$ ERROR-TEXT:TYPE ;

: REPORT-DUPLICATE ( ptr u8 n -- )
   s" duplicate definition: " ERROR-TEXT:TYPE
   ERROR-TEXT:TYPE
   AT-SOURCE
   S\" \n" ERROR-TEXT:TYPE ;


\ Appends sym's tags at RSN, after the effect FACTS-SAME? left in RSBUF.
: FACTS-TAGS ( n -- )
   {: sym:n :}
   sym CTL-WORD-SYM  sym DFER-FIND-SYM if CHECKER-OWNER-ABI:BINDING-DEFER or then
   REND-CTL 2drop
   sym PRIM-FIRST-IDX {: prim:n :}
   prim 0 <> if prim 1 - PE-FLAGS@ else 0 then REND-PE 2drop
   sym PIMM-IX {: ix:n :}
   ix 0 < if -1 else ix PIMM-CNT@ then REND-PIMM 2drop
   sym NORET-CREATES@ REND-CREATES 2drop ;


\ Only a record this check published, at or above MARK and still active, is
\ asked: a duplicate or an unrecorded effect publishes none.
: CHECK-FACTS ( ptr u8 n n -- )
   {: na:ptr nu:n mark:n :}
   na nu CHECKER-RECORD-SYM? {: sym:n :}
   sym 0= if exit then
   sym USIG-NEWEST {: rec1:n :}
   rec1 0= if exit then
   rec1 1 - mark < if exit then
   rec1 1 - E-PTR ER.ACTIVE @ EFF-ACTIVE <> if exit then
   sym FACTS-SAME? if
      CK-LIVE-PEND? 0= if exit then
      DOES-SPLIT @ 0 <> if exit then
      sym FACTS-TAGS
      RQM @ 0= if RSBUF RSN @ ndict@ CHECK:FACTS! exit then
   then
   RQM @ 0 <> if s" hook: facts text cannot say: " else s" hook: facts text differs: " then
   ERROR-TEXT:TYPE
   NMB NMU @ ERROR-TEXT:TYPE
   s" : " ERROR-TEXT:TYPE
   RSBUF RSN @ ERROR-TEXT:TYPE
   S\" \n" ERROR-TEXT:TYPE
   CHECK-RC throw ;


\ Whether sym's newest effect has a facts text that says it, left in RSBUF.
: FACTS-TEXT? ( n -- bool )
   {: sym:n :}
   sym 0= if RES-FALSE exit then
   sym USIG-NEWEST {: rec1:n :}
   rec1 0= if RES-FALSE exit then
   rec1 1 - E-PTR ER.ACTIVE @ EFF-ACTIVE <> if RES-FALSE exit then
   sym FACTS-SAME? 0= if RES-FALSE exit then
   sym FACTS-TAGS
   RQM @ 0= ;


\ The does> field entry, once the definer's record ndict@ 1 - is appended: it
\ refuses nothing and writes facts only there, as `: PKG:X` hands it the tail X.
: DOES-PUBLISHED ( ptr u8 n bool -- )
   {: a:ptr u:n committed:bool :}
   a u committed CHECKER-NATIVE-DOES-FINISH
   committed 0= if exit then
   a u CHECKER-RECORD-SYM? {: sym:n :}
   sym FACTS-TEXT? 0= if exit then
   sym VIS-SYM-REC CHECKER-RESOLVE:REC>INDEX {: ix:n :}
   ix ndict@ 1 - <> if exit then
   RSBUF RSN @ ix CHECK:FACTS!
   sym NORET-CREATES@ {: made:n :}
   made 0= if exit then
   0 RSN !  made 1 - REND-RECORD
   RQM @ 0 <> if 2drop exit then
   CHECK:LAY-FACTS ;


\ checker.f's TRUST-RAW, TRUST-DECL and CHECKER-DEFCAST, which the fields held:
\ a checked body cannot name them (UNSAFE-TOK?), so each entry calls its token.
variable RAW-REG
variable DECL-REG
variable DEFCAST-REG
CAST: REG-XT ( n -- [ ptr u8 n ptr u8 n -- ] )


\ The raw registrar's field entry: a created word or constant takes its text
\ through CHECK:FIND-FACTS!, or 0 when none says its effect.
: RAW-DECLARED ( ptr u8 n ptr u8 n -- )
   {: na:ptr nu:n sa:ptr su:n :}
   na nu sa su RAW-REG @ REG-XT execute
   CHECKER-VERIFY-PKG-DEPTH @ 0 <> CHK-CAND @ 0 <> or if exit then
   data-base CHECKER-REG:CK-LASTC-CELL + 0 ptr-field @ {: rec:ptr :}
   rec NULL-PTR = if exit then
   rec CHECKER-RESOLVE:REC>INDEX {: ix:n :}
   ix 0 < if exit then
   LASTC-LINK-SYM FACTS-TEXT? if RSBUF RSN @ else NULL-PTR 0 then
   ix CHECK:FIND-FACTS! ;


\ The pending record's facts once a registrar recorded the live window for this
\ name; an unsaid text stores 0, so no earlier registrar's text stays.
: PENDING-FACTS ( ptr u8 n -- )
   {: a:ptr u:n :}
   CK-LIVE-PEND? 0= if exit then
   CK-CLOSED-OPEN @ 0= if exit then
   DOES-SPLIT @ 0 <> if exit then
   CK-CLOSED-SYM @ CK-CLOSED-IX @ CK-CLOSED-OFF@ CK-PEND-SYM {: sym:n :}
   sym 0= if exit then
   a u CHECKER-RECORD-SYM? sym <> if exit then
   sym FACTS-TEXT? if RSBUF RSN @ CK-CLOSED-IX @ CHECK:FACTS!
   else NULL-PTR 0 CK-CLOSED-IX @ CHECK:FIND-FACTS! then ;


\ The declaration registrars' field entries: each runs its registrar, then lays
\ the declared name's facts.
: DECLARED ( ptr u8 n ptr u8 n -- )
   {: na:ptr nu:n sa:ptr su:n :}
   na nu sa su DECL-REG @ REG-XT execute  na nu PENDING-FACTS ;


: DEFERRED ( ptr u8 n -- )
   {: a:ptr u:n :}
   a u CHECKER-DEFER  a u PENDING-FACTS ;


: CAST-DECLARED ( ptr u8 n ptr u8 n -- )
   {: na:ptr nu:n sa:ptr su:n :}
   na nu sa su DEFCAST-REG @ REG-XT execute  na nu PENDING-FACTS ;


: LINEAR-DECLARED ( ptr u8 n ptr u8 n -- )
   {: na:ptr nu:n sa:ptr su:n :}
   na nu sa su CHECKER-LINEAR  na nu PENDING-FACTS ;


\ In multi-error mode CHECK already emitted the diagnostic, counted the reject
\ and kept the declared signature. A non-zero hook return publishes the
\ definition (zero rejects and unpublishes it), so the name resolves for later
\ definitions; the driver exits nonzero via MULTI-ERR-END.
public

: HOOK ( ptr u8 n -- n )
   {: a:ptr u:n :}
   CHECKER-REPORT-RESET
   a u FIRST-TOKEN {: na:ptr nu:n :}
   na nu CHECKER-CERT-DUP? if na nu REPORT-DUPLICATE DUP-RC throw then
   UEND @ {: mark:n :}
   a u CHECK! REPORT-UNCHECKABLE
   dup -1 = if na nu mark CHECK-FACTS then
   MULTI-ERR? if drop -1 exit then
   dup -1 <> if
      \ A muted or replaced DIAGXT can emit nothing even in JSON mode.
      CHECKER-JSON-REPORTED? 0= if
         s" hook: non-certified definition: " ERROR-TEXT:TYPE
         NMB NMU @ ERROR-TEXT:TYPE
         s"  at '" ERROR-TEXT:TYPE
         FAILTK FAILTU @ ERROR-TEXT:TYPE
         S\" '\n" ERROR-TEXT:TYPE
      then
      CHECK-RC throw then ;

\ At a piece's end (src/habu/linker.f PIECE-END), each record a `TRUST` row or an
\ amender (`INTRINSIC`, `generates:`, `names:`, NOMINAL-PROVIDER, a does>
\ publication) changed, held from entry FROM on, takes its facts text again from
\ its symbol's newest effect, as a definition's does: the record the symbol names
\ takes the text, and a held record it no longer names an empty one. A text that
\ cannot say the effect, or a symbol no record holds, fails the build: a link
\ would read the record's older facts.
: AMENDED ( n -- )
   {: from:n :}
   CHECK:HELD-FACTS from ?do
      i CHECK:HELD-FACT@ 2drop CHECK:HELD-AMENDED = if
         i CHECK:HELD-AMENDED@ {: ix:n sym:n :}
         sym VIS-SYM-REC CHECKER-RESOLVE:REC>INDEX {: now:n :}
         now ix <> if NULL-PTR 0 ix CHECK:FACTS! then
         now 0 >=  sym FACTS-TEXT?  and 0= if
            s" hook: facts text cannot say: " ERROR-TEXT:TYPE
            sym SYM-NAME$ ERROR-TEXT:TYPE
            S\" \n" ERROR-TEXT:TYPE
            CHECK-RC throw
         then
         RSBUF RSN @ now CHECK:FACTS!
      then
   loop ;

\ The active primitive rows of sym after row k: whether one is, and whether
\ each survives its facts text (FACTS-SAME-AT?).
: ROW-OF? ( n n -- bool ) {: sym:n k:n :} k PE-ACTIVE?  k PE-SYM@ sym =  and ;

: MORE-ROWS? ( n n -- bool )
   {: sym:n k:n :}
   #PE @ k 1 + ?do sym i ROW-OF? if RES-TRUE unloop exit then loop
   RES-FALSE ;

: ROWS-SAY? ( n n -- bool )
   {: sym:n k:n :}
   #PE @ k 1 + ?do
      sym i ROW-OF? if
         sym i PE-EFF@ FACTS-SAME-AT? 0= if RES-FALSE unloop exit then
      then
   loop
   RES-TRUE ;


\ Sym's tags alone in RSBUF, the facts text of a primitive the checker types by
\ what it does (`execute`, `?dup`). False when it has none or a text cannot say
\ them.
: TAGS-TEXT? ( n -- bool )
   {: sym:n :}
   0 RSN !  0 RQM !
   sym FACTS-TAGS
   RSN @ 0 >  RQM @ 0=  and ;


\ Sym's primitive rows as one facts text in RSBUF: the first row's effect and
\ tags, then each further row after `overload`; with no row, its tags alone.
\ False when a text cannot say them, or a created effect, whose text ends the
\ facts, would hide the rows.
: ROWS-TEXT? ( n -- bool )
   {: sym:n :}
   sym PRIM-FIRST-IDX {: first:n :}
   first 0= if sym TAGS-TEXT? exit then
   first 1 - {: k:n :}
   sym k ROWS-SAY? 0= if RES-FALSE exit then
   sym k PE-EFF@ FACTS-SAME-AT? 0= if RES-FALSE exit then
   sym FACTS-TAGS
   sym k MORE-ROWS? 0= if RQM @ 0= exit then
   sym NORET-CREATES@ 0 <> if RES-FALSE exit then
   #PE @ k 1 + ?do sym i ROW-OF? if i REND-OVERLOAD 2drop then loop
   RQM @ 0= ;


\ Rec's own facts text as it stands, else, in an hb whose records carry none,
\ sym's rows' (ROWS-TEXT?); rec NULL-PTR or sym 0 is none. A text read back and
\ rendered again can lose what it said: a text with no primitive tag and no
\ overload reads as an external effect, which holds no primitive row. Empty
\ when neither says them. A rendered text lies in RSBUF, which the next render
\ reuses.
: OWN-OR-ROWS ( ptr n n -- ptr u8 n )
   {: rec:ptr sym:n :}
   rec NULL-PTR <> if
      rec CHECKER-RESOLVE:REC>INDEX CHECK:FACTS@ dup 0 <> if exit then 2drop
   then
   sym 0= if NULL-PTR 0 exit then
   sym ROWS-TEXT? if RSBUF RSN @ else NULL-PTR 0 then ;

\ The facts text this checker gives the primitive named na nu, for a linked
\ engine's record of it (OWN-OR-ROWS). Empty when no primitive has the name.
: PRIMITIVE-FACTS ( ptr u8 n -- ptr u8 n )
   {: na:ptr nu:n :}
   na nu WL:PRIMITIVES CHECKER-RESOLVE:WL-PROBE {: rec:ptr :}
   rec NULL-PTR = if NULL-PTR 0 exit then
   rec  na nu CHECKER-GLOBAL-SYM?  OWN-OR-ROWS ;

\ The facts text this checker gives the primitive named na nu that package pa pu
\ owns (src/habu/prims.f EPPRIM:), for the record a build lays for it in that
\ package's private wordlist (OWN-OR-ROWS): rec is this hb's own such record,
\ NULL-PTR for none.
: OWNED-FACTS ( ptr n ptr u8 n ptr u8 n -- ptr u8 n )
   {: rec:ptr pa:ptr pu:n na:ptr nu:n :}
   rec  pa pu SYM-PRIVATE na nu CHECKER-PKG-SYM?  OWN-OR-ROWS ;

\ Dynamic preflight/hook installation asserts the canonical checker identities;
\ ordinary checked code cannot type execution-token installation.
TRUSTED: INSTALL ( -- )
   ['] PREFLIGHT set-preflight
   ['] HOOK set-check ;

\ Replace checker.f's CHECKER-NATIVE-DOES-FINISH, TRUST-RAW, TRUST-DECL,
\ CHECKER-DEFER, CHECKER-DEFCAST and CHECKER-LINEAR, now the renderer exists.
' DOES-PUBLISHED CHECKER-REG:DECLARATIONS CHECKER-OWNER-ABI:NATIVE-DOES-FINISH-OFF + xt!
CHECKER-REG:DECLARATIONS CHECKER-OWNER-ABI:RAW-OFF + CELL-VIEW @ RAW-REG xt!
CHECKER-REG:DECLARATIONS CHECKER-OWNER-ABI:EFFECT-OFF + CELL-VIEW @ DECL-REG xt!
CHECKER-REG:DECLARATIONS CHECKER-OWNER-ABI:CAST-OFF + CELL-VIEW @ DEFCAST-REG xt!
' RAW-DECLARED CHECKER-REG:DECLARATIONS CHECKER-OWNER-ABI:RAW-OFF + xt!
' DECLARED CHECKER-REG:DECLARATIONS CHECKER-OWNER-ABI:EFFECT-OFF + xt!
' DECLARED CHECKER-REG:DECLARATIONS CHECKER-OWNER-ABI:TRUST-DECL-OFF + xt!
' DEFERRED CHECKER-REG:DECLARATIONS CHECKER-OWNER-ABI:DEFER-OFF + xt!
' CAST-DECLARED CHECKER-REG:DECLARATIONS CHECKER-OWNER-ABI:CAST-OFF + xt!
' LINEAR-DECLARED CHECKER-REG:DECLARATIONS CHECKER-OWNER-ABI:LINEAR-OFF + xt!
INSTALL

;package
