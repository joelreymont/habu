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

\ This file loads before src/habu/layout.f, so the cells the engine's refusal
\ tail reads (habu2.f LCOMPILEDIE) and the pending definition's two cells are
\ spelled here as data-base offsets, as src/core/include.f spells the SRCLOC
\ pair. layout.f reserves them as SRCLOC:INB-CELL, PEND-CELL and PENDTKA-CELL;
\ the cursor, INP-CELL, is src/core/layout-buffer.f STGT-INP-CELL. The path
\ pair, SRC-PATH and SRC-PATHLEN-OFF, is this package's from src/core/checker.f.
$2810 RESERVED-PTR-U8-CELL SRC-START
STGT-INP-CELL RESERVED-PTR-U8-CELL SRC-CURSOR
$3688 constant PEND-OFF
$2850 RESERVED-PTR-U8-CELL SRC-NAME

\ The name as written, which is what the wall prints: the first token of the
\ source the engine hands the hook.
: FIRST-TOKEN ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   0 begin dup u < if a over + c@ 32 > else 0 0= 0= then while 1 + repeat
   a swap ;

\ The line of a byte of the buffer being read.
: LINE-AT ( ptr u8 -- n )
   SRC-START @ swap ERROR-TEXT:LINE ;

\ The line of the definition's name, where the wall places a duplicate. At a
\ colon definition's closing `;` the definition is pending (PEND-CELL) and its
\ opener kept where its name starts (PENDTKA-CELL); create, variable and
\ constant run the hook with nothing pending (habu2.f C-DEFHOOK), PENDTKA-CELL
\ still at the last colon definition's name and the cursor just past their own.
\ A name not between the buffer's start and the cursor is not counted to, since
\ the count would read outside the buffer: it is placed at the cursor.
: NAME-LINE ( -- n )
   SRC-START @ SRC-NAME @ SRC-CURSOR @ {: b:ptr nm:ptr p:ptr :}
   data-base PEND-OFF + @ 0 <>  nm b - 0 >= and  p nm - 0 >= and
   if nm LINE-AT else p LINE-AT then ;

\ ` at <path>:<line>` while a source file is open, and nothing on stdin or at
\ the REPL, as the tail writes it.
: AT-SOURCE ( -- )
   data-base SRC-PATHLEN-OFF + @ {: u:n :}
   u 0= if exit then
   s"  at " ERROR-TEXT:TYPE
   SRC-PATH @ u ERROR-TEXT:TYPE
   s" :" ERROR-TEXT:TYPE
   NAME-LINE ERROR-TEXT:DIGITS$ ERROR-TEXT:TYPE ;

: REPORT-DUPLICATE ( ptr u8 n -- )
   s" duplicate definition: " ERROR-TEXT:TYPE
   ERROR-TEXT:TYPE
   AT-SOURCE
   S\" \n" ERROR-TEXT:TYPE ;

\ In multi-error mode CHECK already emitted the diagnostic, counted the reject
\ and kept the declared signature. A non-zero hook return publishes the
\ definition (zero rejects and unpublishes it), so the name resolves for later
\ definitions; the driver exits nonzero via MULTI-ERR-END.
public

: HOOK ( ptr u8 n -- n ) {: a:ptr u:n :}
   CHECKER-REPORT-RESET
   a u FIRST-TOKEN {: na:ptr nu:n :}
   na nu CHECKER-CERT-DUP? if na nu REPORT-DUPLICATE DUP-RC throw then
   a u CHECK! REPORT-UNCHECKABLE
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

\ Dynamic preflight/hook installation asserts the canonical checker identities;
\ ordinary checked code cannot type execution-token installation.
TRUSTED: INSTALL ( -- )
   ['] PREFLIGHT set-preflight
   ['] HOOK set-check ;

INSTALL

;package
