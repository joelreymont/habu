\ native-window-owner-child.f - tools/native-build-core.f's window, reduced to
\ the checker handover: LOAD-WINDOW is its LOAD-TARGET through
\ src/core/cell-effects.f. Later script arguments load the fixture's
\ dependencies through the retained compiler.

require src/lib/errors.f
require src/core/prefix-boundary.f
require src/lib/string.f
\ src/lib/memory.f is required here and in no window prefix file, so NUM's
\ families are host-only through the handover: the retained checker knows them
\ and the window must not. A suite child loading after the window's seal may
\ require it (src/lib/test/subject.f).
require src/lib/memory.f
require src/lib/process.f
require src/habu/address-cells.f
require src/compiler/native/string.f

package NW-OWNER

$400 constant PATH-CAP

create PATH-BUF PATH-CAP allot
variable PATH-U
variable ADDRESS-ABI

: PATH! ( ptr u8 n -- ) {: a:ptr u:n :}
   u PATH-CAP > if s" window: fixture path too long" 76 die then
   a PATH-BUF u BYTE-COPY
   u PATH-U ! ;

\ The name is gone after the reset; the compiled call site is not.
: PATH$ ( -- ptr u8 n ) PATH-BUF PATH-U @ ;

\ --- adapted from tools/native-build-core.f ---------------------------------

\ Keep the host's engine declarations, the cells below its heap floor
\ (BOOT-LAYOUT:HEAP-START-CELL), and discard those of the retired heap above it:
\ filtered, not truncated, because the rows are not partitioned in registration
\ order (tools/native-build-core.f states why).
: ADDR-ROWS ( -- n ) data-base SNAP-RELOC:XTCELL-N-CELL + @ ;

: ADDR-ROW@ ( n -- n ) ADDRESS-CELLS:ROW@ ;

\ The layout name itself, not habu2.f's host-side mirror, and no source-constant
\ fallback: this fixture runs only under the engine the tree just built. A
\ silent zero would classify every row as heap and empty the table.
: HOST-HEAP-START ( -- n ) data-base BOOT-LAYOUT:HEAP-START-CELL + @ ;

: KEEP-ROWS-BELOW ( n -- ) ADDRESS-CELLS:KEEP-BELOW ;

: RESET-ADDRESS-ROWS ( -- )
   HOST-HEAP-START {: floor:n :}
   floor 0= if s" window: host publishes no heap floor" 76 die then
   floor KEEP-ROWS-BELOW ;

defer RESET-SOURCE ( -- )

\ The checker publishes its own declaration owner into the engine's target
\ declaration cell as it loads (src/core/checker.f), so the host's owner is a
\ cell read before the window loads its replacement.
: CHECKER-OWNER ( -- ptr u8 )
   data-base NCOMP-DISPATCH:TARGET-DECL-CELL + 0 ptr-field @ ;

\ The execution token belongs to the retained private checker owner, whose
\ record holds it as a code address integer.
CAST: RESET-XT ( n -- [ -- ] )

: RESET-CHECKER ( ptr u8 -- ) {: owner:ptr :}
   owner 0= if exit then
   owner NCOMP-DISPATCH:DECL-RESET-OFF + CELL-VIEW @ RESET-XT is RESET-SOURCE
   RESET-SOURCE ;

\ Installing a replacement's callbacks must not claim a nonzero source owner
\ before the explicit transfer below; the retained compiler still uses it.
: CHECK-RETAINED-OWNER ( ptr u8 -- ) {: source:ptr :}
   data-base NCOMP-DISPATCH:DECL-CELL + 0 ptr-field @ source <> if
      s" window: replacement checker claimed the source before transfer" 76 die then
   data-base NCOMP-DISPATCH:TARGET-DECL-CELL + 0 ptr-field @ source = if
      s" window: replacement checker did not publish a distinct target" 76 die then ;

: LOGICAL-RESET ( ptr u8 -- )
   RESET-CHECKER
   [: s" src/habu/window-rewind.f" included ;] SOURCE-ROOT:WITH-CWD
   RESET-ADDRESS-ROWS ;

\ The dictionary rows hold the fresh source reset and checker handover as code
\ address integers.
CAST: SOURCE-RESET-XT ( n -- [ -- ] )
CAST: SOURCE-PROVIDE-XT ( n -- [ ptr u8 n -- ] )

: ACTIVATE-SOURCE ( -- )
   s" SOURCE-INPUT:RESET" XREF-FIND dup XREF-FOUND? 0= if
      drop s" window: fresh source reset missing" 76 die
   then
   XREF-START SOURCE-RESET-XT execute ;

: TAKE-OVER ( -- )
   s" CHECKER-REG:HANDOVER" XREF-FIND dup XREF-FOUND? 0= if
      drop s" window: fresh checker handover missing" 76 die
   then
   XREF-START SOURCE-RESET-XT execute ;

: TARGET-PROVIDE ( ptr u8 n -- )
   s" provided" XREF-FIND dup XREF-FOUND? 0= if
      drop s" window: fresh provided missing" 76 die
   then
   XREF-START SOURCE-PROVIDE-XT execute ;

variable SOURCE-READY

: RECOVERY$ ( -- ptr u8 n ) s" --recovery-row" ;

: OPTIONAL-FILE? ( ptr u8 n -- bool )
   2dup s" --literals" CORE-STR= if 2drop false exit then
   RECOVERY$ CORE-STR= 0= ;

: REGISTER-LOADED ( n -- ) {: last:n :}
   \ The early core and each optional path are already present in the target
   \ dictionary. Its new loader must record them before a later require runs.
   s" src/core/does-clause.f" TARGET-PROVIDE
   s" src/core/layout-buffer.f" TARGET-PROVIDE
   last 1+ 1 ?do
      i SCRIPT-ARGV$ 2dup OPTIONAL-FILE? if TARGET-PROVIDE else 2drop then
   loop ;

: LOAD-OPTIONAL ( ptr u8 n n -- ) {: path:ptr u:n idx:n :}
   path u included
   path u s" src/core/include.f" STR= if
      ACTIVATE-SOURCE
      idx REGISTER-LOADED
      true SOURCE-READY !
   else
      SOURCE-READY @ if path u TARGET-PROVIDE then
   then ;

\ --- the recovery-row mode ---------------------------------------------------
\ `--recovery-row` after the fixture path: just before the transfer the retained
\ checker refuses one definition under multi-error collection, which leaves its
\ row in EFF-RECOVERY (src/core/checker.f), a fact of this run and never a
\ source grant. Tier 0 scans no body while the hook is off, as it is through the
\ prefix, so the definition compiles at tier 1, where the checker scans every
\ body.

: RECOVERY-MODE? ( -- bool )
   SCRIPT-ARGC 1 ?do
      i SCRIPT-ARGV$ RECOVERY$ CORE-STR= if true unloop exit then
   loop false ;

: RECOVERY-ROW ( -- )
   tier@ {: was:n :}
   1 set-tier
   MULTI-ERR-BEGIN
   s" : NW-RECOVERY ( n -- bool ) 1 + ;" evaluate-closed
   MULTI-ERR-END {: refused:n :}
   was set-tier
   refused 1 <> if s" window: the recovery definition was not refused" 76 die then ;

\ --- the window prefix, native-build's LOAD-TARGET through cell-effects.f ---

: LOAD-WINDOW ( ptr u8 -- )
   {: source:ptr :}
   s" src/core/util.f" included
   s" src/core/cell.f" included
   s" src/core/pointer-storage.f" included
   s" src/core/engine-error.f" included
   s" src/core/exec-vector.f" included
   s" src/core/checker-fetch-abi.f" included
   s" src/core/checker-owner-extension.f" included
   s" src/core/checker-owner-abi.f" included
   s" src/habu/created-body-abi.f" included
   s" src/habu/prims.f" included
   s" src/core/does-clause.f" included
   s" src/core/checker.f" included
   source CHECK-RETAINED-OWNER
   s" src/core/engine-error-effects.f" included
   s" src/core/lower-cert-base.f" included
   s" src/core/type-schema.f" included
   s" src/core/type-family.f" included
   s" src/core/render.f" included
   s" src/core/sumtype.f" included
   s" src/core/layout-buffer.f" included
   s" src/core/layout-valid.f" included
   RECOVERY-MODE? if RECOVERY-ROW then
   TAKE-OVER
   \ A source-loaded retained compiler must now use the replacement owner too.
   CHECKER-OWNER:CAPTURE-PREPARE
   s" src/core/check-hook.f" included
   s" src/core/roles.f" included
   s" src/core/cell-effects.f" included
   \ Optional dependency paths are loaded by this retained continuation: the
   \ replacement prefix has not installed its own include words yet.
   SCRIPT-ARGC 1 ?do
      i SCRIPT-ARGV$
      \ The retained compiler owns literals after its namespace is retired.
      2dup s" --literals" CORE-STR= if
         2drop NSTR:WINDOW-OPEN
      else 2dup RECOVERY$ CORE-STR= if 2drop else i LOAD-OPTIONAL then then
   loop
   PATH$ included
   \ Call the retained production detector after the replacement checker loads.
   ADDRESS-CELLS:CURRENT? if 1 else 0 then ADDRESS-ABI @ <> if
      s" window: address-cell ABI changed during reset" 76 die
   then ;

\ A quotation carries no locals, so the retained owner crosses the catch here.
PTR-VARIABLE SRC-OWNER

public

\ The verdict prints as `window: <code>`, 0 accepted or the thrown code, so a
\ refusal stays distinct from an acceptance; a deadline exits PROC-TIMEOUT-RC
\ instead (src/lib/test/subject.f RC-TIMED-OUT).
: RUN ( ptr u8 n -- )
   PATH!
   ADDRESS-CELLS:CURRENT? if 1 else 0 then ADDRESS-ABI !
   CHECKER-OWNER SRC-OWNER !
   SRC-OWNER @ LOGICAL-RESET
   [: SRC-OWNER @ LOAD-WINDOW ;] catch {: code:n :}
   code E-PROC-TIMEOUT = if
      s" window: a deadline expired in the window load" PROC-TIMEOUT-RC die
   then
   s" window: " type code . ;

;package

\ Entry: the first script argument is the fixture path.
0 SCRIPT-ARGV$ NW-OWNER:RUN
