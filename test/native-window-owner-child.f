\ native-window-owner-child.f - tools/native-build.f's window, reduced to the
\ checker handover. RESET-ADDRESS-ROWS through LOGICAL-RESET are that tool's
\ words, copied; LOAD-WINDOW is its LOAD-TARGET through src/core/cell-effects.f,
\ with one fixture after the checked call-store handoff. Optional later script
\ arguments load the fixture's dependencies through the retained continuation.
\
\ The verdict goes to stdout as `window: <code>` - 0 when the window accepted
\ the fixture, the thrown code when it refused. Printing rather than exiting
\ keeps a refusal distinguishable from an acceptance for the parent test.
\
\ lib/memory.f is required HERE and in no window file, so CAD-NUM's families
\ are host-only: the retained checker knows them and the window must not.

require lib/errors.f
require src/core/prefix-boundary.f
require lib/string.f
require lib/memory.f
require src/habu/address-cells.f

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

\ --- copied from tools/native-build.f ---------------------------------------

\ Keep the host's actual engine declarations -- the cells below ITS heap floor,
\ which the engine publishes at boot in BOOT-LAYOUT:HEAP-START-CELL -- and discard
\ every declaration belonging to the retired heap above it. Filtered rather than
\ truncated because the rows are not partitioned in registration order. The
\ reasoning, the source-constant fallback and its structural bound are stated once
\ in tools/native-build.f; this fixture reaches the window through the same steps.
: ADDR-ROWS ( -- n ) data-base SNAP-RELOC:XTCELL-N-CELL + @ ;

: ADDR-ROW@ ( n -- n ) ADDRESS-CELLS:ROW@ ;

\ The layout name itself, not habu2.f's host-side mirror, and no source-constant
\ fallback: this fixture only ever runs under the engine the tree just built, so
\ the cell is always there. Saying so out loud is the point -- a silent zero would
\ classify every row as heap and empty the table, which is not a window this
\ fixture can claim anything about.
: HOST-HEAP-START ( -- n ) data-base BOOT-LAYOUT:HEAP-START-CELL + @ ;

: KEEP-ROWS-BELOW ( n -- ) ADDRESS-CELLS:KEEP-BELOW ;

: RESET-ADDRESS-ROWS ( -- )
   HOST-HEAP-START {: floor:n :}
   floor 0= if s" window: host publishes no heap floor" 76 die then
   floor KEEP-ROWS-BELOW ;

defer RESET-SOURCE ( -- )
defer IMPORT-CHECKED ( ptr u8 -- )

\ Resolve the current source owner in its own package.
TRUSTED: CHECKER-OWNER ( -- ptr u8 )
   s" package CHECKER-REG DECLARATIONS ;package" evaluate ;

\ These execution tokens belong to the retained/target private checker owners.
TRUSTED: RESET-CHECKER ( ptr u8 -- ) {: owner:ptr :}
   owner 0= if exit then
   owner NCOMP-DISPATCH:DECL-RESET-OFF + CELL-VIEW @ is RESET-SOURCE
   RESET-SOURCE ;

TRUSTED: TRANSFER-CHECKER ( ptr u8 -- ) {: source:ptr :}
   CHECKER-OWNER {: owner:ptr :}
   owner 0= if s" window: target checker owner missing" 76 die then
   owner NCOMP-DISPATCH:DECL-TRANSFER-OFF + CELL-VIEW @ is IMPORT-CHECKED
   source IMPORT-CHECKED ;

\ Installing a replacement's callbacks must not claim a nonzero source owner
\ before the explicit transfer below; the retained compiler still uses it.
: CHECK-RETAINED-OWNER ( ptr u8 -- ) {: source:ptr :}
   data-base NCOMP-DISPATCH:DECL-CELL + 0 ptr-field @ source <> if
      s" window: replacement checker claimed the source before transfer" 76 die then
   data-base NCOMP-DISPATCH:TARGET-DECL-CELL + 0 ptr-field @ source = if
      s" window: replacement checker did not publish a distinct target" 76 die then ;

TRUSTED: LOGICAL-RESET ( ptr u8 -- )
   0 set-check
   0 set-top-check
   RESET-CHECKER
   CORE-PREFIX:FIRST-RECORD seed-ndict!
   RESET-ADDRESS-ROWS ;

\ --- the window prefix, native-build's LOAD-TARGET through cell-effects.f ---

: LOAD-WINDOW ( ptr u8 -- ) {: source:ptr :}
   s" src/core/util.f" included
   s" src/core/cell.f" included
   s" src/core/pointer-storage.f" included
   s" src/core/engine-error.f" included
   s" src/core/exec-vector.f" included
   s" src/core/checker-owner-abi.f" included
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
   source TRANSFER-CHECKER
   \ A source-loaded retained compiler must now use the replacement owner too.
   CHECKER-OWNER:CAPTURE-PREPARE
   s" src/core/check-hook.f" included
   s" src/core/roles.f" included
   s" src/core/cell-effects.f" included
   \ Optional dependency paths are loaded by this retained continuation: the
   \ replacement prefix has not installed its own include words yet.
   SCRIPT-ARGC 1 ?do i SCRIPT-ARGV$ included loop
   PATH$ included
   \ Call the retained production detector after the replacement checker loads.
   ADDRESS-CELLS:CURRENT? if 1 else 0 then ADDRESS-ABI @ <> if
      s" window: address-cell ABI changed during reset" 76 die
   then ;

\ A quotation carries no locals, so the retained owner crosses the catch here.
PTR-VARIABLE SRC-OWNER

public

: RUN ( ptr u8 n -- )
   PATH!
   ADDRESS-CELLS:CURRENT? if 1 else 0 then ADDRESS-ABI !
   CHECKER-OWNER SRC-OWNER !
   SRC-OWNER @ LOGICAL-RESET
   [: SRC-OWNER @ LOAD-WINDOW ;] catch
   s" window: " type . ;

;package

\ Entry: the first script argument is the fixture path.
0 SCRIPT-ARGV$ NW-OWNER:RUN
