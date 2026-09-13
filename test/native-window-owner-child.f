\ native-window-owner-child.f - tools/native-build.f's window, reduced to the
\ checker handover. RESET-ADDRESS-ROWS through LOGICAL-RESET are that tool's
\ words, copied; LOAD-WINDOW is its LOAD-TARGET through src/core/cell-effects.f,
\ with one fixture after the checked call-store handoff.
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

package NW-OWNER

$400 constant PATH-CAP

create PATH-BUF PATH-CAP allot
variable PATH-U

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

: ADDR-ROWS! ( n -- ) data-base SNAP-RELOC:XTCELL-N-CELL + ! ;

: ADDR-ROW@ ( n -- n ) {: k:n :}
   k cells data-base SNAP-RELOC:XTCELL-ROWS-OFF + + @ ;

: ADDR-ROW! ( n n -- ) {: k:n row:n :}
   row k cells data-base SNAP-RELOC:XTCELL-ROWS-OFF + + ! ;

\ The layout name itself, not habu2.f's host-side mirror, and no source-constant
\ fallback: this fixture only ever runs under the engine the tree just built, so
\ the cell is always there. Saying so out loud is the point -- a silent zero would
\ classify every row as heap and empty the table, which is not a window this
\ fixture can claim anything about.
: HOST-HEAP-START ( -- n ) data-base BOOT-LAYOUT:HEAP-START-CELL + @ ;

: KEEP-ROWS-BELOW ( n -- ) {: floor:n :}
   0 ADDR-ROWS 0 ?do
      i ADDR-ROW@ {: row:n :}
      row SNAP-RELOC:XTCELL-OFF-MASK and floor < if
         dup row ADDR-ROW! 1+
      then
   loop
   ADDR-ROWS! ;

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
   s" src/core/checker.f" included
   s" src/core/engine-error-effects.f" included
   s" src/core/lower-cert-base.f" included
   s" src/core/type-schema.f" included
   s" src/core/type-family.f" included
   s" src/core/render.f" included
   s" src/core/sumtype.f" included
   s" src/core/layout-buffer.f" included
   s" src/core/layout-valid.f" included
   source TRANSFER-CHECKER
   s" src/core/check-hook.f" included
   s" src/core/roles.f" included
   s" src/core/cell-effects.f" included
   PATH$ included ;

\ A quotation carries no locals, so the retained owner crosses the catch here.
PTR-VARIABLE SRC-OWNER

public

: RUN ( ptr u8 n -- )
   PATH!
   CHECKER-OWNER SRC-OWNER !
   SRC-OWNER @ LOGICAL-RESET
   [: SRC-OWNER @ LOAD-WINDOW ;] catch
   s" window: " type . ;

;package

\ Entry: the fixture path is the one script argument.
0 SCRIPT-ARGV$ NW-OWNER:RUN
