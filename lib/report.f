\ report.f - a small declarative table reporting engine for the analyzers. Declare
\ a table's columns once with COL+ (header text, alignment, and a cell quotation
\ ( row -- ) that appends the formatted cell into the render buffer); then render the
\ SAME column set as CSV (REPORT:CSV) or a Markdown table (REPORT:MD). This replaces the
\ open-coded "RENDER:LBAR cell RENDER:BAR cell RENDER:RBAR" / "cell RENDER:CM cell RENDER:CM" repetition with one
\ column declaration per table. Bespoke prose, nested JSON, and per-row sub-strings
\ compose around these with the render.f words. Quotations are stored as xts and
\ EXECUTEd per (row, column).
\
\ The module lives in `package REPORT`. External callers use the qualified public
\ API (REPORT:RESET, REPORT:COL+, REPORT:CSV, REPORT:MD and the REPORT:AL-L /
\ REPORT:AL-R alignment constants); the column store and per-cell helpers are
\ package-private.
\
\ Column storage is transactional. A column's header text pointer, header length,
\ and alignment are owned as ONE typed record (the `col` structure) held in ONE
\ bounds-checked buffer (COL-AT), so no field is ever written through raw
\ `create ... allot` + `cells +` address arithmetic. COL+ preflights the 64-column
\ capacity BEFORE the first store and bumps the committed count (COL-N) only after
\ the record and the per-column emitter are both stored, so adding a 65th column
\ throws E-REPORT-CAPACITY without mutating any prior column, the count, or
\ adjacent memory. The emitter is an xt<[ n -- ]>; the checker cannot hold an xt in
\ a structure field (it rejects `[` as a payload type), so it rides its own,
\ equally bounded, typed buffer (COL-XT) indexed by the SAME committed count.
\ A structure must be public to have a constructor at all (a private structure has
\ no construction surface), so REPORT-COL:MAKE / REPORT-COL:UNMAKE are generated as
\ an internal record ABI; they are not part of the documented REPORT API.

require lib/errors.f
require lib/render.f

package REPORT

public

\ One column's addressable fields as a single record: ha = header text pointer,
\ hn = header text byte length, al = alignment code (AL-L / AL-R). (Structure bodies
\ take no inline comments, hence the field notes here.)
STRUCTURE col<>
  FIELD ha ptr u8
  FIELD hn n
  FIELD al n
;STRUCTURE

private

64 constant COL-MAX
COL-MAX TYPED-BUFFER COL-AT col        \ per-column header + alignment record ( n -- ptr col )
COL-MAX TYPED-BUFFER COL-XT [ n -- ]   \ per-column cell emitter ( row -- ); typed so store + execute stay checked
create COL-CANARY 1 cells allot        \ sentinel cell just past the column buffers: a corrupt store would land here
$5EED constant COL-CANARY-INIT         \ known value the canary must keep across a rejected COL+
COL-CANARY-INIT COL-CANARY !
variable COL-N                         \ committed column count; incrementing it is the transaction commit point
variable TBL-R  variable TBL-C

public

0 constant AL-L   1 constant AL-R           \ column alignment: left / right

private

: COL-FULL? ( -- bool ) COL-N @ COL-MAX >= ;                       \ no room for another column
: COL-HDR@ ( n -- ptr u8 n ) COL-AT @ REPORT-COL:UNMAKE drop ;     \ header pointer + length (drop alignment)
: COL-AL@  ( n -- n )        COL-AT @ REPORT-COL:UNMAKE nip nip ;   \ alignment code (drop header pointer + length)
: COL-CELL ( n n -- ) {: row:n c:n :} row c COL-XT @ execute ;     \ emit one cell

public

: RESET ( -- ) 0 COL-N ! ;
: COL+ ( ptr u8 n n [ n -- ] -- ) {: ha:ptr u:n al:n q :}   \ q = xt<[ n -- ]>: typed-local-lint: allow-bare-local
   COL-FULL? if E-REPORT-CAPACITY throw then                \ preflight capacity before ANY store
   COL-N @ {: idx:n :}
   ha u al REPORT-COL:MAKE idx COL-AT !                     \ store header + alignment as one record
   q idx COL-XT !                                           \ store the cell emitter at the same index
   idx 1+ COL-N ! ;                                         \ commit the row + count last

\ CSV: comma-joined header + one comma-joined line per row
: CSV ( n -- ) {: nrows:n :}
   0 TBL-C ! begin TBL-C @ COL-N @ < while
      TBL-C @ 0 > if RENDER:CM then  TBL-C @ COL-HDR@ RENDER:RB+  TBL-C @ 1+ TBL-C !
   repeat RENDER:NL
   0 TBL-R ! begin TBL-R @ nrows < while
      0 TBL-C ! begin TBL-C @ COL-N @ < while
         TBL-C @ 0 > if RENDER:CM then  TBL-R @ TBL-C @ COL-CELL  TBL-C @ 1+ TBL-C !
      repeat RENDER:NL
      TBL-R @ 1+ TBL-R !
   repeat ;

\ Markdown: header row, alignment row (---:/---), then one row per record
: MD ( n -- ) {: nrows:n :}
   0 TBL-C ! begin TBL-C @ COL-N @ < while
      TBL-C @ 0 = if RENDER:LBAR else RENDER:BAR then  TBL-C @ COL-HDR@ RENDER:RB+  TBL-C @ 1+ TBL-C !
   repeat RENDER:RBAR RENDER:NL
   0 TBL-C ! begin TBL-C @ COL-N @ < while
      TBL-C @ 0 = if RENDER:LBAR else RENDER:BAR then
      TBL-C @ COL-AL@ AL-R = if s" ---:" RENDER:RB+ else s" ---" RENDER:RB+ then
      TBL-C @ 1+ TBL-C !
   repeat RENDER:RBAR RENDER:NL
   0 TBL-R ! begin TBL-R @ nrows < while
      0 TBL-C ! begin TBL-C @ COL-N @ < while
         TBL-C @ 0 = if RENDER:LBAR else RENDER:BAR then  TBL-R @ TBL-C @ COL-CELL  TBL-C @ 1+ TBL-C !
      repeat RENDER:RBAR RENDER:NL
      TBL-R @ 1+ TBL-R !
   repeat ;

;package
