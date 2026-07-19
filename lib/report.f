\ report.f - a small declarative table reporting engine for the analyzers. Declare
\ a table's columns once with COL+ (header text, alignment, and a cell quotation
\ ( row -- ) that appends the formatted cell into the render buffer); then render the
\ SAME column set as CSV (REPORT:CSV) or a Markdown table (REPORT:MD). This replaces the
\ open-coded "LBAR cell BAR cell RBAR" / "cell CM cell CM" repetition with one
\ column declaration per table. Bespoke prose, nested JSON, and per-row sub-strings
\ compose around these with the render.f words. Quotations are stored as xts and
\ EXECUTEd per (row, column).
\
\ The module lives in `package REPORT`. External callers use the qualified public
\ API (REPORT:RESET, REPORT:COL+, REPORT:CSV, REPORT:MD and the REPORT:AL-L /
\ REPORT:AL-R alignment constants); the column store and per-cell helpers are
\ package-private.

require lib/render.f

package REPORT

private

64 constant COL-MAX
create COL-HA COL-MAX cells allot  create COL-HN COL-MAX cells allot   \ header text
create COL-AL COL-MAX cells allot                                       \ alignment
COL-MAX TYPED-BUFFER COL-XT [ n -- ]                                    \ per-column cell emitter ( row -- ); typed so store+execute stay checked
variable COL-N
variable TBL-R  variable TBL-C

public

0 constant AL-L   1 constant AL-R           \ column alignment: left / right

: RESET ( -- ) 0 COL-N ! ;
: COL+ ( ptr u8 n n [ n -- ] -- ) {: ha u al q :}
   ha COL-HA COL-N @ cells + !  u COL-HN COL-N @ cells + !
   al COL-AL COL-N @ cells + !  q COL-N @ COL-XT !
   COL-N @ 1+ COL-N ! ;

private

: COL-HDR@ ( n -- ptr u8 n ) {: c :} COL-HA c cells + @  COL-HN c cells + @ ;
: COL-CELL ( n n -- ) {: row:n c:n :} row c COL-XT @ execute ;   \ emit one cell

public

\ CSV: comma-joined header + one comma-joined line per row
: CSV ( n -- ) {: nrows:n :}
   0 TBL-C ! begin TBL-C @ COL-N @ < while
      TBL-C @ 0 > if CM then  TBL-C @ COL-HDR@ RB+  TBL-C @ 1+ TBL-C !
   repeat RB-NL
   0 TBL-R ! begin TBL-R @ nrows < while
      0 TBL-C ! begin TBL-C @ COL-N @ < while
         TBL-C @ 0 > if CM then  TBL-R @ TBL-C @ COL-CELL  TBL-C @ 1+ TBL-C !
      repeat RB-NL
      TBL-R @ 1+ TBL-R !
   repeat ;

\ Markdown: header row, alignment row (---:/---), then one row per record
: MD ( n -- ) {: nrows:n :}
   0 TBL-C ! begin TBL-C @ COL-N @ < while
      TBL-C @ 0 = if LBAR else BAR then  TBL-C @ COL-HDR@ RB+  TBL-C @ 1+ TBL-C !
   repeat RBAR RB-NL
   0 TBL-C ! begin TBL-C @ COL-N @ < while
      TBL-C @ 0 = if LBAR else BAR then
      COL-AL TBL-C @ cells + @ AL-R = if s" ---:" RB+ else s" ---" RB+ then
      TBL-C @ 1+ TBL-C !
   repeat RBAR RB-NL
   0 TBL-R ! begin TBL-R @ nrows < while
      0 TBL-C ! begin TBL-C @ COL-N @ < while
         TBL-C @ 0 = if LBAR else BAR then  TBL-R @ TBL-C @ COL-CELL  TBL-C @ 1+ TBL-C !
      repeat RBAR RB-NL
      TBL-R @ 1+ TBL-R !
   repeat ;

;package
