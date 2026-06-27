\ report.f - a small declarative table reporting engine for the analyzers. Declare
\ a table's columns once with COL+ (header text, alignment, and a cell quotation
\ ( row -- ) that appends the formatted cell into the render buffer); then render the
\ SAME column set as CSV (TBL-CSV) or a Markdown table (TBL-MD). This replaces the
\ open-coded "LBAR cell BAR cell RBAR" / "cell CM cell CM" repetition with one
\ column declaration per table. Bespoke prose, nested JSON, and per-row sub-strings
\ compose around these with the render.f words. Quotations are stored as xts and
\ EXECUTEd per (row, column). Depends on odin/render.f.

64 constant COL-MAX
0 constant AL-L   1 constant AL-R           \ column alignment: left / right
create COL-HA COL-MAX cells allot  create COL-HN COL-MAX cells allot   \ header text
create COL-AL COL-MAX cells allot                                       \ alignment
create COL-XT COL-MAX cells allot                                       \ cell quotation ( row -- )
variable COL-N
variable TBL-R  variable TBL-C

: TBL-RESET ( -- ) 0 COL-N ! ;
: COL+ ( ptr u8 n n [ n -- ] -- ) {: ha u al q :}
   ha COL-HA COL-N @ cells + !  u COL-HN COL-N @ cells + !
   al COL-AL COL-N @ cells + !  q COL-XT COL-N @ cells + !
   COL-N @ 1+ COL-N ! ;

: COL-HDR@ ( n -- ptr u8 n ) {: c :} COL-HA c cells + @  COL-HN c cells + @ ;
: COL-CELL ( n n -- ) {: row c :} row COL-XT c cells + @ execute ;   \ emit one cell

\ CSV: comma-joined header + one comma-joined line per row
: TBL-CSV ( n -- ) {: nrows :}
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
: TBL-MD ( n -- ) {: nrows :}
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
