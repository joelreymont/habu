\ report-test.f - coverage for the report.f declarative table engine: one column
\ set rendered to both CSV and a Markdown table.

require lib/test.f
require lib/report.f

: RT-COLS ( -- ) TBL-RESET
   s" id" AL-R [: RB# ;] COL+
   s" sq" AL-R [: dup * RB# ;] COL+ ;

\ expected outputs built in the SB builder (separate from the RB render buffer)
: EXP-CSV ( -- ptr u8 n )
   SB-RESET s" id,sq" SB-APPEND 10 SB-APPEND-C
   s" 0,0" SB-APPEND 10 SB-APPEND-C  s" 1,1" SB-APPEND 10 SB-APPEND-C
   s" 2,4" SB-APPEND 10 SB-APPEND-C  SB$ ;
: EXP-MD ( -- ptr u8 n )
   SB-RESET s" | id | sq |" SB-APPEND 10 SB-APPEND-C
   s" | ---: | ---: |" SB-APPEND 10 SB-APPEND-C
   s" | 0 | 0 |" SB-APPEND 10 SB-APPEND-C  s" | 1 | 1 |" SB-APPEND 10 SB-APPEND-C
   s" | 2 | 4 |" SB-APPEND 10 SB-APPEND-C  SB$ ;

: RPT-RUN ( -- )
   T-RESET
   RT-COLS  RB-RESET 3 TBL-CSV  RB$ EXP-CSV STR= T-ASSERT
   RT-COLS  RB-RESET 3 TBL-MD   RB$ EXP-MD  STR= T-ASSERT ;

RPT-RUN
T-REPORT
