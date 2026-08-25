\ report-test.f - coverage for the report.f declarative table engine: one column
\ set rendered to both CSV and a Markdown table.

require lib/test.f
require lib/report.f

: RT-COLS ( -- ) REPORT:RESET
   s" id" REPORT:AL-R [: RENDER:RB# ;] REPORT:COL+
   s" sq" REPORT:AL-R [: dup * RENDER:RB# ;] REPORT:COL+ ;

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
   RT-COLS  RENDER:RESET 3 REPORT:CSV  RENDER:RB$ EXP-CSV STR= T-ASSERT
   RT-COLS  RENDER:RESET 3 REPORT:MD   RENDER:RB$ EXP-MD  STR= T-ASSERT ;

RPT-RUN

\ ---- transactional column-capacity regressions -----------------------------
\ Reopen the package so the tests can read the private column store directly:
\ the committed count, each stored column, and the adjacent canary cell. These
\ prove the 65th column is rejected BEFORE any store, so a full table never
\ corrupts the count, a prior column, or neighbouring memory (the pre-fix bug
\ wrote three header/alignment fields through unchecked address arithmetic
\ before the typed store's bounds check could reject the overflow).
package REPORT

: RT-NOP ( n -- ) drop ;                    \ a cell emitter with the COL+ [ n -- ] effect

: RT-ADD ( n -- ) {: i:n :}                 \ append column i: fixed header, alignment keyed to i
   s" c" i 1 and [: RT-NOP ;] COL+ ;

: RT-FILL ( -- )                            \ RESET, then append exactly COL-MAX columns
   RESET  COL-MAX 0 ?do i RT-ADD loop ;

: RT-OVERFLOW ( -- ) COL-MAX RT-ADD ;       \ the (COL-MAX+1)-th append; must throw before any store

: RT-CHECK-ALL ( -- )                       \ every committed column still holds its keyed values
   COL-MAX 0 ?do
      i COL-HDR@ s" c" T$=
      i COL-AL@ i 1 and T=
   loop ;

: RT-REUSE ( -- )                           \ RESET clears the count; a fresh short table reads back correctly
   RESET
   COL-N @ 0 T=
   0 RT-ADD  1 RT-ADD
   COL-N @ 2 T=
   0 COL-HDR@ s" c" T$=   0 COL-AL@ 0 T=
   1 COL-HDR@ s" c" T$=   1 COL-AL@ 1 T= ;

: RT-CAP-RUN ( -- )
   RT-FILL                                  \ exactly COL-MAX columns succeed
   COL-N @ COL-MAX T=
   RT-CHECK-ALL                             \ all committed columns hold their keyed values
   [: RT-OVERFLOW ;] catch E-REPORT-CAPACITY T=   \ the 65th is rejected with the named code
   COL-N @ COL-MAX T=                        \ count unchanged by the rejected add
   RT-CHECK-ALL                             \ every prior column byte-identical after the reject
   COL-CANARY @ COL-CANARY-INIT T=          \ adjacent memory intact
   RT-REUSE ;                               \ reset + reuse behaves

RT-CAP-RUN

;package

T-REPORT
