\ effect-scale-test.f - scale + non-quadratic growth suite for the CAD effect-row
\ algebra (dot habu-define-finite-cad-0bdf52ad). Run:
\   bin/hb --load src/cad/effect-scale-test.f
\
\ Proves the row imposes NO small by-value semantic capacity (the rejected 25/127
\ by-value cap): compose / REMAP / UNION / classify / SNAPSHOT / replay at 4096
\ distinct bindings, then a MEASURED non-quadratic build bound - build+freeze is
\ timed at 1024 and 4096 bindings and the 4x-size ratio must stay well under the
\ quadratic 16x (EMIT is O(1), FREEZE sorts once O(n log n)). Direct-loaded
\ (lib/nominal precedent); reopens package CAD-EFFECT for the timed builder loop.

require src/cad/effect.f
require lib/test.f
require lib/time.f

create ES-BUF $80000 allot        \ 512 KiB snapshot buffer
variable ES-T1  variable ES-T4

4096 constant ES-N                 \ target distinct-binding scale

package CAD-EFFECT
private

: ES-BUILD ( n n -- effect-row ) {: m:n base:n :}   \ m distinct state-write bindings from base
   NEW m 0 do STATE-WRITE OPERAND base i + EMIT loop FREEZE ;
: ES-READS ( n -- effect-row ) {: m:n :}            \ m distinct parameter-read bindings
   NEW m 0 do PARAM-READ OPERAND i EMIT loop FREEZE ;

: ES-TIME ( n -- n ) {: m:n :}                      \ ns to build+freeze m distinct bindings
   RESET
   TIME-MONO-NS {: t0:n :}
   m 0 ES-BUILD drop
   TIME-MONO-NS t0 - ;

\ ---- functional scale: compose / remap / union / classify / snapshot / replay --
: ES-FUNC ( -- )
   RESET
   ES-N 0 ES-BUILD {: ra:effect-row :}
   ra SIZE ES-N T=                                  \ 4096 distinct bindings in one row
   ra 7 REMAP SIZE ES-N T=                          \ REMAP preserves count at scale
   ra ROW-BARRIER? TTRUE                            \ a state-write row is a barrier at scale
   ra ROW-DUP-OK? TFALSE
   ES-N 4096 ES-BUILD {: rb:effect-row :}           \ a disjoint second 4096-row (index base 4096)
   ra rb UNION SIZE ES-N 2 * T=                     \ union of two disjoint 4096-rows -> 8192
   ES-N ES-READS ROW-DUP-OK? TTRUE                  \ a 4096 parameter-read row stays duplicable
   RESET
   ES-N 0 ES-BUILD drop                             \ one 4096-binding row published
   ES-BUF $80000 SNAPSHOT {: len:n :}
   RESET  ES-BUF len RESTORE 1 T= ;                 \ replay the row from the snapshot

\ ---- measured non-quadratic growth --------------------------------------------
: ES-WARM ( -- )   256 ES-TIME drop ;               \ page in code paths before timing
: ES-MEASURE ( -- )
   ES-WARM
   1024 ES-TIME ES-T1 !
   4096 ES-TIME ES-T4 !
   s" scale: build(1024)=" type ES-T1 @ .
   s" ns build(4096)=" type ES-T4 @ .
   s" ns  4x-size ratio x100=" type ES-T4 @ 100 * ES-T1 @ 1 max / . cr
   \ 4x size: quadratic -> ~16x, n log n -> ~4.7x. Require < 10x to exclude quadratic.
   ES-T4 @  ES-T1 @ 10 *  < TTRUE ;

: ES-ALL ( -- )
   T-RESET
   ES-FUNC
   ES-MEASURE
   T-REPORT ;

ES-ALL
;package
