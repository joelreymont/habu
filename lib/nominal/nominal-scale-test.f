\ nominal-scale-test.f - scale + non-quadratic growth suite for the sealed
\ immutable nominal collection (dot habu-add-immutable-nominal-9290a81f).
\ Run: bin/hb --load lib/nominal/nominal-scale-test.f
\
\ Proves the substrate at the target scale the by-value PRODUCT could never reach:
\ compose / REMAP / UNION / SNAPSHOT / replay at 4096 distinct bindings, then a
\ MEASURED non-quadratic bound - build time is timed at 1024 and 4096 bindings and
\ the 4x-size ratio must stay well under the quadratic 16x (append is O(1), FREEZE
\ sorts once O(n log n)). Direct-loaded (lib/cad-num-types-test.f precedent);
\ reopens package NOM for the timed builder loop.

require lib/nominal/snapshot.f
require lib/test.f
require lib/time.f

create NS-BUF $80000 allot                    \ 512 KiB snapshot buffer
variable NS-T1  variable NS-T4

4096 constant NS-N                             \ target distinct-binding scale

package NOM
private

: NS-MKB ( n n n -- binding ) {: a:n b:n slot:n :}   a ROOT swap CONS b CONS slot BIND ;

: NS-BUILD ( n n -- row ) {: m:n tag:n :}      \ m distinct bindings ROOT/i/tag
   NEW m 0 do i tag i NS-MKB ADD loop FREEZE ;

: NS-TIME ( n -- n ) {: m:n :}                 \ ns to build+freeze a row of m distinct bindings
   RESET
   TIME-MONO-NS {: t0:n :}
   m 0 NS-BUILD drop
   TIME-MONO-NS t0 - ;

\ ---- functional scale: compose / remap / union / snapshot / replay -----------
: NS-FUNC ( -- )
   RESET
   NS-N 0 NS-BUILD {: ra:row :}
   ra SIZE NS-N T=                             \ 4096 distinct bindings in one row
   ra 7 REMAP SIZE NS-N T=                     \ remap preserves count at scale
   NS-N 1 NS-BUILD {: rb:row :}               \ a disjoint second 4096-row (tag 1)
   ra rb UNION SIZE NS-N 2 * T=               \ union of two disjoint 4096-rows
   RESET
   NS-N 0 NS-BUILD drop                        \ one 4096-binding row published
   NS-BUF $80000 SNAPSHOT {: len:n :}
   RESET  NS-BUF len RESTORE 1 T= ;            \ replay the row from the snapshot

\ ---- measured non-quadratic growth -------------------------------------------
: NS-WARM ( -- )   256 NS-TIME drop ;          \ page in code paths before timing
: NS-MEASURE ( -- )
   NS-WARM
   1024 NS-TIME NS-T1 !
   4096 NS-TIME NS-T4 !
   s" scale: build(1024)=" type NS-T1 @ .
   s" ns build(4096)=" type NS-T4 @ .
   s" ns  4x-size ratio x100=" type NS-T4 @ 100 * NS-T1 @ 1 max / . cr
   \ 4x size: quadratic -> ~16x, n log n -> ~4.7x. Require < 10x to exclude quadratic.
   NS-T4 @  NS-T1 @ 10 *  < TTRUE ;

: NS-ALL ( -- )
   T-RESET
   NS-FUNC
   NS-MEASURE
   T-REPORT ;

NS-ALL
;package
