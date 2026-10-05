\ Synthetic AOT DATA sites and address rows cross file, owned-value and merge
\ boundaries without capturing or executing host instruction bytes.
require test/aot-chain-capture-lib.f

package AOT-CHAIN-SUITE

create ART-BUF FS-PATH-CAP allot   variable ART-U
: ART$ ( -- ptr u8 n ) ART-BUF ART-U @ ;

: RUN-CHILD ( -- ) s" bin/hb" RUN-ENGINE ;

: RUN-DATA-SITES ( ptr u8 n ptr u8 n -- )
   {: mode:ptr modeu:n transport:ptr transportu:n :}
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   s" test/aot-data-sites.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   ART$ >LEN PROC-ARGV+
   mode modeu >LEN PROC-ARGV+
   transport transportu >LEN PROC-ARGV+
   RUN-CHILD ;

: RUN-ADDRESS-CELLS ( ptr u8 n -- ) {: mode:ptr modeu:n :}
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   s" test/aot-address-cells.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   ART$ >LEN PROC-ARGV+
   mode modeu >LEN PROC-ARGV+
   RUN-CHILD ;

: SPAN-CASE ( ptr u8 n n -- ) {: a:ptr u:n want:n :}
   a u s" file" RUN-DATA-SITES want ROW-RC
   a u s" owned" RUN-DATA-SITES want ROW-RC ;

: ADDRESS-BUDGET-CASE ( ptr u8 n -- )
   RUN-ADDRESS-CELLS $4B ROW-RC
   s" encoded sections exceed their byte budget" ERR-SAID? ;

: PROBE-ADDRESS-STORAGE ( -- )
   s" rows" RUN-ADDRESS-CELLS 0 ROW-RC s" aot-address-cells: ok" SAID?
   s" reserve-negative" RUN-ADDRESS-CELLS REFUSE-RC ROW-RC
   s" reserve-overflow" RUN-ADDRESS-CELLS REFUSE-RC ROW-RC
   s" reserve-limit" RUN-ADDRESS-CELLS REFUSE-RC ROW-RC
   s" budget-write" ADDRESS-BUDGET-CASE
   s" budget-owned" ADDRESS-BUDGET-CASE
   s" budget-read" ADDRESS-BUDGET-CASE
   s" budget-import" ADDRESS-BUDGET-CASE
   s" budget-merge" ADDRESS-BUDGET-CASE ;

: PROBE-DATA-SITES ( -- )
   s" sites" s" file" RUN-DATA-SITES 0 ROW-RC
   s" aot-data-sites: ok" SAID?
   s" bad-code-carrier" s" file" RUN-DATA-SITES REFUSE-RC ROW-RC
   s" DATA carrier lies in the CODE band" ERR-SAID?
   s" reserve-overflow" s" file" RUN-DATA-SITES REFUSE-RC ROW-RC
   s" relocation site count exceeds the code blob bound" ERR-SAID?
   s" reserve-limit" s" file" RUN-DATA-SITES REFUSE-RC ROW-RC
   s" reserve-negative" s" file" RUN-DATA-SITES REFUSE-RC ROW-RC
   s" bad-order" s" file" RUN-DATA-SITES REFUSE-RC ROW-RC
   s" DATA sites follow CODE sites" ERR-SAID?
   s" shared-overflow" s" owned" RUN-DATA-SITES $4B ROW-RC
   s" CODE sites is larger than the buffer it fills" ERR-SAID?
   s" span-negative" $4B SPAN-CASE
   s" span-min" $4B SPAN-CASE
   s" span-zero" 0 SPAN-CASE
   s" span-cap" 0 SPAN-CASE
   s" span-large" $4B SPAN-CASE
   s" span-negative" s" merge" RUN-DATA-SITES $4B ROW-RC
   s" window DATA span exceeds what this engine can bake" ERR-SAID?
   s" span-zero" s" merge" RUN-DATA-SITES 0 ROW-RC ;

: BODY ( -- )
   SETUP
   ROOT$ s" small.aot" ART-BUF JOIN-PATH ART-U !
   PROBE-DATA-SITES
   PROBE-ADDRESS-STORAGE ;

public

: RUN ( -- )
   [: BODY ;] RUN-PROBES
   s" aot-chain-storage: ok" type cr ;

;package

AOT-CHAIN-SUITE:RUN
