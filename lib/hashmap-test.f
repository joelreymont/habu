\ hashmap-test.f - coverage for HM:HASH64 / HM:PROBE / HM:CLEAR.

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/hashmap.f

8 constant HMT-CAP
create HMT-K HMT-CAP cells allot
create HMT-U HMT-CAP cells allot

: HMT-SLOT ( n -- n ) {: key:n :} HMT-K HMT-U HMT-CAP key HM:PROBE ;
: HMT-INS ( n -- ) {: key:n :}        \ insert if absent (mark slot used)
   key HMT-SLOT {: s:n :}
   HMT-U s cells + @ 0= if key HMT-K s cells + ! -1 HMT-U s cells + ! then ;

\ full-table fixture: a power-of-two table with every slot occupied by a distinct
\ key, so probing an absent key must exhaust the scan and throw rather than hang.
4 constant HMF-CAP
create HMF-K HMF-CAP cells allot
create HMF-U HMF-CAP cells allot
: HMF-FILL ( -- )                     \ slots written directly, so the fill owes the hash nothing
   HMF-U HMF-CAP HM:CLEAR
   0 HMF-K 0 cells + !  -1 HMF-U 0 cells + !
   1 HMF-K 1 cells + !  -1 HMF-U 1 cells + !
   2 HMF-K 2 cells + !  -1 HMF-U 2 cells + !
   3 HMF-K 3 cells + !  -1 HMF-U 3 cells + ! ;


\ Probe-length regression. A hash whose entropy never reaches the bits PROBE
\ masks leaves the table CORRECT but linear - every probe confirms its candidate
\ against keys[] - so only a probe count, never an equality, catches it. Both
\ shapes hold HMP-N distinct keys in HMP-CAP slots: load 0.5, mean 1.5 probes.
8192 constant HMP-CAP
4096 constant HMP-N
2 constant HMP-BOUND                  \ mean probes allowed per successful lookup
40 constant HMP-HIGH-SHIFT            \ key entropy parked above bit 33

create HMP-K HMP-CAP cells allot
create HMP-U HMP-CAP cells allot
variable HMP-TOTAL
variable HMP-I


: HMP-HOME ( n -- n ) {: key:n :}     \ slot the probe starts at
   key HM:HASH64 HMP-CAP 1- and ;


: HMP-COST+ ( n n -- ) {: slot:n key:n :}
   slot key HMP-HOME - HMP-CAP 1- and 1+  \ ( n ) displacement from home, +1 probe
   HMP-TOTAL @ + HMP-TOTAL ! ;


: HMP-INS ( n -- ) {: key:n :}        \ insert an absent key, counting its probes
   HMP-K HMP-U HMP-CAP key HM:PROBE {: slot:n :}
   slot key HMP-COST+
   key HMP-K slot cells + !
   -1 HMP-U slot cells + ! ;


: HMP-PROBES ( n -- n ) {: shift:n :} \ total probes for HMP-N keys whose entropy sits at `shift`
   HMP-U HMP-CAP HM:CLEAR
   0 HMP-TOTAL !
   0 HMP-I !
   begin HMP-I @ HMP-N < while
      HMP-I @ shift lshift HMP-INS
      HMP-I @ 1+ HMP-I !
   repeat
   HMP-TOTAL @ ;


: HMP-LIMIT ( -- n ) HMP-N HMP-BOUND * ;


\ A bound keeps both sides: a label alone leaves a failure with no measurement.
: HMP-UNDER-BOUND ( n -- ) {: total:n :}
   total HMP-LIMIT >= if
      s" assert: probes " type total .  s" limit " type HMP-LIMIT .
   then
   total HMP-LIMIT < TTRUE ;


: HM-RUN ( -- )
   T-RESET
   \ HM:HASH64 is fmix64: known answers recomputed from murmur3's published
   \ constants, so a changed constant or a non-wrapping multiply shows up here
   5  HM:HASH64 $D66AD737D54C5575 T=
   17 HM:HASH64 $EB269B691FF3FB36 T=
   \ fresh table: every slot empty
   HMT-U HMT-CAP HM:CLEAR
   5 HMT-INS  17 HMT-INS  4 HMT-INS
   \ 5 -> slot 5 ; 17 -> slot 6 ; 4 shares 5's home slot and probes past both
   5  HMT-SLOT 5 T=
   17 HMT-SLOT 6 T=
   4  HMT-SLOT 7 T=
   \ re-probing an existing key returns its slot (idempotent), not a new one
   5 HMT-SLOT 5 T=
   \ an untouched slot stays empty
   HMT-U 3 cells + @ 0 T=
   \ a never-inserted key resolves to an empty slot (used = 0 there)
   HMT-U 99 HMT-SLOT cells + @ 0 T=
   \ HM:CLEAR resets: the once-used slot 5 is empty again
   HMT-U HMT-CAP HM:CLEAR
   HMT-U 5 cells + @ 0 T=
   \ full table + absent key: bounded scan throws E-HM-FULL instead of looping forever
   [: HMF-FILL  HMF-K HMF-U HMF-CAP 99 HM:PROBE drop ;] E-HM-FULL TTHROWSQ
   \ cap=0 makes the mask an identity that returns an out-of-bounds slot: rejected
   [: HMT-K HMT-U 0 12345 HM:PROBE drop ;] E-HM-CAP TTHROWSQ
   \ cap=3 is not a power of two, so the mask probes only a subset: rejected
   [: HMT-K HMT-U 3 5 HM:PROBE drop ;] E-HM-CAP TTHROWSQ
   \ CLEAR shares the invariant: a non-power-of-two cap is rejected there too
   [: HMT-U 3 HM:CLEAR ;] E-HM-CAP TTHROWSQ
   \ key entropy confined to the low 12 bits: the dense frame-index shape
   s" low 12-bit keys stay under the probe bound" T-LABEL
   0 HMP-PROBES HMP-UNDER-BOUND
   \ key entropy only above bit 33, where a fold that never mixes it down into
   \ the masked slot bits degenerates the table into a scan
   s" keys above bit 33 stay under the probe bound" T-LABEL
   HMP-HIGH-SHIFT HMP-PROBES HMP-UNDER-BOUND ;

HM-RUN
T-REPORT
