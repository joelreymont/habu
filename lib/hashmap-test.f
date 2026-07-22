\ hashmap-test.f - coverage for HM:HASH64 / HM:PROBE / HM:CLEAR.
\ Load after lib/errors.f lib/string.f lib/test.f lib/hashmap.f.

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
: HMF-FILL ( -- )                     \ keys 0..3 land in slots 0..3 (identity hash), all used
   HMF-U HMF-CAP HM:CLEAR
   0 HMF-K 0 cells + !  -1 HMF-U 0 cells + !
   1 HMF-K 1 cells + !  -1 HMF-U 1 cells + !
   2 HMF-K 2 cells + !  -1 HMF-U 2 cells + !
   3 HMF-K 3 cells + !  -1 HMF-U 3 cells + ! ;

: HM-RUN ( -- )
   T-RESET
   \ HM:HASH64 is identity for small keys (high bits zero)
   5  HM:HASH64 5  T=
   17 HM:HASH64 17 T=
   \ fresh table: every slot empty
   HMT-U HMT-CAP HM:CLEAR
   5 HMT-INS  17 HMT-INS  33 HMT-INS
   \ 5 -> slot 5 ; 17 -> slot 1 ; 33 (mod 8 = 1) collides with 17 -> probes to slot 2
   5  HMT-SLOT 5 T=
   17 HMT-SLOT 1 T=
   33 HMT-SLOT 2 T=
   \ re-probing an existing key returns its slot (idempotent), not a new one
   5 HMT-SLOT 5 T=
   \ an untouched slot stays empty
   HMT-U 7 cells + @ 0 T=
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
   [: HMT-U 3 HM:CLEAR ;] E-HM-CAP TTHROWSQ ;

HM-RUN
T-REPORT
