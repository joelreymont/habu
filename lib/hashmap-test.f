\ hashmap-test.f - coverage for HASH64 / HM-PROBE / HM-CLEAR.
\ Load after lib/errors.f lib/string.f lib/test.f lib/hashmap.f.

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/hashmap.f

8 constant HMT-CAP
create HMT-K HMT-CAP cells allot
create HMT-U HMT-CAP cells allot

: HMT-SLOT ( n -- n ) {: key :} HMT-K HMT-U HMT-CAP key HM-PROBE ;
: HMT-INS ( n -- ) {: key :}        \ insert if absent (mark slot used)
   key HMT-SLOT {: s :}
   HMT-U s cells + @ 0= if key HMT-K s cells + ! -1 HMT-U s cells + ! then ;

: HM-RUN ( -- )
   T-RESET
   \ HASH64 is identity for small keys (high bits zero)
   5  HASH64 5  T=
   17 HASH64 17 T=
   \ fresh table: every slot empty
   HMT-U HMT-CAP HM-CLEAR
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
   \ HM-CLEAR resets: the once-used slot 5 is empty again
   HMT-U HMT-CAP HM-CLEAR
   HMT-U 5 cells + @ 0 T= ;

HM-RUN
T-REPORT
