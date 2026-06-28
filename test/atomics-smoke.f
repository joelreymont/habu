here 7 and 8 swap - 7 and allot
variable CTR
CTR 7 and . cr            \ expect 0 (aligned)
0 CTR !
5 CTR atomic-add . cr     \ expect 0  (LDADDAL old)
CTR @ . cr                \ expect 5
CTR atomic@ . cr          \ expect 5  (LDAR)
7 CTR atomic!             \ STLR
CTR @ . cr                \ expect 7
7 9 CTR atomic-cas . cr   \ expect 7  (CASAL swap)
CTR @ . cr                \ expect 9
50 99 CTR atomic-cas . cr \ expect 9  (no swap)
CTR @ . cr                \ expect 9
fence
s" aligned-atomics ok" type cr
