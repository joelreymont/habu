# Maxima Hotspots

- scale: `1`
- heap_mb: `1024`
- nursery_mb: `32`

| workload | jit_ns | interp_ns | interp/jit |
|---|---:|---:|---:|
| ratsimp | 337472500 | 328492750 | 0.973 |
| integrate | 165775333 | 165812417 | 1.000 |
| factor | 85827375 | 85416209 | 0.995 |
| solve | 12551292 | 12631583 | 1.006 |
| determinant | 1223375 | 1231542 | 1.007 |

- loader_ns: jit=`12518810541`, interp=`12327240333`
- jit_compiled: jit=`0`, interp=`0`
- jit_adm: cand=`4145/4145`, elig=`2/2`, comp=`0/0`, sk_speed=`4133/4133`, sk_safety=`10/10`, sk_chunk=`0/0`, fail_unsup=`2/0`, fail_other=`0/2`
- jit_gate: pass=`False`, wins=`0/5` (min speedup `1.010`, min wins `2`), compiled=`0` (min `32`), delta=`0`
