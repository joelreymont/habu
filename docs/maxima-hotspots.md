# Maxima Hotspots

- scale: `1`
- heap_mb: `1024`
- nursery_mb: `32`

| workload | jit_ns | interp_ns | interp/jit |
|---|---:|---:|---:|
| ratsimp | 340136084 | 341057083 | 1.003 |
| integrate | 165999375 | 166190292 | 1.001 |
| factor | 87909500 | 85881334 | 0.977 |
| solve | 12719291 | 12555208 | 0.987 |
| determinant | 1179625 | 1244583 | 1.055 |

- loader_ns: jit=`12255589167`, interp=`12288330458`
- jit_compiled: jit=`0`, interp=`0`
- jit_adm: cand=`4145/4145`, elig=`2/2`, comp=`0/0`, sk_speed=`0/0`, sk_safety=`4143/4143`, sk_chunk=`0/0`, fail_unsup=`2/0`, fail_other=`0/2`
- jit_gate: pass=`False`, wins=`1/5` (min speedup `1.010`, min wins `2`), compiled=`0` (min `32`), delta=`0`
