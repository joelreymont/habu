# Maxima Hotspots

- scale: `1`
- heap_mb: `1024`
- nursery_mb: `32`

| workload | jit_ns | interp_ns | interp/jit |
|---|---:|---:|---:|
| integrate | 173163250 | 166381209 | 0.961 |
| factor | 54375792 | 49636333 | 0.913 |
| ratsimp | 41375334 | 36791167 | 0.889 |
| solve | 14753125 | 13732250 | 0.931 |
| determinant | 2193084 | 1998125 | 0.911 |

- loader_ns: jit=`17247930833`, interp=`15844721834`
- jit_compiled: jit=`395`, interp=`0`
- jit_adm: cand=`4139/4139`, elig=`3421/3421`, comp=`395/0`, sk_speed=`0/0`, sk_safety=`0/0`, sk_chunk=`277/277`, fail_unsup=`2906/0`, fail_other=`120/3421`
- jit_gate: pass=`False`, wins=`0/5` (min speedup `1.010`, min wins `2`), compiled=`395` (min `32`), delta=`395`
