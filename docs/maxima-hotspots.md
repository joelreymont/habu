# Maxima Hotspots

- scale: `1`
- heap_mb: `1024`
- nursery_mb: `32`

| workload | jit_ns | interp_ns | interp/jit |
|---|---:|---:|---:|
| ratsimp | 349049958 | 348835000 | 0.999 |
| integrate | 166942709 | 167679917 | 1.004 |
| factor | 88826625 | 87469417 | 0.985 |
| solve | 12918000 | 12903708 | 0.999 |
| determinant | 1210041 | 1301333 | 1.075 |

- loader_ns: jit=`12083243500`, interp=`12130720583`
- jit_compiled: jit=`0`, interp=`0`
- jit_gate: pass=`False`, wins=`1/5` (min speedup `1.010`, min wins `2`), compiled=`0` (min `32`), delta=`0`
