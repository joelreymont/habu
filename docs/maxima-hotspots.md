# Maxima Hotspots

- scale: `1`
- heap_mb: `1024`
- nursery_mb: `32`

| workload | jit_ns | interp_ns | interp/jit |
|---|---:|---:|---:|
| integrate | 138512125 | 141464250 | 1.021 |
| factor | 46767750 | 47563042 | 1.017 |
| ratsimp | 36277375 | 33868958 | 0.934 |
| solve | 12904625 | 12716791 | 0.985 |
| determinant | 2115125 | 1983208 | 0.938 |

- loader_ns: jit=`16326703959`, interp=`15056312250`
- jit_compiled: jit=`548`, interp=`0`
- jit_adm: cand=`4139/0`, elig=`3421/0`, comp=`548/0`, sk_speed=`0/0`, sk_safety=`0/0`, sk_chunk=`277/0`, fail_unsup=`2674/0`, fail_other=`199/0`
- call_shape(run): total=`219210/219250`, fixed=`124210/124250`, optional=`3720/3720`, key=`85160/85160`, rest=`6120/6120`, dynamic=`134490/134530`, tail=`92400/92800`
- jit_gate: pass=`True`, wins=`2/5` (min speedup `1.010`, min wins `2`), compiled=`548` (min `32`), delta=`548`
