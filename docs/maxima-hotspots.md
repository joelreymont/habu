# Maxima Hotspots

- scale: `1`
- heap_mb: `1024`
- nursery_mb: `32`

| workload | jit_ns | interp_ns | interp/jit |
|---|---:|---:|---:|
| integrate | 149893084 | 150190708 | 1.002 |
| factor | 46880041 | 46676417 | 0.996 |
| ratsimp | 35104459 | 34029833 | 0.969 |
| solve | 12200084 | 12252000 | 1.004 |
| determinant | 1951959 | 1839458 | 0.942 |

- loader_ns: jit=`15954744791`, interp=`14398262167`
- jit_compiled: jit=`548`, interp=`0`
- jit_direct_calls: jit=`309`, interp=`0`
- jit_adm: cand=`4139/0`, elig=`3421/0`, comp=`548/0`, sk_speed=`0/0`, sk_safety=`0/0`, sk_chunk=`277/0`, fail_unsup=`2674/0`, fail_other=`199/0`
- call_shape(run): total=`219210/219250`, fixed=`124210/124250`, optional=`3720/3720`, key=`85160/85160`, rest=`6120/6120`, dynamic=`134490/134530`, tail=`92400/92800`
- jit_gate: pass=`False`, wins=`0/5` (min speedup `1.010`, min wins `2`), compiled=`548` (min `32`), delta=`548`
