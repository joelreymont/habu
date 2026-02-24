# Maxima Hotspots

- scale: `1`
- heap_mb: `1024`
- nursery_mb: `32`

| workload | jit_ns | interp_ns | interp/jit |
|---|---:|---:|---:|
| integrate | 141787917 | 144178375 | 1.017 |
| factor | 45040833 | 45430875 | 1.009 |
| ratsimp | 33672208 | 33033500 | 0.981 |
| solve | 11554125 | 11699709 | 1.013 |
| determinant | 1764375 | 1663125 | 0.943 |

- loader_ns: jit=`15045998750`, interp=`13775464667`
- jit_compiled: jit=`548`, interp=`0`
- jit_direct_calls: jit=`309`, interp=`0`
- jit_adm: cand=`4139/0`, elig=`3421/0`, comp=`548/0`, sk_speed=`0/0`, sk_safety=`0/0`, sk_chunk=`277/0`, fail_unsup=`2674/0`, fail_other=`199/0`, cache_comp=`0/0`, cache_unsup=`3/0`, cache_fail=`0/0`
- call_shape(run): total=`219210/219250`, fixed=`124210/124250`, optional=`3720/3720`, key=`85160/85160`, rest=`6120/6120`, dynamic=`134490/134530`, tail=`92400/92800`
- jit_gate: pass=`True`, wins=`2/5` (min speedup `1.010`, min wins `2`), compiled=`548` (min `32`), delta=`548`
