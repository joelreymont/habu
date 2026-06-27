\ maki/gpu-sgd-test.f - a maki SGD optimizer step runs on the Orin GPU, matches CPU.
\
\ The tensor SGD update w[i] -= lr*g[i] (maki/array.f T-SGD!) lowers onto the
\ checked SAXPY kernel: a=-lr, x=grad, y=weight => a*x+y = w - lr*g. Put grad as x
\ and weight as y, then G-SGD lr. Values chosen f32-exact so the device result
\ equals the CPU golden bit-for-bit. Load after maki/gpu.f.

T-RESET

\ w = [2,4,6,8], g = [1,1,1,1], lr = 0.5  =>  w' = w - 0.5*g = [1.5,3.5,5.5,7.5]
1.0 2.0 0 G-PUT
1.0 4.0 1 G-PUT
1.0 6.0 2 G-PUT
1.0 8.0 3 G-PUT

G-SETUP
0.5 G-SGD
G-RELEASE

\ each updated weight matches the CPU SGD step (as f32 bits)
0 G-RESULT  1.5 F64>F32 T=
1 G-RESULT  3.5 F64>F32 T=
2 G-RESULT  5.5 F64>F32 T=
3 G-RESULT  7.5 F64>F32 T=

T-REPORT
