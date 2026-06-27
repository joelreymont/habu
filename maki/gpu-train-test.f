\ maki/gpu-train-test.f - maki trains on the Orin GPU: 3 SGD epochs converge.
\
\ w=[2,4,6,8], x=t=1, lr=0.25.  Each epoch the SGD update runs on the device
\ (G-EPOCH -> G-SGD -> SAXPY kernel) and the host recomputes grad=2*(w-1) from the
\ GPU-returned weights. Trajectory w' = 0.5*w + 0.5 (f32-exact):
\   epoch1 [1.5,2.5,3.5,4.5]  epoch2 [1.25,1.75,2.25,2.75]  epoch3 [1.125,1.375,1.625,1.875]
\ Loss sum_i (w[i]-1)^2 falls 84 -> 1.3125 (training reduces the loss). Load after
\ maki/gpu.f, maki/array.f, maki/gpu-train.f. Prereq: cubin at /tmp/saxpy.cubin.

variable LOSS0

T-RESET

G-INIT-W
G-LOSS LOSS0 !                             \ initial loss = 84

G-SETUP
0.25 G-EPOCH  0.25 G-EPOCH  0.25 G-EPOCH    \ 3 device SGD epochs
G-RELEASE

\ weights converged along the exact trajectory (final = 0.5^3 path)
0 G-WBITS  1.125 F64>F32 T=
1 G-WBITS  1.375 F64>F32 T=
2 G-WBITS  1.625 F64>F32 T=
3 G-WBITS  1.875 F64>F32 T=

\ training reduced the loss
G-LOSS LOSS0 @ f< TTRUE

T-REPORT
