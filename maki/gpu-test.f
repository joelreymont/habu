\ maki/gpu-test.f - a maki tensor op (AXPY) runs on the Orin GPU, matches the CPU.

T-RESET

\ x = [1,2,3,4], y = [10,20,30,40], a = 2.0  =>  y' = 2*x + y = [12,24,36,48]
1.0 10.0 0 G-PUT
2.0 20.0 1 G-PUT
3.0 30.0 2 G-PUT
4.0 40.0 3 G-PUT

G-SETUP
2.0 G-LAUNCH
G-RELEASE

\ each GPU result element matches the CPU golden (as f32 bits)
0 G-RESULT  12.0 F64>F32 T=
1 G-RESULT  24.0 F64>F32 T=
2 G-RESULT  36.0 F64>F32 T=
3 G-RESULT  48.0 F64>F32 T=

T-REPORT
