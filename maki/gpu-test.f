\ maki/gpu-test.f - a maki tensor op (AXPY) runs on the Orin GPU, matches the CPU.

T-RESET

\ x = [1,2,3,4], y = [10,20,30,40], a = 2.0  =>  y' = 2*x + y = [12,24,36,48]
1.0 10.0 0 GPU:PUT
2.0 20.0 1 GPU:PUT
3.0 30.0 2 GPU:PUT
4.0 40.0 3 GPU:PUT

GPU:SETUP
2.0 GPU:LAUNCH
GPU:RELEASE

\ each GPU result element matches the CPU golden (as f32 bits)
0 GPU:RESULT  12.0 F64>F32 T=
1 GPU:RESULT  24.0 F64>F32 T=
2 GPU:RESULT  36.0 F64>F32 T=
3 GPU:RESULT  48.0 F64>F32 T=

T-REPORT
