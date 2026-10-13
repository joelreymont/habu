#include <math.h>
#include <stdarg.h>

float habu_f32_mix(float a, double b, int tag, float c) {
    if (a != 16777216.0f || b != 3.125 || tag != 7 || !signbit(c))
        return -1.0f;
    return 0.1f;
}

double habu_f32_registers(
    int i0, float f0, int i1, float f1, int i2, float f2, int i3, float f3,
    int i4, float f4, int i5, float f5, int i6, float f6, int i7, float f7) {
    if (i0 != 10 || i1 != 11 || i2 != 12 || i3 != 13 ||
        i4 != 14 || i5 != 15 || i6 != 16 || i7 != 17 ||
        f0 != 0.5f || f1 != 1.5f || f2 != 2.5f || f3 != 3.5f ||
        f4 != 4.5f || f5 != 5.5f || f6 != 6.5f || f7 != 7.5f)
        return -1.0;
    return 1234.5;
}

float habu_f32_variadic(float base, int count, ...) {
    va_list args;
    va_start(args, count);
    double added = count == 1 ? va_arg(args, double) : -100.0;
    va_end(args);
    return base + (float)added;
}
