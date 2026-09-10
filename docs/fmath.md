# Float mathematics and integer roots

`require lib/fmath.f` exposes the checked `FMATH` vocabulary without libm.

| Word | Contract |
| --- | --- |
| `ISQRT-FLOOR ( n -- n )` | Exact floor of the root of a nonnegative signed cell. |
| `ISQRT-CEIL ( n -- n )` | Exact ceiling; no square of the result is needed. |
| `FLN ( r -- r )` | Natural logarithm of a positive finite binary64 value. |
| `FPOW ( r r -- r )` | Positive finite base raised to a finite exponent. |
| `FEXP ( r -- r )` | Existing degree-six range-reduced exponential approximation. |
| `FROUND ( r -- n )` | Nearest signed integer, with half values away from zero. |

Invalid root inputs, logarithm inputs, bases and exponents throw
`FMATH:E-DOMAIN` (-9020). An overflowing power or a nonfinite intermediate
exponent product throws `FMATH:E-OUTPUT` (-9021). Underflow may return zero.
Negative and zero bases are outside `FPOW`'s contract, including zero raised
to zero. `FROUND` rejects nonfinite inputs with `E-DOMAIN` and values outside
the signed-cell range with `E-OUTPUT`. It compares the fractional part before
rounding, preserving values immediately below a half and large exact integers.
`FEXP` retains its existing polynomial and range reduction.

The integer roots use Newton iteration with integer division and an initial
upper bound of 2^32. The ceiling test uses quotient and remainder, avoiding
overflow when the ceiling is 3037000500.

`FLN` scales its argument by exact powers of two into
[1/sqrt(2), sqrt(2)]. It evaluates
`ln(x) = 2 * (z + z^3/3 + z^5/5 + ...)`, where `z=(x-1)/(x+1)`,
through degree 41 and adds the removed exponent times ln(2). Centering the
interval on one preserves accuracy for arguments immediately below one.

`FPOW` evaluates `exp(exponent * ln(base))`. Its exponential uses a
range-reduced Taylor series through degree 16, then scales the result by
powers of two. It scales the polynomial itself, avoiding an intermediate
`2^1024` infinity when the final result is finite. The logarithm and product
rounding can be amplified by large exponents; this is an approximation, not
a correctly rounded replacement for the platform math library.

Run the focused native tests from the Habu root:

```sh
bin/hb --load lib/fmath-test.f
```

These test the signed-cell boundaries and root inequalities over 65536
inputs, float domain refusals, adjacent-to-one logarithms, subnormal powers,
finite powers above 2^1023, and the existing exponential behavior. An
additional development sweep against Python's platform math functions
(seed 9020) passed 516 positive logarithm inputs across the binary64 exponent
range at absolute error <=2e-12, and 514 power pairs at absolute error
<=max(5e-12*abs(reference), 4*ulp(reference)). These are sampled checks, not
uniform accuracy bounds. The sweep constructed binary inputs from exact
integer significands and powers of two to avoid decimal-parser rounding.
