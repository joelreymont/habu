\ again.f - loaded twice by src/lib/error-test.f: the second load declares the
\ same name again, and `constant` throws E-DUPLICATE-DEFINITION.

require src/lib/error.f

-9680 s" the twice-loaded test code" ERROR:CODE E-AGAIN
