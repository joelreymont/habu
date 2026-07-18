\ type-nominal-dup-bad.f — a duplicate nominal declaration is refused fail-closed.
\ DEFTYPE routes a redeclared/reserved name through TYPE-RESERVED?, which dies
\ with "checker: bad or duplicate signature type" (exit 70). Registered as a
\ negative gate case in test/candidate-validation.f; the ARMED marker proves the
\ first (valid) declaration executed before the duplicate refusal fired.

DEFTYPE DUP-NOMINAL
s" TYPE-NOMINAL-DUP-ARMED" type cr
DEFTYPE DUP-NOMINAL
