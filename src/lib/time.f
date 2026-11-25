\ time.f - checked wrappers around native clocks, in `package TIME`.
\ MONO-NS is monotonic nanoseconds: compare readings for order or elapsed time
\ only, never as absolute values. It counts real time. ELAPSED-MS
\ measures the time since a MONO-NS reading.
\
\ STORAGE CLASS. CALLER-OWNED: engine primitives; no module state.

package TIME

1000000 constant NS-PER-MS

public

: MONO-NS ( -- n )
   mono-ns ;

\ Whole milliseconds from a MONO-NS reading to now, truncated.
: ELAPSED-MS ( n -- n )
   mono-ns swap - NS-PER-MS / ;

;package
