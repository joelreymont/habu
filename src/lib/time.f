\ time.f - checked wrappers around native clocks, in `package TIME`.
\ MONO-NS is monotonic nanoseconds: compare readings for order or elapsed time
\ only, never as absolute values. It counts real time.
\
\ STORAGE CLASS. CALLER-OWNED: engine primitives; no module state.

package TIME

public

: MONO-NS ( -- n )
   mono-ns ;

;package
