\ fault.f - the fault of package ENCODING (src/lib/encoding.f), bytes an
\ encoding cannot decode, in a part of its own so that each decoder part can
\ throw it.
\ STORAGE CLASS. PROCESS-WIDE constants.

require src/lib/error.f

package ENCODING
public

-9890 constant E-FIRST
-9899 constant E-LAST
-9890 s" bytes the encoding cannot decode" ERROR:CODE E-DECODE

;package
