\ codegen-test.f - the shared codegen byte buffer (src/lib/codegen.f): a
\ buffer throws E-CG-CAP / E-CG-VALUE.
\ Run: bin/hb --load src/lib/test.f src/lib/codegen.f src/lib/codegen-test.f

require src/lib/test.f
require src/lib/codegen.f

16 CODEGEN:BUFFER CGT-D

: CGT-FILL ( -- )
   CGT-D CODEGEN:RESET
   s" 0123456789abcdef" CGT-D CODEGEN:APPEND-STRING   \ 16 bytes == cap
   CGT-D CODEGEN:CONTENTS s" 0123456789abcdef" T$= ;

: CGT-THROWS ( -- )
   [: CGT-D CODEGEN:RESET  s" 0123456789abcdefg" CGT-D CODEGEN:APPEND-STRING ;]
      E-CG-CAP TTHROWSQ                    \ 17 > 16
   [: CGT-D CODEGEN:RESET  -5 CGT-D CODEGEN:APPEND-DECIMAL ;]
      E-CG-VALUE TTHROWSQ ;                \ negative

: CGT-RUN ( -- )
   T-RESET
   CGT-FILL
   CGT-THROWS
   T-REPORT ;

CGT-RUN
