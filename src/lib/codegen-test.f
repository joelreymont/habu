\ codegen-test.f - the shared codegen byte buffer (src/lib/codegen.f): a plain
\ buffer throws E-CG-CAP / E-CG-VALUE, a BUFFER-E: buffer the caller's codes.
\ Run: bin/hb --load src/lib/test.f src/lib/codegen.f src/lib/codegen-test.f

require src/lib/test.f
require src/lib/codegen.f

16 CODEGEN:BUFFER CGT-D
8 -7001 -7002 CODEGEN:BUFFER-E CGT-E     \ explicit per-buffer codes (arbitrary caller sentinels)

: CGT-BUILD ( -- )
   CGT-D CODEGEN:RESET
   [char] x CGT-D CODEGEN:APPEND-BYTE
   s" =" CGT-D CODEGEN:APPEND-STRING
   1234 CGT-D CODEGEN:APPEND-DECIMAL
   CGT-D CODEGEN:CONTENTS s" x=1234" T$= ;

: CGT-RESET-ZERO ( -- )
   CGT-D CODEGEN:RESET
   s" abc" CGT-D CODEGEN:APPEND-STRING
   CGT-D CODEGEN:RESET
   0 CGT-D CODEGEN:APPEND-DECIMAL
   CGT-D CODEGEN:CONTENTS s" 0" T$= ;

: CGT-FILL ( -- )
   CGT-D CODEGEN:RESET
   s" 0123456789abcdef" CGT-D CODEGEN:APPEND-STRING   \ 16 bytes == cap
   CGT-D CODEGEN:CONTENTS s" 0123456789abcdef" T$= ;

: CGT-THROWS ( -- )
   [: CGT-D CODEGEN:RESET  s" 0123456789abcdefg" CGT-D CODEGEN:APPEND-STRING ;]
      E-CG-CAP TTHROWSQ                    \ 17 > 16: default buffer overflow -> module code
   [: CGT-D CODEGEN:RESET  -5 CGT-D CODEGEN:APPEND-DECIMAL ;]
      E-CG-VALUE TTHROWSQ                  \ default buffer negative decimal -> module code
   [: CGT-E CODEGEN:RESET  s" 123456789" CGT-E CODEGEN:APPEND-STRING ;]
      -7001 TTHROWSQ                       \ 9 > 8: explicit buffer overflow -> caller code
   [: CGT-E CODEGEN:RESET  -1 CGT-E CODEGEN:APPEND-DECIMAL ;]
      -7002 TTHROWSQ ;                     \ explicit buffer negative decimal -> caller code

: CGT-RUN ( -- )
   T-RESET
   CGT-BUILD
   CGT-RESET-ZERO
   CGT-FILL
   CGT-THROWS
   T-REPORT ;

CGT-RUN
