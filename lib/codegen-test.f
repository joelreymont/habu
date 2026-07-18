\ codegen-test.f - focused tests for the shared codegen byte buffer (lib/codegen.f).
\ Run: bin/hb --load lib/test.f lib/codegen.f lib/codegen-test.f
\
\ Proves the round trip (append-byte / append-string / append-decimal / contents),
\ that RESET discards prior content, that a full buffer accepts exactly its capacity,
\ and that both error surfaces fire: a buffer minted with the plain definer throws
\ the module's own E-CG-CAP / E-CG-VALUE, while a buffer minted with BUFFER-E: throws
\ the exact caller-supplied codes (the mechanism maki and value-nominal use to keep
\ their existing E-EXT-*/E-VNOM-* throw codes after migrating onto this module).

require lib/test.f
require lib/codegen.f

16 CODEGEN:BUFFER CGT-D                  \ default codes: E-CG-CAP / E-CG-VALUE
8 -7001 -7002 CODEGEN:BUFFER-E CGT-E     \ explicit per-buffer codes (arbitrary caller sentinels)

: CGT-CODES ( -- )
   E-CG-CAP -4700 T=
   E-CG-VALUE -4701 T= ;

\ append-byte, append-string, and append-decimal compose into the contents.
: CGT-BUILD ( -- )
   CGT-D CODEGEN:RESET
   [char] x CGT-D CODEGEN:APPEND-BYTE
   s" =" CGT-D CODEGEN:APPEND-STRING
   1234 CGT-D CODEGEN:APPEND-DECIMAL
   CGT-D CODEGEN:CONTENTS s" x=1234" T$= ;

\ RESET drops prior content; append-decimal of 0 emits a single '0'.
: CGT-RESET-ZERO ( -- )
   CGT-D CODEGEN:RESET
   s" abc" CGT-D CODEGEN:APPEND-STRING
   CGT-D CODEGEN:RESET
   0 CGT-D CODEGEN:APPEND-DECIMAL
   CGT-D CODEGEN:CONTENTS s" 0" T$= ;

\ exactly cap bytes fit.
: CGT-FILL ( -- )
   CGT-D CODEGEN:RESET
   s" 0123456789abcdef" CGT-D CODEGEN:APPEND-STRING   \ 16 bytes == cap
   CGT-D CODEGEN:CONTENTS s" 0123456789abcdef" T$= ;

\ both error surfaces: default buffer uses the module codes, BUFFER-E: buffer uses
\ the exact caller codes.
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
   CGT-CODES
   CGT-BUILD
   CGT-RESET-ZERO
   CGT-FILL
   CGT-THROWS
   T-REPORT ;

CGT-RUN
