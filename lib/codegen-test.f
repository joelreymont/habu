\ codegen-test.f - focused tests for the shared codegen byte buffer (lib/codegen.f).
\ Run: bin/hb --load lib/test.f lib/codegen.f lib/codegen-test.f
\
\ Proves the round trip (append-byte / append-string / append-decimal / contents),
\ that RESET discards prior content, that a full buffer accepts exactly its capacity,
\ and that negative capacity, overflow, and negative decimal input throw the
\ module-owned errors.

require lib/test.f
require lib/codegen.f

16 CODEGEN:BUFFER CGT-D

\ The capacity guard must fire before the definer parses a name.
-1 ' CODEGEN:BUFFER catch constant CGT-NEG-CAP-RC

: CGT-CODES ( -- )
   E-CG-CAP -4700 T=
   E-CG-VALUE -4701 T=
   CGT-NEG-CAP-RC E-CG-CAP T= ;

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

: CGT-THROWS ( -- )
   [: CGT-D CODEGEN:RESET  s" 0123456789abcdefg" CGT-D CODEGEN:APPEND-STRING ;]
      E-CG-CAP TTHROWSQ
   [: CGT-D CODEGEN:RESET  -5 CGT-D CODEGEN:APPEND-DECIMAL ;]
      E-CG-VALUE TTHROWSQ ;

: CGT-RUN ( -- )
   T-RESET
   CGT-CODES
   CGT-BUILD
   CGT-RESET-ZERO
   CGT-FILL
   CGT-THROWS
   T-REPORT ;

CGT-RUN
