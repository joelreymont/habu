\ float32-buffer-test.f - F32 byte-buffer conversion tests.

require lib/test.f
require lib/float32.f
require lib/float32-buffer.f

package F32-BUF-TEST

using F32-BUF

private

create BUF 16 allot
create SRC 4 cells allot
create BACK 4 cells allot

: SCALARS ( -- )
   $123456789ABCDEF0 BUF STORE
   BUF LOAD $9ABCDEF0 T= ;

: ARRAYS ( -- )
   1.0 SRC 0 cells + !
   1.7 SRC 1 cells + !
   2.0 SRC 2 cells + !
   0.5 SRC 3 cells + !
   SRC 4 BUF PACK
   BUF      LOAD $3F800000 T=
   BUF 4  + LOAD $3FD9999A T=
   BUF 8  + LOAD $40000000 T=
   BUF 12 + LOAD $3F000000 T=
   BUF 4 BACK UNPACK
   BACK 0 cells + @ F32:NARROW $3F800000 T=
   BACK 1 cells + @ F32:NARROW $3FD9999A T=
   BACK 2 cells + @ F32:NARROW $40000000 T=
   BACK 3 cells + @ F32:NARROW $3F000000 T= ;

public

: RUN ( -- )
   T-RESET
   SCALARS
   ARRAYS
   T-REPORT ;

;using
;package

F32-BUF-TEST:RUN
