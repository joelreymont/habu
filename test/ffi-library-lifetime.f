\ ffi-library-lifetime.f - FFI releases the library references its rows own
\ when an image is prepared, under --load.
\ Run: bin/hb --load test/ffi-library-lifetime.f
\
\ The assertions are test/ffi-library-lifetime-subject.f's; test/stripped-image.f
\ runs the same ones in a stripped executable.

require lib/test.f
require test/ffi-library-lifetime-subject.f

package FFI-LIBRARY-LIFETIME-TEST

: RUN ( -- )
   T-RESET
   FFI-LIBRARY-LIFETIME:CHECK
   T-REPORT ;

RUN

;package
