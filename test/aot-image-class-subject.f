\ A stripped image classifies its own executable after restoring its DATA.
require src/lib/engine-id.f
require src/lib/fs.f
require tools/image-size-lib.f

package AOT-IMAGE-CLASS-SUBJECT

public

: RUN ( -- )
   ENGINE-ID:PATH$ IMAGE-SIZE:MEASURE
   IMAGE-SIZE:CLASS$ type cr
   ENGINE-ID:PATH$ FILE-SIZE IMAGE-SIZE:TOTAL-BYTES <> if
      s" aot-image-class: measured size differs from the executable" 74 die
   then
   s" size=ok" type cr ;

;package
