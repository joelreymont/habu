\ The checker pins a byte view's lifetime, region and access, and keeps
\ C2-MEM's pack private.
require src/lib/test.f
require src/lib/test/subject.f
require src/lib/c2-bytes.f

package C2-BYTES-REJECTIONS
public
: RUN ( -- )
   T-RESET
   s" a borrowed slice compiles through this load path" T-LABEL
   s" : X-C2-SLICE-VALID ( read-view<p,q,u8> -- read-view<p,q,u8> ) 0 1 C2-BYTES:SLICE ;" 0 SUBJECT:EXITED? TTRUE
   s" read slices keep their source region and loan" T-LABEL
   s" : X-C2-SLICE-LIFE ( read-view<p,q,u8> -- read-view<x,y,u8> ) 0 1 C2-BYTES:SLICE ;" 70 SUBJECT:EXITED? TTRUE
   s" a mutable prefix cannot change its allocation region" T-LABEL
   s" : X-C2-PREFIX-REGION ( mut-view<p,q,a,u8> -- mut-view<p,q,b,u8> ) 0 C2-BYTES:PREFIX ;" 70 SUBJECT:EXITED? TTRUE
   s" a read source cannot become the mutable copy destination" T-LABEL
   s" : X-C2-READ-COPY ( read-view<p,q,u8> read-view<x,y,u8> -- read-view<p,q,u8> n ) C2-BYTES:COPY ;" 70 SUBJECT:EXITED? TTRUE
   s" a mutable prefix cannot be called through a read view" T-LABEL
   s" : X-C2-READ-PREFIX ( read-view<p,q,u8> -- read-view<p,q,u8> ) 0 C2-BYTES:PREFIX ;" 70 SUBJECT:EXITED? TTRUE
   s" the private read pack cannot be called or ticked" T-LABEL
   s" : X-C2-PACK ( ^u8 n -- read-view<p,q,u8> ) C2-MEM:READ-PACK ;" 70 SUBJECT:EXITED? TTRUE
   s" ' C2-MEM:READ-PACK drop" 70 SUBJECT:EXITED? TTRUE
   T-REPORT
   s" c2-bytes-rejections: ok" type cr ;
;package

C2-BYTES-REJECTIONS:RUN
