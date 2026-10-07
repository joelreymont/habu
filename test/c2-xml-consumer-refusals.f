\ Checked source cannot reinterpret a cursor or its source projection.
require lib/test.f
require lib/test/subject.f
require lib/xml/c2.f
require lib/c2-bytes.f

package C2-XML-CONSUMER-REFUSALS
private
$4000 constant CAP
create OUT CAP allot
create ERR CAP allot

: STATUS? ( ptr u8 n n -- bool ) {: expected:n :}
   OUT CAP >LEN ERR CAP >LEN 10000 >MS SUBJECT:RUN
   MATCH outcome
      exited OF expected = ENDOF
      signaled OF drop false ENDOF
      timeout OF false ENDOF
   ;MATCH
   >r 2drop r> ;

public
: RUN ( -- )
   T-RESET
   s" a source name keeps the source lifetime, not the cursor lifetime" T-LABEL
   s" : X-C2-LIFE ( mut-view<b,i,a,init<i,XML-C2:reader-state<p,q>>> -- mut-view<b,i,a,init<i,XML-C2:reader-state<p,q>>> read-view<b,i,u8> ) XML-C2:NAME$ ;" 70 STATUS? TTRUE
   s" a cursor cannot become a raw byte pointer" T-LABEL
   s" : X-C2-RAW ( mut-view<b,i,a,init<i,XML-C2:reader-state<p,q>>> -- ptr u8 n ) ;" 70 STATUS? TTRUE
   s" a source projection cannot be written through mutable byte access" T-LABEL
   s" : X-C2-SOURCE-WRITE ( mut-view<b,i,a,init<i,XML-C2:reader-state<p,q>>> -- mut-view<b,i,a,init<i,XML-C2:reader-state<p,q>>> ) XML-C2:NAME$ 0 65 C2-MEM:MUT-BYTE! ;" 70 STATUS? TTRUE
   s" a cursor cannot be held in an ordinary local" T-LABEL
   s" : X-C2-LOCAL ( mut-view<b,i,a,init<i,XML-C2:reader-state<p,q>>> -- mut-view<b,i,a,init<i,XML-C2:reader-state<p,q>>> ) {: held :} held ;" 70 STATUS? TTRUE
   s" a cursor cannot be copied out of its callback" T-LABEL
   s" : X-C2-COPY ( mut-view<b,l,a,u8> read-view<p,q,u8> -- mut-view<b,l,a,u8> ) [: dup ;] XML-C2:WITH-READER ;" 70 STATUS? TTRUE
   s" a raw storage cell cannot hold a cursor" T-LABEL
   s" variable X-C2-SLOT : X-C2-STOW ( mut-view<b,i,a,init<i,XML-C2:reader-state<p,q>>> -- ) X-C2-SLOT ! ;" 70 STATUS? TTRUE
   s" the parser header constructor cannot be called after the module seals it" T-LABEL
   s" : X-C2-FORGE ( -- ) XML--C2-READER--STATE:MAKE ;" 70 STATUS? TTRUE
   s" a reopened package cannot export the raw cursor receiver" T-LABEL
   s" package XML-C2 public EXPORT CURSOR-UNPACK ;package" 70 STATUS? TTRUE
   s" another package cannot export the parser header constructor" T-LABEL
   s" package X-C2-ALIAS public EXPORT XML--C2-READER--STATE:MAKE ;package" 70 STATUS? TTRUE
   s" a borrowed slice compiles through this load path" T-LABEL
   s" : X-C2-SLICE-VALID ( read-view<p,q,u8> -- read-view<p,q,u8> ) 0 1 C2-BYTES:SLICE ;" 0 STATUS? TTRUE
   s" read slices keep their source region and loan" T-LABEL
   s" : X-C2-SLICE-LIFE ( read-view<p,q,u8> -- read-view<x,y,u8> ) 0 1 C2-BYTES:SLICE ;" 70 STATUS? TTRUE
   s" a mutable prefix cannot change its allocation region" T-LABEL
   s" : X-C2-PREFIX-REGION ( mut-view<p,q,a,u8> -- mut-view<p,q,b,u8> ) 0 C2-BYTES:PREFIX ;" 70 STATUS? TTRUE
   s" a read source cannot become the mutable copy destination" T-LABEL
   s" : X-C2-READ-COPY ( read-view<p,q,u8> read-view<x,y,u8> -- read-view<p,q,u8> n ) C2-BYTES:COPY ;" 70 STATUS? TTRUE
   s" a mutable prefix cannot be called through a read view" T-LABEL
   s" : X-C2-READ-PREFIX ( read-view<p,q,u8> -- read-view<p,q,u8> ) 0 C2-BYTES:PREFIX ;" 70 STATUS? TTRUE
   s" the private read pack cannot be called or ticked" T-LABEL
   s" : X-C2-PACK ( ptr u8 n -- read-view<p,q,u8> ) C2-MEM:READ-PACK ;" 70 STATUS? TTRUE
   s" ' C2-MEM:READ-PACK drop" 70 STATUS? TTRUE
   s" private XML scalar adapters cannot be reexported" T-LABEL
   s" package XML-C2 public EXPORT C-RAW ;package" 70 STATUS? TTRUE
   T-REPORT
   s" c2-xml-consumer-refusals: ok" type cr ;
;package

C2-XML-CONSUMER-REFUSALS:RUN
