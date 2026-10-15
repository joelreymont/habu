\ Checker and package boundaries of the append-only producer.
require src/lib/test.f
require src/lib/test/subject.f
require src/lib/c2-owner.f

package C2-OWNER-PRODUCER-REFUSALS
private

: FAIL-APPEND ( C2-MEM:owner<p,i,a> -- C2-MEM:owner<p,i,a> )
   1 48 lshift MEM:BYTES-ALLOC-LEN C2-MEM:ALLOC
   C2-MEM:PUBLISH drop ;

: FAIL-WRAPPER ( C2-MEM:owner<p,i,a> -- C2-MEM:owner<p,i,a> )
   FAIL-APPEND ;

: EARLY-THROW ( C2-MEM:owner<p,i,a> -- C2-MEM:owner<p,i,a> )
   -9363 throw ;

TRUSTED: TRUSTED-THROW-OWNER ( C2-MEM:owner<p,i,a> -- C2-MEM:owner<p,i,a> )
   -9363 throw ;

TRUSTED: TRUSTED-THROW-VIEW ( read-view<p,q,u8> -- read-view<p,q,u8> )
   -9363 throw ;

: REPLACE-R-CALLBACK ( [ read-view<p,q,u8> -- read-view<p,q,u8> ] -- [ read-view<p,q,u8> -- read-view<p,q,u8> ] )
   >r r> drop 0 >r -9363 throw ;

public
EXPORT FAIL-APPEND
EXPORT FAIL-WRAPPER
EXPORT EARLY-THROW
EXPORT TRUSTED-THROW-OWNER
EXPORT TRUSTED-THROW-VIEW
EXPORT REPLACE-R-CALLBACK

: RUN ( -- )
   T-RESET
   s" a failed append caught directly cannot restore its consumed owner" T-LABEL
   s" -1 JSON-DIAGS ! : C2OP-CATCH ( C2-MEM:owner<p,i,a> -- C2-MEM:owner<p,i,a> ) [: C2-OWNER-PRODUCER-REFUSALS:FAIL-APPEND ;] catch drop 17 MEM:BYTES-ALLOC-LEN C2-MEM:ALLOC C2-MEM:PUBLISH drop ;"
   70 s\" \"code\":\"E-STALE-READ\"" SUBJECT:REFUSED? TTRUE
   s" a wrapper cannot hide the failed append's consumed owner" T-LABEL
   s" -1 JSON-DIAGS ! : C2OP-WRAP-CATCH ( C2-MEM:owner<p,i,a> -- C2-MEM:owner<p,i,a> ) [: C2-OWNER-PRODUCER-REFUSALS:FAIL-WRAPPER ;] catch drop 17 MEM:BYTES-ALLOC-LEN C2-MEM:ALLOC C2-MEM:PUBLISH drop ;"
   70 s\" \"code\":\"E-STALE-READ\"" SUBJECT:REFUSED? TTRUE
   s" a throw before consuming an owner leaves the owner usable" T-LABEL
   s" : C2OP-EARLY-CATCH ( C2-MEM:owner<p,i,a> -- C2-MEM:owner<p,i,a> ) [: C2-OWNER-PRODUCER-REFUSALS:EARLY-THROW ;] catch drop 17 MEM:BYTES-ALLOC-LEN C2-MEM:ALLOC C2-MEM:PUBLISH drop ;" 0 SUBJECT:EXITED? TTRUE
   s" a trusted callee that throws leaves its owner stale" T-LABEL
   s" -1 JSON-DIAGS ! : C2OP-TRUSTED-OWNER-CATCH ( C2-MEM:owner<p,i,a> -- C2-MEM:owner<p,i,a> ) [: C2-OWNER-PRODUCER-REFUSALS:TRUSTED-THROW-OWNER ;] catch drop 17 MEM:BYTES-ALLOC-LEN C2-MEM:ALLOC C2-MEM:PUBLISH drop ;"
   70 s\" \"code\":\"E-STALE-READ\"" SUBJECT:REFUSED? TTRUE
   s" an early throw keeps an owner parked on the return stack usable" T-LABEL
   s" : C2OP-EARLY-R-OWNER-CATCH ( C2-MEM:owner<p,i,a> -- C2-MEM:owner<p,i,a> ) >r [: -9363 throw ;] catch drop r> 17 MEM:BYTES-ALLOC-LEN C2-MEM:ALLOC C2-MEM:PUBLISH drop ;" 0 SUBJECT:EXITED? TTRUE
   s" a trusted callee that throws leaves its read view stale" T-LABEL
   s" -1 JSON-DIAGS ! : C2OP-TRUSTED-VIEW-CATCH ( read-view<p,q,u8> -- u8 read-view<p,q,u8> ) [: C2-OWNER-PRODUCER-REFUSALS:TRUSTED-THROW-VIEW ;] catch drop 0 C2-MEM:BYTE@ ;"
   70 s\" \"code\":\"E-STALE-READ\"" SUBJECT:REFUSED? TTRUE
   s" a throw cannot restore a scoped callback it replaced on its return stack" T-LABEL
   s" : C2OP-R-CALLBACK-CATCH ( read-view<p,q,u8> [ read-view<p,q,u8> -- read-view<p,q,u8> ] -- read-view<p,q,u8> ) [: C2-OWNER-PRODUCER-REFUSALS:REPLACE-R-CALLBACK ;] catch drop execute ;" 70 SUBJECT:EXITED? TTRUE
   s" an early throw keeps a callback parked on the return stack usable" T-LABEL
   s" : C2OP-EARLY-R-CALLBACK-CATCH ( read-view<p,q,u8> [ read-view<p,q,u8> -- read-view<p,q,u8> ] -- read-view<p,q,u8> ) >r [: -9363 throw ;] catch drop r> execute ;" 0 SUBJECT:EXITED? TTRUE
   s" an owner cannot be duplicated" T-LABEL
   s" : C2OP-DUP ( C2-MEM:owner<p,i,a> -- C2-MEM:owner<p,i,a> ) dup drop ;" 70 SUBJECT:EXITED? TTRUE
   s" a published mutable view cannot be reused" T-LABEL
   s" : C2OP-REUSE ( mut-view<p,l,a,u8> -- read-view<p,l,u8> ) dup C2-MEM:PUBLISH swap 0 C2-MEM:MUT-BYTE! ;" 70 SUBJECT:EXITED? TTRUE
   s" an input region cannot claim a new allocation" T-LABEL
   s" : C2OP-FORGE ( C2-MEM:owner<p,i,a> NUM:alloc-byte-len -- C2-MEM:owner<p,i,a> mut-view<p,p,a,u8> ) C2-MEM:ALLOC ;" 70 SUBJECT:EXITED? TTRUE
   s" two allocations cannot claim the same fresh region" T-LABEL
   s" : C2OP-COLLAPSE ( C2-MEM:owner<p,i,a> -- C2-MEM:owner<p,i,a> mut-view<p,p,fresh-region-b,u8> mut-view<p,p,fresh-region-b,u8> ) 16 MEM:BYTES-ALLOC-LEN C2-MEM:ALLOC swap 16 MEM:BYTES-ALLOC-LEN C2-MEM:ALLOC rot swap ;" 70 SUBJECT:EXITED? TTRUE
   s" a raw-pointer disposer cannot receive an owned view" T-LABEL
   s" : C2OP-RAW-DISPOSE ( ptr u8 NUM:alloc-byte-len -- ) 2drop ; : C2OP-RAW ( C2-MEM:owner<p,i,a> -- C2-MEM:owner<p,i,a> mut-view<p,p,fresh-region-b,u8> ) 16 MEM:BYTES-ALLOC-LEN [: C2OP-RAW-DISPOSE ;] C2-MEM:ALLOC-DISPOSE ;" 70 SUBJECT:EXITED? TTRUE
   s" a disposer tied to an existing region cannot claim a fresh one" T-LABEL
   s" : C2OP-RIGID ( C2-MEM:owner<p,i,a> NUM:alloc-byte-len [ mut-view<p,p,a,u8> -- ] -- C2-MEM:owner<p,i,a> mut-view<p,p,fresh-region-b,u8> ) C2-MEM:ALLOC-DISPOSE ;" 70 SUBJECT:EXITED? TTRUE
   s" two disposer allocations cannot claim one fresh region" T-LABEL
   s" : C2OP-DROP-VIEW ( mut-view<p,p,a,u8> -- ) C2-MEM:PUBLISH drop ; : C2OP-TWO-DISPOSE ( C2-MEM:owner<p,i,a> -- C2-MEM:owner<p,i,a> mut-view<p,p,fresh-region-b,u8> mut-view<p,p,fresh-region-b,u8> ) 16 MEM:BYTES-ALLOC-LEN [: C2OP-DROP-VIEW ;] C2-MEM:ALLOC-DISPOSE swap 16 MEM:BYTES-ALLOC-LEN [: C2OP-DROP-VIEW ;] C2-MEM:ALLOC-DISPOSE rot swap ;" 70 SUBJECT:EXITED? TTRUE
   s" the raw owner constructor cannot be called" T-LABEL
   s" : C2OP-CTOR ( ptr u8 n -- C2-MEM:owner<p,i,a> ) C2-MEM:OWNER-MAKE ;" 70 SUBJECT:EXITED? TTRUE
   s" a mutable view cannot be unpacked outside the owner package" T-LABEL
   s" : C2OP-UNPACK ( mut-view<p,l,a,u8> -- ptr u8 n ) C2-MEM:MUT-UNPACK ;" 70 SUBJECT:EXITED? TTRUE
   s" ticking the unpacker does not cross its internal boundary" T-LABEL
   s" : C2OP-TICK ( mut-view<p,l,a,u8> -- ptr u8 n ) ['] C2-MEM:MUT-UNPACK execute ;" 70 SUBJECT:EXITED? TTRUE
   \ The product seals C2-MEM with every package it bakes (src/core/internal-mark.f
   \ SEAL-PACKAGES): the engine refuses `package C2-MEM` by name, exit 84, before
   \ the checker sees a body. The checker's reopen cases run where C2-MEM stays
   \ open (test/c2-reopen-refusals.f).
   s" the product refuses a reopen of C2-MEM by name" T-LABEL
   s" package C2-MEM ;package" ENGINE-ERROR:SEAL-PACKAGE s" C2-MEM" SUBJECT:REFUSED? TTRUE
   T-REPORT
   s" c2-owner-producer-refusals: ok" type cr ;

;package

C2-OWNER-PRODUCER-REFUSALS:RUN
