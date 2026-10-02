\ Checker and package boundaries of the append-only producer.
require lib/test.f
require lib/test/subject.f
require lib/c2-owner.f

package C2-OWNER-PRODUCER-REFUSALS
private

$4000 constant CAP
create OUT CAP allot
create ERR CAP allot

: REJECT ( ptr u8 n -- bool )
   OUT CAP >LEN ERR CAP >LEN 10000 >MS SUBJECT:RUN
   MATCH outcome
      exited OF 70 = ENDOF
      signaled OF drop false ENDOF
      timeout OF false ENDOF
   ;MATCH
   >r 2drop r> ;

: STALE? ( ptr u8 n -- bool )
   OUT CAP >LEN ERR CAP >LEN 10000 >MS SUBJECT:RUN
   MATCH outcome
      exited OF 70 = ENDOF
      signaled OF drop false ENDOF
      timeout OF false ENDOF
   ;MATCH
   {: outu:len erru:len refused:bool :}
   refused outu LEN>N 0= and
   ERR erru LEN>N s\" \"code\":\"E-STALE-READ\"" CONTAINS? and ;

\ The product seals C2-MEM with every package it bakes (src/core/internal-mark.f
\ SEAL-PACKAGES): the engine refuses `package C2-MEM` by name, exit 84, before
\ the checker sees a body. The checker's reopen cases run where C2-MEM stays
\ open (test/c2-reopen-refusals.f).
: SEALED? ( ptr u8 n -- bool )
   OUT CAP >LEN ERR CAP >LEN 10000 >MS SUBJECT:RUN
   MATCH outcome
      exited OF ENGINE-ERROR:SEAL-PACKAGE = ENDOF
      signaled OF drop false ENDOF
      timeout OF false ENDOF
   ;MATCH
   {: outu:len erru:len refused:bool :}
   refused outu LEN>N 0= and
   ERR erru LEN>N s" C2-MEM" CONTAINS? and ;

: ACCEPT? ( ptr u8 n -- bool )
   OUT CAP >LEN ERR CAP >LEN 10000 >MS SUBJECT:RUN
   MATCH outcome
      exited OF 0= ENDOF
      signaled OF drop false ENDOF
      timeout OF false ENDOF
   ;MATCH
   >r 2drop r> ;

: FAIL-APPEND ( C2-MEM:owner<p,i,a> -- C2-MEM:owner<p,i,a> )
   1 48 lshift MEM:BYTES-ALLOC-LEN C2-MEM:ALLOC
   C2-MEM:PUBLISH drop ;

: FAIL-WRAPPER ( C2-MEM:owner<p,i,a> -- C2-MEM:owner<p,i,a> )
   FAIL-APPEND ;

: EARLY-THROW ( C2-MEM:owner<p,i,a> -- C2-MEM:owner<p,i,a> )
   -9363 throw ;

TRUSTED: REPLACE-R-OWNER ( | C2-MEM:owner<p,i,a> -- | C2-MEM:owner<p,i,a> )
   r> drop 0 >r -9363 throw ;

TRUSTED: REPLACE-R-VIEW ( | read-view<p,q,u8> -- | read-view<p,q,u8> )
   r> r> 2drop 0 0 >r >r -9363 throw ;

TRUSTED: REPLACE-R-CALLBACK ( | [ read-view<p,q,u8> -- read-view<p,q,u8> ] -- | [ read-view<p,q,u8> -- read-view<p,q,u8> ] )
   r> drop 0 >r -9363 throw ;

: EARLY-R-OWNER ( | C2-MEM:owner<p,i,a> -- | C2-MEM:owner<p,i,a> )
   -9363 throw ;

: EARLY-R-CALLBACK ( | [ read-view<p,q,u8> -- read-view<p,q,u8> ] -- | [ read-view<p,q,u8> -- read-view<p,q,u8> ] )
   -9363 throw ;

public
EXPORT FAIL-APPEND
EXPORT FAIL-WRAPPER
EXPORT EARLY-THROW
EXPORT REPLACE-R-OWNER
EXPORT REPLACE-R-VIEW
EXPORT REPLACE-R-CALLBACK
EXPORT EARLY-R-OWNER
EXPORT EARLY-R-CALLBACK

: RUN ( -- )
   T-RESET
   s" a failed append caught directly cannot restore its consumed owner" T-LABEL
   s" -1 JSON-DIAGS ! : C2OP-CATCH ( C2-MEM:owner<p,i,a> -- C2-MEM:owner<p,i,a> ) [: C2-OWNER-PRODUCER-REFUSALS:FAIL-APPEND ;] catch drop 17 MEM:BYTES-ALLOC-LEN C2-MEM:ALLOC C2-MEM:PUBLISH drop ;" STALE? TTRUE
   s" a wrapper cannot hide the failed append's consumed owner" T-LABEL
   s" -1 JSON-DIAGS ! : C2OP-WRAP-CATCH ( C2-MEM:owner<p,i,a> -- C2-MEM:owner<p,i,a> ) [: C2-OWNER-PRODUCER-REFUSALS:FAIL-WRAPPER ;] catch drop 17 MEM:BYTES-ALLOC-LEN C2-MEM:ALLOC C2-MEM:PUBLISH drop ;" STALE? TTRUE
   s" a throw before consuming an owner leaves the owner usable" T-LABEL
   s" : C2OP-EARLY-CATCH ( C2-MEM:owner<p,i,a> -- C2-MEM:owner<p,i,a> ) [: C2-OWNER-PRODUCER-REFUSALS:EARLY-THROW ;] catch drop 17 MEM:BYTES-ALLOC-LEN C2-MEM:ALLOC C2-MEM:PUBLISH drop ;" ACCEPT? TTRUE
   s" a trusted throw cannot restore a replaced return owner" T-LABEL
   s" -1 JSON-DIAGS ! : C2OP-R-OWNER-CATCH ( C2-MEM:owner<p,i,a> -- C2-MEM:owner<p,i,a> ) >r [: C2-OWNER-PRODUCER-REFUSALS:REPLACE-R-OWNER ;] catch drop r> 17 MEM:BYTES-ALLOC-LEN C2-MEM:ALLOC C2-MEM:PUBLISH drop ;" STALE? TTRUE
   s" a checked early throw keeps a return owner usable" T-LABEL
   s" : C2OP-EARLY-R-OWNER-CATCH ( C2-MEM:owner<p,i,a> -- C2-MEM:owner<p,i,a> ) >r [: C2-OWNER-PRODUCER-REFUSALS:EARLY-R-OWNER ;] catch drop r> 17 MEM:BYTES-ALLOC-LEN C2-MEM:ALLOC C2-MEM:PUBLISH drop ;" ACCEPT? TTRUE
   s" a trusted throw cannot restore a replaced return read view" T-LABEL
   s" -1 JSON-DIAGS ! : C2OP-R-VIEW-CATCH ( read-view<p,q,u8> -- u8 read-view<p,q,u8> ) >r [: C2-OWNER-PRODUCER-REFUSALS:REPLACE-R-VIEW ;] catch drop r> 0 C2-MEM:BYTE@ ;" STALE? TTRUE
   s" a trusted throw cannot restore a nested scoped return callback" T-LABEL
   s" : C2OP-R-CALLBACK-CATCH ( read-view<p,q,u8> [ read-view<p,q,u8> -- read-view<p,q,u8> ] -- read-view<p,q,u8> ) >r [: C2-OWNER-PRODUCER-REFUSALS:REPLACE-R-CALLBACK ;] catch drop r> execute ;" REJECT TTRUE
   s" a checked early throw keeps a return callback usable" T-LABEL
   s" : C2OP-EARLY-R-CALLBACK-CATCH ( read-view<p,q,u8> [ read-view<p,q,u8> -- read-view<p,q,u8> ] -- read-view<p,q,u8> ) >r [: C2-OWNER-PRODUCER-REFUSALS:EARLY-R-CALLBACK ;] catch drop r> execute ;" ACCEPT? TTRUE
   s" an owner cannot be duplicated" T-LABEL
   s" : C2OP-DUP ( C2-MEM:owner<p,i,a> -- C2-MEM:owner<p,i,a> ) dup drop ;" REJECT TTRUE
   s" a published mutable view cannot be reused" T-LABEL
   s" : C2OP-REUSE ( mut-view<p,l,a,u8> -- read-view<p,l,u8> ) dup C2-MEM:PUBLISH swap 0 C2-MEM:MUT-BYTE! ;" REJECT TTRUE
   s" an input region cannot claim a new allocation" T-LABEL
   s" : C2OP-FORGE ( C2-MEM:owner<p,i,a> NUM:alloc-byte-len -- C2-MEM:owner<p,i,a> mut-view<p,p,a,u8> ) C2-MEM:ALLOC ;" REJECT TTRUE
   s" two allocations cannot claim the same fresh region" T-LABEL
   s" : C2OP-COLLAPSE ( C2-MEM:owner<p,i,a> -- C2-MEM:owner<p,i,a> mut-view<p,p,fresh-region-b,u8> mut-view<p,p,fresh-region-b,u8> ) 16 MEM:BYTES-ALLOC-LEN C2-MEM:ALLOC swap 16 MEM:BYTES-ALLOC-LEN C2-MEM:ALLOC rot swap ;" REJECT TTRUE
   s" a raw-pointer disposer cannot receive an owned view" T-LABEL
   s" : C2OP-RAW-DISPOSE ( ptr u8 NUM:alloc-byte-len -- ) 2drop ; : C2OP-RAW ( C2-MEM:owner<p,i,a> -- C2-MEM:owner<p,i,a> mut-view<p,p,fresh-region-b,u8> ) 16 MEM:BYTES-ALLOC-LEN [: C2OP-RAW-DISPOSE ;] C2-MEM:ALLOC-DISPOSE ;" REJECT TTRUE
   s" a disposer tied to an existing region cannot claim a fresh one" T-LABEL
   s" : C2OP-RIGID ( C2-MEM:owner<p,i,a> NUM:alloc-byte-len [ mut-view<p,p,a,u8> -- ] -- C2-MEM:owner<p,i,a> mut-view<p,p,fresh-region-b,u8> ) C2-MEM:ALLOC-DISPOSE ;" REJECT TTRUE
   s" two disposer allocations cannot claim one fresh region" T-LABEL
   s" : C2OP-DROP-VIEW ( mut-view<p,p,a,u8> -- ) C2-MEM:PUBLISH drop ; : C2OP-TWO-DISPOSE ( C2-MEM:owner<p,i,a> -- C2-MEM:owner<p,i,a> mut-view<p,p,fresh-region-b,u8> mut-view<p,p,fresh-region-b,u8> ) 16 MEM:BYTES-ALLOC-LEN [: C2OP-DROP-VIEW ;] C2-MEM:ALLOC-DISPOSE swap 16 MEM:BYTES-ALLOC-LEN [: C2OP-DROP-VIEW ;] C2-MEM:ALLOC-DISPOSE rot swap ;" REJECT TTRUE
   s" the raw owner constructor cannot be called" T-LABEL
   s" : C2OP-CTOR ( ptr u8 n -- C2-MEM:owner<p,i,a> ) C2--MEM-OWNER:MAKE ;" REJECT TTRUE
   s" a mutable view cannot be unpacked outside the owner package" T-LABEL
   s" : C2OP-UNPACK ( mut-view<p,l,a,u8> -- ptr u8 n ) C2-MEM:MUT-UNPACK ;" REJECT TTRUE
   s" ticking the unpacker does not cross its internal boundary" T-LABEL
   s" : C2OP-TICK ( mut-view<p,l,a,u8> -- ptr u8 n ) ['] C2-MEM:MUT-UNPACK execute ;" REJECT TTRUE
   s" the product refuses a reopen of C2-MEM by name" T-LABEL
   s" package C2-MEM ;package" SEALED? TTRUE
   T-REPORT
   s" c2-owner-producer-refusals: ok" type cr ;

;package

C2-OWNER-PRODUCER-REFUSALS:RUN
