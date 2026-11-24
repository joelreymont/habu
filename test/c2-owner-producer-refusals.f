\ Checker and package boundaries of the append-only producer.
require src/lib/test.f
require src/lib/test/subject.f
require src/lib/c2-memory.f

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
   s" -1 REJECTION:JSON ! : C2OP-CATCH ( C2-MEM:owner<p,i,a> -- C2-MEM:owner<p,i,a> ) [: C2-OWNER-PRODUCER-REFUSALS:FAIL-APPEND ;] catch drop 17 MEM:BYTES-ALLOC-LEN C2-MEM:ALLOC C2-MEM:PUBLISH drop ;"
   70 s\" \"code\":\"E-STALE-READ\"" SUBJECT:REFUSED? TTRUE
   s" a wrapper cannot hide the failed append's consumed owner" T-LABEL
   s" -1 REJECTION:JSON ! : C2OP-WRAP-CATCH ( C2-MEM:owner<p,i,a> -- C2-MEM:owner<p,i,a> ) [: C2-OWNER-PRODUCER-REFUSALS:FAIL-WRAPPER ;] catch drop 17 MEM:BYTES-ALLOC-LEN C2-MEM:ALLOC C2-MEM:PUBLISH drop ;"
   70 s\" \"code\":\"E-STALE-READ\"" SUBJECT:REFUSED? TTRUE
   s" a throw before consuming an owner leaves the owner usable" T-LABEL
   s" : C2OP-EARLY-CATCH ( C2-MEM:owner<p,i,a> -- C2-MEM:owner<p,i,a> ) [: C2-OWNER-PRODUCER-REFUSALS:EARLY-THROW ;] catch drop 17 MEM:BYTES-ALLOC-LEN C2-MEM:ALLOC C2-MEM:PUBLISH drop ;" 0 SUBJECT:EXITED? TTRUE
   s" a trusted callee that throws leaves its owner stale" T-LABEL
   s" -1 REJECTION:JSON ! : C2OP-TRUSTED-OWNER-CATCH ( C2-MEM:owner<p,i,a> -- C2-MEM:owner<p,i,a> ) [: C2-OWNER-PRODUCER-REFUSALS:TRUSTED-THROW-OWNER ;] catch drop 17 MEM:BYTES-ALLOC-LEN C2-MEM:ALLOC C2-MEM:PUBLISH drop ;"
   70 s\" \"code\":\"E-STALE-READ\"" SUBJECT:REFUSED? TTRUE
   s" an early throw keeps an owner parked on the return stack usable" T-LABEL
   s" : C2OP-EARLY-R-OWNER-CATCH ( C2-MEM:owner<p,i,a> -- C2-MEM:owner<p,i,a> ) >r [: -9363 throw ;] catch drop r> 17 MEM:BYTES-ALLOC-LEN C2-MEM:ALLOC C2-MEM:PUBLISH drop ;" 0 SUBJECT:EXITED? TTRUE
   s" a trusted callee that throws leaves its read view stale" T-LABEL
   s" -1 REJECTION:JSON ! : C2OP-TRUSTED-VIEW-CATCH ( read-view<p,q,u8> -- u8 read-view<p,q,u8> ) [: C2-OWNER-PRODUCER-REFUSALS:TRUSTED-THROW-VIEW ;] catch drop 0 C2-MEM:BYTE@ ;"
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
   s" : C2OP-RAW-DISPOSE ( ^u8 NUM:alloc-byte-len -- ) 2drop ; : C2OP-RAW ( C2-MEM:owner<p,i,a> -- C2-MEM:owner<p,i,a> mut-view<p,p,fresh-region-b,u8> ) 16 MEM:BYTES-ALLOC-LEN [: C2OP-RAW-DISPOSE ;] C2-MEM:ALLOC-DISPOSE ;" 70 SUBJECT:EXITED? TTRUE
   s" a disposer tied to an existing region cannot claim a fresh one" T-LABEL
   s" : C2OP-RIGID ( C2-MEM:owner<p,i,a> NUM:alloc-byte-len [ mut-view<p,p,a,u8> -- ] -- C2-MEM:owner<p,i,a> mut-view<p,p,fresh-region-b,u8> ) C2-MEM:ALLOC-DISPOSE ;" 70 SUBJECT:EXITED? TTRUE
   s" two disposer allocations cannot claim one fresh region" T-LABEL
   s" : C2OP-DROP-VIEW ( mut-view<p,p,a,u8> -- ) C2-MEM:PUBLISH drop ; : C2OP-TWO-DISPOSE ( C2-MEM:owner<p,i,a> -- C2-MEM:owner<p,i,a> mut-view<p,p,fresh-region-b,u8> mut-view<p,p,fresh-region-b,u8> ) 16 MEM:BYTES-ALLOC-LEN [: C2OP-DROP-VIEW ;] C2-MEM:ALLOC-DISPOSE swap 16 MEM:BYTES-ALLOC-LEN [: C2OP-DROP-VIEW ;] C2-MEM:ALLOC-DISPOSE rot swap ;" 70 SUBJECT:EXITED? TTRUE
   s" a nested owner cannot bind a view ALLOC returned" T-LABEL
   s" : C2OP-NEST-INIT ( mut-view<p,i,C2-MEM:root<a>,init<i,C2-MEM:owner-state>> -- mut-view<p,i,C2-MEM:root<a>,init<i,C2-MEM:owner-state>> ) C2-MEM:BIND C2-MEM:UNBIND ; : C2OP-ODD-INIT ( mut-view<p,i,C2-MEM:root<a>,init<i,C2-MEM:owner-state>> -- mut-view<p,i,C2-MEM:root<a>,init<i,C2-MEM:owner-state>> ) C2-MEM:BIND C2-MEM:OWNER-SIZE C2-MEM:ALLOC C2-MEM:SEED-OWNER [: C2OP-NEST-INIT ;] C2-MEM:WITH-INIT C2-MEM:PUBLISH drop C2-MEM:UNBIND ; : C2OP-ODD-ROOT ( mut-view<p,p,C2-MEM:root<a>,u8> -- mut-view<p,p,C2-MEM:root<a>,u8> ) C2-MEM:SEED-OWNER [: C2OP-ODD-INIT ;] C2-MEM:WITH-INIT ; C2-MEM:OWNER-SIZE [: C2OP-ODD-ROOT ;] C2-MEM:WITH-MUT"
   70 s" error[E-MISMATCH]: the callback takes a `C2-MEM:root<a>` view, and the view given has another region" SUBJECT:REFUSED? TTRUE
   s" a trusted row cannot forge a root region" T-LABEL
   s" TRUSTED: C2OP-FORGE-ROOT ( mut-view<p,p,a,u8> -- mut-view<p,p,C2-MEM:root<a>,u8> ) ;"
   70 s" it gives a `C2-MEM:root<a>` view no input supplies" SUBJECT:REFUSED? TTRUE
   s" a defer row cannot forge a root region" T-LABEL
   s" defer C2OP-FORGE-DEFER ( mut-view<p,p,a,u8> -- mut-view<p,p,C2-MEM:root<a>,u8> )"
   70 s" it gives a `C2-MEM:root<a>` view no input supplies" SUBJECT:REFUSED? TTRUE
   s" a root region the row hands its callback is not supplied" T-LABEL
   s" TRUSTED: C2OP-FORGE-Q ( mut-view<p,p,a,u8> [ mut-view<p,p,C2-MEM:root<a>,u8> -- ] -- mut-view<p,p,C2-MEM:root<a>,u8> ) drop ;"
   70 s" take the `C2-MEM:root<a>` view as an input: only `C2-MEM:WITH-MUT` makes one" SUBJECT:REFUSED? TTRUE
   s" a callback's output supplies a root region" T-LABEL
   s" : C2OP-ROOT-Q ( [ -- mut-view<p,p,C2-MEM:root<a>,u8> ] -- mut-view<p,p,C2-MEM:root<a>,u8> ) execute ;" 0 SUBJECT:EXITED? TTRUE
   s" a row cannot lend its callback a root region it was not given" T-LABEL
   s" TRUSTED: C2OP-LEND-ROOT ( mut-view<p,p,a,u8> [ mut-view<p,p,C2-MEM:root<a>,u8> -- ] -- ) drop drop ;"
   70 s" it gives a `C2-MEM:root<a>` view no input supplies" SUBJECT:REFUSED? TTRUE
   s" a record's callback field supplies no root region" T-LABEL
   s" STRUCTURE c2op-rcb<p,b> FIELD run [ mut-view<p,p,b,u8> -- ] ;STRUCTURE TRUSTED: C2OP-RCB-ROOT ( mut-view<p,p,a,u8> c2op-rcb<p,C2-MEM:root<a>> -- mut-view<p,p,C2-MEM:root<a>,u8> ) drop ;"
   70 s" it gives a `C2-MEM:root<a>` view no input supplies" SUBJECT:REFUSED? TTRUE
   s" a returned callback's input supplies its output's root region" T-LABEL
   s" defer C2OP-RET-ROOT ( mut-view<p,p,a,u8> -- [ mut-view<p,p,C2-MEM:root<a>,u8> -- mut-view<p,p,C2-MEM:root<a>,u8> ] )" 0 SUBJECT:EXITED? TTRUE
   s" an owner's field supplies its root region" T-LABEL
   s" defer C2OP-OWNER-ROOT ( C2-MEM:owner<p,i,C2-MEM:root<a>> -- mut-view<p,p,C2-MEM:root<a>,u8> )" 0 SUBJECT:EXITED? TTRUE
   s" a phantom argument supplies no root region" T-LABEL
   s" STRUCTURE c2op-bx<t> FIELD raw n ;STRUCTURE TRUSTED: C2OP-BX-MK ( mut-view<p,i,a,init<i,C2-MEM:owner-state>> -- c2op-bx<mut-view<p,i,C2-MEM:root<a>,init<i,C2-MEM:owner-state>>> ) ; TRUSTED: C2OP-BX-UN ( c2op-bx<mut-view<p,i,b,init<i,C2-MEM:owner-state>>> -- mut-view<p,i,b,init<i,C2-MEM:owner-state>> ) ; TRUSTED: C2OP-BX-SINK ( mut-view<p,i,b,init<i,C2-MEM:owner-state>> -- ) ; : C2OP-BX-USE ( mut-view<p,i,a,init<i,C2-MEM:owner-state>> -- ) C2OP-BX-MK C2OP-BX-UN C2-MEM:BIND C2-MEM:UNBIND C2OP-BX-SINK ;"
   70 s" it gives a `C2-MEM:root<a>` view no input supplies" SUBJECT:REFUSED? TTRUE
   s" a phantom slot holds no root region a word can unbox" T-LABEL
   s" STRUCTURE c2op-bx<t> FIELD raw n ;STRUCTURE TRUSTED: C2OP-BX-W ( c2op-bx<mut-view<p,p,C2-MEM:root<a>,u8>> -- mut-view<p,p,C2-MEM:root<a>,u8> ) ;"
   70 s" it gives a `C2-MEM:root<a>` view no input supplies" SUBJECT:REFUSED? TTRUE
   s" a pointer's element supplies no root region" T-LABEL
   s" TRUSTED: C2OP-PTR-MK ( mut-view<p,i,a,init<i,C2-MEM:owner-state>> -- ptr<s,mut-view<p,i,C2-MEM:root<a>,init<i,C2-MEM:owner-state>>> ) ; TRUSTED: C2OP-PTR-UN ( ptr<s,mut-view<p,i,b,init<i,C2-MEM:owner-state>>> -- mut-view<p,i,b,init<i,C2-MEM:owner-state>> ) ; TRUSTED: C2OP-PTR-SINK ( mut-view<p,i,b,init<i,C2-MEM:owner-state>> -- ) ; : C2OP-PTR-USE ( mut-view<p,i,a,init<i,C2-MEM:owner-state>> -- ) C2OP-PTR-MK C2OP-PTR-UN C2-MEM:BIND C2-MEM:UNBIND C2OP-PTR-SINK ;"
   70 s" it gives a `C2-MEM:root<a>` view no input supplies" SUBJECT:REFUSED? TTRUE
   s" a callback's own output does not supply its input" T-LABEL
   s" TRUSTED: C2OP-LR ( mut-view<p,i,a,init<i,C2-MEM:owner-state>> [ mut-view<p,i,C2-MEM:root<a>,init<i,C2-MEM:owner-state>> -- mut-view<p,i,C2-MEM:root<a>,init<i,C2-MEM:owner-state>> ] -- mut-view<p,i,a,init<i,C2-MEM:owner-state>> ) ;"
   70 s" it gives a `C2-MEM:root<a>` view no input supplies" SUBJECT:REFUSED? TTRUE
   s" a callback in a two-argument record does not supply its own input" T-LABEL
   s" STRUCTURE c2op-ko<t,u> FIELD v t FIELD w u ;STRUCTURE TRUSTED: C2OP-KO-Q ( c2op-ko<[ mut-view<p,p,C2-MEM:root<a>,u8> -- mut-view<p,p,C2-MEM:root<a>,u8> ],n> -- ) ;"
   70 s" take the `C2-MEM:root<a>` view as an input: only `C2-MEM:WITH-MUT` makes one" SUBJECT:REFUSED? TTRUE
   s" a recursive family's declaration is checked" T-LABEL
   s" STRUCTURE c2op-pr<t> FIELD v t FIELD nx ^c2op-pr<^u8> ;STRUCTURE STRUCTURE c2op-grow<t> FIELD v t FIELD nx ^c2op-grow<^t> ;STRUCTURE" 0 SUBJECT:EXITED? TTRUE
   s" a callback over a plain region cannot bind" T-LABEL
   s" : C2OP-PLAIN-NEST ( mut-view<p,i,a,init<i,C2-MEM:owner-state>> -- mut-view<p,i,a,init<i,C2-MEM:owner-state>> ) C2-MEM:BIND C2-MEM:UNBIND ; : C2OP-PLAIN-INIT ( mut-view<p,i,a,init<i,C2-MEM:owner-state>> -- mut-view<p,i,a,init<i,C2-MEM:owner-state>> ) C2-MEM:BIND C2-MEM:OWNER-SIZE C2-MEM:ALLOC C2-MEM:SEED-OWNER [: C2OP-PLAIN-NEST ;] C2-MEM:WITH-INIT C2-MEM:PUBLISH drop C2-MEM:UNBIND ; : C2OP-PLAIN-ROOT ( mut-view<p,p,a,u8> -- mut-view<p,p,a,u8> ) C2-MEM:SEED-OWNER [: C2OP-PLAIN-INIT ;] C2-MEM:WITH-INIT ; C2-MEM:OWNER-SIZE [: C2OP-PLAIN-ROOT ;] C2-MEM:WITH-MUT"
   70 s" declared type variable `a` is specialized to family `c2-mem:root`" SUBJECT:REFUSED? TTRUE
   s" the raw owner constructor cannot be called" T-LABEL
   s" : C2OP-CTOR ( ^u8 n -- C2-MEM:owner<p,i,a> ) C2-MEM:OWNER-MAKE ;" 70 SUBJECT:EXITED? TTRUE
   s" a mutable view cannot be unpacked outside the owner package" T-LABEL
   s" : C2OP-UNPACK ( mut-view<p,l,a,u8> -- ^u8 n ) C2-MEM:MUT-UNPACK ;" 70 SUBJECT:EXITED? TTRUE
   s" ticking the unpacker does not cross its internal boundary" T-LABEL
   s" : C2OP-TICK ( mut-view<p,l,a,u8> -- ^u8 n ) ['] C2-MEM:MUT-UNPACK execute ;" 70 SUBJECT:EXITED? TTRUE
   \ The engine refuses `package C2-MEM` by name, exit 70, before the checker
   \ sees a body.
   s" the product refuses a reopen of C2-MEM by name" T-LABEL
   s" package C2-MEM ;package" 70 s" C2-MEM" SUBJECT:REFUSED? TTRUE
   T-REPORT
   s" c2-owner-producer-refusals: ok" type cr ;

;package

C2-OWNER-PRODUCER-REFUSALS:RUN
