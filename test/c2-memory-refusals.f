\ Refusal cases run in fresh evaluators of the admitted native C2 image.
require lib/test.f
require lib/test/subject.f
require lib/adt/option.f
require lib/task.f
require lib/c2-memory.f

package C2-MEMORY-REFUSALS
private

STRUCTURE holder 1 FIELD value a ;STRUCTURE

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

: STALE? ( ptr u8 n -- bool )
   OUT CAP >LEN ERR CAP >LEN 10000 >MS SUBJECT:RUN
   MATCH outcome
      exited OF 70 = ENDOF
      signaled OF drop false ENDOF
      timeout OF false ENDOF
   ;MATCH
   {: outu:len erru:len refused:bool :}
   refused outu LEN>N 0= and
   ERR erru LEN>N s" stale cell" CONTAINS? and ;

public

: RUN ( -- )
   T-RESET
   s" a read view uses the read byte operation" T-LABEL
   s" : C2-MEM-READ-OK ( read-view<p,q,u8> -- u8 read-view<p,q,u8> ) 0 C2-MEM:BYTE@ ;" 0 STATUS? TTRUE
   s" a mutable view uses the mutable byte operation" T-LABEL
   s" : C2-MEM-MUT-OK ( mut-view<p,q,a,u8> -- u8 mut-view<p,q,a,u8> ) 0 C2-MEM:MUT-BYTE@ ;" 0 STATUS? TTRUE
   s" native scoped invocation rejects a boolean finisher" T-LABEL
   s" 1 set-tier TRUSTED: C2-BAD-FIN ( -- ) [: ;] [: ;] [: if then ;] c2-invoke ;" 67 STATUS? TTRUE
   s" native scoped invocation is trusted-only" T-LABEL
   s" : C2-UNTRUSTED-INVOKE ( -- ) [: ;] [: ;] [: drop ;] c2-invoke ;" 70 STATUS? TTRUE
   s" tier 1 scoped invocation is trusted-only" T-LABEL
   s" 1 set-tier : C2-UNTRUSTED-INVOKE-P2 ( -- ) [: ;] [: ;] [: drop ;] c2-invoke ;" 70 STATUS? TTRUE
   s" an exclusive view cannot be copied" T-LABEL
   s" : C2-MEM-COPY ( mut-view<p,q,a,u8> -- mut-view<p,q,a,u8> ) dup drop ;" 70 STATUS? TTRUE
   s" an exclusive view cannot be dropped" T-LABEL
   s" : C2-MEM-DROP ( mut-view<p,q,a,u8> -- ) drop ;" 70 STATUS? TTRUE
   s" an ordinary local cannot capture exclusive authority" T-LABEL
   s" : C2-MEM-LOCAL ( mut-view<p,q,a,u8> -- mut-view<p,q,a,u8> ) {: held :} held ;" 70 STATUS? TTRUE
   s" a by-value sum cannot hide an exclusive view" T-LABEL
   s" : C2-MEM-OPTION ( mut-view<p,q,a,u8> -- ) OPTION:SOME drop ;" 70 STATUS? TTRUE
   s" a return-stack cell cannot hide exclusive authority" T-LABEL
   s" : C2-MEM-RETURN ( mut-view<p,q,a,u8> -- ) >r ;" 70 STATUS? TTRUE
   s" a typed global cannot hold exclusive authority" T-LABEL
   s" TYPED-VARIABLE C2-MEM-GLOBAL mut-view<p,q,a,u8>" 70 STATUS? TTRUE
   s" a task exit quotation cannot capture a borrowed local" T-LABEL
   s" : C2-MEM-TASK ( read-view<p,q,u8> -- ) {: view :} [: view drop ;] TASK:SELF TASK:AT-EXIT ;" 75 STATUS? TTRUE
   s" a mutable view cannot be relabeled as a raw pointer" T-LABEL
   s" : C2-MEM-RAW ( mut-view<p,q,a,u8> -- ptr u8 n ) ;" 70 STATUS? TTRUE
   s" a mutable view cannot be relabeled as shared" T-LABEL
   s" : C2-MEM-SHARE ( mut-view<p,q,a,u8> -- read-view<p,q,u8> ) ;" 70 STATUS? TTRUE
   s" distinct allocation regions cannot be exchanged" T-LABEL
   s" : C2-MEM-REGION ( mut-view<p,q,a,u8> -- mut-view<p,q,b,u8> ) ;" 70 STATUS? TTRUE
   s" a child ceiling cannot be widened to its parent" T-LABEL
   s" : C2-MEM-CEILING ( mut-view<p,l,a,u8> -- mut-view<p,p,a,u8> ) ;" 70 STATUS? TTRUE
   s" a read child cannot call the mutable writer" T-LABEL
   s" : C2-MEM-READ-WRITE ( read-view<p,q,u8> -- read-view<p,q,u8> ) 0 7 C2-MEM:MUT-BYTE! ;" 70 STATUS? TTRUE
   s" a callback cannot keep an extra owner authority" T-LABEL
   s" : C2-MEM-OWNER-ESCAPE ( -- ) 1 MEM:BYTES-ALLOC-LEN [: dup ;] C2-MEM:WITH-MUT drop ;" 70 STATUS? TTRUE
   s" a mutable parent cannot remain usable during a shared loan" T-LABEL
   s" : C2-MEM-PARENT-USE ( mut-view<p,q,a,u8> -- mut-view<p,q,a,u8> ) dup [: 0 C2-MEM:BYTE@ swap drop ;] C2-MEM:WITH-READ swap drop ;" 70 STATUS? TTRUE
   s" a read child cannot escape in a sum" T-LABEL
   s" : C2-MEM-CHILD-SUM ( mut-view<p,q,a,u8> -- mut-view<p,q,a,u8> ) [: dup OPTION:SOME swap ;] C2-MEM:WITH-READ swap drop ;" 70 STATUS? TTRUE
   s" a read child cannot escape in a record" T-LABEL
   s" : C2-MEM-CHILD-RECORD ( mut-view<p,q,a,u8> -- mut-view<p,q,a,u8> ) [: dup C2-MEMORY-REFUSALS-HOLDER:MAKE swap ;] C2-MEM:WITH-READ swap drop ;" 70 STATUS? TTRUE
   s" a read child cannot escape through a quotation result" T-LABEL
   s" : C2-MEM-CHILD-QUOTE ( mut-view<p,q,a,u8> -- mut-view<p,q,a,u8> ) [: 0 ['] C2-MEM:BYTE@ dup >r execute r> swap ;] C2-MEM:WITH-READ swap drop ;" 70 STATUS? TTRUE
   s" a read child cannot escape on the return stack" T-LABEL
   s" : C2-MEM-CHILD-RETURN ( mut-view<p,q,a,u8> -- mut-view<p,q,a,u8> ) [: dup >r ;] C2-MEM:WITH-READ r> drop ;" 70 STATUS? TTRUE
   s" a read child cannot escape through raw global storage" T-LABEL
   s" variable C2-MEM-SLOT : C2-MEM-CHILD-STORE ( mut-view<p,q,a,u8> -- mut-view<p,q,a,u8> ) [: dup C2-MEM-SLOT ! ;] C2-MEM:WITH-READ ;" 70 STATUS? TTRUE
   s" a caught exclusive callback cannot restore its consumed parent" T-LABEL
   s" : C2-MEM-MUT-THROW ( mut-view<p,l,a,u8> -- mut-view<p,l,a,u8> ) 1 throw ; : C2-MEM-MUT-LOAN-THROW ( mut-view<p,q,a,u8> -- mut-view<p,q,a,u8> ) [: C2-MEM-MUT-THROW ;] C2-MEM:WITH-MUT-LOAN ; : C2-MEM-MUT-CATCH ( mut-view<p,q,a,u8> -- mut-view<p,q,a,u8> ) [: C2-MEM-MUT-LOAN-THROW ;] catch drop 0 C2-MEM:MUT-BYTE@ swap drop ;" STALE? TTRUE
   s" a caught shared callback cannot restore its mutable parent" T-LABEL
   s" : C2-MEM-READ-THROW ( read-view<p,l,u8> -- read-view<p,l,u8> ) 1 throw ; : C2-MEM-READ-LOAN-THROW ( mut-view<p,q,a,u8> -- mut-view<p,q,a,u8> ) [: C2-MEM-READ-THROW ;] C2-MEM:WITH-READ ; : C2-MEM-READ-CATCH ( mut-view<p,q,a,u8> -- mut-view<p,q,a,u8> ) [: C2-MEM-READ-LOAN-THROW ;] catch drop 0 C2-MEM:MUT-BYTE@ swap drop ;" STALE? TTRUE
   s" an ordinary declaration cannot invent unique scope and region binders" T-LABEL
   \ The bad stored signature is rendered before its code is thrown, so it exits 70.
   s" TRUSTED: C2-MEM-FORGE ( -- mut-view<p,q,a,u8> ) 0 0 ;" 70 STATUS? TTRUE
   s" a reopened memory package cannot call the raw allocator" T-LABEL
   s" package C2-MEM private : C2-MEM-RAW-ALLOC ( R NUM:alloc-byte-len [ R ptr u8 n -- S ptr u8 n | U -- U ] | U -- S | U ) ALLOC-RUN ; ;package" 70 STATUS? TTRUE
   s" a reopened memory package cannot tick the raw allocator" T-LABEL
   s" package C2-MEM private : C2-MEM-TICK-ALLOC ( -- ) ['] ALLOC-RUN drop ; ;package" 70 STATUS? TTRUE
   s" a reopened memory package cannot export the raw allocator" T-LABEL
   s" package C2-MEM public EXPORT ALLOC-RUN ;package" 70 STATUS? TTRUE
   s" a reopened memory package cannot call the raw loan scope" T-LABEL
   s" package C2-MEM private : C2-MEM-RAW-LOAN ( R ptr u8 n [ R ptr u8 n -- S ptr u8 n | U -- U ] | U -- S ptr u8 n | U ) LOAN-RUN ; ;package" 70 STATUS? TTRUE
   s" a reopened memory package cannot tick the raw loan scope" T-LABEL
   s" package C2-MEM private : C2-MEM-TICK-LOAN ( -- ) ['] LOAN-RUN drop ; ;package" 70 STATUS? TTRUE
   s" a reopened memory package cannot export the raw loan scope" T-LABEL
   s" package C2-MEM public EXPORT LOAN-RUN ;package" 70 STATUS? TTRUE
   s" a standalone memory load cannot call runtime frame lookup" T-LABEL
   s" package C2-MEM private : C2-MEM-RAW-ROOT ( ptr u8 -- ptr n ) ROOT-FRAME ; ;package" 70 STATUS? TTRUE
   s" a standalone memory load cannot tick runtime frame lookup" T-LABEL
   s" package C2-MEM private : C2-MEM-TICK-ROOT ( -- ) ['] ROOT-FRAME drop ; ;package" 70 STATUS? TTRUE
   s" a standalone memory load cannot export runtime frame lookup" T-LABEL
   s" package C2-MEM public EXPORT ROOT-FRAME ;package" 70 STATUS? TTRUE
   s" a standalone memory load cannot call runtime append" T-LABEL
   s" package C2-MEM private : C2-MEM-RAW-APPEND ( ptr n NUM:alloc-byte-len [ ptr u8 NUM:alloc-byte-len -- ] -- ptr u8 n ) APPEND ; ;package" 70 STATUS? TTRUE
   s" a standalone memory load cannot tick runtime append" T-LABEL
   s" package C2-MEM private : C2-MEM-TICK-APPEND ( -- ) ['] APPEND drop ; ;package" 70 STATUS? TTRUE
   s" a standalone memory load cannot export runtime append" T-LABEL
   s" package C2-MEM public EXPORT APPEND ;package" 70 STATUS? TTRUE
   T-REPORT
   s" c2-memory-refusals: ok" type cr ;

;package

C2-MEMORY-REFUSALS:RUN
