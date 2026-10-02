\ The C2 writer admits only the public runtime entries.
require lib/test.f
require lib/test/subject.f
require lib/c2-memory.f
require src/habu/xref.f

package C2-MEMORY-SHADOW
public
TRUSTED: WITH-MUT ( n -- n ) ;
TRUSTED: WITH-READ ( n -- n ) ;
;package

package C2-MEMORY-ALIAS
public
EXPORT C2-MEM:WITH-MUT
EXPORT C2-MEM:WITH-READ
;package

package C2-MEMORY-NATIVE-BOUNDARY
private

create OUT $1000 allot
create ERR $1000 allot

: STATUS? ( ptr u8 n n -- bool ) {: expected:n :}
   OUT $1000 >LEN ERR $1000 >LEN 10000 >MS SUBJECT:RUN
   MATCH outcome
      exited OF expected = ENDOF
      signaled OF drop false ENDOF
      timeout OF false ENDOF
   ;MATCH
   >r 2drop r> ;

: ENTRY ( ptr u8 n -- n )
   XREF-FIND dup XREF-FOUND? 0= if
      drop s" c2-memory-native-boundary: missing entry" 71 die
   then XREF-START ;

public

: RUN ( -- )
   T-RESET
   s" the real owner entry has kind 3" T-LABEL
   s" C2-MEM:WITH-MUT" ENTRY scope-kind? 3 T=
   s" the real shared loan entry has kind 2" T-LABEL
   s" C2-MEM:WITH-READ" ENTRY scope-kind? 2 T=
   s" the real exclusive loan entry has kind 4" T-LABEL
   s" C2-MEM:WITH-MUT-LOAN" ENTRY scope-kind? 4 T=
   s" the real initialized entry has kind 5" T-LABEL
   s" C2-MEM:WITH-INIT" ENTRY scope-kind? 5 T=
   s" another package's same-spelled words have no authority" T-LABEL
   s" C2-MEMORY-SHADOW:WITH-MUT" ENTRY scope-kind? 0 T=
   s" C2-MEMORY-SHADOW:WITH-READ" ENTRY scope-kind? 0 T=
   s" an alias of the exact code entry retains its authority" T-LABEL
   s" C2-MEMORY-ALIAS:WITH-MUT" ENTRY scope-kind? 3 T=
   s" C2-MEMORY-ALIAS:WITH-READ" ENTRY scope-kind? 2 T=
   s" checked compilation cannot tick a scoped entry" T-LABEL
   s" : C2-NATIVE-TICK ( -- ) ['] C2-MEM:WITH-MUT drop ;" 70 STATUS? TTRUE
   s" : C2-NATIVE-READ-TICK ( -- ) ['] C2-MEM:WITH-READ drop ;" 70 STATUS? TTRUE
   s" unchecked interpretation cannot tick a scoped entry" T-LABEL
   s" 0 set-check ' C2-MEM:WITH-MUT drop" 70 STATUS? TTRUE
   s" 0 set-check ' C2-MEM:WITH-READ drop" 70 STATUS? TTRUE
   s" unchecked compilation cannot tick a scoped entry" T-LABEL
   s" 0 set-check : C2-NATIVE-RAW-TICK ( -- ) ['] C2-MEM:WITH-MUT drop ;" 70 STATUS? TTRUE
   s" 0 set-check : C2-NATIVE-RAW-READ-TICK ( -- ) ['] C2-MEM:WITH-READ drop ;" 70 STATUS? TTRUE
   T-REPORT
   s" c2-memory-native-boundary: ok" type cr ;

;package

C2-MEMORY-NATIVE-BOUNDARY:RUN
