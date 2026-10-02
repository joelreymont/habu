\ C2 exclusive-view intrinsic: a two-cell stack value carries exactly one
\ linear authority while its element parameter only describes the pointee.
\ Run through the source load path: bin/hb --load test/c2-mut-view.f

require lib/test.f
require lib/adt/option.f
require lib/test/subject.f

package C2-MUT-VIEW
private

DEFLINEAR C2-MUT-VIEW:tok

: KEEP ( mut-view<p,q,a,u8> -- mut-view<p,q,a,u8> ) ;
: MOVE ( mut-view<p,q,a,u8> n -- n mut-view<p,q,a,u8> ) swap ;
: WRAP ( mut-view<p,q,a,u8> -- option<mut-view<p,q,a,u8>> ) OPTION:SOME ;
: UNWRAP ( option<mut-view<p,q,a,u8>> -- mut-view<p,q,a,u8> )
   MATCH option
      none OF 1 throw ENDOF
      some OF ENDOF
   ;MATCH ;
: PHANTOM ( mut-view<p,q,a,tok> -- option<mut-view<p,q,a,tok>> ) OPTION:SOME ;

$4000 constant CAP
create OUT CAP allot
create ERR CAP allot

: REJECT ( ptr u8 n n -- bool ) {: expected:n :}
   OUT CAP >LEN ERR CAP >LEN 10000 >MS SUBJECT:RUN
   MATCH outcome
      exited OF expected = ENDOF
      signaled OF drop false ENDOF
      timeout OF false ENDOF
   ;MATCH
   >r 2drop r> ;

public

: CHECK ( -- )
   s" exclusive view cannot be copied" T-LABEL
   s" : C2-MUT-DUP ( mut-view<p,q,a,u8> -- mut-view<p,q,a,u8> mut-view<p,q,a,u8> ) dup ;" 70 REJECT TTRUE
   s" exclusive view cannot be dropped" T-LABEL
   s" : C2-MUT-DROP ( mut-view<p,q,a,u8> -- ) drop ;" 70 REJECT TTRUE
   s" exclusive view cannot be captured by a local" T-LABEL
   s" : C2-MUT-LOCAL ( mut-view<p,q,a,u8> -- mut-view<p,q,a,u8> ) {: value :} value ;" 70 REJECT TTRUE
   s" nested exclusive view cannot be copied" T-LABEL
   s" : C2-MUT-NEST-DUP ( option<mut-view<p,q,a,u8>> -- option<mut-view<p,q,a,u8>> option<mut-view<p,q,a,u8>> ) dup ;" 70 REJECT TTRUE
   s" nested exclusive view cannot be dropped" T-LABEL
   s" : C2-MUT-NEST-DROP ( option<mut-view<p,q,a,u8>> -- ) drop ;" 70 REJECT TTRUE
   s" linear element parameter does not permit copying the view" T-LABEL
   s" : C2-MUT-PHANTOM-DUP ( mut-view<p,q,a,tok> -- mut-view<p,q,a,tok> mut-view<p,q,a,tok> ) dup ;" 70 REJECT TTRUE
   s" a throw cannot restore overwritten exclusive authority" T-LABEL
   s" : C2-MUT-THROW ( mut-view<p,q,a,u8> n -- mut-view<p,q,a,u8> n ) swap 1 throw ; : C2-MUT-STALE ( mut-view<p,q,a,u8> n -- mut-view<p,q,a,u8> n ) ['] C2-MUT-THROW catch drop ;" 70 REJECT TTRUE
   s" a region is distinct from a scope" T-LABEL
   s" : C2-MUT-BAD-REGION ( mut-view<p,q,p,u8> -- mut-view<p,q,a,u8> ) ;" 70 REJECT TTRUE
   s" a scope is distinct from a region" T-LABEL
   s" : C2-MUT-BAD-SCOPE ( mut-view<a,q,a,u8> -- mut-view<p,q,a,u8> ) ;" 70 REJECT TTRUE
   s" raw storage cannot hold an exclusive view" T-LABEL
   s" variable C2-MUT-RAW : C2-MUT-STORE ( mut-view<p,q,a,u8> -- ) C2-MUT-RAW ! ;" 70 REJECT TTRUE
   s" typed pointer storage cannot hold an exclusive view" T-LABEL
   s" : C2-MUT-TYPED-STORE ( mut-view<p,q,a,u8> ptr mut-view<p,q,a,u8> -- ) ! ;" 70 REJECT TTRUE
   s" a typed global cannot hold an exclusive view" T-LABEL
   s" TYPED-VARIABLE C2-MUT-SLOT mut-view<p,q,a,u8>" 70 REJECT TTRUE
   s" two raw cells cannot introduce exclusive authority" T-LABEL
   s" : C2-MUT-FORGE ( -- mut-view<p,q,a,u8> ) 0 0 ;" 70 REJECT TTRUE
   s" a shared view cannot become exclusive by identity" T-LABEL
   s" : C2-MUT-UPGRADE ( read-view<p,q,u8> -- mut-view<p,q,a,u8> ) ;" 70 REJECT TTRUE
   s" cast cannot erase exclusive authority" T-LABEL
   s" NEWTYPE c2-mut-erase 1 CAST: C2-MUT-CAST ( c2-mut-erase<mut-view<p,q,a,u8>> -- n )" 67 REJECT TTRUE ;

;package

T-RESET
C2-MUT-VIEW:CHECK
T-REPORT
s" c2-mut-view: ok" type cr
