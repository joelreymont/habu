\ Public origin consumers must observe source changes even when an EXPORT
\ keeps the same XT. Evaluated text inside a loaded file has no file span.
require src/lib/test.f
require src/lib/fs.f
require src/habu/xref.f

ENGINE-ORIGIN:ARM
package ORIGIN-LOOKUP-ALIAS
: VALUE ( -- n ) 42 ;
;package
include test/fixtures/engine-origin-first.f
require test/outer-loop-on.f

package ENGINE-ORIGIN-LOOKUP-TEST
private
variable SYMBOL
variable SEEN
create SOURCE 512 allot

: ALIAS ( n -- )
   {: sym:n :}
   sym ENGINE-ORIGIN:TARGET
      {: path:ptr pathu:n at:n end:n name:ptr nameu:n found:bool :}
   found name nameu s" VALUE" CORE-STR= and if
      sym SYMBOL ! 1 SEEN +!
   then ;

: COUNT-IN ( ptr u8 n -- n )
   0 SEEN ! ['] ALIAS ENGINE-ORIGIN:EACH-IN SEEN @ ;

: LOCATED? ( n -- bool )
   ENGINE-ORIGIN:TARGET >r 2drop 2drop 2drop r> ;

: LOCATION ( ptr u8 n -- )
   {: want:ptr wantu:n :}
   SYMBOL @ ENGINE-ORIGIN:TARGET
      {: path:ptr pathu:n at:n end:n name:ptr nameu:n found:bool :}
   found TTRUE
   path pathu want wantu T$=
   name nameu s" VALUE" T$=
   end at - 5 T=
   want wantu SOURCE 512 FS:READ-ALL {: size:n :}
   at 0 >= end size <= and TTRUE
   SOURCE at + end at - name nameu T$= ;

: ALIAS-XT ( -- n )
   s" ORIGIN-LOOKUP-ALIAS:VALUE" XREF-FIND XREF-START ;

public
: CAUGHT-CHILD ( -- )
   [: s" test/fixtures/engine-origin-first.f" included ;] 78 TTHROWSQ ;

: RUN ( -- )
   T-RESET
   s" located export warms lookup" T-LABEL
   s" test/fixtures/engine-origin-first.f" COUNT-IN 1 T=
   s" test/fixtures/engine-origin-first.f" LOCATION
   SYMBOL @ {: sym:n :}
   ALIAS-XT {: xt:n :}

   s" an unlocated same-XT export hides its old source" T-LABEL
   s" test/fixtures/engine-origin-eval.f" included
   ALIAS-XT xt T=
   sym LOCATED? TFALSE
   s" test/fixtures/engine-origin-first.f" COUNT-IN 0 T=

   s" a caught child refusal restores the parent's owned source" T-LABEL
   s" test/fixtures/engine-origin-parent.f" included
   s" test/fixtures/engine-origin-parent.f" COUNT-IN 1 T=
   s" test/fixtures/engine-origin-parent.f" LOCATION

   s" a shebang source uses its execution copy" T-LABEL
   s" test/fixtures/engine-origin-shebang.f" included
   s" test/fixtures/engine-origin-shebang.f" COUNT-IN 1 T=
   s" test/fixtures/engine-origin-shebang.f" LOCATION

   s" a new located export replaces the unlocated row" T-LABEL
   s" test/fixtures/engine-origin-next.f" included
   ALIAS-XT xt T=
   s" test/fixtures/engine-origin-next.f" COUNT-IN 1 T=
   SYMBOL @ sym T=
   s" test/fixtures/engine-origin-next.f" LOCATION
   s" test/fixtures/engine-origin-first.f" COUNT-IN 0 T=

   s" a rejected duplicate export keeps its prior origin" T-LABEL
   [: s" test/fixtures/engine-origin-first.f" included ;] 78 TTHROWS
   s" test/fixtures/engine-origin-next.f" COUNT-IN 1 T=
   s" test/fixtures/engine-origin-next.f" LOCATION
   s" test/fixtures/engine-origin-first.f" COUNT-IN 0 T=

   s" unknown symbols have no origin" T-LABEL
   0 LOCATED? TFALSE
   -1 LOCATED? TFALSE
   $7FFFFFFFFFFFFFFF LOCATED? TFALSE
   ENGINE-ORIGIN:DISARM
   T-REPORT ;
;package
ENGINE-ORIGIN-LOOKUP-TEST:RUN
