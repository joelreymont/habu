\ Evaluate definition-only fixture source with one result on either path.
require src/lib/prelude.f

package NATIVE-EVAL
private

TYPED-VARIABLE SOURCE str


: EVALUATE-SOURCE ( -- )
   SOURCE @ evaluate-closed ;

public


: DEFINE-RC ( str -- n )
   SOURCE !
   [: EVALUATE-SOURCE ;] catch ;

;package
