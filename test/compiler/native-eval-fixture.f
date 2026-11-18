split-str
\ Evaluate definition-only fixture source with one result on either path.
require src/lib/prelude.f

package NATIVE-EVAL
private

PTR-VARIABLE SOURCE
variable LENGTH


: EVALUATE-SOURCE ( -- )
   SOURCE @ LENGTH @ evaluate-closed ;

public


: DEFINE-RC ( str -- n )
   LENGTH ! SOURCE !
   [: EVALUATE-SOURCE ;] catch ;

;package
