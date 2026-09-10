\ Evaluate definition-only fixture source with one result on either path.
require lib/prelude.f

package NATIVE-EVAL
private

PTR-VARIABLE SOURCE
variable LENGTH


TRUSTED: EVALUATE-SOURCE ( -- )
   SOURCE @ LENGTH @ evaluate ;

public


: DEFINE-RC ( ptr u8 n -- n )
   LENGTH ! SOURCE !
   [: EVALUATE-SOURCE ;] catch ;

;package
