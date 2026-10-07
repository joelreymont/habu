\ Compile the same hostile-tag dispatch without loading the parent test suite
\ inside the child's timeout window.
require lib/prelude.f

package NMX-FORGE
private

ENUM hue
   red
   green
   blue
;ENUM

: E-HUE ( hue -- n )
   MATCH hue
      red OF 10 ENDOF
      green OF 20 ENDOF
      blue OF 30 ENDOF
   ;MATCH ;

\ The hostile tag: a plain cell that names no variant of hue.
CAST: >HUE ( n -- hue )

public

: FORGE ( -- )
   99 >HUE E-HUE drop ;

;package

NMX-FORGE:FORGE
