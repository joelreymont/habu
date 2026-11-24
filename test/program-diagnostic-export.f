\ A failed use of a re-exported signature names the checked consumer.
package XPS
public
: XP-INC ( n -- n ) 1+ ;
;package

package XPD
public
EXPORT XPS:XP-INC
;package

s" XPU3 ( -- n ) XPD:XP-INC" CHECK! drop
s" ok" type cr
