require test/fixtures/native-file-state-prefix.f
package FILE-TYPES
public
NEWTYPE item
STRUCTURE pair FIELD left n FIELD right n ;STRUCTURE
STRUCTURE bundle<a> DERIVE init FIELD value ptr a FIELD inner pair ;STRUCTURE
SUMTYPE answer<a> VARIANT good a ;VARIANT VARIANT bad n ;VARIANT ;SUMTYPE
DEFLINEAR FILE-TYPES:token
SUMTYPE choice VARIANT call [ n -- n ] ;VARIANT VARIANT scalar n ;VARIANT ;SUMTYPE
TYPED-VARIABLE SAVED choice
: INIT-SAVED ( -- ) 456 construct choice scalar SAVED ! ;
: SAVED-VALUE ( -- n ) SAVED @ MATCH choice call OF 10 swap execute ENDOF scalar OF ENDOF ;MATCH ;
INIT-SAVED
defer CHOOSE ( n -- n )
: CHECKED ( n -- n ) 1+ ;
;package
