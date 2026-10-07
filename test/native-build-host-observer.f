\ A former engine's fixed observer registration enters the real build path.
package NATIVE-BUILD-HOST-OBSERVER
: CALLBACK ( -- ) ;
' CALLBACK data-base $2CF0 + xt!
;package
