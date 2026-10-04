require src/habu/code-span.f
require src/habu/xref.f

package NATIVE-HOST-ALIAS
public
EXPORT NATIVE-HOST-SOURCE:HOST42
;package

s" NATIVE-HOST-SOURCE:HOST42" UNDEFINE-NAME

package NATIVE-HOST-SOURCE
public
: HOST42 ( -- n ) 99 ;
: NEW-CALLER ( -- n ) HOST42 ;
;package
