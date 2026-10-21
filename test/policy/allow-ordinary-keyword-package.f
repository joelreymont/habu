\ Admit a package whose ordinary public word is named require, then read a
\ separate source unit under the seal so that it resolves through the gate.
require src/lib/vocabulary.f

package PUSER
public
: require ( -- n ) 42 ;
: SAY ( n -- ) . ;
;package

: RUN ( -- )
   s" PUSER" VOCABULARY:ALLOW-PACKAGE VOCABULARY:RESTRICT
   0 SCRIPT-ARGV$ included ;
RUN
