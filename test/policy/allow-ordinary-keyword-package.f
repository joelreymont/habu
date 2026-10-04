\ Admit a package whose ordinary public word is named require, then read a
\ separate source unit under the seal so that it resolves through the gate.
require lib/policy.f

package PUSER
public
: require ( -- n ) 42 ;
: SAY ( n -- ) . ;
;package

: RUN ( -- )
   s" PUSER" POLICY:ALLOW POLICY:SEAL
   0 SCRIPT-ARGV$ included ;
RUN
