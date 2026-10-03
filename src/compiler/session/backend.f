\ backend.f - a provider identity and lease over the existing compiler context.
require src/compiler/target.f
require src/compiler/ir/context.f
require src/compiler/session/lease.f

package NSESSION
public

STRUCTURE session 0
   FIELD ctx IR-CTX:ctx
   FIELD provider CTARGET:backend-id
   FIELD lease NLEASE:lease
;STRUCTURE

private

variable WORK-OWNER

: CTX ( NSESSION:session -- IR-CTX:ctx )
   NSESSION-SESSION:UNMAKE 2drop ;

: LEASE ( NSESSION:session -- NLEASE:lease )
   NSESSION-SESSION:UNMAKE {: c:IR-CTX:ctx id:CTARGET:backend-id l:NLEASE:lease :}
   l ;

: CLEAR ( -- )
   0 WORK-OWNER ! ;

: ENTER ( R NSESSION:session [ R NSESSION:session -- S ] -- S )
   {: body :}
   dup CTX IR-CTX:SERIAL WORK-OWNER !
   body [: CLEAR ;] finally ;

public

: NEW ( IR-CTX:ctx NLEASE:lease -- NSESSION:session )
   {: c:IR-CTX:ctx l:NLEASE:lease :}
   l NLEASE:CHECK
   c IR-CTX:BINDING@ CBIND:TARGET@ CTARGET:ARCH@ CTARGET:ROW CTARGET:BACKEND@
   CTARGET-BACKEND:UNMAKE {: id:CTARGET:backend-id arch:CTARGET:arch lower emit :}
   c id l NSESSION-SESSION:MAKE ;

: RESOLVE ( NSESSION:session -- IR-CTX:ctx n )
   NSESSION-SESSION:UNMAKE {: c:IR-CTX:ctx id:CTARGET:backend-id l:NLEASE:lease :}
   l NLEASE:WORK-CK
   c IR-CTX:BINDING@ CBIND:TARGET@ CTARGET:ARCH@ {: arch:CTARGET:arch :}
   c IR-CTX:SERIAL WORK-OWNER @ <> if NLEASE:E-STATE throw then
   id CTARGET:ID-ROW {: row:n :}
   row CTARGET:BACKEND@ CTARGET-BACKEND:UNMAKE
   {: found:CTARGET:backend-id target:CTARGET:arch lower emit :}
   arch target CTARGET-ARCH:EQ 0= if NLEASE:E-STATE throw then
   c row ;

: WITH-WORK ( R NSESSION:session [ R NSESSION:session -- S ] -- S )
   {: body :}
   dup CTX IR-CTX:BINDING@ drop
   dup LEASE {: l:NLEASE:lease :}
   body l [: ENTER ;] NLEASE:WORK ;

;package
