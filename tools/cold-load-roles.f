\ cold-load-roles.f - the host's checker and compiler roles after tools/cold-load.f
\ reloads the tree: tools/build-producer.f's digests of the sources the reload
\ read, installed as tools/native-build-core.f PREFIX-COMPILER-FINISH installs
\ a target's. cold-load.f loads this after src/habu/native-runtime.f.
require src/core/sha256.f
require src/habu/xref.f
require tools/build-producer.f

package COLD-ROLES
private

CAST: ROLES-XT ( n -- [ ptr u8 ptr u8 [ ptr u8 n -- ] -- ] )
CAST: ENTRY-XT ( n -- [ ptr u8 n -- ] )

create SHA SHA256-CTX-BYTES allot

: FAIL ( ptr u8 n -- ) 76 die ;

\ INSTALL-ROLES is private to SOURCE-PRODUCER: no qualified name reaches it.
: INSTALLER ( -- [ ptr u8 ptr u8 [ ptr u8 n -- ] -- ] )
   s" SOURCE-PRODUCER" XREF-NAMESPACE-WL XREF-FIND-WL
   dup XREF-FOUND? 0= if drop s" cold-load: SOURCE-PRODUCER missing" FAIL then
   XREF-PKG-PRIVATE  s" INSTALL-ROLES" rot XREF-FIND-WL
   dup XREF-FOUND? 0= if drop s" cold-load: INSTALL-ROLES missing" FAIL then
   XREF-START ROLES-XT ;

: ROLE ( ptr u8 n bool -- ptr u8 )
   {: digest:ptr size:n known:bool :}
   known 0= size 32 <> or if s" cold-load: missing loaded producer role" FAIL then
   digest ;

: DIGEST ( ptr u8 n ptr u8 -- )
   {: a:ptr u:n out:ptr :}
   SHA a u out SHA256-IN ;

public

\ REPLAY is COLD-SOURCES:REPLAY, which cold-load.f loads without registering
\ it, so this file cannot require it.
: INSTALL ( [ [ ptr u8 n ptr u8 n ptr u8 n -- ] [ -- ] [ ptr u8 n ptr u8 n -- ] [ ptr u8 n ptr u8 -- ] -- ] -- )
   {: replay :}
   BUILD-PRODUCER:START
   RTARGET:HOST-TARGET RTARGET:PROFILE$ BUILD-PRODUCER:OPTION
   [: BUILD-PRODUCER:OBSERVE ;] [: BUILD-PRODUCER:FREEZE-CHECKER ;]
      [: BUILD-PRODUCER:COMPILER-DONE ;] [: DIGEST ;] replay execute
   BUILD-PRODUCER:CHECKER$ ROLE
   BUILD-PRODUCER:COMPILER$ ROLE
   data-base NCOMP-DISPATCH:XT-CELL + CELL-VIEW @ ENTRY-XT
   INSTALLER execute ;

;package
