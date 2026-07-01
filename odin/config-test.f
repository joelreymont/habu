\ config-test.f - rig-validation oracle, ported from src/config.zig tests.
\ Run: ../habu/bin/hb --load odin/config-test.f

require lib/errors.f
require lib/string.f
require lib/test.f
require odin/config.f

package CONFIG
private
create S1001 49 c, 48 c, 48 c, 49 c,
create S1002 49 c, 48 c, 48 c, 50 c,
create S1003 49 c, 48 c, 48 c, 51 c,
create S1004 49 c, 48 c, 48 c, 52 c,

\ add an example camera: identity set, geometry per `geom`
: ADD-CAM ( ptr u8 n n -- ) {: sa:ptr sn:n geom:n :}   \ serial + geom flag
   sa sn  6 1 11 geom ADD ;                      \ name/sdk/model lengths > 0

: SETUP-RIG ( -- )                                  \ 4 unique cameras, no geometry
   RESET
   S1001 4 0 ADD-CAM  S1002 4 0 ADD-CAM  S1003 4 0 ADD-CAM  S1004 4 0 ADD-CAM ;

: CFG-RUN ( -- )
   T-RESET
   \ four unique serials -> full rig OK
   SETUP-RIG  FULL-RIG OK T=
   \ duplicate serial -> DUPLICATE-SERIAL
   RESET  S1001 4 0 ADD-CAM  S1001 4 0 ADD-CAM  S1003 4 0 ADD-CAM  S1004 4 0 ADD-CAM
   FULL-RIG DUPLICATE-SERIAL T=
   \ full rig but no geometry -> localization not ready
   SETUP-RIG  LOC-READY UNKNOWN-GEOMETRY T=
   \ full rig with geometry on every camera -> localization ready
   RESET
   S1001 4 -1 ADD-CAM  S1002 4 -1 ADD-CAM  S1003 4 -1 ADD-CAM  S1004 4 -1 ADD-CAM
   LOC-READY OK T=
   \ wrong camera count -> not a full rig
   RESET  S1001 4 0 ADD-CAM  S1002 4 0 ADD-CAM  S1003 4 0 ADD-CAM
   FULL-RIG NOT-FULL-RIG T=
   \ no cameras -> missing camera
   RESET  UNIQUE MISSING-CAMERA T=
   \ empty serial -> missing identity
   RESET  S1001 0 0 ADD-CAM
   UNIQUE MISSING-IDENTITY T= ;

CFG-RUN
T-REPORT
end-package
