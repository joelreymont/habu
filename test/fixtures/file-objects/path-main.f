\ path-main.f - a program whose top level fills ENGINE-ID's path cache, built by
\ tools/hb-build.f before and after bare-main.f; its MAIN prints the running
\ program's own path.

require src/lib/engine-id.f

package FILE-OBJECTS-PATH-MAIN
ENGINE-ID:PATH$ drop drop
public
: SHOW ( -- ) ENGINE-ID:PATH$ type cr ;
;package

: MAIN ( -- ) FILE-OBJECTS-PATH-MAIN:SHOW ;
