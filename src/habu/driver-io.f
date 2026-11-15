\ driver-io.f - writes a build's image file. It reaches MBUF, so a booted
\ engine cannot load it; what a booted engine needs lives in src/habu/fdio.f
\ (FDIO:WALL) and src/habu/sign-id.f.

require src/habu/fdio.f
require src/os/image-bytes.f
using IMAGE-BYTES

package DRIVER
variable DRV-WFD

: DRV-WRITE-IMAGE-PATH ( ptr u8 n -- )
   PATH0 1537 493 open DRV-WFD !
   DRV-WFD @ 0 < IF s" driver: cannot open output" 74 die THEN
   DRV-WFD @ MBUF MLEN@ FDIO:WALL
   DRV-WFD @ close ;

public
: DRV-WRITE-IMAGE ( img ptr u8 n -- ) {: phase path:ptr pathu :}
   phase IMG-DROP
   path pathu DRV-WRITE-IMAGE-PATH ;
;package
;using
