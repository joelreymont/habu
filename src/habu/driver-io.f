split-str
\ driver-io.f - writes a build's image file. It reaches MBUF, so a booted
\ engine cannot load it; what a booted engine needs lives in src/habu/fdio.f
\ (FDIO:WALL) and src/habu/sign-id.f.

require src/core/exit-status.f
require src/habu/fdio.f
require src/os/image-bytes.f
using IMAGE-BYTES

package DRIVER
variable DRV-WFD

: DRV-WRITE-IMAGE-PATH ( str -- )
   PATH0 1537 493 open DRV-WFD !
   DRV-WFD @ 0 < IF s" driver: cannot open output" EXIT-STATUS:IOERR die THEN
   DRV-WFD @ MBUF MLEN@ FDIO:WALL
   DRV-WFD @ close ;

public
: DRV-WRITE-IMAGE ( img str -- ) {: phase path:^ pathu :}
   phase IMG-DROP
   path pathu DRV-WRITE-IMAGE-PATH ;
;package
;using
