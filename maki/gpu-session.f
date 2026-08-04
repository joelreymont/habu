\ gpu-session.f - persistent checked GPU driver lifetime.

require lib/memory.f
require lib/adt/result.f
require maki/cuda-run.f

package GPU

public

DEFLINEAR GPU:session

private

0 constant GS-DEV-OFF
1 cells constant GS-CTX-OFF
2 cells constant GS-STREAM-OFF
3 cells constant GS-BOUND-OFF
4 cells constant GS-BYTES

1 constant GS-NONBLOCKING

: GS-LEN ( -- CAD-NUM:alloc-byte-len )
   GS-BYTES MEM:BYTES-ALLOC-LEN ;

\ The checker cannot bind a ptr n backing block to an opaque linear owner.
\ Retirement owner: habu-checker-ptr-lifetime-f59d1e9d.
TRUSTED: GS-MINT ( ptr n -- GPU:session ) ;

\ The checker cannot recover a linear owner's ptr n backing block.
\ Retirement owner: habu-checker-ptr-lifetime-f59d1e9d.
TRUSTED: GS-TAKE ( GPU:session -- ptr n ) ;

\ The checker cannot retype an allocated byte span as a cell block.
\ Retirement owner: habu-checker-ptr-lifetime-f59d1e9d.
TRUSTED: HOST-BLOCK ( ptr u8 -- ptr n ) ;

\ The checker cannot retype a cell block as its allocated byte span.
\ Retirement owner: habu-checker-ptr-lifetime-f59d1e9d.
TRUSTED: HOST-BYTES> ( ptr n -- ptr u8 ) ;

: GS@ ( ptr n n -- n )
   + @ ;

: GS! ( n ptr n n -- )
   + ! ;

: GS-DEV ( ptr n -- cuda-dev )
   GS-DEV-OFF GS@ >CUDA-DEV ;

: GS-CTX ( ptr n -- cuda-ctx )
   GS-CTX-OFF GS@ >CUDA-CTX ;

: GS-STREAM ( ptr n -- CUDA:stream )
   GS-STREAM-OFF GS@ CUDA:>STREAM ;

: GS-INIT ( ptr n -- ) {: st:ptr :}
   0 st GS-DEV-OFF GS!
   0 st GS-CTX-OFF GS!
   0 st GS-STREAM-OFF GS!
   0 st GS-BOUND-OFF GS! ;

: GS-ALLOC ( ptr u8 -- ptr u8 )
   drop GS-LEN MEM:ALLOC-BYTES drop ;

: GS-NEW ( ptr u8 -- ptr n )
   HOST-BLOCK dup GS-INIT ;

: GS-FREE ( ptr n -- )
   HOST-BYTES> GS-LEN MEM:RELEASE-BYTES ;

: GS-OPEN-DRIVER ( ptr n -- ptr n n )
   [: MKD:OPEN ;] catch ;

: GS-FIRST ( n n -- n ) {: first:n code:n :}
   first 0= code 0 <> and if code else first then ;

: GS-RELEASE-BODY ( ptr n n -- ptr n n ) {: st:ptr first:n :}
   st
   first st GS-DEV MKD:CUDEVICEPRIMARYCTXRELEASE RC>N GS-FIRST ;

: GS-RELEASE ( ptr n n -- ptr n n )
   [: GS-RELEASE-BODY ;] catch GS-FIRST ;

: GS-ACQUIRE-BODY ( ptr n n -- ptr n n )
   drop
   dup {: st:ptr :}
   0 MKD:CUINIT RC>N dup 0 <> if exit then drop
   st GS-DEV-OFF + 0 >IDX MKD:CUDEVICEGET RC>N dup 0 <> if exit then drop
   st GS-CTX-OFF + st GS-DEV MKD:CUDEVICEPRIMARYCTXRETAIN RC>N
   dup 0 <> if 0 st GS-CTX-OFF GS! exit then drop
   st GS-CTX-OFF GS@ 0= if
      E-CUDA GS-RELEASE exit
   then
   st GS-CTX MKD:CUCTXSETCURRENT RC>N dup 0 <> if exit then drop
   1 st GS-BOUND-OFF GS!
   st GS-STREAM-OFF + GS-NONBLOCKING MKD:CUSTREAMCREATE RC>N
   dup 0 <> if 0 st GS-STREAM-OFF GS! exit then drop
   st GS-STREAM-OFF GS@ 0= if E-CUDA exit then
   0 ;

: GS-ACQUIRE ( ptr n -- ptr n n )
   0 [: GS-ACQUIRE-BODY ;] catch GS-FIRST ;

: GS-BIND-BODY ( ptr n n -- ptr n n ) {: st:ptr first:n :}
   st
   first st GS-CTX MKD:CUCTXSETCURRENT RC>N GS-FIRST ;

: GS-BIND ( ptr n n -- ptr n n )
   [: GS-BIND-BODY ;] catch GS-FIRST ;

: GS-SYNC-BODY ( ptr n n -- ptr n n ) {: st:ptr first:n :}
   st
   first st GS-STREAM MKD:CUSTREAMSYNCHRONIZE RC>N GS-FIRST ;

: GS-SYNC ( ptr n n -- ptr n n )
   [: GS-SYNC-BODY ;] catch GS-FIRST ;

: GS-DESTROY-BODY ( ptr n n -- ptr n n ) {: st:ptr first:n :}
   st
   first st GS-STREAM MKD:CUSTREAMDESTROY RC>N GS-FIRST ;

: GS-DESTROY ( ptr n n -- ptr n n )
   [: GS-DESTROY-BODY ;] catch GS-FIRST ;

: GS-UNBIND-BODY ( ptr n n -- ptr n n ) {: st:ptr first:n :}
   st
   first 0 >CUDA-CTX MKD:CUCTXSETCURRENT RC>N GS-FIRST ;

: GS-UNBIND ( ptr n n -- ptr n n )
   [: GS-UNBIND-BODY ;] catch GS-FIRST ;

: GS-CLOSE-BLOCK ( ptr n n -- ptr n n )
   GS-BIND
   GS-SYNC
   GS-DESTROY
   GS-UNBIND
   GS-RELEASE ;

: GS-ABORT-BLOCK ( ptr n n -- ptr n n )
   over GS-STREAM-OFF GS@ 0 <> if
      GS-SYNC
      GS-DESTROY
   then
   over GS-BOUND-OFF GS@ 0 <> if GS-UNBIND then
   over GS-CTX-OFF GS@ 0 <> if GS-RELEASE then ;

: GS-ABORT ( ptr n n -- n ) {: code:n :}
   code GS-ABORT-BLOCK {: first:n :}
   GS-FREE
   first ;

: GS-START ( ptr u8 -- result<GPU:session,n> )
   GS-NEW GS-OPEN-DRIVER {: code:n :}
   code 0 <> if code GS-ABORT RESULT:ERR exit then
   GS-ACQUIRE {: acq:n :}
   acq 0 <> if acq GS-ABORT RESULT:ERR exit then
   GS-MINT RESULT:OK ;

public

: OPEN ( -- result<GPU:session,n> )
   NULL$ drop [: GS-ALLOC ;] catch {: code:n :}
   code 0 <> if drop code RESULT:ERR exit then
   GS-START ;

: CLOSE ( GPU:session -- result<n,n> )
   GS-TAKE 0 GS-CLOSE-BLOCK {: code:n :}
   GS-FREE
   code 0= if 0 RESULT:OK else code RESULT:ERR then ;

;package
