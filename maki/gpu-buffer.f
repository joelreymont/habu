\ gpu-buffer.f - persistent checked GPU device-buffer ownership.

require maki/gpu-session.f

package GPU

public

DEFLINEAR GPU:buffer

private

using CAD-NUM
using MEM
using MKD

0 constant GB-DEV-OFF
1 cells constant GB-LEN-OFF
2 cells constant GB-BYTES

: GB-HOST-LEN ( -- CAD-NUM:alloc-byte-len )
   GB-BYTES BYTES-ALLOC-LEN ;

\ The checker cannot bind this opaque linear owner to its live host block.
\ Keep its mint/take boundary distinct; retirement owner:
\ habu-checker-ptr-lifetime-f59d1e9d.
TRUSTED: GB-MINT ( ptr n -- GPU:buffer ) ;

TRUSTED: GB-TAKE ( GPU:buffer -- ptr n ) ;

\ Typed buffer-header projections and cuda-devptr byte advance are not yet
\ checker-expressible. Retirement owner: habu-type-gpu-buf-dd0dda87.
TRUSTED: GB-LEN>N ( CAD-NUM:alloc-byte-len -- n ) ;

TRUSTED: GB-BYTE-LEN>N ( CAD-NUM:byte-len -- n ) ;

TRUSTED: GB-BYTE-OFF>N ( CAD-NUM:byte-off -- n ) ;

TRUSTED: GB-DEVPTR+ ( cuda-devptr CAD-NUM:byte-off -- cuda-devptr ) + ;

: GB-ALLOC-HOST ( ptr u8 -- ptr u8 )
   drop GB-HOST-LEN ALLOC-BYTES drop ;

: GB-INIT ( ptr n CAD-NUM:alloc-byte-len -- ptr n )
   {: buf:ptr len:CAD-NUM:alloc-byte-len :}
   0 buf GB-DEV-OFF GS!
   len GB-LEN>N buf GB-LEN-OFF GS!
   buf ;

: GB-NEW ( ptr u8 CAD-NUM:alloc-byte-len -- ptr n )
   swap HOST-BLOCK swap GB-INIT ;

: GB-FREE-HOST ( ptr n -- )
   HOST-BYTES> GB-HOST-LEN RELEASE-BYTES ;

: GB-DEV ( ptr n -- cuda-devptr )
   GB-DEV-OFF GS@ >CUDA-DEVPTR ;

: GB-RAW-DEV ( ptr n -- n )
   GB-DEV-OFF GS@ ;

: GB-RAW-LEN ( ptr n -- n )
   GB-LEN-OFF GS@ ;

: GB-PREPARE ( ptr n CAD-NUM:alloc-byte-len -- ptr n ptr n n )
   {: len:CAD-NUM:alloc-byte-len :}
   NULL$ drop [: GB-ALLOC-HOST ;] catch {: code:n :}
   code 0 <> if HOST-BLOCK code exit then
   len GB-NEW 0 ;

: GB-BIND-STEP ( ptr n ptr n n -- ptr n ptr n n )
   {: st:ptr buf:ptr first:n :}
   st first GS-BIND {: code:n :}
   drop
   st buf code ;

: GB-ALLOC-BODY ( ptr n ptr n n -- ptr n ptr n n )
   {: st:ptr buf:ptr first:n :}
   st buf
   first buf buf GB-RAW-LEN >LEN CUMEMALLOC RC>N GS-FIRST ;

: GB-ALLOC-CALL ( ptr n ptr n n -- ptr n ptr n n )
   [: GB-ALLOC-BODY ;] catch GS-FIRST ;

: GB-DEV-CODE ( ptr n ptr n n -- ptr n ptr n n )
   dup 0 <> if exit then
   over GB-RAW-DEV 0= if drop E-CUDA then ;

: GB-FREE-BODY ( ptr n ptr n n -- ptr n ptr n n )
   {: st:ptr buf:ptr first:n :}
   st buf
   first buf GB-DEV CUMEMFREE RC>N GS-FIRST ;

: GB-FREE-CALL ( ptr n ptr n n -- ptr n ptr n n )
   [: GB-FREE-BODY ;] catch GS-FIRST ;

: GB-DISCARD ( ptr n ptr n n -- ptr n ptr n n )
   over GB-RAW-DEV 0 <> if GB-FREE-CALL then
   {: code:n :}
   dup GB-FREE-HOST
   code ;

: GB-ALLOC-RAW ( ptr n CAD-NUM:alloc-byte-len -- ptr n ptr n n )
   GB-PREPARE
   dup 0 <> if exit then
   GB-ALLOC-CALL GB-DEV-CODE
   dup 0 <> if GB-DISCARD then ;

: GB-RANGE? ( ptr n CAD-NUM:byte-off CAD-NUM:byte-len -- bool )
   {: buf:ptr off:CAD-NUM:byte-off len:CAD-NUM:byte-len :}
   off GB-BYTE-OFF>N {: o:n :}
   len GB-BYTE-LEN>N {: u:n :}
   o buf GB-RAW-LEN > if 0 0 > exit then
   u buf GB-RAW-LEN o - <= ;

: GB-NONOVERLAP?
   ( CAD-NUM:byte-off CAD-NUM:byte-off CAD-NUM:byte-len -- bool )
   {: dst:CAD-NUM:byte-off src:CAD-NUM:byte-off len:CAD-NUM:byte-len :}
   dst GB-BYTE-OFF>N {: d:n :}
   src GB-BYTE-OFF>N {: s:n :}
   len GB-BYTE-LEN>N {: u:n :}
   d s <= if u s d - <= exit then
   u d s - <= ;

: GB-UPLOAD-BODY
   ( cuda-devptr ptr u8 CAD-NUM:byte-len n -- cuda-devptr ptr u8 CAD-NUM:byte-len n )
   {: dst:cuda-devptr src:ptr len:CAD-NUM:byte-len first:n :}
   dst src len
   first dst src len GB-BYTE-LEN>N >LEN CUMEMCPYHTOD RC>N GS-FIRST ;

: GB-UPLOAD-CALL
   ( cuda-devptr ptr u8 CAD-NUM:byte-len n -- cuda-devptr ptr u8 CAD-NUM:byte-len n )
   [: GB-UPLOAD-BODY ;] catch GS-FIRST ;

: GB-UPLOAD-RAW
   ( ptr n ptr n CAD-NUM:byte-off ptr u8 CAD-NUM:byte-len -- ptr n ptr n n )
   {: st:ptr buf:ptr off:CAD-NUM:byte-off src:ptr len:CAD-NUM:byte-len :}
   buf off len GB-RANGE? 0= if st buf E-BUF-BOUNDS exit then
   st buf 0 GB-BIND-STEP {: code:n :}
   drop drop
   code 0 <> if st buf code exit then
   st buf buf GB-DEV off GB-DEVPTR+ src len 0 GB-UPLOAD-CALL {: copy-code:n :}
   2drop 2drop drop
   st buf copy-code ;

: GB-DOWNLOAD-BODY
   ( ptr u8 cuda-devptr CAD-NUM:byte-len n -- ptr u8 cuda-devptr CAD-NUM:byte-len n )
   {: dst:ptr src:cuda-devptr len:CAD-NUM:byte-len first:n :}
   dst src len
   first dst src len GB-BYTE-LEN>N >LEN CUMEMCPYDTOH RC>N GS-FIRST ;

: GB-DOWNLOAD-CALL
   ( ptr u8 cuda-devptr CAD-NUM:byte-len n -- ptr u8 cuda-devptr CAD-NUM:byte-len n )
   [: GB-DOWNLOAD-BODY ;] catch GS-FIRST ;

: GB-DOWNLOAD-RAW
   ( ptr n ptr n CAD-NUM:byte-off ptr u8 CAD-NUM:byte-len -- ptr n ptr n n )
   {: st:ptr buf:ptr off:CAD-NUM:byte-off dst:ptr len:CAD-NUM:byte-len :}
   buf off len GB-RANGE? 0= if st buf E-BUF-BOUNDS exit then
   st buf 0 GB-BIND-STEP {: code:n :}
   drop drop
   code 0 <> if st buf code exit then
   st buf dst buf GB-DEV off GB-DEVPTR+ len 0 GB-DOWNLOAD-CALL {: copy-code:n :}
   2drop 2drop drop
   st buf copy-code ;

: GB-COPY-BODY
   ( cuda-devptr cuda-devptr CAD-NUM:byte-len n -- cuda-devptr cuda-devptr CAD-NUM:byte-len n )
   {: dst:cuda-devptr src:cuda-devptr len:CAD-NUM:byte-len first:n :}
   dst src len
   first dst src len GB-BYTE-LEN>N >LEN CUMEMCPYDTOD RC>N GS-FIRST ;

: GB-COPY-CALL
   ( cuda-devptr cuda-devptr CAD-NUM:byte-len n -- cuda-devptr cuda-devptr CAD-NUM:byte-len n )
   [: GB-COPY-BODY ;] catch GS-FIRST ;

: GB-COPY-CHECK
   ( ptr n CAD-NUM:byte-off CAD-NUM:byte-off CAD-NUM:byte-len -- n )
   {: buf:ptr dst:CAD-NUM:byte-off src:CAD-NUM:byte-off len:CAD-NUM:byte-len :}
   buf dst len GB-RANGE? 0= if E-BUF-BOUNDS exit then
   buf src len GB-RANGE? 0= if E-BUF-BOUNDS exit then
   dst src len GB-NONOVERLAP? 0= if E-BUF-OVERLAP exit then
   0 ;

: GB-COPY-ARGS
   ( ptr n CAD-NUM:byte-off CAD-NUM:byte-off -- cuda-devptr cuda-devptr )
   {: buf:ptr dst:CAD-NUM:byte-off src:CAD-NUM:byte-off :}
   buf GB-DEV dst GB-DEVPTR+
   buf GB-DEV src GB-DEVPTR+ ;

: GB-COPY-RAW
   ( ptr n ptr n CAD-NUM:byte-off CAD-NUM:byte-off CAD-NUM:byte-len -- ptr n ptr n n )
   {: st:ptr buf:ptr dst:CAD-NUM:byte-off src:CAD-NUM:byte-off len:CAD-NUM:byte-len :}
   buf dst src len GB-COPY-CHECK {: check:n :}
   check 0 <> if st buf check exit then
   st buf 0 GB-BIND-STEP {: bind:n :}
   2drop
   bind 0 <> if st buf bind exit then
   st buf buf dst src GB-COPY-ARGS len 0 GB-COPY-CALL {: copy:n :}
   2drop 2drop drop
   st buf copy ;

: GB-FREE-RAW ( ptr n ptr n -- ptr n n )
   0 GB-BIND-STEP GB-FREE-CALL {: code:n :}
   dup GB-FREE-HOST drop
   code ;

public

: ALLOC
   ( GPU:session CAD-NUM:alloc-byte-len -- GPU:session result<GPU:buffer,n> )
   swap GS-TAKE swap
   {: st:ptr len:CAD-NUM:alloc-byte-len :}
   st 0 GS-BIND {: bind-code:n :} drop
   bind-code 0 <> if st GS-MINT bind-code RESULT:ERR exit then
   st len GB-ALLOC-RAW {: code:n :}
   code 0 <> if drop GS-MINT code RESULT:ERR exit then
   swap GS-MINT swap
   GB-MINT RESULT:OK ;

: UPLOAD
   ( GPU:session GPU:buffer byte-off ptr u8 byte-len -- GPU:session GPU:buffer result<n,n> )
   {: off:CAD-NUM:byte-off src:ptr len:CAD-NUM:byte-len :}
   GB-TAKE swap GS-TAKE swap
   off src len GB-UPLOAD-RAW {: code:n :}
   GB-MINT swap GS-MINT swap
   code 0= if 0 RESULT:OK else code RESULT:ERR then ;

: DOWNLOAD
   ( GPU:session GPU:buffer CAD-NUM:byte-off ptr u8 CAD-NUM:byte-len -- GPU:session GPU:buffer result<n,n> )
   {: off:CAD-NUM:byte-off dst:ptr len:CAD-NUM:byte-len :}
   GB-TAKE swap GS-TAKE swap
   off dst len GB-DOWNLOAD-RAW {: code:n :}
   GB-MINT swap GS-MINT swap
   code 0= if 0 RESULT:OK else code RESULT:ERR then ;

: COPY
   ( GPU:session GPU:buffer CAD-NUM:byte-off CAD-NUM:byte-off CAD-NUM:byte-len -- GPU:session GPU:buffer result<n,n> )
   {: dst:CAD-NUM:byte-off src:CAD-NUM:byte-off len:CAD-NUM:byte-len :}
   GB-TAKE swap GS-TAKE swap
   dst src len GB-COPY-RAW {: code:n :}
   GB-MINT swap GS-MINT swap
   code 0= if 0 RESULT:OK else code RESULT:ERR then ;

\ Kernel argument marshalling must receive a cuda-devptr, while the session and
\ buffer remain the lifetime owners. The checker cannot bind that borrowed
\ address to those owners yet; retirement owner:
\ habu-checker-ptr-lifetime-f59d1e9d.
: SPAN
   ( GPU:session GPU:buffer CAD-NUM:byte-off CAD-NUM:byte-len -- GPU:session GPU:buffer result<cuda-devptr,n> )
   {: off:CAD-NUM:byte-off len:CAD-NUM:byte-len :}
   GB-TAKE swap GS-TAKE swap {: st:ptr buf:ptr :}
   buf off len GB-RANGE? 0= if
      st GS-MINT buf GB-MINT E-BUF-BOUNDS RESULT:ERR exit
   then
   st buf 0 GB-BIND-STEP {: code:n :} 2drop
   st GS-MINT buf GB-MINT
   code 0 <> if code RESULT:ERR exit then
   buf GB-DEV off GB-DEVPTR+ RESULT:OK ;

: FREE ( GPU:session GPU:buffer -- GPU:session result<n,n> )
   GB-TAKE swap GS-TAKE swap GB-FREE-RAW {: code:n :}
   GS-MINT
   code 0= if 0 RESULT:OK else code RESULT:ERR then ;

;using
;using
;using

;package
