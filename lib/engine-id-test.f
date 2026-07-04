\ engine-id-test.f - focused tests for lib/engine-id.f (self path + content key).
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/engine-id.f lib/engine-id-test.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/engine-id.f

64 constant EIDT-KEY-LEN
create EIDT-KEY2 EIDT-KEY-LEN allot

: EIDT-PATH ( -- )
   ENGINE-PATH$ nip 0 > TTRUE                       \ resolves non-empty (else fail-closed throw)
   ENGINE-PATH$ ENGINE-PATH$ STR= TTRUE ;           \ recorded once, stable across calls

: EIDT-KEY ( -- )
   ENGINE-KEY$ nip EIDT-KEY-LEN T=                  \ 64-char hex digest
   ENGINE-KEY$ ENGINE-KEY$ STR= TTRUE               \ cached, stable across calls
   ENGINE-PATH$ EIDT-KEY2 SHA256-FILE-HEX 0 T=      \ the binary is hashable at that path
   ENGINE-KEY$ EIDT-KEY2 EIDT-KEY-LEN STR= TTRUE ;  \ cached key == real content hash

: EIDT-MAIN ( -- )
   T-RESET
   EIDT-PATH
   EIDT-KEY
   T-REPORT ;

EIDT-MAIN
