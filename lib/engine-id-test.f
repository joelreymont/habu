\ engine-id-test.f - focused tests for lib/engine-id.f (self path + content key).
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/engine-id.f lib/engine-id-test.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/engine-id.f

64 constant EIDT-KEY-LEN
create EIDT-FSHA-CTX SHA256-FILE-CTX-BYTES allot   \ this fixture's file-digest context
create EIDT-KEY2 EIDT-KEY-LEN allot

: EIDT-KEY ( -- )
   ENGINE-ID:KEY$ nip EIDT-KEY-LEN T=               \ 64-char hex digest
   EIDT-FSHA-CTX ENGINE-ID:PATH$ EIDT-KEY2 SHA256-FILE-HEX-IN 0 T=   \ the binary is hashable at that path
   ENGINE-ID:KEY$ EIDT-KEY2 EIDT-KEY-LEN STR= TTRUE ;  \ cached key == real content hash

: EIDT-MAIN ( -- )
   T-RESET
   EIDT-KEY
   T-REPORT ;

EIDT-MAIN
