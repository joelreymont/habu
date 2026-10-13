\ engine-id-test.f - focused tests for src/lib/engine-id.f (self path + content key).
\ Run: bin/hb --load src/lib/errors.f src/lib/string.f src/lib/test.f src/lib/engine-id.f src/lib/engine-id-test.f

require src/lib/errors.f
require src/lib/string.f
require src/lib/test.f
require src/lib/engine-id.f

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
