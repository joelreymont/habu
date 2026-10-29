\ mapped.f - test-only witness: is a byte span still mapped? A growth path proves
\ its release by the old span's absence: write(2) from an unmapped source answers
\ EFAULT, from a mapped one the byte count. Capture the base before growth, ask
\ LIVE? after. The sink must be a real file: /dev/null never reads the buffer and
\ answers 1 for a released span (measured; mapped-test.f asserts both answers).
\ STORAGE CLASS. PROCESS-WIDE: the sink path and descriptor are static.

require src/lib/errors.f
require src/lib/fs.f
require src/lib/fs-mutate.f

package MAPPED
private

create SINK-DIR PATH-CAP allot
create SINK-PATH PATH-CAP allot
variable SINK-FD

: DIR! ( ptr u8 n -- n ) {: a:ptr u:n :}
   a SINK-DIR u BYTE-COPY
   u ;

: OPEN-SINK ( -- n )
   s" habu-mapped" FS:MKTEMP-DIR DIR! {: du:n :}
   SINK-DIR du s" sink" SINK-PATH FS:JOIN-PATH {: u:n :}
   SINK-PATH u FS:>PATHZ FS:O-WRONLY FS:O-CREAT or FS:O-TRUNC or FS:MODE-0644 open {: fd:n :}
   fd 0 < if E-FS-OPEN throw then
   SINK-PATH u FS:REMOVE-FILE
   SINK-DIR du FS:REMOVE-DIR
   fd SINK-FD !
   fd ;

\ fd 0 is stdin, so a zero cell means "not opened yet", never a usable sink. The
\ sink is unlinked once open and never read, so concurrent test processes do not meet.
: SINK ( -- n )
   SINK-FD @ dup 0 > if exit then
   drop OPEN-SINK ;

public

: LIVE? ( ptr u8 -- bool )
   SINK swap 1 write 0 > ;

;package
