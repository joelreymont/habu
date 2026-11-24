split-str
\ mapped.f - test-only witnesses: is a byte span still mapped, and did a body
\ release the mappings it took? A growth path proves its release by the old
\ span's absence: write(2) from an unmapped source answers EFAULT, from a mapped
\ one the byte count. Capture the base before growth, ask LIVE? after. The sink
\ must be a real file: /dev/null never reads the buffer and answers 1 for a
\ released span (measured; mapped-test.f asserts both answers).
\ STORAGE CLASS. PROCESS-WIDE: the sink path, its descriptor and the probe
\ addresses are static.

require src/lib/error.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/memory.f

package MAPPED
private

create SINK-DIR PATH-CAP allot
create SINK-PATH PATH-CAP allot
variable SINK-FD

: DIR! ( str -- n ) {: a:^ u:n :}
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

4096 constant HOLE-BYTES
8 constant HOLE-COUNT
HOLE-COUNT TYPED-BUFFER HOLE-AT ^u8

: HOLE-LEN ( -- NUM:alloc-byte-len ) HOLE-BYTES MEM:BYTES-ALLOC-LEN ;
: PROBE ( -- ^u8 ) HOLE-LEN MEM:ALLOC-BYTES drop ;

: FREE-HOLES ( -- )
   HOLE-COUNT 0 ?do i HOLE-AT @ HOLE-LEN MEM:RELEASE-BYTES loop ;

\ Every probe is mapped before any is freed, so the holes are distinct.
: HOLES ( -- )
   HOLE-COUNT 0 ?do PROBE i HOLE-AT ! loop
   FREE-HOLES ;

: FILLED? ( -- bool )
   true HOLE-COUNT 0 ?do
      PROBE {: again:^ :}
      again i HOLE-AT @ = and
      again i HOLE-AT !
   loop
   FREE-HOLES ;

public

: LIVE? ( ^u8 -- bool )
   SINK swap 1 write 0 > ;

\ Whether eight freed one-page probes map again where they were after the
\ body: a leaked page takes a hole, a wider leak only where enough adjoin.
: RELEASED? ( [ -- ] -- bool )
   \ A body opening the task's first MEM pool scope needs a warm-up call
   \ first, as the pool keeps that chunk.
   HOLES execute FILLED? ;

;package
