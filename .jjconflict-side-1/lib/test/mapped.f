\ mapped.f - test-only witness: is a byte span still mapped?
\
\ A growth path is proved to release its old span by the ABSENCE of that mapping,
\ never by a memory metric with slack. write(2) copies from the caller's buffer,
\ so it answers EFAULT for an unmapped source and the byte count for a mapped
\ one: one byte decides it, with no magic number and no tolerance. A caller
\ captures a buffer's base BEFORE the growth step and asks LIVE? after it; a path
\ that forgot its release answers true.
\
\ THE SINK MUST BE A REAL FILE. /dev/null's write handler returns the count
\ without ever reading the buffer, so it answers 1 for a released span and the
\ witness goes blind - measured, and the reason lib/test/mapped-test.f asserts
\ both answers rather than just the true one. The sink is unlinked as soon as it
\ is open: the descriptor keeps the inode alive for the run and no file is left
\ behind. Its contents are never read, so concurrent test processes cannot
\ disturb each other.
\
\ Load after lib/errors.f, lib/fs.f and lib/fs-mutate.f.

require lib/errors.f
require lib/fs.f
require lib/fs-mutate.f

package MAPPED
private

create SINK-DIR FS-PATH-CAP allot
create SINK-PATH FS-PATH-CAP allot
variable SINK-FD

: DIR! ( ptr u8 n -- n ) {: a:ptr u:n :}
   a SINK-DIR u BYTE-COPY
   u ;

: OPEN-SINK ( -- n )
   s" habu-mapped" TMPDIR-MKDIR DIR! {: du:n :}
   SINK-DIR du s" sink" SINK-PATH JOIN-PATH {: u:n :}
   SINK-PATH u FS-PATHZ FS-O-WRONLY FS-O-CREAT or FS-O-TRUNC or FS-MODE-0644 open {: fd:n :}
   fd 0 < if E-FS-OPEN throw then
   SINK-PATH u REMOVE-FILE
   SINK-DIR du REMOVE-DIR
   fd SINK-FD !
   fd ;

\ fd 0 is stdin, so a zero cell means "not opened yet", never a usable sink.
: SINK ( -- n )
   SINK-FD @ dup 0 > if exit then
   drop OPEN-SINK ;

public

: LIVE? ( ptr u8 -- bool )
   SINK swap 1 write 0 > ;

;package
