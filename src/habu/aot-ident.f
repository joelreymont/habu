\ aot-ident.f - what a chain capture was taken from: the closure it loaded and a
\ digest over its bytes. The closure is derived: the capture brackets include.f's
\ REQUIRE-N at window open and close (tools/aot-chain-capture.f), the metabuild
\ fills it from an artifact's list (src/habu/aot-file.f). Engine primitives only,
\ no registry read: the stdin metabuild host has no src/lib/, and the one
\ tools/bootstrap.sh builds does not compile src/core/include.f.

package AOT-IDENT

public
\ The table holds MAX paths of up to the tree's PATH-CAP bytes each
\ (src/core/util.f), so every path include.f loads is one it records. A
\ consumer that has to size storage for a whole closure derives it from those
\ two: src/habu/habu2.f assembles the paths into one image table and
\ src/habu/aot-file.f into one artifact section, and no second budget can
\ disagree with this one.
256 constant MAX               \ closure files; the chain measures 43
private

$10000 constant CHUNK          \ file read granularity for the streaming digest
$4A constant REFUSE-RC

create PATHS MAX PATH-CAP * allot
create LENS  MAX cells allot
create CHUNK-BUF CHUNK allot
\ This file's own digest context: one digest is open at a time here, so every
\ file's content digest is finished before the framed list is hashed through the
\ same context. FILE-DIGESTS is bounded scratch recomputed on every call, never a
\ content cache.
create SHA-CTX SHA256-CTX-BYTES allot
create FILE-DIGESTS MAX 32 * allot
create FRAME-WORD 8 allot
variable N

: SLOT ( n -- ptr u8 ) PATH-CAP * PATHS + ;
: LEN@ ( n -- n ) cells LENS + @ ;
: LEN! ( n n -- ) {: u:n ix:n :} u ix cells LENS + ! ;

: ?ROOM ( n -- ) {: k:n :}
   k MAX < if exit then
   s" aot-ident: closure exceeds the file table" REFUSE-RC die ;

: ?FITS ( n -- ) {: u:n :}
   u 0 < u PATH-CAP > or if
      s" aot-ident: closure path longer than the path cap" REFUSE-RC die
   then ;

public

\ Empty the table. A filler starts here, so a second fill cannot leave a tail of
\ the first one's paths behind it.
: RESET ( -- ) 0 N ! ;

\ Append one path, in load order. Both refusals are the table's own invariants -
\ more paths than it holds, or a path longer than a slot - and a path that does
\ not fit is refused rather than cut, because a cut path names a different file
\ and would still hash. The length is the caller's, so a negative one, which no
\ slot holds either, is refused with the long one.
: PATH+ ( ptr u8 n -- ) {: a:ptr u:n :}
   N @ ?ROOM
   u ?FITS
   a  N @ SLOT  u BYTE-COPY
   u N @ LEN!
   N @ 1+ N ! ;

: COUNT ( -- n ) N @ ;

: PATH$ ( n -- ptr u8 n ) {: ix:n :}
   ix N @ < 0= if
      s" aot-ident: closure index out of range" REFUSE-RC die
   then
   ix SLOT ix LEN@ ;

private

variable RD

\ Stream one closure file into the running digest. A read that fails is refused,
\ never skipped: a digest over the files that happened to open is not a digest of
\ the closure.
: FEED ( n -- ) {: ix:n :}
   ix PATH$ PATH0 0 0 open {: fd:n :}
   fd 0 < if
      s" aot-ident: cannot open " type ix PATH$ type cr
      s" aot-ident: closure file unreadable" REFUSE-RC die
   then
   begin
      fd CHUNK-BUF CHUNK read RD !
      RD @ 0 < if
         fd close
         s" aot-ident: closure file read failed" REFUSE-RC die
      then
      RD @ 0 >
   while
      SHA-CTX CHUNK-BUF RD @ SHA256-FEED
   repeat
   fd close ;

: FILE-DIGEST ( n -- ) {: ix:n :}
   SHA-CTX SHA256-BEGIN
   ix FEED
   SHA-CTX FILE-DIGESTS ix 32 * + SHA256-END ;

\ Framing integers are unsigned little-endian cells, independent of host byte
\ order. Counts and path lengths were bounded when the closure was recorded.
: FEED-U64 ( n -- ) {: value:n :}
   8 0 ?do value i 8 * rshift $FF and FRAME-WORD i + c! loop
   SHA-CTX FRAME-WORD 8 SHA256-FEED ;

public

\ Closure identity: fixed domain, version, file count, then per file its path
\ length, exact recorded path bytes and 32-byte SHA-256 content digest. A reader
\ recomputes it from disk, so a mismatch means a stale artifact, which a
\ recapture cures. The producer identity is not here: src/lib/engine-id.f in a
\ booted capture, SHA256-FILE-IN over the emitted engine in the metabuild.
: CHAIN-DIGEST ( ptr u8 -- ) {: out:ptr :}
   N @ 0= if
      s" aot-ident: chain digest asked for before the closure was latched" REFUSE-RC die
   then
   N @ 0 ?do i FILE-DIGEST loop
   SHA-CTX SHA256-BEGIN
   SHA-CTX s" Habu AOT source closure" SHA256-FEED
   2 FEED-U64
   N @ FEED-U64
   N @ 0 ?do
      SHA-CTX i PATH$ dup FEED-U64 SHA256-FEED
      SHA-CTX FILE-DIGESTS i 32 * + 32 SHA256-FEED
   loop
   SHA-CTX out SHA256-END ;

;package
