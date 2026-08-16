\ aot-ident.f — what a chain capture was taken FROM: the closure it loaded, and
\ the digest over that closure's bytes.
\
\ An AOT artifact crossing between two processes has to answer two questions the
\ bytes themselves cannot: which engine produced it, and which sources the capture
\ was taken from. This file owns the second one, and it owns it as a DERIVED fact
\ rather than a declared one.
\
\ THE CLOSURE IS THE ENGINE'S OWN RECORD, NOT A LIST ANYBODY MAINTAINS.
\ src/core/include.f keeps every required path in REQUIRE-PATHS/REQUIRE-LENS, in
\ load order, and REQUIRE-N is the cursor into it. So a producer that reads
\ REQUIRE-N as its capture window opens and again as it closes has bracketed
\ exactly the files the window compiled — 43 of them for the compiler chain,
\ starting src/compiler/native/migrate.f and ending src/compiler/native/branch.f —
\ with no second copy of that set to go stale. A hand-written closure list would
\ agree with the engine only until the first chain file gained a dependency.
\
\ THE REGISTRY READ IS NOT HERE, AND THAT IS THE POINT. This file holds a list and
\ its digest; who fills the list is the filler's business. The capture reads the
\ require registry (tools/aot-chain-capture.f, which is the only process with a
\ window span to bracket) and the metabuild reads the list an artifact carries
\ (src/habu/aot-file.f). Two fillers, one table, one digest — the shape
\ src/habu/aot-decl.f already uses for the capture buffers. It also keeps this
\ file loadable in BOTH metabuild hosts: tools/bootstrap.sh's stdin source does
\ not compile src/core/include.f, so a REQUIRE-SLOT here would break the recovery
\ route, and the digest the reader re-derives has to be the same code the producer
\ ran or the comparison proves nothing.
\
\ THE DIGEST IS RE-DERIVABLE, WHICH IS THE POINT. It is SHA-256 over the ordered
\ concatenation of those files' bytes, so a reader holding the artifact's file
\ list can recompute it from disk and compare. That turns "this artifact says it
\ came from the chain" into "this artifact came from the chain THAT IS ON DISK NOW",
\ and a mismatch means a stale artifact, which a recapture cures.
\
\ WHAT THIS FILE DOES NOT OWN: the producer identity. Both sides read it, but from
\ different places — a capture running in a booted engine asks lib/engine-id.f for
\ the content key of the binary it is running, and the metabuild recomputes
\ SHA256-FILE over the engine it just emitted. One fact, two independent readings,
\ compared for equality; neither belongs to the closure.
\
\ ENGINE PRIMITIVES ONLY (SHA256*, PATH0, open/read/close, the include registry),
\ because the two processes that need it are a booted bin/hb and the stdin
\ metabuild host, and the host carries no lib/.

package AOT-IDENT

public
\ The table's two bounds are public because a consumer that has to size storage
\ for a whole closure must derive it from here: src/habu/habu2.f assembles the
\ paths into one image table and its buffer is exactly what these two allow, so
\ no second budget can disagree with this one.
256 constant MAX               \ closure files; the chain measures 43
$100 constant PATH-CAP         \ per-path bytes; include.f's own cap is larger, and
                               \ a path that does not fit is refused rather than cut
private

$10000 constant CHUNK          \ file read granularity for the streaming digest
$4A constant REFUSE-RC

create PATHS MAX PATH-CAP * allot
create LENS  MAX cells allot
create CHUNK-BUF CHUNK allot
variable N

: SLOT ( n -- ptr u8 ) PATH-CAP * PATHS + ;
: LEN@ ( n -- n ) cells LENS + @ ;
: LEN! ( n n -- ) {: u:n ix:n :} u ix cells LENS + ! ;

: ?ROOM ( n -- ) {: k:n :}
   k MAX < if exit then
   s" aot-ident: closure exceeds the file table" REFUSE-RC die ;

: ?FITS ( n -- ) {: u:n :}
   u PATH-CAP <= if exit then
   s" aot-ident: closure path longer than the path cap" REFUSE-RC die ;

public

\ Empty the table. A filler starts here, so a second fill cannot leave a tail of
\ the first one's paths behind it.
: RESET ( -- ) 0 N ! ;

\ Append one path, in load order. Both refusals are the table's own invariants -
\ more paths than it holds, or a path longer than a slot - and a path that does
\ not fit is refused rather than cut, because a cut path names a different file
\ and would still hash.
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
      CHUNK-BUF RD @ SHA256-UPDATE
   repeat
   fd close ;

public

\ SHA-256 over the ordered concatenation of the closure's bytes, into a 32-byte
\ buffer. Order is load order, so the digest answers "these files, in the order
\ the chain pulled them in" - a reader with the artifact's list recomputes it
\ from disk and refuses on mismatch.
: CHAIN-DIGEST ( ptr u8 -- ) {: out:ptr :}
   N @ 0= if
      s" aot-ident: chain digest asked for before the closure was latched" REFUSE-RC die
   then
   SHA256-RESET
   N @ 0 ?do i FEED loop
   out SHA256-FINAL ;

;package
