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

256 constant MAX               \ closure files; the chain measures 43
$100 constant PATH-CAP         \ per-path bytes; include.f's own cap is larger, and
                               \ a path that does not fit is refused rather than cut
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

\ Latch the closure from the engine's require registry. r0 and r1 are REQUIRE-N as
\ the window opened and closed, so [r0,r1) is what the window loaded and nothing
\ else. An empty range is a capture that compiled no file and is refused here
\ rather than producing an artifact that claims to come from nothing.
: CLOSURE! ( n n -- ) {: r0:n r1:n :}
   r1 r0 <= if
      s" aot-ident: the capture loaded no file" REFUSE-RC die
   then
   r1 r0 - ?ROOM
   0 N !
   r1 r0 ?do
      i REQUIRE-LEN@ ?FITS
      i REQUIRE-SLOT  N @ SLOT  i REQUIRE-LEN@ BYTE-COPY
      i REQUIRE-LEN@ N @ LEN!
      N @ 1+ N !
   loop ;

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
