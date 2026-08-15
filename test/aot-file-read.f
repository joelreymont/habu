\ aot-file-read.f — read an AOT artifact through the production reader and say so.
\
\ WHAT IT IS FOR. src/habu/aot-file.f READ is what the metabuild runs on the
\ artifact a capture wrote, and every refusal it can raise has to be provable from
\ the outside. This is the smallest program that runs THAT WORD, out of THAT FILE,
\ with the closure it really has: nothing here re-implements a header check, a
\ digest or a section walk, so a case that passes here passes because the reader
\ refused, not because a copy of the reader refused.
\
\ It is not the capture tool. The capture writes and then reads its own artifact
\ back; this reads one somebody else prepared, which is what lets a suite hand it
\ a corrupted or forged file. The reader is the same word in both.
\
\ THE PRODUCER KEY it will accept is the SHA-256 of the engine it is running -
\ exactly what tools/aot-chain-capture.f stamps into an artifact it writes, so a
\ good artifact from this engine is accepted and one carrying any other key is
\ refused. `-- <path> mismatch` asks for a key that is deliberately not this
\ engine's, which is how the producer refusal is provoked without building a
\ second engine.
\
\ Run:  bin/hb --load test/aot-file-read.f -- <artifact> [mismatch]
\ Prints `aot-file-read: accepted` and exits 0, or the reader's own diagnostic and
\ its exit code.

require src/arch/arm64/asm.f
require src/arch/arm64/icode.f
require src/habu/layout.f
require src/habu/aot-decl.f
require src/habu/aot-ident.f
require src/habu/fdio.f
require src/habu/aot-file.f
require lib/engine-id.f

package AOT-FILE-READ
using AOT-BUF

$4A constant REFUSE-RC

create KEY 32 allot

: ?ARGS ( -- )
   SCRIPT-ARGC 0 > if exit then
   s" aot-file-read: usage: --load test/aot-file-read.f -- <artifact> [mismatch]"
   REFUSE-RC die ;

: MISMATCH? ( -- bool )
   SCRIPT-ARGC 2 < if false exit then
   1 SCRIPT-ARGV$ s" mismatch" CORE-STR= ;

\ The key of the binary this process is running, read the same way the capture
\ reads it. One flipped byte turns it into a key no engine has, which is the
\ producer refusal's input and is not a key some other real engine might collide
\ with by accident.
: KEY! ( -- )
   ENGINE-ID:PATH$ KEY SHA256-FILE 0 <> if
      s" aot-file-read: cannot hash the engine that is running" REFUSE-RC die
   then
   MISMATCH? if KEY c@ 1 xor KEY c! then ;

\ What came back, so a reader that accepted an artifact without filling anything
\ cannot pass by exiting 0. The suite asserts these against the capture's census.
: REPORT ( -- )
   s" aot-file-read: accepted" type cr
   s" recs=" type AOT-REC-N @ .
   s" sites=" type AOT-SITE-N @ .
   s" blob=" type AOT-BLOB-LEN @ .
   s" names=" type AOT-NAMES-LEN @ .
   s" dsites=" type AOT-DSITE-N @ .
   s" csites=" type AOT-CSITE-N @ .
   s" xtoff=" type AOT-WINDOW:XTOFF-N @ .
   s" datasz=" type AOT-DATA-SIZE @ .
   s" closure=" type AOT-IDENT:COUNT .
   s" first=" type 0 AOT-IDENT:PATH$ type cr ;

public

: MAIN ( -- )
   ?ARGS
   KEY!
   KEY 0 SCRIPT-ARGV$ AOT-FILE:READ
   REPORT ;

;package

AOT-FILE-READ:MAIN
