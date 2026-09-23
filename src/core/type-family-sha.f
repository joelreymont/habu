\ type-family-sha.f — constructor package-name SHA-256 hook (item 8; docs
\ /type-families.md §12, PLAN Package Shape). type-family.f loads before
\ sha256.f in every engine prefix (native boot prefix, stage-build source,
\ Gforth bootstrap), so its Package Shape hash fallback reaches SHA-256 through
\ the TF-SHA16-XT friend hook, installed here once both the registry and the
\ hash exist. TF-SHA16 writes the first 16 lowercase hex digits of SHA-256 over
\ (ptr,n) — the exact prefix SHA256>HEX renders — into the 16-byte output.
\
\ THE TWO STATICS BELOW ARE THE CHECKER'S, and they are the whole of the digest
\ storage the engine keeps: src/core/sha256.f holds none. They are single-task
\ by the checker's nature — one image compiles one source in one task, and this
\ hook answers that compile's registry — so the hash runs through one context
\ and reads its digest back out of one buffer. Every other caller of SHA-256
\ declares a context of its own; two at once need two.
create TF-SHA-CTX SHA256-CTX-BYTES allot
create SHA-DIGEST $20 allot

: TF-SHA16 ( ptr u8 n ptr u8 -- ) {: a u:n out :}
   TF-SHA-CTX a u SHA-DIGEST SHA256-IN
   8 0 do  SHA-DIGEST i ZBYTE@  out i 2 * ZPTR+  BYTE>HEX  loop ;

\ Capture copies these cells into the engine it writes, so the context goes back
\ to the state SHA256-BEGIN leaves and the building host's last package hash does
\ not travel in the image. This is what SHA256-SNAPSHOT-PREPARE did for the one
\ static context sha256.f no longer has.
: TF-SHA-SNAPSHOT-PREPARE ( -- )
   TF-SHA-CTX SHA256-BEGIN ;

: TF-SHA16-INSTALL ( -- )
   [: TF-SHA16 ;] is TF-SHA16-XT ;
TF-SHA16-INSTALL
