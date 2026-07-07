\ type-family-sha.f — constructor package-name SHA-256 hook (item 8; docs
\ /type-families.md §12, PLAN Package Shape). type-family.f loads before
\ sha256.f in every engine prefix (native boot prefix, stage-build source,
\ Gforth bootstrap), so its Package Shape hash fallback reaches SHA-256 through
\ the TF-SHA16-XT friend cell, installed here once both the registry and the
\ hash exist. TF-SHA16 writes the first 16 lowercase hex digits of SHA-256 over
\ (ptr,n) — the exact prefix SHA256>HEX renders — into the 16-byte output.

: TF-SHA16 ( ptr u8 n ptr u8 -- )
   >r  SHA-DIGEST SHA256
   r> SHA-DST!  SHA-DIGEST SHA-SRC!
   8 0 do  SHA-SRC@ i ZBYTE@  SHA-DST@ i 2 * ZPTR+  BYTE>HEX  loop ;
' TF-SHA16 TF-SHA16-XT !
