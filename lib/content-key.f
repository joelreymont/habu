\ content-key.f - manifest-hashed content cache keys.
\
\ The module lives in `package CONTENT-KEY`. A key is built through one FOLD, and
\ the fold is a value the caller carries: CONTENT-KEY:OPEN returns a
\ CONTENT-KEY:fold, CONTENT-KEY:TEXT+ / FILE+ / DIGEST+ take that fold and give
\ it back, and CONTENT-KEY:FINAL / FINAL-HEX close it into a digest.
\ CONTENT-KEY:DISCARD closes one whose key is not going to be taken. Because
\ every operation names its own fold, two keys derived at overlapping times
\ cannot mix bytes - see the fold-handle section below for what that replaced.
\ FILE+ digests the file's current bytes on every call. The read-only fold
\ census FOLDS / FOLD-FILL serves the capacity-throw reporter. Every other
\ helper and all buffers are package-private.
\
\ Requires the SHA-256 words; native bin/hb already carries src/core/sha256.f.

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/type/deftype.f

package CONTENT-KEY

$40000 constant CK-CAP
$54 constant CK-TEXT-TAG
$46 constant CK-FILE-TAG
$44 constant CK-DIGEST-TAG
$FFFF constant CK-FRAG-MAX       \ the two length bytes a text or file fragment carries

4 constant CK-FOLD-N

\ The fold pool's own conditions get their own codes (the package-scoped shape
\ lib/type/deftype.f uses for package VNOM). Throwing E-STR-CAPACITY for "no
\ free fold" or E-STR-BOUNDS for "this handle no longer owns its slot" would
\ send a reader to the preimage buffer, which is the one thing that is fine.
-6920 constant E-CK-FOLDS    \ every fold slot is in use; an earlier fold was never closed
-6921 constant E-CK-STALE    \ the handle does not own the slot it names (closed, or never opened)

create CK-BUF CK-CAP allot
CK-FOLD-N TYPED-BUFFER CK-FOLD-A ptr u8   \ one preimage buffer address per fold slot
create CK-FOLD-U CK-FOLD-N cells allot
create CK-FOLD-G CK-FOLD-N cells allot
create CK-DG 40 allot
create CK-FILE-DG 40 allot
create CK-SHA-CTX SHA256-CTX-BYTES allot        \ this package's digest context
create CK-FSHA-CTX SHA256-FILE-CTX-BYTES allot  \ ... and its file-digest context

variable CK-GEN

\ ---- fold handles -----------------------------------------------------------
\ A key is built by folding fields into a preimage buffer. There used to be ONE
\ such buffer, so two folds that overlapped in time - a key derived while
\ another key's fold was open, which is what a nested cache-key derivation is -
\ mixed their bytes into a single wrong key, silently and identically for both
\ (dot habu-content-key-folds-9d2888c2). Every fold now owns a slot, and every
\ operation names its fold, so overlapping folds cannot reach each other's
\ bytes: it is the handle, not an ordering convention, that keeps them apart.
\
\ The handle is a nominal cell (`CONTENT-KEY:fold`), so a plain integer cannot
\ stand in for one; the converters that mint it are undefined at the end of the
\ package, so no caller outside can forge one either. It carries the OWNING
\ GENERATION as well as the slot, and every operation checks it, so a handle
\ kept past FINAL names a slot it no longer owns and throws instead of writing
\ into whatever fold holds that slot now.
\
\ Slot 0 folds into the static CK-BUF, so the ordinary single-fold run costs no
\ allocation at all; the slots an overlapping fold needs take their buffers from
\ the checked MEM: surface on first use.

public

DEFTYPE FOLD

private

\ CK-CAP is a positive library constant: MEM:BYTES-ALLOC-LEN narrows it to the
\ validated alloc role before MEM:ALLOC-BYTES, which throws E-MEM-SIZE on any
\ refusal (unreachable for the constant).
: CK-SLOT-BUF ( n -- ptr u8 ) {: s:n :}
   s 0= if CK-BUF exit then
   s CK-FOLD-A @ 0= if
      CK-CAP MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop s CK-FOLD-A !
   then
   s CK-FOLD-A @ ;

: CK-SLOT-U@ ( n -- n )
   cells CK-FOLD-U + @ ;

: CK-SLOT-U! ( n n -- ) {: u:n s:n :}
   u s cells CK-FOLD-U + ! ;

: CK-SLOT-G@ ( n -- n )
   cells CK-FOLD-G + @ ;

: CK-SLOT-G! ( n n -- ) {: g:n s:n :}
   g s cells CK-FOLD-G + ! ;

: CK-FREE-SLOT ( -- n )
   0 begin dup CK-FOLD-N < while
      dup CK-SLOT-G@ 0= if exit then
      1+
   repeat drop E-CK-FOLDS throw ;

\ The handle packs the owning generation above the slot, so no two handles are
\ ever equal and a released one can never be mistaken for its successor.
: CK-SLOT-OF ( fold -- n )
   FOLD>N CK-FOLD-N mod ;

: CK-GEN-OF ( fold -- n )
   FOLD>N CK-FOLD-N / ;

: CK-LIVE ( fold -- n ) {: f:fold :}
   f CK-SLOT-OF {: s:n :}
   s CK-SLOT-G@ f CK-GEN-OF <> if E-CK-STALE throw then
   s ;

: CK-OPEN ( -- fold )
   CK-FREE-SLOT {: s:n :}
   CK-GEN @ 1+ dup CK-GEN ! {: g:n :}
   0 s CK-SLOT-U!
   g s CK-SLOT-G!
   g CK-FOLD-N * s + >FOLD ;

: CK-CLOSE ( fold -- )
   CK-LIVE {: s:n :}
   0 s CK-SLOT-U!
   0 s CK-SLOT-G! ;

public

: OPEN ( -- fold )
   CK-OPEN ;

\ Release a fold whose key is not going to be taken. An early return out of a
\ key derivation would otherwise strand the slot until the process ended, and
\ the pool would eventually refuse to open a fold at all; DISCARD is the
\ abandon path, and forgetting it fails loudly (E-CK-FOLDS from the next OPEN)
\ rather than corrupting anybody's bytes.
: DISCARD ( fold -- )
   CK-CLOSE ;

private

: CK-CAP-CHECK ( n n -- ) {: s:n n:n :}
   n 0 < if E-STR-BOUNDS throw then
   s CK-SLOT-U@ n + CK-CAP > if E-STR-CAPACITY throw then ;

: CK-U8+ ( n n -- ) {: s:n c:n :}
   s 1 CK-CAP-CHECK
   c 0 < if E-STR-BOUNDS throw then
   c STR-BYTE-MAX > if E-STR-BOUNDS throw then
   c s CK-SLOT-BUF s CK-SLOT-U@ + c!
   s CK-SLOT-U@ 1+ s CK-SLOT-U! ;

: CK-BYTES+ ( n ptr u8 n -- ) {: s:n a:ptr u:n :}
   s u CK-CAP-CHECK
   a s CK-SLOT-BUF s CK-SLOT-U@ + u BYTE-COPY
   s CK-SLOT-U@ u + s CK-SLOT-U! ;

\ A fragment is its tag, its length in two little-endian bytes and its bytes:
\ FILE+ folds the file's name, and a path of PATH-CAP bytes has to fold whole.
: CK-FRAG+ ( n n ptr u8 n -- ) {: s:n tag:n a:ptr u:n :}
   u 0 < if E-STR-BOUNDS throw then
   u CK-FRAG-MAX > if E-STR-BOUNDS throw then
   s tag CK-U8+
   s u STR-BYTE-MAX and CK-U8+
   s u 8 rshift CK-U8+
   s a u CK-BYTES+ ;

public

: TEXT+ ( fold ptr u8 n -- fold ) {: f:fold a:ptr u:n :}
   f CK-LIVE CK-TEXT-TAG a u CK-FRAG+
   f ;

: DIGEST+ ( fold ptr u8 -- fold ) {: f:fold dg:ptr :}
   f CK-LIVE {: s:n :}
   s CK-DIGEST-TAG CK-U8+
   s 32 CK-U8+
   s dg 32 CK-BYTES+
   f ;

private

: CK-FILE-DIGEST! ( ptr u8 n -- ) {: a:ptr u:n :}
   CK-FSHA-CTX a u CK-FILE-DG SHA256-FILE-IN dup 0 <> if throw then drop ;

public

\ Read the physical path, but fold the supplied logical name. A build can name
\ a source relative to its root without changing where its bytes are read.
: FILE-NAMED+ ( fold ptr u8 n ptr u8 n -- fold )
   {: f:fold a:ptr u:n name:ptr size:n :}
   f CK-LIVE CK-FILE-TAG name size CK-FRAG+
   a u CK-FILE-DIGEST!
   f CK-FILE-DG DIGEST+ ;

: FILE+ ( fold ptr u8 n -- fold ) {: f:fold a:ptr u:n :}
   f a u a u FILE-NAMED+ ;

\ Finalizing a key closes its fold; the slot is released for the next one.
: FINAL ( fold ptr u8 -- ) {: f:fold dst:ptr :}
   f CK-LIVE {: s:n :}
   CK-SHA-CTX s CK-SLOT-BUF s CK-SLOT-U@ dst SHA256-IN
   f CK-CLOSE ;

: FINAL-HEX ( fold ptr u8 -- ) {: f:fold hex:ptr :}
   f CK-DG FINAL
   CK-DG hex SHA256>HEX ;

\ Read-only introspection. The fold census (FOLDS/FOLD-FILL) lets the
\ capacity-throw reporter name the fold that overflowed without holding a
\ handle - it runs from a throw handler, where there is no fold to pass.
: BUF-CAP ( -- n )   CK-CAP ;

: FOLDS ( -- n )   CK-FOLD-N ;

: FOLD-FILL ( n -- n ) {: s:n :}
   s 0 < if E-STR-BOUNDS throw then
   s CK-FOLD-N >= if E-STR-BOUNDS throw then
   s CK-SLOT-G@ 0= if 0 exit then
   s CK-SLOT-U@ ;

\ Erase the mint. The nominal stays nameable outside as CONTENT-KEY:fold, but
\ the only words that cross between it and a plain cell are gone from the public
\ wordlist, so a handle can be obtained ONLY from OPEN. Compiled callers above
\ keep their direct xts.
undefine >FOLD
undefine FOLD>N

;package
