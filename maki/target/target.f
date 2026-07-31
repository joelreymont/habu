\ maki/target/target.f - immutable target descriptors and nominal identities.
\
\ Semantic facts own identity; labels are presentation only. Equal descriptors
\ intern to one CAD-KIND:target-id even when callers supply another label. The
\ registry is append-only, and every public lookup either returns a validated
\ nominal id or throws. Raw representation conversions remain private.

require lib/prelude.f
require lib/string.f
require lib/fmt.f
require maki/cad-kinds.f

-5247 constant E-TARGET-WIRE     \ ID>WIRE output buffer smaller than the fixed wire width
-5252 constant E-TARGET-CAP
-5253 constant E-TARGET-FACT
-5254 constant E-TARGET-ID
-5255 constant E-TARGET-LABEL
-5256 constant E-TARGET-UNKNOWN
-5249 constant E-TARGET-VER       \ no ptxas ISA .version is known for this target's arch

package TARGET
public

1 constant ISA-PTX

1  constant CAP-PTX
2  constant CAP-FP16
4  constant CAP-BF16
8  constant CAP-TF32
16 constant CAP-MMA
32 constant CAP-ASYNC
64 constant CAP-BARRIER
127 constant CAP-ALL

\ The descriptor is the immutable bundle of semantic facts that owns target
\ identity - the exact six cells DESC-HASH folds, DESC-FACTS$ serializes and
\ DESC-EQ? compares. Declared through the unified ENUM front end in full mode
\ (the arity token after the name selects it), so every payload cell carries a
\ named FIELD instead of a positional type token. The six names are the ones this
\ file already uses for those cells everywhere else - the typed locals of
\ DESC-HASH / DESC-FACTS$ / DESC-CK, the public ISA@ / ARCH@ / WARP@ / THREADS@ /
\ SHARED@ / CAPS@ readers, and the canonical isa=|arch=|warp=|threads=|shared=|caps=
\ serialization keys - so the declaration and its consumers read as one vocabulary
\ and a reordered cell is a renamed slot rather than a silent swap of two
\ indistinguishable n's. The generated TARGET-DESCRIPTOR:VALUE constructor, its
\ checked effect, and every `construct descriptor value` / `MATCH descriptor` site
\ are unchanged: the spelling and the payload binding order derive from the
\ package, the family tail and the declaration order, none of which the mode
\ touches.
\
\ The six cells, in declaration order (DESC-CK states the admissible range of
\ each): isa, the ISA family, where ISA-PTX is the only admissible value; arch,
\ the SM architecture number (87, 121, ...); warp, the warp width in threads, a
\ power of two no wider than 64; threads, the maximum threads per block, a
\ multiple of warp; shared, the shared-memory budget in bytes per block; caps,
\ the capability bitset, a subset of CAP-ALL that must include CAP-PTX. No
\ definer accepts a comment inside a declaration body, so the per-cell notes live
\ here rather than on the FIELD lines.
ENUM descriptor<>
   VARIANT value
      FIELD isa n
      FIELD arch n
      FIELD warp n
      FIELD threads n
      FIELD shared n
      FIELD caps n
   ;VARIANT
;ENUM

\ WIRE>ID decode result (MODEL-CAD-V2-PLAN.md § 23.9 "Foreign identity
\ constructors and wire codecs", the art-result custom-sum idiom): `ok` carries the
\ refined nominal id in its `id` field; the reject arms are the fixed-width
\ byte-decode refusals the contract names (wrong width, unresolved/out-of-range
\ value). A bespoke per-package sum, not result<a,b>, so a total ok construction
\ leaves no free error variable. Declared through the unified ENUM front end in
\ full mode, so the one payload of `ok` is a named FIELD; the generated
\ TARGET-ID--RESULT:OK / :WRONG-WIDTH / :UNKNOWN constructors and every MATCH site
\ are unchanged, for the same reason the descriptor's are.
ENUM id-result<a>
   VARIANT ok FIELD id a ;VARIANT
   VARIANT wrong-width ;VARIANT
   VARIANT unknown ;VARIANT
;ENUM

;package

package TARGET
private

16 constant TGT-CAP
32 constant TGT-LABEL-MAX
TGT-CAP TGT-LABEL-MAX * constant TGT-LABEL-CAP
32 constant CK-BYTES                        \ SHA-256 content-key width (cross-process wire form)

TGT-CAP LAYOUT-BUFFER TGT-DESCS descriptor
create TGT-LABELS TGT-LABEL-CAP allot
create TGT-LO TGT-CAP cells allot
create TGT-LL TGT-CAP cells allot
create TGT-CK TGT-CAP CK-BYTES * allot       \ per-id SHA-256 content key over the canonical facts
variable TGT-LABEL-U
variable TGT-N
TYPED-VARIABLE TGT-SM87 CAD-KIND:target-id  \ interned sm87 target id ( -- ptr CAD-KIND:target-id )
TYPED-VARIABLE TGT-SM121A CAD-KIND:target-id \ interned sm_121a (GB10) target id

TRUSTED: RAW>TARGET-ID ( n -- CAD-KIND:target-id ) ;
TRUSTED: TARGET-ID>RAW ( CAD-KIND:target-id -- n ) ;

$cbf29ce484222325 constant TGT-HASH-BASIS
$100000001b3 constant TGT-HASH-PRIME

\ Declared-ahead identity surface (dead-code audit 2026-07-13, keep): the
\ DESC-HASH chain plus public DIGEST@/EQUAL?/FACTS$/LABEL$/RESOLVE/COUNT serve
\ Model-CAD V2 section 9.1 content-addressed object identity - the artifact DB
\ keys objects by semantic digest (epic habu-epic-model-cad-70b629a9), matching
\ the idle CAD-KIND identity kinds precedent. Focused suites are today's only
\ callers; do not re-flag these as dead.

: DESC-UN ( descriptor -- n n n n n n )
   MATCH descriptor
      value OF ENDOF
   ;MATCH ;

: HASH-MIX ( n n -- n )
   xor TGT-HASH-PRIME * ;

: DESC-HASH ( descriptor -- n )
   DESC-UN {: isa:n arch:n warp:n threads:n shared:n caps:n :}
   TGT-HASH-BASIS
   isa HASH-MIX arch HASH-MIX warp HASH-MIX
   threads HASH-MIX shared HASH-MIX caps HASH-MIX ;

\ Canonical, label-independent descriptor serialization: the exact facts that own
\ identity (isa/arch/warp/threads/shared/caps), rendered deterministically. This is the
\ content the cross-process content key hashes (SHA-256), and the public FACTS$ view.
: DESC-FACTS$ ( descriptor -- ptr u8 n )
   DESC-UN
   {: isa:n arch:n warp:n threads:n shared:n caps:n :}
   SB-RESET
   s" isa=" SB-APPEND isa FMT:SB-INT
   s" |arch=" SB-APPEND arch FMT:SB-INT
   s" |warp=" SB-APPEND warp FMT:SB-INT
   s" |threads=" SB-APPEND threads FMT:SB-INT
   s" |shared=" SB-APPEND shared FMT:SB-INT
   s" |caps=" SB-APPEND caps FMT:SB-INT
   SB$ ;

: CK@ ( n -- ptr u8 )   CK-BYTES * TGT-CK + ;   \ per-id content-key slot base

: CK-EQ? ( ptr u8 ptr u8 -- bool ) {: pa:ptr pb:ptr :}   \ fixed 32-byte content-key compare
   0 begin dup CK-BYTES < while
      dup {: k:n :}
      pa k + c@  pb k + c@  <> if drop false exit then
      1+
   repeat drop true ;

: CK-FIND ( ptr u8 -- n ) {: p:ptr :}   \ raw id whose content key equals p's 32 bytes, or -1
   TGT-N @ 0 ?do
      i CK@ p CK-EQ? if i unloop exit then
   loop -1 ;

: DESC-EQ? ( descriptor descriptor -- bool )
   \ typed-local-lint: allow-bare-local - closed descriptor bundles preserve roles.
   {: a b :}
   a DESC-UN {: ai:n aa:n aw:n at:n as:n ac:n :}
   b DESC-UN {: bi:n ba:n bw:n bt:n bs:n bc:n :}
   ai bi = aa ba = and aw bw = and
   at bt = and as bs = and ac bc = and ;

: POW2? ( n -- bool ) {: v:n :}
   v 0 > v v 1- and 0= and ;

: LABEL-CK ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0= u TGT-LABEL-MAX > or if E-TARGET-LABEL throw then
   u 0 ?do
      a i + c@ {: c:n :}
      c 32 <= c 127 >= or if E-TARGET-LABEL throw then
   loop ;

: DESC-CK ( descriptor -- )
   DESC-UN
   {: isa:n arch:n warp:n threads:n shared:n caps:n :}
   isa ISA-PTX <> if E-TARGET-FACT throw then
   arch 0 <= arch 999 > or if E-TARGET-FACT throw then
   warp POW2? 0= warp 64 > or if E-TARGET-FACT throw then
   threads warp < threads 2048 > or if E-TARGET-FACT throw then
   threads warp mod 0<> if E-TARGET-FACT throw then
   shared 0 <= if E-TARGET-FACT throw then
   caps CAP-ALL invert and 0<> if E-TARGET-FACT throw then
   caps CAP-PTX and 0= if E-TARGET-FACT throw then ;

: ID-CK ( CAD-KIND:target-id -- n )
   TARGET-ID>RAW dup 0 < over TGT-N @ >= or if E-TARGET-ID throw then ;

: DESC-RAW@ ( n -- descriptor )
   TGT-DESCS @ ;

: DESC-RAW! ( descriptor n -- )
   TGT-DESCS ! ;

: DESC-FIND ( descriptor -- n )
   \ typed-local-lint: allow-bare-local - closed descriptor bundle preserves its role.
   {: d :}
   TGT-N @ 0 ?do
      d i DESC-RAW@ DESC-EQ? if i unloop exit then
   loop
   -1 ;

: LABEL-PUT ( ptr u8 n n -- ) {: a:ptr u:n raw:n :}
   TGT-LABEL-U @ u + TGT-LABEL-CAP > if E-TARGET-CAP throw then
   TGT-LABEL-U @ {: off:n :}
   a TGT-LABELS off + u BYTE-COPY
   off raw cells TGT-LO + !
   u raw cells TGT-LL + !
   off u + TGT-LABEL-U ! ;

: SM87! ( CAD-KIND:target-id -- )
   TGT-SM87 ! ;

: SM121A! ( CAD-KIND:target-id -- )
   TGT-SM121A ! ;

\ ---- § 23.9 foreign-id wire codecs --------------------------------------------
\ Two audited public codecs share the private RAW>TARGET-ID refinement:
\   - ID>WIRE / WIRE>ID: the PROCESS-LOCAL registry raw as a fixed-width 8-byte
\     little-endian scalar. Admissible only for intra-process wire paths; NEVER for a
\     digest-covered, cross-process, or durable identity (a raw index means a different
\     descriptor in another process).
\   - KEY>WIRE / WIRE>KEY: the CROSS-PROCESS content key - SHA-256 over the canonical
\     label-independent facts (DESC-FACTS$), fixed 32-byte little-endian (§ 23.9
\     origin-class table, content-addressed registry intern row; "TARGET:DIGEST@ already
\     yields the content key" - DIGEST@ is the 64-bit fold view, KEY>WIRE is its
\     collision-resistant 256-bit wire form). Equal descriptors digest identically in
\     every process, so this form survives process death: WIRE>KEY resolves the 32 bytes
\     against the local registry by CONTENT, never by registration order. The per-id
\     content key is interned at REGISTER (TGT-CK).
8 constant WIRE-BYTES

: LE-PUT ( n ptr u8 n -- ) {: v:n a:ptr w:n :}
   0 begin dup w < while
      dup {: k:n :}
      v k 8 * rshift $FF and  a k + c!
      1+
   repeat drop ;

: LE-GET ( ptr u8 n -- n ) {: a:ptr w:n :}
   0  0 begin dup w < while
      dup {: k:n :}
      a k + c@ k 8 * lshift  rot or swap
      1+
   repeat drop ;

: R-OK ( a -- id-result<a> )          TARGET-ID--RESULT:OK ;
: R-WRONG-WIDTH ( -- id-result<a> )   TARGET-ID--RESULT:WRONG-WIDTH ;
: R-UNKNOWN ( -- id-result<a> )       TARGET-ID--RESULT:UNKNOWN ;

public

: DESCRIPTOR ( n n n n n n -- descriptor )
   construct descriptor value dup DESC-CK ;

: REGISTER ( ptr u8 n descriptor -- CAD-KIND:target-id )
   \ typed-local-lint: allow-bare-local - d is a closed descriptor bundle.
   {: label:ptr labelu:n d :}
   label labelu LABEL-CK
   d DESC-CK
   d DESC-FIND {: found:n :}
   found 0 >= if found RAW>TARGET-ID exit then
   TGT-N @ TGT-CAP >= if E-TARGET-CAP throw then
   TGT-LABEL-U @ labelu + TGT-LABEL-CAP > if E-TARGET-CAP throw then
   TGT-N @ {: raw:n :}
   d raw DESC-RAW!
   label labelu raw LABEL-PUT
   d DESC-FACTS$  raw CK@  SHA256              \ intern the cross-process content key
   raw 1+ TGT-N !
   raw RAW>TARGET-ID ;

: RESOLVE ( descriptor -- CAD-KIND:target-id )
   dup DESC-CK
   DESC-FIND dup 0 < if drop E-TARGET-UNKNOWN throw then
   RAW>TARGET-ID ;

: VALIDATE ( CAD-KIND:target-id -- CAD-KIND:target-id )
   dup ID-CK drop ;

\ ID>WIRE is total for a valid nominal id: it writes the id's fixed-width canonical
\ bytes into the caller buffer and returns the byte count (E-TARGET-WIRE if the cap
\ cannot hold the fixed width). WIRE>ID is the audited boundary: it reads the fixed
\ width, validates the raw through the append-only registry FAIL-CLOSED, and refines
\ to the nominal id only on success, returning a typed id-result (§ 23.9).
: ID>WIRE ( CAD-KIND:target-id ptr u8 n -- n )
   {: id:CAD-KIND:target-id out:ptr cap:n :}
   cap WIRE-BYTES < if E-TARGET-WIRE throw then
   id ID-CK  out  WIRE-BYTES  LE-PUT
   WIRE-BYTES ;

: WIRE>ID ( ptr u8 n -- id-result<CAD-KIND:target-id> )
   {: a:ptr u:n :}
   u WIRE-BYTES <> if R-WRONG-WIDTH exit then
   a WIRE-BYTES LE-GET {: raw:n :}
   raw 0 < raw TGT-N @ >= or if R-UNKNOWN exit then
   raw RAW>TARGET-ID R-OK ;

\ KEY>WIRE writes the id's cross-process content key (SHA-256 over the canonical facts,
\ 32 fixed bytes); total for a valid id (E-TARGET-WIRE if the cap cannot hold CK-BYTES).
\ WIRE>KEY is the audited cross-process boundary: it reads the 32-byte content key and
\ resolves it against the local registry BY CONTENT (never by registration order),
\ refining to the nominal id only on a content match.
: KEY>WIRE ( CAD-KIND:target-id ptr u8 n -- n )
   {: id:CAD-KIND:target-id out:ptr cap:n :}
   cap CK-BYTES < if E-TARGET-WIRE throw then
   id ID-CK CK@  out  CK-BYTES  BYTE-COPY
   CK-BYTES ;

: WIRE>KEY ( ptr u8 n -- id-result<CAD-KIND:target-id> )
   {: a:ptr u:n :}
   u CK-BYTES <> if R-WRONG-WIDTH exit then
   a CK-FIND {: raw:n :}
   raw 0 < if R-UNKNOWN exit then
   raw RAW>TARGET-ID R-OK ;

: DESCRIPTOR@ ( CAD-KIND:target-id -- descriptor )
   ID-CK DESC-RAW@ ;

: LABEL$ ( CAD-KIND:target-id -- ptr u8 n )
   ID-CK {: raw:n :}
   TGT-LABELS raw cells TGT-LO + @ +
   raw cells TGT-LL + @ ;

: DIGEST@ ( CAD-KIND:target-id -- n )
   DESCRIPTOR@ DESC-HASH ;

: EQUAL? ( CAD-KIND:target-id CAD-KIND:target-id -- bool )
   {: a:CAD-KIND:target-id b:CAD-KIND:target-id :}
   a DESCRIPTOR@ b DESCRIPTOR@ DESC-EQ? ;

: ISA@ ( CAD-KIND:target-id -- n )
   DESCRIPTOR@ DESC-UN drop drop drop drop drop ;

: ARCH@ ( CAD-KIND:target-id -- n )
   DESCRIPTOR@ DESC-UN drop drop drop drop nip ;

: WARP@ ( CAD-KIND:target-id -- n )
   DESCRIPTOR@ DESC-UN drop drop drop nip nip ;

: THREADS@ ( CAD-KIND:target-id -- n )
   DESCRIPTOR@ DESC-UN drop drop nip nip nip ;

: SHARED@ ( CAD-KIND:target-id -- n )
   DESCRIPTOR@ DESC-UN drop nip nip nip nip ;

: CAPS@ ( CAD-KIND:target-id -- n )
   DESCRIPTOR@ DESC-UN nip nip nip nip nip ;

\ Raw-descriptor field views (the DESCRIPTOR@-free duals of SHARED@/CAPS@): a
\ caller holding an un-interned descriptor value (e.g. TARGET:DESCRIPTOR built
\ for a legality probe) reads its shared-memory budget / capability bitset
\ without touching the append-only, capped registry.
: DESC-SHARED@ ( descriptor -- n )
   DESC-UN drop nip nip nip nip ;

: DESC-CAPS@ ( descriptor -- n )
   DESC-UN nip nip nip nip nip ;

: FACTS$ ( CAD-KIND:target-id -- ptr u8 n )
   DESCRIPTOR@ DESC-FACTS$ ;

: COUNT ( -- n )
   TGT-N @ ;

: SM87 ( -- CAD-KIND:target-id )
   TGT-SM87 @ VALIDATE ;

: SM121A ( -- CAD-KIND:target-id )
   TGT-SM121A @ VALIDATE ;

\ The PTX ISA .version the target's ptxas requires: an sm_121a .target directive
\ is rejected below PTX ISA 8.8 (measured with CUDA 13 ptxas on the GB10), while
\ sm_87 keeps the 8.3 it has always emitted. Derived from the descriptor's arch,
\ so the emitted .version and the assembler arch trace to one target fact. Any
\ arch without a known minimum ISA is a fail-closed throw, never a default.
: ARCH-VER$ ( CAD-KIND:target-id -- ptr u8 n )
   ARCH@ {: arch:n :}
   arch 87 = if s" 8.3" exit then
   arch 121 = if s" 8.8" exit then
   E-TARGET-VER throw ;

private

: SM87-DESC ( -- descriptor )
   ISA-PTX 87 32 1024 49152
   CAP-PTX CAP-FP16 or CAP-BF16 or CAP-TF32 or CAP-MMA or CAP-ASYNC or CAP-BARRIER or
   DESCRIPTOR ;

: SM121A-DESC ( -- descriptor )
   ISA-PTX 121 32 1024 49152
   CAP-ALL
   DESCRIPTOR ;

: INIT ( -- )
   s" sm_87" SM87-DESC REGISTER SM87!
   s" sm_121a" SM121A-DESC REGISTER SM121A! ;

INIT

;package
