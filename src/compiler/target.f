\ target.f - the immutable compiler target contract.
\
\ docs/compiler-ir-design.md section 5.4. No compiler pass may read target facts
\ out of scattered globals; it reads them out of one immutable value that says
\ what machine the compilation is for. This file owns the first stage of that
\ value: architecture, ABI, byte order, pointer width and feature set, plus the
\ canonical preimage and digest that give the combination an identity later
\ stages can name.
\
\ WHAT THE VALIDATOR OWNS. A contract is rejected when its facts cannot all be
\ true of one machine - a PTX kernel ABI on an AArch64 core, a big-endian
\ Darwin ABI, a matrix-multiply feature on a core that has no matrix unit. It is
\ NOT rejected merely because this compiler cannot yet emit code for it. Those
\ are different questions with different owners: coherence is decided here, once,
\ at construction; backend capability is decided later by the stage that has to
\ produce instructions, against the contract (design section 8, "target-specific
\ operations are legal for the target contract"). Collapsing the two would make
\ the contract undeclarable for any machine we are about to add support for, and
\ - worse - would let a newly supported machine reuse the digest of an existing
\ one, because the field that distinguishes them was never allowed to vary.
\
\ Every semantic field carries its own closed family, so a swapped argument at
\ construction is a checker reject rather than a silently different target. The
\ feature set is nominal too (a one-field structure over a bit mask), so a bare
\ integer cannot be passed where a feature set is wanted; callers build one from
\ the named feature words and CTARGET:WITH.
\
\ IDENTITY. CTARGET:DIGEST is SHA-256 over the canonical preimage: the
\ domain-separation tag, the schema version, and one eight-byte slot for each of
\ the five semantic fields, in declaration order. The per-field codes below are
\ the stable wire codes; they are what fixes the digest, so a variant may be
\ added to a family but an existing variant's code may never be renumbered
\ without bumping SCHEMA. Because the encoding is a fixed-arity product of
\ per-field injective codes, two contracts have the same preimage exactly when
\ they have the same five fields - which is what makes the digest load-bearing
\ rather than decorative.
\
\ FORGERY. `contract` is a public family, so its generated MAKE can assemble any
\ five field values without passing through CTARGET:CONTRACT. Every word here
\ whose result carries identity - ENCODE, DIGEST, SAME? - therefore revalidates
\ its input instead of trusting that it came from the checked constructor. The
\ plain field readers do not: they only project a value the caller already holds.

require lib/prelude.f
require lib/errors.f
require src/compiler/digest.f

package CTARGET
public

\ The instruction-set architectures a compilation can target.
ENUM arch DERIVE eq
   aarch64
   ptx
   a32
   thumb2
   c66x
   x86-64
;ENUM

\ The calling and object convention. AAPCS32 base PCS covers both ARM and
\ Thumb code; a floating-point register-argument PCS would be a distinct ABI.
ENUM abi DERIVE eq
   aapcs64-darwin
   aapcs64-linux
   ptx-kernel
   aapcs32
   c6000-eabi
   sysv-amd64
;ENUM

\ Byte order of stored multi-byte values.
ENUM endian DERIVE eq
   little
   big
;ENUM

\ Width of an address. A closed family rather than an integer, so there is no
\ out-of-domain width to validate and no magic number at a call site.
ENUM ptr-width DERIVE eq
   bits32
   bits64
;ENUM

\ The feature set: which optional capabilities the target is known to have. A
\ one-field nominal record over a bit mask, so it cannot be confused with any
\ other integer, and so the vocabulary check has a single owner (FEATURE-SET).
STRUCTURE features 0 DERIVE eq
   FIELD bits n
;STRUCTURE

\ The contract itself. Fields in declaration order, deepest stack field first:
\ architecture, ABI, byte order, pointer width, feature set. It does not DERIVE
\ eq - a structure field is not a derivable role - so SAME? below is the
\ hand-written field-by-field identity, and it is the equality the digest is
\ proved to agree with.
STRUCTURE contract 0
   FIELD arch arch
   FIELD abi abi
   FIELD endian endian
   FIELD ptr ptr-width
   FIELD features features
;STRUCTURE

private

\ ---- feature vocabulary ------------------------------------------------------
\ One bit per capability. The bit values are part of the canonical preimage, so
\ they are stable: a new capability takes the next free bit and never reuses one.
$001 constant BIT-BASE     \ the architecture's baseline instruction set
$002 constant BIT-FP       \ hardware floating point including fused multiply-add
$004 constant BIT-SIMD     \ packed vector arithmetic
$008 constant BIT-FP16     \ half-precision arithmetic
$010 constant BIT-BF16     \ bfloat16 arithmetic
$020 constant BIT-TF32     \ tensorfloat32 arithmetic
$040 constant BIT-ATOMIC   \ hardware atomic read-modify-write
$080 constant BIT-MMA      \ matrix-multiply-accumulate unit
$100 constant BIT-ASYNC    \ asynchronous global-to-shared copy

BIT-BASE BIT-FP or BIT-SIMD or BIT-FP16 or BIT-BF16 or BIT-TF32 or
BIT-ATOMIC or BIT-MMA or BIT-ASYNC or constant BIT-ALL

\ What each architecture can have. A feature outside its architecture's mask is
\ a description of a machine that does not exist, not an unsupported target.
BIT-BASE BIT-FP or BIT-SIMD or BIT-FP16 or BIT-BF16 or BIT-ATOMIC or
constant MASK-AARCH64

BIT-BASE BIT-FP or BIT-FP16 or BIT-BF16 or BIT-TF32 or BIT-ATOMIC or
BIT-MMA or BIT-ASYNC or constant MASK-PTX

\ A32 and Thumb-2 describe instruction states, not one core's optional units.
\ A concrete core declares only the features it actually implements.
BIT-BASE BIT-FP or BIT-SIMD or BIT-FP16 or BIT-BF16 or BIT-ATOMIC or
constant MASK-ARM32

\ C66x has packed arithmetic. F-FP includes fused multiply-add, so its
\ non-fused floating-point instructions do not satisfy that feature.
BIT-BASE BIT-SIMD or constant MASK-C66X

\ x86-64 baseline carries SSE2, and cores implement FMA, the half and bfloat
\ formats and the full atomic set. The matrix unit is deliberately absent: AMX
\ is a machine this compiler has no way to describe the operands of, so a
\ contract claiming it would name a target no stage could answer for.
BIT-BASE BIT-FP or BIT-SIMD or BIT-FP16 or BIT-BF16 or BIT-ATOMIC or
constant MASK-X86-64

: MK ( n -- CTARGET:features )
   CTARGET-FEATURES:MAKE ;

: BITS ( CTARGET:features -- n )
   CTARGET-FEATURES:UNMAKE ;

\ ---- stable wire codes -------------------------------------------------------
\ One injective code per family. These fix the digest; see the header.
: ARCH-CODE ( CTARGET:arch -- n )
   MATCH arch
      aarch64 OF 0 ENDOF
      ptx     OF 1 ENDOF
      a32     OF 2 ENDOF
      thumb2  OF 3 ENDOF
      c66x    OF 4 ENDOF
      x86-64  OF 5 ENDOF
   ;MATCH ;

: ABI-CODE ( CTARGET:abi -- n )
   MATCH abi
      aapcs64-darwin OF 0 ENDOF
      aapcs64-linux  OF 1 ENDOF
      ptx-kernel     OF 2 ENDOF
      aapcs32       OF 3 ENDOF
      c6000-eabi    OF 4 ENDOF
      sysv-amd64    OF 5 ENDOF
   ;MATCH ;

: ENDIAN-CODE ( CTARGET:endian -- n )
   MATCH endian
      little OF 0 ENDOF
      big    OF 1 ENDOF
   ;MATCH ;

: PTR-CODE ( CTARGET:ptr-width -- n )
   MATCH ptr-width
      bits32 OF 0 ENDOF
      bits64 OF 1 ENDOF
   ;MATCH ;

\ ---- coherence rules ---------------------------------------------------------
: ABI-ARCH? ( CTARGET:arch CTARGET:abi -- bool )
   {: a:arch b:abi :}
   b MATCH abi
      aapcs64-darwin OF a CTARGET-ARCH:AARCH64 CTARGET-ARCH:EQ ENDOF
      aapcs64-linux  OF a CTARGET-ARCH:AARCH64 CTARGET-ARCH:EQ ENDOF
      ptx-kernel     OF a CTARGET-ARCH:PTX CTARGET-ARCH:EQ ENDOF
      aapcs32       OF a CTARGET-ARCH:A32 CTARGET-ARCH:EQ
                       a CTARGET-ARCH:THUMB2 CTARGET-ARCH:EQ or ENDOF
      c6000-eabi    OF a CTARGET-ARCH:C66X CTARGET-ARCH:EQ ENDOF
      sysv-amd64    OF a CTARGET-ARCH:X86-64 CTARGET-ARCH:EQ ENDOF
   ;MATCH ;

\ Is the ABI defined for big-endian storage? Darwin's arm64 ABI is little-endian
\ by specification and PTX addresses little-endian memory; the Linux AArch64 ABI
\ is defined for both byte orders (aarch64 and aarch64_be).
: BIG-OK? ( CTARGET:abi -- bool )
   MATCH abi
      aapcs64-darwin OF false ENDOF
      aapcs64-linux  OF true ENDOF
      ptx-kernel     OF false ENDOF
      aapcs32       OF true ENDOF
      c6000-eabi    OF true ENDOF
      sysv-amd64    OF false ENDOF
   ;MATCH ;

\ Is the ABI defined for 32-bit addresses? Both AArch64 ABIs named here are
\ LP64; a 32-bit AArch64 ABI is a different ABI and would be a different variant.
\ PTX selects its address size per module (.address_size 32 or 64).
: PTR32-OK? ( CTARGET:abi -- bool )
   MATCH abi
      aapcs64-darwin OF false ENDOF
      aapcs64-linux  OF false ENDOF
      ptx-kernel     OF true ENDOF
      aapcs32       OF true ENDOF
      c6000-eabi    OF true ENDOF
      sysv-amd64    OF false ENDOF
   ;MATCH ;

: PTR64-OK? ( CTARGET:abi -- bool )
   MATCH abi
      aapcs64-darwin OF true ENDOF
      aapcs64-linux  OF true ENDOF
      ptx-kernel     OF true ENDOF
      aapcs32       OF false ENDOF
      c6000-eabi    OF false ENDOF
      sysv-amd64    OF true ENDOF
   ;MATCH ;

: MASK-N ( CTARGET:arch -- n )
   MATCH arch
      aarch64 OF MASK-AARCH64 ENDOF
      ptx     OF MASK-PTX ENDOF
      a32     OF MASK-ARM32 ENDOF
      thumb2  OF MASK-ARM32 ENDOF
      c66x    OF MASK-C66X ENDOF
      x86-64  OF MASK-X86-64 ENDOF
   ;MATCH ;

: ABI-CK ( CTARGET:arch CTARGET:abi -- )
   {: a:arch b:abi :}
   a b ABI-ARCH? 0= if E-CTGT-ABI throw then ;

: ENDIAN-CK ( CTARGET:abi CTARGET:endian -- )
   {: b:abi e:endian :}
   e CTARGET-ENDIAN:BIG CTARGET-ENDIAN:EQ 0= if exit then
   b BIG-OK? 0= if E-CTGT-ENDIAN throw then ;

: PTR-CK ( CTARGET:abi CTARGET:ptr-width -- )
   {: b:abi p:ptr-width :}
   p MATCH ptr-width
      bits32 OF b PTR32-OK? ENDOF
      bits64 OF b PTR64-OK? ENDOF
   ;MATCH 0= if E-CTGT-PTR throw then ;

: FEATURE-CK ( CTARGET:arch CTARGET:features -- )
   {: a:arch f:features :}
   f BITS {: bits:n :}
   bits BIT-ALL invert and 0<> if E-CTGT-FEATURE-BITS throw then
   bits BIT-BASE and 0= if E-CTGT-BASE throw then
   bits a MASK-N invert and 0<> if E-CTGT-FEATURE throw then ;

: CK ( CTARGET:arch CTARGET:abi CTARGET:endian CTARGET:ptr-width CTARGET:features -- )
   {: a:arch b:abi e:endian p:ptr-width f:features :}
   a b ABI-CK
   b e ENDIAN-CK
   b p PTR-CK
   a f FEATURE-CK ;

\ ---- canonical preimage ------------------------------------------------------
1 constant SCHEMA
7 constant SLOTS
0 constant SLOT-TAG
1 constant SLOT-SCHEMA
2 constant SLOT-ARCH
3 constant SLOT-ABI
4 constant SLOT-ENDIAN
5 constant SLOT-PTR
6 constant SLOT-FEATURES

SLOTS CDIGEST:SLOT-BYTES * constant PRE-BYTES
create PRE PRE-BYTES allot

public

\ ---- feature set -------------------------------------------------------------
: FEATURE-SET ( n -- CTARGET:features )
   dup BIT-ALL invert and 0<> if E-CTGT-FEATURE-BITS throw then
   MK ;

: FEATURES-N ( CTARGET:features -- n )
   BITS ;

: WITH ( CTARGET:features CTARGET:features -- CTARGET:features )
   BITS swap BITS or MK ;

\ Does the set hold every feature of the probe set?
: HAS? ( CTARGET:features CTARGET:features -- bool )
   {: set:features probe:features :}
   probe BITS {: want:n :}
   set BITS want and want = ;

\ The features an architecture is able to have.
: ARCH-MASK ( CTARGET:arch -- CTARGET:features )
   MASK-N MK ;

: F-BASE ( -- CTARGET:features )   BIT-BASE MK ;
: F-FP ( -- CTARGET:features )     BIT-FP MK ;
: F-SIMD ( -- CTARGET:features )   BIT-SIMD MK ;
: F-FP16 ( -- CTARGET:features )   BIT-FP16 MK ;
: F-BF16 ( -- CTARGET:features )   BIT-BF16 MK ;
: F-TF32 ( -- CTARGET:features )   BIT-TF32 MK ;
: F-ATOMIC ( -- CTARGET:features ) BIT-ATOMIC MK ;
: F-MMA ( -- CTARGET:features )    BIT-MMA MK ;
: F-ASYNC ( -- CTARGET:features )  BIT-ASYNC MK ;

\ ---- construction and validation --------------------------------------------
\ The production entry point: an incoherent combination throws a named error and
\ no contract value is produced.
: CONTRACT ( CTARGET:arch CTARGET:abi CTARGET:endian CTARGET:ptr-width CTARGET:features -- CTARGET:contract )
   {: a:arch b:abi e:endian p:ptr-width f:features :}
   a b e p f CK
   a b e p f CTARGET-CONTRACT:MAKE ;

\ Recheck a contract that may have been assembled by the generated constructor.
: VALIDATE ( CTARGET:contract -- CTARGET:contract )
   CTARGET-CONTRACT:UNMAKE CONTRACT ;

\ ---- field readers -----------------------------------------------------------
: ARCH@ ( CTARGET:contract -- CTARGET:arch )
   CTARGET-CONTRACT:UNMAKE drop drop drop drop ;

: ABI@ ( CTARGET:contract -- CTARGET:abi )
   CTARGET-CONTRACT:UNMAKE drop drop drop nip ;

: ENDIAN@ ( CTARGET:contract -- CTARGET:endian )
   CTARGET-CONTRACT:UNMAKE drop drop nip nip ;

: PTR-WIDTH@ ( CTARGET:contract -- CTARGET:ptr-width )
   CTARGET-CONTRACT:UNMAKE drop nip nip nip ;

: FEATURES@ ( CTARGET:contract -- CTARGET:features )
   CTARGET-CONTRACT:UNMAKE nip nip nip nip ;

\ The pointer width in bits, for consumers that need the number.
: PTR-BITS ( CTARGET:contract -- n )
   PTR-WIDTH@
   MATCH ptr-width
      bits32 OF 32 ENDOF
      bits64 OF 64 ENDOF
   ;MATCH ;

\ ---- identity ----------------------------------------------------------------
\ Field-by-field identity. Both inputs are revalidated first, so a forged
\ contract cannot be compared as if it were a declarable target.
: SAME? ( CTARGET:contract CTARGET:contract -- bool )
   VALIDATE CTARGET-CONTRACT:UNMAKE {: ya:arch yb:abi ye:endian yp:ptr-width yf:features :}
   VALIDATE CTARGET-CONTRACT:UNMAKE {: xa:arch xb:abi xe:endian xp:ptr-width xf:features :}
   xa ya CTARGET-ARCH:EQ
   xb yb CTARGET-ABI:EQ and
   xe ye CTARGET-ENDIAN:EQ and
   xp yp CTARGET-PTR--WIDTH:EQ and
   xf yf CTARGET-FEATURES:EQ and ;

\ The canonical preimage. The bytes live in this module and stay valid until the
\ next ENCODE call; DIGEST is the copy-free consumer, and the target/policy
\ binding reads the slots straight back out.
: ENCODE ( CTARGET:contract -- ptr u8 n )
   VALIDATE CTARGET-CONTRACT:UNMAKE
   {: a:arch b:abi e:endian p:ptr-width f:features :}
   CDIGEST:TAG-TARGET PRE SLOT-TAG CDIGEST:SLOT!
   SCHEMA PRE SLOT-SCHEMA CDIGEST:SLOT!
   a ARCH-CODE PRE SLOT-ARCH CDIGEST:SLOT!
   b ABI-CODE PRE SLOT-ABI CDIGEST:SLOT!
   e ENDIAN-CODE PRE SLOT-ENDIAN CDIGEST:SLOT!
   p PTR-CODE PRE SLOT-PTR CDIGEST:SLOT!
   f BITS PRE SLOT-FEATURES CDIGEST:SLOT!
   PRE PRE-BYTES ;

: DIGEST ( CTARGET:contract -- CDIGEST:digest )
   ENCODE CDIGEST:COMPUTE ;

\ ---- the backend registry ----------------------------------------------------
\ WHAT THIS ANSWERS. A contract says what machine a compilation is for. It does
\ not say that this image can produce code for that machine - the header above
\ says why those are different questions - and the answer belongs to whoever can
\ produce the instructions. A backend is therefore a MODULE: it registers a row
\ when it loads, and there is no list of backends anywhere for a new one to be
\ added to. An image that never loads src/arch/arm64/ has no aarch64 row, and
\ asking either stage about an aarch64 contract refuses with E-CTGT-UNLOADED
\ rather than compiling with a backend that is not there. That is what lets a
\ binary carry only the backend its own sources require.
\
\ THE ROW. The key is the architecture's wire code. That code is injective and
\ may never be renumbered - the identity rule above is what fixes it - which is
\ exactly what a key needs, and taking it means the registry has no opinion on
\ how many variants the family has: the table is sized by how many backends one
\ image may hold, so a new architecture adds nothing here. (It also keeps a row
\ a plain cell. A TYPED-BUFFER of the family VALUE is refused by the native
\ chain's own dialect - E-HIR-UNMODELED, measured while compiling this file into
\ an engine - so the row could not have held one anyway.)
\
\ Beside the key sit the two stages - lowering, which builds the machine module,
\ and emission, which writes the instructions - each a quotation answering
\ whether that stage can serve one exact contract. They are separate rows
\ because the two stages ask separately, each with its own refusal, and a
\ backend may lower for a machine whose instructions it cannot yet write. A
\ quotation is not a structure field, so the row is parallel TYPED-BUFFERs
\ indexed together; a table that later stores a stage's pass beside its
\ predicate is indexed the same way, which is why ROW is public and is the one
\ answer to "which row is this architecture".
\
\ A ROW IS NEVER RELEASED. Code that has loaded cannot unload. A second module
\ claiming an architecture that already has a row is a build defect - `require`
\ makes loading one module twice a no-op - so it is refused instead of replacing
\ the row the loaded code is already reaching through.

public

\ The rows an image may hold at once, and the size of every table indexed
\ beside them.
4 constant BACKEND-ROWS

private

BACKEND-ROWS TYPED-BUFFER B-ARCH  n
BACKEND-ROWS TYPED-BUFFER B-LOWER [ CTARGET:contract -- bool ]
BACKEND-ROWS TYPED-BUFFER B-EMIT  [ CTARGET:contract -- bool ]

\ Rows 0..B-N-1 are claimed, in claim order. A claim publishes itself by bumping
\ this AFTER the row's cells are stored, so a half-built row is never found.
variable B-N

: FIND-ROW ( CTARGET:arch -- n )
   ARCH-CODE {: a:n :}
   -1 B-N @ 0 ?do
      a i B-ARCH @ = if drop i leave then
   loop ;

public

: REGISTERED? ( CTARGET:arch -- bool )
   FIND-ROW 0 >= ;

\ The row an architecture's backend holds, so a table indexed beside the
\ registry's own rows has one place to ask.
: ROW ( CTARGET:arch -- n )
   FIND-ROW dup 0 < if E-CTGT-UNLOADED throw then ;

\ The registration a backend module performs as it loads.
: REGISTER ( CTARGET:arch [ CTARGET:contract -- bool ] [ CTARGET:contract -- bool ] -- )
   {: a:arch lower emit :}
   a REGISTERED? if E-CTGT-REGISTERED throw then
   B-N @ {: row:n :}
   row BACKEND-ROWS >= if E-CTGT-ROW throw then
   a ARCH-CODE row B-ARCH !
   lower row B-LOWER !
   emit row B-EMIT !
   row 1+ B-N ! ;

\ Can the loaded backend for this contract's architecture lower for it? Emit for
\ it? The contract is revalidated here for the reason the header gives, so the
\ backend's own predicate is asked about a declarable machine. An architecture
\ with no backend refuses; one with a backend gets that backend's answer, so
\ "no code for this machine in this image" and "this backend does not serve this
\ machine" stay different answers with different owners.
: LOWERS? ( CTARGET:contract -- bool )
   VALIDATE dup ARCH@ ROW B-LOWER @ execute ;

: EMITS? ( CTARGET:contract -- bool )
   VALIDATE dup ARCH@ ROW B-EMIT @ execute ;


;package
