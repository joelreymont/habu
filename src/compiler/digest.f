\ digest.f - canonical deterministic digests for compiler contract records.
\
\ docs/compiler-ir-design.md sections 5.4, 5.6 and 5.7. Every compiler record
\ whose identity has to survive caching, replay and proof witnesses is digested
\ the same way: the record is written out as a CANONICAL PREIMAGE - a fixed
\ sequence of eight-byte little-endian slots - and SHA-256 is taken over exactly
\ those bytes. Slot 0 is always a domain-separation tag owned by this package and
\ slot 1 is always the producing module's own schema version, so two records of
\ different kinds, or of different schema generations, can never share a preimage.
\
\ Why a preimage of fixed-width slots rather than rendered text: the encoding has
\ to be INJECTIVE on the record's semantic domain, because the digest is the
\ record's identity. With a fixed slot count per (tag, version) and one slot per
\ semantic field, the position of a byte determines which field it belongs to, so
\ two records with different field values necessarily differ in some preimage
\ byte. Text rendering would need separators, escaping and a length argument to
\ reach the same property. A record made of another record's preimage (the
\ target/policy binding) concatenates the whole component preimage, tag and
\ version included, so a component's schema change flows into the composite
\ digest without any module having to remember to bump a second version number.
\
\ Digests are VALUES, not authority tokens. `digest` is a public family, so its
\ generated MAKE can assemble any four cells; nothing here treats a presented
\ digest as evidence. A caller that needs to know an artifact was built under a
\ given contract RECOMPUTES the contract's digest and compares. That is also why
\ CDIGEST:COMPUTE takes the caller's bytes rather than owning a builder: the
\ producing module owns its preimage buffer and its field order.
\
\ Not reentrant. The engine's SHA-256 state (src/core/sha256.f) is a single
\ process-wide context, and this module keeps one output buffer, so a digest must
\ be computed on one thread at a time. That is a pre-existing property of every
\ SHA-256 caller in the tree (lib/content-key.f, maki/db/transaction.f,
\ maki/target/target.f), not something the compiler records introduce; a
\ multi-threaded compiler needs a reentrant hash context first.

require lib/errors.f

package CDIGEST
public

\ A 256-bit digest held as the four 64-bit little-endian words of the SHA-256
\ output, deepest word first. Multi-cell, so no single-cell scalar can launder
\ into it. DERIVE eq gives the typed identity compare CDIGEST-DIGEST:EQ.
STRUCTURE digest 0 DERIVE eq
   FIELD w0 n
   FIELD w1 n
   FIELD w2 n
   FIELD w3 n
;STRUCTURE

\ One preimage slot is one 64-bit little-endian cell. Producers size their
\ buffers in slots and composite producers convert a component's byte length
\ back into a slot count with it.
8 constant SLOT-BYTES

\ Domain-separation tags. Slot 0 of a canonical preimage names the record kind,
\ and this package is the single place the kinds are allocated, so two record
\ kinds cannot silently share one.
1 constant TAG-TARGET
2 constant TAG-NUMERIC
3 constant TAG-BINDING
4 constant TAG-SCHEMA        \ one dialect operation schema record
5 constant TAG-SCHEMA-TABLE  \ a dialect's whole schema table, chained over its record digests
6 constant TAG-A64-ROUTINE   \ one ARM64 routine machine-effect contract

private

32 constant OUT-BYTES

create DG OUT-BYTES allot

public

\ Write one semantic field code into a preimage slot, little-endian.
: SLOT! ( n ptr u8 n -- )
   {: v:n base:ptr slot:n :}
   base slot SLOT-BYTES * + {: at:ptr :}
   SLOT-BYTES 0 ?do
      v i 8 * rshift $FF and  at i + c!
   loop ;

\ Read one preimage slot back. The inverse of SLOT! and the reader composite
\ producers use to copy a component preimage into their own.
: SLOT@ ( ptr u8 n -- n )
   {: base:ptr slot:n :}
   base slot SLOT-BYTES * + {: at:ptr :}
   0
   SLOT-BYTES 0 ?do
      at i + c@  i 8 * lshift  or
   loop ;

\ SHA-256 over exactly the caller's preimage bytes, read out as four words.
: COMPUTE ( ptr u8 n -- CDIGEST:digest )
   DG SHA256
   DG 0 SLOT@  DG 1 SLOT@  DG 2 SLOT@  DG 3 SLOT@
   CDIGEST-DIGEST:MAKE ;

;package
