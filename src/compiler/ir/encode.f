\ encode.f - the canonical wire frame: the one authority that says how a
\ canonicalized module becomes bytes, and what those bytes commit to.
\
\ docs/compiler-ir-design.md section 6.6 fixes the order a frozen module
\ serializes in - header, target and numeric-policy digests, dialect/schema
\ versions, then the tables - adds "The encoding includes explicit counts and
\ lengths; it does not serialize host addresses or arena capacities", and names
\ IR:ENCODE and IR:DIGEST among the five words the stage publishes.
\
\ THE AUTHORITY SPLIT THIS FILE INHERITS. src/compiler/ir/canon.f owns the
\ canonical ordinal of every interned row and the renumbered cell stream: the
\ section 6.6 table order with every stored reference already rewritten into
\ canonical numbering. This file owns everything around that stream - the
\ header, the format version, the field width, the counts and lengths, the
\ full-input consumption rule, and the SHA-256 content digest. So a module's
\ canonical CONTENT has exactly one owner and its canonical BYTES have exactly
\ one owner, and neither can disagree with the other. Nothing here reads inside
\ the payload: this file never learns how many sections the stream has or what a
\ row of one looks like, so a change to a table's row shape is canon.f's alone
\ and a change to framing or versioning is this file's alone.
\
\ WHY THE FRAME IS WRITTEN INTO THE CALLER'S BYTES. A canonical table is a
\ resource: later passes rewrite references through it, so it has an identity
\ and canon.f keeps it in an owned registry with a committed ceiling of eight
\ tables per context. A frame is not a resource, it is a VALUE - like a digest,
\ it is exactly its bytes and nothing observes it twice - and every real consumer
\ (a cache file, a proof witness, a content key) has to copy it into storage of
\ its own anyway. So ENCODE writes into a byte span the caller already owns and
\ answers the length it wrote, SIZE tells the caller the exact length beforehand,
\ and a span too short is refused by name. The encoder holds no registry, takes
\ no arena, and consumes none of canon.f's eight slots, so encoding a module can
\ never be the thing that exhausts a context.
\
\ WHAT THE HEADER BINDS, AND WHY EACH FIELD IS THERE. Section 5.7 requires that
\ identical inputs produce the same canonical module and digest, which means a
\ digest is only meaningful together with what the module was built under. The
\ header therefore states, before any payload byte:
\
\   the magic and the format major/minor version, so bytes that are not one of
\   our frames, or are a generation this reader was not written for, are refused
\   rather than parsed;
\
\   the bound target contract's digest and the numeric policy's digest, taken
\   from the compilation context, so the same module compiled under a different
\   target or a different floating-point policy is a different frame;
\
\   the dialect's schema major/minor version, taken from the module's own frozen
\   schema table, so a frame binds the generation of the operation vocabulary its
\   payload was checked against;
\
\   the dialect name's canonical symbol ordinal, which is where in the payload's
\   symbol table the dialect's own name is;
\
\   the canonical row count of each of the four interned tables, and the number
\   of payload slots that follow. These are the "explicit counts and lengths"
\   section 6.6 asks for: a reader validates the frame's shape from the header
\   alone, before it allocates anything or reads a payload slot.
\
\ HOW THE MODULE AND THE TABLE ARE PROVED TO BE EACH OTHER. ENCODE is handed a
\ frozen module and a canonical table, and a header built from one module's
\ schema table over another module's stream would be a lie no later check could
\ catch. The proof is structural and costs nothing extra: the header needs the
\ dialect name's canonical ordinal, and asking the canonical table for the
\ ordinal of an identity the module minted is IR-CANON's own owner check, so a
\ mismatched pair leaves as E-IR-CANON-OWNER before a byte is written. Every
\ module has that identity to ask about, because IR-BUILD:NEW-BUILDER interns the
\ dialect's name before any caller can intern anything. Asking for it also proves
\ the module is frozen, because the dialect name is read through the module's
\ frozen schema view, so once that one lookup answers, every other field this
\ file states is a total read and the frame will be written whole.
\
\ FIELD WIDTH. Every field of the frame, header and payload alike, is one
\ eight-byte little-endian slot, and that slot is CDIGEST's preimage slot rather
\ than a second convention declared here: CDIGEST:SLOT! and CDIGEST:SLOT@ are the
\ tree's one owner of "a semantic field is eight little-endian bytes", the same
\ packing canon.f mirrors for the byte runs inside its stream, and reusing it
\ means a frame slot and a digest preimage slot can never drift apart. No field
\ is variable width, so a byte's position determines which field it belongs to,
\ which is what makes the digest below an identity rather than a summary.
\
\ THE CONTENT DIGEST. DIGEST is SHA-256 over exactly a whole frame's bytes, and
\ only over a frame that already passed the framing check, so a digest is never
\ taken of bytes whose shape was not proven. Two modules built along two
\ different intern insertion orders have the same canonical stream, so they have
\ the same frame and the same digest; anything the module orders on purpose -
\ block layout, instruction order, which operand is which - is in the stream, so
\ it stays in the bytes and in the digest.
\
\ WHY THE DIALECT'S SCHEMA-TABLE DIGEST IS NOT IN THE FRAME. canon.f's header
\ suggests binding IR-SCHEMA:FTABLE-DIGEST here, and that turned out not to be
\ possible: that digest folds each schema record's stored operand, result and
\ attribute-key lists, and those lists hold module-local INSERTION ordinals, so
\ two equivalent modules built along two intern orders have two different
\ schema-table digests. test/compiler/ir-encode.f measures that difference
\ directly. Putting it in the frame would make a module's identity depend on the
\ order its tables were interned in, which is the one thing canonicalization
\ exists to remove, so the frame binds the dialect's canonical name ordinal and
\ its schema major and minor version instead. That is exactly what design section
\ 6.6 asks the header to state - "dialect/schema versions" - while the
\ schema-digest of design line 602 belongs to the pass witness header of section
\ 6.7, which binds one pass over one module in one process and may therefore use
\ a non-canonical digest. A canonical schema-table digest is a real missing
\ capability, owned by IR-SCHEMA or by a canonicalized schema section in canon.f
\ rather than recomputed here, and it is tracked by its own dot.
\
\ WHY A FRAME IS NOT A CDIGEST PREIMAGE RECORD. CDIGEST allocates a
\ domain-separation tag per record kind, and slot 0 of one of its preimages is
\ that tag. A frame is not one of those records: those are fixed-slot-count
\ records for one (tag, version) pair, and a frame is variable length and
\ self-describing. It carries its own magic in slot 0 and its own format version
\ in slots 1 and 2, which do the same separating work, and the magic is four
\ ASCII bytes rather than a small integer, so a frame's leading slot can never
\ collide with a CDIGEST tag. CDIGEST is used here for its slot packing and its
\ SHA-256, not for its record discipline.
\
\ THE COMMITTED FRAME CEILING. A payload may hold at most CELL-MAX slots. The
\ number covers IR-BUILD's committed production plan with room left over - that
\ plan's ceilings are 1920 rows across the eight ordered tables, at most sixteen
\ stated slots each, plus the 1408 pool slots the variable-length content of
\ those rows comes out of - and it is also the decoder limit: a presented frame
\ whose header claims more is refused before its caller sizes a buffer from that
\ claim. A stated row count larger than the payload that would have to hold it is
\ refused the same way.
\
\ FULL-INPUT CONSUMPTION. A frame's byte length must be exactly its header plus
\ the payload slots its header states. Truncated bytes and trailing bytes are the
\ same refusal, because either one means the presented span is not one frame.

require lib/prelude.f
require lib/errors.f
require src/compiler/digest.f
require src/compiler/target.f
require src/compiler/numeric-policy.f
require src/compiler/binding.f
require src/compiler/ir/context.f
require src/compiler/ir/schema.f
require src/compiler/ir/build.f
require src/compiler/ir/canon.f

package IR-ENCODE
private

\ ---- the format --------------------------------------------------------------
\ "HBIR" read as four little-endian ASCII bytes: H at byte zero.
$52494248 constant MAGIC
1 constant FMT-MAJOR
0 constant FMT-MINOR

\ ---- the header's slots ------------------------------------------------------
\ Fixed positions in one eight-byte-slot header. The three digests take four
\ slots each, deepest word first, which is the order CDIGEST reads them out in.
0 constant HS-MAGIC
1 constant HS-MAJOR                  \ frame format major version
2 constant HS-MINOR                  \ frame format minor version
3 constant HS-TARGET                 \ bound target contract digest
7 constant HS-POLICY                 \ bound numeric policy digest
11 constant HS-SMAJOR                \ the dialect's schema major version
12 constant HS-SMINOR                \ the dialect's schema minor version
13 constant HS-DIALECT               \ the dialect name's canonical symbol ordinal
14 constant HS-SYMS                  \ canonical symbol rows
15 constant HS-TYPES
16 constant HS-ATTRS
17 constant HS-SRCS
18 constant HS-CELLS                 \ payload slots that follow the header
19 constant HDR-SLOTS

HDR-SLOTS CDIGEST:SLOT-BYTES * constant HDR-BYTES

\ The committed ceiling on one payload, and so on one frame.
32768 constant CELL-MAX

\ ---- one digest, four slots --------------------------------------------------
: PUT-DIGEST ( ptr u8 n CDIGEST:digest -- )
   CDIGEST-DIGEST:UNMAKE
   {: p at:n w0:n w1:n w2:n w3:n :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   w0 p at CDIGEST:SLOT!
   w1 p at 1+ CDIGEST:SLOT!
   w2 p at 2 + CDIGEST:SLOT!
   w3 p at 3 + CDIGEST:SLOT! ;

: GET-DIGEST ( ptr u8 n -- CDIGEST:digest )
   {: p at:n :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   p at CDIGEST:SLOT@
   p at 1+ CDIGEST:SLOT@
   p at 2 + CDIGEST:SLOT@
   p at 3 + CDIGEST:SLOT@
   CDIGEST-DIGEST:MAKE ;

\ ---- what the header binds ---------------------------------------------------
: TARGET-DIG ( IR-CTX:ctx -- CDIGEST:digest )
   IR-CTX:BINDING@ CBIND:TARGET@ CTARGET:DIGEST ;

: POLICY-DIG ( IR-CTX:ctx -- CDIGEST:digest )
   IR-CTX:BINDING@ CBIND:POLICY@ CNUM:DIGEST ;

: SCHEMA-MAJ ( IR-BUILD:module -- n )
   IR-BUILD:FSCHEMA-ROWS IR-SCHEMA:FMAJOR@ ;

: SCHEMA-MIN ( IR-BUILD:module -- n )
   IR-BUILD:FSCHEMA-ROWS IR-SCHEMA:FMINOR@ ;

: DIALECT-SYM ( IR-BUILD:module -- IR-ID:ir-symbol-id )
   {: m:IR-BUILD:module :}
   m IR-BUILD:FSCHEMA-ROWS  m IR-BUILD:FKEY  IR-SCHEMA:FDIALECT@ ;

\ The dialect name's canonical ordinal, and with it the proof that this table
\ numbers this module's rows: an identity another module minted is IR-CANON's own
\ E-IR-CANON-OWNER refusal, and it runs before any byte is written.
: DIALECT-ORD ( IR-BUILD:module IR-CANON:table -- n )
   {: m:IR-BUILD:module t:IR-CANON:table :}
   t  m DIALECT-SYM  IR-CANON:SYMBOL-ORD ;

\ ---- the committed ceiling ---------------------------------------------------
: CELLS-CK ( n -- n )
   {: k:n :}
   k 0 < k CELL-MAX > or if E-IR-ENCODE-CAP throw then
   k ;

\ ---- writing the header ------------------------------------------------------
: PUT-FORMAT ( ptr u8 -- )
   {: p :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   MAGIC p HS-MAGIC CDIGEST:SLOT!
   FMT-MAJOR p HS-MAJOR CDIGEST:SLOT!
   FMT-MINOR p HS-MINOR CDIGEST:SLOT! ;

: PUT-BINDING ( IR-CTX:ctx ptr u8 -- )
   {: c:IR-CTX:ctx p :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   p HS-TARGET c TARGET-DIG PUT-DIGEST
   p HS-POLICY c POLICY-DIG PUT-DIGEST ;

\ The dialect's canonical ordinal is passed in rather than looked up here,
\ because looking it up is the pairing proof and that has to run before the frame
\ is written to, not part way through writing it.
: PUT-DIALECT ( IR-BUILD:module n ptr u8 -- )
   {: m:IR-BUILD:module dia:n p :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   m SCHEMA-MAJ p HS-SMAJOR CDIGEST:SLOT!
   m SCHEMA-MIN p HS-SMINOR CDIGEST:SLOT!
   dia p HS-DIALECT CDIGEST:SLOT! ;

: PUT-COUNTS ( IR-CANON:table ptr u8 -- )
   {: t:IR-CANON:table p :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   t IR-CANON:SYMBOLS p HS-SYMS CDIGEST:SLOT!
   t IR-CANON:TYPES p HS-TYPES CDIGEST:SLOT!
   t IR-CANON:ATTRS p HS-ATTRS CDIGEST:SLOT!
   t IR-CANON:SOURCES p HS-SRCS CDIGEST:SLOT!
   t IR-CANON:CELLS CELLS-CK p HS-CELLS CDIGEST:SLOT! ;

: PUT-HEAD ( IR-CTX:ctx IR-BUILD:module IR-CANON:table n ptr u8 -- )
   {: c:IR-CTX:ctx m:IR-BUILD:module t:IR-CANON:table dia:n p :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   p PUT-FORMAT
   c p PUT-BINDING
   m dia p PUT-DIALECT
   t p PUT-COUNTS ;

\ ---- writing the payload -----------------------------------------------------
\ The canonical stream, slot for slot, after the header. This is the whole of
\ what the encoder knows about the payload: how many slots there are and what
\ each one holds.
: PUT-PAYLOAD ( IR-CANON:table ptr u8 -- )
   {: t:IR-CANON:table p :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   t IR-CANON:CELLS 0 ?do
      t i IR-CANON:CELL@  p  HDR-SLOTS i +  CDIGEST:SLOT!
   loop ;

\ ---- checking a presented frame ----------------------------------------------
\ Bytes too short to hold a header, or a leading slot that is not the magic, are
\ not one of our frames at all, so nothing else about them is believed.
: MAGIC-CK ( ptr u8 n -- )
   {: p n:n :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   n HDR-BYTES < if E-IR-ENCODE-STATE throw then
   p HS-MAGIC CDIGEST:SLOT@ MAGIC <> if E-IR-ENCODE-STATE throw then ;

\ A different major version is a different format. A higher minor version states
\ header or section content this reader was not written to see, so it is refused
\ rather than read as far as it is understood.
: VERSION-CK ( ptr u8 -- )
   {: p :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   p HS-MAJOR CDIGEST:SLOT@ FMT-MAJOR <> if E-IR-ENCODE-VERSION throw then
   p HS-MINOR CDIGEST:SLOT@ {: minor:n :}
   minor 0 < minor FMT-MINOR > or if E-IR-ENCODE-VERSION throw then ;

\ Full-input consumption: the span is exactly one frame, so a truncated span and
\ a span with anything after the payload are the same refusal.
: LENGTH-CK ( ptr u8 n -- )
   {: p n:n :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   p HS-CELLS CDIGEST:SLOT@ CELLS-CK HDR-SLOTS + CDIGEST:SLOT-BYTES * {: want:n :}
   want n <> if E-IR-ENCODE-FRAME throw then ;

\ Every canonical row costs at least one payload slot, so a stated row count
\ larger than the payload is a claim the payload could not carry.
: COUNT-CK ( ptr u8 n -- )
   {: p at:n :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   p at CDIGEST:SLOT@ {: rows:n :}
   p HS-CELLS CDIGEST:SLOT@ {: k:n :}
   rows 0 < rows k > or if E-IR-ENCODE-CAP throw then ;

: COUNTS-CK ( ptr u8 -- )
   {: p :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   p HS-SYMS COUNT-CK
   p HS-TYPES COUNT-CK
   p HS-ATTRS COUNT-CK
   p HS-SRCS COUNT-CK ;

public

\ ---- the framing check -------------------------------------------------------
\ What every reader below runs first, and what a caller runs before it believes
\ anything about presented bytes. Nothing here allocates and nothing here reads
\ a payload slot, so a malformed, mis-versioned, truncated, oversized or
\ trailing-data span is refused by name at no cost.
: FRAME-CK ( ptr u8 n -- )
   {: p n:n :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   p n MAGIC-CK
   p VERSION-CK
   p n LENGTH-CK
   p COUNTS-CK ;

private

\ ---- reading one validated header slot ---------------------------------------
: HDR-AT ( ptr u8 n n -- n )
   {: p n:n at:n :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   p n FRAME-CK
   p at CDIGEST:SLOT@ ;

: HDR-DIG ( ptr u8 n n -- CDIGEST:digest )
   {: p n:n at:n :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   p n FRAME-CK
   p at GET-DIGEST ;

public

\ ---- encoding ----------------------------------------------------------------
\ The exact byte length the frame for this canonical table will have, so a
\ caller sizes its span before it asks for the bytes.
: SIZE ( IR-CANON:table -- n )
   IR-CANON:CELLS CELLS-CK HDR-SLOTS + CDIGEST:SLOT-BYTES * ;

\ Write the frame and answer the length written. Everything that can refuse runs
\ before the first byte is written: SIZE proves the table is live and inside the
\ frame ceiling, the room check proves the destination holds the whole frame, and
\ the dialect ordinal proves the module is frozen and that this table numbers its
\ rows. Past that point every header field and every payload slot is a total
\ read, so a refused encode never leaves half a frame behind.
: ENCODE ( IR-CTX:ctx IR-BUILD:module IR-CANON:table ptr u8 n -- n )
   {: c:IR-CTX:ctx m:IR-BUILD:module t:IR-CANON:table p room:n :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   t SIZE {: len:n :}
   room len < if E-IR-ENCODE-ROOM throw then
   m t DIALECT-ORD {: dia:n :}
   c m t dia p PUT-HEAD
   t p PUT-PAYLOAD
   len ;

\ ---- the content digest ------------------------------------------------------
\ SHA-256 over exactly one frame's bytes. The framing check runs first, so a
\ digest is only ever the identity of bytes whose shape was proven.
: DIGEST ( ptr u8 n -- CDIGEST:digest )
   {: p n:n :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   p n FRAME-CK
   p n CDIGEST:COMPUTE ;

\ ---- reading a frame ---------------------------------------------------------
: FORMAT-MAJOR ( ptr u8 n -- n )
   HS-MAJOR HDR-AT ;

: FORMAT-MINOR ( ptr u8 n -- n )
   HS-MINOR HDR-AT ;

: TARGET-DIGEST ( ptr u8 n -- CDIGEST:digest )
   HS-TARGET HDR-DIG ;

: POLICY-DIGEST ( ptr u8 n -- CDIGEST:digest )
   HS-POLICY HDR-DIG ;

: SCHEMA-MAJOR ( ptr u8 n -- n )
   HS-SMAJOR HDR-AT ;

: SCHEMA-MINOR ( ptr u8 n -- n )
   HS-SMINOR HDR-AT ;

: DIALECT ( ptr u8 n -- n )
   HS-DIALECT HDR-AT ;

: SYMBOLS ( ptr u8 n -- n )
   HS-SYMS HDR-AT ;

: TYPES ( ptr u8 n -- n )
   HS-TYPES HDR-AT ;

: ATTRS ( ptr u8 n -- n )
   HS-ATTRS HDR-AT ;

: SOURCES ( ptr u8 n -- n )
   HS-SRCS HDR-AT ;

: CELLS ( ptr u8 n -- n )
   HS-CELLS HDR-AT ;

\ One payload slot of a validated frame, in the canonical stream's own numbering.
: CELL@ ( ptr u8 n n -- n )
   {: p n:n k:n :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   p n FRAME-CK
   p HS-CELLS CDIGEST:SLOT@ {: have:n :}
   k 0 < k have >= or if E-IR-ENCODE-BOUND throw then
   p HDR-SLOTS k + CDIGEST:SLOT@ ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;package
