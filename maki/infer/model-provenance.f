\ model-provenance.f - the model artifact-set provenance pin (sealed package
\ MODELPROV; inference leaf S3c, epic habu-epic-gb10-uma-391d12e8).
\
\ CONCERN: one immutable value, the pin, that names WHICH published artifact set
\ a model came from. It is a 32-byte CONTENT-KEY digest over the whole artifact
\ set - the model family label, its revision, the configuration digest, the
\ tokenizer digest, the tensor artifact's own whole-file digest, the typed
\ weight-format adapter, the residency choice, and the packing convention - plus
\ a private proof token so a raw four-cell value cannot pose as a pin.
\
\ TWO IDENTITY DOMAINS THAT MUST NOT COLLAPSE. MDLCFG:cfgkey is the identity of
\ the normalized behavioral CONFIGURATION and is what weight lookup and layer
\ identity compare. MODELPROV:mprov is the identity of the ARTIFACT SET on disk
\ and is consumed by the pack manifest and by compatibility checks - never by
\ weight lookup, never by bind. The two carry different private proof families
\ (MDLCFG:cfg-proof and MODELPROV:prov-proof), so neither value can be passed
\ where the other is required; the cross-domain checker negative in
\ model-provenance-test.f pins that.
\
\ CONSEQUENTLY THIS PACKAGE IS OFF THE COMPUTE PATH. Nothing in the bind or
\ lookup chain requires it (inference design rev 4, blackboard
\ 20260724-191041.846 correction 1), and it defines no configuration, geometry,
\ tensor, or slot authority of its own: the adapter identity is MODEL:adapter,
\ the residency choice is WSTORE:residency, and the digest machinery is
\ CONTENT-KEY. `packing` is the one family this package does own, because the
\ transpose convention a PACKER wrote with is a fact about the published pack;
\ it is not GPT2BIND's per-key source orientation, which describes the incoming
\ checkpoint instead.
\
\ ONE INPUT IS DERIVED; THE OTHER TWO DIGESTS ARE THE CALLER'S ASSERTION. Be
\ precise about which is which, because it decides what a pin proves. The TENSOR
\ artifact is derived: PIN is handed its path and the digest it is expected to
\ have, digests the file itself, and refuses with a named throw when the file
\ cannot be read (E-PROV-ARTIFACT) or when the bytes on disk disagree with the
\ expectation (E-PROV-DIGEST); the digest folded afterwards is the one PIN
\ COMPUTED, so no pin describes tensor bytes nobody read. The CONFIGURATION and
\ TOKENIZER digests are ASSERTED: PIN is not handed those artifacts and cannot
\ check them, so it validates only their shape - exactly 64 lower-case
\ hexadecimal characters, which stops a pin over "unknown" or a truncated digest
\ but not one over an honestly-formatted wrong value. A caller that needs those
\ two derived as well must digest the files and pass what it computed. Labels
\ must be non-empty, at most LABEL-MAX bytes, and printable with no spaces, so
\ the same artifact set cannot pin two ways through stray whitespace. The path
\ length arrives as a validated CAD-NUM:byte-len: a raw cell cannot reach the
\ NUL-terminating path primitive through this word, which is what keeps a
\ negative or over-long length from becoming a memory fault instead of a
\ refusal.
\
\ PIN RESETS THE SHARED PREIMAGE BUFFER. CONTENT-KEY has one process-wide
\ accumulator, and PIN calls CONTENT-KEY:RESET unconditionally before folding.
\ A caller assembling its own content key must therefore compute the pin FIRST
\ and fold the pin's rendering into its own key afterwards; folding a pin in the
\ middle of another preimage discards that preimage.
\
\ KEY PREIMAGE, in this declared order: the preimage schema version, family
\ label, revision label, configuration digest, tokenizer digest, computed tensor
\ digest, the adapter variant's canonical tag text, the residency variant's tag
\ text, the packing variant's tag text. Every row CONTENT-KEY:TEXT+ folds is tag-
\ and length-delimited by the library, so adjacent fields cannot alias -
\ "ab"+"c" and "a"+"bc" are different preimages, which model-provenance-test.f
\ asserts by walking the real preimage rows.
\
\ THE VERSION ROW IS FIRST AND IS OURS, NOT THE CALLER'S. A manifest persists the
\ KEY-HEX rendering, so the preimage format is durable data the day it is first
\ written; SCHEMA-V names the format this build folds, so a later format change
\ is an explicit bump that visibly re-renders every key rather than a silent
\ collision between two meanings of the same digest. It is a source constant
\ rather than a PIN argument on purpose: a caller must not be able to stamp a
\ pin with a format version this code does not implement. It folds as its 8 raw
\ little-endian cell bytes, the same way MDLCFG:BUILD folds mcfg's
\ schema-version, so a reader meets one version convention across the two
\ identity domains.
\
\ THE PROOF, AND THE GAP IT DOES NOT CLOSE. prov-proof is an arity-0 NEWTYPE,
\ the maki/typestate.f ART:built shape also used by MDLCFG: the engine
\ fail-closes a zero-field STRUCTURE used as a product field (throw 7127), so a
\ nominal cell family is the one shape that rides inside mprov while staying
\ constructible ONLY through the package-private trusted mint. A raw n in the
\ proof slot is a checker reject and the mint is unresolvable outside the
\ package. The honest limit, shared with ART:built and MDLCFG: mprov is
\ non-linear, so a holder of a REAL pin can UNMAKE it and re-MAKE a value with
\ different key cells and that same stale proof. Closing that needs the sealed
\ destructure / linear UNMAKE checker capability tracked by dot
\ habu-checker-sealed-destructure-d967fc03. The proof stops forgery from
\ nothing; it does not yet stop tampering by a holder.
\
\ SERIALIZATION: THIS PACKAGE PUBLISHES NO INVERSE, WHICH IS NOT THE SAME AS
\ THERE BEING NONE. KEY-HEX renders a pin as its canonical 64-character
\ hexadecimal text so a manifest can write it and later compare it against a
\ freshly computed pin, and manifest validation compares keys instead of
\ re-deriving digests. MODELPROV deliberately exports no word that turns text
\ back into an mprov. But do not read that as unforgeability: until the sealed
\ destructure / linear UNMAKE capability above lands, the generated
\ MODELPROV-MPROV:UNMAKE hands any holder of ONE real pin the proof token, and
\ CONTENT-KEY:HEX-NIB plus MODELPROV-MPROV:MAKE reassemble a pin around cells
\ decoded from arbitrary stored text - roughly twenty-five lines of ordinary
\ checked code outside this package, no trusted word required. So a pin proves
\ integrity against ACCIDENT - a mismatched artifact set, a stale manifest row, a
\ silently changed preimage format - and does NOT prove possession or resist a
\ deliberate forger who already holds a genuine pin. Any protocol that needs the
\ stronger property must add a signature over the rendering; it cannot get it
\ from this type. model-provenance-test.f pins the gap as a checker verdict that
\ ACCEPTS today, so the day the capability lands the suite fails and forces this
\ paragraph to be retired.
\
\ ONE PIN AT A TIME. The two scratch buffers are package-private and static (the
\ MDLCFG KBUF discipline), so PIN is not re-entrant; nothing calls it from inside
\ itself and no public word exposes them.
\
\ maki -> habu only. Owns -7635..-7639.

require lib/prelude.f
require lib/content-key.f
require lib/cad-num-arithmetic.f         \ CAD-NUM:byte-len, the validated path-length role
require maki/infer/model-types.f
require maki/infer/weight-store.f

package MODELPROV

public

\ ---- named rejection codes (one per validated input class) --------------------
-7635 constant E-PROV-TEXT      \ a label is empty, over LABEL-MAX, or not printable
-7636 constant E-PROV-HEX       \ a supplied digest is not exactly 64 lowercase hex characters
-7637 constant E-PROV-ARTIFACT  \ the tensor artifact could not be read and digested
-7638 constant E-PROV-DIGEST    \ the artifact's computed digest differs from the expected one
-7639 constant E-PROV-BUF       \ KEY-HEX given a buffer smaller than the 64-character rendering

\ ---- the packing convention the producing packer used ------------------------
ENUM packing as-stored transposed ;ENUM

\ ---- the private-mint proof (see header: arity-0 nominal, ART:built shape) ---
NEWTYPE prov-proof 0

\ ---- the pin: a 32-byte content digest, plus its proof -----------------------
\ The digest is its OWN nested structure with a DERIVED equality, and MPROV=
\ delegates to it. That is deliberate: a hand-written four-cell comparison can be
\ narrowed to three by an edit and still pass every test whose cases differ in
\ all four cells, so the comparison must not be hand-written at all. (DERIVE eq
\ only refuses when the proof field sits in the SAME structure, so nesting the
\ digest one level down buys the generated equality back.)
STRUCTURE provkey 0 DERIVE eq
   FIELD k0 n  FIELD k1 n  FIELD k2 n  FIELD k3 n
;STRUCTURE

STRUCTURE mprov 0
   FIELD key provkey
   FIELD tok prov-proof
;STRUCTURE

private

TRUSTED: MINT-PROV-PROOF ( -- prov-proof )  0 ;

\ Audited proof-erasure projection (the WSTORE BLEN>N discipline): the core path
\ primitive still consumes a bare cell, so the validated role is read back here.
\ Package-private with no public inverse, so a raw length cannot re-enter as a
\ role.
TRUSTED: PLEN>N ( CAD-NUM:byte-len -- n ) ;

1 constant SCHEMA-V            \ the first published mprov preimage version
64 constant HEX-LEN            \ characters in a rendered SHA-256 digest
32 constant KEY-BYTES          \ raw bytes in a SHA-256 digest
128 constant LABEL-MAX         \ longest accepted family / revision label
$21 constant PRINT-LO          \ '!' - lowest accepted label byte (excludes space)
$7E constant PRINT-HI          \ '~' - highest accepted label byte
$30 constant DIGIT-LO          \ '0'
$39 constant DIGIT-HI          \ '9'
$61 constant HEXA-LO           \ 'a'
$66 constant HEXA-HI           \ 'f'

create NSCR 8 allot            \ one-cell fold scratch (cell bits -> raw bytes)
create KBUF KEY-BYTES allot    \ CONTENT-KEY:FINAL landing pad, and KEY-HEX staging
create HEXBUF HEX-LEN allot    \ the tensor artifact's computed hexadecimal digest

\ ---- input validation --------------------------------------------------------
: PRINTABLE? ( n -- bool ) {: c:n :}
   c PRINT-LO >=  c PRINT-HI <=  and ;

: LABEL-OK ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0 <= if E-PROV-TEXT throw then
   u LABEL-MAX > if E-PROV-TEXT throw then
   u 0 ?do a i + c@ PRINTABLE? 0= if E-PROV-TEXT throw then loop ;

: HEX-DIGIT? ( n -- bool ) {: c:n :}
   c DIGIT-LO >=  c DIGIT-HI <=  and if true exit then
   c HEXA-LO >=  c HEXA-HI <=  and ;

\ Canonical digest text: exactly HEX-LEN characters, lower case only, so one
\ artifact cannot pin two ways and "unknown" or a truncated digest is refused.
: HEX-OK ( ptr u8 n -- ) {: a:ptr u:n :}
   u HEX-LEN <> if E-PROV-HEX throw then
   u 0 ?do a i + c@ HEX-DIGIT? 0= if E-PROV-HEX throw then loop ;

: CHECK-LABELS ( ptr u8 n ptr u8 n -- )
   LABEL-OK LABEL-OK ;

\ The two digests PIN only folds; the expected artifact digest is validated by
\ VERIFY-ARTIFACT, beside the comparison that consumes it.
: CHECK-DIGESTS ( ptr u8 n ptr u8 n -- )
   HEX-OK HEX-OK ;

\ ---- the one artifact PIN reads for itself -----------------------------------
\ Digests the tensor file and refuses unless the bytes on disk render to exactly
\ the expected text; HEXBUF then holds the digest the fold will use, so the key
\ describes bytes that were actually read.
\ The length is erased to a bare cell only here, one line before the primitive
\ that needs it; an over-long path is refused by that primitive's own named code
\ (src/core/util.f E-PATH-RANGE), which is a length no filesystem can open rather
\ than a provenance question.
: VERIFY-ARTIFACT ( ptr u8 CAD-NUM:byte-len ptr u8 n -- )
   {: pth:ptr pthu:CAD-NUM:byte-len wnt:ptr wntu:n :}
   wnt wntu HEX-OK
   pth pthu PLEN>N HEXBUF SHA256-FILE-HEX 0 <> if E-PROV-ARTIFACT throw then
   HEXBUF HEX-LEN wnt wntu STR= 0= if E-PROV-DIGEST throw then ;

\ ---- key preimage folds ------------------------------------------------------
\ A cell folds as its 8 raw little-endian bytes, exactly as MDLCFG:BUILD folds
\ mcfg's schema-version - one convention for a version row, not two.
: FOLD-N ( n -- )
   NSCR !  NSCR 8 CONTENT-KEY:TEXT+ ;

: FOLD-TEXTS ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- )
   {: fam:ptr famu:n rev:ptr revu:n cfg:ptr cfgu:n tkz:ptr tkzu:n :}
   fam famu CONTENT-KEY:TEXT+
   rev revu CONTENT-KEY:TEXT+
   cfg cfgu CONTENT-KEY:TEXT+
   tkz tkzu CONTENT-KEY:TEXT+ ;

\ Each variant folds its own canonical stable tag text: the pin travels between
\ processes and releases, so a runtime tag number must never reach the preimage.
: FOLD-ADAPTER ( MODEL:adapter -- )
   MATCH MODEL:adapter
      hf-gpt2 OF s" hf-gpt2" CONTENT-KEY:TEXT+ ENDOF
   ;MATCH ;

: FOLD-RESIDENCY ( WSTORE:residency -- )
   MATCH WSTORE:residency
      mapped    OF s" mapped" CONTENT-KEY:TEXT+ ENDOF
      allocated OF s" allocated" CONTENT-KEY:TEXT+ ENDOF
   ;MATCH ;

: FOLD-PACKING ( packing -- )
   MATCH packing
      as-stored  OF s" as-stored" CONTENT-KEY:TEXT+ ENDOF
      transposed OF s" transposed" CONTENT-KEY:TEXT+ ENDOF
   ;MATCH ;

\ Locals, not stack order: the arms must fold in the DECLARED order, and a bare
\ stack version would fold them backwards.
: FOLD-TAGS ( MODEL:adapter WSTORE:residency packing -- )
   {: ad:MODEL:adapter res:WSTORE:residency pk:packing :}
   ad FOLD-ADAPTER  res FOLD-RESIDENCY  pk FOLD-PACKING ;

\ The mint is the constructor's final act: nothing partially validated ever
\ carries a key.
: KEY-FINAL ( -- mprov )
   KBUF CONTENT-KEY:FINAL
   KBUF 0 cells + @  KBUF 1 cells + @  KBUF 2 cells + @  KBUF 3 cells + @
   MODELPROV-PROVKEY:MAKE
   MINT-PROV-PROOF MODELPROV-MPROV:MAKE ;

public

\ ---- the preimage format version a stored rendering was produced under -------
\ Read-only and argument-free: a manifest persists this beside the hexadecimal
\ so a later reader knows which preimage produced it, and still cannot choose it.
: SCHEMA ( -- n )
   SCHEMA-V ;

\ ---- the sole constructor ----------------------------------------------------
\ Arguments read in preimage order: the two labels, the configuration and
\ tokenizer digests, the tensor artifact's path and expected digest, then the
\ three typed convention values. Every refusal throws before CONTENT-KEY:RESET,
\ so a rejected call leaves no half-built preimage behind.
: PIN ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n ptr u8 CAD-NUM:byte-len ptr u8 n MODEL:adapter WSTORE:residency packing -- mprov )
   {: fam:ptr famu:n rev:ptr revu:n cfg:ptr cfgu:n tkz:ptr tkzu:n
      pth:ptr pthu:CAD-NUM:byte-len wnt:ptr wntu:n
      ad:MODEL:adapter res:WSTORE:residency pk:packing :}
   fam famu rev revu CHECK-LABELS
   cfg cfgu tkz tkzu CHECK-DIGESTS
   pth pthu wnt wntu VERIFY-ARTIFACT
   CONTENT-KEY:RESET
   SCHEMA-V FOLD-N
   fam famu rev revu cfg cfgu tkz tkzu FOLD-TEXTS
   HEXBUF HEX-LEN CONTENT-KEY:TEXT+
   ad res pk FOLD-TAGS
   KEY-FINAL ;

private

\ Read projection off a duplicated copy: mprov is non-linear, so the accessors
\ UNMAKE a copy, bind and drop the proof, and keep the digest.
: MP-KEY ( mprov -- provkey )
   MODELPROV-MPROV:UNMAKE {: tk:prov-proof :} ;

: MP-CELLS ( mprov -- n n n n )
   MP-KEY MODELPROV-PROVKEY:UNMAKE ;

public

\ ---- identity comparison (what a manifest compatibility check runs) ----------
\ The comparison itself is generated from the digest's field list, so no edit can
\ quietly narrow it to three cells.
: MPROV= ( mprov mprov -- bool )
   MP-KEY swap MP-KEY  MODELPROV-PROVKEY:EQ ;

\ ---- the canonical rendering a manifest stores -------------------------------
\ Writes exactly HEX-LEN characters into the caller's buffer and refuses a buffer
\ that cannot hold them. This package publishes no word that reads the rendering
\ back, which is a smaller claim than unforgeability - see the SERIALIZATION
\ paragraph in the header for the inverse that UNMAKE plus CONTENT-KEY:HEX-NIB
\ still allow, and what a pin does and does not prove because of it.
: KEY-HEX ( mprov ptr u8 n -- mprov ) {: dst:ptr cap:n :}
   cap HEX-LEN < if E-PROV-BUF throw then
   dup MP-CELLS {: a:n b:n c:n d:n :}
   a KBUF 0 cells + !  b KBUF 1 cells + !
   c KBUF 2 cells + !  d KBUF 3 cells + !
   KBUF dst SHA256>HEX ;

private
get-current prot-wid-add
public
get-current prot-wid-add
;package
