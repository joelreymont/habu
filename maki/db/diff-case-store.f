\ maki/db/diff-case-store.f - the DURABLE per-case output store for the differential
\ runner (MODEL-CAD-V2-PLAN.md § "Automatic differential verification": "store every
\ input/output/environment"; dot habu-v2-differential-runner-13359019).
\
\ CONCERN: durable, crash-safe, content-keyed PUBLICATION of ONE executed differential
\ case's full record - its content-addressed input identity, its subject and reference
\ OUTPUTS, its classified OUTCOME, and its environment digest - so every case survives
\ process death and a FRESH process rehydrates it and byte-matches. This is a DIFFERENT
\ concern from maki/db/diff-runner.f (the in-memory RUN with its bounded LOG-CAP=64
\ first-slice run-log, an in-process view) and from maki/db/diff-suite-id.f (the interned
\ suite IDENTITY): the run-log stays the in-memory view; THIS file is the durable backing
\ where storing is TOTAL per case (unbounded file-per-case; no silent LOG-CAP truncation)
\ or fails loudly (named throw). Package CASESTORE.
\
\ ---- KEY COMPOSITION (exactly the landed EMIT-EVIDENCE keying + a per-case id) ---------
\ The record's leading 128-byte DESCRIPTOR is
\   subject-key(32) || suite-digest(32) || environment-key(32) || case-id(32)
\ built from the SAME cross-process content-key words the runner's success evidence uses -
\ PRODUCER:KEY>WIRE (subject), DIFFSUITE:DIGEST-INTO (suite), CONFIG:KEY>WIRE (environment
\ digest) - plus DIFFSUITE:CASE-ID (the deterministic content-addressed input id the
\ MINIMIZE distinct-CASE-ID mechanism already mints). Every component is content-derived and
\ registration-order-independent, so a fresh process that rebuilds the identical suite /
\ environment / subject derives a BYTE-IDENTICAL store key and finds the record (the
\ maki/db/keywire-xproc content-key-survives-process-death property). The STORE KEY is
\ SHA-256(descriptor); the file is <root>/cases/<hex(store-key)>.
\
\ ---- THE RECORD (a fixed-width byte-stable envelope) -----------------------------------
\   subj-key(32) suite-digest(32) env-key(32) case-id(32) param(8)
\   subj-kind(1) subj-val(8) ref-kind(1) ref-val(8) verdict(1)      REC-W = 155
\ subj-kind 0 produced / 1 faulted (subj-val = the produced scalar, 0 on fault); ref-kind
\ 0 value / 1 skip (ref-val = the reference scalar, 0 on skip); verdict is the runner's
\ classified DIFFRUN:CASE-VERDICT>N ordinal (0 agree / 1 mismatch / 2 subject-fault /
\ 3 reference-skip). All scalars are fixed little-endian, so the wire form is length-free and
\ byte-identical across processes.
\
\ ---- CRASH-SAFE WRITE (the commit-store temp+rename idiom) -----------------------------
\ PUT builds the whole fixed record, derives the content key, and publishes with ATOMIC-WRITE-
\ FILE (lib/fs-mutate.f temp+rename): a reader ever sees the file absent or the COMPLETE
\ record, never a torn value. Content-addressing makes a re-PUT byte-identical (idempotent).
\ DURABILITY BOUNDARY (shared with maki/db/commit-store.f, dotted there): rename gives
\ atomicity + correct PROCESS-crash recovery; a native fsync/dir-sync primitive for power-loss
\ durability is the remaining capability.
\
\ ---- LOAD (fresh-process rehydration + integrity) --------------------------------------
\ LOAD rebuilds the descriptor from (suite, environment, case), re-derives the store key, reads
\ the file, and confirms the file's embedded descriptor equals the rebuilt one (the content-path
\ integrity check: the file at this key really is this case's record) - returning the typed
\ load-result (ok<slot> / absent / malformed / mismatch), never a value+flag sentinel. RECORD-INTO
\ single-sources the exact record bytes so a caller can byte-match a loaded REC@ against a
\ re-derived expected record (the keywire-xproc decisive byte-match).
\
\ Typed domain outcomes are the bespoke load-result sum; throws are reserved for root/width
\ boundaries. maki -> habu only; diff-case-store owns -5415..-5416.

require lib/prelude.f
require lib/string.f
require lib/fs.f
require lib/fs-mutate.f
require maki/cad-kinds.f
require maki/producer.f            \ CAD-KIND:producer-id KEY>WIRE (subject content key)
require maki/config.f              \ CAD-KIND:config-id KEY>WIRE (environment digest / key)
require maki/db/diff-suite.f       \ DIFFSUITE:suite DIGEST-INTO / SUBJECT@ / CASE-ID
require maki/db/diff-runner.f      \ DIFFRUN run-result / ref-result / case-verdict + CASE-VERDICT>N

-5415 constant E-CASESTORE-ROOT    \ resolved store root path empty or over the path cap
-5416 constant E-CASESTORE-KEYW    \ an owner content-key width != CKW (fail-closed descriptor-layout guard)

package CASESTORE
public

\ Typed LOAD outcome: ok carries the pooled record slot in its `slot` field; absent is no
\ durable file; malformed is a present file of the wrong size; mismatch is a present file whose
\ embedded descriptor does not match the looked-up case (content-path corruption). A bespoke
\ per-package sum, not result<a,b>.
\
\ Declared through the unified ENUM front end in full mode (the arity after the name selects
\ it), so the payload is a named FIELD rather than a positional one. The name is this file's
\ own: the LOAD contract above writes the outcome as `ok<slot>`, the pool is addressed by slot
\ throughout (SLOT-ALLOC, LREC-AT, LOAD-CAP "record slots"), and the whole rehydrated-read
\ surface below binds that value as the slot. The generated CASESTORE-LOAD--RESULT:OK /
\ :ABSENT / :MALFORMED / :MISMATCH constructors and every MATCH site are unchanged, because
\ both the spellings and the payload binding order derive from the package, the family tail
\ and the declaration order - none of which the mode touches.
ENUM load-result 1
   VARIANT ok FIELD slot a ;VARIANT
   VARIANT absent ;VARIANT
   VARIANT malformed ;VARIANT
   VARIANT mismatch ;VARIANT
;ENUM

private

\ ---- readable wrappers over the generated constructor spellings ----------------
: LR-OK ( n -- load-result<n> )     CASESTORE-LOAD--RESULT:OK ;
: LR-ABSENT ( -- load-result<n> )   CASESTORE-LOAD--RESULT:ABSENT ;
: LR-MALFORMED ( -- load-result<n> ) CASESTORE-LOAD--RESULT:MALFORMED ;
: LR-MISMATCH ( -- load-result<n> ) CASESTORE-LOAD--RESULT:MISMATCH ;

\ ---- fixed widths + record layout ----------------------------------------------
32 constant CKW                    \ content-key / digest width (owner KEY>WIRE cross-process form)
8  constant U64W                   \ fixed little-endian scalar width
64 constant HEX-W                  \ hex rendering of a 32-byte key
8  constant LOAD-CAP               \ concurrently-rehydrated record slots (ring reuse)

0   constant OFF-SUBJ              \ subject producer content key
32  constant OFF-SUITE             \ suite content digest
64  constant OFF-ENV               \ environment config content key (the environment digest)
96  constant OFF-CASE              \ deterministic content-addressed case-id
128 constant DESC-W                \ descriptor width (the store-key preimage)
128 constant OFF-PARAM             \ case parameter n (u64)
136 constant OFF-SKIND             \ subject run-result kind (0 produced / 1 faulted)
137 constant OFF-SVAL              \ subject produced scalar (u64; 0 on fault)
145 constant OFF-RKIND             \ reference ref-result kind (0 value / 1 skip)
146 constant OFF-RVAL              \ reference scalar (u64; 0 on skip)
154 constant OFF-VERD              \ classified verdict ordinal (CASE-VERDICT>N)
155 constant REC-W                 \ whole fixed record

\ ---- store buffers (all fixed; only lengths live in variables) -----------------
create ROOT-BUF FS-PATH-CAP allot   variable ROOT-U
create SUB-BUF  FS-PATH-CAP allot   variable SUB-U
create PATH-BUF FS-PATH-CAP allot   variable PATH-U
create HEXBUF   HEX-W allot
create KEYBUF   CKW allot                     \ scratch: a case's store key (SHA-256 of the descriptor)
create RECBUF   REC-W allot                    \ PUT scratch: one whole record
create DBUF     DESC-W allot                    \ LOAD scratch: rebuilt descriptor
create LREC     LOAD-CAP REC-W * allot          \ pooled rehydrated records
variable LOAD-NEXT                              \ ring cursor
variable ROOT-SET                               \ -1 once the root is pinned/resolved

\ ---- fixed little-endian scalar codec (the maki/db/artifact.f pattern) ---------
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

: MEM= ( ptr u8 ptr u8 n -- bool ) {: pa:ptr pb:ptr n:n :}
   0 begin dup n < while
      dup {: k:n :}
      pa k + c@  pb k + c@  <> if drop false exit then
      1+
   repeat drop true ;

\ ---- store root: HABU_DIFF_CASE_STORE or a private default ---------------------
: DEFAULT$ ( -- ptr u8 n )   s" tmp/diff-case-store" ;

: ENV$ ( -- ptr u8 n )
   s" HABU_DIFF_CASE_STORE" GETENV dup 0 > if exit then
   2drop DEFAULT$ ;

: ROOT-SET! ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0 <= u FS-PATH-CAP > or if E-CASESTORE-ROOT throw then
   a ROOT-BUF u BYTE-COPY  u ROOT-U !  true ROOT-SET ! ;

: ROOT-RESOLVE ( -- )
   ROOT-SET @ if exit then
   ENV$ ROOT-SET! ;

public
\ ROOT! pins the store root (a test / fresh-process child driver points it at a private dir).
: ROOT! ( ptr u8 n -- )   ROOT-SET! ;

\ ROOT$ resolves (env or default, once) and returns the store root path.
: ROOT$ ( -- ptr u8 n )   ROOT-RESOLVE ROOT-BUF ROOT-U @ ;

private
\ ---- path builders (distinct buffers so one path survives building the next) ---
: SUB$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}      \ <root>/<name> -> SUB-BUF
   ROOT$ a u SUB-BUF JOIN-PATH SUB-U !
   SUB-BUF SUB-U @ ;

: CASE-PATH$ ( ptr u8 -- ptr u8 n ) {: kp:ptr :}     \ <root>/cases/<hex(store-key)> -> PATH-BUF
   kp HEXBUF SHA256>HEX
   s" cases" SUB$ HEXBUF HEX-W PATH-BUF JOIN-PATH PATH-U !
   PATH-BUF PATH-U @ ;

: ENSURE-DIRS ( -- )
   ROOT$ MAKE-DIRS
   s" cases" SUB$ MAKE-DIRS ;

\ ---- descriptor: subject-key || suite-digest || env-key || case-id -------------
\ Each owner content-key width is guarded == CKW so a future wider key fails closed rather
\ than truncating into the fixed descriptor layout (the artifact.f DEP+ width-guard rule).
: DESC-INTO ( DIFFSUITE:suite CAD-KIND:config-id n ptr u8 -- )
   {: h:DIFFSUITE:suite env:CAD-KIND:config-id k:n out:ptr :}
   h DIFFSUITE:SUBJECT@  out OFF-SUBJ +   CKW PRODUCER:KEY>WIRE   CKW <> if E-CASESTORE-KEYW throw then
   h                     out OFF-SUITE +  CKW DIFFSUITE:DIGEST-INTO CKW <> if E-CASESTORE-KEYW throw then
   env                   out OFF-ENV +    CKW CONFIG:KEY>WIRE      CKW <> if E-CASESTORE-KEYW throw then
   h k                   out OFF-CASE +   DIFFSUITE:CASE-ID ;

\ ---- outcome lowering (the runner's typed sums -> byte-stable scalars) ---------
\ The SUMs are MATCHed straight off the stack (a qualified sum type is a valid stack-comment
\ input type, but not a {: :} local annotation), so no sum value is ever bound as a local.
: LOWER-SUBJ ( DIFFRUN:run-result -- n n )         \ kind (0 produced / 1 faulted), value
   MATCH DIFFRUN:run-result
      produced OF {: v:n :}  0 v  ENDOF
      faulted  OF  1 0  ENDOF
   ;MATCH ;

: LOWER-REF ( DIFFRUN:ref-result -- n n )          \ kind (0 value / 1 skip), value
   MATCH DIFFRUN:ref-result
      value OF {: v:n :}  0 v  ENDOF
      skip  OF  1 0  ENDOF
   ;MATCH ;

\ ---- lowered-outcome scratch (a single-writer builder, the artifact.f cursor idiom) --
variable OSK  variable OSV                     \ subject kind / value
variable ORK  variable ORV                     \ reference kind / value
variable OVO                                   \ verdict ordinal

\ STASH-OUTCOME consumes the runner's three typed result sums (top of stack: case-verdict,
\ then ref-result, then run-result) and stashes their byte-stable scalars.
: STASH-OUTCOME ( DIFFRUN:run-result DIFFRUN:ref-result DIFFRUN:case-verdict -- )
   DIFFRUN:CASE-VERDICT>N OVO !
   LOWER-REF  ORV !  ORK !
   LOWER-SUBJ OSV !  OSK ! ;

\ FILL-RECORD writes the whole fixed record into out from (suite, env, case) + the stashed
\ outcome scalars. STASH-OUTCOME must have run for this case first.
: FILL-RECORD ( DIFFSUITE:suite CAD-KIND:config-id n ptr u8 -- )
   {: h:DIFFSUITE:suite env:CAD-KIND:config-id k:n out:ptr :}
   h env k out DESC-INTO
   k     out OFF-PARAM + U64W LE-PUT
   OSK @ out OFF-SKIND + c!
   OSV @ out OFF-SVAL + U64W LE-PUT
   ORK @ out OFF-RKIND + c!
   ORV @ out OFF-RVAL + U64W LE-PUT
   OVO @ out OFF-VERD + c! ;

public

\ RECORD-INTO builds the whole fixed record into a caller buffer (>= REC-W). It single-sources
\ the exact byte layout so PUT and a fresh-process byte-match build identical bytes.
: RECORD-INTO ( DIFFSUITE:suite CAD-KIND:config-id n DIFFRUN:run-result DIFFRUN:ref-result DIFFRUN:case-verdict ptr u8 -- )
   {: out:ptr :}                                 \ grab the buffer (TOS); non-sum, bindable
   STASH-OUTCOME                                 \ consume the three result sums (now TOS)
   {: h:DIFFSUITE:suite env:CAD-KIND:config-id k:n :}
   h env k out FILL-RECORD ;

\ REC-WIDTH is the fixed record byte width (for a caller's byte-match buffer).
: REC-WIDTH ( -- n )   REC-W ;

private
\ STORE-KEY-INTO derives a case's 32-byte store key = SHA-256(descriptor) into KEYBUF.
: STORE-KEY-INTO ( DIFFSUITE:suite CAD-KIND:config-id n ptr u8 -- )
   {: h:DIFFSUITE:suite env:CAD-KIND:config-id k:n desc:ptr :}
   h env k desc DESC-INTO
   desc DESC-W KEYBUF SHA256 ;

public

\ PUT durably publishes one executed case's record, keyed by its descriptor's content key.
\ Crash-safe (temp+rename); total (writes the whole fixed record) or throws.
: PUT ( DIFFSUITE:suite CAD-KIND:config-id n DIFFRUN:run-result DIFFRUN:ref-result DIFFRUN:case-verdict -- )
   STASH-OUTCOME                                 \ consume the three result sums (TOS)
   {: h:DIFFSUITE:suite env:CAD-KIND:config-id k:n :}
   ENSURE-DIRS
   h env k RECBUF FILL-RECORD
   RECBUF DESC-W KEYBUF SHA256                    \ store key = SHA-256(descriptor)
   KEYBUF CASE-PATH$ {: pa:ptr pu:n :}
   pa pu RECBUF REC-W ATOMIC-WRITE-FILE ;

\ HAS? is true once a durable record file exists for (suite, environment, case).
: HAS? ( DIFFSUITE:suite CAD-KIND:config-id n -- bool )
   {: h:DIFFSUITE:suite env:CAD-KIND:config-id k:n :}
   h env k DBUF STORE-KEY-INTO
   KEYBUF CASE-PATH$ FILE? ;

\ PATH$ is the content-addressed absolute path of a case's durable record (valid until the
\ next path build). The path is a pure function of (suite, environment, case) identity.
: PATH$ ( DIFFSUITE:suite CAD-KIND:config-id n -- ptr u8 n )
   {: h:DIFFSUITE:suite env:CAD-KIND:config-id k:n :}
   h env k DBUF STORE-KEY-INTO
   KEYBUF CASE-PATH$ ;

private
: SLOT-ALLOC ( -- n )   LOAD-NEXT @  dup 1+ LOAD-CAP mod LOAD-NEXT ! ;
: LREC-AT ( n -- ptr u8 )   REC-W * LREC + ;

public

\ LOAD rehydrates a case's record from disk into a pooled slot, verifying the file's embedded
\ descriptor matches the looked-up case (content-path integrity). Fresh-process safe: the store
\ key is content-derived, so a rebuilt suite/environment/subject resolves the same file.
: LOAD ( DIFFSUITE:suite CAD-KIND:config-id n -- load-result<n> )
   {: h:DIFFSUITE:suite env:CAD-KIND:config-id k:n :}
   h env k DBUF STORE-KEY-INTO
   KEYBUF CASE-PATH$ {: pa:ptr pu:n :}
   pa pu FILE? 0= if LR-ABSENT exit then
   pa pu FILE-SIZE REC-W <> if LR-MALFORMED exit then
   SLOT-ALLOC {: s:n :}
   pa pu  s LREC-AT REC-W READ-ALL  REC-W <> if LR-MALFORMED exit then
   s LREC-AT DBUF DESC-W MEM= 0= if LR-MISMATCH exit then
   s LR-OK ;

\ ---- rehydrated-slot read surface ----------------------------------------------
: PARAM@ ( n -- n ) {: s:n :}       s LREC-AT OFF-PARAM + U64W LE-GET ;
: SUBJ-KIND@ ( n -- n ) {: s:n :}   s LREC-AT OFF-SKIND + c@ ;
: SUBJ-VAL@ ( n -- n ) {: s:n :}    s LREC-AT OFF-SVAL + U64W LE-GET ;
: REF-KIND@ ( n -- n ) {: s:n :}    s LREC-AT OFF-RKIND + c@ ;
: REF-VAL@ ( n -- n ) {: s:n :}     s LREC-AT OFF-RVAL + U64W LE-GET ;
: VERDICT@ ( n -- n ) {: s:n :}     s LREC-AT OFF-VERD + c@ ;
: REC@ ( n -- ptr u8 n ) {: s:n :}  s LREC-AT REC-W ;

\ ---- test-only store lifecycle -------------------------------------------------
\ ROOT+ resolves and CREATES the store root + cases dir, returning the root path.
: ROOT+ ( -- ptr u8 n )   ENSURE-DIRS ROOT$ ;

\ RESET removes the whole store tree (test-only); a missing store is a no-op.
: RESET ( -- )
   ROOT$ {: a:ptr u:n :}
   u 0 > if a u EXISTS? if a u REMOVE-TREE then then ;

private
: CASESTORE-INIT ( -- )   0 LOAD-NEXT ! ;
CASESTORE-INIT
;package
