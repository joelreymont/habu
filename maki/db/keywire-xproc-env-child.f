\ maki/db/keywire-xproc-env-child.f - the FRESH-PROCESS decode side of the cross-process
\ ENVELOPE + TRANSACTION identity test (dot habu-wire-content-key-e5efaa74, § 23.9;
\ the "encode in one process image, decode in a fresh one" requirement applied to the
\ composite codecs, not just the per-family KEY>WIRE surfaces).
\
\ CONCERN: the decode-in-a-fresh-process half of the DECISIVE composite proof. The parent
\ (maki/db/keywire-xproc-env-test.f) BUILDs an artifact envelope (self artifact-id +
\ dependency set + the four 32-byte foreign ids + a source-revision + the 8-byte event) and
\ a transaction (base revision + write/read/dep), ENCODEs both to a fixed-layout file, and
\ spawns a FRESH bin/hb that loads THIS file and calls RESOLVE-ENV on it. Package KWXPC-ENV.
\
\ The point it proves: an ENVELOPE (and a transaction's base revision) encoded in one
\ process DECODEs correctly in a fresh process even though the fresh process assigns
\ DIFFERENT registry raw indices. SHIFT registers decoys FIRST so every real content-key id
\ (schema/producer/config/rev/artifact) gets a raw that does NOT match the parent's; were
\ the wire form a process-local raw, DECODE would resolve the wrong id or go
\ out of range and fail. With the 32-byte content key every field resolves BY CONTENT, so
\ envelope identity (and the transaction Merkle base) survives process death.
\
\ The audit-event is the ONE non-content-key field (an 8-byte occurrence SEQUENCE, § 23.9),
\ so it does NOT survive by content: the child reproduces the journal DEPTH the parent had
\ (append the parent-recorded event sequence + 1 filler events) so the recorded raw is in
\ range. This is the flagged journal replica-identity limitation made concrete: content keys
\ survive by content, the sequence event survives only by deterministic replay of append
\ order. Target uses SM87 (raw 0 in every process); its content key round-trips.

require lib/prelude.f
require lib/string.f
require lib/fs.f
require maki/schema.f
require maki/producer.f
require maki/config.f
require maki/rev.f
require maki/target/target.f
require maki/numpolicy.f
require maki/journal.f
require maki/artifact.f
require maki/db/artifact.f            \ ARTIFACT:DECODE-WEIGHT + art-result (envelope codec)
require maki/db/transaction.f         \ TX:DECODE + TX:result + BASE-REV (transaction codec)

package KWXPC-ENV
public

\ ---- shared descriptors (identical content -> identical content key) -----------
: D-SCHEMA$   ( -- ptr u8 n )  s" xpe-schema-def" ;
: D-PRODUCER$ ( -- ptr u8 n )  s" xpe/producer" ;
: D-CONFIG$   ( -- ptr u8 n )  s" xpe-cfg-facts" ;
: D-ID$       ( -- ptr u8 n )  s" xpe-self-key" ;
: D-DEP1$     ( -- ptr u8 n )  s" xpe-dep-1" ;
: D-DEP2$     ( -- ptr u8 n )  s" xpe-dep-2" ;
: D-REV$      ( -- ptr u8 n )  s" xpe-base-rev-content" ;
: D-SREV$     ( -- ptr u8 n )  s" xpe-source-rev-content" ;
: D-TXR$      ( -- ptr u8 n )  s" xpe-txn-read-obj" ;
: D-TXW$      ( -- ptr u8 n )  s" xpe-txn-write-obj" ;

\ ---- registrations (content-addressed; used by parent and child) ---------------
: REG-SCHEMA   ( -- CAD-KIND:schema-id )          D-SCHEMA$ SCHEMA:REGISTER ;
: REG-PRODUCER ( -- CAD-KIND:producer-id )        D-PRODUCER$ PRODUCER:REGISTER ;
: REG-CONFIG   ( -- CAD-KIND:config-id )          D-CONFIG$ CONFIG:REGISTER ;
: REG-NPOL     ( -- CAD-KIND:numeric-policy-id )  NPOL-DOM:RELATIVE NPOL:REGISTER ;
: REG-TARGET   ( -- CAD-KIND:target-id )          TARGET:SM87 ;
: REG-ID       ( -- CAD-KIND:artifact-id )        D-ID$ ARTIFACT:REGISTER ;
: REG-DEP1     ( -- CAD-KIND:artifact-id )        D-DEP1$ ARTIFACT:REGISTER ;
: REG-DEP2     ( -- CAD-KIND:artifact-id )        D-DEP2$ ARTIFACT:REGISTER ;
: REG-REV      ( -- CAD-KIND:rev-id )             D-REV$ REV:COMMIT ;
: REG-SREV     ( -- CAD-KIND:rev-id )             D-SREV$ REV:COMMIT ;
: REG-TXR      ( -- CAD-KIND:artifact-id )        D-TXR$ ARTIFACT:REGISTER ;
: REG-TXW      ( -- CAD-KIND:artifact-id )        D-TXW$ ARTIFACT:REGISTER ;

\ REG-ALL interns every content-addressed identity the encoded envelope + transaction
\ reference (so their content keys resolve BY CONTENT). Transaction read/write objects are
\ minted from the wire by ARTIFACT:REGISTER during DECODE, so they need no pre-registration.
: REG-ALL ( -- )
   REG-SCHEMA drop  REG-PRODUCER drop  REG-CONFIG drop  REG-NPOL drop
   REG-ID drop  REG-DEP1 drop  REG-DEP2 drop  REG-SREV drop  REG-REV drop ;

private

\ SHIFT registers decoys first so the child's real ids get raw indices that DIFFER from the
\ parent's - the wedge that separates a content-key wire form from a raw-index one. Target
\ is excluded (fixed SM87, raw 0 everywhere); the journal is reproduced by depth, not shift.
: SHIFT ( -- )
   s" xpe-decoy-s1" SCHEMA:REGISTER drop
   s" xpe-decoy-s2" SCHEMA:REGISTER drop
   s" xpe-decoy-p1" PRODUCER:REGISTER drop
   s" xpe-decoy-c1" CONFIG:REGISTER drop
   s" xpe-decoy-r1" REV:COMMIT drop
   s" xpe-decoy-r2" REV:COMMIT drop
   s" xpe-decoy-a1" ARTIFACT:REGISTER drop
   s" xpe-decoy-a2" ARTIFACT:REGISTER drop ;

\ ---- fixed key-file layout: [seq u32][env-len u32][env..][txn-len u32][txn..] ---
4 constant LW                              \ length / sequence field width (u32 LE)
create FB $2000 allot                       \ file buffer
variable FC                                 \ parse cursor into FB
variable OKF

: FAIL! ( -- )   false OKF ! ;

: LE32@ ( ptr u8 -- n ) {: a:ptr :}
   a c@  a 1+ c@ 8 lshift or  a 2 + c@ 16 lshift or  a 3 + c@ 24 lshift or ;

: TAKE32 ( -- n )                           \ read a u32 at the cursor and advance
   FB FC @ + LE32@  FC @ LW + FC ! ;

: FILL-JOURNAL ( n -- ) {: seq:n :}         \ append seq+1 filler events so raw `seq` is in range
   0 begin dup seq <= while
      s" xpe-journal-filler" JOURNAL:APPEND drop
      1+
   repeat drop ;

: ENV-OK? ( art-result<n> -- bool )         \ decode ok -> true, any reject -> false
   MATCH ARTIFACT:art-result
      ok OF drop true ENDOF
      malformed OF false ENDOF
      noncanonical OF false ENDOF
      bounds OF false ENDOF
      duplicate OF false ENDOF
      unknown-required OF false ENDOF
      kind-mismatch OF false ENDOF
      digest-mismatch OF false ENDOF
   ;MATCH ;

\ The effect must name TX:result QUALIFIED. The old tail `tx-result` was unique in the
\ repository, so a bare token reached it by the "sole eligible public row in another
\ package" rule; the new tail is shared with the global result family in
\ lib/adt/result.f, which the global row rule reaches FIRST. A bare `result` here
\ would silently mean that arity-2 family instead of this one.
: TXN-BASE-OK? ( TX:result<n> -- bool )  \ decode ok AND base rev resolves to REG-REV by content
   MATCH TX:result
      ok OF TX:TXN-OF TX:BASE-REV  REG-REV  REV:EQUAL? ENDOF
      duplicate-write OF false ENDOF
      omitted-read OF false ENDOF
      malformed OF false ENDOF
      bounds OF false ENDOF
   ;MATCH ;

public

\ RESOLVE-ENV reads the parent's fixed-layout blob, reproduces the journal depth, shifts the
\ content-key registries so the reals land at different raws than the parent, registers the
\ real descriptors, then DECODEs the envelope and the transaction. It prints XPROC-OK iff the
\ envelope decodes ok AND the transaction decodes ok with its base revision resolving to the
\ same content (proving both survived process death).
: RESOLVE-ENV ( ptr u8 n -- ) {: p:ptr u:n :}
   true OKF !
   p u FB $2000 READ-ALL {: got:n :}
   0 FC !
   TAKE32 {: seq:n :}
   seq FILL-JOURNAL
   SHIFT
   REG-ALL
   TAKE32 {: envlen:n :}
   FB FC @ +  {: envp:ptr :}
   FC @ envlen + FC !
   envp envlen ARTIFACT:DECODE-WEIGHT ENV-OK? 0= if FAIL! then
   TAKE32 {: txnlen:n :}
   FB FC @ +  {: txnp:ptr :}
   FC @ txnlen + FC !
   txnp txnlen TX:DECODE TXN-BASE-OK? 0= if FAIL! then
   OKF @ if s" XPROC-OK" else s" XPROC-FAIL" then type ;

;package
