\ maki/db/keywire-xproc-child.f - the FRESH-PROCESS decode side of the cross-process
\ content-key identity test (dot habu-wire-content-key-e5efaa74, § 23.9 "Foreign
\ identity constructors and wire codecs").
\
\ CONCERN: the decode-in-a-fresh-process half of the decisive test. The parent
\ (maki/db/keywire-xproc-test.f) registers each migrated family's real descriptor,
\ KEY>WIRE's its 32-byte SHA-256 content key (8-byte rank for numeric-policy) into a
\ fixed-layout key file, and spawns a FRESH bin/hb that loads THIS file and calls
\ RESOLVE-ALL on the key file. Package KWXPC.
\
\ The point it proves: a content key emitted in one process resolves BY CONTENT to the
\ same descriptor in a fresh process, even though the fresh process assigns DIFFERENT
\ registry raw indices. SHIFT deliberately registers decoys first so every real gets a
\ raw index that does NOT match the parent's; if the wire form were the process-local
\ raw (the pre-migration form), WIRE>KEY would resolve to the wrong descriptor or out of
\ range. With the content key it resolves correctly, so identity survives process death.
\ The shared D-* descriptor words keep the parent and child content identical (equal
\ content -> equal content key), and RESOLVE-ALL prints XPROC-OK only if every family's
\ decoded identity projects back to the expected descriptor.

require lib/prelude.f
require lib/string.f
require lib/fs.f
require maki/schema.f
require maki/producer.f
require maki/config.f
require maki/rev.f
require maki/target/target.f
require maki/numpolicy.f
require maki/artifact.f

package KWXPC
public

\ ---- fixed key-file layout (shared by parent encode and child decode) ----------
32  constant KW-CK                     \ SHA-256 content-key width (six families)
8   constant KW-NP                     \ numeric-policy rank content-key width
0   constant O-SCHEMA
32  constant O-PRODUCER
64  constant O-CONFIG
96  constant O-REV
128 constant O-TARGET
160 constant O-NPOL
168 constant O-ARTIFACT
200 constant KW-TOTAL                   \ 32*6 + 8

\ ---- shared real descriptors (identical content -> identical content key) -------
: D-SCHEMA$   ( -- ptr u8 n )  s" xp-schema-def" ;
: D-PRODUCER$ ( -- ptr u8 n )  s" xp/producer" ;
: D-CONFIG$   ( -- ptr u8 n )  s" xp-cfg-facts" ;
: D-REV$      ( -- ptr u8 n )  s" xp-rev-content" ;
: D-ARTIFACT$ ( -- ptr u8 n )  s" xp-store-key" ;
\ Target uses the always-INIT-registered SM87 rather than a fresh descriptor: the target
\ registry is capped at TGT-CAP=16 and target-test.f fills it earlier in the maki suite,
\ so the parent cannot register a new target. SM87 is raw 0 in both processes, so target's
\ cross-process check proves content-key ROUND-TRIP (not order-independence, which the
\ six uncapped families demonstrate and target-test.f TT-CKEY-ALL proves in-process).
: D-TARGET-FACTS$ ( -- ptr u8 n )                  \ FACTS$ of TARGET:SM87
   s" isa=1|arch=87|warp=32|threads=1024|shared=49152|caps=127" ;
: D-NPOL-NAME$ ( -- ptr u8 n )  s" rel" ;   \ NPOL:NAME of NPOL-DOM:RELATIVE

\ ---- real-descriptor registrations (content-addressed; used by parent and child) --
: REG-SCHEMA   ( -- CAD-KIND:schema-id )          D-SCHEMA$ SCHEMA:REGISTER ;
: REG-PRODUCER ( -- CAD-KIND:producer-id )        D-PRODUCER$ PRODUCER:REGISTER ;
: REG-CONFIG   ( -- CAD-KIND:config-id )          D-CONFIG$ CONFIG:REGISTER ;
: REG-REV      ( -- CAD-KIND:rev-id )             D-REV$ REV:COMMIT ;
: REG-TARGET   ( -- CAD-KIND:target-id )          TARGET:SM87 ;
: REG-NPOL     ( -- CAD-KIND:numeric-policy-id )  NPOL-DOM:RELATIVE NPOL:REGISTER ;
: REG-ARTIFACT ( -- CAD-KIND:artifact-id )        D-ARTIFACT$ ARTIFACT:REGISTER ;

private

create KB 256 allot                     \ key file buffer (KW-TOTAL used)
variable OKF                             \ all-families-resolved flag

: FAIL! ( -- )   false OKF ! ;

\ SHIFT registers decoys first so the child's real ids get raw indices that DIFFER from
\ the parent's - the wedge that separates a content-key wire form from a raw-index one.
\ Target is excluded (it uses the fixed SM87, raw 0 in every process).
: SHIFT ( -- )
   s" xp-decoy-1" SCHEMA:REGISTER drop
   s" xp-decoy-2" SCHEMA:REGISTER drop
   s" xp-dp-1" PRODUCER:REGISTER drop
   s" xp-dc-1" CONFIG:REGISTER drop
   s" xp-dr-1" REV:COMMIT drop
   s" xp-da-1" ARTIFACT:REGISTER drop ;

: CK-SCHEMA ( -- )
   REG-SCHEMA drop
   KB O-SCHEMA + KW-CK SCHEMA:WIRE>KEY
   MATCH SCHEMA:id-result
      ok          OF SCHEMA:NAME$ D-SCHEMA$ STR= 0= if FAIL! then ENDOF
      wrong-width OF FAIL! ENDOF
      unknown     OF FAIL! ENDOF
   ;MATCH ;

: CK-PRODUCER ( -- )
   REG-PRODUCER drop
   KB O-PRODUCER + KW-CK PRODUCER:WIRE>KEY
   MATCH PRODUCER:id-result
      ok          OF PRODUCER:NAME$ D-PRODUCER$ STR= 0= if FAIL! then ENDOF
      wrong-width OF FAIL! ENDOF
      unknown     OF FAIL! ENDOF
   ;MATCH ;

: CK-CONFIG ( -- )
   REG-CONFIG drop
   KB O-CONFIG + KW-CK CONFIG:WIRE>KEY
   MATCH CONFIG:id-result
      ok          OF CONFIG:FACTS$ D-CONFIG$ STR= 0= if FAIL! then ENDOF
      wrong-width OF FAIL! ENDOF
      unknown     OF FAIL! ENDOF
   ;MATCH ;

: CK-REV ( -- )
   REG-REV drop
   KB O-REV + KW-CK REV:WIRE>KEY
   MATCH REV:id-result
      ok          OF REV:CONTENT$ D-REV$ STR= 0= if FAIL! then ENDOF
      wrong-width OF FAIL! ENDOF
      unknown     OF FAIL! ENDOF
   ;MATCH ;

: CK-TARGET ( -- )
   REG-TARGET drop
   KB O-TARGET + KW-CK TARGET:WIRE>KEY
   MATCH TARGET:id-result
      ok          OF TARGET:FACTS$ D-TARGET-FACTS$ STR= 0= if FAIL! then ENDOF
      wrong-width OF FAIL! ENDOF
      unknown     OF FAIL! ENDOF
   ;MATCH ;

: CK-NPOL ( -- )
   REG-NPOL drop
   KB O-NPOL + KW-NP NPOL:WIRE>KEY
   MATCH NPOL:id-result
      ok          OF NPOL:POLICY-DOM NPOL:NAME D-NPOL-NAME$ STR= 0= if FAIL! then ENDOF
      wrong-width OF FAIL! ENDOF
      unknown     OF FAIL! ENDOF
   ;MATCH ;

: CK-ARTIFACT ( -- )
   REG-ARTIFACT drop
   KB O-ARTIFACT + KW-CK ARTIFACT:WIRE>KEY
   MATCH ARTIFACT:id-result
      ok          OF ARTIFACT:KEY$ D-ARTIFACT$ STR= 0= if FAIL! then ENDOF
      wrong-width OF FAIL! ENDOF
      unknown     OF FAIL! ENDOF
   ;MATCH ;

public

\ RESOLVE-ALL reads the parent's fixed-layout key file, shifts the registries so the
\ real ids land at different raws than the parent, decodes every family's content key
\ BY CONTENT, and prints XPROC-OK iff all decoded identities project to their expected
\ descriptor (else XPROC-FAIL). The printed token is the parent's captured verdict.
: RESOLVE-ALL ( ptr u8 n -- ) {: p:ptr u:n :}
   true OKF !
   p u KB 256 READ-ALL {: got:n :}
   got KW-TOTAL <> if FAIL! then
   got KW-TOTAL = if
      SHIFT
      CK-SCHEMA CK-PRODUCER CK-CONFIG CK-REV CK-TARGET CK-NPOL CK-ARTIFACT
   then
   OKF @ if s" XPROC-OK" else s" XPROC-FAIL" then type ;

;package
