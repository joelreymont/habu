\ whitebox-key.f - the key and the name of the unsealed engine
\ test/whitebox-engine.f builds.
\
\ THE KEY IS WHAT THE IMAGE IS BUILT FROM: the engine binary that runs the
\ builder, and the builder's own ordered require/include closure - which is the
\ engine's whole boot prefix, because tools/native-build.f names every prefix
\ source it compiles. Both go into the content key below, exactly as
\ test/fixture-writer.f keys the writer image, so an edit anywhere in that
\ closure changes the key and no stale host is reused. Keying on the installed
\ binary alone made the artifact track bin/hb instead of the checkout: a tree
\ whose engine sources changed without a reinstall got a whitebox host built
\ from the older sources.
\
\ This is a module of its own so that a file can key the engine, or take its
\ build deadline, without reaching the image: the gate grants a row the keyed
\ images whose modules its load closure reaches (test/gate-images.f),
\ test/whitebox-engine-key-test.f keys a copied tree with ENTRY-PATH! and never
\ runs the engine, and the gate times its build row from BUILD-TIMEOUT-MS.

require lib/errors.f
require lib/string.f
require lib/fs.f
require lib/build-cache.f
require lib/content-key.f
require lib/engine-candidate.f
require tools/event-closure-lib.f

package WHITEBOX-KEY

64 constant KEY-HEX-LEN
128 constant NAME-CAP

create KEY-HEX KEY-HEX-LEN allot
create NAME-BUF NAME-CAP allot

variable NAME-U
variable CLOSURE-IDX

public

\ The builder this host is built from, and whose closure the key folds. Public
\ because test/whitebox-engine-key-test.f copies exactly this entry's closure: a
\ second spelling of the path there would go on keying the old entry after a
\ rename.
: BUILDER$ ( -- ptr u8 n )
   s" tools/native-build.f" ;

\ Every keyed image's name starts with this, which is also how
\ test/whitebox-engine.f's prune finds the family.
: IMAGE-PREFIX$ ( -- ptr u8 n )
   s" hb-whitebox-" ;

\ The builder's own deadline (test/whitebox-engine.f BUILD-RUN). It starts only
\ after the key is hashed, so a caller that builds the engine under a deadline
\ of its own gives that one a margin beyond this (test/gate-images.f
\ BUILD-ROW-TIMEOUT-MS): the inner deadline then governs, BUILD-RUN dies with
\ the builder's status, and the exit registry removes the work directory.
360000 constant BUILD-TIMEOUT-MS

private

\ Discovery rejects fail-closed, so a closure that cannot be reproduced cannot be
\ keyed - the key never silently covers fewer files than the build reads.
: CLOSURE-CK+ ( CONTENT-KEY:fold -- CONTENT-KEY:fold )
   0 CLOSURE-IDX !
   begin CLOSURE-IDX @ EC:COUNT < while
      CLOSURE-IDX @ EC:PATH$ CLOSURE-IDX @ EC:NAME$ CONTENT-KEY:FILE-NAMED+
      CLOSURE-IDX @ 1+ CLOSURE-IDX !
   repeat ;

: KEY! ( ptr u8 n -- ) {: a:ptr u:n :}
   a u EC:BUILD
   CONTENT-KEY:OPEN
   s" whitebox-engine-v3" CONTENT-KEY:TEXT+
   ENGINE-CANDIDATE:PATH$ s" host-engine" CONTENT-KEY:FILE-NAMED+
   CLOSURE-CK+
   KEY-HEX CONTENT-KEY:FINAL-HEX ;

: NAME! ( -- )
   IMAGE-PREFIX$ {: a:ptr u:n :}
   u KEY-HEX-LEN + NAME-CAP > if E-FS-CAPACITY throw then
   a NAME-BUF u BYTE-COPY
   KEY-HEX NAME-BUF u + KEY-HEX-LEN BYTE-COPY
   u KEY-HEX-LEN + NAME-U ! ;

public

\ The keyed artifact path a builder entry resolves to, written into the caller's
\ own buffer (FS-PATH-CAP bytes) and length cell. test/whitebox-engine.f names
\ the tree's builder; test/whitebox-engine-key-test.f names a copied tree's,
\ which is why the derivation takes both from the caller.
: ENTRY-PATH! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u:n dst:ptr up:ptr :}
   a u KEY!
   NAME!
   BUILD-CACHE:ROOT$ NAME-BUF NAME-U @ dst JOIN-PATH up ! ;

;package
