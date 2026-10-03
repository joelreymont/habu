\ internal-mark.f - seal source authority and record its call arity.
\ Dictionary reads use XREF; private engine boundaries apply the flags and
\ read checker registries before the runtime is sealed.
require src/core/prefix-boundary.f

package ENGINE-INTERNAL

\ THE PASS LEAVES NO RECORD NUMBER IN DATA: a record number is the host's
\ (docs/bootstrap.md, "A record number is the host's"). So the cursors are loop
\ indices, the prefix's first record is a local, and IMK-PASS retires the
\ REG-PROTECT registrations (src/core/util.f) once it has decided what to do
\ with them.

: IMK-REC ( n -- ptr n )
   XREF-REC ;

: IMK-FLAGS@ ( n -- n )
   IMK-REC XREF-FLAGS ;

: IMK-WID ( n -- n )
   IMK-REC XREF-WORDLIST ;

: IMK-NAME-A ( n -- ptr u8 )
   IMK-REC XREF-NAME-A ;

: IMK-NAME-U ( n -- n )
   IMK-FLAGS@ DNAME-LEN-MASK and ;

\ Definers carry an explicit dictionary kind, preserved by native publication
\ and capture. Every other non-namespace entry requires source authority;
\ neither a particular prologue nor an inferred native call shape grants it.
: IMK-EXECUTABLE? ( n -- bool )
   IMK-FLAGS@ DKIND:MASK and 0 = ;

: IMK-GLOBAL-EXECUTABLE? ( n -- bool )
   dup IMK-WID 0 = IF IMK-EXECUTABLE? ELSE drop 0 0= 0= THEN ;

TRUSTED: KNOWN-MIN-IN ( ptr u8 n -- n ) EFFECT-EXTERNAL-MIN-IN ;
TRUSTED: MARK-INTERNAL ( n -- ) int-mark ;
TRUSTED: MARK-MIN-IN ( n n -- ) min-in-mark ;
TRUSTED: PROTECTED-COUNT ( -- n ) REG-PROT-N @ ;
TRUSTED: PROTECTED-RECORD ( n -- n ) cells REG-PROT-IDX + @ ;
TRUSTED: PROTECTED-RETIRE ( -- )
   PROTECTED-COUNT 0 ?do 0 i cells REG-PROT-IDX + ! loop
   0 REG-PROT-N ! ;

: IMK-MIN-IN ( n -- n )
   dup IMK-NAME-A swap IMK-NAME-U KNOWN-MIN-IN ;

\ A GLOBAL RECORD NO TOP-LEVEL ROW TYPES MAY STILL BE A PACKAGE'S PRIMITIVE: a
\ package-private row (checker.f CLOSE-PRIVATE) types it for checked code inside
\ the owner, where the engine binds the name to this record. Marking it DNAME-INT
\ would refuse that caller too, so the record takes the owner's row instead, and
\ DNAME-INT stays the mark of a record no row types anywhere.
: IMK-OWNED-MIN-IN ( n -- n )
   dup IMK-NAME-A swap IMK-NAME-U EFFECT-OWNED-MIN-IN ;

: IMK-MARK ( n n -- ) {: i:n m:n :}   \ unknown -> DNAME-INT; known din>0 -> DNAME-MIN-IN
   m 0 < IF i MARK-INTERNAL EXIT THEN
   m 0 > IF i m MARK-MIN-IN THEN ;

: IMK-CLASSIFY ( n -- ) {: i:n :}   \ a global record, under its bare name
   i IMK-GLOBAL-EXECUTABLE? 0= IF EXIT THEN
   i IMK-MIN-IN dup 0 < IF drop i IMK-OWNED-MIN-IN THEN
   i swap IMK-MARK ;

: IMK-WALK ( n -- )          \ classify every source-prefix record, from the first
   ndict@ swap ?do i IMK-CLASSIFY loop ;

\ ---- package publics, under their qualified name ---------------------------
\ A package word carries its wordlist, not 0, so IMK-WALK above never reaches
\ one - and until dot habu-pkg-publics-escape-41532ee7 that left every package
\ public top-level executable whatever the checker knew: `0 0 SCHEMA-REG:REWIND`
\ wiped the schema registry and exited 0, the same unchecked execution class as
\ the bare U-TYPE this file was written for.
\
\ THE QUALIFIED SPELLING REACHES PUBLICS AND NOTHING ELSE, so publics are the
\ whole of the escape. habu1.f FIND-NMATCH resolves PKG:TAIL by taking the
\ search wid from the package row's [0] - the package's PUBLIC wordlist - and a
\ package PRIVATE word therefore has no top-level spelling at all: bare misses
\ it because it is not global, and qualified misses it because the qualifier
\ never names its wordlist. Nothing to mark, and test/internal-word-gate.f pins
\ that a private stays E-UNDEFINED under its own qualified name.
\
\ THE QUESTION IS THE SAME QUESTION, spelled the way the token is: source arity of
\ "PKG:TAIL". checker.f CHECKER-FIND-ACTIVE-SYM routes a qualified name straight
\ to CHECKER-PUBLIC-SYM? -> SYM-FIND (checker.f:4212) keyed on (package,
\ SYM-PUBLIC, tail), which is the key PPRIM; interns and the key the checker
\ itself uses at a reference site. So a package public the checker can type stays
\ callable only when that effect carries authority, by the same rule as a global.
\
\ WHY THE PACKAGE ROWS DRIVE THE LOOP rather than a wid -> package map: the row
\ IS the record that carries both halves of the answer, its public wordlist in
\ [0] and the package name in its own name field, so reading it once per package
\ needs no second table to fall out of step with the dictionary. The wid-0 arm
\ above decides first, so the two passes partition the records no matter what a
\ row's [0] holds. Each package's walk starts AT its own row because no earlier
\ record can be in its public wordlist: habu2.f C-PACKAGE-NEW-RECORD allocates
\ both wids and writes [0] as it publishes the row, so the id does not exist
\ before the row does. Measured on the boot prefix, that is 112k record tests
\ instead of 345k.
\
\ A namespace row carries DICT-WL:NAMESPACE, distinct from every allocated
\ wordlist id, so the inner walk cannot classify the namespace itself.
1024 constant IMK-QCAP        \ qualified-name scratch: package + ':' + tail
create IMK-QBUF IMK-QCAP allot
variable IMK-QU
variable IMK-QI

: IMK-Q+ ( ptr u8 n -- ) {: a:ptr u:n :}
   IMK-QU @ u + IMK-QCAP > IF
      s" internal-mark: qualified name exceeds scratch" 76 die THEN
   0 IMK-QI !
   BEGIN IMK-QI @ u < WHILE
      a IMK-QI @ + c@ IMK-QBUF IMK-QU @ + c!
      IMK-QU @ 1 + IMK-QU !
      IMK-QI @ 1 + IMK-QI !
   REPEAT ;

: IMK-QUAL ( n n -- ptr u8 n ) {: p:n i:n :}   \ package row, word record -> PKG:TAIL
   0 IMK-QU !
   p IMK-NAME-A p IMK-NAME-U IMK-Q+
   s" :" IMK-Q+
   i IMK-NAME-A i IMK-NAME-U IMK-Q+
   IMK-QBUF IMK-QU @ ;

: IMK-CLASSIFY-PUB ( n n -- ) {: p:n i:n :}
   i IMK-EXECUTABLE? 0= IF EXIT THEN
   i p i IMK-QUAL KNOWN-MIN-IN IMK-MARK ;

\ Package row p's public colon records, from p or the prefix's first record.
: IMK-PKG-PUBLICS ( n n -- ) {: p:n first:n :}
   p IMK-REC XREF-PKG-PUBLIC {: pub:n :}
   ndict@ p 1 + first max ?do
      i IMK-WID pub = IF p i IMK-CLASSIFY-PUB THEN
   loop ;

: IMK-WALK-PACKAGES ( n -- ) {: first:n :}   \ classify every package's public wordlist
   ndict@ 0 ?do
      i IMK-WID DICT-WL:NAMESPACE = IF i first IMK-PKG-PUBLICS THEN
   loop ;

: IMK-NAMED? ( n ptr u8 n -- bool ) {: i:n a:ptr u:n :}
   i IMK-NAME-U u = IF i IMK-NAME-A i IMK-NAME-U a u CORE-STR= ELSE 0 0= 0= THEN ;

: IMK-PRIM? ( n -- bool )    \ record n is one of the marking prims themselves
   dup s" int-mark" IMK-NAMED? IF drop 0 0= EXIT THEN
   s" min-in-mark" IMK-NAMED? ;

: IMK-SEAL-PRIM ( n -- )      \ close the loop: the marking prims are themselves internal
   0 ?do i IMK-PRIM? IF i MARK-INTERNAL THEN loop ;

\ Registry write-protection (dot habu-protect-type-field-04d91409). A din=0
\ registry control cell (variable/create) is a data record, so IMK-WALK exempts
\ it and its bare name stays executable — a bare `<cell> !` mutates the registry
\ past the public API. util.f's REG-PROTECT recorded each such cell's
\ dictionary index in REG-PROT-IDX[0, REG-PROT-N); int-mark them so interpret /
\ tick fail closed on the bare name exactly like a sig-less colon word, while the
\ core compiled callers (resolved before this pass) keep working.
: IMK-SEAL-REGISTRY ( -- )
   PROTECTED-COUNT 0 ?do i PROTECTED-RECORD MARK-INTERNAL loop ;

\ ---- the whitebox image ----------------------------------------------------
\ THE ONE BUILD THAT ASKS FOR NO SEAL. A whitebox suite reaches inside the
\ engine it tests - it reopens an engine package, names a pre-hook global, ticks
\ an internal word - and this pass is what closes every one of those doors in the
\ shipped image. Wrapping each probe in a TRUSTED: body does not reopen them
\ either: the seal gates INTERPRET and TICK, so the refusal lands on the bare
\ token whatever the body around it is.
\
\ So the honest whitebox host is an engine built without this pass, and the
\ builder asks for one here, the way src/core/top-row.f takes HABU_TOP_TIER:
\ test/whitebox-engine.f sets HABU_WHITEBOX_IMAGE=1 in the environment of the
\ ONE tools/native-build.f run whose image the gate hands to whitebox suites,
\ under its own content key and never over bin/hb. The variable is read at
\ target-load time and nowhere else, so no shipped engine can be unsealed after
\ the fact: an installed image carries the marks its build wrote, and
\ test/internal-word-gate.f keeps pinning that the product's are there.
: IMK-WHITEBOX? ( -- bool )
   s" HABU_WHITEBOX_IMAGE" GETENV s" 1" CORE-STR= ;

\ AND THE IMAGE SAYS WHICH IT IS. An environment variable is a request, not a
\ property: once the pass has answered it, what the image IS has to be readable
\ from the image, or a build that was never asked for a whitebox host cannot
\ tell that it made one. So the pass writes its own verdict into this cell, the
\ capture bakes the cell with the rest of DATA, and every later reader - the
\ build's own smoke run before it promotes the binary
\ (tools/native-build-core.f), test/whitebox-engine-suite.f on both engines -
\ asks the engine instead of the environment. The package seal the native build
\ applies at capture (SEAL-PACKAGES below) reads this same verdict.
\
\ A COLD ENGINE ANSWERS FOR ITS BOOT, not for its build, and that is the honest
\ answer for one: it reads this file from source every time it starts, so the
\ pass runs again and the cell is set again. Only a seeded image - the product,
\ and the whitebox host beside it - carries a verdict its build wrote.
0 constant IMAGE-SEALED         \ the pass ran: internal names are DNAME-INT
1 constant IMAGE-WHITEBOX       \ the pass stood down: they are ordinary words
variable IMK-CLASS

: IMK-SEAL ( -- )
   IMAGE-SEALED IMK-CLASS !
   CORE-PREFIX:FIRST-RECORD {: first:n :}
   first IMK-WALK
   first IMK-WALK-PACKAGES
   IMK-SEAL-REGISTRY
   first IMK-SEAL-PRIM ;

\ Either class ends the registrations: the seal has read them, and a whitebox
\ image has no seal to read them.
: IMK-PASS ( -- )
   IMK-WHITEBOX? IF IMAGE-WHITEBOX IMK-CLASS ! ELSE IMK-SEAL THEN
   PROTECTED-RETIRE ;

public

\ The two class values are defined above because IMK-PASS assigns them; they are
\ published here because IMAGE-CLASS's answer is meaningless without them.
EXPORT IMAGE-SEALED
EXPORT IMAGE-WHITEBOX

\ IMAGE-SEALED or IMAGE-WHITEBOX, as the pass that shaped this image left it.
: IMAGE-CLASS ( -- n )
   IMK-CLASS @ ;

\ Seal every package the capture ships: both wordlists of every namespace row at
\ or above `first` take the protected bit, so `package NAME` and a definition
\ into either wordlist exit ENGINE-ERROR:SEAL-PACKAGE, and the capture keeps
\ none of their private symbols (src/core/checker-surface.f KEEP?). IMK-PASS
\ cannot do this: it runs when this file loads, before the compiler, the REPL
\ and the manifest's own packages exist. So the native build calls this at its
\ capture with the window's first record (tools/native-build-core.f
\ PREPARE-TARGET), before NATIVE-RUNTIME:CAPTURE-PREPARE sweeps; an application
\ image (APP-IMAGE:SAVE) never calls it and keeps its packages reopenable. The
\ whitebox image stands down here as IMK-PASS did, on the verdict it wrote. A
\ package with no private wordlist holds 0 there, the global wordlist, which is
\ never protected: the capture refuses an image that marks it.
: SEAL-PACKAGES ( n -- )
   {: first:n :}
   IMK-CLASS @ IMAGE-WHITEBOX = IF EXIT THEN
   ndict@ first ?do
      i XREF-REC {: rec:ptr :}
      rec XREF-WORDLIST DICT-WL:NAMESPACE = IF
         rec XREF-PKG-PUBLIC prot-wid-add
         rec XREF-PKG-PRIVATE dup 0= IF drop ELSE prot-wid-add THEN
      THEN
   loop ;

private

get-current prot-wid-add
' IMK-PASS
;package
execute
