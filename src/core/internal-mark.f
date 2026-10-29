\ internal-mark.f - seal source authority and record its call arity.
\ Dictionary reads use XREF; private engine boundaries apply the flags and
\ read checker registries before the runtime is sealed.
require src/core/prefix-boundary.f

package ENGINE-INTERNAL

\ THE PASS LEAVES NO RECORD NUMBER IN DATA: a record number is the host's
\ (docs/bootstrap.md, "Generation Chain Check"). So the cursors are loop
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

TRUSTED: MARK-INTERNAL ( n -- ) int-mark ;
TRUSTED: MARK-MIN-IN ( n n -- ) min-in-mark ;

\ A suite bundle decides the seal while the freshly loaded checker's rows still
\ exist. The driver copies this transient plan before either capture releases
\ dynamic storage.
DYNAMIC-BUFFER IMK-PLAN-REC n
DYNAMIC-BUFFER IMK-PLAN-MIN n
variable IMK-PLAN-N
variable IMK-BUNDLE-MODE

: IMK-RECORD ( n n -- ) {: i:n m:n :}
   IMK-PLAN-N @ 1+ IMK-PLAN-REC-RESERVE
   IMK-PLAN-N @ 1+ IMK-PLAN-MIN-RESERVE
   i IMK-PLAN-N @ IMK-PLAN-REC !
   m IMK-PLAN-N @ IMK-PLAN-MIN !
   1 IMK-PLAN-N +! ;

: IMK-MIN-IN ( n -- n )
   dup IMK-NAME-A swap IMK-NAME-U EFFECT-EXTERNAL-MIN-IN ;

\ A GLOBAL RECORD NO TOP-LEVEL ROW TYPES MAY STILL BE A PACKAGE'S PRIMITIVE: a
\ package-private row (checker.f CLOSE-PRIVATE) types it for checked code inside
\ the owner, where the engine binds the name to this record. Marking it DNAME-INT
\ would refuse that caller too, so the record takes the owner's row instead, and
\ DNAME-INT stays the mark of a record no row types anywhere.
: IMK-OWNED-MIN-IN ( n -- n )
   dup IMK-NAME-A swap IMK-NAME-U EFFECT-OWNED-MIN-IN ;

: IMK-MARK ( n n -- ) {: i:n m:n :}   \ unknown -> DNAME-INT; known din>0 -> DNAME-MIN-IN
   IMK-BUNDLE-MODE @ IF i m IMK-RECORD EXIT THEN
   m 0 < IF i MARK-INTERNAL EXIT THEN
   m 0 > IF i m MARK-MIN-IN THEN ;

: IMK-CLASSIFY ( n -- ) {: i:n :}   \ a global record, under its bare name
   i IMK-GLOBAL-EXECUTABLE? 0= IF EXIT THEN
   i IMK-MIN-IN dup 0 < IF drop i IMK-OWNED-MIN-IN THEN
   i swap IMK-MARK ;

: IMK-WALK ( n -- )          \ classify every source-prefix record, from the first
   ndict@ swap ?do i IMK-CLASSIFY loop ;

\ ---- package publics, under their qualified name ---------------------------
\ A package word carries its wordlist, not 0, so IMK-WALK never reaches one. The
\ qualified spelling reaches publics and nothing else: habu1.f FIND-NMATCH
\ searches the package row's [0], the public wordlist, so a private has no
\ top-level spelling (test/internal-word-gate.f pins it E-UNDEFINED). The
\ question is the one asked of a global, keyed as source spells it: checker.f
\ CHECKER-FIND-ACTIVE-SYM routes PKG:TAIL to CHECKER-PUBLIC-SYM?, the key PPRIM:
\ interns.
\ The package rows drive the loop: a row holds its public wordlist in [0] and its
\ name, so no wid -> package table can fall out of step. Each walk starts at its
\ own row because habu2.f C-PACKAGE-NEW-RECORD allocates the wids as it publishes
\ the row (112k record tests instead of 345k on the boot prefix). A namespace row
\ carries DICT-WL:NAMESPACE, distinct from every wordlist id, so the inner walk
\ cannot classify the namespace itself.
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
   i p i IMK-QUAL EFFECT-EXTERNAL-MIN-IN IMK-MARK ;

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
   dup s" min-in-mark" IMK-NAMED? IF drop 0 0= EXIT THEN
   s" rep-mark" IMK-NAMED? ;

: IMK-SEAL-PRIM ( n -- )      \ close the loop: the marking prims are themselves internal
   0 ?do i IMK-PRIM? IF i -1 IMK-MARK THEN loop ;

\ Registry write-protection (src/core/util.f REG-PROTECT): int-mark each
\ registered data record so interpret and tick fail closed on its bare name;
\ compiled core callers resolved before this pass keep working.
: IMK-SEAL-REGISTRY ( -- )
   REG-PROT-COUNT 0 ?do i REG-PROT-RECORD -1 IMK-MARK loop ;

\ ---- the whitebox image ----------------------------------------------------
\ The one build without the seal is the gate's whitebox engine (hb-build
\ --whitebox): its suites reopen engine packages and tick internal words, and a
\ TRUSTED: body does not help because the seal gates INTERPRET and TICK. That
\ build sets the target's ENGINE-UNSEAL (src/core/util.f) before this file loads;
\ IMK-PASS reads it through ENGINE-UNSEAL?, and every other load is sealed.

\ The pass writes its verdict into IMK-CLASS and the capture bakes it, so later
\ readers (the build's smoke run in tools/native-build-core.f,
\ test/whitebox-engine-key-test.f, SEAL-PACKAGES below) read the image, never the
\ request. A cold engine reruns the pass at every boot; only a seeded image
\ carries a verdict its build wrote.
0 constant IMAGE-SEALED         \ the pass ran: internal names are DNAME-INT
1 constant IMAGE-WHITEBOX       \ the pass stood down: they are ordinary words
variable IMK-CLASS

\ The sealed arm also closes the checker's authority gate: until the seal, a
\ checked body naming an internal word is checked against the build's row
\ (src/core/checker.f CALL-AUTHORITY). The whitebox arm leaves it open.
: IMK-SEAL ( -- )
   IMAGE-SEALED IMK-CLASS !
   CHECKER-EFFECT-AUTHORITY:SEAL
   CORE-PREFIX:FIRST-RECORD {: first:n :}
   first IMK-WALK
   first IMK-WALK-PACKAGES
   IMK-SEAL-REGISTRY
   first IMK-SEAL-PRIM ;

: IMK-BUNDLE? ( -- bool )
   s" HABU_GATE_COMPILER_BUNDLE" GETENV s" 1" CORE-STR= ;

: IMK-PLAN-SEAL ( -- )
   0 IMK-PLAN-N !
   -1 IMK-BUNDLE-MODE !
   IMAGE-WHITEBOX IMK-CLASS !
   CORE-PREFIX:FIRST-RECORD {: first:n :}
   first IMK-WALK
   first IMK-WALK-PACKAGES
   IMK-SEAL-REGISTRY
   first IMK-SEAL-PRIM ;

\ Either class ends the registrations: the seal has read them, and a whitebox
\ image has no seal to read them.
: IMK-PASS ( -- )
   IMK-BUNDLE? IF IMK-PLAN-SEAL ELSE
      ENGINE-UNSEAL? IF IMAGE-WHITEBOX IMK-CLASS ! ELSE IMK-SEAL THEN
   THEN
   REG-PROT-RETIRE ;

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
\ into either exit ENGINE-ERROR:SEAL-PACKAGE, and the capture keeps none of their
\ private symbols (checker-surface.f KEEP?). IMK-PASS runs before the compiler,
\ the REPL and the manifest's packages exist, so the native build calls this at
\ capture (tools/native-build-core.f PREPARE-TARGET), before
\ NATIVE-RUNTIME:CAPTURE-PREPARE sweeps; an application image (APP-IMAGE:SAVE)
\ keeps its packages reopenable. The whitebox image stands down here too. A
\ package with no private wordlist holds 0 there, the global wordlist: the
\ capture refuses an image that protects it.
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

: PLAN-N ( -- n ) IMK-PLAN-N @ ;

: PLAN-PAIR ( n -- n n ) {: k:n :}
   k IMK-PLAN-REC @ k IMK-PLAN-MIN @ ;

\ The retained driver alone resolves this private entry. Its plan lives
\ outside target DATA while the first capture discards transient buffers.
: APPLY-PLAN ( ptr n n -- ) {: plan:ptr count:n :}
   IMK-BUNDLE-MODE @ 0= IF s" internal-mark: no suite seal plan" 76 die THEN
   0 IMK-BUNDLE-MODE !
   IMAGE-SEALED IMK-CLASS !
   CHECKER-EFFECT-AUTHORITY:SEAL
   count 0 ?do
      plan i 2 * cells + @ plan i 2 * 1+ cells + @ IMK-MARK
   loop ;

get-current prot-wid-add
' IMK-PASS
;package
execute
