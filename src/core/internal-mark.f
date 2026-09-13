\ internal-mark.f - seal source authority and record its call arity.
\ Dictionary reads use XREF; private engine boundaries apply the flags and
\ read checker registries before the runtime is sealed.
require src/core/prefix-boundary.f

package ENGINE-INTERNAL


variable IMK-I
variable IMK-FIRST

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

: FIRST-CORE ( -- n ) IMK-FIRST @ ;
TRUSTED: KNOWN-MIN-IN ( ptr u8 n -- n ) EFFECT-EXTERNAL-MIN-IN ;
TRUSTED: MARK-INTERNAL ( n -- ) int-mark ;
TRUSTED: MARK-MIN-IN ( n n -- ) min-in-mark ;
TRUSTED: PROTECTED-COUNT ( -- n ) REG-PROT-N @ ;
TRUSTED: PROTECTED-RECORD ( n -- n ) cells REG-PROT-IDX + @ ;

: IMK-MIN-IN ( n -- n )
   dup IMK-NAME-A swap IMK-NAME-U KNOWN-MIN-IN ;

: IMK-MARK ( n n -- ) {: i:n m:n :}   \ unknown -> DNAME-INT; known din>0 -> DNAME-MIN-IN
   m 0 < IF i MARK-INTERNAL EXIT THEN
   m 0 > IF i m MARK-MIN-IN THEN ;

: IMK-CLASSIFY ( n -- ) {: i:n :}   \ a global record, under its bare name
   i IMK-GLOBAL-EXECUTABLE? 0= IF EXIT THEN
   i i IMK-MIN-IN IMK-MARK ;

: IMK-WALK ( -- )            \ classify every source-prefix record
   FIRST-CORE IMK-I !
   BEGIN IMK-I @ ndict@ < WHILE
      IMK-I @ IMK-CLASSIFY
      IMK-I @ 1 + IMK-I !
   REPEAT ;

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
variable IMK-P               \ package-row cursor (IMK-I carries the inner walk)

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

: IMK-PKG-PUBLICS ( n -- ) {: p:n :}   \ every public colon record of package row p
   p IMK-REC XREF-START {: pub:n :}
   p 1 + FIRST-CORE max IMK-I !
   BEGIN IMK-I @ ndict@ < WHILE
      IMK-I @ IMK-WID pub = IF p IMK-I @ IMK-CLASSIFY-PUB THEN
      IMK-I @ 1 + IMK-I !
   REPEAT ;

: IMK-WALK-PACKAGES ( -- )   \ classify every package's public wordlist
   0 IMK-P !
   BEGIN IMK-P @ ndict@ < WHILE
      IMK-P @ IMK-WID DICT-WL:NAMESPACE = IF IMK-P @ IMK-PKG-PUBLICS THEN
      IMK-P @ 1 + IMK-P !
   REPEAT ;

: IMK-NAMED? ( n ptr u8 n -- bool ) {: i:n a:ptr u:n :}
   i IMK-NAME-U u = IF i IMK-NAME-A i IMK-NAME-U a u CORE-STR= ELSE 0 0= 0= THEN ;

: IMK-PRIM? ( n -- bool )    \ record n is one of the marking prims themselves
   dup s" int-mark" IMK-NAMED? IF drop 0 0= EXIT THEN
   s" min-in-mark" IMK-NAMED? ;

: IMK-SEAL-PRIM ( -- )       \ close the loop: the marking prims are themselves internal
   0 IMK-I !
   BEGIN IMK-I @ FIRST-CORE < WHILE
      IMK-I @ IMK-PRIM? IF IMK-I @ MARK-INTERNAL THEN
      IMK-I @ 1 + IMK-I !
   REPEAT ;

\ Registry write-protection (dot habu-protect-type-field-04d91409). A din=0
\ registry control cell (variable/create) is a data record, so IMK-WALK exempts
\ it and its bare name stays executable — a bare `<cell> !` mutates the registry
\ past the public API. type-family.f's REG-PROTECT recorded each such cell's
\ dictionary index in REG-PROT-IDX[0, REG-PROT-N); int-mark them so interpret /
\ tick fail closed on the bare name exactly like a sig-less colon word, while the
\ core compiled callers (resolved before this pass) keep working.
: IMK-SEAL-REGISTRY ( -- )
   0 IMK-I !
   BEGIN IMK-I @ PROTECTED-COUNT < WHILE
      IMK-I @ PROTECTED-RECORD MARK-INTERNAL
      IMK-I @ 1 + IMK-I !
   REPEAT ;

: IMK-PASS ( -- )
   CORE-PREFIX:FIRST-RECORD IMK-FIRST !
   IMK-WALK
   IMK-WALK-PACKAGES
   IMK-SEAL-REGISTRY
   IMK-SEAL-PRIM ;

get-current prot-wid-add
' IMK-PASS
;package
execute
