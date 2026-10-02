\ package-seal.f - every package a product engine bakes is sealed.
\
\ Run: bin/hb --load test/package-seal.f
\
\ The native build seals every package its capture ships before the capture's
\ checker sweep runs (src/core/internal-mark.f SEAL-PACKAGES, called first by
\ tools/native-build-core.f PREPARE-TARGET): both wordlists of each baked
\ package take the protected bit, which the image restores at boot. So on the
\ product engine:
\   - `package NAME` on a baked package exits ENGINE-ERROR:SEAL-PACKAGE (84)
\     with the package name on stderr. TOP-ROW and NATIVE-RUNTIME seal nothing
\     themselves and load after the internal-word pass, so before the build
\     sealed them both reopened;
\   - a definition into a baked package's public wordlist by its qualified name
\     exits 84 naming the word;
\   - a package a program makes after boot is not sealed: it reopens, and the
\     words of both blocks run;
\   - a baked package's public words still resolve, qualified and through
\     `using`;
\   - every namespace row below the seal floor has a protected public wordlist
\     and a private wordlist that is protected or absent (0, the global
\     wordlist, which the capture refuses to mark).
\ Each program runs in a disposable fork of this engine (lib/test/subject.f),
\ so a refusal ends the fork and not the suite. The whitebox engine keeps every
\ package open (test/whitebox-engine-suite.f), so the image class is asserted
\ first.

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/process.f
require lib/test/subject.f

package PACKAGE-SEAL

4096 constant CAP
30000 constant TIMEOUT-MS

create OUT CAP allot
create ERR CAP allot
variable OUT-U
variable ERR-U
variable RC
variable ROWS
variable OPEN

: OUT$ ( -- ptr u8 n ) OUT OUT-U @ ;
: ERR$ ( -- ptr u8 n ) ERR ERR-U @ ;

\ One program, evaluated by a disposable fork of this engine.
: LOAD ( ptr u8 n -- )
   {: a:ptr u:n :}
   a u OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS SUBJECT:RUN
   {: outu:len erru:len outcome:outcome :}
   outu LEN>N OUT-U !
   erru LEN>N ERR-U !
   outcome PROC-OUTCOME>RC RC>N RC ! ;

: REPORT ( -- )
   s" package-seal child rc " type RC @ . OUT$ type ERR$ type cr ;

\ The program exits the seal's code with `name` on stderr.
: SEALED ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: lab:ptr labu:n src:ptr srcu:n name:ptr nameu:n :}
   src srcu LOAD
   RC @ ENGINE-ERROR:SEAL-PACKAGE <> if REPORT then
   lab labu T-LABEL RC @ ENGINE-ERROR:SEAL-PACKAGE T=
   lab labu T-LABEL ERR$ name nameu CONTAINS? TTRUE ;

\ The program exits 0 and prints exactly `want`.
: PRINTS ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: lab:ptr labu:n src:ptr srcu:n want:ptr wantu:n :}
   src srcu LOAD
   RC @ 0<> if REPORT then
   lab labu T-LABEL RC @ 0 T=
   lab labu T-LABEL OUT$ want wantu T$= ;

: SECTION-CLASS ( -- )
   s" the engine under test is the sealed product" T-LABEL
   ENGINE-INTERNAL:IMAGE-CLASS ENGINE-INTERNAL:IMAGE-SEALED T= ;

: SECTION-REOPEN ( -- )
   s" a baked package loaded after the internal-word pass refuses `package`"
   s" package TOP-ROW ;package" s" TOP-ROW" SEALED
   s" and so does the package whose capture preparation runs the sweep"
   s" package NATIVE-RUNTIME ;package" s" NATIVE-RUNTIME" SEALED
   s" a definition into a baked package's public wordlist is refused"
   s" : TOP-ROW:ZZ ( -- ) ;" s" TOP-ROW:ZZ" SEALED
   s" by the engine's protected-wordlist guard" T-LABEL
   ERR$ s" cannot publish into protected word" CONTAINS? TTRUE ;

: SECTION-APPLICATION ( -- )
   s" a package a program makes reopens, and both blocks' words run"
   S\" package MINE\n: ONE ( -- n ) 1 ;\n;package\npackage MINE\npublic\n: TWO ( -- n ) ONE 1 + ;\n;package\nMINE:TWO . cr\n"
   S\" 2\n\n" PRINTS ;

: SECTION-PUBLIC ( -- )
   s" a baked package's public word resolves qualified"
   S\" ENGINE-INTERNAL:IMAGE-CLASS . cr\n"
   S\" 0\n\n" PRINTS
   s" and through using"
   S\" using ENGINE-INTERNAL\nIMAGE-CLASS . cr\n;using\n"
   S\" 0\n\n" PRINTS ;

\ Both wordlists of a namespace row are sealed; a row with no private
\ wordlist holds 0 there.
: ROW-SEALED? ( ptr n -- bool )
   {: rec:ptr :}
   rec XREF-PKG-PRIVATE {: pri:n :}
   rec XREF-PKG-PUBLIC XREF-WID-PROTECTED?
   pri 0= pri XREF-WID-PROTECTED? or and ;

\ Every namespace row below the seal floor is a package the engine baked.
: CENSUS ( -- )
   0 ROWS !  0 OPEN !
   SEAL-NDICT@ 0 ?do
      i XREF-REC {: rec:ptr :}
      rec XREF-WORDLIST XREF-NAMESPACE-WL = if
         ROWS @ 1 + ROWS !
         rec ROW-SEALED? 0= if
            OPEN @ 1 + OPEN !
            s" package-seal: open " type rec XREF-NAME$ type cr
         then
      then
   loop ;

: SECTION-CENSUS ( -- )
   CENSUS
   s" package-seal: baked packages " type ROWS @ . cr
   s" the engine bakes packages" T-LABEL
   ROWS @ 0 > TTRUE
   s" and every one is sealed in both wordlists" T-LABEL
   OPEN @ 0 T= ;

public

: RUN ( -- )
   T-RESET
   SECTION-CLASS
   SECTION-REOPEN
   SECTION-APPLICATION
   SECTION-PUBLIC
   SECTION-CENSUS
   T-REPORT ;

;package

PACKAGE-SEAL:RUN
