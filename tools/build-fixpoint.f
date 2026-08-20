\ build-fixpoint.f - checked self-rebuild orchestration.
\
\ Load after lib/errors.f, lib/string.f, lib/memory.f, lib/fs.f, lib/fs-mutate.f,
\ lib/process.f, lib/process-argv.f, lib/process-env.f, and lib/codesign.f.
\ The stamp key hashes the engine and the emitted stage sources with the baked
\ SHA256 words; only the native-chain fold below reaches for lib/content-key.f
\ and the shared closure walk, because that input is a file LIST rather than one
\ file (see the stamp key section below).

require lib/adt/option.f                 \ option<CAD-NUM:index> STR:FIND-SUB consumer
require src/habu/verify-source.f
require tools/stdin-closure-lib.f
\ the manifest is the one place a stdin-closure path lives; read its names bare.
using STDIN-CLOSURE

\ The tool itself lives in package BUILD-FIXPOINT. Everything below is private
\ to it; the export block at the end of the file names the whole surface other
\ files call. The sibling packages in this file (COMPILER-BUILD, BUILD-EXT,
\ CAD-NUM, VERIFY) close and reopen BUILD-FIXPOINT around themselves, because
\ packages do not nest, and import what they need with `using BUILD-FIXPOINT`.
package BUILD-FIXPOINT

262144 constant BF-SOURCE-CAP
32768 constant BF-CMP-CAP
$10000 constant BF-CERT-DIAG-CAP
4 constant BF-MAX-GENS
10 constant BF-LF
64 constant BF-USAGE-RC
74 constant BF-BUILD-RC
34 constant BF-DQ
$2F constant BF-SLASH
64 constant BF-STAMP-HEX-U
12 constant BF-STAMP-PREFIX-U
32 constant BF-STAMP-DG-U
256 constant BF-STAMP-CAP
128 constant BF-PIN-CAP

\ Load-discipline guard. build-fixpoint.f is the caller-composed depth-0 --load
\ entry (BF-CLI-SELF-DISPATCH below); the lib preamble is --load-ed by the caller,
\ never require-d here, so an old native seed can supply the minimal set its
\ primitives support (see lib/process-env.f). Without the preamble the FS-PATH-CAP
\ buffer create just below otherwise dies mid-load with a bare
\ `E-UNDEFINED: FS-PATH-CAP`; name the required load list instead. FS-PATH-CAP
\ (lib/fs.f) is the preamble sentinel - it is the first preamble word this file
\ references, so its absence is exactly the missing-preamble case.
\
\ CHECKER-RESOLVES? asks which word this token would BIND to here, so the guard
\ answers the same inside this package as outside it. It used to have to be a
\ PUBLIC word called after `;package`, because the query it had then answered in
\ the defining scope and reported a loaded preamble as missing from inside the
\ package (dot habu-checker-defined-answers-1504bbde). It is an ordinary private
\ word now.
: BF-NEED-PREAMBLE ( -- )
   s" FS-PATH-CAP" CHECKER-RESOLVES? if exit then
   S\" build-fixpoint: missing required load; --load lib/errors.f lib/string.f lib/memory.f lib/fs.f lib/fs-mutate.f lib/process.f lib/process-argv.f lib/process-env.f lib/codesign.f tools/build-fixpoint.f tools/build-fixpoint-main.f before the build verb\n" BF-USAGE-RC die ;
BF-NEED-PREAMBLE

;package

\ The package closes for these two requires, and not for the guard above: both
\ files open packages of their own, and `package` inside an open package is
\ rejected as nesting. They also come AFTER the guard on purpose - both pull the
\ preamble libraries in through their own requires, so requiring them earlier
\ would satisfy FS-PATH-CAP by side effect and the missing-preamble case would
\ stop being reported at all.
require lib/content-key.f                \ the chain fold
require tools/event-closure-lib.f        \ the chain fold

package BUILD-FIXPOINT

create BF-LF-BUF 1 allot
create BF-PIN-KEYS BF-PIN-CAP BF-STAMP-DG-U * allot
create BF-PIN-DIGS BF-PIN-CAP BF-STAMP-DG-U * allot
create BF-PIN-KEYBUF 40 allot
create BF-PIN-DIGBUF 40 allot
create BF-STAMP-KEY 80 allot
create BF-STAMP-OLD 80 allot
create BF-STAMP-DG 40 allot
create BF-REC-PREFIX-DG 40 allot
create BF-REC-STAGE-DG 40 allot
create BF-REC-STDIN-DG 40 allot
create BF-CERT-DIAG BF-CERT-DIAG-CAP allot
create BF-STAMP-BUF BF-STAMP-CAP allot
create BF-STAMP-PATH-BUF FS-PATH-CAP allot
create BF-STAMP-DIR-BUF FS-PATH-CAP allot
create BF-STAMP-DEF-BUF FS-PATH-CAP allot
create BF-ENGINE-BUF FS-PATH-CAP allot
create BF-INSTALL-TMP-BUF FS-PATH-CAP allot
create BF-HOST-DST-BUF FS-PATH-CAP allot
create BF-HOST-TMP-BUF FS-PATH-CAP allot
BF-LF BF-LF-BUF c!

variable BF-ART-PATH-A
variable BF-OUT-PATH-A
variable BF-A-PATH-A
variable BF-B-PATH-A
variable BF-SOURCE-BUF-A
variable BF-CMP-A-BUF-A
variable BF-CMP-B-BUF-A
variable BF-SOURCE-LEN
variable BF-A-LEN
variable BF-B-LEN
variable BF-FDA
variable BF-FDB
variable BF-APP-IN
variable BF-APP-OUT
variable BF-RA
variable BF-RB
variable BF-APP-RD
variable BF-APP-WR
variable BF-APP-OFF
variable BF-GEN
variable BF-FOUND
variable BF-PID
variable BF-TMP-A
variable BF-TMP-U
variable BF-STRIP-R
variable BF-STRIP-W
variable BF-STRIP-OFF
variable BF-STAMP-PATH-U
variable BF-STAMP-DIR-U
variable BF-STAMP-DEF-U
variable BF-STAMP-U
variable BF-REC-PREFIX?
variable BF-REC-STAGE?
variable BF-REC-STDIN?
variable BF-ENGINE-U
variable BF-INSTALL-TMP-U
variable BF-HOST-DST-U
variable BF-HOST-TMP-U
variable BF-FORCE
variable BF-PIN-N
variable BF-PIN-ON
variable BF-CERT-RC
variable BF-CERT-DIAG-U
variable BF-CERT-LAB-A
variable BF-CERT-LAB-U
variable BF-CERT-PATH-A
variable BF-CERT-PATH-U

: BF-TMP-A-FIELD ( -- ptr ptr u8 )
   BF-TMP-A 0 ptr-field ;

: BF-CERT-LAB-A-FIELD ( -- ptr ptr u8 )
   BF-CERT-LAB-A 0 ptr-field ;

: BF-CERT-PATH-A-FIELD ( -- ptr ptr u8 )
   BF-CERT-PATH-A 0 ptr-field ;

: BF-TMP-A@ ( -- ptr u8 )
   BF-TMP-A-FIELD @ ;

: BF-TMP-A! ( ptr u8 -- )
   BF-TMP-A-FIELD ! ;

: BF-CERT-LAB-A@ ( -- ptr u8 )
   BF-CERT-LAB-A-FIELD @ ;

: BF-CERT-LAB-A! ( ptr u8 -- )
   BF-CERT-LAB-A-FIELD ! ;

: BF-CERT-PATH-A@ ( -- ptr u8 )
   BF-CERT-PATH-A-FIELD @ ;

: BF-CERT-PATH-A! ( ptr u8 -- )
   BF-CERT-PATH-A-FIELD ! ;

: BF-PTR-U8-FIELD ( ptr a -- ptr ptr u8 )
   0 ptr-field ;

: BF-PTR-U8@ ( ptr a -- ptr u8 )
   BF-PTR-U8-FIELD @ ;

: BF-PTR-U8! ( ptr u8 ptr a -- )
   BF-PTR-U8-FIELD ! ;

: BF-ALLOC-BUF ( n -- ptr u8 )
   MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop ;

: BF-BUF ( ptr a n -- ptr u8 ) {: slot:ptr cap :}
   slot @ 0= if cap BF-ALLOC-BUF slot BF-PTR-U8! then
   slot BF-PTR-U8@ ;

: BF-SOURCE-BUF ( -- ptr u8 )
   BF-SOURCE-BUF-A BF-SOURCE-CAP BF-BUF ;

: BF-CMP-A ( -- ptr u8 )
   BF-CMP-A-BUF-A BF-CMP-CAP BF-BUF ;

: BF-CMP-B ( -- ptr u8 )
   BF-CMP-B-BUF-A BF-CMP-CAP BF-BUF ;

: BF-PATH-BUF ( ptr a -- ptr u8 )
   FS-PATH-CAP BF-BUF ;

: BF-ART-PATH ( -- ptr u8 )
   BF-ART-PATH-A BF-PATH-BUF ;

: BF-OUT-PATH ( -- ptr u8 )
   BF-OUT-PATH-A BF-PATH-BUF ;

: BF-A-PATH ( -- ptr u8 )
   BF-A-PATH-A BF-PATH-BUF ;

: BF-B-PATH ( -- ptr u8 )
   BF-B-PATH-A BF-PATH-BUF ;

: BF-TRUE ( -- bool )
   0 0= ;

: BF-FALSE ( -- bool )
   0 0= 0= ;

: BF-TMP! ( ptr u8 n -- )
   {: a:ptr u :}
   u BF-TMP-U !
   a BF-TMP-A! ;

: BF-TMP-OVERRIDE$ ( -- ptr u8 n )
   BF-TMP-A@ BF-TMP-U @ ;

: BF-TMP-RESET ( -- )
   0 BF-TMP-U ! ;

: BF-TMP$ ( -- ptr u8 n )
   BF-TMP-U @ 0 > if BF-TMP-OVERRIDE$ exit then
   s" HB_TMP" GETENV dup 0= if drop drop s" /tmp" then ;

: BF-TMP> ( ptr u8 n ptr u8 -- n ) {: name:ptr nameu dst:ptr :}
   BF-TMP$ {: root:ptr rootu :}
   rootu 0 <= if E-BUILD-PATH throw then
   rootu 1 + nameu + FS-PATH-CAP > if E-BUILD-PATH throw then
   root rootu name nameu dst JOIN-PATH ;

: BF-ART$ ( ptr u8 n -- ptr u8 n )
   BF-ART-PATH BF-TMP> BF-ART-PATH swap ;

: BF-OUT$ ( ptr u8 n -- ptr u8 n )
   BF-OUT-PATH BF-TMP> BF-OUT-PATH swap ;

: BF-A$ ( ptr u8 n -- ptr u8 n )
   BF-A-PATH BF-TMP> BF-A-LEN !
   BF-A-PATH BF-A-LEN @ ;

: BF-B$ ( ptr u8 n -- ptr u8 n )
   BF-B-PATH BF-TMP> BF-B-LEN !
   BF-B-PATH BF-B-LEN @ ;

: BF-EXPECT-PATH ( ptr u8 n -- ) {: path:ptr pathu :}
   pathu 0 <= if E-BUILD-PATH throw then
   path pathu FILE? 0= if E-BUILD-PATH throw then ;

: BF-EXPECT ( ptr u8 n -- )
   BF-ART$ BF-EXPECT-PATH ;

: BF-RC0 ( n -- )
   0 <> if E-BUILD-STATUS throw then ;

: BF-REMOVE-TMP ( ptr u8 n -- ) {: a:ptr u :}
   a u BF-A$ 2dup EXISTS? if REMOVE-FILE else 2drop then ;

: BF-RENAME-TMP ( ptr u8 n ptr u8 n -- ) {: src:ptr srcu dst:ptr dstu :}
   src srcu BF-A$ dst dstu BF-B$ RENAME-FILE ;

: BF-CHMOD-X-TMP ( ptr u8 n -- )
   BF-A$ CHMOD-X ;

: BF-OPEN-INPUT ( ptr u8 n -- n )
   FS-PATHZ open-rd dup 0 < if E-BUILD-PATH throw then ;

: BF-PREPARE-ENV ( -- )
   PROC-ENV-RESET
   s" HB_TMP" >LEN BF-TMP$ >LEN PROC-ENV+
   s" HABU_ENGINE_SIZE_MAP" GETENV {: map:ptr mapu:n :}
   mapu 0 > if s" HABU_ENGINE_SIZE_MAP" >LEN map mapu >LEN PROC-ENV+ then ;

: BF-FINISH-PID ( pid -- n ) {: pid :}
   PROC-ARGV-ENV-RESET
   pid PID>N 0 < if E-PROC-SPAWN throw then
   pid PROC-WAIT-RC MATCH result ok OF ENDOF err OF ENDOF ;MATCH ;   \ completion code (ok 0 / err nonzero)

: BF-RUN-ENV-FDS ( ptr u8 n n n n -- n ) {: exe:ptr exeu infd outfd errfd :}
   BF-PREPARE-ENV
   PROC-ARGV-RESET
   exe exeu >LEN PROC-ARGV-PREPARE PROC-ENV-PREPARE infd >FD outfd >FD errfd >FD
   PROC-SPAWN-ARGV-ENV-RAW BF-FINISH-PID ;

: BF-RUN-ENV-INFD ( ptr u8 n n -- n ) {: exe:ptr exeu infd :}
   BF-PREPARE-ENV
   PROC-ARGV-RESET
   exe exeu >LEN PROC-ARGV-PREPARE PROC-ENV-PREPARE infd >FD -1 >FD -1 >FD
   PROC-SPAWN-ARGV-ENV-RAW {: pid :}
   infd close
   pid BF-FINISH-PID ;

: BF-PREPARE-STAGE-ARGV ( ptr u8 n -- ptr u8 ptr a )
   PROC-ARGV-RESET
   s" --" >LEN PROC-ARGV+
   BF-TMP$ >LEN PROC-ARGV+
   >LEN PROC-ARGV-PREPARE ;

: BF-PREPARE-LOAD-STAGE-ARGV ( ptr u8 n ptr u8 n -- ptr u8 ptr a ) {: exe:ptr exeu:n src:ptr srcu:n :}
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   src srcu >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   BF-TMP$ >LEN PROC-ARGV+
   exe exeu >LEN PROC-ARGV-PREPARE ;

: BF-RUN-STAGE-ENV-INFD ( ptr u8 n n -- n ) {: exe:ptr exeu infd :}
   BF-PREPARE-ENV
   exe exeu BF-PREPARE-STAGE-ARGV
   PROC-ENV-PREPARE infd >FD -1 >FD -1 >FD
   PROC-SPAWN-ARGV-ENV-RAW {: pid :}
   infd close
   pid BF-FINISH-PID ;

: BF-RUN-LOAD-STAGE ( ptr u8 n ptr u8 n -- n ) {: exe:ptr exeu:n src:ptr srcu:n :}
   BF-PREPARE-ENV
   exe exeu src srcu BF-PREPARE-LOAD-STAGE-ARGV
   PROC-ENV-PREPARE -1 >FD -1 >FD -1 >FD
   PROC-SPAWN-ARGV-ENV-RAW BF-FINISH-PID ;

: BF-RUN-ENV-EXE ( ptr u8 n -- n )
   -1 -1 -1 BF-RUN-ENV-FDS ;

: BF-RUN-STAGE-ENV-EXE ( ptr u8 n -- n )
   BF-PREPARE-ENV
   BF-PREPARE-STAGE-ARGV
   PROC-ENV-PREPARE -1 >FD -1 >FD -1 >FD
   PROC-SPAWN-ARGV-ENV-RAW BF-FINISH-PID ;

: BF-RUN-ENV-PATH-INFILE ( ptr u8 n ptr u8 n -- n ) {: exe:ptr exeu src:ptr srcu :}
   exe exeu src srcu BF-OPEN-INPUT BF-RUN-ENV-INFD ;

: BF-RUN-STAGE-PATH-INFILE ( ptr u8 n ptr u8 n -- n ) {: exe:ptr exeu src:ptr srcu :}
   exe exeu src srcu BF-OPEN-INPUT BF-RUN-STAGE-ENV-INFD ;

: BF-RUN-ENV-TMP ( ptr u8 n -- n )
   BF-A$ BF-RUN-ENV-EXE ;

: BF-RUN-STAGE-TMP ( ptr u8 n -- n )
   BF-A$ BF-RUN-STAGE-ENV-EXE ;

: BF-RUN-ENV-TMP-INFILE ( ptr u8 n ptr u8 n -- n ) {: exe:ptr exeu src:ptr srcu :}
   exe exeu BF-A$ src srcu BF-B$ BF-OPEN-INPUT BF-RUN-ENV-INFD ;

: BF-CODESIGN-VERIFY-TMP ( ptr u8 n -- ) {: a:ptr u :}
   a u BF-A$ CODESIGN:VERIFY ;

: BF-CODESIGN-FORCE-TMP ( ptr u8 n -- ) {: a:ptr u :}
   a u BF-A$ CODESIGN:FORCE ;

: BF-RESET-OUT ( ptr u8 n -- )
   BF-OUT$ BF-SOURCE-BUF 0 WRITE-ALL ;

: BF-APPEND-BYTES ( ptr u8 n ptr u8 n -- ) {: out:ptr outu a:ptr u :}
   out outu BF-OUT$ a u APPEND-FILE ;

: BF-APPEND-LF ( ptr u8 n -- ) {: out:ptr outu :}
   out outu BF-OUT$ BF-LF-BUF 1 APPEND-FILE ;

: BF-APPEND-LINE ( ptr u8 n ptr u8 n -- ) {: out:ptr outu:n a:ptr u:n :}
   out outu a u BF-APPEND-BYTES
   out outu BF-APPEND-LF ;

\ Promoted for the sibling packages further down this file: they are separate
\ packages, so they reach these through `using BUILD-FIXPOINT` and cannot see
\ private tails. Everything the rest of the repository calls is promoted in one
\ block at the end of the file.
public
EXPORT BF-A$
EXPORT BF-APPEND-LINE
EXPORT BF-B$
EXPORT BF-FINISH-PID
EXPORT BF-PREPARE-ENV
EXPORT BF-TMP$
private

;package

package COMPILER-BUILD
using BUILD-FIXPOINT

: ARGV ( ptr u8 n ptr u8 n -- ptr u8 ptr a ) {: exe:ptr exeu:n src:ptr srcu:n :}
   PROC-ARGV-RESET
   s" --build" >LEN PROC-ARGV+
   src srcu >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   BF-TMP$ >LEN PROC-ARGV+
   exe exeu >LEN PROC-ARGV-PREPARE ;

public

: RUN ( ptr u8 n ptr u8 n -- n ) {: exe:ptr exeu:n src:ptr srcu:n :}
   BF-PREPARE-ENV
   exe exeu src srcu ARGV
   PROC-ENV-PREPARE -1 >FD -1 >FD -1 >FD
   PROC-SPAWN-ARGV-ENV-RAW BF-FINISH-PID ;

: RUN-TMP ( ptr u8 n ptr u8 n -- n ) {: exe:ptr exeu:n src:ptr srcu:n :}
   exe exeu BF-A$ src srcu BF-B$ RUN ;

: SEAL ( ptr u8 n -- )
   s" SEAL-FRIEND" BF-APPEND-LINE ;

;using
;package

package BUILD-FIXPOINT

\ THE HOST CAPABILITY THE EMITTED REWIND NEEDS. The stage source rewinds to the
\ mark src/core/lower-cert-seal.f records at the core prefix's end, and it
\ carries no copy of that prefix for an older rewind to land on. So a host
\ engine without the mark cannot build this tree at all, and the honest place to
\ say so is here - before a byte is emitted - naming the file and the repair.
\ Emitting the old earliest-marker rewind instead would truncate below a prefix
\ the source no longer re-emits, and the first symptom would be an undefined
\ core word reported from somewhere inside a 900KB generated file.
\
\ CHECKER-RESOLVES? is the check because it is the only existence query in the
\ boot prefix that answers for a package-qualified name: `search-wl` searches
\ one wordlist on the raw spelling and a package name is not itself a
\ dictionary word, so it answers 0 for PKG:TAIL whether or not the package has
\ it (measured, against STR:LENGTH).
: BF-WATERMARK? ( -- bool )
   s" PREFIX-MARK:CURSORS" CHECKER-RESOLVES? ;

\ AND THE FACT THE NAME CANNOT CARRY. Resolvability answers "could this host
\ compile the call"; it cannot answer "did this host's mark record the boundary
\ set", because a name resolves whether or not anything ever wrote through it -
\ and an engine built from a tree that carried the words but never took the
\ boundary is exactly the host that would emit a rewind restoring nothing. So the
\ value is read too, and it is DATA no other writer can produce: CU is private to
\ PREFIX-MARK and src/core/lower-cert-seal.f assigns it once, from
\ CHECKER-BOUND:CURSORS. Zero therefore means the boundary set never landed in
\ this engine, not that it never ran - the mark is top-level in a prefix file, so
\ every host that boots has reached it.
\
\ Through `evaluate`, and only after the resolvability check has passed, because
\ the two clauses answer different hazards and the compile one comes first: a
\ host without the word cannot COMPILE a body that names it, so this file's load
\ would die E-UNDEFINED before any guard could speak. That is what a resolvability
\ query is for, and it is why the value cannot simply be called.
\
\ WHAT THIS DELIBERATELY DOES NOT CHECK, and why the reason is a measurement and
\ not an omission: that the recorded width still equals the seam's live one.
\ CHECKER-BOUND lives below the lower-cert seal, and the seal is what makes it
\ unreachable from every consumer outside the prefix - measured here,
\ `s" CHECKER-BOUND:CURSORS" CHECKER-RESOLVES?` answers 0 on a booted engine while
\ the same query answers -1 for PREFIX-MARK:CURSORS. It is the same wall that made
\ src/habu/prefix-rewind.f reach the seam through a trusted row rather than a
\ name. So no consumer can hold the two numbers at once, and the property that
\ the count comes OFF the seam is carried by construction instead: one assignment,
\ in the file that owns the mark, from the seam itself.
\ Retirement: habu-builder-trust-rows-c5d41af6.
TRUSTED: BF-EVAL-N ( ptr u8 n -- n ) evaluate ;

: BF-MARK-CURSORS ( -- n )
   s" PREFIX-MARK:CURSORS" BF-EVAL-N ;

: BF-REQUIRE-WATERMARK ( -- )
   BF-WATERMARK? 0= if
      s" build-fixpoint: host engine has no core-prefix watermark (src/core/lower-cert-seal.f); reseed bin/hb from a current engine and refresh" BF-BUILD-RC die
   then
   BF-MARK-CURSORS 0= if
      s" build-fixpoint: host engine's core-prefix mark carries no checker boundary (src/core/lower-cert-seal.f never recorded CHECKER-BOUND:CURSORS); reseed bin/hb from a current engine and refresh" BF-BUILD-RC die
   then ;

\ The one line every generated engine source opens its payload with. The
\ earliest-marker form src/habu/hide.f still carries is the recovery script's
\ mirror, not this builder's.
: BF-STAGE2-HIDE-DEFS ( ptr u8 n -- )
   s" PREFIX-REWIND:TO-CORE" BF-APPEND-LINE ;

: BF-APP-CLOSE ( ptr n -- ) {: p:ptr :}
   p @ dup 0 >= if close else drop then
   -1 p ! ;

: BF-APP-THROW ( n -- )
   BF-APP-IN BF-APP-CLOSE
   BF-APP-OUT BF-APP-CLOSE
   throw ;

: BF-APP-RESET ( -- )
   -1 BF-APP-IN !
   -1 BF-APP-OUT ! ;

: BF-APP-OPEN ( ptr u8 n ptr u8 n -- ) {: src:ptr srcu dst:ptr dstu :}
   src srcu FS-PATHZ open-rd BF-APP-IN !
   BF-APP-IN @ 0 < if E-FS-OPEN BF-APP-THROW then
   dst dstu OPEN-APPEND-FD BF-APP-OUT !
   BF-APP-OUT @ 0 < if E-FS-OPEN BF-APP-THROW then ;

: BF-APP-WRITE-CHUNK ( n -- ) {: u :}
   0 BF-APP-OFF !
   begin BF-APP-OFF @ u < while
      BF-APP-OUT @ BF-SOURCE-BUF BF-APP-OFF @ + u BF-APP-OFF @ - write BF-APP-WR !
      BF-APP-WR @ 0 <= if E-FS-IO BF-APP-THROW then
      BF-APP-WR @ u BF-APP-OFF @ - > if E-FS-IO BF-APP-THROW then
      BF-APP-OFF @ BF-APP-WR @ + BF-APP-OFF !
   repeat ;

: BF-APPEND-FILE-STREAM ( ptr u8 n ptr u8 n -- ) {: src:ptr srcu dst:ptr dstu :}
   BF-APP-RESET
   src srcu dst dstu BF-APP-OPEN
   begin
      BF-APP-IN @ BF-SOURCE-BUF BF-SOURCE-CAP read BF-APP-RD !
      BF-APP-RD @ 0 < if E-FS-IO BF-APP-THROW then
      BF-APP-RD @ BF-SOURCE-CAP > if E-FS-IO BF-APP-THROW then
      BF-APP-RD @ 0 >
   while
      BF-APP-RD @ BF-APP-WRITE-CHUNK
   repeat
   BF-APP-IN BF-APP-CLOSE
   BF-APP-OUT BF-APP-CLOSE ;

\ ---------------------------------------------------------------------------
\ Boot-prefix hash pin: close the mid-build boot-reload TOCTOU.
\
\ Every checkout source file emitted into a stage image is streamed by
\ BF-APPEND-SOURCE. The same boot-prefix files (checker/core/target/emitter
\ surface) are re-read across the stage2, stdin, and snap emissions and the
\ stamp-key re-emit. Between those reads a source edit would let stage N build
\ from one revision and stage N+1 from another, silently entering the installed
\ image. The pin records each path's content digest on first read and
\ re-verifies on every reload; a drifted file throws E-BUILD-BOOT-DRIFT and
\ fails the build.
\
\ Design: per-file, keyed by SHA-256 of the path (fixed 32 bytes, independent of
\ path length), value = SHA-256 of the file content. Per-file (not one digest
\ over a concatenated manifest) so the map spans all three emissions with no
\ separate file list to drift, and so a mismatch names the exact drifted path.
\ Baking the combined pin into the image for boot-time reload verification needs
\ engine work and is tracked by dot habu-boot-pin-bake.
: BF-PIN-RESET ( -- )
   0 BF-PIN-N ! ;

: BF-PIN-ON! ( -- )
   BF-TRUE BF-PIN-ON ! ;

: BF-PIN-OFF! ( -- )
   BF-FALSE BF-PIN-ON ! ;

: BF-PIN-KEY@ ( n -- ptr a )
   BF-STAMP-DG-U * BF-PIN-KEYS + ;

: BF-PIN-DIG@ ( n -- ptr a )
   BF-STAMP-DG-U * BF-PIN-DIGS + ;

: BF-PIN-COMPUTE ( ptr u8 n -- )
   2dup BF-PIN-KEYBUF SHA256
   BF-PIN-DIGBUF SHA256-FILE dup 0 <> if throw then drop ;

: BF-PIN-MATCH? ( n -- bool ) {: row:n :}
   BF-PIN-KEYBUF BF-STAMP-DG-U row BF-PIN-KEY@ BF-STAMP-DG-U STR= ;

: BF-PIN-FIND ( -- n )
   0 begin dup BF-PIN-N @ < while
      dup BF-PIN-MATCH? if exit then
      1 +
   repeat drop -1 ;

: BF-PIN-APPEND ( -- )
   BF-PIN-N @ BF-PIN-CAP >= if E-STR-CAPACITY throw then
   BF-PIN-KEYBUF BF-PIN-N @ BF-PIN-KEY@ BF-STAMP-DG-U BYTE-COPY
   BF-PIN-DIGBUF BF-PIN-N @ BF-PIN-DIG@ BF-STAMP-DG-U BYTE-COPY
   BF-PIN-N @ 1 + BF-PIN-N ! ;

: BF-PIN-VERIFY-AT ( n -- )
   BF-PIN-DIG@ BF-STAMP-DG-U BF-PIN-DIGBUF BF-STAMP-DG-U STR= 0= if
      E-BUILD-BOOT-DRIFT throw
   then ;

: BF-PIN-FILE ( ptr u8 n -- )
   BF-PIN-ON @ 0= if 2drop exit then
   BF-PIN-COMPUTE
   BF-PIN-FIND dup 0 < if drop BF-PIN-APPEND exit then
   BF-PIN-VERIFY-AT ;

: BF-APPEND-SOURCE ( ptr u8 n ptr u8 n -- ) {: out:ptr outu src:ptr srcu :}
   src srcu BF-PIN-FILE
   src srcu out outu BF-OUT$ BF-APPEND-FILE-STREAM
   out outu BF-APPEND-LF ;

public
EXPORT BF-APPEND-SOURCE
EXPORT BF-BUILD-RC
private

;package

package BUILD-EXT
using BUILD-FIXPOINT

variable PATH-A
variable PATH-U
PTR-VARIABLE KEEP-A
variable KEEP-U

: PATH-FIELD ( -- ptr ptr u8 )
   PATH-A 0 ptr-field ;

: PATH@ ( -- ptr u8 )
   PATH-FIELD @ ;

: KEEP@ ( -- ptr u8 )
   KEEP-A @ ;

: CLEAR ( -- )
   0 PATH-U !
   0 KEEP-U ! ;

: SET ( ptr u8 n -- )
   PATH-U !
   PATH-FIELD ! ;

: KEEP-SET ( ptr u8 n -- )
   KEEP-U !
   KEEP-A ! ;

CLEAR

public

: ASSERT-EMPTY ( -- )
   PATH-U @ 0 <> KEEP-U @ 0 <> or if
      s" build-fixpoint: production extension enabled" BF-BUILD-RC die
   then ;

: APPEND ( ptr u8 n -- ) {: out:ptr outu:n :}
   KEEP-U @ 0 > if out outu KEEP@ KEEP-U @ BF-APPEND-SOURCE then
   PATH-U @ 0 > if out outu PATH@ PATH-U @ BF-APPEND-SOURCE then ;

: APPEND-KEEP ( ptr u8 n -- ) {: out:ptr outu:n :}
   KEEP-U @ 0 > if
      out outu KEEP@ KEEP-U @ BF-APPEND-SOURCE
   then ;

;using
;package

package BUILD-FIXPOINT

: BF-READ-SOURCE ( ptr u8 n -- )
   BF-SOURCE-BUF BF-SOURCE-CAP READ-ALL BF-SOURCE-LEN ! ;

: BF-SOURCE-HAS? ( ptr u8 n -- bool )
   BF-SOURCE-BUF BF-SOURCE-LEN @ 2swap CONTAINS? ;

\ typed STR:FIND-SUB boundary: route byte-lengths through the STR: role surface,
\ project the option<CAD-NUM:index> result back to the switchover option<idx>.
;package

package CAD-NUM
public
: BF-IX>N ( CAD-NUM:index -- n ) INDEX>N ;
;package

package BUILD-FIXPOINT

: BF-FIND ( ptr u8 n ptr u8 n -- option<idx> ) {: a:ptr u:n b:ptr v:n :}
   a u STR:LENGTH b v STR:LENGTH STR:FIND-SUB MATCH option
     none OF OPTION:NONE ENDOF
     some OF CAD-NUM:BF-IX>N >IDX OPTION:SOME ENDOF
   ;MATCH ;

: BF-SOURCE-FIND ( ptr u8 n -- option<idx> )
   BF-SOURCE-BUF BF-SOURCE-LEN @ 2swap BF-FIND ;

: BF-SOURCE-REQUIRE ( option<idx> -- n )
   MATCH option
     none OF s" build-fixpoint: source split marker missing" BF-BUILD-RC die ENDOF
     some OF IDX>N ENDOF
   ;MATCH ;

: BF-SRC-C@ ( n -- u8 )
   BF-SOURCE-BUF + c@ ;

: BF-SRC-C! ( u8 n -- )
   BF-SOURCE-BUF + c! ;

: BF-STRIP-WRITE ( u8 -- )
   BF-STRIP-W @ BF-SRC-C!
   BF-STRIP-W @ 1+ BF-STRIP-W ! ;

: BF-STRIP-R++ ( -- )
   BF-STRIP-R @ 1+ BF-STRIP-R ! ;

: BF-STRIP-SKIP-PAREN ( -- )
   BF-STRIP-R++
   begin BF-STRIP-R @ BF-STRIP-OFF @ < while
      BF-STRIP-R @ BF-SRC-C@ 41 = if BF-STRIP-R++ exit then
      BF-STRIP-R++
   repeat ;

: BF-STRIP-KEEP ( -- )
   BF-STRIP-R @ BF-SRC-C@ BF-STRIP-WRITE
   BF-STRIP-R++ ;

: BF-STRIP-RANGE ( n -- n )
   BF-STRIP-OFF !
   0 BF-STRIP-R !
   0 BF-STRIP-W !
   begin BF-STRIP-R @ BF-STRIP-OFF @ < while
      BF-STRIP-R @ BF-SRC-C@ 40 = if
         BF-STRIP-SKIP-PAREN
      else
         BF-STRIP-KEEP
      then
   repeat
   BF-STRIP-W @ ;

: BF-APPEND-SOURCE-BEFORE ( ptr u8 n ptr u8 n ptr u8 n -- ) {: out:ptr outu:n src:ptr srcu:n mark:ptr marku:n :}
   src srcu BF-READ-SOURCE
   mark marku BF-SOURCE-FIND BF-SOURCE-REQUIRE {: off:n :}
   out outu BF-SOURCE-BUF off BF-APPEND-BYTES
   out outu BF-APPEND-LF ;

: BF-APPEND-SOURCE-BEFORE-STRIPPED ( ptr u8 n ptr u8 n ptr u8 n -- ) {: out:ptr outu:n src:ptr srcu:n mark:ptr marku:n :}
   src srcu BF-READ-SOURCE
   mark marku BF-SOURCE-FIND BF-SOURCE-REQUIRE BF-STRIP-RANGE {: kept:n :}
   out outu BF-SOURCE-BUF kept BF-APPEND-BYTES
   out outu BF-APPEND-LF ;

: BF-APPEND-SOURCE-FROM ( ptr u8 n ptr u8 n ptr u8 n -- ) {: out:ptr outu:n src:ptr srcu:n mark:ptr marku:n :}
   src srcu BF-READ-SOURCE
   mark marku BF-SOURCE-FIND BF-SOURCE-REQUIRE {: off:n :}
   out outu BF-SOURCE-BUF off + BF-SOURCE-LEN @ off - BF-APPEND-BYTES
   out outu BF-APPEND-LF ;

: BF-SOURCE-MUST-HAVE ( ptr u8 n -- )
   BF-SOURCE-HAS? 0= if s" build-fixpoint: native emitter shape missing" BF-BUILD-RC die then ;

: BF-SOURCE-MUST-LACK ( ptr u8 n -- )
   BF-SOURCE-HAS? if s" build-fixpoint: unsafe native emitter shape" BF-BUILD-RC die then ;

\ habu2.f/habu1.f preflight history: the typed-shape/bare-locals asserts were
\ retired for the blocking stage compile (`LOWER-CERT-HOOK:INSTALL` checks every
\ emitter word), and the final same-type codegen-role asserts (habu2
\ `CLOC-MAIN LABEL@ B,` must-have / `CLOC-MAIN @ B ;` must-lack; habu1
\ `14 SP SPAWN-ADESC-OFF SZA-I @ + STR,` must-have / `... + over + STR,`
\ must-lack) retired for the structural check tools/codegen-role-test.f
\ (gate suite codegen-role): it runs the extracted real emitters on the live
\ arm64 primitives and asserts the label-relative branch fixup and the exact
\ descriptor-slot store progression, with corruption fixtures covering both
\ historic bad forms.

\ icode.f is emitted after the checker-boot hook reinstall, so the stage
\ compile checks it, and the BLOCKING BF-CERTIFY static scan also covers its
\ typed shape (VERIFY:SOURCE-BUF checks the whole emitted source, the
\ BFR-CHECK-OFF prelude window included, and a reject kills the build), so
\ the typed-shape asserts retired with habu1/habu2's. Kept below:
\ runtime invariants the checker cannot express -- fail-closed mmap error
\ handling, and the no-static-allot executable-memory shape (JIT code/label/
\ fixup storage must live in mmap'd regions, never `create ... allot` data
\ space).
: BF-PREFLIGHT-ICODE ( -- )
   s" src/arch/arm64/icode.f" BF-READ-SOURCE
   s" icode: code mmap failed" BF-SOURCE-MUST-HAVE
   s" icode: table mmap failed" BF-SOURCE-MUST-HAVE
   s" create CODE CODE-CAP-BYTES allot" BF-SOURCE-MUST-LACK
   s" create LBLP LBL-CAP cells allot" BF-SOURCE-MUST-LACK
   s" create FXS 2048 cells allot" BF-SOURCE-MUST-LACK ;

\ habu1.f/habu2.f preflight retired (see the history comment above BF-PREFLIGHT-ICODE):
\ REG-PRIM/FPRIM/FPRIM-L/SPAWN-DUP2-ACTION/SPAWN-CHDIR-ACTION and friends are emitted
\ after `LOWER-CERT-HOOK:INSTALL` and so are compiled checked (a stack-effect regression fails
\ the stage compile, blocking); the residual same-type codegen roles the checker cannot
\ express (label-relative branch fixup, spawn descriptor-slot store progression) are
\ covered by the structural tools/codegen-role-test.f (gate suite codegen-role). Only
\ BF-PREFLIGHT-ICODE remains here, for icode's mmap/no-static-allot runtime invariants.
: BF-PREFLIGHT ( -- )
   BF-REQUIRE-WATERMARK
   BF-PREFLIGHT-ICODE ;

: BF-TARGET-UNKNOWN ( -- )
   s" build-fixpoint: unknown target" BF-BUILD-RC die ;

: BF-APPEND-TARGET-LAYOUT ( ptr u8 n -- ) {: out:ptr outu :}
   HB-TARGET-LINUX? if
      out outu s" src/os/linux/layout.f" BF-APPEND-SOURCE
      exit
   then
   HB-TARGET-MACOS? if
      out outu s" src/os/macos/layout.f" BF-APPEND-SOURCE
      exit
   then
   BF-TARGET-UNKNOWN ;

: BF-APPEND-TARGET-SYS ( ptr u8 n -- ) {: out:ptr outu :}
   HB-TARGET-LINUX? if
      out outu s" src/os/linux/sys.f" BF-APPEND-SOURCE
      exit
   then
   HB-TARGET-MACOS? if
      out outu s" src/os/macos/sys.f" BF-APPEND-SOURCE
      exit
   then
   BF-TARGET-UNKNOWN ;

: BF-APPEND-TARGET-PROC-WATCH ( ptr u8 n -- ) {: out:ptr outu:n :}
   HB-TARGET-LINUX? if
      out outu s" src/os/linux/proc-watch.f" BF-APPEND-SOURCE
      exit
   then
   HB-TARGET-MACOS? if
      out outu s" src/os/macos/proc-watch.f" BF-APPEND-SOURCE
      exit
   then
   BF-TARGET-UNKNOWN ;

: BF-APPEND-TARGET-PROC-CONTROL ( ptr u8 n -- ) {: out:ptr outu:n :}
   HB-TARGET-LINUX? if
      out outu s" src/os/linux/proc-control.f" BF-APPEND-SOURCE
      exit
   then
   HB-TARGET-MACOS? if
      out outu s" src/os/macos/proc-control.f" BF-APPEND-SOURCE
      exit
   then
   BF-TARGET-UNKNOWN ;

: BF-APPEND-TARGET-FLAG ( ptr u8 n -- ) {: out:ptr outu :}
   HB-TARGET-LINUX? if
      out outu s" src/os/linux/target.f" BF-APPEND-SOURCE
      exit
   then
   HB-TARGET-MACOS? if
      out outu s" src/os/macos/target.f" BF-APPEND-SOURCE
      exit
   then
   BF-TARGET-UNKNOWN ;

: BF-APPEND-IMAGE-BYTES ( ptr u8 n -- ) {: out:ptr outu :}
   out outu s" src/os/image-bytes.f" BF-APPEND-SOURCE ;

: BF-APPEND-TARGET-IMAGE ( ptr u8 n -- ) {: out:ptr outu :}
   HB-TARGET-LINUX? if
      out outu s" src/os/linux/elf.f" BF-APPEND-SOURCE
      out outu s" src/os/linux/sign.f" BF-APPEND-SOURCE
      exit
   then
   HB-TARGET-MACOS? if
      out outu s" src/os/macos/macho.f" BF-APPEND-SOURCE
      out outu s" src/os/macos/sign2.f" BF-APPEND-SOURCE
      exit
   then
   BF-TARGET-UNKNOWN ;

: BF-APPEND-ROLES ( ptr u8 n -- ) {: out:ptr outu:n :}
   out outu s" src/core/roles.f" BF-APPEND-SOURCE ;

: BF-APPEND-CHECKER-BOOT ( ptr u8 n -- ) {: out:ptr outu:n :}
   out outu s" src/core/util.f" BF-APPEND-SOURCE
   out outu s" src/core/cell.f" BF-APPEND-SOURCE
   out outu s" src/core/pointer-storage.f" BF-APPEND-SOURCE
   out outu s" src/core/engine-error.f" BF-APPEND-SOURCE
   out outu s" src/core/exec-vector.f" BF-APPEND-SOURCE
   out outu s" src/core/checker.f" BF-APPEND-SOURCE
   out outu s" src/core/engine-error-effects.f" BF-APPEND-SOURCE
   out outu s" src/core/lower-cert-base.f" BF-APPEND-SOURCE
   out outu s" src/core/render.f" BF-APPEND-SOURCE
   out outu s" src/core/check-hook.f" BF-APPEND-SOURCE
   out outu s" src/core/cell-effects.f" BF-APPEND-SOURCE
   out outu s" src/core/pointer-storage-effects.f" BF-APPEND-SOURCE
   out outu s" src/core/util-effects.f" BF-APPEND-SOURCE
   out outu s" src/core/render-effects.f" BF-APPEND-SOURCE
   out outu s" src/core/checker-effects.f" BF-APPEND-SOURCE
   out outu s" src/core/lower-cert-effects.f" BF-APPEND-SOURCE
   out outu s" src/core/type-schema.f" BF-APPEND-SOURCE
   out outu s" src/core/type-family.f" BF-APPEND-SOURCE
   out outu s" src/core/sumtype.f" BF-APPEND-SOURCE
   out outu s" src/core/layout-buffer.f" BF-APPEND-SOURCE
   out outu s" src/core/layout-valid.f" BF-APPEND-SOURCE
   out outu s" src/core/declaration-transaction.f" BF-APPEND-SOURCE
   out outu s" src/core/generated-declaration.f" BF-APPEND-SOURCE ;

: BF-APPEND-DECL-FILES ( ptr u8 n -- ) {: out:ptr outu:n :}
   \ The shared declaration-event transaction, then the STRUCTURE constructor
   \ generator, then the STRUCTURE declarer (which calls the generator, so it must
   \ come after it), then the ENUM declarer (a pure event-driven leaf) — all after
   \ the checker hook.
   out outu s" src/core/decl-event.f"     BF-APPEND-SOURCE
   out outu s" src/core/structure-make.f" BF-APPEND-SOURCE
   out outu s" src/core/structure-decl.f" BF-APPEND-SOURCE
   out outu s" src/core/enum-decl.f"      BF-APPEND-SOURCE ;

: BF-APPEND-CORE-FILES ( ptr u8 n -- ) {: out:ptr outu:n :}
   out outu s" src/core/structures.f" BF-APPEND-SOURCE ;

: BF-APPEND-CORE-BYTES ( ptr u8 n -- ) {: out:ptr outu:n :}
   out outu s" src/core/bytes.f" BF-APPEND-SOURCE ;

: BF-APPEND-HABU-LAYOUT ( ptr u8 n -- ) {: out:ptr outu :}
   out outu s" src/habu/layout.f" BF-APPEND-SOURCE ;

: BF-APPEND-ENV-BASE ( ptr u8 n -- ) {: out:ptr outu :}
   out outu s" src/os/env-base.f" BF-APPEND-SOURCE ;

: BF-APPEND-SCRIPT-ARGV ( ptr u8 n -- ) {: out:ptr outu :}
   out outu s" src/os/script-argv.f" BF-APPEND-SOURCE ;

: BF-APPEND-COMBINATORS ( ptr u8 n -- ) {: out:ptr outu :}
   out outu s" src/core/combinators.f" BF-APPEND-SOURCE ;

\ The whole-span descriptor write, ahead of driver-io.f (its only caller in a
\ builder) and ahead of aot-decl.f, which is where the AOT artifact writer's
\ closure begins.
: BF-APPEND-FDIO ( ptr u8 n -- ) {: out:ptr outu:n :}
   out outu s" src/habu/fdio.f" BF-APPEND-SOURCE ;

: BF-APPEND-INCLUDE ( ptr u8 n -- ) {: out:ptr outu :}
   out outu SDC-INCLUDE$ BF-APPEND-SOURCE ;

: BF-APPEND-ENUMS ( ptr u8 n -- ) {: out:ptr outu :}
   out outu s" src/core/enums.f" BF-APPEND-SOURCE ;

\ THE BOOT PREFIX, in the engine's own load order.
\
\ These are the files src/habu/habu2.f PFX-LOAD-BASE-FILES makes every engine
\ re-read at boot, and this word is the build's mirror of that list:
\ BF-APPEND-CHECKER-BOOT is PFX-LOAD-CHECKER-FILES, BF-APPEND-DECL-FILES is
\ PFX-LOAD-DECL-FILES, and BF-APPEND-BOOT-CORE is PFX-LOAD-CORE-FILES. Each
\ path is spelled once, in the appender that already owned it.
\
\ The assembly is CERTIFY-ONLY: nothing here is ever fed to a stage compile.
\ The compiling host already carries these definitions from its own boot and
\ the generated stage source rewinds to them (src/habu/hide.f) instead of
\ recompiling them, so their static check has no other home. That check is the
\ only gate they have (dot habu-staged-fixpoint-src-0b5fc6e6), which is why it
\ is a build phase of its own rather than a side effect of the stage source.
: BF-APPEND-BOOT-CORE ( ptr u8 n -- ) {: out:ptr outu:n :}
   out outu BF-APPEND-CORE-FILES
   out outu BF-APPEND-ROLES
   out outu BF-APPEND-CORE-BYTES
   out outu BF-APPEND-TARGET-FLAG
   out outu BF-APPEND-TARGET-LAYOUT
   out outu BF-APPEND-HABU-LAYOUT
   out outu BF-APPEND-ENV-BASE
   out outu BF-APPEND-INCLUDE
   out outu BF-APPEND-ENUMS
   out outu s" src/core/sha256.f" BF-APPEND-SOURCE
   out outu s" src/core/type-family-sha.f" BF-APPEND-SOURCE
   out outu BF-APPEND-COMBINATORS
   out outu s" src/habu/xref.f" BF-APPEND-SOURCE
   out outu s" src/core/generated-declaration-dictionary.f" BF-APPEND-SOURCE
   out outu s" src/core/generated-declaration-protection.f" BF-APPEND-SOURCE
   out outu s" src/core/layout-buffer-seal.f" BF-APPEND-SOURCE
   out outu s" src/core/lower-cert-seal.f" BF-APPEND-SOURCE ;

: BF-APPEND-BOOT-PREFIX ( ptr u8 n -- ) {: out:ptr outu:n :}
   out outu BF-APPEND-CHECKER-BOOT
   out outu BF-APPEND-DECL-FILES
   out outu BF-APPEND-BOOT-CORE ;

\ WHAT IS NOT HERE. Every core-prefix file the compiling host already carries
\ from its own boot is absent: the payload rewinds to the mark at the end of
\ that prefix (BF-STAGE2-HIDE-DEFS) rather than truncating past it and
\ recompiling. src/os/script-argv.f is the exception that proves the rule - the
\ engine loads it AFTER the mark, so the rewind takes it away and the payload
\ has to bring it back. The absent files are still statically checked, as their
\ own build phase: BF-APPEND-BOOT-PREFIX.
: BF-APPEND-COMMON ( ptr u8 n -- ) {: out:ptr outu :}
   out outu s" src/arch/arm64/asm.f" BF-APPEND-SOURCE
   out outu s" src/arch/arm64/icode.f" BF-APPEND-SOURCE
   out outu s" src/arch/arm64/mnem.f" BF-APPEND-SOURCE
   out outu BF-APPEND-TARGET-SYS
   out outu BF-APPEND-SCRIPT-ARGV
   out outu s" src/habu/treeshake.f" BF-APPEND-SOURCE
   out outu s" src/habu/rt.f" BF-APPEND-SOURCE
   out outu s" src/habu/crash.f" BF-APPEND-SOURCE
   out outu BF-APPEND-IMAGE-BYTES
   out outu BF-APPEND-TARGET-IMAGE
   out outu BF-APPEND-TARGET-PROC-WATCH
   out outu BF-APPEND-TARGET-PROC-CONTROL
   out outu s" src/habu/habu1.f" BF-APPEND-SOURCE
   out outu BUILD-EXT:APPEND
   out outu s" src/habu/prof.f" BF-APPEND-SOURCE
   out outu s" src/habu/regalloc.f" BF-APPEND-SOURCE
   out outu s" src/habu/jit.f" BF-APPEND-SOURCE
   out outu s" src/habu/engine-size.f" BF-APPEND-SOURCE
   out outu BF-APPEND-FDIO
   out outu SDC-DECL$ BF-APPEND-SOURCE
   out outu SDC-IDENT$ BF-APPEND-SOURCE
   out outu s" src/habu/habu2.f" BF-APPEND-SOURCE ;

: BF-APPEND-DRIVER-IO ( ptr u8 n -- ) {: out:ptr outu :}
   out outu s" src/habu/driver-io.f" BF-APPEND-SOURCE ;

: BF-APPEND-RUN-PRELUDE ( ptr u8 n -- ) {: out:ptr outu :}
   out outu s" src/habu/hide.f" BF-APPEND-SOURCE
   out outu s" src/habu/prefix-rewind.f" BF-APPEND-SOURCE
   out outu s" BFR-CHECK-OFF" BF-APPEND-LINE
   out outu BF-STAGE2-HIDE-DEFS
   out outu s" LOWER-CERT-HOOK:INSTALL" BF-APPEND-LINE ;

: BF-APPEND-STDIN-RUN-PRELUDE ( ptr u8 n -- ) {: out:ptr outu :}
   out outu BF-APPEND-RUN-PRELUDE ;

: BF-EMIT-PREFIX-SOURCE ( ptr u8 n -- ) {: out:ptr outu:n :}
   out outu BF-RESET-OUT
   out outu BF-APPEND-BOOT-PREFIX ;

: BF-EMIT-SOURCE ( ptr u8 n ptr u8 n -- ) {: out:ptr outu driver:ptr driveru :}
   out outu BF-RESET-OUT
   out outu BF-APPEND-RUN-PRELUDE
   out outu BF-APPEND-COMMON
   out outu COMPILER-BUILD:SEAL
   out outu BF-APPEND-DRIVER-IO
   out outu driver driveru BF-APPEND-SOURCE ;

: BF-EMIT-RUN-SOURCE ( ptr u8 n ptr u8 n -- ) {: out:ptr outu driver:ptr driveru :}
   out outu BF-RESET-OUT
   out outu BF-APPEND-RUN-PRELUDE
   out outu BF-APPEND-COMMON
   out outu COMPILER-BUILD:SEAL
   out outu BF-APPEND-DRIVER-IO
   out outu driver driveru BF-APPEND-SOURCE ;

: BF-EMIT-STDIN-RUN-SOURCE ( ptr u8 n ptr u8 n -- ) {: out:ptr outu driver:ptr driveru :}
   out outu BF-RESET-OUT
   out outu BF-APPEND-STDIN-RUN-PRELUDE
   out outu BF-APPEND-COMMON
   out outu COMPILER-BUILD:SEAL
   out outu BF-APPEND-DRIVER-IO
   out outu SDC-ARM$ BF-APPEND-SOURCE
   out outu SDC-AOT$ BF-APPEND-SOURCE
   out outu SDC-FILE$ BF-APPEND-SOURCE
   out outu driver driveru BF-APPEND-SOURCE ;

\ Snapshot keep surface: what the persisted image must carry that the rewind
\ above does not already leave live. SNAP-TAIL-MARK opens the builder-only tail
\ after it, and snap.f retires everything from that marker before SNAP:PERSIST.
\
\ This used to re-read the whole core prefix here. It could not work twice: the
\ prefix installs into one-shot `defer` cells that the first load seals, so the
\ second load's rows went nowhere - and every snapshot carried the orphaned
\ first copy in its persisted DATA. The keep surface is now what it always
\ meant: src/os/script-argv.f, which the engine loads AFTER the mark and the
\ rewind therefore takes away, plus whatever an extension build keeps.
: BF-APPEND-SNAP-KEEP ( ptr u8 n -- ) {: out:ptr outu:n :}
   out outu BF-APPEND-SCRIPT-ARGV
   out outu BUILD-EXT:APPEND-KEEP ;

: BF-APPEND-TARGET-REPL-TERM ( ptr u8 n -- ) {: out:ptr outu:n :}
   HB-TARGET-LINUX? if
      out outu s" src/os/linux/repl-term.f" BF-APPEND-SOURCE
      exit
   then
   HB-TARGET-MACOS? if
      out outu s" src/os/macos/repl-term.f" BF-APPEND-SOURCE
      exit
   then
   BF-TARGET-UNKNOWN ;

: BF-APPEND-SNAP-REPL ( ptr u8 n -- ) {: out:ptr outu:n :}
   out outu BF-APPEND-TARGET-REPL-TERM
   out outu s" src/habu/repl.f" BF-APPEND-SOURCE
   out outu s" src/habu/debug-watch.f" BF-APPEND-SOURCE
   out outu s" src/habu/stepper.f" BF-APPEND-SOURCE
   out outu s" src/habu/debug.f" BF-APPEND-SOURCE ;

: BF-APPEND-SNAP-MARK ( ptr u8 n -- )
   s" : SNAP-TAIL-MARK ( -- ) ;" BF-APPEND-LINE ;

: BF-APPEND-SNAP-BUILD ( ptr u8 n -- ) {: out:ptr outu:n :}
   out outu s" src/arch/arm64/asm.f" BF-APPEND-SOURCE
   out outu s" src/arch/arm64/icode.f" BF-APPEND-SOURCE
   out outu s" src/arch/arm64/mnem.f" BF-APPEND-SOURCE
   out outu BF-APPEND-TARGET-SYS
   out outu s" src/habu/treeshake.f" BF-APPEND-SOURCE
   out outu s" src/habu/rt.f" BF-APPEND-SOURCE
   out outu s" src/habu/crash.f" BF-APPEND-SOURCE
   out outu BF-APPEND-IMAGE-BYTES
   out outu BF-APPEND-TARGET-IMAGE
   out outu BF-APPEND-TARGET-PROC-WATCH
   out outu BF-APPEND-TARGET-PROC-CONTROL
   out outu s" src/habu/habu1.f" BF-APPEND-SOURCE
   out outu s" src/habu/prof.f" BF-APPEND-SOURCE
   out outu s" src/habu/regalloc.f" BF-APPEND-SOURCE
   out outu s" src/habu/jit.f" BF-APPEND-SOURCE
   out outu s" src/habu/engine-size.f" BF-APPEND-SOURCE
   out outu BF-APPEND-FDIO
   out outu SDC-DECL$ BF-APPEND-SOURCE
   out outu SDC-IDENT$ BF-APPEND-SOURCE
   out outu s" src/habu/habu2.f" BF-APPEND-SOURCE
   out outu BF-APPEND-DRIVER-IO ;

: BF-APPEND-SNAP-PRESEAL ( ptr u8 n -- ) {: out:ptr outu:n :}
   out outu BF-APPEND-STDIN-RUN-PRELUDE
   out outu BF-APPEND-SNAP-KEEP
   out outu BF-APPEND-SNAP-REPL
   out outu BF-APPEND-SNAP-MARK ;

: BF-APPEND-SNAP-SEALED ( ptr u8 n ptr u8 n -- ) {: out:ptr outu:n driver:ptr driveru:n :}
   out outu COMPILER-BUILD:SEAL
   out outu BF-APPEND-SNAP-BUILD
   out outu driver driveru BF-APPEND-SOURCE ;

: BF-EMIT-SNAP-RUN-SOURCE ( ptr u8 n ptr u8 n -- ) {: out:ptr outu:n driver:ptr driveru:n :}
   out outu BF-RESET-OUT
   out outu BF-APPEND-SNAP-PRESEAL
   out outu driver driveru BF-APPEND-SNAP-SEALED ;

\ Snapshot source with one extra test source appended after the builder tail
\ (snap-lib.f and the target image words are live by then) and just before the
\ driver, so a snapshot fixture can plant return-stack canaries or arm
\ the test-only close seam before SNAP:PERSIST runs. Builder-only: the injected
\ source sits above the SNAP-TAIL-MARK, so RETIRE-AND-PERSIST forgets it before the
\ written and nothing reaches a shipped snapshot.
: BF-APPEND-SNAP-SEALED-WITH ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: out:ptr outu:n inject:ptr injectu:n driver:ptr driveru:n :}
   out outu COMPILER-BUILD:SEAL
   out outu BF-APPEND-SNAP-BUILD
   out outu inject injectu BF-APPEND-SOURCE
   out outu driver driveru BF-APPEND-SOURCE ;

: BF-EMIT-SNAP-RUN-SOURCE-WITH ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: out:ptr outu:n inject:ptr injectu:n driver:ptr driveru:n :}
   out outu BF-RESET-OUT
   out outu BF-APPEND-SNAP-PRESEAL
   out outu inject injectu driver driveru BF-APPEND-SNAP-SEALED-WITH ;

: BF-CLOSE-CMP ( -- )
   BF-FDA @ dup 0 >= if close else drop then
   BF-FDB @ dup 0 >= if close else drop then
   -1 BF-FDA !
   -1 BF-FDB ! ;

: BF-OPEN-CMP ( ptr u8 n ptr u8 n -- ) {: a:ptr au b:ptr bu :}
   -1 BF-FDA !
   -1 BF-FDB !
   a au FS-PATHZ open-rd BF-FDA !
   BF-FDA @ 0 < if E-BUILD-PATH throw then
   b bu FS-PATHZ open-rd BF-FDB !
   BF-FDB @ 0 < if
      BF-FDA @ close -1 BF-FDA ! E-BUILD-PATH throw
   then ;

: BF-READ-A ( -- n )
   BF-FDA @ BF-CMP-A BF-CMP-CAP read BF-RA !
   BF-RA @ 0 < if BF-CLOSE-CMP E-FS-IO throw then
   BF-RA @ BF-CMP-CAP > if BF-CLOSE-CMP E-FS-IO throw then
   BF-RA @ ;

: BF-READ-B ( -- n )
   BF-FDB @ BF-CMP-B BF-CMP-CAP read BF-RB !
   BF-RB @ 0 < if BF-CLOSE-CMP E-FS-IO throw then
   BF-RB @ BF-CMP-CAP > if BF-CLOSE-CMP E-FS-IO throw then
   BF-RB @ ;

: BF-FILE= ( ptr u8 n ptr u8 n -- bool )
   BF-OPEN-CMP
   begin
      BF-READ-A BF-RA !
      BF-READ-B BF-RB !
      BF-RA @ BF-RB @ <> if BF-CLOSE-CMP BF-FALSE exit then
      BF-RA @ 0= if BF-CLOSE-CMP BF-TRUE exit then
      BF-CMP-A BF-RA @ BF-CMP-B BF-RB @ STR= 0= if
         BF-CLOSE-CMP BF-FALSE exit
      then
   again ;

: BF-TMP-FILE= ( ptr u8 n ptr u8 n -- bool ) {: an:ptr anu bn:ptr bnu :}
   an anu BF-A-PATH BF-TMP> BF-A-LEN !
   bn bnu BF-B-PATH BF-TMP> BF-B-LEN !
   BF-A-PATH BF-A-LEN @ BF-B-PATH BF-B-LEN @ BF-FILE= ;

: BF-PREFIX-SOURCE ( -- )
   s" prefix-src" BF-EMIT-PREFIX-SOURCE ;

: BF-STAGE2-SOURCE ( -- )
   s" stage2-src" s" src/habu/stage2.f" BF-EMIT-SOURCE ;

: BF-STDIN-SOURCE ( -- )
   s" stage2-src" SDC-DRIVER$ BF-EMIT-STDIN-RUN-SOURCE ;

: BF-SNAP-SOURCE ( -- )
   s" hb-snap-src" s" src/habu/snap.f" BF-EMIT-SNAP-RUN-SOURCE ;

: BF-CERT-LABEL$ ( -- ptr u8 n )
   BF-CERT-LAB-A@ BF-CERT-LAB-U @ ;

: BF-CERT-PATH$ ( -- ptr u8 n )
   BF-CERT-PATH-A@ BF-CERT-PATH-U @ ;

: BF-CERTIFY-INPUT! ( ptr u8 n ptr u8 n -- )
   BF-CERT-PATH-U !
   BF-CERT-PATH-A!
   BF-CERT-LAB-U !
   BF-CERT-LAB-A! ;

\ The one raw checking-disabled line a generated source must never carry: the
\ generated-output tests (tools/build-fixpoint-test.f) pin its absence in the
\ real emitted stage2 and snap sources, and BFR-CHECK-OFF (src/habu/hide.f) is
\ the named boundary those sources call instead.
: BF-BOUNDARY-RAW-OFF$ ( -- ptr u8 n )
   s\" \n0 set-check\n" ;

: BF-CERTIFY-ACT ( -- )
   BF-CERT-LABEL$ DIAG-FILE!
   BF-FALSE DIAG-JSON!
   BF-CERT-DIAG BF-CERT-DIAG-CAP DIAG-BUFFER!
   BF-CERT-PATH$ FILE-SIZE MEM-ALLOC-64K-SPAN {: buf:ptr cap:n :}
   BF-CERT-PATH$ buf cap READ-ALL {: u:n :}
   buf u VERIFY:SOURCE-BUF ;

: BF-CERTIFY-RC ( ptr u8 n ptr u8 n -- n )
   BF-CERTIFY-INPUT!
   0 BF-CERT-DIAG-U !
   [: BF-CERTIFY-ACT ;] catch BF-CERT-RC !
   DIAG-BUFFER$ nip BF-CERT-DIAG-U !
   DIAG-BUFFER-OFF
   BF-CERT-RC @ ;

: BF-CERTIFY-REPORT ( ptr u8 n -- ) {: lab:ptr labu:n :}
   s" certify: " type lab labu type
   s"  rejected rc " type BF-CERT-RC @ . s" (blocking)" type cr
   BF-CERT-DIAG-U @ 0 > IF BF-CERT-DIAG BF-CERT-DIAG-U @ type cr THEN ;

\ BLOCKING: a generated stage source that fails VERIFY:SOURCE-BUF kills the
\ build (E-BUILD-CERTIFY) after reporting the diagnostic. This is the staged trust induction (dot
\ habu-staged-fixpoint-src-0b5fc6e6): the RUNNING stage-N engine certifies the
\ full assembled stage-N+1 source - the exact bytes the build consumes,
\ checker-boot prelude included - before any stage compile, so a type error in
\ emitted engine source can no longer warn its way into an installed binary.
\ No escape hatch: the gforth recovery lane (docs/bootstrap.md) reaches this
\ native refresh only after a working bin/hb exists, and a tree whose
\ generated sources reject must be repaired, not installed.
: BF-CERTIFY-GENERATED ( ptr u8 n ptr u8 n -- )
   BF-CERTIFY-RC
   0= IF exit THEN
   BF-CERT-LABEL$ BF-CERTIFY-REPORT
   E-BUILD-CERTIFY throw ;

\ The stage engine reads its source from the fixed `stage2-src` name in the temp
\ root (BF-PREPARE-STAGE-ARGV runs hb-stage with just `-- <tmp>`, no --load), so
\ both build phases emit into that one path. BF-STAGE2-SOURCE writes the stage2
\ source, then BF-STDIN-SOURCE OVERWRITES the same path with the stdin driver
\ source before BF-CERTIFY-STDIN runs. Each certify therefore reads the same
\ physical path but its own phase's distinct content — the `stage2-src`/`stdin-src`
\ argument is the diagnostic label for the phase, not a second file name.
\ The boot prefix's own certify phase. Its bytes are assembled into their own
\ stage-input path and read straight back through VERIFY:SOURCE-BUF; no build
\ step ever compiles that file.
: BF-CERTIFY-PREFIX ( -- )
   s" prefix-src" s" prefix-src" BF-A$ BF-CERTIFY-GENERATED ;

: BF-CERTIFY-STAGE2 ( -- )
   s" stage2-src" s" stage2-src" BF-A$ BF-CERTIFY-GENERATED ;

: BF-CERTIFY-STDIN ( -- )
   s" stdin-src" s" stage2-src" BF-A$ BF-CERTIFY-GENERATED ;

: BF-CERTIFY-SNAP ( -- )
   s" hb-snap-src" s" hb-snap-src" BF-A$ BF-CERTIFY-GENERATED ;

\ Self-check certification census (dot habu-census-assert-the-f3a20b1f). Every
\ certify above fail-closes on any uncheckable/rejected definition, so reaching
\ this report proves Uncheckable and Rejected are 0. CENSUS-COUNT reopens
\ VERIFY to reuse its tokenizer (NEXT-SCAN skips strings and `\`/`( )` comments and
\ each colon body), so the tally is exactly the top-level colon definitions the
\ certify scanner verified. The count is per target: both assemblies include the
\ target's src/os leg, so linux-arm64 and macos-arm64 measure their
\ own totals, mirroring the per-target CODELEN rows in test/gate-build-size.f.
\
\ The total is reported with its two phases beside it because they answer
\ different questions: the boot prefix is what the host already carries and the
\ assembled source is what a stage compile consumes. A phase that silently
\ emptied would otherwise hide inside one sum.
;package

package VERIFY
variable CENSUS-N
: CENSUS-SKIP-BODY ( -- )
   begin NEXT-SCAN dup 0 > while
      s" ;" CORE-STR= if exit then
   repeat 2drop ;
public
: CENSUS-COUNT ( ptr u8 n -- n )
   SOURCE! SCAN-RESET
   0 CENSUS-N !
   begin NEXT-SCAN dup 0 > while
      s" :" CORE-STR= if 1 CENSUS-N +! CENSUS-SKIP-BODY then
   repeat 2drop
   CENSUS-N @ ;
;package

package BUILD-FIXPOINT

: BF-CENSUS-TARGET$ ( -- ptr u8 n )
   HB-TARGET-LINUX? if s" linux-arm64" exit then
   HB-TARGET-MACOS? if s" macos-arm64" exit then
   BF-TARGET-UNKNOWN ;

: BF-CENSUS-COUNT ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u BF-A$ FILE-SIZE MEM-ALLOC-64K-SPAN {: buf:ptr cap:n :}
   a u BF-A$ buf cap READ-ALL {: len:n :}
   buf len VERIFY:CENSUS-COUNT ;

: BF-CENSUS-REPORT ( -- )
   s" prefix-src" BF-CENSUS-COUNT {: pfx:n :}
   s" stage2-src" BF-CENSUS-COUNT {: stg:n :}
   s" self-check census (" type BF-CENSUS-TARGET$ type
   s" ): 0 uncheckable, 0 rejected, certified = " type pfx stg + .
   s"   boot prefix = " type pfx .
   s"   assembled = " type stg . ;

: BF-SRC-DIGEST ( ptr u8 n ptr u8 -- ) {: a:ptr u:n dg:ptr :}
   a u BF-A$ dg SHA256-FILE dup 0 <> if throw then drop ;

: BF-STAGE2-DIGEST ( ptr u8 -- ) {: dg:ptr :}
   s" stage2-src" dg BF-SRC-DIGEST ;

: BF-PREFIX-DIGEST ( ptr u8 -- ) {: dg:ptr :}
   s" prefix-src" dg BF-SRC-DIGEST ;

: BF-RECORD-RESET ( -- )
   0 BF-REC-PREFIX? !
   0 BF-REC-STAGE? !
   0 BF-REC-STDIN? ! ;

: BF-RECORD-PREFIX ( -- )
   BF-REC-PREFIX-DG BF-PREFIX-DIGEST
   -1 BF-REC-PREFIX? ! ;

: BF-RECORD-STAGE ( -- )
   BF-REC-STAGE-DG BF-STAGE2-DIGEST
   -1 BF-REC-STAGE? ! ;

: BF-RECORD-STDIN ( -- )
   BF-REC-STDIN-DG BF-STAGE2-DIGEST
   -1 BF-REC-STDIN? ! ;

: BF-BOOTSTRAP-STAGE ( -- )
   s" stage2-got" BF-REMOVE-TMP
   s" hb-stage" BF-REMOVE-TMP
   s" bin/hb" s" stage2-src" BF-A$ COMPILER-BUILD:RUN BF-RC0
   s" stage2-got" BF-EXPECT
   s" stage2-got" s" hb-stage" BF-RENAME-TMP
   s" hb-stage" BF-CHMOD-X-TMP ;

: BF-RUN-STAGE ( -- )
   s" stage2-got" BF-REMOVE-TMP
   s" hb-stage" BF-RUN-STAGE-TMP BF-RC0
   s" stage2-got" BF-EXPECT ;

: BF-PROMOTE-STAGE ( -- )
   s" stage2-got" s" hb-stage" BF-RENAME-TMP
   s" hb-stage" BF-CHMOD-X-TMP ;

: BF-VERIFY-STAGE ( -- )
   s" hb-stage" BF-CODESIGN-VERIFY-TMP ;

: BF-STAGE-MATCH? ( -- bool )
   s" hb-stage" s" stage2-got" BF-TMP-FILE= ;

: BF-STAGE-FIXPOINT-FROM-SOURCE ( -- )
   BF-BOOTSTRAP-STAGE
   0 BF-GEN !
   0 BF-FOUND !
   begin BF-GEN @ BF-MAX-GENS < while
      BF-RUN-STAGE
      BF-STAGE-MATCH? if
         BF-VERIFY-STAGE
         -1 BF-FOUND !
         BF-MAX-GENS BF-GEN !
      else
         BF-PROMOTE-STAGE
         BF-GEN @ 1 + BF-GEN !
      then
   repeat
   BF-FOUND @ 0= if s" FIXPOINT BROKEN: no convergence after 4 generations" BF-BUILD-RC die then
   s" bin/hb refresh OK: compiler fixpoint" type cr ;

: BF-STAGE-FIXPOINT ( -- )
   BF-PREFLIGHT
   BF-PREFIX-SOURCE
   BF-CERTIFY-PREFIX
   BF-RECORD-PREFIX
   BF-STAGE2-SOURCE
   BF-CERTIFY-STAGE2
   BF-CENSUS-REPORT
   BF-RECORD-STAGE
   BF-STAGE-FIXPOINT-FROM-SOURCE ;

\ ---------------------------------------------------------------------------
\ The chain capture, and the driver that bakes it.
\
\ THE DRIVER OWNS THE PATHS. docs/forth.md is explicit that a build source must
\ not read them out of an environment, so the artifact and the engine that
\ produced it arrive at src/habu/stdin.f as a call spliced into the source this
\ builder generates. The generator lives here, with the rest of the build's path
\ construction, and tools/aot-chain-bake.f is a caller of it.
\
\ THE TWO LITERALS GO IN A COLON BODY, never on a top-level line. An interpret-mode
\ `s"` ALLOTS its bytes at HERE, which CAPTURE-REPL latches as the capture window's
\ DATA base, so a top-level literal made the emitted engine depend on the LENGTH of
\ the paths spliced into it - two builds of one tree from different temporary roots
\ wrote engines differing in 12081 bytes. Compiled into a body the same literal
\ lands in code below the window and the seed's canonical CODE-B0 absorbs it.
\ src/habu/stdin.f DP-MARK refuses the top-level shape by name.
create BF-CHAIN-DRV-BUF FS-PATH-CAP allot   variable BF-CHAIN-DRV-U
create BF-ART-A-BUF FS-PATH-CAP allot       variable BF-ART-A-U
create BF-ART-B-BUF FS-PATH-CAP allot       variable BF-ART-B-U
create BF-ART-DG-A BF-STAMP-DG-U allot
create BF-ART-DG-B BF-STAMP-DG-U allot
variable BF-AGEN
variable BF-AFOUND
variable BF-DRV-R

: BF-CHAIN-DRIVER$ ( -- ptr u8 n )
   BF-CHAIN-DRV-BUF BF-CHAIN-DRV-U @ ;

: BF-CHAIN-DRIVER-PATH! ( -- )
   s" chain-driver.f" BF-CHAIN-DRV-BUF BF-TMP> BF-CHAIN-DRV-U ! ;

: BF-ART-A$ ( -- ptr u8 n )
   s" chain.aot" BF-ART-A-BUF BF-TMP> BF-ART-A-U !
   BF-ART-A-BUF BF-ART-A-U @ ;

: BF-ART-B$ ( -- ptr u8 n )
   s" chain-b.aot" BF-ART-B-BUF BF-TMP> BF-ART-B-U !
   BF-ART-B-BUF BF-ART-B-U @ ;

: BF-CAPTURE-TOOL$ ( -- ptr u8 n )
   s" tools/aot-chain-capture.f" ;

: BF-PREPARE-CAPTURE-ARGV ( ptr u8 n ptr u8 n -- ptr u8 ptr a ) {: exe:ptr exeu:n art:ptr artu:n :}
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   BF-CAPTURE-TOOL$ >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   art artu >LEN PROC-ARGV+
   exe exeu >LEN PROC-ARGV-PREPARE ;

: BF-RUN-CAPTURE ( ptr u8 n ptr u8 n -- n ) {: exe:ptr exeu:n art:ptr artu:n :}
   BF-PREPARE-ENV
   exe exeu art artu BF-PREPARE-CAPTURE-ARGV
   PROC-ENV-PREPARE -1 >FD -1 >FD -1 >FD
   PROC-SPAWN-ARGV-ENV-RAW BF-FINISH-PID ;

: BF-CAPTURE-INTO ( ptr u8 n -- ) {: art:ptr artu:n :}
   s" hb-host" BF-A$ art artu BF-RUN-CAPTURE BF-RC0 ;

: BF-ART-DIGEST ( ptr u8 n ptr u8 -- ) {: p:ptr u:n dg:ptr :}
   p u dg SHA256-FILE dup 0 <> if throw then drop ;

: BF-ART-MATCH? ( -- bool )
   BF-ART-A$ BF-ART-DG-A BF-ART-DIGEST
   BF-ART-B$ BF-ART-DG-B BF-ART-DIGEST
   BF-ART-DG-A BF-STAMP-DG-U BF-ART-DG-B BF-STAMP-DG-U STR= ;

\ TWO CAPTURES, TWO PROCESSES, ONE ANSWER - the promoted acceptance, run in the
\ build and fail-closed. A capture reads a booted engine's live dictionary and
\ writes addresses out of it, so "the same tree gives the same artifact" is a
\ property of the capture and not of the sources; proving it needs a second
\ process, whose ASLR and heap differ, rather than a second call. The loop is
\ BF-STAGE-FIXPOINT-FROM-SOURCE's shape: a generation that disagrees promotes and
\ retries, and running out of generations is a named death, not a warning.
: BF-ARTIFACT-FIXPOINT ( -- )
   BF-ART-A$ BF-CAPTURE-INTO
   0 BF-AGEN !
   0 BF-AFOUND !
   begin BF-AGEN @ BF-MAX-GENS < while
      BF-ART-B$ BF-CAPTURE-INTO
      BF-ART-MATCH? if
         -1 BF-AFOUND !
         BF-MAX-GENS BF-AGEN !
      else
         s" chain-b.aot" s" chain.aot" BF-RENAME-TMP
         BF-AGEN @ 1 + BF-AGEN !
      then
   repeat
   BF-AFOUND @ 0= if
      s" ARTIFACT-FIXPOINT BROKEN: two captures of one engine disagree after 4 generations"
      BF-BUILD-RC die
   then
   s" chain capture OK: two processes wrote the same artifact" type cr ;

: BF-DRV-WS? ( n -- bool ) {: c:n :}
   c 32 = c 9 = or c 13 = or c 10 = or ;

: BF-DRV-LAST ( -- n )
   BF-SOURCE-LEN @ 1 -
   begin dup 0 >= while
      dup BF-SRC-C@ BF-DRV-WS? 0= if exit then
      1 -
   repeat ;

: BF-DRV-TAIL-BAD ( -- )
   s" build-fixpoint: src/habu/stdin.f no longer ends with STDIN-DRIVER:RUN" BF-BUILD-RC die ;

: BF-DRV-TAIL$ ( -- ptr u8 n ) s" STDIN-DRIVER:RUN" ;

\ Everything up to the trailing `STDIN-DRIVER:RUN`, which the generated driver
\ replaces with the artifact declaration plus the same call. Fail closed if the
\ tail moved: a driver assembled around a tail that is no longer there would run
\ some other program.
: BF-DRV-KEEP ( -- n )
   BF-DRV-LAST {: l:n :}
   BF-DRV-TAIL$ {: t:ptr tu:n :}
   l 1 + tu < if BF-DRV-TAIL-BAD then
   l 1 + tu - BF-SOURCE-BUF + tu  t tu STR= 0= if BF-DRV-TAIL-BAD then
   l 1 + tu > if l tu - BF-SRC-C@ BF-DRV-WS? 0= if BF-DRV-TAIL-BAD then then
   l 1 + tu - ;

\ THE ONE SPLIT OF THE STDIN DRIVER, and the only word any generator asks. It
\ reads src/habu/stdin.f into BF-SOURCE-BUF and answers how many of those bytes a
\ generated driver keeps - everything up to the terminal `STDIN-DRIVER:RUN` the
\ generator replaces. Three generators want that: this file's own chain driver,
\ tools/aot-chain-bake.f through it, and test/aot-wid-build.f, which used to
\ carry its own copy of the whitespace scan, the tail token and the fail-closed
\ check. Two spellings of one fact drift, and the fact is which bytes of a
\ shipped source file a build may keep.
: BF-DRV-SOURCE-KEEP ( -- n )
   SDC-DRIVER$ BF-READ-SOURCE
   BF-DRV-KEEP ;

\ A path holding a `"` would end the generated string literal early and the driver
\ would compile some other program. Refuse rather than escape.
: BF-DRV-PATH? ( ptr u8 n -- ) {: a:ptr u:n :}
   0 BF-DRV-R !
   begin BF-DRV-R @ u < while
      a BF-DRV-R @ + c@ BF-DQ = if
         s" build-fixpoint: a path with a quote cannot be spliced into the driver"
         BF-BUILD-RC die
      then
      BF-DRV-R @ 1 + BF-DRV-R !
   repeat ;

: BF-DRV+ ( ptr u8 n -- ) {: a:ptr u:n :}
   BF-CHAIN-DRIVER$ a u APPEND-FILE ;

\ ( artifact-path producer-engine-path -- ): stdin.f with its tail replaced by the
\ declaration of THIS artifact and the same call.
: BF-CHAIN-DRIVER-FROM ( ptr u8 n ptr u8 n -- ) {: art:ptr artu:n eng:ptr engu:n :}
   art artu BF-DRV-PATH?
   eng engu BF-DRV-PATH?
   BF-CHAIN-DRIVER-PATH!
   BF-DRV-SOURCE-KEEP {: keep:n :}
   BF-CHAIN-DRIVER$ BF-SOURCE-BUF keep WRITE-ALL
   S\" : ART-DECL ( -- ) s\" " BF-DRV+
   art artu BF-DRV+
   S\" \" s\" " BF-DRV+
   eng engu BF-DRV+
   S\" \" STDIN-DRIVER:ARTIFACT! ;\nART-DECL\n" BF-DRV+
   BF-DRV-TAIL$ BF-DRV+
   BF-LF-BUF 1 BF-DRV+ ;

: BF-CHAIN-DRIVER! ( -- )
   BF-ART-A$ s" hb-host" BF-B$ BF-CHAIN-DRIVER-FROM ;

\ ---------------------------------------------------------------------------
\ TWO ENGINES, ONE PREFIX.
\
\ Once the compiler chain is baked, `require src/compiler/native/migrate.f` is a
\ registry no-op in the product and src/core/include.f dies on duplicates, so the
\ product can never capture its own chain. The build therefore emits the same
\ prefix twice and the two emissions differ by ONE parameter - the artifact the
\ driver declares (src/habu/stdin.f STDIN-DRIVER:ARTIFACT!). No mode flag, no
\ second prefix, no second source list:
\
\   hb-host    the CAPTURE HOST. No artifact: today's bin/hb shape. A build
\              artifact, never installed - but the engine every size gate has
\              always measured, and the one the capture runs in.
\   hb-stdin   the PRODUCT, seeded from the artifact captured in hb-host.
\              BF-INSTALL-HB is untouched, so bin/hb is the product by
\              construction.
: BF-EMIT-ENGINE ( ptr u8 n -- ) {: drv:ptr drvu:n :}
   s" stage2-src" drv drvu BF-EMIT-STDIN-RUN-SOURCE
   BF-CERTIFY-STDIN
   BF-RUN-STAGE
   s" stage2-got" s" hb-stdin-mk" BF-RENAME-TMP
   s" hb-stdin-mk" BF-CHMOD-X-TMP
   s" hb-stdin-got" BF-REMOVE-TMP
   s" hb-stdin-mk" BF-RUN-ENV-TMP BF-RC0
   s" hb-stdin-got" BF-EXPECT
   s" hb-stdin-got" s" hb-stdin" BF-RENAME-TMP
   s" hb-stdin" BF-CHMOD-X-TMP
   s" hb-stdin" BF-CODESIGN-VERIFY-TMP ;

\ The capture host and the maker that wrote it. The maker is kept because the
\ CODELEN gate re-runs it with HABU_ENGINE_SIZE_MAP to attribute the engine's
\ bytes, and the engine it must attribute is the host.
: BF-KEEP-HOST ( -- )
   s" hb-stdin" s" hb-host" BF-RENAME-TMP
   s" hb-stdin-mk" s" hb-host-mk" BF-RENAME-TMP ;

: BF-BUILD-STDIN-FROM-STAGE ( -- )
   SDC-DRIVER$ BF-EMIT-ENGINE
   BF-RECORD-STDIN                              \ the HOST's source is the stamp's: the
   BF-KEEP-HOST                                 \ product's names a temporary artifact path
   BF-ARTIFACT-FIXPOINT
   BF-CHAIN-DRIVER!
   BF-CHAIN-DRIVER$ BF-EMIT-ENGINE ;

: BF-BUILD-STDIN ( -- )
   BF-PREFLIGHT
   BF-BUILD-STDIN-FROM-STAGE ;

: BF-BUILD-STDIN-FRESH ( -- )
   BF-STAGE-FIXPOINT
   BF-BUILD-STDIN-FROM-STAGE ;

\ THE SNAPSHOT BUILDS FROM THE CAPTURE HOST, which is the shape snap has always
\ consumed: the dev snapshot's keep surface is the plain engine's, and a seeded
\ product's is not - its chain arrives from the AOT seed at boot, which a snapshot
\ restore skips. Building the image from the host keeps the two products
\ independent instead of making the snapshot a function of the chain.
\ It is a word because tools/build-fixpoint-test.f builds its own snap0 the same
\ way. While each side named its engine itself, the fixture went on naming the
\ product after this rule landed, and the image it built died at boot with
\ `hb: AOT call site unresolved` - the seed's own refusal, reported against an
\ engine production never asks for.
: BF-SNAP-ENGINE$ ( -- ptr u8 n )
   s" hb-host" ;

: BF-BUILD-SNAP-FROM-STDIN ( -- )
   BF-SNAP-SOURCE
   BF-CERTIFY-SNAP
   s" hb-snap0" BF-REMOVE-TMP
   s" hb-new" BF-REMOVE-TMP
   BF-SNAP-ENGINE$ s" hb-snap-src" COMPILER-BUILD:RUN-TMP BF-RC0
   s" hb-snap0" BF-EXPECT
   s" hb-snap0" s" hb-new" BF-RENAME-TMP
   s" hb-new" BF-CODESIGN-FORCE-TMP
   s" hb-new" BF-CHMOD-X-TMP
   s" hb-new" BF-EXPECT
   s" snapshot image OK: candidate validated" type cr ;

: BF-BUILD-ALL ( -- )
   BUILD-EXT:ASSERT-EMPTY
   BF-BUILD-STDIN-FRESH ;

: BF-BUILD-SNAP-FRESH ( -- )
   BUILD-EXT:ASSERT-EMPTY
   BF-BUILD-ALL
   BF-BUILD-SNAP-FROM-STDIN ;

: BF-ENGINE! ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0 <= if E-BUILD-PATH throw then
   u FS-PATH-CAP > if E-BUILD-PATH throw then
   a BF-ENGINE-BUF u BYTE-COPY
   u BF-ENGINE-U ! ;

: BF-ENGINE-RESET ( -- )
   0 BF-ENGINE-U ! ;

: BF-ENGINE$ ( -- ptr u8 n )
   BF-ENGINE-U @ 0 > if BF-ENGINE-BUF BF-ENGINE-U @ exit then
   s" HABU_FIXPOINT_ENGINE" GETENV dup 0 > if exit then drop drop
   s" bin/hb" ;

: BF-INSTALL-TMP$ ( -- ptr u8 n )
   BF-ENGINE$ s" .tmp" BF-INSTALL-TMP-BUF FS-MUT-SUFFIX-PATH BF-INSTALL-TMP-U !
   BF-INSTALL-TMP-BUF BF-INSTALL-TMP-U @ ;

: BF-INSTALL-CLEAN-TMP ( -- )
   BF-INSTALL-TMP$ 2dup EXISTS? if REMOVE-FILE else 2drop then ;

: BF-INSTALL-HB ( -- )
   BF-INSTALL-CLEAN-TMP
   s" hb-stdin" BF-A$ BF-INSTALL-TMP$ COPY-FILE-STREAM
   BF-INSTALL-TMP$ CHMOD-X
   BF-INSTALL-TMP$ BF-ENGINE$ RENAME-FILE
   s" hb-stdin" BF-REMOVE-TMP ;

\ The CAPTURE HOST is kept beside the engine it captured for. A fixture whose
\ subject is source-loading the chain (a capture, a load-time delta, a
\ family-count bracket) cannot run against the product - the product already
\ provides the chain, so the load it wants to measure is a no-op - and
\ rebuilding a host per fixture is the cost this file exists to pay once. The
\ path derives from BF-ENGINE$ the way the install tmp does, so a fixture
\ install under HABU_FIXPOINT_ENGINE keeps its host in its own sandbox and
\ never writes this repo's bin.
: BF-HOST-DST$ ( -- ptr u8 n )
   BF-ENGINE$ s" -host" BF-HOST-DST-BUF FS-MUT-SUFFIX-PATH BF-HOST-DST-U !
   BF-HOST-DST-BUF BF-HOST-DST-U @ ;

: BF-HOST-TMP$ ( -- ptr u8 n )
   BF-ENGINE$ s" -host.tmp" BF-HOST-TMP-BUF FS-MUT-SUFFIX-PATH BF-HOST-TMP-U !
   BF-HOST-TMP-BUF BF-HOST-TMP-U @ ;

: BF-INSTALL-HOST ( -- )
   BF-HOST-TMP$ 2dup EXISTS? if REMOVE-FILE else 2drop then
   s" hb-host" BF-A$ BF-HOST-TMP$ COPY-FILE-STREAM
   BF-HOST-TMP$ CHMOD-X
   BF-HOST-TMP$ BF-HOST-DST$ RENAME-FILE ;

: BF-BIN-HB? ( ptr u8 n -- bool )
   2dup s" bin/hb" STR= if 2drop BF-TRUE exit then
   s" bin/hb-host" STR= ;

: BF-REMOVE-BIN-OTHER ( ptr u8 n -- ) {: path:ptr pathu :}
   path pathu FILE? if
      path pathu BF-BIN-HB? 0= if path pathu REMOVE-FILE then
   then ;

: BF-CLEAN-BIN ( -- )
   s" bin" [: BF-REMOVE-BIN-OTHER ;] WALK-FILES ;

: BF-INSTALL ( -- )
   BUILD-EXT:ASSERT-EMPTY
   BF-BUILD-STDIN-FRESH
   BF-INSTALL-HB
   BF-INSTALL-HOST
   BF-CLEAN-BIN
   s" bin/hb ready (small checked engine, tty REPL + stdin)" type cr ;

: BF-STAMP-PATH! ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0 <= if E-BUILD-PATH throw then
   u FS-PATH-CAP > if E-BUILD-PATH throw then
   a BF-STAMP-PATH-BUF u BYTE-COPY
   u BF-STAMP-PATH-U ! ;

: BF-STAMP-PATH-RESET ( -- )
   0 BF-STAMP-PATH-U ! ;

: BF-STAMP-DIR-XDG? ( -- bool )
   s" XDG_CACHE_HOME" GETENV dup 0= if drop drop BF-FALSE exit then
   s" habu-fixpoint" BF-STAMP-DIR-BUF JOIN-PATH BF-STAMP-DIR-U !
   BF-TRUE ;

: BF-STAMP-DIR-HOME? ( -- bool )
   s" HOME" GETENV dup 0= if drop drop BF-FALSE exit then
   s" .cache/habu-fixpoint" BF-STAMP-DIR-BUF JOIN-PATH BF-STAMP-DIR-U !
   BF-TRUE ;

: BF-STAMP-DIR-TMP ( -- )
   s" TMPDIR" GETENV dup 0= if drop drop s" /tmp" then
   s" habu-fixpoint" BF-STAMP-DIR-BUF JOIN-PATH BF-STAMP-DIR-U ! ;

: BF-STAMP-DIR$ ( -- ptr u8 n )
   BF-STAMP-DIR-XDG? if BF-STAMP-DIR-BUF BF-STAMP-DIR-U @ exit then
   BF-STAMP-DIR-HOME? if BF-STAMP-DIR-BUF BF-STAMP-DIR-U @ exit then
   BF-STAMP-DIR-TMP
   BF-STAMP-DIR-BUF BF-STAMP-DIR-U @ ;

: BF-STAMP-DEFAULT? ( -- bool )
   BF-STAMP-PATH-U @ 0 > if BF-FALSE exit then
   s" HABU_FIXPOINT_STAMP" GETENV nip 0= ;

: BF-STAMP-PATH$ ( -- ptr u8 n )
   BF-STAMP-PATH-U @ 0 > if BF-STAMP-PATH-BUF BF-STAMP-PATH-U @ exit then
   s" HABU_FIXPOINT_STAMP" GETENV dup 0 > if exit then drop drop
   BF-STAMP-DIR$ s" stamp" BF-STAMP-DEF-BUF JOIN-PATH BF-STAMP-DEF-U !
   BF-STAMP-DEF-BUF BF-STAMP-DEF-U @ ;

: BF-PARENT-U ( ptr u8 n -- n ) {: a:ptr u:n :}
   u begin dup 0 > while
      1 -
      a over + c@ BF-SLASH = if exit then
   repeat ;

: BF-STAMP-ENSURE-DIR ( -- )
   BF-STAMP-DEFAULT? if BF-STAMP-DIR$ MAKE-DIRS exit then
   BF-STAMP-PATH$ {: a:ptr u:n :}
   a u BF-PARENT-U {: pu:n :}
   pu 0 > if a pu MAKE-DIRS then ;

: BF-STAMP-BYTES+ ( ptr u8 n -- ) {: a:ptr u:n :}
   BF-STAMP-U @ u + BF-STAMP-CAP > if E-STR-CAPACITY throw then
   a BF-STAMP-BUF BF-STAMP-U @ + u BYTE-COPY
   BF-STAMP-U @ u + BF-STAMP-U ! ;

: BF-STAMP-C+ ( n -- ) {: c:n :}
   BF-STAMP-U @ 1 + BF-STAMP-CAP > if E-STR-CAPACITY throw then
   c BF-STAMP-BUF BF-STAMP-U @ + c!
   BF-STAMP-U @ 1 + BF-STAMP-U ! ;

: BF-STAMP-FRAG+ ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0 <= if E-STR-BOUNDS throw then
   u STR-BYTE-MAX > if E-STR-BOUNDS throw then
   u BF-STAMP-C+
   a u BF-STAMP-BYTES+ ;

: BF-STAMP-DG+ ( ptr u8 n ptr u8 -- ) {: tag:ptr tagu:n dg:ptr :}
   tag tagu BF-STAMP-FRAG+
   dg BF-STAMP-DG-U BF-STAMP-FRAG+ ;

: BF-STAMP-ENGINE+ ( -- )
   BF-ENGINE$ BF-STAMP-DG SHA256-FILE dup 0 <> if throw then drop
   s" engine" BF-STAMP-DG BF-STAMP-DG+ ;

\ ---------------------------------------------------------------------------
\ The stamp key - what the refresh weighs when it decides a rebuild is
\ unnecessary: the key's preimage, and the comparison against the stored stamp.
\
\ The chain capture is the one build input this key cannot reach by digesting the
\ engine and the two emitted stage sources. The capture runs in the host engine
\ from sources read straight from the checkout, so editing the chain changes no
\ emitted byte and no other stamp input, and the refresh would answer `fixpoint:
\ cached` over a product baked from the older chain. Fold the CAPTURE TOOL's whole
\ ordered require/include closure into the preimage instead - ONE row, and it
\ subsumes the chain's own 43 files because the tool requires the chain entry
\ inside its window. A second row over the nested set would be two authorities for
\ one fact. Discovery is the same walk the hb-build artifact cache keys with
\ (tools/event-closure-lib.f over tools/source-discovery.f), so exactly the files
\ that actually load are keyed: an edit to any of them rebuilds, an edit to a file
\ the capture does not load does not. Discovery rejects fail-closed, so a closure
\ that cannot be reproduced fails the refresh rather than being keyed as though it
\ were absent.
\
\ The closure digest is captured ONCE per process and every key derived
\ afterwards replays that capture. The capture happens before the build, because
\ reading the chain afterwards would record content the build never compiled: an
\ edit landing mid-build would be written into the stamp and the next refresh
\ would skip over an engine built from the older chain. Capturing first can only
\ over-invalidate, which costs one rebuild and never ships a stale engine.

create CHAIN-DG 40 allot
variable CHAIN-DONE?
variable CHAIN-I

: ENTRY$ ( -- ptr u8 n )
   BF-CAPTURE-TOOL$ ;

: CLOSURE+ ( CONTENT-KEY:fold ptr u8 n -- CONTENT-KEY:fold ) {: a:ptr u:n :}
   a u EC:BUILD
   0 CHAIN-I !
   begin CHAIN-I @ EC:COUNT < while
      CHAIN-I @ EC:PATH$ CONTENT-KEY:FILE+
      CHAIN-I @ 1+ CHAIN-I !
   repeat ;

\ The ordered closure of one entry file, hashed into a 32-byte digest.
: CHAIN-DIGEST! ( ptr u8 n ptr u8 -- ) {: a:ptr u:n dst:ptr :}
   CONTENT-KEY:OPEN
   s" native-chain-closure-v1" CONTENT-KEY:TEXT+
   a u CLOSURE+
   dst CONTENT-KEY:FINAL ;

: CHAIN-RECORD ( -- )
   CHAIN-DONE? @ if exit then
   ENTRY$ CHAIN-DG CHAIN-DIGEST!
   BF-TRUE CHAIN-DONE? ! ;

\ The preimage tag for the capture-source digest. It is a word because two
\ places must spell it alike: the key below writes the field, and
\ tools/build-fixpoint-test.f rebuilds the same framed field to prove the
\ digest reached the preimage. While the tag was a literal in both, renaming it
\ here left the fixture searching for a tag the key no longer writes - and the
\ fixture's own negative case went on passing, because an absent tag is absent
\ either way.
: BF-STAMP-CAPTURE-TAG$ ( -- ptr u8 n )
   s" capture-src" ;

: BF-STAMP-KEY-BEGIN ( -- )
   CHAIN-RECORD
   0 BF-STAMP-U !
   s" build-fixpoint-stamp-v2" BF-STAMP-FRAG+
   BF-STAMP-ENGINE+
   BF-STAMP-CAPTURE-TAG$ CHAIN-DG BF-STAMP-DG+ ;

\ The boot prefix is a keyed build input in its own right. Its bytes reach no
\ stage source, so without this row an edit to checker.f would leave every
\ other stamp input identical and the refresh would answer `fixpoint: cached`
\ over a product whose prefix had changed underneath it.
: BF-STAMP-PREFIX-KEY+ ( -- )
   BF-STAMP-DG BF-PREFIX-DIGEST
   s" prefix-src" BF-STAMP-DG BF-STAMP-DG+ ;

: BF-STAMP-STAGE-KEY+ ( -- )
   BF-STAMP-DG BF-STAGE2-DIGEST
   s" stage2-src" BF-STAMP-DG BF-STAMP-DG+ ;

: BF-STAMP-STDIN-KEY+ ( -- )
   BF-STAMP-DG BF-STAGE2-DIGEST
   s" stdin-src" BF-STAMP-DG BF-STAMP-DG+ ;

: BF-STAMP-KEY-END ( -- )
   BF-STAMP-BUF BF-STAMP-U @ BF-STAMP-DG SHA256
   BF-STAMP-DG BF-STAMP-KEY SHA256>HEX ;

: BF-STAMP-KEY! ( -- )
   BF-STAMP-KEY-BEGIN
   BF-PREFIX-SOURCE
   BF-STAMP-PREFIX-KEY+
   BF-STAGE2-SOURCE
   BF-STAMP-STAGE-KEY+
   BF-STDIN-SOURCE
   BF-STAMP-STDIN-KEY+
   BF-STAMP-KEY-END ;

: BF-STAMP-RECORDED-KEY! ( -- )
   BF-REC-PREFIX? @ 0= if E-BUILD-STATUS throw then
   BF-REC-STAGE? @ 0= if E-BUILD-STATUS throw then
   BF-REC-STDIN? @ 0= if E-BUILD-STATUS throw then
   BF-STAMP-KEY-BEGIN
   s" prefix-src" BF-REC-PREFIX-DG BF-STAMP-DG+
   s" stage2-src" BF-REC-STAGE-DG BF-STAMP-DG+
   s" stdin-src" BF-REC-STDIN-DG BF-STAMP-DG+
   BF-STAMP-KEY-END ;

: BF-STAMP-READ? ( -- bool )
   BF-STAMP-PATH$ FILE? 0= if BF-FALSE exit then
   BF-STAMP-PATH$ FILE-SIZE BF-STAMP-HEX-U 1 + <> if BF-FALSE exit then
   BF-STAMP-PATH$ BF-STAMP-OLD BF-STAMP-HEX-U 1 + READ-ALL BF-STAMP-HEX-U 1 + <> if BF-FALSE exit then
   BF-STAMP-OLD BF-STAMP-HEX-U + c@ BF-LF <> if BF-FALSE exit then
   BF-TRUE ;

\ Every stamped verb asks this first, so it is where the chain capture lands
\ before the build starts. `--force` answers false without deriving a key, so
\ the capture has to happen ahead of that answer rather than inside the key.
: BF-STAMP-MATCH? ( -- bool )
   CHAIN-RECORD
   BF-FORCE @ 0 <> if BF-FALSE exit then
   BF-STAMP-READ? 0= if BF-FALSE exit then
   BF-STAMP-KEY!
   BF-STAMP-OLD BF-STAMP-HEX-U BF-STAMP-KEY BF-STAMP-HEX-U STR= ;

: BF-STAMP-WRITE ( -- )
   BF-STAMP-ENSURE-DIR
   BF-STAMP-RECORDED-KEY!
   BF-LF BF-STAMP-KEY BF-STAMP-HEX-U + c!
   BF-STAMP-PATH$ BF-STAMP-KEY BF-STAMP-HEX-U 1 + ATOMIC-WRITE-FILE ;

: BF-STAMP-CACHED ( -- )
   s" fixpoint: cached " type
   BF-STAMP-KEY BF-STAMP-PREFIX-U type cr ;

: BF-STDIN-HB= ( -- bool )
   s" hb-stdin" BF-A$ BF-ENGINE$ BF-FILE= ;

: BF-ALL-STAMP ( -- )
   BF-STDIN-HB= if BF-STAMP-WRITE then ;

: BF-BUILD-ALL-CACHED ( -- )
   BF-STAMP-MATCH? if BF-STAMP-CACHED exit then
   BF-RECORD-RESET
   BF-BUILD-ALL
   BF-ALL-STAMP ;

: BF-INSTALL-CACHED ( -- )
   BF-STAMP-MATCH? if BF-STAMP-CACHED exit then
   BF-RECORD-RESET
   BF-INSTALL
   BF-STAMP-WRITE ;

: BF-USAGE ( -- )
   s" usage: tools/build-fixpoint.f [all|install|stage|stdin|snap] [--force]" BF-USAGE-RC die ;

: BF-ARG0= ( ptr u8 n -- bool )
   0 SCRIPT-ARGV$ STR= ;

: BF-ARGN= ( n ptr u8 n -- bool ) {: idx:n a:ptr u:n :}
   idx SCRIPT-ARGV$ a u STR= ;

: BF-PARSE-FORCE ( -- n )
   0 BF-FORCE !
   SCRIPT-ARGC 0= if 0 exit then
   SCRIPT-ARGC 1 - s" --force" BF-ARGN= if
      -1 BF-FORCE !
      SCRIPT-ARGC 1 - exit
   then
   SCRIPT-ARGC ;

: BF-MAIN ( -- )
   BUILD-EXT:ASSERT-EMPTY
   BF-PARSE-FORCE {: argn:n :}
   BF-PIN-RESET BF-PIN-ON!
   argn 0= if BF-BUILD-ALL-CACHED exit then
   argn 1 <> if BF-USAGE then
   s" all" BF-ARG0= if BF-BUILD-ALL-CACHED exit then
   s" install" BF-ARG0= if BF-INSTALL-CACHED exit then
   s" stage" BF-ARG0= if BF-STAGE-FIXPOINT exit then
   s" stdin" BF-ARG0= if BF-BUILD-STDIN-FRESH exit then
   s" snap" BF-ARG0= if BF-BUILD-SNAP-FRESH exit then
   BF-USAGE ;

\ Fail-closed CLI boundary. BTHROW's no-handler path exits with the raw throw
\ code masked to 8 bits and NO diagnostic (a code that is a multiple of 256
\ exits 0), so a crashed refresh child whose BF-RC0 E-BUILD-STATUS throw
\ escaped BF-MAIN used to fail silently with an arbitrary exit code. BF-CLI
\ catches every escaped throw, names it on stderr, and dies with the
\ deterministic build rc so any failure anywhere in the refresh chain is loud
\ and nonzero under every seed.
$80 constant BF-FAIL-CAP
$18 constant BF-FAIL-DG-CAP
create BF-FAIL-BUF BF-FAIL-CAP allot
create BF-FAIL-DG BF-FAIL-DG-CAP allot
variable BF-FAIL-U
variable BF-FAIL-V
variable BF-FAIL-N

: BF-FAIL-C+ ( n -- ) {: c:n :}
   BF-FAIL-U @ 1 + BF-FAIL-CAP > if s" build-fixpoint: fail message overflow" BF-BUILD-RC die then
   c BF-FAIL-BUF BF-FAIL-U @ + c!
   BF-FAIL-U @ 1 + BF-FAIL-U ! ;

: BF-FAIL+ ( ptr u8 n -- ) {: a:ptr u:n :}
   BF-FAIL-U @ u + BF-FAIL-CAP > if s" build-fixpoint: fail message overflow" BF-BUILD-RC die then
   a BF-FAIL-BUF BF-FAIL-U @ + u BYTE-COPY
   BF-FAIL-U @ u + BF-FAIL-U ! ;

: BF-FAIL-DIGITS+ ( -- )
   0 BF-FAIL-N !
   BF-FAIL-V @ 0 = if $30 BF-FAIL-C+ exit then
   begin BF-FAIL-V @ 0 > while
      BF-FAIL-V @ 10 mod $30 +  BF-FAIL-DG BF-FAIL-N @ + c!
      BF-FAIL-N @ 1 + BF-FAIL-N !
      BF-FAIL-V @ 10 / BF-FAIL-V !
   repeat
   begin BF-FAIL-N @ 0 > while
      BF-FAIL-N @ 1 - BF-FAIL-N !
      BF-FAIL-DG BF-FAIL-N @ + c@ BF-FAIL-C+
   repeat ;

: BF-FAIL-CODE+ ( n -- ) {: rc:n :}
   rc 0 < if $2D BF-FAIL-C+ then
   rc 0 < if 0 rc - else rc then BF-FAIL-V !
   BF-FAIL-DIGITS+ ;

: BF-FAIL-NAME+ ( n -- ) {: rc:n :}
   rc E-BUILD-STATUS = if s"  (E-BUILD-STATUS: refresh child failed)" BF-FAIL+ exit then
   rc E-BUILD-PATH = if s"  (E-BUILD-PATH: build artifact missing)" BF-FAIL+ exit then
   rc E-BUILD-CERTIFY = if s"  (E-BUILD-CERTIFY: generated stage source rejected)" BF-FAIL+ exit then ;

: BF-FAIL-DIE ( n -- ) {: rc:n :}
   0 BF-FAIL-U !
   s" build-fixpoint: failed: uncaught throw code " BF-FAIL+
   rc BF-FAIL-CODE+
   rc BF-FAIL-NAME+
   BF-LF BF-FAIL-C+
   BF-FAIL-BUF BF-FAIL-U @ BF-BUILD-RC die ;

variable BF-CLI-RAN

: BF-CLI ( -- )
   BF-CLI-RAN @ if exit then          \ run the CLI at most once per process: the
   BF-TRUE BF-CLI-RAN !                \ tail self-dispatch and either explicit
   [: BF-MAIN ;] catch {: rc:n :}      \ entry wrapper must not both drive a build
   rc 0 <> if rc BF-FAIL-DIE then ;

\ Compiled callers retain direct xts; erase the mutable extension authority so
\ reopening BUILD-EXT cannot select an arbitrary source file.
;package

package BUILD-EXT
undefine SET
undefine CLEAR
undefine PATH@
undefine PATH-FIELD
undefine PATH-A
undefine PATH-U
undefine KEEP-SET
undefine KEEP@
undefine KEEP-A
undefine KEEP-U
;package

package BUILD-FIXPOINT

\ Fail-closed CLI self-dispatch. tools/build-fixpoint-main.f and
\ tools/build-fixpoint-refresh.f are explicit wrappers that call BF-CLI. Before
\ self-dispatch, omitting an entry wrapper from the recovery command left an
\ explicit build verb unclaimed, so the loaded stdin engine read its program
\ from the closed stdin, hit EOF, and exited 0 having built nothing. Run the CLI
\ here when the command line named THIS file (SCRIPT-NAMED-LOAD?; a file another
\ file required stays inert) AND a build verb was passed after `--`.
\
\ That question used to be asked as INCLUDE-DEPTH 0, which was only ever a
\ proxy: it was true because `--load` inlined its argv files into the top-level
\ source stream, so a named file happened to sit at depth 0. Once `--load`
\ started loading them through the require registry (dot
\ habu-make-load-consult-85c88fb3) every named file sat at depth 1 and the
\ dispatch went silently inert - the exact footgun below, restored. The loader
\ records which paths the command line named, so ask that. The verb gate keeps a co-loaded tool that consumes its own
\ `--`-flag args (e.g. tools/hb-build.f under tools/hb-build-test.f, loaded at
\ depth 0 alongside this file) from being hijacked, and BF-CLI is idempotent so
\ calls from either explicit entry wrapper cannot double-build.
: BF-CLI-VERB-ARG0? ( -- bool )
   SCRIPT-ARGC 0 <= if BF-FALSE exit then
   s" all"     BF-ARG0= if BF-TRUE exit then
   s" install" BF-ARG0= if BF-TRUE exit then
   s" stage"   BF-ARG0= if BF-TRUE exit then
   s" stdin"   BF-ARG0= if BF-TRUE exit then
   s" snap"    BF-ARG0= if BF-TRUE exit then
   BF-FALSE ;

: BF-CLI-SELF-DISPATCH ( -- )
   SCRIPT-NAMED-LOAD? 0= if exit then
   BF-CLI-VERB-ARG0? 0= if exit then
   BF-CLI ;

\ ---------------------------------------------------------------------------
\ The BUILD-FIXPOINT surface. Everything above is private to the tool; these
\ are the words other files call, promoted here in one place so the whole
\ export list is readable at a glance. Consumers `require
\ tools/build-fixpoint.f` and then `using BUILD-FIXPOINT`; the tool's own test
\ reopens `package BUILD-FIXPOINT` instead, because it drives internals no
\ caller is meant to see.

public

EXPORT BF-APPEND-BYTES
EXPORT BF-APPEND-BOOT-PREFIX
EXPORT BF-APPEND-COMMON
EXPORT BF-APPEND-DRIVER-IO
EXPORT BF-APPEND-LF
EXPORT BF-APPEND-RUN-PRELUDE
EXPORT BF-BUILD-STDIN-FROM-STAGE
EXPORT BF-CERTIFY-STDIN
EXPORT BF-CHAIN-DRIVER$
EXPORT BF-CHAIN-DRIVER-FROM
EXPORT BF-CHMOD-X-TMP
EXPORT BF-CLI
EXPORT BF-CLI-SELF-DISPATCH
EXPORT BF-CODESIGN-FORCE-TMP
EXPORT BF-CODESIGN-VERIFY-TMP
EXPORT BF-DRV-SOURCE-KEEP
EXPORT BF-EMIT-ENGINE
EXPORT BF-EMIT-SNAP-RUN-SOURCE-WITH
EXPORT BF-EMIT-STDIN-RUN-SOURCE
EXPORT BF-ENGINE!
EXPORT BF-ENGINE$
EXPORT BF-ENGINE-RESET
EXPORT BF-EXPECT
EXPORT BF-OUT$
EXPORT BF-PREFLIGHT
EXPORT BF-RC0
EXPORT BF-REMOVE-TMP
EXPORT BF-RENAME-TMP
EXPORT BF-RESET-OUT
EXPORT BF-RUN-ENV-TMP
EXPORT BF-RUN-STAGE
EXPORT BF-SOURCE-BUF
EXPORT BF-SOURCE-CAP
EXPORT BF-SOURCE-LEN
EXPORT BF-STAGE-FIXPOINT
EXPORT BF-STAGE-FIXPOINT-FROM-SOURCE
EXPORT BF-STAGE2-SOURCE
EXPORT BF-TMP!
EXPORT BF-TMP-RESET

;package

\ Run the CLI with the package CLOSED. A build certifies its own generated
\ sources in this process (BF-CERTIFY-ACT calls VERIFY:SOURCE-BUF), and the
\ checker records and resolves names in whatever package scope is open when it
\ runs, so driving a build from inside BUILD-FIXPOINT would verify the emitted
\ engine source against this package's wordlist instead of the global one.
BUILD-FIXPOINT:BF-CLI-SELF-DISPATCH
