\ run-closure-lint.f - closure lint for the gate phase file sets.
\
\ A phase PASS-stamp key must cover every file that can change the phase
\ verdict. The file sets that build those keys are declared by hand in
\ test/run-files.f, so a set goes stale the moment a member grows a new
\ require/include edge and nobody updates the declaration. A stale set keys on
\ less than the phase reads, which is a FALSE PASS: the missing file changes,
\ the key does not, and the cache serves the old verdict.
\
\ This lint closes that hole. For every member of a declared set it reads the
\ member and reports each EXISTING source file the member names that the set
\ does not list. Two shapes name a file: a `require`/`include` line, and an
\ `s" ...f"` literal. Members under src/ are keyed but not scanned, and a
\ member that names paths as DATA rather than loading them is exempt from the
\ literal scan alone (EXEMPT? below).
\
\ Non-existent paths are skipped on purpose: a generated or temp path spelled
\ in a literal is not a source file the phase reads, and demanding it be a set
\ member would make the lint unsatisfiable.
\
\ Run: it has no entry point of its own. test/run-result-cache-test.f drives it
\ over the real declared sets; test/run-closure-lint-test.f drives it over
\ fixtures.

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/fs.f

package RUN-CLOSURE

private

$100000 constant FILE-CAP
$8000 constant SET-CAP
$09 constant TAB-C
$20 constant BL-C
$22 constant DQ-C
$5C constant BACKSLASH-C

create SET-BUF SET-CAP allot

variable SET-LEN
variable SET-N
variable FILE-A
variable FILE-LEN
variable CUR-A
variable CUR-LEN
variable CUR-EXEMPT
variable ERR#
variable PREPARED
variable REPORT?
-1 REPORT? !                                \ findings print unless a caller silences them

: FILE-A-FIELD ( -- ptr ptr u8 )
   FILE-A 0 ptr-field ;

: CUR-A-FIELD ( -- ptr ptr u8 )
   CUR-A 0 ptr-field ;

: CUR$ ( -- ptr u8 n )
   CUR-A-FIELD @ CUR-LEN @ ;

\ --- the declared set, as a packed [len][bytes] run -------------------------

: SET-ENTRY ( n -- ptr u8 n ) {: want:n :}
   0 want begin dup 0 > while
      swap dup SET-BUF + c@ + 1 + swap 1-
   repeat drop {: off:n :}
   SET-BUF off + 1 + SET-BUF off + c@ ;

: MEMBER? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   0 begin dup SET-N @ < while
      dup SET-ENTRY a u STR= if drop 0 0= exit then
      1+
   repeat drop 0 0= 0= ;

\ --- reading the member under scan ------------------------------------------

: LOAD-CUR ( -- )
   CUR$ FILE-A-FIELD @ FILE-CAP READ-ALL FILE-LEN ! ;

: FILE-C@ ( n -- n ) {: i:n :}
   FILE-A-FIELD @ i + c@ ;

: FILE$ ( n n -- ptr u8 n ) {: i:n j:n :}
   FILE-A-FIELD @ i + j i - ;

: SPACE-C? ( n -- bool ) {: c:n :}
   c BL-C = if 0 0= exit then
   c TAB-C = if 0 0= exit then
   c STR-CR = ;

: SKIP-SPACES ( n n -- n ) {: i:n end:n :}
   i begin dup end < while
      dup FILE-C@ SPACE-C? 0= if exit then
      1+
   repeat ;

: TOKEN-END ( n n -- n ) {: i:n end:n :}
   i begin dup end < while
      dup FILE-C@ SPACE-C? if exit then
      1+
   repeat ;

: LINE-END ( n -- n )
   begin dup FILE-LEN @ < while
      dup FILE-C@ STR-LF = if exit then
      1+
   repeat ;

\ --- the finding -------------------------------------------------------------
\ The message names the SET, not just the file. Its earlier wording was
\ "-> missing <path>", which reads as "that file is not on disk" -- and the one
\ real finding it ever produced was investigated for half a day as a scanner
\ bug on exactly that misreading, while the file sat in the tree and the set
\ was the stale thing.

: SOURCE-REF? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" .f" ENDS-WITH? if 0 0= exit then
   a u s" .fs" ENDS-WITH? ;

: REPORT ( ptr u8 n -- ) {: a:ptr u:n :}
   1 ERR# +!
   REPORT? @ 0= if exit then
   s" result-cache closure: " type
   CUR$ type
   s"  references " type
   a u type
   s" , which the phase file set in test/run-files.f does not list" type cr ;

: REF-CHECK ( ptr u8 n -- ) {: a:ptr u:n :}
   a u SOURCE-REF? 0= if exit then
   a u EXISTS? 0= if exit then
   a u MEMBER? if exit then
   a u REPORT ;

\ --- the two shapes that name a file ----------------------------------------

: REQUIRE-LINE? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" require" STR= if 0 0= exit then
   a u s" include" STR= ;

: LINT-REQUIRE ( n n -- ) {: p:n end:n :}
   p end TOKEN-END {: q:n :}
   p q FILE$ REQUIRE-LINE? 0= if exit then
   q end SKIP-SPACES {: r:n :}
   r end >= if exit then
   r r end TOKEN-END FILE$ REF-CHECK ;

: SQUOTE-AT? ( n n -- bool ) {: i:n end:n :}
   i 2 + end >= if 0 0= 0= exit then
   i FILE-C@ dup $73 = swap $53 = or 0= if 0 0= 0= exit then
   i 1 + FILE-C@ DQ-C = 0= if 0 0= 0= exit then
   i 2 + FILE-C@ BL-C = ;

: SQUOTE-BOUNDARY? ( n n -- bool ) {: i:n off:n :}
   i off <= if 0 0= exit then
   i 1- FILE-C@ SPACE-C? ;

: SQUOTE-CLOSE ( n n -- n ) {: i:n end:n :}
   i begin dup end < while
      dup FILE-C@ DQ-C = if exit then
      1+
   repeat ;

: LINT-SQUOTE-AT ( n n -- ) {: i:n end:n :}
   i 3 + end SQUOTE-CLOSE {: j:n :}
   j end >= if exit then
   i 3 + j FILE$ REF-CHECK ;

: LINT-SQUOTES ( n n -- ) {: off:n end:n :}
   off begin dup end < while
      dup end SQUOTE-AT? if
         dup off SQUOTE-BOUNDARY? if
            dup end LINT-SQUOTE-AT
         then
      then
      1+
   repeat drop ;

: LINT-LINE ( n n -- ) {: off:n end:n :}
   off end SKIP-SPACES {: p:n :}
   p end >= if exit then
   p FILE-C@ BACKSLASH-C = if exit then
   p end LINT-REQUIRE
   CUR-EXEMPT @ 0= if off end LINT-SQUOTES then ;

\ --- members that name paths as data ----------------------------------------
\ Exempt from the LITERAL scan only; their require/include edges are still
\ checked. Each entry is a file whose `s" ...f"` literals are a TABLE of paths
\ the tree acts on, not a list of sources the phase loads. Keying the phase on
\ those rows would be a false coverage claim: the tools that read them accept
\ an arbitrary path from their caller, so the rows are policy data, not a load
\ closure.

: EXEMPT? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   \ the schedulers: they name other phases' files by design
   a u s" test/run-lib.f" STR= if 0 0= exit then
   a u s" test/run-resident.f" STR= if 0 0= exit then
   a u s" test/run-worker.f" STR= if 0 0= exit then
   \ the reviewed dynamic-loader boundaries read by tools/source-discovery.f
   a u s" tools/dynamic-tail-manifest.f" STR= if 0 0= exit then
   \ the audited checker-hook allowlist read by CHECKED-BOUNDARY-LINT, whose
   \ FILE entry lints whatever path its caller hands it
   a u s" tools/hook-sites.f" STR= if 0 0= exit then
   \ the boot-prefix path list. Its rows ARE a load closure - the source bin/hb
   \ re-reads at process start - and they are covered, but by folding rather
   \ than by membership: test/run-engine-set.f FILES walks this same list
   \ into every phase key, so whatever BP-EACH names is keyed the moment it is
   \ named. Demanding the rows also be set members would duplicate that list in
   \ a second place and make adding a prefix file a two-file edit.
   \ test/run-result-cache-test.f ENGINE-KEY holds the folding side up: it
   \ asserts a phase key moves with the checker and schema-registry sources, so
   \ this exemption cannot outlive the coverage that justifies it.
   a u s" tools/boot-pin.f" STR= ;

: CUR-EXEMPT! ( -- )
   0 CUR-EXEMPT !
   CUR$ EXEMPT? if -1 CUR-EXEMPT ! then ;

public

\ Findings are printed by default. A fixture test that provokes them on purpose
\ silences them, so a deliberate finding cannot be mistaken for a red gate in
\ the log; the count is what it asserts either way.
: REPORT-ON ( -- )
   -1 REPORT? ! ;

: REPORT-OFF ( -- )
   0 REPORT? ! ;

\ Allocate the scan buffer once per process. Separate from RESET so a driver
\ that lints several sets pays for the mapping once.
: PREPARE ( -- )
   PREPARED @ 0 <> if exit then
   FILE-CAP MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop FILE-A-FIELD !
   -1 PREPARED ! ;

: RESET ( -- )
   0 ERR# ! ;

: SET-RESET ( -- )
   0 SET-LEN !
   0 SET-N ! ;

: SET+ ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0 <= if E-STR-BOUNDS throw then
   u STR-BYTE-MAX > if E-STR-BOUNDS throw then
   SET-LEN @ u + 1 + SET-CAP > if E-STR-CAPACITY throw then
   u SET-BUF SET-LEN @ + c!
   a SET-BUF SET-LEN @ + 1 + u BYTE-COPY
   SET-LEN @ u + 1 + SET-LEN !
   SET-N @ 1 + SET-N ! ;

\ Lint one member of the current set. src/ members are keyed but not scanned.
: FILE ( ptr u8 n -- ) {: a:ptr u:n :}
   a CUR-A-FIELD !
   u CUR-LEN !
   a u s" src/" STARTS-WITH? if exit then
   CUR-EXEMPT!
   LOAD-CUR
   0 begin dup FILE-LEN @ < while
      dup LINE-END {: end:n :}
      dup end LINT-LINE
      drop end 1 +
   repeat drop ;

\ Lint every member of the current set against the current set.
: RUN ( -- )
   0 begin dup SET-N @ < while
      dup SET-ENTRY FILE
      1+
   repeat drop ;

: FINDINGS ( -- n )
   ERR# @ ;

;package
