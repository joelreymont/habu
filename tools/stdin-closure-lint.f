\ stdin-closure-lint.f - fail-closed drift gate for the stdin driver closure.
\
\ Load after lib/errors.f, lib/string.f, lib/fs.f. Requires the manifest
\ tools/stdin-closure-lib.f. On load it runs a baked fail-closed self-test and
\ then SDCL-LINT over the repo, throwing when a consumer has drifted from the
\ manifest (gate 17e). Consumers and how each is bound to the manifest:
\   tools/build-fixpoint.f - stdin emit + stamp digest name the paths through the
\                            SDC-INCLUDE$/SDC-AOT$/SDC-DRIVER$ accessors (STRUCT).
\   tools/srclist.f        - canonical stdin order names SDC-AOT$/SDC-DRIVER$.
\   tools/bootstrap.sh     - audited launcher: its emit_src stdin branch must
\                            cat every SDC-HOST file (literal paths).
\   test/run-files.f       - TR-UNDER-SOURCE-FILES must list every SDC-KEYED file.
\   tools/hb-build-lib.f   - proven outside the per-file closure: its maker key
\                            hashes the whole engine binary via BF-ENGINE$.

require lib/errors.f
require lib/string.f
require lib/fs.f
require tools/stdin-closure-lib.f

$40000 constant SDCL-CAP
$80 constant SDCL-NAME-CAP
$4A constant SDCL-NAME-RC

create SDCL-BUF SDCL-CAP allot
create SDCL-NAME SDCL-NAME-CAP allot
variable SDCL-LEN
variable SDCL-NAME-U
variable SDCL-BAD

: SDCL-NL ( -- ) 10 emit ;

: SDCL-LOAD ( ptr u8 n -- )
   SDCL-BUF SDCL-CAP READ-ALL SDCL-LEN ! ;

: SDCL-NAME! ( ptr u8 n -- ) {: a:ptr u:n :}
   u SDCL-NAME-CAP > if s" stdin-closure-lint: consumer name too long" SDCL-NAME-RC die then
   a SDCL-NAME u BYTE-COPY
   u SDCL-NAME-U ! ;

: SDCL-NAME$ ( -- ptr u8 n ) SDCL-NAME SDCL-NAME-U @ ;

: SDCL-CONSUMER! ( ptr u8 n -- ) {: a:ptr u:n :}
   a u SDCL-NAME!
   a u SDCL-LOAD ;

: SDCL-HAS? ( ptr u8 n -- bool )
   SDCL-BUF SDCL-LEN @ 2swap CONTAINS? ;

: SDCL-NEED ( ptr u8 n -- ) {: na:ptr nu:n :}
   na nu SDCL-HAS? if exit then
   s" stdin-closure-lint: " type SDCL-NAME$ type
   s"  missing closure entry " type na nu type SDCL-NL
   1 SDCL-BAD +! ;

\ typed-local-lint: allow-bare-local - callback fires per manifest row.
: SDCL-KEYED-NEED ( n ptr u8 n n -- ) {: ix:n pa:ptr pu:n fl:n :}
   fl SDC-KEYED SDC-ROLE? if pa pu SDCL-NEED then ;

\ typed-local-lint: allow-bare-local - callback fires per manifest row.
: SDCL-HOST-NEED ( n ptr u8 n n -- ) {: ix:n pa:ptr pu:n fl:n :}
   fl SDC-HOST SDC-ROLE? if pa pu SDCL-NEED then ;

\ build-fixpoint names paths through the checked accessors, so verify the
\ accessor tokens (not raw paths) appear in its stdin source assembly + digest.
: SDCL-CHECK-BUILD-FIXPOINT ( -- )
   s" tools/build-fixpoint.f" SDCL-CONSUMER!
   s" SDC-INCLUDE$" SDCL-NEED
   s" SDC-AOT$" SDCL-NEED
   s" SDC-DRIVER$" SDCL-NEED ;

\ srclist names the stdin metabuild-host closure through the same accessors.
: SDCL-CHECK-SRCLIST ( -- )
   s" tools/srclist.f" SDCL-CONSUMER!
   s" SDC-AOT$" SDCL-NEED
   s" SDC-DRIVER$" SDCL-NEED
   s" src/habu/driver-io.f" SDCL-NEED ;

\ bootstrap.sh is the audited launcher: its stdin metabuild source (emit_src)
\ must cat every SDC-HOST file as a literal path (no Habu accessor available).
: SDCL-CHECK-BOOTSTRAP ( -- )
   s" tools/bootstrap.sh" SDCL-CONSUMER!
   [: SDCL-HOST-NEED ;] SDC-WALK ;

\ run-files keys the candidate build: every SDC-KEYED file must be listed.
: SDCL-CHECK-RUN-FILES ( -- )
   s" test/run-files.f" SDCL-CONSUMER!
   [: SDCL-KEYED-NEED ;] SDC-WALK ;

\ hb-build's maker key covers the closure via the whole-engine hash BF-ENGINE$,
\ so it is proven outside the per-file closure (must still hash the engine).
: SDCL-CHECK-HB-BUILD ( -- )
   s" tools/hb-build-lib.f" SDCL-CONSUMER!
   s" BF-ENGINE$" SDCL-NEED ;

: SDCL-SCAN ( -- )
   SDCL-CHECK-BUILD-FIXPOINT
   SDCL-CHECK-SRCLIST
   SDCL-CHECK-BOOTSTRAP
   SDCL-CHECK-RUN-FILES
   SDCL-CHECK-HB-BUILD ;

\ Fail-closed proof: the presence detector must answer both ways. A sentinel the
\ manifest cannot contain must read absent, and a token it does contain must read
\ present, so a detector wired shut (always-present) or open (always-absent)
\ cannot pass silently. Runs before the real scan; no output on success.
: SDCL-SELFTEST ( -- )
   s" tools/stdin-closure-lib.f" SDCL-CONSUMER!
   s" SDCL-SENTINEL-ABSENT-TOKEN" SDCL-HAS? if
      s" stdin-closure-lint: SELF-TEST FAILED: sentinel token present" SDCL-NAME-RC die then
   s" SDC-INCLUDE$" SDCL-HAS? 0= if
      s" stdin-closure-lint: SELF-TEST FAILED: known token read absent" SDCL-NAME-RC die then ;

: SDCL-LINT ( -- )
   0 SDCL-BAD !
   SDCL-SCAN
   s" stdin-closure-lint: drift finding(s)=" type SDCL-BAD @ . SDCL-NL
   SDCL-BAD @ 0 > if 1 throw then ;

SDCL-SELFTEST
SDCL-LINT
