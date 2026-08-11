\ ddc-scheduled.f - change-triggered Diverse Double-Compiling audit.
\
\ The DDC audit (tools/ddc-verify.f) proves bin/hb builds byte-identically
\ through the native fixpoint and the independent gforth recovery chain. It is
\ expensive (a full gforth bootstrap + native refresh, HABU_ALLOW_BOOTSTRAP=1,
\ minutes), so it is never per-commit. This wraps it in a change trigger so the
\ ordinary gate never pays the DDC cost: a content-key over the bootstrap-chain
\ source (src/habu, src/arch, bootstrap/cg) is compared to the committed marker
\ tools/ddc-marker.txt. Unchanged -> the last audit still covers this tree, skip
\ DDC. Changed -> run the DDC audit; on byte-identical convergence rewrite the
\ marker (commit it with the change), on divergence fail loudly.
\
\ Run it from a scheduler (weekly) or on any src/habu|src/arch|bootstrap/cg
\ touch, with HABU_ALLOW_BOOTSTRAP=1 so the audit can drive the gforth host:
\   HABU_ALLOW_BOOTSTRAP=1 bin/hb --load tools/ddc-verify.f tools/ddc-scheduled.f tools/ddc-scheduled-drive.f
\
\ The content-key is order-INDEPENDENT (XOR of per-file path+content digests) so
\ it is identical on every checkout regardless of directory enumeration order,
\ and it changes when any bootstrap-chain file is added, removed, or edited.

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/content-key.f
require tools/ddc-verify.f

package DDCS

32 constant DG-LEN                           \ SHA256 digest bytes
64 constant KEY-HEX-LEN
1 constant DIVERGENT-RC
$80 constant MARKER-CAP

create ACC DG-LEN allot                      \ order-independent XOR accumulator
create FILE-DG DG-LEN allot                  \ per-file (path+content) digest
create KEY-HEX KEY-HEX-LEN allot
create MARKER-BUF MARKER-CAP allot
variable MARKER-U
create MARKER-PATH-BUF FS-PATH-CAP allot     \ marker file path (overridable for tests)
variable MARKER-PATH-U

\ ---- private helpers ------------------------------------------------------
: MARKER$ ( -- ptr u8 n )
   MARKER-PATH-BUF MARKER-PATH-U @ ;

: ACC-RESET ( -- )
   DG-LEN 0 ?do 0 ACC i + c! loop ;

: ACC-XOR ( -- )                             \ XOR the per-file digest FILE-DG into ACC
   DG-LEN 0 ?do
      FILE-DG i + c@  ACC i + c@ xor  ACC i + c!
   loop ;

\ One file: content-key over its path AND bytes -> 32-byte digest, XOR'd in.
\ CONTENT-KEY:FILE+ folds the path plus the SHA256 of the content (never mtime), so the
\ key tracks a rename, an edit, an add, or a delete of any file.
: KEY-FILE ( ptr u8 n -- ) {: a:ptr u:n :}
   CONTENT-KEY:OPEN
   a u CONTENT-KEY:FILE+
   FILE-DG CONTENT-KEY:FINAL
   ACC-XOR ;

: KEY-ROOT ( ptr u8 n -- )
   [: KEY-FILE ;] WALK-FILES ;

: MARKER-READ ( -- )                         \ load marker file (0 bytes if absent)
   MARKER$ FILE? 0= if 0 MARKER-U ! exit then
   MARKER$ MARKER-BUF MARKER-CAP READ-ALL MARKER-U ! ;

: MARKER-KEY$ ( -- ptr u8 n )                \ marker contents, trimmed of trailing LF
   MARKER-BUF MARKER-U @ TRIM ;

: MARKER-WRITE ( -- )                        \ record the current key as the audited marker
   KEY-HEX MARKER-BUF KEY-HEX-LEN BYTE-COPY
   STR-LF MARKER-BUF KEY-HEX-LEN + c!
   MARKER$ MARKER-BUF KEY-HEX-LEN 1+ WRITE-ALL ;

defer AUDIT ( -- n )                         \ 0 = byte-identical convergence; nonzero = divergent

: BOOTSTRAP-SET? ( -- bool )
   s" HABU_ALLOW_BOOTSTRAP" GETENV s" 1" STR= ;

\ Default audit: enforce the bootstrap env gate with a clean rc (not a throw) so
\ a scheduler without the env fails legibly, then run the real DDC.
: AUDIT-DDC ( -- n )
   BOOTSTRAP-SET? 0= if
      s" ddc-scheduled: HABU_ALLOW_BOOTSTRAP!=1; set it to run the DDC audit" type cr
      DIVERGENT-RC exit
   then
   DDC-VERIFY ;

public

: MARKER-PATH! ( ptr u8 n -- ) {: a:ptr u:n :}
   u FS-PATH-CAP > if E-FS-PATH throw then
   a MARKER-PATH-BUF u BYTE-COPY
   u MARKER-PATH-U ! ;

: KEY$ ( -- ptr u8 n )
   KEY-HEX KEY-HEX-LEN ;

: COMPUTE ( -- )                             \ fill KEY$ with the current content-key
   ACC-RESET
   s" src/habu" KEY-ROOT
   s" src/arch" KEY-ROOT
   s" bootstrap/cg" KEY-ROOT
   ACC KEY-HEX SHA256>HEX ;

: CHANGED? ( -- bool )                       \ true iff current key differs from the marker
   MARKER-READ
   KEY$ MARKER-KEY$ STR= 0= ;

: AUDIT! ( [ -- n ] -- )                     \ override the audit seam (tests)
   is AUDIT ;

\ 0 = unchanged or converged; nonzero = bootstrap chain diverged (fail).
: RUN ( -- n )
   COMPUTE
   CHANGED? 0= if
      s" ddc-scheduled: bootstrap chain unchanged (" type KEY$ type
      s" ); DDC re-verify not needed" type cr
      0 exit
   then
   s" ddc-scheduled: src/habu|src/arch|bootstrap/cg changed since marker; running DDC audit" type cr
   AUDIT {: rc:n :}
   rc 0 <> if
      s" ddc-scheduled: DDC DIVERGENT - bootstrap chain does not converge (see ddc: lines)" type cr
      rc exit
   then
   MARKER-WRITE
   s" ddc-scheduled: DDC byte-identical; marker refreshed to " type KEY$ type cr
   s" ddc-scheduled: commit tools/ddc-marker.txt with this change" type cr
   0 ;

: MAIN ( -- )
   RUN {: rc:n :}
   rc 0 <> if s" ddc-scheduled: bootstrap chain audit failed" rc die then ;

: INSTALL ( -- )                             \ bind the production audit seam
   [: AUDIT-DDC ;] AUDIT! ;

INSTALL
s" tools/ddc-marker.txt" MARKER-PATH!

;package
