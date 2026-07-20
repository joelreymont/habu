\ maki/data-loader-test.f - acceptance for the v0 text corpus loader
\ (maki/data-loader.f) end-to-end: write a small public-domain fixture to a temp
\ file, read it with the Habu-native reader, build+encode the char vocab, then draw
\ a seeded (B,T) batch with maki/batch-loader.f BL-LOAD and check the get_batch
\ contract on the ENCODED text - the RxC = B x T row shape, the target shifted one
\ token (y = x >> 1) within each contiguous window, determinism (same seed same
\ batch, different seed different), and the file rejects (missing -> E-FS-OPEN,
\ empty -> E-DL-EMPTY). DLT-prefixed (the maki suite shares one dictionary).
\
\ Fixture isolation: every fixture lives under a unique per-run private directory
\ that TMPDIR-MKDIR (lib/fs-mutate.f) allocates and locks down 0700 under the
\ harness temp root - the same pattern every other suite uses (see
\ lib/object-index-test.f, lib/fs-mutate-test.f). Concurrent runs never share a
\ path, the missing-file name is created inside that fresh root so it cannot be a
\ stale leftover, writes reject a substituted symlink (DLT-SAFE-WRITE) so an
\ attacker-planted link can never truncate an arbitrary target through WRITE-ALL,
\ and cleanup runs on every path (including a throw) without masking the original
\ error, leaving nothing behind.

require lib/test.f
require lib/prelude.f
require lib/fs.f
require lib/fs-mutate.f
require maki/examples/nanogpt/data-loader.f
require maki/examples/nanogpt/batch-loader.f

package MAKI

: DLT-TEXT ( -- ptr u8 n )  s" To be, or not to be, that is the question:" ;

\ ---- per-run private fixture root + child paths (all built at setup time) -----
create DLT-ROOT-BUF    FS-PATH-CAP allot   variable DLT-ROOT-U     \ unique 0700 dir under the harness temp root
create DLT-PATH-BUF    FS-PATH-CAP allot   variable DLT-PATH-U     \ corpus fixture
create DLT-EMPTY-BUF   FS-PATH-CAP allot   variable DLT-EMPTY-U    \ empty fixture
create DLT-MISS-BUF    FS-PATH-CAP allot   variable DLT-MISS-U     \ absent name (never created inside the root)
create DLT-SENT-BUF    FS-PATH-CAP allot   variable DLT-SENT-U     \ symlink target the safe write must not truncate
create DLT-LINK-BUF    FS-PATH-CAP allot   variable DLT-LINK-U     \ planted symlink at a fixture-style path
create DLT-SCRATCH-BUF FS-PATH-CAP allot   variable DLT-SCRATCH-U  \ throw-path artifact
create DLT-ISO-A-BUF   FS-PATH-CAP allot   variable DLT-ISO-A-U    \ first isolation root, for the distinctness check

: DLT-ROOT$        ( -- ptr u8 n )  DLT-ROOT-BUF    DLT-ROOT-U @ ;
: DLT-PATH         ( -- ptr u8 n )  DLT-PATH-BUF    DLT-PATH-U @ ;
: DLT-EMPTY-PATH   ( -- ptr u8 n )  DLT-EMPTY-BUF   DLT-EMPTY-U @ ;
: DLT-MISSING-PATH ( -- ptr u8 n )  DLT-MISS-BUF    DLT-MISS-U @ ;
: DLT-SENT$        ( -- ptr u8 n )  DLT-SENT-BUF    DLT-SENT-U @ ;
: DLT-LINK$        ( -- ptr u8 n )  DLT-LINK-BUF    DLT-LINK-U @ ;
: DLT-SCRATCH$     ( -- ptr u8 n )  DLT-SCRATCH-BUF DLT-SCRATCH-U @ ;
: DLT-ISO-A$       ( -- ptr u8 n )  DLT-ISO-A-BUF   DLT-ISO-A-U @ ;

256 constant DLT-TCAP                    \ caller text byte buffer
create DLT-TEXTBUF DLT-TCAP allot
create DLT-CORPUS  DLT-TCAP cells allot  \ encoded ids (one cell per byte)
2 constant DLT-B                          \ sequences
4 constant DLT-T                          \ tokens per sequence
4 constant DLT-DIM                        \ embedding width (arena factor)
8 constant DLT-ROWS                       \ B*T
7  constant DLT-SEED
99 constant DLT-SEED2
create DLT-IDS0 DLT-ROWS cells allot      \ determinism snapshot: ids
create DLT-TGT0 DLT-ROWS cells allot      \ determinism snapshot: targets
variable DLT-N                            \ corpus token count

: DLT-SENT-TEXT ( -- ptr u8 n )  s" SENTINEL-KEEP-ME" ;

\ WRITE-ALL opens O_CREAT|O_TRUNC and FOLLOWS symlinks, so a link planted at the
\ fixture path would truncate whatever it points at. lstat-reject a symlink first;
\ the unpredictable private root already defeats a precreated fixed-path link, and
\ this is the belt-and-suspenders that closes the same-user substitution directly.
: DLT-SAFE-WRITE ( ptr u8 n ptr u8 n -- ) {: path:ptr pathu:n src:ptr srcu:n :}
   path pathu SYMLINK? if E-FS-PATH-UNSAFE throw then
   path pathu src srcu WRITE-ALL ;

: DLT-SETUP ( -- )
   s" habu-maki-data-loader" TMPDIR-MKDIR 2dup CLEANUP-TREE+ {: a:ptr u:n :}
   a DLT-ROOT-BUF u BYTE-COPY  u DLT-ROOT-U !
   DLT-ROOT$ s" corpus.txt"   DLT-PATH-BUF    JOIN-PATH DLT-PATH-U !
   DLT-ROOT$ s" empty.txt"    DLT-EMPTY-BUF   JOIN-PATH DLT-EMPTY-U !
   DLT-ROOT$ s" absent.txt"   DLT-MISS-BUF    JOIN-PATH DLT-MISS-U !
   DLT-ROOT$ s" sentinel.txt" DLT-SENT-BUF    JOIN-PATH DLT-SENT-U !
   DLT-ROOT$ s" linked.txt"   DLT-LINK-BUF    JOIN-PATH DLT-LINK-U !
   DLT-ROOT$ s" scratch.txt"  DLT-SCRATCH-BUF JOIN-PATH DLT-SCRATCH-U ! ;

: DLT-WRITE-FIXTURE ( -- )  DLT-PATH       DLT-TEXT    DLT-SAFE-WRITE ;
: DLT-WRITE-EMPTY   ( -- )  DLT-EMPTY-PATH DLT-TEXTBUF 0 DLT-SAFE-WRITE ;
: DLT-LOAD ( -- )
   DLT-PATH DLT-TEXTBUF DLT-TCAP DLT-CORPUS DLT-TCAP DL-LOAD-CORPUS DLT-N ! ;
: DLT-BATCH ( n -- ) {: seed:n :}         \ draw a batch at the given seed
   DLT-CORPUS DLT-N @ DLT-B DLT-T DLT-DIM seed BL-LOAD ;

\ y = x >> 1 within each window: target[b*T+t] is the input one step ahead
\ (BL-IDS[b*T+t+1]) for t in [0,T-1) - proves contiguity + the one-token shift.
: DLT-SHIFT-OK? ( -- bool )
   DLT-B 0 ?do
      DLT-T 1- 0 ?do
         BL-TGT  j DLT-T * i +      T-GET
         BL-IDS  j DLT-T * i + 1+   T-GET  f= 0= if false unloop unloop exit then
      loop
   loop true ;

: DLT-SNAP ( -- )
   DLT-ROWS 0 ?do  BL-IDS i T-GET DLT-IDS0 i T-SET  BL-TGT i T-GET DLT-TGT0 i T-SET  loop ;

: DLT-EQ? ( ptr a ptr a n -- bool ) {: a:ptr b:ptr n:n :}
   n 0 ?do  a i T-GET b i T-GET f= 0= if false unloop exit then  loop true ;

\ same seed reproduces the exact batch
: DLT-SAME-SEED? ( -- bool )
   DLT-SEED DLT-BATCH  DLT-SNAP
   DLT-SEED DLT-BATCH
   BL-IDS DLT-IDS0 DLT-ROWS DLT-EQ?
   BL-TGT DLT-TGT0 DLT-ROWS DLT-EQ?  and ;

\ a different seed yields a different batch
: DLT-DIFF-SEED? ( -- bool )
   DLT-SEED DLT-BATCH  DLT-SNAP
   DLT-SEED2 DLT-BATCH
   BL-IDS DLT-IDS0 DLT-ROWS DLT-EQ? 0= ;

: DLT-TRY-MISSING ( -- )
   DLT-MISSING-PATH DLT-TEXTBUF DLT-TCAP DLT-CORPUS DLT-TCAP DL-LOAD-CORPUS drop ;
: DLT-TRY-EMPTY ( -- )
   DLT-WRITE-EMPTY
   DLT-EMPTY-PATH DLT-TEXTBUF DLT-TCAP DLT-CORPUS DLT-TCAP DL-LOAD-CORPUS drop ;

\ ---- checked assertion groups ------------------------------------------------
: DLT-COUNT-OK? ( -- )                      \ corpus token count == fixture bytes
   DLT-N @  DLT-TEXT nip  T= ;

: DLT-SHAPE-OK? ( -- )                      \ get_batch shape (RxC = B x T) + one-token shift
   DLT-SEED DLT-BATCH
   BL-ROWS DLT-ROWS T=
   BL-T@   DLT-T    T=
   DLT-SHIFT-OK? TTRUE ;

: DLT-DETERMINISM-OK? ( -- )
   DLT-SAME-SEED? TTRUE
   DLT-DIFF-SEED? TTRUE ;

: DLT-REJECTS-OK? ( -- )                    \ file rejects (fail closed)
   [: DLT-TRY-MISSING ;] E-FS-OPEN  TTHROWSQ
   [: DLT-TRY-EMPTY   ;] E-DL-EMPTY TTHROWSQ ;

: DLT-STALE-OK? ( -- )                      \ anti-stale invariant
   \ the missing name lives in a freshly-minted unique root, so it cannot be a
   \ stale leftover from a prior run falsifying the missing-file reject.
   DLT-MISSING-PATH EXISTS? TFALSE ;

: DLT-SYMLINK-REJECT-OK? ( -- )             \ symlink substitution is refused, target untouched
   DLT-SENT$ DLT-SENT-TEXT WRITE-ALL                            \ an arbitrary writable target, with content
   DLT-SENT$ DLT-LINK$ MAKE-SYMLINK                            \ plant link -> target at a fixture-style path
   DLT-LINK$ SYMLINK? TTRUE
   [: DLT-LINK$ DLT-TEXT DLT-SAFE-WRITE ;] E-FS-PATH-UNSAFE TTHROWSQ
   DLT-SENT$ FILE-SIZE DLT-SENT-TEXT nip T= ;                  \ target NOT truncated: byte count preserved

: DLT-ISOLATION-OK? ( -- )                  \ concurrent two-run isolation
   \ two allocations each yield a distinct private 0700 dir, so two simultaneous
   \ runs never share a fixture path. Both are registered for teardown.
   s" habu-maki-dl-iso" TMPDIR-MKDIR 2dup CLEANUP-TREE+ {: a:ptr au:n :}
   a DLT-ISO-A-BUF au BYTE-COPY  au DLT-ISO-A-U !
   a au DIR? TTRUE
   s" habu-maki-dl-iso" TMPDIR-MKDIR 2dup CLEANUP-TREE+ {: b:ptr bu:n :}
   b bu DIR? TTRUE
   DLT-ISO-A$ b bu T$<> ;

: DLT-THROW-BODY ( -- )                     \ create an artifact, then hit a REAL loader error
   DLT-SCRATCH$ DLT-TEXT DLT-SAFE-WRITE
   DLT-SCRATCH$ EXISTS? TTRUE
   DLT-TRY-EMPTY ;                                             \ empty corpus -> E-DL-EMPTY throw

: DLT-THROW-CLEANUP ( -- )                  \ throw-path cleanup, no masking
   \ same discipline as MAIN's teardown: body under catch, artifact removed on
   \ EVERY path, and the original throw code re-surfaced (not masked by cleanup).
   [: DLT-THROW-BODY ;] catch {: rc:n :}
   DLT-SCRATCH$ EXISTS? if DLT-SCRATCH$ REMOVE-FILE then       \ fail-safe teardown of the artifact
   rc E-DL-EMPTY T=                                            \ REMOVE-FILE throws E-FS-IO, never E-DL-EMPTY: masking would be caught
   DLT-SCRATCH$ EXISTS? TFALSE ;

: DLT-BODY ( -- )
   DLT-SETUP
   DLT-WRITE-FIXTURE
   DLT-LOAD
   DLT-COUNT-OK?
   DLT-SHAPE-OK?
   DLT-DETERMINISM-OK?
   DLT-STALE-OK?
   DLT-SYMLINK-REJECT-OK?
   DLT-REJECTS-OK?
   DLT-ISOLATION-OK?
   DLT-THROW-CLEANUP ;

: DLT-NO-ARTIFACTS ( -- )                   \ nothing the suite created is left behind after teardown
   DLT-ROOT$      EXISTS? TFALSE
   DLT-PATH       EXISTS? TFALSE
   DLT-EMPTY-PATH EXISTS? TFALSE
   DLT-ISO-A$     EXISTS? TFALSE ;

: DLT-TEARDOWN ( n -- ) {: rc:n :}
   rc 0= if
      CLEANUP-RUN                                             \ body ok: a cleanup failure is a real error -> propagate
      exit
   then
   \ body already failing: run cleanup best-effort so the ORIGINAL code surfaces.
   \ Structural cleanup is the one place the rules permit swallowing an error; the
   \ drop discards only the cleanup code, never rc, which is re-thrown below.
   [: CLEANUP-RUN ;] catch drop
   rc throw ;

: MAIN ( -- )
   T-RESET
   CLEANUP-RESET
   [: DLT-BODY ;] catch DLT-TEARDOWN
   DLT-NO-ARTIFACTS
   T-REPORT ;

MAIN

;package
