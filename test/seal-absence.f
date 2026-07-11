\ seal-absence.f - Gforth stage0 absence-parity fixture (TFAM 2b-iv).
\
\ The friend-arena seal (TFAM 2b-i) is a runtime range guard on every raw-write
\ SINK (! c! +! atomic* patch32 snap-rebase, syscall write buffers). The native
\ engine (src/habu/habu1.f) carries the full sink set; the Gforth stage0 mirror
\ (bootstrap/cg/forth.fs) is a strict SUBSET — it has no atomics, no snap-rebase,
\ no readlink/stat64/lstat64/getdirentries64/poll/ffi-call syscalls, no CHECKER-*
\ registry mutators, and no package/public/private intrinsic (census-tfam-2b.md
\ discrepancy 5). 2b-i already mirrored PROT-GUARD onto the sinks stage0 DOES
\ have (BSTORE/BPLUSSTORE/BCSTORE, read/ioctl/mmap buffers, patch32) plus
\ EMIT-SEAL-FRIEND on both cold-prefix entry paths, and test/seal.f runtime-proves
\ those traps against the sealed candidate.
\
\ Parity for the ABSENT surfaces cannot be "add a matching guard" — there is
\ nothing to guard. Parity is proving the absence STAYS absent: a guard-bypass
\ surface must not silently appear in the stage0 mirror unguarded. This fixture
\ pins today's absence list (a named table, SAB-ABSENT-*) and scans the mirror
\ source; any pinned surface that appears on a code line without a PROT-GUARD on
\ that line fails the gate closed, forcing a conscious decision (wire the guard +
\ a seal trap fixture + re-pin) when someone extends stage0. It also pins the
\ PRESENT seal machinery (the PROT-GUARD sink guards and the EMIT-SEAL-FRIEND
\ entry seals) so a mirrored guard cannot be silently deleted.
\
\ Wordlist-creation (BWORDLIST/BSETCUR/BSETCHECK) and the execute/compile sinks
\ (BEXEC/BCOMPILE/C-POSTPONE) are intentionally NOT PROT-GUARD surfaces on either
\ side: they are trusted engine primitives that mutate friend-arena cells through
\ dedicated `DATA <CELL> STR,` stores (LESSONS.md, TFAM 2b-i) or transfer control,
\ never through a computed raw store, so the seal deliberately leaves them open
\ (test/seal.f's positive forge proves post-seal defines/packages still work).
\ Friend-scoping those definition/control surfaces is a checker/compiler-level
\ capability = a later slice (2b-iii) on BOTH sides; there is no 2b-i-style latch
\ parity to mirror for them here, so this fixture pins their guard-free status by
\ omission (they are absent from the guarded-sink presence pins).
\
\ Scan model: comments are `\`-to-EOL (Gforth requires the backslash to be
\ whitespace-delimited); forth.fs holds no backslash inside strings, so a
\ per-line truncate at the first ws-delimited backslash yields the code portion
\ with no false negatives. Matching is case-sensitive substring on the code
\ portion so a registered name inside an `s" name"` literal is still caught.
\
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/memory.f lib/fs.f
\   test/seal-absence.f

require lib/errors.f
require lib/string.f
require lib/adt/option.f                 \ option<idx> FIND-SUB consumer (switchover wave A)
require lib/test.f
require lib/memory.f
require lib/fs.f

$40000 constant SAB-CAP                 \ mirror scan buffer (forth.fs ~137 KB + headroom)
$800 constant SAB-NAMES-CAP             \ packed absent-name table capacity (bytes)
92 constant SAB-BSLASH                  \ ASCII '\' — the line-comment introducer
10 constant SAB-GUARD-PINS              \ PROT-GUARD code sites: 1 def + 9 guarded sinks (incl cp!/ndict!)
3 constant SAB-SEAL-PINS                \ EMIT-SEAL-FRIEND code sites: 1 def + 2 entry seals

variable SAB-VIOL#                      \ unguarded surfaces found in the current scan
variable SAB-REPORT?                    \ -1 while scanning real forth.fs (print offenders)
variable SAB-LINE#                      \ 1-based line number of the current scan line
variable SAB-LSTART                     \ line-split cursor into the scanned buffer
variable SAB-CNT                        \ SAB-OCCURS running match count
variable SAB-CSTART                     \ SAB-OCCURS scan cursor
variable SAB-TOT                        \ SAB-COUNT-CODE running total
variable SAB-FLEN                       \ bytes of forth.fs read into the scan buffer

\ --- named absent-surface table: packed [len][bytes] records ---

create SAB-NAMES SAB-NAMES-CAP allot
variable SAB-NAMES-LEN

: SAB-NAME, ( ptr u8 n -- ) {: a:ptr u:n :}
   u STR-BYTE-MAX > if E-STR-BOUNDS throw then
   SAB-NAMES-LEN @ 1 + u + SAB-NAMES-CAP > if E-TBL-BOUNDS throw then
   u SAB-NAMES SAB-NAMES-LEN @ + c!
   a SAB-NAMES SAB-NAMES-LEN @ + 1 + u BYTE-COPY
   SAB-NAMES-LEN @ 1 + u + SAB-NAMES-LEN ! ;

: SAB-NAME-AT ( n -- n ptr u8 n ) {: pos:n :}
   SAB-NAMES pos + c@ {: nu:n :}
   pos 1 + nu +
   SAB-NAMES pos + 1 +
   nu ;

: SAB-ADD-ATOMICS ( -- )
   s" atomic!" SAB-NAME,  s" atomic-add" SAB-NAME,  s" atomic-cas" SAB-NAME, ;

: SAB-ADD-SNAP ( -- )
   s" snap-rebase" SAB-NAME,  s" BSNAPREBASE" SAB-NAME, ;

: SAB-ADD-SYSCALLS ( -- )
   s" readlink" SAB-NAME,  s" stat64" SAB-NAME,  s" lstat64" SAB-NAME,
   s" getdirentries64" SAB-NAME,  s" poll" SAB-NAME,  s" ffi-call" SAB-NAME, ;

: SAB-ADD-CHECKER ( -- )
   s" CHECKER-PACKAGE" SAB-NAME,  s" CHECKER-PUBLIC" SAB-NAME,
   s" CHECKER-PRIVATE" SAB-NAME,  s" CHECKER-END-PACKAGE" SAB-NAME,
   s" CHECKER-DEFER" SAB-NAME,  s" CHECKER-USIG-ADD" SAB-NAME,
   s" CHECKER-UNDEFINE" SAB-NAME,  s" CHECKER-DEFTYPE" SAB-NAME,
   s" CHECKER-DEFLINEAR" SAB-NAME,  s" CHECKER-DEFRECORD" SAB-NAME,
   s" CHECKER-USIGS-TRUNCATE-FROM" SAB-NAME, ;

: SAB-ADD-PACKAGE ( -- )
   s" package" SAB-NAME,  s" public" SAB-NAME,  s" private" SAB-NAME,
   s" end-package" SAB-NAME,  s" C-PACKAGE" SAB-NAME,  s" C-PUBLIC" SAB-NAME,
   s" C-PRIVATE" SAB-NAME,  s" C-END-PACKAGE" SAB-NAME, ;

\ TFAM 2b-v: the protected-WID registry (records the WIDs of sealed system /
\ generated constructor PACKAGES, and gates the sealed-WID guards) is a native-only
\ mechanism. Its precondition -- the package system -- is already pinned absent
\ above, so the registry cannot exist in stage0; pin its cells/routines too so a
\ registry cannot silently appear in the mirror without a conscious re-pin.
: SAB-ADD-PROTWID ( -- )
   s" PROT-WID-N-CELL" SAB-NAME,  s" PROT-WID-OFF" SAB-NAME,
   s" LPROTWIDQ" SAB-NAME,  s" LAOTWIDGATE" SAB-NAME,
   s" EM-AOT-REGISTER-PROT-WIDS" SAB-NAME, ;

: SAB-INIT-NAMES ( -- )
   0 SAB-NAMES-LEN !
   SAB-ADD-ATOMICS  SAB-ADD-SNAP  SAB-ADD-SYSCALLS
   SAB-ADD-CHECKER  SAB-ADD-PACKAGE  SAB-ADD-PROTWID ;

\ --- comment stripping + substring scan ---

: SAB-WS? ( n -- bool )
   dup STR-SPACE = swap STR-TAB = or ;

: SAB-CODE-LEN ( ptr u8 n -- n ) {: a:ptr u:n :}
   0 begin dup u < while
      dup a + c@ SAB-BSLASH = if
         dup 0 = if drop 0 exit then
         dup 1 - a + c@ SAB-WS? if exit then
      then
      1 +
   repeat drop u ;

: SAB-VIOL+ ( ptr u8 n n -- ) {: np:ptr nu:n line:n :}
   SAB-VIOL# @ 1 + SAB-VIOL# !
   SAB-REPORT? @ 0 = if exit then
   s" seal-absence: bootstrap/cg/forth.fs:" type line .
   s" gained unguarded stage0 surface " type np nu type cr ;

: SAB-SCAN-LINE ( ptr u8 n n -- ) {: a:ptr u:n line:n :}
   a u SAB-CODE-LEN {: codeu:n :}
   codeu 0 = if exit then
   a codeu s" PROT-GUARD" CONTAINS? if exit then   \ guarded line: consciously allowed
   0 begin dup SAB-NAMES-LEN @ < while
      SAB-NAME-AT
      2dup a codeu 2swap CONTAINS? if
         line SAB-VIOL+
      else 2drop then
   repeat drop ;

: SAB-SCAN-BUF ( ptr u8 n -- ) {: a:ptr u:n :}
   1 SAB-LINE# !  0 SAB-LSTART !
   begin
      a u STR-LF SAB-LSTART @ SPLIT-NEXT
   while
      SAB-LSTART !
      SAB-LINE# @ SAB-SCAN-LINE
      SAB-LINE# @ 1 + SAB-LINE# !
   repeat 2drop drop ;

\ --- code-only occurrence counter (presence pins) ---

: SAB-OCCURS ( ptr u8 n ptr u8 n -- n ) {: a:ptr u:n b:ptr v:n :}
   v 0 = if 0 exit then
   0 SAB-CNT !  0 SAB-CSTART !
   begin SAB-CSTART @ v + u <= while
      a SAB-CSTART @ +  u SAB-CSTART @ -  b v FIND-SUB MATCH option
        none OF SAB-CNT @ exit ENDOF
        some OF IDX>N ENDOF
      ;MATCH
      SAB-CNT @ 1 + SAB-CNT !
      SAB-CSTART @ + v + SAB-CSTART !
   repeat SAB-CNT @ ;

: SAB-COUNT-CODE ( ptr u8 n ptr u8 n -- n ) {: a:ptr u:n b:ptr v:n :}
   0 SAB-TOT !  0 SAB-LSTART !
   begin
      a u STR-LF SAB-LSTART @ SPLIT-NEXT
   while
      SAB-LSTART !
      2dup SAB-CODE-LEN nip
      b v SAB-OCCURS
      SAB-TOT @ + SAB-TOT !
   repeat 2drop drop
   SAB-TOT @ ;

\ --- scan buffer + mirror source read ---

variable SAB-BUF-A

: SAB-BUF-A-FIELD ( -- ptr ptr u8 )
   SAB-BUF-A 0 ptr-field ;

variable SAB-READY

: SAB-ALLOC ( -- )
   SAB-READY @ 0 <> if exit then
   SAB-CAP MEM-ALLOC-BYTES drop SAB-BUF-A-FIELD !
   -1 SAB-READY ! ;

: SAB-BUF ( -- ptr u8 )
   SAB-ALLOC
   SAB-BUF-A-FIELD @ ;

: SAB-LOAD-FORTH ( -- )
   s" bootstrap/cg/forth.fs" SAB-BUF SAB-CAP READ-ALL SAB-FLEN ! ;

: SAB-FORTH$ ( -- ptr u8 n )
   SAB-BUF SAB-FLEN @ ;

\ --- self proofs: the scanner rejects a seeded sink, honors the guard escape,
\ ignores comment-only mentions, and detects every pinned surface ---

: SAB-SELF-CHECK1 ( ptr u8 n -- ) {: np:ptr nu:n :}
   s" pinned surface is detected as an unguarded sink" T-LABEL
   0 SAB-VIOL# !
   np nu SAB-SCAN-BUF
   SAB-VIOL# @ 0 > TTRUE ;

: SAB-SELF-EACH ( -- )
   0 begin dup SAB-NAMES-LEN @ < while
      SAB-NAME-AT SAB-SELF-CHECK1
   repeat drop ;

: SAB-SELF-NEG ( -- )
   s" seeded unguarded sink line fails closed" T-LABEL
   0 SAB-VIOL# !
   s" : BX ( -- ) atomic-add A B 0 STR, ;" SAB-SCAN-BUF
   SAB-VIOL# @ 0 > TTRUE ;

: SAB-SELF-GUARD-OK ( -- )
   s" absent name on a PROT-GUARD line is an allowed guarded add" T-LABEL
   0 SAB-VIOL# !
   s" : BX ( -- ) B G-POP A G-POP B PROT-GUARD atomic-add A B 0 STR, ;" SAB-SCAN-BUF
   SAB-VIOL# @ 0 T= ;

: SAB-SELF-COMMENT-OK ( -- )
   s" absent name only in a backslash comment is ignored" T-LABEL
   0 SAB-VIOL# !
   s" : BX ( -- ) A B 0 STR, ;   \ atomic-add snap-rebase package here" SAB-SCAN-BUF
   SAB-VIOL# @ 0 T= ;

: SAB-SELF-TESTS ( -- )
   0 SAB-REPORT? !
   SAB-SELF-NEG
   SAB-SELF-GUARD-OK
   SAB-SELF-COMMENT-OK
   SAB-SELF-EACH ;

\ --- real proofs against the stage0 mirror source ---

: SAB-REAL-ABSENCE ( -- )
   s" stage0 mirror keeps every guard-bypass surface absent" T-LABEL
   0 SAB-VIOL# !
   -1 SAB-REPORT? !
   SAB-FORTH$ SAB-SCAN-BUF
   0 SAB-REPORT? !
   SAB-VIOL# @ 0 T= ;

: SAB-REAL-GUARDS ( -- )
   s" stage0 raw-store/syscall PROT-GUARD sinks stay present" T-LABEL
   SAB-FORTH$ s" PROT-GUARD" SAB-COUNT-CODE SAB-GUARD-PINS T=
   s" stage0 seal is emitted on both cold-prefix entry paths" T-LABEL
   SAB-FORTH$ s" EMIT-SEAL-FRIEND" SAB-COUNT-CODE SAB-SEAL-PINS T= ;

: SAB-REAL-TESTS ( -- )
   SAB-LOAD-FORTH
   SAB-REAL-ABSENCE
   SAB-REAL-GUARDS ;

: SAB-MAIN ( -- )
   T-RESET
   SAB-INIT-NAMES
   SAB-SELF-TESTS
   SAB-REAL-TESTS
   T-REPORT
   s" seal-absence-test: ok" type cr ;

SAB-MAIN
