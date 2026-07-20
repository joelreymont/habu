\ seal-absence.f - Gforth stage0 absence-parity fixture (TFAM 2b-iv).
\
\ The friend-arena seal (TFAM 2b-i) is a runtime range guard on every raw-write
\ SINK (! c! +! atomic* patch32 snap-rebase, syscall write buffers). The native
\ engine (src/habu/habu1.f) carries the full sink set; the Gforth stage0 mirror
\ (bootstrap/cg/forth.fs) is a strict SUBSET — it has no atomics, no snap-rebase,
\ no readlink/stat64/lstat64/getdirentries64/poll/ffi-call syscalls and no
\ CHECKER-* registry mutators except the reviewed checker-defer registration
\ bridge, whose exact sites are presence-pinned below (SAB-REAL-CHKDEFER).
\ 2b-i already mirrored GUARD-SPAN onto the sinks stage0 DOES
\ have (BSTORE/BPLUSSTORE/BCSTORE, read/ioctl/mmap buffers, patch32) plus
\ EMIT-SEAL-FRIEND-TOKEN on both cold-prefix entry paths, and test/seal.f runtime-proves
\ those traps against the sealed candidate.
\
\ Parity for the ABSENT surfaces cannot be "add a matching guard" — there is
\ nothing to guard. Parity is proving the absence STAYS absent: a guard-bypass
\ surface must not silently appear in the stage0 mirror unguarded. This fixture
\ pins today's absence list (a named table, SAB-ABSENT-*) and scans the mirror
\ source; any pinned surface that appears on a code line without a GUARD-SPAN on
\ that line fails the gate closed, forcing a conscious decision (wire the guard +
\ a seal trap fixture + re-pin) when someone extends stage0. It also pins the
\ PRESENT seal machinery (the GUARD-SPAN sink guards and the EMIT-SEAL-FRIEND
\ entry seals) so a mirrored guard cannot be silently deleted.
\
\ Wordlist-creation (BWORDLIST/BSETCUR/BSETCHECK) and the execute/compile sinks
\ (BEXEC/BCOMPILE/C-POSTPONE) are intentionally NOT PROT-GUARD surfaces on either
\ side: they are trusted engine primitives that mutate friend-arena cells through
\ dedicated `DATA <CELL> STR,` stores (LESSONS.md, TFAM 2b-i) or transfer control,
\ never through a computed raw store, so the seal deliberately leaves them open
\ (test/seal.f's positive forge proves post-seal defines/packages still work).
\ Package scope and protected-WID membership are now present on both native and
\ recovery paths. This fixture pins that registry separately from the remaining
\ genuinely absent surfaces.
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
require lib/adt/option.f                 \ option<CAD-NUM:index> STR:FIND-SUB consumer (switchover wave A)
require lib/test.f
require lib/memory.f
require lib/fs.f

\ White-box CAD-NUM role readers (precedent: lib/string-test.f STR-T-*>RAW):
\ reopen the unsealed CAD-NUM package to project the typed STR: index/byte-len/
\ byte-off results back to raw cells, keeping SAB-OCCURS and the line scanners
\ byte-identical. Plain checked words over the audited private *>N projections -
\ not a new boundary.
package CAD-NUM
public
: SAB-IX>RAW ( CAD-NUM:index -- n ) INDEX>N ;
: SAB-BL>RAW ( CAD-NUM:byte-len -- n ) BYTE-LEN>N ;
: SAB-BO>RAW ( CAD-NUM:byte-off -- n ) BYTE-OFF>N ;
;package

\ Typed STR:SPLIT-NEXT with the raw-shaped result the line scanners consume:
\ convert the length/offset args to roles, then project the sub-length and
\ next-offset back to raw cells. Byte-identical to the raw split for in-buffer
\ offsets.
: SAB-SPLIT-NEXT ( ptr u8 n n n -- ptr u8 n n bool ) {: a:ptr u:n sep:n start:n :}
   a u STR:LENGTH sep start STR:OFFSET STR:SPLIT-NEXT MATCH option
     none OF a 0 start STR-FALSE ENDOF   \ mirror the raw split's (a, 0, start, false)
     some OF STR-SPLIT:UNMAKE {: fa:ptr bl:CAD-NUM:byte-len bo:CAD-NUM:byte-off :}
        fa  bl CAD-NUM:SAB-BL>RAW  bo CAD-NUM:SAB-BO>RAW  STR-TRUE ENDOF
   ;MATCH ;

$40000 constant SAB-CAP                 \ mirror scan buffer (forth.fs ~137 KB + headroom)
$800 constant SAB-NAMES-CAP             \ packed absent-name table capacity (bytes)
92 constant SAB-BSLASH                  \ ASCII '\' — the line-comment introducer
11 constant SAB-GUARD-PINS              \ GUARD-SPAN definition + bounded/runtime sink lines (incl. BMUNMAP)
3 constant SAB-SEAL-PINS                \ EMIT-SEAL-FRIEND code sites: 1 def + 2 entry seals
2 constant SAB-CHKDEFER-PINS            \ CHECKER-DEFER code sites: C-CALL-CHECKER-DEFER def + C-DEFER call

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
   s" CHECKER-USIG-ADD" SAB-NAME,
   s" CHECKER-UNDEFINE" SAB-NAME,  s" CHECKER-DEFFAMILY" SAB-NAME,
   s" CHECKER-DEFLINEAR" SAB-NAME,  s" CHECKER-DEFRECORD" SAB-NAME,
   s" CHECKER-USIGS-TRUNCATE-FROM" SAB-NAME, ;

: SAB-INIT-NAMES ( -- )
   0 SAB-NAMES-LEN !
   SAB-ADD-ATOMICS  SAB-ADD-SNAP  SAB-ADD-SYSCALLS
   SAB-ADD-CHECKER ;

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
   a codeu s" GUARD-SPAN" CONTAINS? if exit then   \ guarded line: consciously allowed
   0 begin dup SAB-NAMES-LEN @ < while
      SAB-NAME-AT
      2dup a codeu 2swap CONTAINS? if
         line SAB-VIOL+
      else 2drop then
   repeat drop ;

: SAB-SCAN-BUF ( ptr u8 n -- ) {: a:ptr u:n :}
   1 SAB-LINE# !  0 SAB-LSTART !
   begin
      a u STR-LF SAB-LSTART @ SAB-SPLIT-NEXT
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
      a SAB-CSTART @ +  u SAB-CSTART @ - STR:LENGTH  b v STR:LENGTH  STR:FIND-SUB MATCH option
        none OF SAB-CNT @ exit ENDOF
        some OF CAD-NUM:SAB-IX>RAW ENDOF
      ;MATCH
      SAB-CNT @ 1 + SAB-CNT !
      SAB-CSTART @ + v + SAB-CSTART !
   repeat SAB-CNT @ ;

: SAB-COUNT-CODE ( ptr u8 n ptr u8 n -- n ) {: a:ptr u:n b:ptr v:n :}
   0 SAB-TOT !  0 SAB-LSTART !
   begin
      a u STR-LF SAB-LSTART @ SAB-SPLIT-NEXT
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
   SAB-CAP MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop SAB-BUF-A-FIELD !
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
   s" absent name on a GUARD-SPAN line is an allowed guarded add" T-LABEL
   0 SAB-VIOL# !
   s" : BX ( -- ) B G-POP A G-POP B 7 GUARD-SPAN atomic-add A B 0 STR, ;" SAB-SCAN-BUF
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
   s" stage0 raw-store/syscall GUARD-SPAN sinks stay present" T-LABEL
   SAB-FORTH$ s" GUARD-SPAN" SAB-COUNT-CODE SAB-GUARD-PINS T=
   s" stage0 seal is emitted on both cold-prefix entry paths" T-LABEL
   SAB-FORTH$ s" EMIT-SEAL-FRIEND" SAB-COUNT-CODE SAB-SEAL-PINS T= ;

: SAB-PRESENT ( ptr u8 n -- )
   SAB-FORTH$ 2swap SAB-COUNT-CODE 0 > TTRUE ;

: SAB-REAL-PROTWID ( -- )
   s" stage0 package reopen uses the protected-WID registry" T-LABEL
   s" PROT-WID-N-CELL" SAB-PRESENT
   s" BPROTWIDADD" SAB-PRESENT
   s" LPROTWIDQ" SAB-PRESENT
   s" C-PACKAGE-PROT-GUARD" SAB-PRESENT ;

\ Deliberate stage0 checker-record surface (dot habu-mirror-checker-defer-6a8a366e):
\ mirror C-DEFER calls the checker's `checker-defer` word through
\ C-CALL-CHECKER-DEFER, exactly as native habu2.f C-DEFER does, so a defer's
\ declared effect is registered and a CHECKED `is NAME` fit-checks on the
\ stage0 path. This ADDS certification capability and bypasses no guard: it is
\ an engine->checker dictionary find + call (the audited C-CALL-TRUST-PEND
\ shape), never a computed raw store into the friend arena. The EXACT
\ code-line occurrence count is pinned so any FUTURE use of this surface
\ re-trips the fence and forces this review again.
: SAB-REAL-CHKDEFER ( -- )
   s" stage0 checker-defer bridge is pinned at its reviewed sites" T-LABEL
   SAB-FORTH$ s" CHECKER-DEFER" SAB-COUNT-CODE SAB-CHKDEFER-PINS T= ;

: SAB-REAL-TESTS ( -- )
   SAB-LOAD-FORTH
   SAB-REAL-ABSENCE
   SAB-REAL-GUARDS
   SAB-REAL-PROTWID
   SAB-REAL-CHKDEFER ;

: SAB-MAIN ( -- )
   T-RESET
   SAB-INIT-NAMES
   SAB-SELF-TESTS
   SAB-REAL-TESTS
   T-REPORT
   s" seal-absence-test: ok" type cr ;

SAB-MAIN
