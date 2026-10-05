\ package-dag-lint-core.f - the HBR2 layer DAG over the require graph.
\
\ docs/browser-runtime.md §1.2 and §27.1 split the browser runtime into layers,
\ each a directory, and make a cycle between them a build failure. The table
\ below says which layers each may import. The root is the working directory.
\ Two checks enforce it:
\
\ THE WALK. From every `.f` file of every layer, the closure of its imports
\ (test/load-refs.f: `require`, `include` and a literal, escapes decoded, handed
\ to `required` or `included`). Each resolves through the loader's own resolver
\ (SOURCE-ROOT:RESOLVE), which this process runs in the root as a layer file's
\ load does: against the root that resolved the importing file, then the root,
\ then the engine's. A layer file is walked from twice: as an entry, whose root
\ is its own directory, and as a require from the root. A require of a file the
\ engine provides is no edge, since it loads nothing; an include reads the file
\ again, so the walk reads it too. A file of a layer the starting layer may not
\ import is a finding, printed with the shortest path that reaches it; so is a
\ require cycle through a layer file, and an import whose path is not a literal
\ the walk can read, in any file it reads. A layer is its directory: a file
\ listed there whose canonical path leaves it, through a symlink, is a finding
\ too, and so is a layer directory the lint cannot read: only one that is
\ absent holds no layer file.
\
\ THE LOAD. Each layer file loads alone, as an entry, in a fresh engine run in
\ the root, so a word it names that neither the engine nor its own require
\ closure defines fails there as E-UNDEFINED, whatever another file requires.
\ With the walk: every package a layer file uses is the engine's or lies in a
\ closure the walk checked.
\
\ What it guards is the layering of honest HBR2 packages that T23 requires. It
\ refuses what it cannot read rather than guess a path, and it is no sandbox
\ against source written to evade it.
\
\ Only the layers are checked: a test or tool that loads several layers at once
\ is not one of them. docs/package-build.md §4.5 keeps governing compile units.

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/fs.f
require lib/fs-list.f
require lib/sort.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/engine-candidate.f
require tools/lint/text.f
require tools/lint/source-lex.f
require tools/lint/lib.f
require test/load-refs.f
require test/suite-budget.f              \ CHILD-MS, the load's hang guard

package PACKAGE-DAG-LINT
private

\ ---- the layers --------------------------------------------------------------
\ A layer imports only layers declared before it, so the layer graph is acyclic
\ by construction. HBR2 keeps RUNTIME from every other layer (§1.2, T23), lets
\ UI import RUNTIME but not RENDER, SYNC or BROWSER (§1.2, §27.1), lets SCENE,
\ RENDER and SYNC import RUNTIME, the foundation, but not BROWSER (§27.1), and
\ makes BROWSER the adapter that may import every portable layer (§27.1). Where
\ HBR2 is silent (UI -> SCENE; SCENE, RENDER and SYNC among themselves) the
\ strictest choice is Habu's own, loosened by one ALLOWS edit when those layers
\ get their first files.

0 constant L-RUNTIME
1 constant L-UI
2 constant L-SCENE
3 constant L-RENDER
4 constant L-SYNC
5 constant L-BROWSER
-1 constant NO-LAYER

: BIT ( n -- n )
   1 swap lshift ;

: LAYER$ ( n -- ptr u8 n ) {: l:n :}
   l L-RUNTIME = if s" RUNTIME" exit then
   l L-UI = if s" UI" exit then
   l L-SCENE = if s" SCENE" exit then
   l L-RENDER = if s" RENDER" exit then
   l L-SYNC = if s" SYNC" exit then
   s" BROWSER" ;

: ALLOWS ( n -- n ) {: l:n :}
   l L-RUNTIME = if 0 exit then
   l L-BROWSER = if L-BROWSER BIT 1- exit then
   L-RUNTIME BIT ;

7 constant DIR-N

: DIR$ ( n -- ptr u8 n ) {: row:n :}
   row 0 = if s" lib/runtime/" exit then
   row 1 = if s" lib/ui/" exit then
   row 2 = if s" lib/scene/" exit then
   row 3 = if s" lib/render/" exit then
   row 4 = if s" lib/sync/" exit then
   row 5 = if s" lib/browser/" exit then
   s" host/browser/" ;

: DIR-LAYER ( n -- n ) {: row:n :}
   row 6 = if L-BROWSER exit then
   row ;

: LAYER-OF ( ptr u8 n -- n ) {: a:ptr u:n :}
   DIR-N 0 ?do
      a u i DIR$ STARTS-WITH? if i DIR-LAYER unloop exit then
   loop
   NO-LAYER ;

: FORBIDDEN? ( n n -- bool ) {: from:n to:n :}
   to NO-LAYER = to from = or if false exit then
   from ALLOWS to BIT and 0= ;

\ ---- the graph ---------------------------------------------------------------
\ A node is a file together with the root its imports resolve against, so one
\ file can be several nodes; the first stands for the file (PRIME). A file is
\ named by its canonical path, relative to the root when it lies under it.

DYNAMIC-BUFFER PATH-BYTES u8             \ the names and roots
DYNAMIC-BUFFER PATH-OFF n
DYNAMIC-BUFFER PATH-LEN n
DYNAMIC-BUFFER OWN-OFF n                 \ the root its imports resolve against
DYNAMIC-BUFFER OWN-LEN n
DYNAMIC-BUFFER PRIME n
DYNAMIC-BUFFER LAYER n
DYNAMIC-BUFFER EDGE-OFF n                \ its first edge; -1 until it is read
DYNAMIC-BUFFER EDGE-CNT n
DYNAMIC-BUFFER SEEN n                    \ the walk that last reached it
DYNAMIC-BUFFER PARENT n                  \ the node that walk reached it from
DYNAMIC-BUFFER READ bool                 \ of a prime: its text was checked
DYNAMIC-BUFFER ON-CYCLE bool             \ of a prime: on a cycle already printed
DYNAMIC-BUFFER EDGES n
DYNAMIC-BUFFER LFILES n                  \ the layer files as entries, in path order
DYNAMIC-BUFFER QUEUE n

variable PATH-U
variable NODE-N
variable FILE-N
variable EDGE-N
variable LFILE-N
variable QHEAD
variable QTAIL
variable STAMP
variable BAD
variable FROM                            \ the layer the current walk starts in
variable SRC                             \ the prime a cycle walk starts from
variable CUR                             \ the node being read

create JOINED FS-PATH-CAP allot

\ The root, canonical: the working directory.
: CROOT$ ( -- ptr u8 n ) SOURCE-ROOT:CWD$ ;

: FILE$ ( n -- ptr u8 n ) {: id:n :}
   0 PATH-BYTES id PATH-OFF @ +
   id PATH-LEN @ ;

: OWNER$ ( n -- ptr u8 n ) {: id:n :}
   0 PATH-BYTES id OWN-OFF @ +
   id OWN-LEN @ ;

\ A canonical path's name.
: NAME ( ptr u8 n -- ptr u8 n ) CROOT$ SOURCE-ROOT:RELATIVE ;

\ The first node of a file, or -1.
: FIRST ( ptr u8 n -- n ) {: a:ptr u:n :}
   NODE-N @ 0 ?do
      i FILE$ a u STR= if i unloop exit then
   loop
   -1 ;

: FIND ( ptr u8 n ptr u8 n -- n ) {: a:ptr u:n o:ptr ou:n :}  \ id, or -1
   NODE-N @ 0 ?do
      i FILE$ a u STR= i OWNER$ o ou STR= and if i unloop exit then
   loop
   -1 ;

: STORE ( ptr u8 n -- n ) {: a:ptr u:n :}  \ where the bytes land
   a 0 PATH-BYTES PATH-U @ + u BYTE-COPY
   PATH-U @  PATH-U @ u + PATH-U ! ;

\ Neither span may lie in PATH-BYTES, which the reserve can move.
: ADD ( ptr u8 n ptr u8 n -- n ) {: a:ptr u:n o:ptr ou:n :}
   NODE-N @ {: id:n :}
   a u FIRST {: first:n :}
   PATH-U @ u + ou + PATH-BYTES-RESERVE
   id 1+ PATH-OFF-RESERVE
   id 1+ PATH-LEN-RESERVE
   id 1+ OWN-OFF-RESERVE
   id 1+ OWN-LEN-RESERVE
   id 1+ PRIME-RESERVE
   id 1+ LAYER-RESERVE
   id 1+ EDGE-OFF-RESERVE
   id 1+ EDGE-CNT-RESERVE
   id 1+ SEEN-RESERVE
   id 1+ PARENT-RESERVE
   id 1+ READ-RESERVE
   id 1+ ON-CYCLE-RESERVE
   id 1+ QUEUE-RESERVE
   a u STORE id PATH-OFF !
   u id PATH-LEN !
   o ou STORE id OWN-OFF !
   ou id OWN-LEN !
   first 0 < if id else first then id PRIME !
   first 0 < if FILE-N @ 1+ FILE-N ! then
   a u LAYER-OF id LAYER !
   -1 id EDGE-OFF !
   0 id EDGE-CNT !
   0 id SEEN !
   false id READ !
   false id ON-CYCLE !
   id 1+ NODE-N !
   id ;

: INTERN ( ptr u8 n ptr u8 n -- n ) {: a:ptr u:n o:ptr ou:n :}
   a u o ou FIND dup 0 >= if exit then
   drop a u o ou ADD ;

\ ---- resolving a load ---------------------------------------------------------
\ SOURCE-ROOT:RESOLVE searches the scope's root, then this process's working
\ directory, which is the root, then the engine's root, as a loader run in the
\ root does.

create REQ FS-PATH-CAP allot
variable REQ-U
create ANS FS-PATH-CAP allot             \ the file selected, empty for none
variable ANS-U
create ANS-ROOT FS-PATH-CAP allot        \ the root that selected it
variable ANS-ROOT-U

: ANS$ ( -- ptr u8 n ) ANS ANS-U @ ;
: ANS-ROOT$ ( -- ptr u8 n ) ANS-ROOT ANS-ROOT-U @ ;

: ASK ( -- )
   0 ANS-U !
   REQ REQ-U @ SOURCE-ROOT:RESOLVE drop {: a:ptr u:n :}
   a u FILE? 0= if exit then
   a ANS u BYTE-COPY
   u ANS-U !
   SOURCE-ROOT:RESOLVED-ROOT$ {: r:ptr ru:n :}
   r ANS-ROOT ru BYTE-COPY
   ru ANS-ROOT-U ! ;

\ Whether a load from node id selects a file, which ANS$ and ANS-ROOT$ then
\ name.
: RESOLVE ( ptr u8 n n -- bool )
   {: a:ptr u:n id:n :}
   u 0= u FS-PATH-CAP > or if false exit then
   a REQ u BYTE-COPY
   u REQ-U !
   id OWNER$ [: ASK ;] SOURCE-ROOT:WITH
   ANS-U @ 0 > ;

: EDGE+ ( n -- ) {: id:n :}
   EDGE-N @ 1+ EDGES-RESERVE
   id EDGE-N @ EDGES !
   EDGE-N @ 1+ EDGE-N ! ;

\ A require of a file the engine provides loads nothing, so it is no edge.
: REF ( ptr u8 n n n -- )
   {: a:ptr u:n line:n kind:n :}
   kind LOAD-REFS:LAUNCHES = if exit then
   a u CUR @ RESOLVE 0= if exit then
   kind LOAD-REFS:REQUIRES = ANS$ ENGINE-PROVIDES? and if exit then
   ANS$ NAME ANS-ROOT$ INTERN EDGE+ ;

\ ---- output ------------------------------------------------------------------

: N. ( n -- ) LINT-MAIN-N$ LINT-MAIN-OUT ;

: ARROW ( -- ) s"  -> " LINT-MAIN-OUT ;

: FINDING ( -- )
   BAD @ 1+ BAD !
   s" package-dag-lint: " LINT-MAIN-OUT ;

: PATH. ( n -- ) FILE$ LINT-MAIN-OUT ;

\ ---- reading a file's loads ---------------------------------------------------

\ Where a file is on disk: a name under the root is relative to it.
: DISK$ ( n -- ptr u8 n ) {: id:n :}
   id FILE$ {: a:ptr u:n :}
   a c@ 47 = if a u exit then
   CROOT$ a u JOINED JOIN-PATH JOINED swap ;

: OPAQUE ( n -- ) {: line:n :}
   FINDING CUR @ PATH. s" :" LINT-MAIN-OUT line N.
   s" : a load the walk cannot read" LINT-MAIN-OUT LINT-MAIN-LF ;

\ A file the lexer refuses cannot be checked, so it is a finding. That and an
\ opaque load are the file's, so only the first of its nodes read reports them.
: SCAN ( n -- ) {: id:n :}
   id EDGE-OFF @ 0 >= if exit then
   EDGE-N @ id EDGE-OFF !
   id CUR !
   id PRIME @ READ @ 0= {: first:bool :}
   true id PRIME @ READ !
   id DISK$ LINT-SOURCE:LOAD
   LINT-SOURCE:TEXT LINT-LEX:SOURCE
   LINT-LEX:ERROR? if
      first if FINDING s" cannot lex " LINT-MAIN-OUT id PATH. LINT-MAIN-LF then
      exit
   then
   first if [: OPAQUE ;] LOAD-REFS:OPAQUE-EACH then
   [: REF ;] LOAD-REFS:EACH
   EDGE-N @ id EDGE-OFF @ - id EDGE-CNT ! ;

: EDGE@ ( n n -- n ) {: id:n k:n :}
   id EDGE-OFF @ k + EDGES @ ;

\ ---- the layer files ---------------------------------------------------------

: BEFORE? ( ptr u8 n ptr u8 n -- bool ) {: a:ptr u:n b:ptr v:n :}
   u v min 0 ?do
      a i + c@ b i + c@ <> if a i + c@ b i + c@ < unloop exit then
   loop
   u v < ;

variable LISTED                          \ the directory being listed

: OUTSIDE ( ptr u8 n ptr u8 n -- )
   {: a:ptr u:n c:ptr cu:n :}
   FINDING s" layer file resolves outside its layer: " LINT-MAIN-OUT
   a u NAME LINT-MAIN-OUT ARROW c cu NAME LINT-MAIN-OUT LINT-MAIN-LF ;

\ A layer file is a node as an entry, under its own directory, and as a
\ require from the root. Its layer is the directory it is listed in.
: LFILE+ ( ptr u8 n -- )
   {: a:ptr u:n :}
   a u s" .f" HAS-EXT? 0= if exit then
   a u SOURCE-ROOT:CANONICAL drop {: c:ptr cu:n :}
   c cu NAME LISTED @ DIR$ STARTS-WITH? 0= if a u c cu OUTSIDE exit then
   c cu NAME c cu SOURCE-ROOT:DIRNAME INTERN {: id:n :}
   c cu NAME CROOT$ INTERN drop
   LFILE-N @ 1+ LFILES-RESERVE
   id LFILE-N @ LFILES !
   LFILE-N @ 1+ LFILE-N ! ;

create WANT FS-PATH-CAP allot            \ the name ENTRY? looks for
variable WANT-U
variable HITS

: HIT ( ptr u8 n -- )
   WANT WANT-U @ STR=CI if HITS @ 1+ HITS ! then ;

\ Whether directory a lists the name, compared without case: a filesystem may
\ fold it, so a name only case apart proves nothing absent. A directory that
\ cannot be listed throws.
: ENTRY? ( ptr u8 n ptr u8 n -- bool )
   {: a:ptr u:n name:ptr nu:n :}
   name WANT nu BYTE-COPY
   nu WANT-U !
   0 HITS !
   a u [: HIT ;] FS-LIST:EACH
   HITS @ 0 > ;

\ Whether a path below the root is absent: a name on it is not an entry of the
\ directory above it. A directory above it that cannot be listed throws.
: ABSENT? ( ptr u8 n -- bool )
   {: a:ptr u:n :}
   a u SOURCE-ROOT:DIRNAME {: p:ptr pu:n :}
   pu CROOT$ nip > if p pu recurse if true exit then then
   p pu a u BASENAME ENTRY? 0= ;

\ Only an absent layer directory holds no layer file: one the lint cannot read,
\ or that is not a directory, throws here.
: WALK-LAYER ( n -- n )
   {: row:n :}
   CROOT$ row DIR$ JOINED JOIN-PATH JOINED swap {: a:ptr u:n :}
   a u DIR? 0= if a u 1- ABSENT? if row exit then then
   a u [: LFILE+ ;] WALK-FILES
   row ;

: LIST-DIR ( n -- )
   {: row:n :}
   row LISTED !
   row [: WALK-LAYER ;] catch {: code:n :}
   drop
   code 0= if exit then
   FINDING s" cannot read layer directory " LINT-MAIN-OUT row DIR$ LINT-MAIN-OUT
   s" , threw " LINT-MAIN-OUT code N. LINT-MAIN-LF ;

\ An empty buffer has no element 0 to sort from.
: LIST ( -- )
   DIR-N 0 ?do i LIST-DIR loop
   LFILE-N @ 0= if exit then
   0 LFILES LFILE-N @ [: FILE$ rot FILE$ 2swap BEFORE? ;] SORT:SORT! ;

: LFILE ( n -- n ) LFILES @ ;

\ ---- walks -------------------------------------------------------------------
\ Breadth first, so the chain printed for a finding is a shortest one.

: MARK ( n n -- ) {: id:n parent:n :}
   STAMP @ id SEEN !
   parent id PARENT ! ;

: SEEN? ( n -- bool ) SEEN @ STAMP @ = ;

: ENQUEUE ( n -- ) {: id:n :}
   id QTAIL @ QUEUE !
   QTAIL @ 1+ QTAIL ! ;

: DEQUEUE ( -- n )
   QHEAD @ QUEUE @  QHEAD @ 1+ QHEAD ! ;

: WALK-RESET ( -- )
   STAMP @ 1+ STAMP !
   0 QHEAD ! 0 QTAIL ! ;

\ Both nodes of a layer file start a walk.
: START ( n -- ) {: id:n :}
   id -1 MARK  id ENQUEUE
   id FILE$ CROOT$ FIND {: root:n :}
   root SEEN? if exit then
   root -1 MARK  root ENQUEUE ;

\ The chain the walk reached a node by, from its start.
: CHAIN ( n -- ) {: id:n :}
   id PARENT @ dup 0 >= if recurse ARROW else drop then
   id PATH. ;

\ ---- require cycles ----------------------------------------------------------
\ A walk from a layer file that comes back to it prints the shortest cycle
\ through it. A layer file on a cycle already printed is not walked again, and
\ a cycle wholly outside the layers is not this lint's to refuse.

: CYCLE ( n -- ) {: last:n :}
   FINDING s" require cycle: " LINT-MAIN-OUT last CHAIN ARROW SRC @ PATH. LINT-MAIN-LF
   last begin dup 0 >= while
      true over PRIME @ ON-CYCLE !
      PARENT @
   repeat drop ;

: CLOSES? ( n n -- bool ) {: next:n from:n :}
   next PRIME @ SRC @ = if from CYCLE true exit then
   next SEEN? 0= if next from MARK  next ENQUEUE then
   false ;

: CLOSED? ( n -- bool ) {: id:n :}
   id SCAN
   id EDGE-CNT @ 0 ?do
      id i EDGE@ id CLOSES? if true unloop exit then
   loop
   false ;

: CYCLE-FROM ( n -- ) {: id:n :}
   id PRIME @ ON-CYCLE @ if exit then
   WALK-RESET
   id PRIME @ SRC !
   id START
   begin QHEAD @ QTAIL @ < while
      DEQUEUE CLOSED? if exit then
   repeat ;

: CYCLES ( -- )
   LFILE-N @ 0 ?do i LFILE CYCLE-FROM loop ;

\ ---- forbidden imports -------------------------------------------------------
\ A walk from all of one layer's files; the first file of a layer it may not
\ import is printed with the chain that reached it, and not walked past.

: FORBIDDEN ( n -- ) {: id:n :}
   FINDING FROM @ LAYER$ LINT-MAIN-OUT s"  may not import " LINT-MAIN-OUT
   id LAYER @ LAYER$ LINT-MAIN-OUT s" : " LINT-MAIN-OUT
   id CHAIN LINT-MAIN-LF ;

: REACH ( n n -- ) {: next:n from:n :}
   next SEEN? if exit then
   next from MARK
   FROM @ next LAYER @ FORBIDDEN? if next FORBIDDEN exit then
   next ENQUEUE ;

: EXPAND ( n -- ) {: id:n :}
   id SCAN
   id EDGE-CNT @ 0 ?do id i EDGE@ id REACH loop ;

: IMPORTS ( n -- ) {: l:n :}
   l FROM !
   WALK-RESET
   LFILE-N @ 0 ?do
      i LFILE LAYER @ l = if i LFILE START then
   loop
   begin QHEAD @ QTAIL @ < while DEQUEUE EXPAND repeat ;

\ ---- each layer file alone ---------------------------------------------------

$10000 constant CAP
DYNAMIC-BUFFER CAP-OUT u8
DYNAMIC-BUFFER CAP-ERR u8

: CAPTURE>N ( result<pcap:captured,pcap:failed> -- n n n )   \ outn errn code (0 on clean exit)
   MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE {: o:len e:len :} o LEN>N e LEN>N 0 ENDOF
     err OF PCAP-FAILED:UNMAKE  {: o:len e:len c:rc :} o LEN>N e LEN>N c RC>N ENDOF
   ;MATCH ;

: LOAD-ARGV ( n -- ) {: id:n :}
   PROC-ARGV-RESET
   PROC-ENV-RESET
   PROC-ENV-INHERIT-MISSING
   s" --load" >LEN PROC-ARGV+
   id FILE$ >LEN PROC-ARGV+ ;

\ The child runs in this process's working directory, the root.
: LOAD-RUN ( -- n n n )
   CAP CAP-OUT-RESERVE
   CAP CAP-ERR-RESERVE
   ENGINE-CANDIDATE:PATH$ >LEN
   0 CAP-OUT CAP >LEN 0 CAP-ERR CAP >LEN
   SUITE-BUDGET:CHILD-MS >MS
   RUN-ARGV-ENV-CAPTURE
   CAPTURE>N ;

: LOAD-ALONE ( n -- ) {: id:n :}
   id LOAD-ARGV
   LOAD-RUN {: outn:n errn:n code:n :}
   code 0= if exit then
   FINDING id PATH. s"  does not load alone, exit " LINT-MAIN-OUT code N. LINT-MAIN-LF
   0 CAP-OUT outn LINT-MAIN-OUT
   0 CAP-ERR errn LINT-MAIN-OUT ;

\ ---- the run -----------------------------------------------------------------

: RESET ( -- )
   0 PATH-U ! 0 NODE-N ! 0 FILE-N ! 0 EDGE-N ! 0 LFILE-N ! 0 STAMP ! 0 BAD ! ;

: SUMMARY ( -- )
   s" package-dag-lint: " LINT-MAIN-OUT LFILE-N @ N. s"  layer file(s), " LINT-MAIN-OUT
   FILE-N @ N. s"  file(s) read, " LINT-MAIN-OUT BAD @ N. s"  finding(s)" LINT-MAIN-OUT
   LINT-MAIN-LF ;

public

\ Check the layers of the tree in the working directory and answer the number of
\ findings, each printed through LINT-OUT-WRITE: the walk's, and each layer file
\ that does not load alone, as an entry in a fresh engine run there. That load is
\ where a word the file names fails when neither the engine nor its own require
\ closure defines it.
: CHECK ( -- n )
   RESET
   LIST
   CYCLES
   L-BROWSER 1+ 0 ?do i IMPORTS loop
   LFILE-N @ 0 ?do i LFILE LOAD-ALONE loop
   SUMMARY
   BAD @ ;

\ CHECK; any finding throws 1, the exit status.
: STRICT ( -- )
   CHECK 0 > if 1 throw then ;

;package
