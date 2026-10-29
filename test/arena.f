\ ARENA records come from the real definer and runtime: each is zeroed, another
\ package's family member included, distinct and keeps its place while the
\ arena grows, each owner's arena is its own, a released arena is reusable, a
\ refused declaration defines nothing, its family included, a DYNAMIC-BUFFER,
\ ARENA or CWORK-VARIABLE whose element holds an owner is refused, a family
\ refusal a multi-error load counts defines no arena words, a top-level name too
\ long for its family is STRUCTURE's refusal, no typed access and no cast copies
\ or forges an owner's head, checked code can neither call nor tick the arena
\ runtime words outside ARENA's generated words, the cast, CWORK-VARIABLE and
\ call refusals live and under tools/check.f --verify-only,
\ a fetch through a family missing its argument is refused for its arity, no
\ package defines its own `field-project`, and a live arena refuses capture,
\ naming itself on stderr, unless a capture hook releases it.
require src/lib/test.f
require src/lib/string.f
require src/lib/process-argv.f
require src/lib/engine-candidate.f
require src/lib/image-lifecycle.f
require test/e2e-tree.f

package ARENA-TEST

STRUCTURE rec DERIVE addr FIELD id n FIELD note ptr u8 FIELD span SPAN:span<u8> ;STRUCTURE
ARENA RECS rec
STRUCTURE pool DERIVE addr FIELD items recs ;STRUCTURE
2 TYPED-BUFFER POOLS pool
TYPED-VARIABLE OWN-A recs
TYPED-VARIABLE OWN-B recs

\ Each owner takes 100 records, three chunks of 32, 64 and 128; owner o's
\ records are remembered at o*100 on.
100 constant PER-OWNER
200 TYPED-BUFFER SEEN ptr rec

TYPED-VARIABLE EVAL-A ptr u8
variable EVAL-U
: EVAL-RUN ( -- ) EVAL-A @ EVAL-U @ INCLUDE-EVALUATE ;
: EVAL ( ptr u8 n -- n ) EVAL-U ! EVAL-A ! [: EVAL-RUN ;] catch ;

\ Families an ARENA of the same name duplicates: HELD has the head field the
\ arena's words would reach, TAKEN does not.
STRUCTURE held DERIVE addr FIELD head ptr u8 ;STRUCTURE
STRUCTURE taken DERIVE addr FIELD x n ;STRUCTURE

\ The refusals counted and the throw code when the text is evaluated under the
\ multi-error load state.
: MULTI ( ptr u8 n -- n n ) MULTI-ERR-BEGIN EVAL MULTI-ERR-END ;

\ `ARENA ovf ptr<spaces>rec`: an element type padded until the declaration's
\ text passes the generator's buffer, refused with 7121 while that text is
\ built.
4000 constant OVF-PAD
OVF-PAD 32 + BUFFER: OVF-SRC
variable OVF-U
: OVF, ( ptr u8 n -- )
   {: a:ptr u:n :}
   u 0 ?do a i + c@ OVF-SRC OVF-U @ + c! OVF-U @ 1 + OVF-U ! loop ;


: OVF-TEXT ( -- ptr u8 n )
   0 OVF-U !  s" ARENA ovf ptr" OVF,
   OVF-PAD 0 ?do s"  " OVF, loop
   s" rec" OVF,  OVF-SRC OVF-U @ ;


: TAKE ( n -- )
   {: owner:n :}
   PER-OWNER 0 do
      owner POOLS POOL-ITEMS RECS-NEW
      dup REC-ID @ 0 T=
      dup REC-NOTE @ NULL-PTR = TTRUE
      dup REC-SPAN @ SPAN:LEN 0 T=
      owner PER-OWNER * i + over REC-ID !
      owner PER-OWNER * i + SEEN !
   loop ;


\ A record that moved or overlapped another would have lost its id.
: KEPT ( n -- )
   {: owner:n :}
   PER-OWNER 0 do
      owner PER-OWNER * i + dup SEEN @ REC-ID @ swap T=
   loop ;


: GROWTH ( -- )
   s" records are zeroed, distinct and kept as the arena grows" T-LABEL
   0 TAKE 0 KEPT
   s" a second owner's arena is its own" T-LABEL
   1 TAKE 0 KEPT 1 KEPT
   0 POOLS POOL-ITEMS RECS-RELEASE
   1 KEPT
   0 POOLS POOL-ITEMS RECS-RELEASE
   s" a released arena hands out zeroed records again" T-LABEL
   0 POOLS POOL-ITEMS RECS-NEW REC-ID @ 0 T=
   0 POOLS POOL-ITEMS RECS-RELEASE
   1 POOLS POOL-ITEMS RECS-RELEASE ;


\ The offsets where the line holding offset `at` starts and ends.
: LINE-START ( ptr u8 n -- n )
   {: a:ptr at:n :}
   at begin dup 0 > while
      a over 1 - + c@ 10 = if exit then 1 -
   repeat ;


: LINE-END ( ptr u8 n n -- n )
   {: a:ptr u:n at:n :}
   at begin dup u < while
      a over + c@ 10 = if exit then 1 +
   repeat ;


\ The child's stderr line giving the reason a name past the family-name cap
\ gets, empty when there is none.
: CAP-LINE$ ( -- ptr u8 n )
   E2E-TREE:ERR$ {: a:ptr u:n :}
   a u s" : name longer than 255 bytes at '" FIND-SUB MATCH option
     none OF NULL-PTR 0 exit ENDOF
     some OF IDX>N ENDOF
   ;MATCH {: at:n :}
   a at LINE-START {: st:n :}
   a u at LINE-END {: en:n :}
   a st +  en st - ;


: REFUSALS ( -- )
   s" a refused element type defines nothing" T-LABEL
   s" ARENA BAD u16" EVAL CHECKER-REJECT-RC T=
   s" BAD-NEW" get-current search-wl 0= TTRUE
   s" ARENA WORSE no-such-type" EVAL CHECKER-REJECT-RC T=
   s" WORSE-NEW" get-current search-wl 0= TTRUE
   s" an element that lives in place is refused where storage moves or unmaps" T-LABEL
   s" DYNAMIC-BUFFER HOLDS pool" EVAL CHECKER-REJECT-RC T=
   s" HOLDS-RESERVE" get-current search-wl 0= TTRUE
   s" ARENA OUTER recs" EVAL CHECKER-REJECT-RC T=
   s" OUTER-NEW" get-current search-wl 0= TTRUE
   s" 0 CWORK-VARIABLE HELD-CW recs" EVAL CHECKER-REJECT-RC T=
   s" HELD-CW" get-current search-wl 0= TTRUE
   s" a declaration refused while it is built defines no family" T-LABEL
   OVF-TEXT EVAL 7121 T=
   s" OVF-NEW" get-current search-wl 0= TTRUE
   s" ARENA OVF rec" EVAL 0 T=
   s" TYPED-VARIABLE OVF-POOL ovf" EVAL 0 T=
   s" OVF-POOL OVF-NEW drop OVF-POOL OVF-RELEASE" EVAL 0 T=
   s" a family refusal counted by a multi-error load defines no arena words" T-LABEL
   s" ARENA HELD rec" MULTI 1 T= 0 T=
   s" HELD-NEW" get-current search-wl 0= TTRUE
   s" HELD-RELEASE" get-current search-wl 0= TTRUE
   s" ARENA TAKEN rec" MULTI 1 T= 0 T=
   s" TAKEN-NEW" get-current search-wl 0= TTRUE
   s" TAKEN-RELEASE" get-current search-wl 0= TTRUE
   s" a top-level name too long for its family is STRUCTURE's refusal" T-LABEL
   E2E-TREE:ERR$ s" habu: bad structure declaration 'a-a-" CONTAINS? TTRUE
   E2E-TREE:ERR$ s" : constructor package name exceeds 255 bytes" CONTAINS? TTRUE
   E2E-TREE:OUT$ S\" arena-long: 7154\nundefined\n" CONTAINS? TTRUE
   s" a multi-error load counts that refusal and defines nothing" T-LABEL
   E2E-TREE:OUT$ S\" arena-long-multi: 1\n0\nundefined\n" CONTAINS? TTRUE
   s" a top-level name past the family-name cap is refused as a bare STRUCTURE refuses it" T-LABEL
   CAP-LINE$ s" habu: bad structure declaration 'a-a-" STARTS-WITH? TTRUE
   E2E-TREE:ERR$ CAP-LINE$ COUNT-SUB 2 T=
   E2E-TREE:OUT$ S\" arena-cap: 7118\n7118\nundefined\n" CONTAINS? TTRUE ;


\ An owner, and a record holding one, lives in place: a copy would share its
\ chunks, and growing or releasing both would unmap them twice.
: COPIES ( -- )
   s" checked code copies no arena owner through @ or !" T-LABEL
   s" : CP ( -- ) OWN-A @ OWN-B ! ;" EVAL CHECKER-REJECT-RC T=
   s" nor a record that holds one" T-LABEL
   s" : HC ( -- ) 0 POOLS @ 1 POOLS ! ;" EVAL CHECKER-REJECT-RC T=
   s" nor store an owner value it was given" T-LABEL
   s" : ST ( recs -- ) OWN-A ! ;" EVAL CHECKER-REJECT-RC T=
   s" no cast hands out an owner's head as bytes or forges one from them" T-LABEL
   s" CAST: >OWN-BYTES ( ptr recs -- ptr u8 )" EVAL E-CAST-OWNER T=
   s" >OWN-BYTES" get-current search-wl 0= TTRUE
   s" CAST: >OWN-FORGE ( ptr u8 -- ptr recs )" EVAL E-CAST-OWNER T=
   s" >OWN-FORGE" get-current search-wl 0= TTRUE
   s" nor unpacks a view of owners into bytes" T-LABEL
   s" CAST: >VIEW-BYTES ( read-view<p,q,recs> -- ptr u8 n )" EVAL E-CAST-OWNER T=
   s" >VIEW-BYTES" get-current search-wl 0= TTRUE ;


\ The refusals' stderr lines are the boundary under test, and a top-level
\ declaration needs a file outside this package, so they run in a child.
: CHILD ( -- )
   s" test/arena-live.f" E2E-TREE:ARGS
   ENGINE-CANDIDATE:PATH$ SOURCE-ROOT:CWD$ E2E-TREE:RUN-ON ;


: LIVE-CHILD ( -- )
   s" capture names the live arena on stderr" T-LABEL
   E2E-TREE:ERR$ s" hb: capture: live arena TOP-RECS" CONTAINS? TTRUE
   E2E-TREE:ERR$ s" hb: capture: live arena ARENA-LIVE:LIVE-RECS" CONTAINS? TTRUE
   E2E-TREE:OUT$ S\" arena-live: 7211\n7211\n0\n" CONTAINS? TTRUE
   E2E-TREE:RC@ 0 T= ;


\ At top level no head accessor exists to plant bytes as a chunk or to copy a
\ head, and an owner is refused whole at the fetch that would copy it.
: OWNER-CHILD ( -- )
   s" no head accessor reaches an owner's head" T-LABEL
   E2E-TREE:ERR$ s" E-UNDEFINED: FOOS:HEAD at " COUNT-SUB 2 T=
   s" an owner is never fetched or stored whole" T-LABEL
   E2E-TREE:ERR$ s" in place: an arena owner is never fetched or stored whole" COUNT-SUB 2 T=
   E2E-TREE:OUT$ S\" arena-forge: 70\narena-head-copy: 70\narena-owner-copy: 70\narena-owner-copy-run: 70\n" CONTAINS? TTRUE
   s" a fetch through a family missing its argument is refused for its arity" T-LABEL
   E2E-TREE:ERR$ s" in g1: wrong arity for type family 'box'" CONTAINS? TTRUE
   E2E-TREE:OUT$ S\" arena-arity: 70\n" CONTAINS? TTRUE ;


\ ARENA's NAME-NEW and NAME-RELEASE and STRUCTURE's accessors bind the engine's
\ projection: a package's own `field-project` is refused at its definition.
: CAPTURE-CHILD ( -- )
   s" no package defines its own field-project" T-LABEL
   E2E-TREE:ERR$ s" reserved word cannot be a definition name: field-project" COUNT-SUB 2 T=
   E2E-TREE:OUT$ S\" arena-capture: 70\nstruct-capture: 70\n" CONTAINS? TTRUE ;


\ Every call or tick of ARENA-NEW or ARENA-RELEASE outside ARENA's generated
\ words is refused at its token, in a body, a quotation, at top level and
\ through a re-export, on a user-declared head or on NULL-PTR; naming the head
\ type stays inert.
: CALL-CHILD ( -- )
   s" only ARENA's generated words call the arena runtime words" T-LABEL
   E2E-TREE:ERR$ s" arena call: only ARENA's generated NAME-NEW and NAME-RELEASE call ARENA-NEW and ARENA-RELEASE" COUNT-SUB 9 T=
   E2E-TREE:OUT$ S\" arena-sig: 0\narena-new: 70\narena-release: 70\narena-null-new: 70\narena-null-release: 70\narena-tick: 70\narena-quot: 70\narena-top-tick: 70\narena-top-call: 70\narena-export: 70\n" CONTAINS? TTRUE ;


\ tools/check.f --verify-only checks a file without running it, so it gets its
\ own child over a scratch tree's copy of the calls, the workspace variable and
\ a head cast. A refused cast stops the verification with its throw code, so
\ the cast is last and the forge direction is a live row only.
: VERIFY-SRC$ ( -- ptr u8 n )
   S\" : NM ( -- ptr u8 n ) s\" x\" ;\nTYPED-VARIABLE HH DYNAMIC-STORAGE:arena-head\n: Z2 ( -- ptr u8 ) HH 8 NM DYNAMIC-STORAGE:ARENA-NEW ;\n: Z3 ( -- ) HH DYNAMIC-STORAGE:ARENA-RELEASE ;\n: Y1 ( -- ptr u8 ) NULL-PTR 8 NM DYNAMIC-STORAGE:ARENA-NEW ;\n: Y2 ( -- ) NULL-PTR DYNAMIC-STORAGE:ARENA-RELEASE ;\npackage ARENA-CAST\n0 CWORK-VARIABLE CW DYNAMIC-STORAGE:arena-head\nCAST: >HEAD-BYTES ( ptr DYNAMIC-STORAGE:arena-head -- ptr u8 )\n;package\n" ;

: VERIFY-CALLS ( -- )
   s" tools/check.f --verify-only refuses the same calls, the workspace variable and a head cast" T-LABEL
   s" arena-verify" E2E-TREE:TREE
   E2E-TREE:ROOT$ FS:CLEANUP-TREE+
   s" test/arena-calls.f" E2E-TREE:AT-A VERIFY-SRC$ FS:WRITE-ALL
   s" tools/check.f" E2E-TREE:ARGS
   s" --" >LEN PROC-ARGV+
   s" --verify-only" >LEN PROC-ARGV+
   s" test/arena-calls.f" >LEN PROC-ARGV+
   ENGINE-CANDIDATE:PATH$ E2E-TREE:ROOT$ E2E-TREE:RUN-ON
   E2E-TREE:ERR$ S\" \"code\":\"E-ARENA-CALL\"" COUNT-SUB 4 T=
   E2E-TREE:OUT$ s" verification stopped by throw 7135" CONTAINS? TTRUE
   E2E-TREE:ERR$ S\" \"code\":\"E-BAD-STORAGE\"" COUNT-SUB 1 T=
   E2E-TREE:RC@ 0 <> TTRUE ;


\ Last: PREPARE runs every capture hook this process registered.
: LIVE-CAPTURE ( -- )
   s" a live arena refuses capture until it is released" T-LABEL
   0 POOLS POOL-ITEMS RECS-NEW drop
   [: IMAGE-LIFECYCLE:PREPARE ;] catch DYNAMIC-STORAGE:E-ARENA-LIVE T=
   0 POOLS POOL-ITEMS RECS-RELEASE
   [: IMAGE-LIFECYCLE:PREPARE ;] catch 0 T= ;


\ The guard runs after every hook, so an owner can release its arena through
\ the lifecycle's own registry.
: HOOK-RELEASE ( -- )
   s" a capture hook that releases its arena admits the capture" T-LABEL
   1 POOLS POOL-ITEMS RECS-NEW drop
   [: 1 POOLS POOL-ITEMS RECS-RELEASE ;] IMAGE-LIFECYCLE:REGISTER
   [: IMAGE-LIFECYCLE:PREPARE ;] catch 0 T= ;


: RUN ( -- )
   T-RESET
   CHILD GROWTH REFUSALS COPIES OWNER-CHILD CALL-CHILD CAPTURE-CHILD LIVE-CHILD
   VERIFY-CALLS
   LIVE-CAPTURE HOOK-RELEASE
   T-REPORT ;

RUN
;package
