\ maki/report.f - Model CAD structured report schema v1 (dot cad-0a).
\
\ The report object carries every field the Model CAD REPL emits: model/shape/
\ dtype/layout/target keys, fusion plan summary, materialized tensors, estimated
\ bytes before/after, per-hot-tensor coalescing status, schedule candidates and
\ the selected one, the four gate verdicts, profile rows, roofline class, the
\ artifact/cache key, and warnings/split reasons. It is the single structured
\ artifact behind every command (docs/model-cad.md Phase 0).
\
\ Representation hiding: a report is an opaque `report` handle (DEFTYPE). Every
\ public constructor/accessor takes or returns `report` plus primitive field
\ values; no signature leaks the record layout. The checker guarantees only a
\ real `report` (from REPORT:NEW) reaches an accessor, so the internals can swap
\ to the sum/enum/product ADT families later (dot cad-adt-swap, docs/type-families.md)
\ without touching a caller. Phase 0 keeps ONE live report (a module-owned store);
\ REPORT:NEW resets it. Multi-report values arrive with the ADT swap.
\
\ Namespace split: the stateful report builder/renderer is package REPORT
\ (callers write REPORT:NEW, REPORT:GATE!, REPORT:RENDER). The verdict/roofline/
\ coalescing/gate-id enums are cross-cutting value substrate shared by the whole
\ CAD pipeline and the device tests, so they stay in the MAKI substrate package.
\
\ Fail closed: an out-of-range verdict tag, gate id, roofline class, coalescing
\ status, or list index is rejected with a named throw code -- never defaulted.
\ maki owns its error range (-5000..-5099); report uses -5010..-5019.

require lib/prelude.f
require lib/string.f
require lib/float.f
require lib/fmt.f

-5010 constant E-RPT-HANDLE    \ report handle unusable (reserved; see REPORT:NEW)
-5011 constant E-RPT-FULL      \ arena or list capacity exceeded
-5012 constant E-RPT-GATE      \ gate id out of range
-5013 constant E-RPT-VERDICT   \ verdict tag out of range
-5014 constant E-RPT-ROOF      \ roofline class out of range
-5015 constant E-RPT-COAL      \ coalescing status out of range
-5016 constant E-RPT-IDX       \ list index out of range

package MAKI
public

\ ---- gate verdict tags ----
\ V-* stay the public numeric vocabulary of the representation-hiding accessors
\ (REPORT:GATE! / REPORT:GATE-TAG@ keep their n signatures); internally the four
\ G-TAG cells store a real ENUM (dot habu-cad-adt-swap, capability S1) and convert
\ at that accessor boundary only. Constructors: MAKI-VERDICT:PASS/FAIL/NOT-RUN.
0 constant V-PASS
1 constant V-FAIL
2 constant V-NOTRUN
3 constant V-N
ENUM verdict
  pass
  fail
  not-run
;ENUM

\ ---- roofline classes ----
\ RC-* stay the public numeric vocabulary of the representation-hiding accessors
\ (REPORT:ROOFLINE! / REPORT:ROOFLINE@ keep their n signatures); internally the
\ report stores a real ENUM (dot habu-cad-adt-swap, capability S1) and converts
\ at that accessor boundary only. Constructors: MAKI-ROOFLINE:UNKNOWN/MEMORY/COMPUTE.
0 constant RC-UNKNOWN
1 constant RC-MEMORY
2 constant RC-COMPUTE
3 constant RC-N
ENUM roofline
  unknown
  memory
  compute
;ENUM

\ ---- per-tensor coalescing status (CAD-PLAN 6.4 access vocabulary) ----
\ CO-* stay the public numeric vocabulary of the representation-hiding accessors
\ (REPORT:HOT+ / REPORT:HOT-STATUS@ keep their n signatures); internally the hot
\ list stores a real ENUM (dot habu-cad-adt-swap, capability S1) and converts at
\ that accessor boundary only. Constructors: MAKI-COSTATUS:UNKNOWN/../GATHERED.
0 constant CO-UNKNOWN
1 constant CO-COALESCED         \ w=1 contiguous unit-stride global access
2 constant CO-STRIDED           \ non-unit innermost stride (column-major read)
3 constant CO-UNALIGNED         \ base alignment below element width -> scalar fallback
4 constant CO-COALESCED-V4      \ w=4 vectorized contiguous access (tile-v4)
5 constant CO-BROADCAST         \ 1-row/1-col input hoisted to a register (6.2.3)
6 constant CO-GATHERED          \ indexed read downstream of a gather
7 constant CO-N
ENUM costatus
  unknown
  coalesced
  strided
  unaligned
  coalesced-v4
  broadcast
  gathered
;ENUM

\ ---- gate ids (index into the fixed 4-gate array) ----
0 constant G-CERTIFY
1 constant G-GOLDEN
2 constant G-GRADCHECK
3 constant G-PROFILE
4 constant G-N

end-package

package REPORT

DEFTYPE report                 \ opaque handle; internals swap to an ADT in cad-adt-swap

16 constant RPT-LCAP           \ max items per growable list
128 constant RPT-CAND-CAP      \ schedule candidates: a whole family space (cad-4, <= 72)
$4000 constant RPT-ARENA-CAP   \ interned-string byte arena (headroom for a full candidate set)
$4000 constant RPT-OUT-CAP     \ render output buffer

\ interned-string arena (all report strings live here; reset by NEW)
create RPT-ARENA RPT-ARENA-CAP allot
variable RPT-ARENA-U

\ render output buffer
create RPT-OUT RPT-OUT-CAP allot
variable RPT-OUT-U

\ scalar fields
variable F-OPS-BEFORE
variable F-OPS-AFTER
variable F-REGIONS
variable F-MATERIALIZED
variable F-BYTES-BEFORE
variable F-BYTES-AFTER
variable F-BYTES-KNOWN
variable F-ROOFLINE            \ holds a `roofline` enum (typed slot below)
variable F-SELECT

\ typed roofline slot: retype the cell address to `ptr roofline` so the enum
\ store/fetch certifies with family identity (capability S1); a raw n or a
\ foreign family cannot reach this cell.
: F-ROOFLINE-AT ( -- ptr roofline )  F-ROOFLINE ;

\ key strings as (offset,length) into the arena; length 0 means unset
variable K-MODEL-O   variable K-MODEL-L
variable K-SHAPE-O   variable K-SHAPE-L
variable K-DTYPE-O   variable K-DTYPE-L
variable K-LAYOUT-O  variable K-LAYOUT-L
variable K-TARGET-O  variable K-TARGET-L
variable K-CACHE-O   variable K-CACHE-L

\ growable string lists: (offset,length) pairs, stride 2
create L-SPLIT RPT-LCAP 2 * cells allot   variable N-SPLIT
create L-WARN  RPT-LCAP 2 * cells allot   variable N-WARN
create L-CAND  RPT-CAND-CAP 2 * cells allot   variable N-CAND

\ hot tensors: (name-offset,name-length,costatus enum) triples, stride 3
create L-HOT  RPT-LCAP 3 * cells allot    variable N-HOT

\ typed coalescing-status slot: retype the third cell of a hot triple to
\ `ptr costatus` so the enum store/fetch certifies with family identity
\ (capability S1); a raw n or a foreign family cannot reach these cells.
: HOT-ST-AT ( n -- ptr costatus )  3 * 2 + cells L-HOT + ;
\ profile rows: (name-offset,name-length,device-us) triples, stride 3
create L-PROF RPT-LCAP 3 * cells allot    variable N-PROF

\ gate verdicts: fixed 4 entries (verdict enum, reason-offset, reason-length)
create G-TAG MAKI:G-N cells allot
create G-RO  MAKI:G-N cells allot
create G-RL  MAKI:G-N cells allot

\ typed verdict slot: retype a G-TAG cell address to `ptr verdict` so the enum
\ store/fetch certifies with family identity (capability S1); a raw n or a
\ foreign family cannot reach these cells.
: G-TAG-AT ( n -- ptr verdict )  cells G-TAG + ;

\ ---- arena -----------------------------------------------------------------
: ARENA-PUT ( ptr u8 n -- n n )                \ intern bytes -> (offset length)
   {: a:ptr u:n :}
   RPT-ARENA-U @ u + RPT-ARENA-CAP > if E-RPT-FULL throw then
   RPT-ARENA-U @ {: off:n :}
   0 begin dup u < while
      dup a + c@  over off + RPT-ARENA + c!
      1+
   repeat drop
   off u + RPT-ARENA-U !
   off u ;

: ARENA-GET ( n n -- ptr u8 n )                \ (offset length) -> string span
   {: off:n len:n :}
   RPT-ARENA off + len ;

\ ---- render output buffer --------------------------------------------------
: OUT-RESET ( -- )  0 RPT-OUT-U ! ;

: OUT-C ( n -- )                               \ append one byte
   {: c:n :}
   RPT-OUT-U @ RPT-OUT-CAP >= if E-RPT-FULL throw then
   c RPT-OUT RPT-OUT-U @ + c!
   RPT-OUT-U @ 1+ RPT-OUT-U ! ;

: OUT+ ( ptr u8 n -- )                         \ append a string span
   {: a:ptr u:n :}
   0 begin dup u < while  dup a + c@ OUT-C  1+  repeat drop ;

: OUT-INT ( n -- )  SB-RESET SB-INT SB$ OUT+ ; \ SB is number-format scratch
: OUT-NL ( -- )  $0A OUT-C ;
: OUT$ ( -- ptr u8 n )  RPT-OUT RPT-OUT-U @ ;

\ ---- generic list slot -----------------------------------------------------
: SL-SLOT ( ptr a n n n -- ptr a )             \ base stride idx field -> cell addr
   >r * r> + cells + ;

\ ---- private raw field readers (used by render; no handle) -----------------
: K-MODEL$  ( -- ptr u8 n )  K-MODEL-O  @ K-MODEL-L  @ ARENA-GET ;
: K-SHAPE$  ( -- ptr u8 n )  K-SHAPE-O  @ K-SHAPE-L  @ ARENA-GET ;
: K-DTYPE$  ( -- ptr u8 n )  K-DTYPE-O  @ K-DTYPE-L  @ ARENA-GET ;
: K-LAYOUT$ ( -- ptr u8 n )  K-LAYOUT-O @ K-LAYOUT-L @ ARENA-GET ;
: K-TARGET$ ( -- ptr u8 n )  K-TARGET-O @ K-TARGET-L @ ARENA-GET ;
: K-CACHE$  ( -- ptr u8 n )  K-CACHE-O  @ K-CACHE-L  @ ARENA-GET ;

\ ---- enum -> text (fail closed) --------------------------------------------
\ verdict enum boundary (dot habu-cad-adt-swap): the stored value is the enum;
\ the public accessors keep their V-* n vocabulary, converted exactly here.
: V-CK ( n -- n )                              \ validate a V-* code
   dup 0 < over MAKI:V-N >= or if E-RPT-VERDICT throw then ;

: >VERDICT ( n -- verdict )                    \ parse boundary: V-* code -> enum
   V-CK
   dup MAKI:V-PASS = if drop MAKI-VERDICT:PASS exit then
   MAKI:V-FAIL = if MAKI-VERDICT:FAIL else MAKI-VERDICT:NOT-RUN then ;

: VERDICT>N ( verdict -- n )                   \ render boundary: enum -> V-* code
   MATCH verdict
      pass    OF MAKI:V-PASS   ENDOF
      fail    OF MAKI:V-FAIL   ENDOF
      not-run OF MAKI:V-NOTRUN ENDOF
   ;MATCH ;

: VERDICT-PASS? ( verdict -- bool )
   MATCH verdict
      pass    OF true  ENDOF
      fail    OF false ENDOF
      not-run OF false ENDOF
   ;MATCH ;

: V-NAME ( verdict -- ptr u8 n )               \ exhaustive: no bad-tag throw possible
   MATCH verdict
      pass    OF s" pass"    ENDOF
      fail    OF s" fail"    ENDOF
      not-run OF s" not-run" ENDOF
   ;MATCH ;

\ roofline enum boundary (dot habu-cad-adt-swap): the stored value is the enum;
\ the public accessors keep their RC-* n vocabulary, converted exactly here.
: RC-CK ( n -- n )                             \ validate an RC-* code
   dup 0 < over MAKI:RC-N >= or if E-RPT-ROOF throw then ;

: >ROOFLINE ( n -- roofline )                  \ parse boundary: RC-* code -> enum
   RC-CK
   dup MAKI:RC-MEMORY = if drop MAKI-ROOFLINE:MEMORY exit then
   MAKI:RC-COMPUTE = if MAKI-ROOFLINE:COMPUTE else MAKI-ROOFLINE:UNKNOWN then ;

: ROOFLINE>N ( roofline -- n )                 \ render boundary: enum -> RC-* code
   MATCH roofline
      unknown OF MAKI:RC-UNKNOWN ENDOF
      memory  OF MAKI:RC-MEMORY  ENDOF
      compute OF MAKI:RC-COMPUTE ENDOF
   ;MATCH ;

: RC-NAME ( roofline -- ptr u8 n )             \ exhaustive: no bad-tag throw possible
   MATCH roofline
      unknown OF s" unknown"       ENDOF
      memory  OF s" memory-bound"  ENDOF
      compute OF s" compute-bound" ENDOF
   ;MATCH ;

\ coalescing-status enum boundary (dot habu-cad-adt-swap): the stored value is
\ the enum; the public accessors keep their CO-* n vocabulary, converted here.
: CO-CK ( n -- n )                             \ validate a CO-* code
   dup 0 < over MAKI:CO-N >= or if E-RPT-COAL throw then ;

: >COSTATUS ( n -- costatus )                  \ parse boundary: CO-* code -> enum
   CO-CK
   dup MAKI:CO-COALESCED    = if drop MAKI-COSTATUS:COALESCED    exit then
   dup MAKI:CO-STRIDED      = if drop MAKI-COSTATUS:STRIDED      exit then
   dup MAKI:CO-UNALIGNED    = if drop MAKI-COSTATUS:UNALIGNED    exit then
   dup MAKI:CO-COALESCED-V4 = if drop MAKI-COSTATUS:COALESCED-V4 exit then
   dup MAKI:CO-BROADCAST    = if drop MAKI-COSTATUS:BROADCAST    exit then
   MAKI:CO-GATHERED = if MAKI-COSTATUS:GATHERED else MAKI-COSTATUS:UNKNOWN then ;

: COSTATUS>N ( costatus -- n )                 \ render boundary: enum -> CO-* code
   MATCH costatus
      unknown      OF MAKI:CO-UNKNOWN      ENDOF
      coalesced    OF MAKI:CO-COALESCED    ENDOF
      strided      OF MAKI:CO-STRIDED      ENDOF
      unaligned    OF MAKI:CO-UNALIGNED    ENDOF
      coalesced-v4 OF MAKI:CO-COALESCED-V4 ENDOF
      broadcast    OF MAKI:CO-BROADCAST    ENDOF
      gathered     OF MAKI:CO-GATHERED     ENDOF
   ;MATCH ;

: CO-NAME ( costatus -- ptr u8 n )             \ exhaustive: no bad-tag throw possible
   MATCH costatus
      unknown      OF s" unknown"      ENDOF
      coalesced    OF s" coalesced"    ENDOF
      strided      OF s" strided"      ENDOF
      unaligned    OF s" unaligned"    ENDOF
      coalesced-v4 OF s" coalesced-v4" ENDOF
      broadcast    OF s" broadcast"    ENDOF
      gathered     OF s" gathered"     ENDOF
   ;MATCH ;

\ ---- machine-render helpers ------------------------------------------------
: EMIT-KEY ( ptr u8 n -- )  OUT+ s" : " OUT+ ;
: EMIT-STR ( ptr u8 n -- )  dup 0= if 2drop s" unset" OUT+ else OUT+ then ;

: EMIT-K$ ( ptr u8 n ptr u8 n -- )             \ "key: value\n"
   2>r EMIT-KEY 2r> EMIT-STR OUT-NL ;

: EMIT-KN ( ptr u8 n n -- )                    \ "key: <int>\n"
   {: v:n :}  EMIT-KEY v OUT-INT OUT-NL ;

: EMIT-IDXKEY ( ptr u8 n n -- )                \ "prefix.<idx>: "
   {: idx:n :}  OUT+ $2E OUT-C idx OUT-INT s" : " OUT+ ;

\ ---- machine-render sections -----------------------------------------------
: R-KEYS ( -- )
   s" report.model"  K-MODEL$  EMIT-K$
   s" report.shape"  K-SHAPE$  EMIT-K$
   s" report.dtype"  K-DTYPE$  EMIT-K$
   s" report.layout" K-LAYOUT$ EMIT-K$
   s" report.target" K-TARGET$ EMIT-K$ ;

: R-FUSION ( -- )
   s" fusion.ops-before" F-OPS-BEFORE  @ EMIT-KN
   s" fusion.ops-after"  F-OPS-AFTER   @ EMIT-KN
   s" fusion.regions"    F-REGIONS     @ EMIT-KN
   N-SPLIT @ 0 ?do
      s" fusion.split" i EMIT-IDXKEY
      L-SPLIT 2 i 0 SL-SLOT @  L-SPLIT 2 i 1 SL-SLOT @ ARENA-GET OUT+ OUT-NL
   loop ;

: R-BYTES ( -- )
   F-BYTES-KNOWN @ 0= if
      s" memory.bytes-before" EMIT-KEY s" unknown" OUT+ OUT-NL
      s" memory.bytes-after"  EMIT-KEY s" unknown" OUT+ OUT-NL
   else
      s" memory.bytes-before" F-BYTES-BEFORE @ EMIT-KN
      s" memory.bytes-after"  F-BYTES-AFTER  @ EMIT-KN
   then
   s" memory.materialized" F-MATERIALIZED @ EMIT-KN ;

: R-COALESCE ( -- )
   N-HOT @ 0 ?do
      s" coalesce.tensor" i EMIT-IDXKEY
      L-HOT 3 i 0 SL-SLOT @  L-HOT 3 i 1 SL-SLOT @ ARENA-GET OUT+ OUT-NL
      s" coalesce.status" i EMIT-IDXKEY
      i HOT-ST-AT @ CO-NAME OUT+ OUT-NL
   loop ;

: R-SCHEDULE ( -- )
   N-CAND @ 0 ?do
      s" schedule.candidate" i EMIT-IDXKEY
      L-CAND 2 i 0 SL-SLOT @  L-CAND 2 i 1 SL-SLOT @ ARENA-GET OUT+ OUT-NL
   loop
   s" schedule.selected" F-SELECT @ EMIT-KN ;

: R-GATE ( ptr u8 n n -- )                     \ prefix gate-id -> verdict/reason lines
   {: gid:n :}
   2dup OUT+ s" .verdict: " OUT+  gid G-TAG-AT @ V-NAME OUT+ OUT-NL
   gid cells G-RL + @ 0 > if
      OUT+ s" .reason: " OUT+
      gid cells G-RO + @  gid cells G-RL + @ ARENA-GET OUT+ OUT-NL
   else
      2drop
   then ;

: R-GATES ( -- )
   s" gate.certify"   MAKI:G-CERTIFY   R-GATE
   s" gate.golden"    MAKI:G-GOLDEN    R-GATE
   s" gate.gradcheck" MAKI:G-GRADCHECK R-GATE
   s" gate.profile"   MAKI:G-PROFILE   R-GATE ;

: R-PROFILE ( -- )
   N-PROF @ 0 ?do
      s" profile.kernel" i EMIT-IDXKEY
      L-PROF 3 i 0 SL-SLOT @  L-PROF 3 i 1 SL-SLOT @ ARENA-GET OUT+ OUT-NL
      s" profile.device-us" i EMIT-IDXKEY
      L-PROF 3 i 2 SL-SLOT @ OUT-INT OUT-NL
   loop ;

: R-ROOFLINE ( -- )
   s" roofline.class" EMIT-KEY  F-ROOFLINE-AT @ RC-NAME OUT+ OUT-NL ;

: R-CACHE ( -- )
   s" cache.key" K-CACHE$ EMIT-K$ ;

: R-WARN ( -- )
   N-WARN @ 0 ?do
      s" warning" i EMIT-IDXKEY
      L-WARN 2 i 0 SL-SLOT @  L-WARN 2 i 1 SL-SLOT @ ARENA-GET OUT+ OUT-NL
   loop ;

\ ---- human-render helpers --------------------------------------------------
: H-GATE ( n -- )                              \ "<verdict>[ (reason)]\n" (label pre-emitted)
   {: gid:n :}
   gid G-TAG-AT @ V-NAME OUT+
   gid cells G-RL + @ 0 > if
      s"  (" OUT+  gid cells G-RO + @ gid cells G-RL + @ ARENA-GET OUT+  $29 OUT-C
   then
   OUT-NL ;

: H-WARN ( -- )
   N-WARN @ 0= if exit then
   s" warnings:" OUT+ OUT-NL
   N-WARN @ 0 ?do
      s"   - " OUT+  L-WARN 2 i 0 SL-SLOT @ L-WARN 2 i 1 SL-SLOT @ ARENA-GET OUT+ OUT-NL
   loop ;

\ ---- failure-packet helpers (repair-packet-core.f discipline; model-cad Phase 7) ---
: P-CLASS ( n -- ptr u8 n )                    \ gate-id -> failure class
   G-TAG-AT @ MATCH verdict                    \ only reached for non-pass gates
      pass    OF s" not-run"            ENDOF
      fail    OF s" contract-violation" ENDOF
      not-run OF s" not-run"            ENDOF
   ;MATCH ;

: P-REASON ( n -- )                            \ gate-id -> reason text (or "none")
   {: gid:n :}
   gid cells G-RL + @ 0 > if
      gid cells G-RO + @ gid cells G-RL + @ ARENA-GET OUT+
   else s" none" OUT+ then ;

: P-REPAIR ( n -- ptr u8 n )                   \ gate-id -> suggested repair family
   case
      MAKI:G-CERTIFY   of s" fix-static-legality"  endof
      MAKI:G-GOLDEN    of s" run-device-golden"    endof
      MAKI:G-GRADCHECK of s" run-device-gradcheck" endof
      MAKI:G-PROFILE   of s" run-device-profile"   endof
      drop E-RPT-GATE throw
   endcase ;

: P-GATE ( ptr u8 n n -- )                     \ prefix gate-id -> one packet line if not passed
   {: gid:n :}
   gid G-TAG-AT @ VERDICT-PASS? if 2drop exit then
   OUT+ s" : class=" OUT+  gid P-CLASS OUT+
   s"  expected=pass actual=" OUT+  gid G-TAG-AT @ V-NAME OUT+
   s"  reason=" OUT+  gid P-REASON
   s"  repair=" OUT+  gid P-REPAIR OUT+
   s"  repro=model:" OUT+  K-MODEL$ EMIT-STR
   OUT-NL ;

\ ---- handle validity (single live report; nominal type is the guarantee) ----
: RPT-DROP ( report -- )  drop ;

public

\ NEW resets the single module-owned report store and returns its handle.
\ Every field starts empty/unset/not-run so an unpopulated report renders honest
\ "unset"/"unknown"/"not-run" values rather than fabricated data.
: NEW ( -- report )
   0 RPT-ARENA-U !
   0 N-SPLIT !  0 N-WARN !  0 N-CAND !  0 N-HOT !  0 N-PROF !
   0 F-OPS-BEFORE !  0 F-OPS-AFTER !  0 F-REGIONS !  0 F-MATERIALIZED !
   0 F-BYTES-BEFORE !  0 F-BYTES-AFTER !  0 F-BYTES-KNOWN !
   MAKI-ROOFLINE:UNKNOWN F-ROOFLINE-AT !  -1 F-SELECT !
   0 K-MODEL-O !  0 K-MODEL-L !  0 K-SHAPE-O !  0 K-SHAPE-L !
   0 K-DTYPE-O !  0 K-DTYPE-L !  0 K-LAYOUT-O ! 0 K-LAYOUT-L !
   0 K-TARGET-O ! 0 K-TARGET-L ! 0 K-CACHE-O !  0 K-CACHE-L !
   MAKI:G-N 0 ?do  MAKI-VERDICT:NOT-RUN i G-TAG-AT !  0 i cells G-RO + !  0 i cells G-RL + !  loop
   0 >report ;

\ ---- key setters/getters ---------------------------------------------------
: MODEL!  ( report ptr u8 n -- report )  ARENA-PUT K-MODEL-L  ! K-MODEL-O  ! ;
: SHAPE!  ( report ptr u8 n -- report )  ARENA-PUT K-SHAPE-L  ! K-SHAPE-O  ! ;
: DTYPE!  ( report ptr u8 n -- report )  ARENA-PUT K-DTYPE-L  ! K-DTYPE-O  ! ;
: LAYOUT! ( report ptr u8 n -- report )  ARENA-PUT K-LAYOUT-L ! K-LAYOUT-O ! ;
: TARGET! ( report ptr u8 n -- report )  ARENA-PUT K-TARGET-L ! K-TARGET-O ! ;
: CACHE!  ( report ptr u8 n -- report )  ARENA-PUT K-CACHE-L  ! K-CACHE-O  ! ;

: MODEL$  ( report -- ptr u8 n )  RPT-DROP K-MODEL$ ;
: SHAPE$  ( report -- ptr u8 n )  RPT-DROP K-SHAPE$ ;
: DTYPE$  ( report -- ptr u8 n )  RPT-DROP K-DTYPE$ ;
: LAYOUT$ ( report -- ptr u8 n )  RPT-DROP K-LAYOUT$ ;
: TARGET$ ( report -- ptr u8 n )  RPT-DROP K-TARGET$ ;
: CACHE$  ( report -- ptr u8 n )  RPT-DROP K-CACHE$ ;

\ ---- fusion-plan scalars ---------------------------------------------------
: OPS! ( report n n -- report )            \ before after
   {: before:n after:n :}  before F-OPS-BEFORE !  after F-OPS-AFTER ! ;
: OPS-BEFORE@ ( report -- n )  RPT-DROP F-OPS-BEFORE @ ;
: OPS-AFTER@  ( report -- n )  RPT-DROP F-OPS-AFTER @ ;

: REGIONS! ( report n -- report ) {: v:n :}  v F-REGIONS ! ;
: REGIONS@ ( report -- n )  RPT-DROP F-REGIONS @ ;

: MATERIALIZED! ( report n -- report ) {: v:n :}  v F-MATERIALIZED ! ;
: MATERIALIZED@ ( report -- n )  RPT-DROP F-MATERIALIZED @ ;

\ ---- memory-plan bytes (known flag: unknown until a real estimate is set) ---
: BYTES! ( report n n -- report )          \ before after (marks known)
   {: before:n after:n :}
   before F-BYTES-BEFORE !  after F-BYTES-AFTER !  1 F-BYTES-KNOWN ! ;
: BYTES-KNOWN? ( report -- bool )  RPT-DROP F-BYTES-KNOWN @ 0= 0= ;
: BYTES-BEFORE@ ( report -- n )  RPT-DROP F-BYTES-BEFORE @ ;
: BYTES-AFTER@  ( report -- n )  RPT-DROP F-BYTES-AFTER @ ;

\ ---- roofline class --------------------------------------------------------
: ROOFLINE! ( report n -- report ) {: rc:n :}   \ RC-* code; >ROOFLINE fail-closes
   rc >ROOFLINE F-ROOFLINE-AT ! ;
: ROOFLINE@ ( report -- n )  RPT-DROP F-ROOFLINE-AT @ ROOFLINE>N ;

\ ---- split reasons ---------------------------------------------------------
: SPLIT+ ( report ptr u8 n -- report )
   N-SPLIT @ RPT-LCAP >= if E-RPT-FULL throw then
   ARENA-PUT {: off:n len:n :}
   off L-SPLIT 2 N-SPLIT @ 0 SL-SLOT !
   len L-SPLIT 2 N-SPLIT @ 1 SL-SLOT !
   N-SPLIT @ 1+ N-SPLIT ! ;
: SPLIT-COUNT ( report -- n )  RPT-DROP N-SPLIT @ ;
: SPLIT@ ( report n -- ptr u8 n ) {: idx:n :}
   RPT-DROP
   idx 0 < idx N-SPLIT @ >= or if E-RPT-IDX throw then
   L-SPLIT 2 idx 0 SL-SLOT @  L-SPLIT 2 idx 1 SL-SLOT @ ARENA-GET ;

\ ---- warnings --------------------------------------------------------------
: WARN+ ( report ptr u8 n -- report )
   N-WARN @ RPT-LCAP >= if E-RPT-FULL throw then
   ARENA-PUT {: off:n len:n :}
   off L-WARN 2 N-WARN @ 0 SL-SLOT !
   len L-WARN 2 N-WARN @ 1 SL-SLOT !
   N-WARN @ 1+ N-WARN ! ;
: WARN-COUNT ( report -- n )  RPT-DROP N-WARN @ ;
: WARN@ ( report n -- ptr u8 n ) {: idx:n :}
   RPT-DROP
   idx 0 < idx N-WARN @ >= or if E-RPT-IDX throw then
   L-WARN 2 idx 0 SL-SLOT @  L-WARN 2 idx 1 SL-SLOT @ ARENA-GET ;

\ ---- schedule candidates + selection ---------------------------------------
: CAND+ ( report ptr u8 n -- report )
   N-CAND @ RPT-CAND-CAP >= if E-RPT-FULL throw then
   ARENA-PUT {: off:n len:n :}
   off L-CAND 2 N-CAND @ 0 SL-SLOT !
   len L-CAND 2 N-CAND @ 1 SL-SLOT !
   N-CAND @ 1+ N-CAND ! ;
: CAND-COUNT ( report -- n )  RPT-DROP N-CAND @ ;
: CAND@ ( report n -- ptr u8 n ) {: idx:n :}
   RPT-DROP
   idx 0 < idx N-CAND @ >= or if E-RPT-IDX throw then
   L-CAND 2 idx 0 SL-SLOT @  L-CAND 2 idx 1 SL-SLOT @ ARENA-GET ;

: SELECT! ( report n -- report ) {: sel:n :}   \ -1 = none, else a candidate index
   sel -1 <> if
      sel 0 < sel N-CAND @ >= or if E-RPT-IDX throw then
   then
   sel F-SELECT ! ;
: SELECT@ ( report -- n )  RPT-DROP F-SELECT @ ;

\ ---- hot tensors + coalescing status ---------------------------------------
: HOT+ ( report ptr u8 n n -- report )     \ name status
   {: st:n :}
   st CO-CK drop                           \ reject a bad status before interning
   N-HOT @ RPT-LCAP >= if E-RPT-FULL throw then
   ARENA-PUT {: off:n len:n :}
   off L-HOT 3 N-HOT @ 0 SL-SLOT !
   len L-HOT 3 N-HOT @ 1 SL-SLOT !
   st >COSTATUS N-HOT @ HOT-ST-AT !
   N-HOT @ 1+ N-HOT ! ;
: HOT-COUNT ( report -- n )  RPT-DROP N-HOT @ ;
: HOT-NAME@ ( report n -- ptr u8 n ) {: idx:n :}
   RPT-DROP
   idx 0 < idx N-HOT @ >= or if E-RPT-IDX throw then
   L-HOT 3 idx 0 SL-SLOT @  L-HOT 3 idx 1 SL-SLOT @ ARENA-GET ;
: HOT-STATUS@ ( report n -- n ) {: idx:n :}
   RPT-DROP
   idx 0 < idx N-HOT @ >= or if E-RPT-IDX throw then
   idx HOT-ST-AT @ COSTATUS>N ;

\ ---- profile rows ----------------------------------------------------------
: PROF+ ( report ptr u8 n n -- report )    \ kernel device-us
   {: us:n :}
   N-PROF @ RPT-LCAP >= if E-RPT-FULL throw then
   ARENA-PUT {: off:n len:n :}
   off L-PROF 3 N-PROF @ 0 SL-SLOT !
   len L-PROF 3 N-PROF @ 1 SL-SLOT !
   us  L-PROF 3 N-PROF @ 2 SL-SLOT !
   N-PROF @ 1+ N-PROF ! ;
: PROF-COUNT ( report -- n )  RPT-DROP N-PROF @ ;
: PROF-NAME@ ( report n -- ptr u8 n ) {: idx:n :}
   RPT-DROP
   idx 0 < idx N-PROF @ >= or if E-RPT-IDX throw then
   L-PROF 3 idx 0 SL-SLOT @  L-PROF 3 idx 1 SL-SLOT @ ARENA-GET ;
: PROF-US@ ( report n -- n ) {: idx:n :}
   RPT-DROP
   idx 0 < idx N-PROF @ >= or if E-RPT-IDX throw then
   L-PROF 3 idx 2 SL-SLOT @ ;

\ ---- gate verdicts ---------------------------------------------------------
: GATE! ( report ptr u8 n n n -- report )  \ reason verdict-tag gate-id
   {: tag:n gid:n :}
   gid 0 < gid MAKI:G-N >= or if E-RPT-GATE throw then
   tag V-CK drop                           \ reject a bad tag before interning
   ARENA-PUT {: off:n len:n :}
   tag >VERDICT gid G-TAG-AT !
   off gid cells G-RO + !
   len gid cells G-RL + ! ;
: GATE-TAG@ ( report n -- n ) {: gid:n :}
   RPT-DROP
   gid 0 < gid MAKI:G-N >= or if E-RPT-GATE throw then
   gid G-TAG-AT @ VERDICT>N ;
: GATE-REASON@ ( report n -- ptr u8 n ) {: gid:n :}
   RPT-DROP
   gid 0 < gid MAKI:G-N >= or if E-RPT-GATE throw then
   gid cells G-RO + @  gid cells G-RL + @ ARENA-GET ;

\ ---- renders ---------------------------------------------------------------
\ Machine render: line-oriented "key: value" an agent parses by splitting on the
\ first ": " -- no prose scraping. Human render: a compact readable digest.
: RENDER ( report -- ptr u8 n )
   RPT-DROP  OUT-RESET
   R-KEYS R-FUSION R-BYTES R-COALESCE R-SCHEDULE R-GATES R-PROFILE
   R-ROOFLINE R-CACHE R-WARN
   OUT$ ;

: RENDER-HUMAN ( report -- ptr u8 n )
   RPT-DROP  OUT-RESET
   s" Model CAD report" OUT+ OUT-NL
   s"   model:     " OUT+ K-MODEL$ EMIT-STR OUT-NL
   s"   target:    " OUT+ K-TARGET$ EMIT-STR OUT-NL
   s"   fusion:    " OUT+ F-OPS-BEFORE @ OUT-INT s"  -> " OUT+ F-OPS-AFTER @ OUT-INT
      s"  ops, " OUT+ F-REGIONS @ OUT-INT s"  regions" OUT+ OUT-NL
   s"   roofline:  " OUT+ F-ROOFLINE-AT @ RC-NAME OUT+ OUT-NL
   s"   certify:   " OUT+ MAKI:G-CERTIFY   H-GATE
   s"   golden:    " OUT+ MAKI:G-GOLDEN    H-GATE
   s"   gradcheck: " OUT+ MAKI:G-GRADCHECK H-GATE
   s"   profile:   " OUT+ MAKI:G-PROFILE   H-GATE
   H-WARN
   OUT$ ;

\ Packet render: one repair-packet-discipline line per gate that did not pass, so an
\ agent (or EXPLAIN) can read failure class / reason / repair family / repro.
: RENDER-PACKETS ( report -- ptr u8 n )
   RPT-DROP  OUT-RESET
   s" packet.certify"   MAKI:G-CERTIFY   P-GATE
   s" packet.golden"    MAKI:G-GOLDEN    P-GATE
   s" packet.gradcheck" MAKI:G-GRADCHECK P-GATE
   s" packet.profile"   MAKI:G-PROFILE   P-GATE
   OUT$ ;

end-package
