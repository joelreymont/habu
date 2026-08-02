\ kernel-manifest-test.f - focused tests for the habu-kernel-manifest renderer.

require lib/errors.f
require lib/string.f
require lib/float.f
require lib/fmt.f
require lib/memory.f
require lib/json-write.f
require lib/test.f
require src/arch/ptx/emit.f
require lib/ptx/kernel-abi.f
require lib/ptx/kernel-manifest.f

package KMAN-TEST

$1000 constant COPY-CAP
26 constant HASH-KEY-LEN               \ ,"manifest_content_hash":"
92 constant HASH-TAIL-LEN              \ key fragment + 64 hex + quote + brace

create COPY-BUF COPY-CAP allot
create KMT-DG 40 allot
create KMT-HEX 72 allot

variable COPY-U

: FIXED-PTX$ ( -- ptr u8 n )
   S\" .version 8.3\ntest-ptx\n" ;

: SAXPY-ABI! ( -- )
   KABI:RESET
   s" SAXPY" KABI:NAME!
   s" ceil-n-256" KABI:GRID!
   256 KABI:BLOCK!
   s" x" s" n" KABI:SPAN+
   s" y" s" n" KABI:SPAN+
   s" a" KABI:UNIFORM+ ;

: ROWS-ABI! ( -- )
   KABI:RESET
   s" SOFTMAX_ROWS" KABI:NAME!
   s" extent-r" KABI:GRID!
   256 KABI:BLOCK!
   s" in" s" r" s" c" KABI:MATRIX+
   s" out" s" r" s" c" KABI:MATRIX+ ;

: MANIFEST-COPY! ( -- )                \ render into COPY-BUF for cross-render compares
   FIXED-PTX$ KMAN:MANIFEST$ {: ma:ptr mu:n :}
   mu COPY-CAP > if E-STR-CAPACITY throw then
   ma COPY-BUF mu BYTE-COPY
   mu COPY-U ! ;

: COPY$ ( -- ptr u8 n )
   COPY-BUF COPY-U @ ;

\ manifest_content_hash contract: sha256 hex of the bytes STRICTLY BEFORE the
\ ,"manifest_content_hash":"..." suffix; the suffix shape itself is pinned.
: HASH-CONTRACT-CHECK ( ptr u8 n -- ) {: ma:ptr mu:n :}
   ma mu HASH-TAIL-LEN - + HASH-KEY-LEN  S\" ,\"manifest_content_hash\":\"" T$=
   ma mu 2 - + 2  S\" \"}" T$=
   ma mu HASH-TAIL-LEN - KMT-DG SHA256
   KMT-DG KMT-HEX SHA256>HEX
   KMT-HEX 64  ma mu 66 - + 64  T$= ;

: KMT-NO-NAME ( -- )
   KABI:RESET
   FIXED-PTX$ KMAN:MANIFEST$ 2drop ;

T-RESET

\ --- SAXPY manifest: pinned schema, per-kind lowering, dedup, slots, hashes ---
SAXPY-ABI!
MANIFEST-COPY!
COPY$ S\" {\"schema\":\"habu-kernel-manifest\",\"name\":\"SAXPY\"," CONTAINS? TTRUE
COPY$ S\" \"target\":\"sm_87\",\"ptx_version\":\"8.3\",\"address_size\":64," CONTAINS? TTRUE
COPY$ S\" \"block\":{\"x\":256,\"y\":1,\"z\":1}" CONTAINS? TTRUE
COPY$ S\" \"grid_derivation\":\"ceil-n-256\"" CONTAINS? TTRUE
COPY$ S\" \"param_bytes\":24" CONTAINS? TTRUE
COPY$ S\" \"name\":\"x\",\"kind\":\"span\",\"elem\":\"f32\",\"align\":0" CONTAINS? TTRUE
COPY$ S\" \"base\":{\"param\":\"p_x\",\"offset\":0,\"size\":8,\"ptx_type\":\".u64\"}" CONTAINS? TTRUE
COPY$ S\" \"len\":{\"param\":\"p_n\",\"offset\":20,\"size\":4,\"ptx_type\":\".u32\",\"dedup_key\":\"n\"}" CONTAINS? TTRUE
COPY$ S\" \"scalar\":{\"param\":\"p_a\",\"offset\":16,\"size\":4,\"ptx_type\":\".f32\"}" CONTAINS? TTRUE
COPY$ S\" \"param_slots\":[{\"param\":\"p_x\",\"offset\":0,\"size\":8,\"ptx_type\":\".u64\",\"role\":\"base\"}" CONTAINS? TTRUE
COPY$ S\" {\"param\":\"p_n\",\"offset\":20,\"size\":4,\"ptx_type\":\".u32\",\"role\":\"len\"}]" CONTAINS? TTRUE
COPY$ S\" \"ptx_sha256\":\"aa89f85505601af35e4a7a0f0d28919b0ea499d2a986c2f29cf9bb014364269a\"" CONTAINS? TTRUE
COPY$ HASH-CONTRACT-CHECK

\ determinism: a second render is byte-identical
FIXED-PTX$ KMAN:MANIFEST$ COPY$ T$=

\ a different PTX input changes ptx_sha256 (and therefore the manifest)
S\" .version 8.3\nother-ptx\n" KMAN:MANIFEST$ COPY$ STR= TFALSE

\ --- matrix lowering: cols param'd, rows launch-derived, stride dense ---
ROWS-ABI!
MANIFEST-COPY!
COPY$ S\" \"name\":\"SOFTMAX_ROWS\"" CONTAINS? TTRUE
COPY$ S\" \"grid_derivation\":\"extent-r\"" CONTAINS? TTRUE
COPY$ S\" \"param_bytes\":20" CONTAINS? TTRUE
COPY$ S\" \"kind\":\"matrix\"" CONTAINS? TTRUE
COPY$ S\" \"cols\":{\"param\":\"p_c\",\"offset\":16,\"size\":4,\"ptx_type\":\".u32\",\"dedup_key\":\"c\"}" CONTAINS? TTRUE
COPY$ S\" \"rows\":{\"source\":\"launch-derived\",\"from\":\"gridDim.x\",\"dedup_key\":\"r\"}" CONTAINS? TTRUE
COPY$ S\" \"stride\":{\"source\":\"dense-derived\",\"equals\":\"c\"}" CONTAINS? TTRUE
COPY$ HASH-CONTRACT-CHECK

\ --- negative: a record without a name cannot be a manifest ---
' KMT-NO-NAME E-KABI-TOKEN TTHROWS

\ leave the default scaffolding record installed for later shared-image tests
SAXPY-ABI!

T-REPORT

;package
