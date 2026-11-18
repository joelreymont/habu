split-str
\ engine-error.f - the dictionary occurrence and OS-backed memory codes.

\ Dictionary occurrence failures belong to DEF-OCC.
package DEF-OCC
public
-7231 constant E-STALE
-7232 constant E-SELECT
-7234 constant E-EXHAUSTED
;package

\ OS-backed memory: -3200..-3299
-3200 constant E-MEM-FIRST
-3299 constant E-MEM-LAST
-3200 constant E-MEM-SIZE
-3201 constant E-MEM-MAP
-3203 constant E-MEM-UNMAP
-3204 constant E-ALLOC-STATE           \ an allocator filled twice
