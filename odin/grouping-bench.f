\ grouping-bench.f - head-to-head: linear-scan frame-index grouping vs the
\ lib/hashmap.f O(1) probe, on GB-N distinct keys (all new groups - the worst
\ case for the linear scan). Time GB-LRUN and GB-HRUN in separate processes.
\ Names are GB- prefixed to avoid the engine's redefinition guard.
\ Load: require lib/errors.f, lib/string.f, lib/hashmap.f, odin/grouping-bench.f
\ Re-run: time each of
\   printf '%s\n' 'require lib/errors.f' 'require lib/string.f' \
\      'require lib/hashmap.f' 'require odin/grouping-bench.f' 'GB-LRUN' | ../habu/bin/hb
\   printf '%s\n' 'require lib/errors.f' 'require lib/string.f' \
\      'require lib/hashmap.f' 'require odin/grouping-bench.f' 'GB-HRUN' | ../habu/bin/hb

16000 constant GB-N
32768 constant GB-HCAP        \ pow2 > GB-N

\ --- linear grouping (the old TG-FIND behavior) ---
create GB-LK GB-N cells allot
variable GB-LN  variable GB-LFI  variable GB-LSI  variable GB-BI
: GB-LFIND ( n -- n ) {: k:n :}
   -1 GB-LFI !  0 GB-LSI !
   begin GB-LSI @ GB-LN @ < GB-LFI @ 0 < and while
      GB-LK GB-LSI @ cells + @ k = if GB-LSI @ GB-LFI ! then
      GB-LSI @ 1+ GB-LSI !
   repeat
   GB-LFI @ ;
: GB-LADD ( n -- ) {: k:n :}
   k GB-LFIND 0 < if k GB-LK GB-LN @ cells + !  GB-LN @ 1+ GB-LN ! then ;
: GB-LRUN ( -- )
   0 GB-LN !  0 GB-BI !
   begin GB-BI @ GB-N < while  GB-BI @ GB-LADD  GB-BI @ 1+ GB-BI !  repeat
   cr ." linear groups: " GB-LN @ . cr ;

\ --- hashmap grouping (lib/hashmap.f probe) ---
create GB-HK GB-HCAP cells allot
create GB-HU GB-HCAP cells allot
variable GB-HN
: GB-HADD ( n -- ) {: k:n :}
   GB-HK GB-HU GB-HCAP k HM-PROBE {: s:n :}
   GB-HU s cells + @ 0= if k GB-HK s cells + !  -1 GB-HU s cells + !  GB-HN @ 1+ GB-HN ! then ;
: GB-HRUN ( -- )
   GB-HU GB-HCAP HM-CLEAR  0 GB-HN !  0 GB-BI !
   begin GB-BI @ GB-N < while  GB-BI @ GB-HADD  GB-BI @ 1+ GB-BI !  repeat
   cr ." hash groups: " GB-HN @ . cr ;
