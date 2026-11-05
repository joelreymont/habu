\ Composition of two native x86-64 windows through the owned artifact reader.
SUITE aot-native-merge
   test/aot-native-merge.f
;SUITE

\ The fixture opens an ARM AOT source window and selects tier 0 before asking
\ how an x64 shadow is linked from that ARM window.
SUITE x86-64-link-records
   test/x86-64-link-records.f
;SUITE
