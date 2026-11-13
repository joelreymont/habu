\ The fixture opens an ARM AOT source window and selects tier 0 before asking
\ how an x64 shadow is linked from that ARM window.
false [if] \ off until dot 3ed6f45d
SUITE x86-64-link-records
   test/x86-64-link-records.f
;SUITE
[then]
