\ app-end.f - a stripped program's entry, which tools/hb-build-core.f loads
\ after the program's source. The entry is the program's global MAIN, whose
\ effect APP-ENTRY:XT! holds to ( -- ): the checker refuses this file at MAIN
\ when there is none or it takes or leaves cells (test/wasm/hb-build.f cells.f).
' MAIN APP-ENTRY:XT!
