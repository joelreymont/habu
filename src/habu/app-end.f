\ app-end.f - a stripped program's entry, loaded after the program's source.
\ The entry is the program's global MAIN, whose effect APP-ENTRY:XT! holds to
\ ( -- ): the checker refuses this file at MAIN when there is none or it takes
\ or leaves cells.
' MAIN APP-ENTRY:XT!
