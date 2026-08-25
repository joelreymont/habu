---
title: Run the address-site recorder in the relocation gate
status: open
priority: 2
issue-type: task
created-at: "2026-07-30T11:59:15.112841+02:00"
---

test/compiler/reloc-cases.f builds the address-literal map band itself, setting the bit for each recorded chain slot by hand, exactly the way the call rows set the call-map bits. That leaves one thing unchecked: the bit index SNAP-RELOC:EMIT-ADDR-SITE writes when a chain is compiled is never compared against the bit index SNAP-RELOC:EMIT-ADDRS reads when the chain is relocated. An off-by-one shared between the recorder and the relocator would pass the gate today, even though the whole point of recording at emit time is that the site cannot be off by an instruction. Fix: decode and run EMIT-ADDR-SITE in package RELOC-VM the way EMIT-ADDRS is already run, with CP pointing at a chain in the fixture's region and DBASE at its base, and have the chain rows build their map through that instead of through the fixture's own AMAP-BIT. The machine needs two mnemonics it does not have yet, LSLV (variable shift left) and STRB (store byte), and the CP and DBASE register aliases as symbols. Falsify by skewing the recorder's shift (the map byte index is offset >> 5 and the bit number is (offset >> 2) & 7): the gate must red. The same gap exists for the call map and EMIT-CEMITBL and can be closed in the same pass.
