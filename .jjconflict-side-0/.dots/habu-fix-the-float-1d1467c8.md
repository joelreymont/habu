---
title: "Fix the float literal reader's silent overflow"
status: open
priority: 2
issue-type: task
created-at: "2026-08-02T21:40:57.425195+02:00"
---

src/habu/habu1.f C-NUM-FLOAT-FINISH reads a float literal as int + frac/10^k, accumulating both the fraction digits and the scale in signed 64-bit cells. Past eighteen fractional digits both wrap: bin/hb reads 0.1234567890123456789 as the cell -4628938082669329042 - a NEGATIVE number - instead of the double 0.1234567890123456789 (4593560419847042655), and reads 0.12345678901234567890123 as 23.99. There is no diagnostic; the wrong value is simply compiled. Two repairs are needed: refuse a literal the reader cannot represent (the tape's INT-VALUE already refuses what the stdlib reader declines, and this is the same discipline for reals), and record what the correctly rounded answer would be. A second, smaller fact from the same code: the value is computed with two SCVTFs, an FDIV and an FADD, so up to three roundings, and a literal is not always the nearest double to the decimal it spells - 1.9482199351819093 reads one ulp below its nearest double (4611452821746767930 against 4611452821746767931), as does 0.11471049746507529. The survey at the head of tools/codegen-compare-corpus3.f records both facts with their probes; the float corpus avoids them by pinning only short exactly-representable literals, and a float compiler that materialises constants has to match whatever this reader is fixed to do.
