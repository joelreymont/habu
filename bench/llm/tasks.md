# Benchmark tasks

Each task is a prompt for a model: the **name**, the declared **`( in -- out )`**,
a one-line spec, and the **tests**. Give the model everything except the body;
take its `: NAME ( sig ) … ;`, verify with `CHECK!`, then run the tests. Record
the metrics in `PROTOCOL.md`. Reference solutions are in `solutions.f`.

| # | Name | Signature | Spec | Tests |
|---|------|-----------|------|-------|
| 1 | `SQUARE` | `( i64 -- i64 )` | square of n | `7 → 49`, `-3 → 9` |
| 2 | `CUBE` | `( i64 -- i64 )` | n cubed | `3 → 27` |
| 3 | `ABSV` | `( i64 -- i64 )` | absolute value | `-5 → 5`, `4 → 4` |
| 4 | `NEG?` | `( i64 -- bool )` | true iff n < 0 | `-5 → -1`, `5 → 0` |
| 5 | `CLAMP0` | `( i64 -- i64 )` | max(n, 0) | `-7 → 0`, `4 → 4` |
| 6 | `SUM3` | `( i64 i64 i64 -- i64 )` | a + b + c | `1 2 3 → 6` |
| 7 | `AVG2` | `( i64 i64 -- i64 )` | (a + b) / 2 (use locals) | `10 20 → 15` |
| 8 | `MAX2` | `( i64 i64 -- i64 )` | the larger of a, b (use locals) | `3 9 → 9`, `9 3 → 9` |
| 9 | `SWAP2` | `( a b -- b a )` | exchange the top two (polymorphic) | `1 2 → 2 1` |
| 10 | `ROT3` | `( a b c -- b c a )` | rotate three (polymorphic) | `1 2 3 → 2 3 1` |
| 11 | `SUMTO` | `( i64 -- i64 )` | 1 + 2 + … + n | `5 → 15`, `0 → 0` |
| 12 | `FACT` | `( i64 -- i64 )` | n! | `5 → 120`, `0 → 1` |
| 13 | `KEEP1` | `( i64 -- i64 )` | n*10 + n, keeping n on the **return stack** | `5 → 55` |
| 14 | `TWICE` | `( i64 -- i64 )` | apply a quotation that doubles | `21 → 42` |
| 15 | `APPLY` | `( i64 [ i64 -- i64 ] -- i64 )` | apply the **quotation parameter** to n | `5 [: dup * ;] → 25` |
| 16 | `MIN2` | `( i64 i64 -- i64 )` | smaller of a, b (locals) | `3 9 → 3`, `9 3 → 3` |
| 17 | `SIGNUM` | `( i64 -- i64 )` | sign of n (-1/0/1), **nested if/else** | `-4 → -1`, `0 → 0`, `7 → 1` |
| 18 | `2DUP2` | `( a b -- a b a b )` | duplicate the top pair (polymorphic) | `1 2 → 1 2 1 2` |
| 19 | `POW` | `( i64 i64 -- i64 )` | b raised to e (loop) | `2 3 → 8`, `5 0 → 1` |
| 20 | `COUNTDOWN` | `( i64 -- i64 )` | count up to n with a loop (= n) | `5 → 5` |
| 21 | `DIP` | `( R x [ R -- S ] -- S x )` | **combinator**: run the quot under the top item | `1 2 [: 10 + ;] → 11 2` |
| 22 | `KEEP` | `( x [ x -- a ] -- a x )` | **combinator**: run the quot on x, keep x | `5 [: dup * ;] → 25 5` |
| 23 | `BI` | `( x [ x -- a ] [ x -- b ] -- a b )` | **combinator**: apply two quots to x | `4 [: 1+ ;] [: dup * ;] → 5 16` |

For a fuller eval (toward 30–50), add: a small tokenizer/parser, a
return-stack-heavy traversal, `tri`/`cleave`, and a **deliberately
underspecified** task where the model must write the test first.
