# Profile Habu Binary

Profile a running Habu binary using macOS `sample` tool to identify hot functions.

## Usage

`/habu-profile <binary> [duration]`

- `<binary>`: Path to the Habu executable (must have embedded symbols)
- `[duration]`: Sampling duration in seconds (default: 2)

## Instructions

1. Run the binary in background:
   ```bash
   $ARGUMENTS &
   PID=$!
   ```

2. Sample the process:
   ```bash
   sample $PID -f /tmp/habu-profile.txt
   ```

3. Parse the profile results:
   - Extract top addresses from "Sort by top of stack"
   - Map addresses to function names using `nm $BINARY | sort`
   - Calculate offset: sample_addr - load_address

4. Report:
   - List top 10 hottest functions with sample counts
   - Show percentage of total samples
   - Include function addresses for follow-up disassembly

## Example Output

```
Profile Results for /tmp/stage1-read (2s sample):

Top Hot Functions:
1. READ-SYM         2297 samples (26.3%)  0x874ac
2. SYMBOL-CHAR?      784 samples ( 9.0%)  0x8ed3c
3. FIND-INTERNED     652 samples ( 7.5%)  0x8f478
4. STRING=           450 samples ( 5.2%)  0x8eef4
...

Load address: 0x10473c000
Total samples: 8731

Suggestions:
- Functions with >10% are inlining candidates
- Use /habu-disasm to inspect hot code
```

## Notes

- Binary must include symbols (built with deliver-v3)
- The sample tool requires the process to run long enough to profile
- Kill the background process after sampling with `kill $PID`
