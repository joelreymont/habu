# Fixing macOS JIT Restrictions

## The Problem

Modern macOS (especially Apple Silicon) uses a "hardened runtime" that blocks executable memory allocation for security reasons. This prevents Habu from executing compiled machine code directly.

**Error you'll see:**
```
Permission denied (errno 13) when calling mmap with PROT_EXEC
```

## Reality Check

**On modern macOS (11+ / Apple Silicon):**
- Ad-hoc code signing with entitlements is NOT sufficient
- Even with JIT entitlements properly applied, mmap with PROT_EXEC fails
- macOS requires either:
  1. Paid Apple Developer account + proper signing + notarization
  2. Disabling SIP (System Integrity Protection) - NOT recommended
  3. Using an older macOS version (pre-11)

**Tested and confirmed:**
- ✅ Entitlements are correctly applied (verified with codesign)
- ❌ mmap still fails with Permission denied (errno 13)
- ❌ Ad-hoc signing insufficient on modern macOS

## Recommended Solutions

### Option 1: Test on Linux (EASIEST - Works Immediately) ⭐

Linux has no restrictions on executable memory allocation. Just run the tests:

```bash
# On Linux (Ubuntu/Debian)
sudo apt install sbcl
cd /path/to/habu
sbcl --script bootstrap/test-executor.lisp
# Expected: All 20/20 tests pass! ✅
```

**Advantages:**
- No security restrictions
- No code signing needed
- Works immediately
- Free and easy

**Easy ways to get Linux:**
- Use a Linux VM (VirtualBox, VMware, UTM)
- Use Docker: `docker run -it --rm -v $(pwd):/habu ubuntu:latest`
- Use WSL2 on Windows
- Use a cloud instance (AWS, DigitalOcean, etc.)

### Option 2: Apple Developer Account + Proper Signing

If you have a paid Apple Developer account ($99/year):

1. Sign with your developer certificate
2. Notarize the binary
3. Entitlements will work properly

This is how commercial apps (Chrome, Firefox, etc.) enable JIT.

### Option 3: Disable SIP (Not Recommended)

You can disable macOS System Integrity Protection, but this significantly reduces system security:

1. Reboot into Recovery Mode (⌘R during boot)
2. Open Terminal from Utilities menu
3. Run: `csrutil disable`
4. Reboot

**We don't recommend this approach.**

## Verification

After applying the fix, verify it works:

```bash
# Test simple execution
sbcl --script bootstrap/test-executor.lisp

# You should see:
# [32m✓[0m ADD-2-3
# [32m✓[0m MUL-6-7
# ... (20 tests total)
# [32mAll execution tests passed![0m
```

## Technical Details

The entitlements file (`jit-entitlements.plist`) grants:

- `com.apple.security.cs.allow-jit` - Allow JIT compilation
- `com.apple.security.cs.allow-unsigned-executable-memory` - Allow mmap with PROT_EXEC
- `com.apple.security.cs.disable-library-validation` - Allow loading unsigned code

These are the same entitlements used by JavaScript engines (V8, JavaScriptCore) and other JIT compilers.

## Why This Is Safe

Code signing with these entitlements is safe because:

1. It only affects SBCL, not your entire system
2. SBCL is a trusted application (you installed it)
3. The entitlements are standard for any JIT compiler
4. You can verify the signature at any time with `codesign -d`

Many development tools require these same entitlements:
- Node.js (V8 engine)
- Safari (JavaScriptCore)
- Python (PyPy JIT)
- Ruby (YJIT)
- Java (HotSpot JIT)

## Troubleshooting

**"command not found: codesign"**
- codesign is included with Xcode Command Line Tools
- Install with: `xcode-select --install`

**"SBCL not found"**
- Install SBCL: `brew install sbcl`
- Or download from: https://www.sbcl.org/

**"Operation not permitted"**
- Make sure you have admin rights
- Try with `sudo`: `sudo ./fix-macos-jit.sh`

**Still getting permission denied**
- Verify signature: `codesign -d --entitlements - $(which sbcl)`
- You should see the JIT entitlements listed
- If not, the signing failed - try manual signing

## References

- Apple Code Signing Guide: https://developer.apple.com/documentation/security/code_signing_services
- Hardened Runtime: https://developer.apple.com/documentation/security/hardened_runtime
- JIT Entitlements: https://developer.apple.com/documentation/bundleresources/entitlements/com_apple_security_cs_allow-jit
