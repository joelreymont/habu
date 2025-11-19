# Apple Developer Notarization for JIT Execution

## Overview

To enable JIT execution (mmap with PROT_EXEC) on modern macOS, you need a paid Apple Developer account ($99/year) and must notarize your binary.

## Prerequisites

1. **Apple Developer Account** ($99/year)
   - Sign up at: https://developer.apple.com/programs/
   - Wait for account approval (usually 24-48 hours)

2. **Xcode Command Line Tools**
   ```bash
   xcode-select --install
   ```

3. **App-Specific Password** (for notarization)
   - Go to: https://appleid.apple.com/account/manage
   - Sign in with your Apple ID
   - In "Sign-In and Security" section, click "App-Specific Passwords"
   - Click "+" to generate a new password
   - Label it "Notarization" and save the generated password

## Step 1: Get Your Developer Certificate

### Option A: Via Xcode (Easiest)

1. Open Xcode
2. Go to **Xcode → Settings → Accounts**
3. Click **+** to add your Apple ID
4. Select your account → click **Manage Certificates**
5. Click **+** → Choose **Developer ID Application**
6. Certificate will be automatically created and installed

### Option B: Via Command Line

```bash
# List available signing identities
security find-identity -v -p codesigning

# You should see something like:
# 1) XXXXXX "Developer ID Application: Your Name (TEAMID)"
```

If you don't see a Developer ID certificate:

```bash
# Request certificate
sudo security add-certificates /path/to/certificate.cer
```

## Step 2: Create Entitlements File

Already created: `jit-entitlements.plist`

```xml
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <key>com.apple.security.cs.allow-jit</key>
    <true/>
    <key>com.apple.security.cs.allow-unsigned-executable-memory</key>
    <true/>
    <key>com.apple.security.cs.disable-library-validation</key>
    <true/>
</dict>
</plist>
```

## Step 3: Sign SBCL with Developer Certificate

```bash
# Find your Developer ID
DEVELOPER_ID=$(security find-identity -v -p codesigning | grep "Developer ID Application" | head -1 | awk -F'"' '{print $2}')

echo "Using identity: $DEVELOPER_ID"

# Copy SBCL binary
cp /opt/homebrew/Cellar/sbcl/2.5.9/libexec/bin/sbcl bin/sbcl-signed

# Sign with Developer ID and entitlements
codesign --sign "$DEVELOPER_ID" \
    --entitlements jit-entitlements.plist \
    --options runtime \
    --force \
    --deep \
    --timestamp \
    bin/sbcl-signed

# Verify signature
codesign -dv --verbose=4 bin/sbcl-signed
```

**Important flags:**
- `--options runtime` - Enable hardened runtime (required for notarization)
- `--timestamp` - Use Apple's timestamp server
- `--deep` - Sign nested code
- `--force` - Replace existing signature

## Step 4: Create Bundle for Notarization

Apple requires binaries to be in a bundle for notarization:

```bash
# Create app bundle structure
mkdir -p Habu.app/Contents/MacOS
mkdir -p Habu.app/Contents/Resources

# Copy signed SBCL
cp bin/sbcl-signed Habu.app/Contents/MacOS/sbcl

# Create Info.plist
cat > Habu.app/Contents/Info.plist <<'EOF'
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <key>CFBundleExecutable</key>
    <string>sbcl</string>
    <key>CFBundleIdentifier</key>
    <string>com.yourname.habu.sbcl</string>
    <key>CFBundleName</key>
    <string>Habu SBCL</string>
    <key>CFBundlePackageType</key>
    <string>APPL</string>
    <key>CFBundleShortVersionString</key>
    <string>1.0</string>
    <key>CFBundleVersion</key>
    <string>1</string>
</dict>
</plist>
EOF

# Sign the bundle
codesign --sign "$DEVELOPER_ID" \
    --entitlements jit-entitlements.plist \
    --options runtime \
    --force \
    --deep \
    --timestamp \
    Habu.app

# Verify bundle signature
codesign -dv --verbose=4 Habu.app
```

## Step 5: Create ZIP for Notarization

```bash
# Create ZIP (required for notarization)
ditto -c -k --keepParent Habu.app Habu.zip
```

## Step 6: Notarize the Bundle

```bash
# Store credentials (one time)
xcrun notarytool store-credentials "habu-notarize" \
    --apple-id "your-apple-id@example.com" \
    --team-id "YOUR_TEAM_ID" \
    --password "xxxx-xxxx-xxxx-xxxx"  # App-specific password from Step 0

# Submit for notarization
xcrun notarytool submit Habu.zip \
    --keychain-profile "habu-notarize" \
    --wait

# This will take 1-5 minutes
# Output will show: "status: Accepted" on success
```

**Finding your Team ID:**
```bash
# List teams
xcrun notarytool history --keychain-profile "habu-notarize"

# Or check Apple Developer portal
# https://developer.apple.com/account/
```

## Step 7: Staple the Notarization

```bash
# Staple notarization ticket to app
xcrun stapler staple Habu.app

# Verify stapling
xcrun stapler validate Habu.app

# Should show: "The validate action worked!"
```

## Step 8: Test JIT Execution

```bash
# Use the notarized SBCL
Habu.app/Contents/MacOS/sbcl --script test-mmap.lisp

# Expected output:
# Testing mmap with PROT_EXEC...
# PROT flags: 7
# MAP flags: 4098
# Success! Allocated executable memory at: XXXXXXXXX
# Success! Freed memory.
```

## Step 9: Create Convenience Script

```bash
cat > bin/sbcl-notarized <<'EOF'
#!/bin/bash
SBCL_HOME="/opt/homebrew/Cellar/sbcl/2.5.9/lib/sbcl" \
exec "$(dirname "$0")/../Habu.app/Contents/MacOS/sbcl" "$@"
EOF

chmod +x bin/sbcl-notarized

# Test it
./bin/sbcl-notarized --script bootstrap/test-executor.lisp
```

## Troubleshooting

### "The binary is not signed with a valid Developer ID"

Check your certificate:
```bash
security find-identity -v -p codesigning
```

You should see "Developer ID Application" - if not, create one in Xcode.

### "Invalid entitlements"

Make sure entitlements file is valid XML:
```bash
plutil -lint jit-entitlements.plist
```

### "Notarization failed - rejected"

Check the logs:
```bash
xcrun notarytool log <submission-id> --keychain-profile "habu-notarize"
```

Common issues:
- Missing `--options runtime` flag
- Missing timestamp
- Bundle ID mismatch
- Unsigned nested code

### "Still getting Permission denied"

Verify all steps:
```bash
# 1. Check signature
codesign -dv --verbose=4 Habu.app

# 2. Check entitlements
codesign -d --entitlements - Habu.app

# 3. Check notarization
spctl -a -vv Habu.app

# 4. Check stapling
xcrun stapler validate Habu.app
```

All should pass without errors.

## Cost Analysis

**One-time costs:**
- Apple Developer Account: $99/year
- Time to set up: ~1 hour

**Per-release costs:**
- Signing: Free (automated)
- Notarization: Free (via Apple)
- Time: ~5 minutes per release

## Alternative: Use Older macOS

If you have access to an older Mac (macOS 10.14 or earlier), JIT works without notarization:

```bash
# On older macOS
sbcl --script bootstrap/test-executor.lisp
# Just works! ✅
```

## Alternative: Test on Linux

Much simpler - no signing, no notarization:

```bash
# On any Linux system
sudo apt install sbcl
sbcl --script bootstrap/test-executor.lisp
# Just works! ✅
```

## Summary

**To enable JIT on modern macOS:**

1. Pay $99 for Apple Developer account
2. Create Developer ID certificate (via Xcode)
3. Generate app-specific password
4. Sign SBCL with certificate + entitlements
5. Create app bundle with Info.plist
6. Submit to Apple for notarization (1-5 min wait)
7. Staple notarization ticket
8. Use notarized SBCL for all development

**Or:**
- Test on Linux (no restrictions, free)
- Use older macOS (no restrictions)
- Pay the $99 if you need macOS support

## Useful Links

- Apple Developer: https://developer.apple.com/
- Code Signing Guide: https://developer.apple.com/documentation/security/code_signing_services
- Notarization Guide: https://developer.apple.com/documentation/security/notarizing_macos_software_before_distribution
- Entitlements Reference: https://developer.apple.com/documentation/bundleresources/entitlements
- Team ID Lookup: https://developer.apple.com/account/

---

**Status:** Complete notarization guide for Apple Developer account
**Cost:** $99/year + ~1 hour initial setup
**Benefit:** JIT execution on modern macOS
