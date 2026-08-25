---
title: "Infra: upgrade Orin (zed) to JetPack 7.2 / CUDA 13"
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T11:39:46.487953+02:00"
---

Device facts (verified 2026-07-04): ssh zed = NVIDIA Jetson Orin NX Engineering Reference Developer Kit (NOT a ZED Box chassis; /proc/device-tree/model), JetPack 6.2.1+b38, L4T r36.4.4, CUDA 12.6, Ubuntu 22.04, root on NVMe (937G, 9% used). Goal: JetPack 7.2 / L4T r38.x / CUDA 13 (Ubuntu 24.04 base). Paths researched: (a) Stereolabs zedbox scripts top out at JP6.2 and their JP7.2 support is in development (kernel modules/drivers) - also they target ZED Box hardware, not this devkit; only relevant if a ZED GMSL camera rig is attached (CHECK before upgrade: lsusb/dmesg for ZED devices; if the box drives ZED X cameras, JP7.2 ZED drivers are NOT ready = defer). (b) NVIDIA image-based OTA: cross-major payloads exist historically (r35.x->r36.3 documented); whether r38.x ota_tools accept BASE_BSP r36.4 must be verified in the r38.2 Developer Guide 'Updating Jetson Linux with Image-Based Over-the-Air Update'; payload GENERATION runs on an x86_64 host (feasible in an x86_64 container under emulation if no native host), APPLICATION runs on-device (remotely executable). (c) Recovery-mode flash (sdkmanager/initrd flash) from a physical x86 host: definitive fallback, NOT remotely executable. Also verify Orin NX (not just AGX) is in the JP7.2 support matrix + devkit NVMe boot config supported. Execution protocol (SUPERVISED): Phase 1 read-only - verify support matrix, OTA feasibility r36->r38, camera-rig check, full backup inventory (what beyond ~/Work needs saving; habu repo is pushed), disk/QSPI bootloader version prerequisites (UEFI capsule update ordering!), then STOP with a go/no-go report. Phase 2 (only after orchestrator approval) - execute the chosen path with rollback notes; expect ssh loss during reboot; verify post-upgrade: nvcc 13, ptxas sm_87 still supported (CUDA 13 drops sm_87?? VERIFY - Orin is sm_87; CUDA 13 support for Orin implies yes, but confirm ptxas -arch=sm_87 works), then rebuild bin/hb (fixpoint), full native gate + maki 61 + device tools on the new stack. Risk: failed cross-major OTA on a headless box needs physical recovery; A/B rootfs slot status to be checked in phase 1.

PHASE 1 VERDICT 2026-07-04: GO technically, DEFER operationally.
TECHNICAL PATH (validated, remotely executable): NVIDIA r39.2 Developer Guide
image-based OTA explicitly supports base "35.5.0, 35.6.x, 36.3.0, 36.4.x,
36.5.0" for ALL Orin NX modules -> we are 36.4.4, in-list. JetPack 7.2 = L4T
r39.2, CUDA 13.2.1 (ships FOR Orin => sm_87 retained), Ubuntu 24.04. NVMe
rootfs handled via l4t_generate_ota_package.sh --external-device nvme0n1 -S
<size>. Payload generation on an x86_64 host: Docker exists on the Mac (daemon
currently stopped; run under --platform linux/amd64 emulation), ~80G workspace
needed, Mac has 920G free. Application runs ON DEVICE (nv_ota_start.sh +
reboots). Device prerequisites verified: Orin NX 16GB p3767-0000, passwordless
sudo, bootloader capsule 36.4.4 slot A normal (2 slots), 820G free on NVMe.
BLOCKER (defer): /sys/class/video4linux shows zedx 9-0010/9-0011/10-0010/
10-0011 = TWO ZED X GMSL stereo cameras on Stereolabs JP6 kernel drivers.
Stereolabs JP7.2 kernel modules are NOT released (their forum, June 2026:
"more time to develop the new kernel modules"; upgrade script promised for
their docs page). Upgrading now orphans the camera rig. WATCH:
https://community.stereolabs.com/t/upgrade-zed-box-mini-to-jetpack-7-2/11394
and the Stereolabs docs reset-update page for the JP7.2 zedx driver release.
PRE-UPGRADE CHECKLIST (whenever unblocked): push/bundle zed-wip-cuda-driver
(a0f31639, local-only); back up ~/Work (35G: habu, Odin, odin-habu, more) -
rsync to Mac; then payload-gen in amd64 container, scp, nv_ota_start.sh,
2 reboots, verify r39.2 + ptxas sm_87 + zedx drivers, rebuild bin/hb fixpoint,
full native gate + maki + device tools. Recovery if bricked: physical x86 host
+ USB recovery-mode reflash (user is local to the device).

DECISION 2026-07-04 (user): DEFER confirmed. No upgrade until Stereolabs ships
JP7.2 zedx drivers. Watch the forum thread + Stereolabs docs; when the driver
release appears, re-run the pre-upgrade checklist above and request user
approval for Phase 2.
