const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const use_hoist = b.option(bool, "use-hoist", "Enable Hoist JIT backend") orelse true;

    const build_opts = b.addOptions();
    build_opts.addOption(bool, "use_hoist", use_hoist);

    // Main executable
    const exe = b.addExecutable(.{
        .name = "habu",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/main.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    exe.root_module.addOptions("build_options", build_opts);

    // Hoist SSA compiler backend (default-on; can be disabled with -Duse-hoist=false)
    var hoist_mod: ?*std.Build.Module = null;
    if (use_hoist) {
        const hoist_dep = b.dependency("hoist", .{
            .target = target,
            .optimize = optimize,
        });
        const m = hoist_dep.artifact("cranelift").root_module;
        hoist_mod = m;
        exe.root_module.addImport("hoist", m);
    }

    // Link Z3 for SMT solving (refinement types)
    exe.root_module.addSystemIncludePath(.{ .cwd_relative = "/opt/homebrew/include" });
    exe.root_module.addLibraryPath(.{ .cwd_relative = "/opt/homebrew/lib" });
    exe.root_module.linkSystemLibrary("z3", .{});
    exe.root_module.linkSystemLibrary("c", .{});

    b.installArtifact(exe);

    // Run command
    const run_cmd = b.addRunArtifact(exe);
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }
    const run_step = b.step("run", "Run the Habu REPL");
    run_step.dependOn(&run_cmd.step);

    // Tests
    const test_filter = b.option([]const u8, "test-filter", "Only compile/run tests matching this filter");
    const test_filters: []const []const u8 = if (test_filter) |f| &.{f} else &.{};
    const lib_tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/lib.zig"),
            .target = target,
            .optimize = optimize,
        }),
        .filters = test_filters,
    });
    lib_tests.root_module.addOptions("build_options", build_opts);

    // Add ohsnap for snapshot testing
    if (b.lazyDependency("ohsnap", .{
        .target = target,
        .optimize = optimize,
    })) |ohsnap_dep| {
        lib_tests.root_module.addImport("ohsnap", ohsnap_dep.module("ohsnap"));
    }

    // Hoist SSA compiler backend for tests
    if (hoist_mod) |m| {
        lib_tests.root_module.addImport("hoist", m);
    }

    // Link Z3 for SMT solving (refinement types)
    // Z3 is optional - tests that don't use it will still work
    lib_tests.root_module.addSystemIncludePath(.{ .cwd_relative = "/opt/homebrew/include" });
    lib_tests.root_module.addLibraryPath(.{ .cwd_relative = "/opt/homebrew/lib" });
    lib_tests.root_module.linkSystemLibrary("z3", .{});
    lib_tests.root_module.linkSystemLibrary("c", .{});

    const test_step = b.step("test", "Run unit tests");
    const test_run = b.addRunArtifact(lib_tests);
    test_step.dependOn(&test_run.step);

    // Error masking check
    const check_errors = b.addSystemCommand(&.{
        "sh",
        "-c",
        "if grep -rn 'catch return null\\|catch return;\\|orelse unreachable\\|catch |_|' src --exclude-dir=tests --exclude='*.md' | grep -v test.zig | grep -v lineedit.zig | grep -v vector.zig; then echo 'ERROR: Found error masking patterns in src/' && exit 1; fi",
    });
    const check_step = b.step("check-errors", "Check for error masking patterns");
    check_step.dependOn(&check_errors.step);
    test_step.dependOn(&check_errors.step);

    // GC Benchmark
    const gc_bench = b.addExecutable(.{
        .name = "gc_bench",
        .root_module = b.createModule(.{
            .root_source_file = b.path("bench/gc.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    gc_bench.root_module.addOptions("build_options", build_opts);
    gc_bench.root_module.addImport("runtime", b.createModule(.{
        .root_source_file = b.path("src/runtime/runtime.zig"),
        .target = target,
        .optimize = optimize,
    }));

    b.installArtifact(gc_bench);

    const bench_run_cmd = b.addRunArtifact(gc_bench);
    if (b.args) |args| {
        bench_run_cmd.addArgs(args);
    }
    const bench_step = b.step("bench", "Run GC benchmark (bench/gc.zig --help)");
    bench_step.dependOn(&bench_run_cmd.step);

    // VM microbench
    const vm_bench = b.addExecutable(.{
        .name = "vm_bench",
        .root_module = b.createModule(.{
            .root_source_file = b.path("bench/vm.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    const habu_bench_mod = b.createModule(.{
        .root_source_file = b.path("src/lib.zig"),
        .target = target,
        .optimize = optimize,
    });
    habu_bench_mod.addOptions("build_options", build_opts);
    if (hoist_mod) |m| {
        habu_bench_mod.addImport("hoist", m);
    }
    vm_bench.root_module.addImport("habu", habu_bench_mod);
    vm_bench.root_module.addOptions("build_options", build_opts);
    // Z3 is required by src/types/smt.zig (imported via src/lib.zig).
    vm_bench.root_module.addSystemIncludePath(.{ .cwd_relative = "/opt/homebrew/include" });
    vm_bench.root_module.addLibraryPath(.{ .cwd_relative = "/opt/homebrew/lib" });
    vm_bench.root_module.linkSystemLibrary("z3", .{});
    vm_bench.root_module.linkSystemLibrary("c", .{});

    b.installArtifact(vm_bench);

    const vm_bench_run_cmd = b.addRunArtifact(vm_bench);
    if (b.args) |args| {
        vm_bench_run_cmd.addArgs(args);
    }
    const vm_bench_step = b.step("bench-vm", "Run VM microbench");
    vm_bench_step.dependOn(&vm_bench_run_cmd.step);

    // JIT microbench
    const jit_bench = b.addExecutable(.{
        .name = "jit_bench",
        .root_module = b.createModule(.{
            .root_source_file = b.path("bench/jit.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    jit_bench.root_module.addImport("habu", habu_bench_mod);
    jit_bench.root_module.addOptions("build_options", build_opts);
    // Z3 is required by src/types/smt.zig (imported via src/lib.zig).
    jit_bench.root_module.addSystemIncludePath(.{ .cwd_relative = "/opt/homebrew/include" });
    jit_bench.root_module.addLibraryPath(.{ .cwd_relative = "/opt/homebrew/lib" });
    jit_bench.root_module.linkSystemLibrary("z3", .{});
    jit_bench.root_module.linkSystemLibrary("c", .{});

    b.installArtifact(jit_bench);

    const jit_bench_run_cmd = b.addRunArtifact(jit_bench);
    if (b.args) |args| {
        jit_bench_run_cmd.addArgs(args);
    }
    const jit_bench_step = b.step("bench-jit", "Run JIT microbench");
    jit_bench_step.dependOn(&jit_bench_run_cmd.step);

    // CL comparison bench (Habu vs SBCL)
    const cl_bench = b.addExecutable(.{
        .name = "cl_bench",
        .root_module = b.createModule(.{
            .root_source_file = b.path("bench/cl_bench.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    cl_bench.root_module.addImport("habu", habu_bench_mod);
    cl_bench.root_module.addOptions("build_options", build_opts);
    cl_bench.root_module.addSystemIncludePath(.{ .cwd_relative = "/opt/homebrew/include" });
    cl_bench.root_module.addLibraryPath(.{ .cwd_relative = "/opt/homebrew/lib" });
    cl_bench.root_module.linkSystemLibrary("z3", .{});
    cl_bench.root_module.linkSystemLibrary("c", .{});

    b.installArtifact(cl_bench);

    const cl_bench_run_cmd = b.addRunArtifact(cl_bench);
    if (b.args) |args| {
        cl_bench_run_cmd.addArgs(args);
    }
    const cl_bench_step = b.step("bench-cl", "Run CL comparison bench");
    cl_bench_step.dependOn(&cl_bench_run_cmd.step);

    // Comprehensive benchmark (Habu vs SBCL)
    const comp_bench = b.addExecutable(.{
        .name = "comprehensive_bench",
        .root_module = b.createModule(.{
            .root_source_file = b.path("bench/comprehensive_bench.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    comp_bench.root_module.addImport("habu", habu_bench_mod);
    comp_bench.root_module.addOptions("build_options", build_opts);
    comp_bench.root_module.addSystemIncludePath(.{ .cwd_relative = "/opt/homebrew/include" });
    comp_bench.root_module.addLibraryPath(.{ .cwd_relative = "/opt/homebrew/lib" });
    comp_bench.root_module.linkSystemLibrary("z3", .{});
    comp_bench.root_module.linkSystemLibrary("c", .{});

    b.installArtifact(comp_bench);

    const comp_bench_run_cmd = b.addRunArtifact(comp_bench);
    if (b.args) |args| {
        comp_bench_run_cmd.addArgs(args);
    }
    const comp_bench_step = b.step("bench-comp", "Run comprehensive benchmark");
    comp_bench_step.dependOn(&comp_bench_run_cmd.step);

    // Maxima workload benchmark (real-world CAS paths)
    const maxima_bench = b.addExecutable(.{
        .name = "maxima_workload_bench",
        .root_module = b.createModule(.{
            .root_source_file = b.path("bench/maxima_workload.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    maxima_bench.root_module.addImport("habu", habu_bench_mod);
    maxima_bench.root_module.addOptions("build_options", build_opts);
    maxima_bench.root_module.addSystemIncludePath(.{ .cwd_relative = "/opt/homebrew/include" });
    maxima_bench.root_module.addLibraryPath(.{ .cwd_relative = "/opt/homebrew/lib" });
    maxima_bench.root_module.linkSystemLibrary("z3", .{});
    maxima_bench.root_module.linkSystemLibrary("c", .{});

    b.installArtifact(maxima_bench);

    const maxima_bench_run_cmd = b.addRunArtifact(maxima_bench);
    if (b.args) |args| {
        maxima_bench_run_cmd.addArgs(args);
    }
    const maxima_bench_step = b.step("bench-maxima", "Run Maxima workload benchmark");
    maxima_bench_step.dependOn(&maxima_bench_run_cmd.step);

    // Bench regression checks (runs gc_bench/vm_bench/jit_bench)
    const bench_check = b.addExecutable(.{
        .name = "bench_check",
        .root_module = b.createModule(.{
            .root_source_file = b.path("bench/check.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    bench_check.root_module.addOptions("build_options", build_opts);
    b.installArtifact(bench_check);

    const bench_check_run_cmd = b.addRunArtifact(bench_check);
    bench_check_run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        bench_check_run_cmd.addArgs(args);
    }

    const bench_check_step = b.step("bench-check", "Run bench regression checks");
    bench_check_step.dependOn(&bench_check_run_cmd.step);

    // GC parity comparison (Habu vs SBCL)
    const gc_parity_cmd = b.addSystemCommand(&.{
        "tools/gc-compare",
        "--json",
        "--iters=20",
        "--live-mb=8",
        "--heap-mb=64",
        "--gate-level=milestone_2x_from_baseline",
    });
    if (b.args) |args| {
        gc_parity_cmd.addArgs(args);
    }
    const gc_parity_step = b.step("gc-parity", "Run GC parity comparison against SBCL");
    gc_parity_step.dependOn(&gc_parity_cmd.step);
}
