/* watcher2: run a command, follow its process tree via EVFILT_PROC
 * (NOTE_FORK/NOTE_EXEC/NOTE_EXIT; NOTE_TRACK is unsupported on macOS).
 * On each fork event, scan the forker's children and register new pids,
 * logging pid, ppid, image path and argv. Periodic sweeps catch stragglers.
 *
 * usage: watcher2 LOGFILE CMD [ARGS...]
 */
#include <sys/event.h>
#include <sys/sysctl.h>
#include <sys/wait.h>
#include <errno.h>
#include <libproc.h>
#include <signal.h>
#include <spawn.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <mach/mach_time.h>

extern char **environ;

#define MAXW 262144
static int kq;
static FILE *logf;
static uint64_t t0;
static mach_timebase_info_data_t tb;
static char watched[MAXW];   /* indexed by pid */
static long alive = 0;
static long lost = 0;

static double now_ms(void) {
    uint64_t t = mach_absolute_time() - t0;
    return (double)t * tb.numer / tb.denom / 1e6;
}

static void get_argv(pid_t pid, char *out, size_t cap) {
    static char buf[65536];
    size_t len = sizeof buf;
    int mib[3] = {CTL_KERN, KERN_PROCARGS2, pid};
    out[0] = 0;
    if (sysctl(mib, 3, buf, &len, NULL, 0) < 0) { snprintf(out, cap, "?argv-errno=%d", errno); return; }
    if (len < sizeof(int)) return;
    int argc = *(int *)buf;
    char *p = buf + sizeof(int), *end = buf + len;
    while (p < end && *p) p++;            /* skip exec path */
    while (p < end && !*p) p++;           /* skip padding */
    size_t o = 0;
    for (int i = 0; i < argc && p < end && o + 1 < cap; i++) {
        while (p < end && o + 1 < cap && *p) out[o++] = *p++;
        p++;
        if (i + 1 < argc && o + 1 < cap) out[o++] = ' ';
    }
    out[o] = 0;
}

static void logproc(const char *tag, pid_t pid, pid_t ppid) {
    char path[PROC_PIDPATHINFO_MAXSIZE];
    char args[512];
    if (proc_pidpath(pid, path, sizeof path) <= 0)
        snprintf(path, sizeof path, "?path-errno=%d", errno);
    get_argv(pid, args, sizeof args);
    fprintf(logf, "%.1f %s %d %d %s | %s\n", now_ms(), tag, (int)pid, (int)ppid, path, args);
}

static void scan_children(pid_t parent);

static void watch(pid_t pid, pid_t ppid, const char *tag) {
    if (pid <= 0 || pid >= MAXW || watched[pid]) return;
    struct kevent kev;
    EV_SET(&kev, pid, EVFILT_PROC, EV_ADD | EV_ENABLE,
           NOTE_EXEC | NOTE_FORK | NOTE_EXIT, 0, NULL);
    if (kevent(kq, &kev, 1, NULL, 0, NULL) < 0) {
        lost++;
        fprintf(logf, "%.1f LOST %d %d - | -\n", now_ms(), (int)pid, (int)ppid);
        return;
    }
    watched[pid] = 1;
    alive++;
    logproc(tag, pid, ppid);
    scan_children(pid);   /* it may have forked before we attached */
}

static void scan_children(pid_t parent) {
    static pid_t pids[4096];
    int n = proc_listpids(PROC_PPID_ONLY, (uint32_t)parent, pids, sizeof pids);
    if (n <= 0) return;
    n /= sizeof(pid_t);
    for (int i = 0; i < n; i++)
        if (pids[i] > 0) watch(pids[i], parent, "CHILD");
}

static void sweep(void) {
    for (pid_t p = 1; p < MAXW; p++)
        if (watched[p]) scan_children(p);
}

int main(int argc, char **argv) {
    if (argc < 3) { fprintf(stderr, "usage: %s LOGFILE CMD [ARGS...]\n", argv[0]); return 2; }
    mach_timebase_info(&tb);
    t0 = mach_absolute_time();
    logf = fopen(argv[1], "w");
    if (!logf) { perror("fopen"); return 2; }
    setvbuf(logf, NULL, _IOLBF, 0);

    kq = kqueue();
    if (kq < 0) { perror("kqueue"); return 2; }

    posix_spawnattr_t attr;
    posix_spawnattr_init(&attr);
    posix_spawnattr_setflags(&attr, POSIX_SPAWN_START_SUSPENDED);
    pid_t root;
    int rc = posix_spawn(&root, argv[2], NULL, &attr, &argv[2], environ);
    if (rc != 0) { fprintf(stderr, "spawn: %s\n", strerror(rc)); return 2; }
    watch(root, getpid(), "ROOT");
    kill(root, SIGCONT);

    int root_exited = 0, root_status = -1;
    struct timespec tick = {0, 200000000};   /* 200ms sweep cadence */
    double last_sweep = 0;
    int drain_rounds = 0;

    while (1) {
        struct kevent ev;
        int n = kevent(kq, NULL, 0, &ev, 1, &tick);
        if (n < 0) { if (errno == EINTR) continue; perror("kevent"); break; }
        if (n == 0) {
            sweep();
            last_sweep = now_ms();
            if (root_exited && (alive <= 0 || ++drain_rounds > 50)) break;
            continue;
        }
        pid_t pid = (pid_t)ev.ident;
        if (ev.fflags & NOTE_FORK) {
            fprintf(logf, "%.1f FORK %d 0 - | -\n", now_ms(), (int)pid);
            scan_children(pid);
        }
        if (ev.fflags & NOTE_EXEC)
            logproc("EXEC", pid, 0);
        if (ev.fflags & NOTE_EXIT) {
            if (pid < MAXW && watched[pid]) { watched[pid] = 0; alive--; }
            fprintf(logf, "%.1f EXIT %d %d - | -\n", now_ms(), (int)pid, (int)((ev.data >> 8) & 0xff));
            if (pid == root) { waitpid(root, &root_status, 0); root_exited = 1; }
            if (root_exited && alive <= 0) break;
        }
        if (now_ms() - last_sweep > 500) { sweep(); last_sweep = now_ms(); }
    }
    fprintf(logf, "%.1f DONE alive=%ld lost=%ld status=%d\n", now_ms(), alive, lost, root_status);
    fclose(logf);
    if (root_status >= 0 && WIFEXITED(root_status)) return WEXITSTATUS(root_status);
    return 1;
}
