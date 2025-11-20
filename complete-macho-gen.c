/* Complete Mach-O Generator for ARM64 - Following bootstrap/SBCL model
 * Includes all required load commands for modern macOS code signing
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <time.h>

/* Mach-O constants */
#define MH_MAGIC_64    0xfeedfacf
#define CPU_TYPE_ARM64 0x0100000c
#define CPU_SUBTYPE_ARM64_ALL 0
#define MH_EXECUTE     2
#define MH_NOUNDEFS    0x00000001
#define MH_DYLDLINK    0x00000004
#define MH_TWOLEVEL    0x00000080
#define MH_PIE         0x00200000

#define LC_SEGMENT_64       0x19
#define LC_MAIN             0x28
#define LC_LOAD_DYLINKER    0x0E
#define LC_LOAD_DYLIB       0x0C
#define LC_SYMTAB           0x02
#define LC_DYSYMTAB         0x0B
#define LC_UUID             0x1B
#define LC_BUILD_VERSION    0x32
#define LC_SOURCE_VERSION   0x2A

#define VM_PROT_READ    1
#define VM_PROT_WRITE   2
#define VM_PROT_EXECUTE 4

static void write_u32(FILE *f, uint32_t val) {
    fwrite(&val, 4, 1, f);
}

static void write_u64(FILE *f, uint64_t val) {
    fwrite(&val, 8, 1, f);
}

static void write_string(FILE *f, const char *str, size_t max_len) {
    size_t len = strlen(str) + 1;  /* include null */
    fwrite(str, 1, len, f);
    /* Pad to max_len */
    for (size_t i = len; i < max_len; i++) {
        fputc(0, f);
    }
}

static void generate_macho(const unsigned char *code, size_t code_len, const char *output) {
    FILE *out = fopen(output, "wb");
    if (!out) {
        perror("fopen");
        exit(1);
    }

    /* Calculate sizes (matching bootstrap) */
    uint32_t header_size = 32;
    uint32_t pagezero_cmd_size = 72;
    uint32_t segment_cmd_size = 72;
    uint32_t section_size = 80;
    uint32_t main_cmd_size = 24;
    uint32_t dylinker_cmd_size = 32;  /* "/usr/lib/dyld" + padding */
    uint32_t dylib_cmd_size = 56;     /* "/usr/lib/libSystem.B.dylib" + padding */
    uint32_t symtab_cmd_size = 24;
    uint32_t dysymtab_cmd_size = 80;
    uint32_t uuid_cmd_size = 24;
    uint32_t build_version_cmd_size = 32;
    uint32_t source_version_cmd_size = 16;

    uint32_t load_cmds_size = pagezero_cmd_size + segment_cmd_size + section_size +
                              dylinker_cmd_size + dylib_cmd_size +
                              symtab_cmd_size + dysymtab_cmd_size +
                              uuid_cmd_size + main_cmd_size +
                              build_version_cmd_size + source_version_cmd_size;

    uint32_t code_offset = 0x4000;  /* 16K page boundary */
    uint64_t vm_addr = 0x100001000;

    /* Write mach_header_64 */
    write_u32(out, MH_MAGIC_64);
    write_u32(out, CPU_TYPE_ARM64);
    write_u32(out, CPU_SUBTYPE_ARM64_ALL);
    write_u32(out, MH_EXECUTE);
    write_u32(out, 10);  /* ncmds */
    write_u32(out, load_cmds_size);
    write_u32(out, MH_NOUNDEFS | MH_DYLDLINK | MH_TWOLEVEL | MH_PIE);
    write_u32(out, 0);  /* reserved */

    /* LC_SEGMENT_64 - __PAGEZERO (null pointer guard, REQUIRED for code signing!) */
    write_u32(out, LC_SEGMENT_64);
    write_u32(out, 72);
    write_string(out, "__PAGEZERO", 16);
    write_u64(out, 0);           /* vmaddr */
    write_u64(out, 0x100000000); /* vmsize = 4GB */
    write_u64(out, 0);           /* fileoff */
    write_u64(out, 0);           /* filesize */
    write_u32(out, 0);           /* maxprot */
    write_u32(out, 0);           /* initprot */
    write_u32(out, 0);           /* nsects */
    write_u32(out, 0);           /* flags */

    /* LC_SEGMENT_64 - __TEXT with __text section */
    write_u32(out, LC_SEGMENT_64);
    write_u32(out, segment_cmd_size + section_size);
    write_string(out, "__TEXT", 16);
    write_u64(out, 0x100000000);     /* vmaddr */
    write_u64(out, 0x4000);          /* vmsize */
    write_u64(out, 0);               /* fileoff */
    write_u64(out, code_offset + code_len);  /* filesize */
    write_u32(out, VM_PROT_READ | VM_PROT_EXECUTE);  /* maxprot */
    write_u32(out, VM_PROT_READ | VM_PROT_EXECUTE);  /* initprot */
    write_u32(out, 1);               /* nsects */
    write_u32(out, 0);               /* flags */

    /* section_64 - __text */
    write_string(out, "__text", 16);
    write_string(out, "__TEXT", 16);
    write_u64(out, vm_addr);         /* addr */
    write_u64(out, code_len);        /* size */
    write_u32(out, code_offset);     /* offset */
    write_u32(out, 2);               /* align (2^2 = 4) */
    write_u32(out, 0);               /* reloff */
    write_u32(out, 0);               /* nreloc */
    write_u32(out, 0x80000400);      /* flags: PURE_INSTRUCTIONS | SOME_INSTRUCTIONS */
    write_u32(out, 0);               /* reserved1 */
    write_u32(out, 0);               /* reserved2 */
    write_u32(out, 0);               /* reserved3 */

    /* LC_LOAD_DYLINKER */
    write_u32(out, LC_LOAD_DYLINKER);
    write_u32(out, dylinker_cmd_size);
    write_u32(out, 12);              /* offset to string */
    write_string(out, "/usr/lib/dyld", dylinker_cmd_size - 12);

    /* LC_LOAD_DYLIB - libSystem */
    write_u32(out, LC_LOAD_DYLIB);
    write_u32(out, dylib_cmd_size);
    write_u32(out, 24);              /* offset to string */
    write_u32(out, 2);               /* timestamp */
    write_u32(out, 0x00010001);      /* current version */
    write_u32(out, 0x00010001);      /* compat version */
    write_string(out, "/usr/lib/libSystem.B.dylib", dylib_cmd_size - 24);

    /* LC_SYMTAB (empty but required) */
    write_u32(out, LC_SYMTAB);
    write_u32(out, 24);
    write_u32(out, 0);  /* symoff */
    write_u32(out, 0);  /* nsyms */
    write_u32(out, 0);  /* stroff */
    write_u32(out, 0);  /* strsize */

    /* LC_DYSYMTAB (empty but required) */
    write_u32(out, LC_DYSYMTAB);
    write_u32(out, 80);
    for (int i = 0; i < 18; i++) {
        write_u32(out, 0);
    }

    /* LC_UUID (required for code signing) */
    write_u32(out, LC_UUID);
    write_u32(out, 24);
    /* Generate simple UUID from timestamp */
    time_t now = time(NULL);
    for (int i = 0; i < 16; i++) {
        fputc((now + i) % 256, out);
    }

    /* LC_MAIN (modern entry point) */
    write_u32(out, LC_MAIN);
    write_u32(out, 24);
    write_u64(out, code_offset);  /* entryoff */
    write_u64(out, 0);            /* stacksize */

    /* LC_BUILD_VERSION (required for modern macOS) */
    write_u32(out, LC_BUILD_VERSION);
    write_u32(out, 32);
    write_u32(out, 1);           /* platform (1 = macOS) */
    write_u32(out, 0x000B0000);  /* minos (11.0.0) */
    write_u32(out, 0x000F0000);  /* sdk (15.0.0) */
    write_u32(out, 1);           /* ntools */
    write_u32(out, 3);           /* tool (3 = ld) */
    write_u32(out, 0x03570000);  /* version */

    /* LC_SOURCE_VERSION */
    write_u32(out, LC_SOURCE_VERSION);
    write_u32(out, 16);
    write_u64(out, 0);

    /* Pad to code offset */
    long pos = ftell(out);
    for (long i = pos; i < code_offset; i++) {
        fputc(0, out);
    }

    /* Write code */
    fwrite(code, 1, code_len, out);

    fclose(out);
}

int main(int argc, char **argv) {
    if (argc != 3) {
        fprintf(stderr, "Usage: %s '<bytes>' <output>\n", argv[0]);
        return 1;
    }

    /* Parse bytes */
    unsigned char code[4096];
    int code_len = 0;
    char *str = strdup(argv[1]);
    char *token = strtok(str, " ,");
    while (token && code_len < 4096) {
        code[code_len++] = (unsigned char)atoi(token);
        token = strtok(NULL, " ,");
    }
    free(str);

    if (code_len == 0) {
        fprintf(stderr, "Error: No bytes parsed\n");
        return 1;
    }

    printf("Generating Mach-O executable with %d bytes of code\n", code_len);

    /* Generate Mach-O */
    generate_macho(code, code_len, argv[2]);

    /* Make executable */
    char cmd[1024];
    snprintf(cmd, sizeof(cmd), "chmod +x %s", argv[2]);
    system(cmd);

    /* Add ad-hoc code signature (like SBCL/bootstrap does) */
    printf("Adding ad-hoc code signature...\n");
    snprintf(cmd, sizeof(cmd), "/usr/bin/codesign -s - -f %s 2>&1", argv[2]);
    int result = system(cmd);

    if (result == 0) {
        printf("✓ Successfully created and signed: %s\n", argv[2]);
    } else {
        fprintf(stderr, "Warning: Code signing returned %d\n", result);
    }

    return 0;
}
