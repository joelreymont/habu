/* Convert byte list to ARM64 Mach-O executable
 * Usage: ./bytes-to-executable <bytes> <output>
 * Example: ./bytes-to-executable "192 3 95 214" test.out
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

/* Minimal Mach-O header for ARM64 */
typedef struct {
    uint32_t magic;      /* mach magic number identifier */
    uint32_t cputype;    /* cpu specifier */
    uint32_t cpusubtype; /* machine specifier */
    uint32_t filetype;   /* type of file */
    uint32_t ncmds;      /* number of load commands */
    uint32_t sizeofcmds; /* size of all the load commands */
    uint32_t flags;      /* flags */
    uint32_t reserved;   /* reserved (64-bit only) */
} mach_header_64;

/* Load command header */
typedef struct {
    uint32_t cmd;        /* type of load command */
    uint32_t cmdsize;    /* total size of command in bytes */
} load_command;

/* Segment command (64-bit) */
typedef struct {
    uint32_t cmd;        /* LC_SEGMENT_64 */
    uint32_t cmdsize;    /* includes sizeof section_64 structs */
    char segname[16];    /* segment name */
    uint64_t vmaddr;     /* memory address */
    uint64_t vmsize;     /* memory size */
    uint64_t fileoff;    /* file offset */
    uint64_t filesize;   /* file size */
    int32_t maxprot;     /* maximum VM protection */
    int32_t initprot;    /* initial VM protection */
    uint32_t nsects;     /* number of sections */
    uint32_t flags;      /* flags */
} segment_command_64;

/* Section (64-bit) */
typedef struct {
    char sectname[16];   /* name of this section */
    char segname[16];    /* segment this section goes in */
    uint64_t addr;       /* memory address */
    uint64_t size;       /* size in bytes */
    uint32_t offset;     /* file offset */
    uint32_t align;      /* section alignment (power of 2) */
    uint32_t reloff;     /* file offset of relocation entries */
    uint32_t nreloc;     /* number of relocation entries */
    uint32_t flags;      /* flags */
    uint32_t reserved1;  /* reserved */
    uint32_t reserved2;  /* reserved */
    uint32_t reserved3;  /* reserved (64-bit only) */
} section_64;

/* Entry point command */
typedef struct {
    uint32_t cmd;        /* LC_MAIN */
    uint32_t cmdsize;    /* 24 */
    uint64_t entryoff;   /* file offset of main() */
    uint64_t stacksize;  /* initial stack size */
} entry_point_command;

#define MH_MAGIC_64    0xfeedfacf
#define CPU_TYPE_ARM64 0x0100000c
#define CPU_SUBTYPE_ARM64_ALL 0
#define MH_EXECUTE     2
#define MH_NOUNDEFS    1
#define MH_PIE         0x200000

#define LC_SEGMENT_64  0x19
#define LC_MAIN        0x28

#define VM_PROT_READ    1
#define VM_PROT_WRITE   2
#define VM_PROT_EXECUTE 4

#define PAGE_SIZE 0x4000

int main(int argc, char **argv) {
    if (argc != 3) {
        fprintf(stderr, "Usage: %s '<bytes>' <output>\n", argv[0]);
        fprintf(stderr, "Example: %s '192 3 95 214' test.out\n", argv[0]);
        return 1;
    }

    /* Parse byte string */
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

    printf("Parsed %d bytes of code\n", code_len);

    /* Create output file */
    FILE *out = fopen(argv[2], "wb");
    if (!out) {
        perror("fopen");
        return 1;
    }

    /* Calculate sizes */
    size_t header_size = sizeof(mach_header_64);
    size_t segment_size = sizeof(segment_command_64) + sizeof(section_64);
    size_t entry_size = sizeof(entry_point_command);
    size_t total_cmds = segment_size + entry_size;
    size_t code_offset = PAGE_SIZE;  /* Start code at page boundary */

    /* Write Mach-O header */
    mach_header_64 header = {
        .magic = MH_MAGIC_64,
        .cputype = CPU_TYPE_ARM64,
        .cpusubtype = CPU_SUBTYPE_ARM64_ALL,
        .filetype = MH_EXECUTE,
        .ncmds = 2,  /* segment + entry point */
        .sizeofcmds = total_cmds,
        .flags = MH_NOUNDEFS | MH_PIE,
        .reserved = 0
    };
    fwrite(&header, sizeof(header), 1, out);

    /* Write __TEXT segment with __text section */
    segment_command_64 text_seg = {
        .cmd = LC_SEGMENT_64,
        .cmdsize = sizeof(segment_command_64) + sizeof(section_64),
        .vmaddr = PAGE_SIZE,
        .vmsize = PAGE_SIZE,
        .fileoff = 0,
        .filesize = code_offset + code_len,
        .maxprot = VM_PROT_READ | VM_PROT_EXECUTE,
        .initprot = VM_PROT_READ | VM_PROT_EXECUTE,
        .nsects = 1,
        .flags = 0
    };
    strncpy(text_seg.segname, "__TEXT", 16);
    fwrite(&text_seg, sizeof(text_seg), 1, out);

    /* Write __text section */
    section_64 text_sect = {
        .addr = PAGE_SIZE,
        .size = code_len,
        .offset = code_offset,
        .align = 2,  /* 2^2 = 4 byte alignment */
        .reloff = 0,
        .nreloc = 0,
        .flags = 0x80000400,  /* S_ATTR_PURE_INSTRUCTIONS | S_ATTR_SOME_INSTRUCTIONS */
        .reserved1 = 0,
        .reserved2 = 0,
        .reserved3 = 0
    };
    strncpy(text_sect.sectname, "__text", 16);
    strncpy(text_sect.segname, "__TEXT", 16);
    fwrite(&text_sect, sizeof(text_sect), 1, out);

    /* Write LC_MAIN (entry point) */
    entry_point_command entry_cmd = {
        .cmd = LC_MAIN,
        .cmdsize = sizeof(entry_point_command),
        .entryoff = code_offset,
        .stacksize = 0
    };
    fwrite(&entry_cmd, sizeof(entry_cmd), 1, out);

    /* Pad to code_offset */
    long current_pos = ftell(out);
    for (long i = current_pos; i < code_offset; i++) {
        fputc(0, out);
    }

    /* Write code */
    fwrite(code, 1, code_len, out);

    fclose(out);

    /* Make executable */
    char chmod_cmd[1024];
    snprintf(chmod_cmd, sizeof(chmod_cmd), "chmod +x %s", argv[2]);
    system(chmod_cmd);

    printf("Created executable: %s\n", argv[2]);
    return 0;
}
