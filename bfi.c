#include <errno.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>

#include <sys/mman.h>

#define NEXT        0
#define PREV        1
#define INCR        2
#define DECR        3
#define PUTC        4
#define GETC        5
#define BEQZ        6
#define BNEZ        7

#define OP          29
#define ARG         0x1fffffff

#define MAX_OP      7
#define MAX_MEM     65536
#define MAX_ARG     0x1fffffff

typedef uint64_t cell_t;

/** Tokenizer & Parser **/

struct inst_t {
    int             rep;
    char            tok;
    struct prog_t * prog;
};

struct prog_t {
    struct inst_t * ins;
    size_t          len;
    size_t          cap;
};

static struct prog_t *prog_new() {
    struct prog_t *p = malloc(sizeof(struct prog_t));
    p->len = 0;
    p->cap = 16;
    p->ins = malloc(p->cap * sizeof(struct inst_t));
    return p;
}

static void prog_free(struct prog_t *p) {
    if (p != NULL) {
        for (size_t i = 0; i < p->len; i++) {
            prog_free(p->ins[i].prog);
        }
        free(p->ins);
        free(p);
    }
}

static void prog_append(struct prog_t *p, char tok, struct prog_t *prog) {
    size_t          pos = p->len;
    struct inst_t * ins = &p->ins[p->len - 1];

    /* merge adjacent instructions */
    if (pos && ins->tok == tok && ins->prog == NULL && ins->rep < MAX_ARG) {
        ins->rep++;
        return;
    }

    /* grow as needed */
    if (p->len >= p->cap) {
        p->cap *= 2;
        p->ins = realloc(p->ins, p->cap * sizeof(struct inst_t));
    }

    /* append new instruction */
    p->ins[pos].rep = 1;
    p->ins[pos].tok = tok;
    p->ins[pos].prog = prog;
    p->len++;
}

static int is_bf(char ch) {
    return (ch == '<') ||
           (ch == '>') ||
           (ch == '+') ||
           (ch == '-') ||
           (ch == '.') ||
           (ch == ',') ||
           (ch == '[') ||
           (ch == ']');
}

static char bf_lex(const char **p) {
    while (**p && !is_bf(**p)) ++*p;
    return *(*p)++;
}

static struct prog_t *bf_parse(const char *src, const char **p) {
    char            tok;
    struct prog_t * ret = prog_new();

    /* scan until EOF */
    for (;;) {
        switch ((tok = bf_lex(p))) {
            case 0: {
                return ret;
            }

            /* basic instructions */
            case '<':
            case '>':
            case '+':
            case '-':
            case '.':
            case ',': {
                prog_append(ret, tok, NULL);
                break;
            }

            /* loop begin */
            case '[': {
                struct prog_t *subp = bf_parse(src, p);
                prog_append(ret, '[', subp);

                /* the matching loop end */
                if (bf_lex(p) == ']') {
                    break;
                }

                /* report syntax error for mismatched loops */
                fprintf(stderr, "* syntax error: ']' expected at position %zd\n", *p - src);
                prog_free(ret);
                return NULL;
            }

            /* loop end */
            case ']': {
                --*p;
                return ret;
            }
        }
    }
}

/** IR Generator **/

struct code_t {
    uint32_t * buf;
    size_t     len;
    size_t     cap;
};

static void code_init(struct code_t *c) {
    c->len = 0;
    c->cap = 16;
    c->buf = malloc(c->cap * sizeof(uint32_t));
}

static void code_free(struct code_t *c) {
    free(c->buf);
}

static void code_append(struct code_t *c, uint32_t op, uint32_t arg) {
    if (op > MAX_OP) {
        fprintf(stderr, "* fatal: invalid opcode %d\n", op);
        abort();
    }
    if (arg > MAX_ARG) {
        fprintf(stderr, "* fatal: invalid argument %d\n", arg);
        abort();
    }
    if (c->len >= c->cap) {
        c->cap *= 2;
        c->buf = realloc(c->buf, c->cap * sizeof(uint32_t));
    }
    c->buf[c->len] = (op << OP) | arg;
    c->len++;
}

static void bf_gen_ir(struct code_t *c, struct prog_t *p) {
    for (size_t i = 0; i < p->len; i++) {
        struct inst_t * ins = &p->ins[i];
        size_t          rep = ins->rep;

        /* generate instructions */
        switch (ins->tok) {
            default: {
                fprintf(stderr, "* fatal: invalid token %c.\n", ins->tok);
                abort();
            }

            /* basic instructions */
            case '>': code_append(c, NEXT, ins->rep); break;
            case '<': code_append(c, PREV, ins->rep); break;
            case '+': code_append(c, INCR, ins->rep); break;
            case '-': code_append(c, DECR, ins->rep); break;
            case '.': code_append(c, PUTC, ins->rep); break;
            case ',': code_append(c, GETC, ins->rep); break;

            /* loops */
            case '[': {
                if (rep != 1 || ins->prog == NULL) {
                    fprintf(stderr, "* fatal: invalid loop start.\n");
                    abort();
                }

                /* loop begin */
                size_t pc = c->len;
                code_append(c, BEQZ, 0);
                bf_gen_ir(c, ins->prog);

                /* check for branch offset */
                if (c->len - pc >= MAX_ARG) {
                    fprintf(stderr, "* fatal: branch too far: %zd\n", c->len - pc);
                    abort();
                }

                /* loop end */
                code_append(c, BNEZ, (pc - c->len) & ARG);
                c->buf[pc] |= (c->len - pc - 1) & ARG;
                break;
            }
        }
    }
}

/** IR Disassembler **/

static int to_int(uint32_t v) {
    int val = v << 3;
    return val < 0 ? ~(~val >> 3) : val >> 3;
}

static void bf_dis(struct code_t *c) {
    for (size_t i = 0; i < c->len; i++) {
        uint32_t bc  = c->buf[i];
        uint32_t op  = bc >> OP;
        uint32_t arg = bc & ARG;

        /* disassemble every bytecode */
        switch (op) {
            case NEXT : printf("%08zx : %08x   next $%d\n", i, bc, arg); break;
            case PREV : printf("%08zx : %08x   prev $%d\n", i, bc, arg); break;
            case INCR : printf("%08zx : %08x   incr $%d\n", i, bc, arg); break;
            case DECR : printf("%08zx : %08x   decr $%d\n", i, bc, arg); break;
            case PUTC : printf("%08zx : %08x   putc $%d\n", i, bc, arg); break;
            case GETC : printf("%08zx : %08x   getc $%d\n", i, bc, arg); break;
            case BEQZ : printf("%08zx : %08x   beqz %08lx\n", i, bc, to_int(arg) + i + 1); break;
            case BNEZ : printf("%08zx : %08x   bnez %08lx\n", i, bc, to_int(arg) + i + 1); break;
            default   : fprintf(stderr, "* fatal: invalid opcode %d.\n", op); abort();
        }
    }
}

/** JIT Compiler **/

struct func_t {
    uint8_t * buf;
    size_t    len;
    size_t    cap;
};

static void func_init(struct func_t *fn) {
    fn->len = 0;
    fn->cap = 16;
    fn->buf = malloc(fn->cap);
}

static void func_exec(struct func_t *fn, void *mem) {
    size_t page  = sysconf(_SC_PAGESIZE);
    size_t size  = (((fn->len - 1) / page) + 1) * page;
    void * block = mmap(NULL, size, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, 0, 0);

    /* check for mapping result */
    if (!block) {
        fprintf(stderr, "* fatal: cannot allocate memory for JIT.\n");
        abort();
    }

    /* copy code into memory page */
    memcpy(block, fn->buf, fn->len);
    free(fn->buf);

    /* make it executable */
    if (mprotect(block, size, PROT_READ | PROT_EXEC) < 0) {
        fprintf(stderr, "* fatal: cannot load function into memory.");
        abort();
    }

    /* call the function, and free the memory block */
    ((void (*)(void *))(block))(mem);
    munmap(block, size);
}

static void func_emit(struct func_t *fn, const void *buf, size_t size) {
    if (fn->len + size > fn->cap) {
        while (fn->len + size > fn->cap) fn->cap *= 2;
        fn->buf = realloc(fn->buf, fn->cap);
    }
    memcpy(&fn->buf[fn->len], buf, size);
    fn->len += size;
}

#if __x86_64__
static void func_byte(struct func_t *fn, uint8_t ins) {
    func_emit(fn, &ins, 1);
}
#endif

static void func_long(struct func_t *fn, uint32_t ins) {
    func_emit(fn, &ins, 4);
}

static void func_ld64(struct func_t *fn, uint8_t reg, uint64_t imm) {
#if __x86_64__
    if (imm > UINT32_MAX) {
        func_byte(0x48 | ((reg >> 3) & 1));                             // movabsq  ...., ....
        func_byte(fn, 0xb8 | (reg & 3));                                //                %reg
        func_emit(fn, &imm, 8);                                         //          $imm
    } else if (reg >= 8) {
        func_byte(fn, 0x41);                                            // movl     ...., ....
        func_byte(fn, 0xb8 | (reg & 3));                                //                %reg
        func_long(imm);                                                 //          $imm
    } else {
        func_byte(fn, 0xb8 | (reg & 3));                                // movl     ...., %reg
        func_long(imm);                                                 //          $imm
    }
#elif __aarch64__
    for (int i = 0, k = 0, v; i < 4; i++) {
        if ((v = ((uint16_t *)&imm)[i]) != 0) {
            func_long(fn, 0xd2800000 | (k << 29) | (i << 21) | (v << 5) | (reg & 0x1f));
            k = 1;
        }
    }
#else
#error "Unsupported CPU architecture"
#endif
}

static void func_prologue(struct func_t *fn) {
#if __x86_64__
    func_emit(fn, "\x55", 1);                                       // pushq    %rbp
    func_emit(fn, "\x48\x89\xe5", 3);                               // movq     %rsp, %rbp
    func_emit(fn, "\x41\x54", 2);                                   // pushq    %r12
    func_emit(fn, "\x41\x55", 2);                                   // pushq    %r13
    func_emit(fn, "\x41\x56", 2);                                   // pushq    %r14
    func_emit(fn, "\x41\x57", 2);                                   // pushq    %r15
    func_ld64(fn, 12, (uint64_t)&putchar_unlocked);                 // movabsq  &putchar_unlocked, %r12
    func_ld64(fn, 13, (uint64_t)&getchar_unlocked);                 // movabsq  &getchar_unlocked, %r13
    func_emit(fn, "\x49\x89\xfe", 3);                               // movq     %rdi, %r14
    func_emit(fn, "\x49\x89\xff", 3);                               // movq     %rdi, %r15
#elif __aarch64__
    func_long(fn, 0xa9bd7bfd);                                      // stp      fp, lr, [sp, #-0x30]!
    func_long(fn, 0xa90273fb);                                      // stp      x27, x28, [sp, #0x20]
    func_long(fn, 0xa9016bf9);                                      // stp      x25, x26, [sp, #0x10]
    func_long(fn, 0x910003fd);                                      // mov      fp, sp
    func_ld64(fn, 25, (uint64_t)&putchar_unlocked);                 // movl     x25, &putchar_unlocked
    func_ld64(fn, 26, (uint64_t)&getchar_unlocked);                 // movl     x26, &getchar_unlocked
    func_long(fn, 0xaa0003fb);                                      // mov      x27, x0
    func_long(fn, 0xaa0003fc);                                      // mov      x28, x0
#else
#error "Unsupported CPU architecture"
#endif
}

static void func_epilogue(struct func_t *fn) {
#if __x86_64__
    func_emit(fn, "\x41\x5f", 2);                                   // popq     %r15
    func_emit(fn, "\x41\x5e", 2);                                   // popq     %r14
    func_emit(fn, "\x41\x5d", 2);                                   // popq     %r13
    func_emit(fn, "\x41\x5c", 2);                                   // popq     %r12
    func_emit(fn, "\x5d", 1);                                       // popq     %rbp
    func_emit(fn, "\xc3", 1);                                       // ret
#elif __aarch64__
    func_long(fn, 0xa9416bf9);                                      // ldp      x25, x26, [sp, #0x10]
    func_long(fn, 0xa94273fb);                                      // ldp      x27, x28, [sp, #0x20]
    func_long(fn, 0xa8c37bfd);                                      // ldp      fp, lr, [sp], #0x30
    func_long(fn, 0xd65f03c0);                                      // ret
#else
#error "Unsupported CPU architecture"
#endif
}

static void func_inst_next(struct func_t *fn, uint32_t n) {
#if __x86_64__
    if (n * sizeof(cell_t) <= INT8_MAX) {
        func_emit(fn, "\x49\x83\xc7", 3);                           // addq     ...., %r15
        func_byte(fn, n * sizeof(cell_t));                          //          $<n * sizeof(cell_t)>
    } else if (n * sizeof(cell_t) <= INT32_MAX) {
        func_emit(fn, "\x49\x81\xc7", 3);                           // addq     ...., %r15
        func_long(fn, n * sizeof(cell_t));                          //          $<n * sizeof(cell_t)>
    } else {
        func_emit(fn, "\xb8", 1);                                   // movl     ..., %eax
        func_long(fn, n * sizeof(cell_t));                          //          <n * sizeof(cell_t)>
        func_emit(fn, "\x49\x01\xc7", 3);                           // addq     %rax, %r15
    }
#elif __aarch64__
    if (n * sizeof(cell_t) <= 4095) {
        func_long(fn, 0x9100039c | ((n * sizeof(cell_t)) << 10));   // add      x28, x28, #<n * sizeof(cell_t)>
    } else {
        func_ld64(fn, 15, n * sizeof(cell_t));                      // movl     x15, <n * sizeof(cell_t)>
        func_long(fn, 0x8b0f039c);                                  // add      x28, x28, x15
    }
#else
#error "Unsupported CPU architecture"
#endif
}

static void func_inst_prev(struct func_t *fn, uint32_t n) {
#if __x86_64__
    if (n * sizeof(cell_t) <= INT8_MAX) {
        func_emit(fn, "\x49\x83\xef", 3);                           // subq     ...., %r15
        func_byte(fn, n * sizeof(cell_t));                          //          $<n * sizeof(cell_t)>
    } else if (n * sizeof(cell_t) <= INT32_MAX) {
        func_emit(fn, "\x49\x81\xef", 3);                           // subq     ...., %r15
        func_long(fn, n * sizeof(cell_t));                          //          $<n * sizeof(cell_t)>
    } else {
        func_emit(fn, "\xb8", 1);                                   // movl     ..., %eax
        func_long(fn, n * sizeof(cell_t));                          //          <n * sizeof(cell_t)>
        func_emit(fn, "\x49\x29\xc7", 3);                           // subq     %rax, %r15
    }
#elif __aarch64__
    if (n * sizeof(cell_t) <= 4095) {
        func_long(fn, 0xd100039c | ((n * sizeof(cell_t)) << 10));   // sub      x28, x28, #<n * sizeof(cell_t)>
    } else {
        func_ld64(fn, 15, n * sizeof(cell_t));                      // movl     x15, <n * sizeof(cell_t)>
        func_long(fn, 0xcb0f039c);                                  // sub      x28, x28, x15
    }
#else
#error "Unsupported CPU architecture"
#endif
}

static void func_inst_incr(struct func_t *fn, uint32_t n) {
#if __x86_64__
    if (n <= INT8_MAX) {
        func_emit(fn, "\x49\x83\x07", 3);                           // addq     ...., (%r15)
        func_byte(fn, n);                                           //          $<n>
    } else if (n <= INT32_MAX) {
        func_emit(fn, "\x49\x81\x07", 3);                           // addq     ...., (%r15)
        func_long(fn, n);                                           //          $<n>
    } else {
        func_emit(fn, "\xb8", 1);                                   // movl     ..., %eax
        func_long(fn, n);                                           //          <n>
        func_emit(fn, "\x49\x01\x07", 3);                           // addq     %rax, (%r15)
    }
#elif __aarch64__
    if (n <= 4095) {
        func_long(fn, 0xf940038e);                                  // ldr      x14, [x28]
        func_long(fn, 0x910001ce | (n << 10));                      // add      x14, x14, #<n>
        func_long(fn, 0xf900038e);                                  // str      x14, [x28]
    } else {
        func_long(fn, 0xf940038e);                                  // ldr      x14, [x28]
        func_ld64(fn, 15, n);                                       // movl     x15, <n>
        func_long(fn, 0x8b0f01ce);                                  // add      x14, x14, x15
        func_long(fn, 0xf900038e);                                  // str      x14, [x28]
    }
#else
#error "Unsupported CPU architecture"
#endif
}

static void func_inst_decr(struct func_t *fn, uint32_t n) {
#if __x86_64__
    if (n <= INT8_MAX) {
        func_emit(fn, "\x49\x83\x2f", 3);                           // subq     ...., (%r15)
        func_byte(fn, n);                                           //          $<n>
    } else if (n <= INT32_MAX) {
        func_emit(fn, "\x49\x81\x2f", 3);                           // subq     ...., (%r15)
        func_long(fn, n);                                           //          $<n>
    } else {
        func_emit(fn, "\xb8", 1);                                   // movl     ..., %eax
        func_long(fn, n);                                           //          <n>
        func_emit(fn, "\x49\x29\x07", 3);                           // subq     %rax, (%r15)
    }
#elif __aarch64__
    if (n <= 4095) {
        func_long(fn, 0xf940038e);                                  // ldr      x14, [x28]
        func_long(fn, 0xd10001ce | (n << 10));                      // sub      x14, x14, #<n>
        func_long(fn, 0xf900038e);                                  // str      x14, [x28]
    } else {
        func_long(fn, 0xf940038e);                                  // ldr      x14, [x28]
        func_ld64(fn, 15, n);                                       // movl     x15, <n>
        func_long(fn, 0xcb0f01ce);                                  // sub      x14, x14, x15
        func_long(fn, 0xf900038e);                                  // str      x14, [x28]
    }
#else
#error "Unsupported CPU architecture"
#endif
}

static void func_inst_putc(struct func_t *fn, uint32_t n) {
#if __x86_64__
    while (n--) {
        func_emit(fn, "\x49\x0f\xb6\x3f", 4);                       // movzbq   (%r15), %rdi
        func_emit(fn, "\x41\xff\xd4", 3);                           // callq    %r12
    }
#elif __aarch64__
    while (n--) {
        func_long(fn, 0x39400380);                                  // ldrb     w0, [x28]
        func_long(fn, 0xd63f0320);                                  // blr      x25
    }
#else
#error "Unsupported CPU architecture"
#endif
}

static void func_inst_getc(struct func_t *fn, uint32_t n) {
#if __x86_64__
    while (n--) func_emit(fn, "\x41\xff\xd5", 3);                   // callq    %r13 * n
    func_emit(fn, "\x0f\xb6\xc0", 3);                               // movzbl   %al, %eax
    func_emit(fn, "\x49\x89\x07", 3);                               // movq     %rax, (%r15)
#elif __aarch64__
    while (n--) func_long(fn, 0xd63f0340);                          // blr      x26 * n
    func_long(fn, 0x92401c00);                                      // and      x0, x0, #0xff
    func_long(fn, 0xf9000380);                                      // str      x0, [x28]
#else
#error "Unsupported CPU architecture"
#endif
}

static void func_inst_beqz_ul(struct func_t *fn) {
#if __x86_64__
    func_emit(fn, "\x49\x83\x3f\x00", 4);                           // cmpq     $0, (%r15)
    func_emit(fn, "\x0f\x84\x00\x00\x00\x00", 6);                   // je       ...
#elif __aarch64__
    func_long(fn, 0xf940038e);                                      // ldr      x14, [x28]
    func_long(fn, 0xb400000e);                                      // cbz      x14, ...
#else
#error "Unsupported CPU architecture"
#endif
}

static void func_inst_bnez_ul(struct func_t *fn) {
#if __x86_64__
    func_emit(fn, "\x49\x83\x3f\x00", 4);                           // cmpq     $0, (%r15)
    func_emit(fn, "\x0f\x85\x00\x00\x00\x00", 6);                   // jne      ...
#elif __aarch64__
    func_long(fn, 0xf940038e);                                      // ldr      x14, [x28]
    func_long(fn, 0xb500000e);                                      // cbnz     x14, ...
#else
#error "Unsupported CPU architecture"
#endif
}

static void func_inst_branch_patch(struct func_t *fn, const uint32_t *pctab, uint32_t br, uint32_t to) {
#if __x86_64__
    uint32_t pc = pctab[br] + 6;
    *(uint32_t *)&fn->buf[pc] = pctab[to] - pc - 4;

#elif __aarch64__
    uint32_t pc  = pctab[br] + 4;
    uint32_t rel = pctab[to] - pc;

    /* aarch64 requires instructions aligned at 4 bytes */
    if (rel & 3) {
        fprintf(stderr, "* fatal: unaligned branch target.\n");
        abort();
    }

    /* check for branch offset */
    if ((rel >> 21) && (rel >> 21) != 0x7ff) {
        fprintf(stderr, "* fatal: branch too far.\n");
        abort();
    }

    /* update branch offset */
    rel = (rel >> 2) & 0x7ffff;
    *(uint32_t *)&fn->buf[pc] |= rel << 5;
#else
#error "Unsupported CPU architecture"
#endif
}

static int bf_jit(struct code_t *c) {
    struct func_t fn;
    uint32_t      op;
    uint32_t      arg;
    int           ret   = -1;
    void *        mem   = calloc(MAX_MEM, sizeof(cell_t));
    uint32_t *    brtab = malloc(c->len * sizeof(uint32_t));
    uint32_t *    pctab = malloc((c->len + 1) * sizeof(uint32_t));

    /* function prologue */
    func_init(&fn);
    func_prologue(&fn);

    /* initialize tables */
    memset(brtab, 0xff, c->len * sizeof(uint32_t));
    memset(pctab, 0xff, (c->len + 1) * sizeof(uint32_t));

    /* compile each instructions */
    for (size_t i = 0; i < c->len; i++) {
        op       = c->buf[i] >> OP;
        arg      = c->buf[i] & ARG;
        pctab[i] = fn.len;

        /* main switch on opcode */
        switch (op) {
            default: {
                fprintf(stderr, "* fatal: invalid opcode %d.\n", op);
                goto exception;
            }

            /* basic instructions */
            case NEXT: func_inst_next(&fn, arg); break;
            case PREV: func_inst_prev(&fn, arg); break;
            case INCR: func_inst_incr(&fn, arg); break;
            case DECR: func_inst_decr(&fn, arg); break;
            case PUTC: func_inst_putc(&fn, arg); break;
            case GETC: func_inst_getc(&fn, arg); break;

            /* conditional branches */
            case BEQZ:
            case BNEZ: {
                ssize_t diff = to_int(arg);
                ssize_t dest = diff + i + 1;

                /* check for branch target */
                if (dest < 0 || dest > c->len) {
                    fprintf(stderr, "* error: branch out of bounds: %zd\n", dest);
                    goto exception;
                }

                /* emit the instruction */
                if (op == BEQZ) {
                    func_inst_beqz_ul(&fn);
                } else {
                    func_inst_bnez_ul(&fn);
                }

                /* record the branch target */
                brtab[i] = dest;
                break;
            }
        }
    }

    /* add function epilogue */
    pctab[c->len] = fn.len;
    func_epilogue(&fn);

    /* patch branch offsets */
    for (size_t i = 0; i < c->len; i++) {
        if (brtab[i] != UINT32_MAX) {
            func_inst_branch_patch(&fn, pctab, i, brtab[i]);
        }
    }

    /* execute the function */
    ret = 0;
    func_exec(&fn, mem);

exception:
    free(mem);
    free(brtab);
    free(pctab);
    return ret;
}

/** Interpreter **/

static int bf_eval(struct code_t *c) {
    cell_t *   mm = calloc(MAX_MEM, sizeof(cell_t));
    cell_t *   dp = mm;
    uint32_t * pc = c->buf;

    /* evaluate every instruction */
    while (pc < c->buf + c->len) {
        uint32_t bc  = *pc++;
        uint32_t op  = bc >> OP;
        uint32_t arg = bc & ARG;

        /* check memory if needed */
        if (op != NEXT && op != PREV) {
            if (__builtin_expect(dp < mm || dp >= mm + MAX_MEM, 0)) {
                free(mm);
                fprintf(stderr, "* error: memory access out of bounds: %zd\n", dp - mm);
                return -1;
            }
        }

        /* main switch on opcode */
        switch (op) {
            default: {
                fprintf(stderr, "* fatal: invalid opcode %d.\n", op);
                abort();
            }

            /* move to next cell */
            case NEXT: {
                dp += arg;
                break;
            }

            /* move to previous cell */
            case PREV: {
                dp -= arg;
                break;
            }

            /* increase the value of current cell */
            case INCR: {
                *dp += arg;
                break;
            }

            /* decrease the value of current cell */
            case DECR: {
                *dp -= arg;
                break;
            }

            /* print the current cell as character */
            case PUTC: {
                while (arg--) putchar_unlocked(*dp);
                break;
            }

            /* read one character from input, and put into the current cell */
            case GETC: {
                while (arg--) *dp = getchar();
                break;
            }

            /* branch if the current cell is zero */
            case BEQZ: {
                if (*dp == 0) pc += to_int(arg);
                break;
            }

            /* branch if the current cell is not zero */
            case BNEZ: {
                if (*dp != 0) pc += to_int(arg);
                break;
            }
        }

        /* check PC if needed */
        if (op == BEQZ || op == BNEZ) {
            if (__builtin_expect(pc < c->buf || pc > c->buf + c->len, 0)) {
                free(mm);
                fprintf(stderr, "* error: branch out of bounds: %zd\n", pc - c->buf);
                return -1;
            }
        }
    }

    /* finishes successfully */
    free(mm);
    return 0;
}

/** Compiler Driver **/

static int bf_main(const char *src, int print_ir, int enable_jit) {
    int             ret  = 0;
    const char *    ptr  = src;
    struct code_t   code = {};
    struct prog_t * prog = bf_parse(src, &ptr);

    /* check for syntax errors */
    if (!prog) {
        return -1;
    }

    /* check for garbage data */
    if (*ptr) {
        fprintf(stderr, "* syntax error: garbage after source: %s\n", ptr);
        prog_free(prog);
        return -1;
    }

    /* generate IR */
    code_init(&code);
    bf_gen_ir(&code, prog);
    prog_free(prog);

    /* select the execution mode */
    if (print_ir) {
        bf_dis(&code);
    } else if (enable_jit) {
        ret = bf_jit(&code);
    } else {
        ret = bf_eval(&code);
    }

    /* release the code */
    code_free(&code);
    return ret;
}

static void usage(char *exe) {
    fprintf(stderr, "usage: %s [-h] [-j] [-p] <input-file>\n", exe);
    fprintf(stderr, "    -h  this help message\n");
    fprintf(stderr, "    -j  enable JIT compilation\n");
    fprintf(stderr, "    -p  print compiled IR bytecode\n");
}

int main(int argc, char **argv) {
    int ch;
    int print_ir   = 0;
    int print_help = 0;
    int enable_jit = 0;

    /* disable output buffering */
    setbuf(stdout, NULL);
    setbuf(stderr, NULL);

    /* parse command line options */
    while ((ch = getopt(argc, argv, "hjp")) != -1) {
        switch (ch) {
            case 'p' : print_ir = 1; break;
            case 'h' : print_help = 1; break;
            case 'j' : enable_jit = 1; break;
            case '?' : return 1;
            default  : fprintf(stderr, "* fatal: invalid return value of getopt().\n"); abort();
        }
    }

    /* check for help */
    if (print_help) {
        usage(argv[0]);
        return 0;
    }

    /* get the source file name */
    if (optind >= argc) {
        fprintf(stderr, "* error: missing input file.\n");
        return 1;
    } else if (optind < argc - 1) {
        fprintf(stderr, "* error: multiple input files.\n");
        return 1;
    }

    /* open the source file */
    FILE * fp = fopen(argv[optind], "r");
    long   size;

    /* check for errors */
    if (!fp) {
        fprintf(stderr, "* error: cannot open file: [%d] %s\n", errno, strerror(errno));
        return 1;
    }

    /* determain the file size */
    if (fseek(fp, 0, SEEK_END) < 0 || (size = ftell(fp)) < 0 || fseek(fp, 0, SEEK_SET) < 0) {
        fclose(fp);
        fprintf(stderr, "* error: cannot get file size: [%d] %s\n", errno, strerror(errno));
        return 1;
    }

    /* allocate memory for the source content */
    char *buf = malloc(size + 1);
    memset(buf, 0, size + 1);

    /* read then close the source file */
    if (fread(buf, 1, size, fp) != size) {
        fclose(fp);
        fprintf(stderr, "* error: cannot read program.\n");
        return 1;
    }

    /* execute the program */
    int ret = bf_main(buf, print_ir, enable_jit);
    free(buf);
    fclose(fp);
    return ret;
}

