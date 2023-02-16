#include <errno.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

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

struct code_t {
    uint32_t * buf;
    size_t     len;
    size_t     cap;
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
        fprintf(stderr, "* fatal: invalid opcode %d\n", op);
        abort();
    }
    if (c->len >= c->cap) {
        c->cap *= 2;
        c->buf = realloc(c->buf, c->cap * sizeof(uint32_t));
    }
    c->buf[c->len] = (op << OP) | arg;
    c->len++;
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

static int to_i32(uint32_t v) {
    int val = v << 3;
    return val < 0 ? ~(~val >> 3) : val >> 3;
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

static void bf_gen_ir(struct code_t *c, struct prog_t *p) {
    for (size_t i = 0; i < p->len; i++) {
        struct inst_t * ins = &p->ins[i];
        size_t          rep = ins->rep;

        /* generate instructions */
        switch (ins->tok) {
            default: {
                fprintf(stderr, "* fatal: invalid opcode %d.\n", ins->tok);
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
                if (c->len - pc > MAX_ARG) {
                    fprintf(stderr, "* fatal: branch too far: %zd\n", c->len - pc);
                    abort();
                }

                /* loop end */
                code_append(c, BNEZ, (pc - c->len) & ARG);
                c->buf[pc] |= c->len - pc - 1;
                break;
            }
        }
    }
}

static int bf_jit(struct code_t *c) {
    fprintf(stderr, "* error: bf_jit: not implemented.\n");
    return -1;
}

static int bf_eval(struct code_t *c) {
    uint32_t *pc = c->buf;
    uint64_t *mm = calloc(MAX_MEM, sizeof(uint64_t));
    uint64_t *dp = mm;

    /* evaluate every instruction */
    while (pc < c->buf + c->len) {
        uint32_t iv  = *pc++;
        uint32_t op  = iv >> OP;
        uint32_t arg = iv & ARG;

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
                while (arg--) putchar((char)*dp);
                fflush(stdout);
                break;
            }

            /* read one character from input, and put into the current cell */
            case GETC: {
                while (arg--) *dp = (uint64_t)getchar();
                break;
            }

            /* branch if the current cell is zero */
            case BEQZ: {
                if (*dp == 0) pc += to_i32(arg);
                break;
            }

            /* branch if the current cell is not zero */
            case BNEZ: {
                if (*dp != 0) pc += to_i32(arg);
                break;
            }
        }

        /* check PC if needed */
        if (op == BEQZ || op == BNEZ) {
            if (__builtin_expect(pc < c->buf || pc >= c->buf + c->len, 0)) {
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

static int bf_execute(const char *src, int with_jit) {
    int             ret  = -1;
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
    if (with_jit) {
        ret = bf_jit(&code);
    } else {
        ret = bf_eval(&code);
    }

    /* release the code */
    code_free(&code);
    return ret;
}

static void usage(char *exe) {
    fprintf(stderr, "usage: %s [-h] [-j] <input-file>\n", exe);
    fprintf(stderr, "    -h  this help message\n");
    fprintf(stderr, "    -j  enable JIT compilation\n");
}

int main(int argc, char **argv) {
    int    jit  = -1;
    char * file = NULL;

    /* check for command line arguments */
    if (argc <= 1) {
        fprintf(stderr, "* error: missing input file.\n");
        return 1;
    }

    /* check for help */
    if (!strcmp(argv[1], "-h")) {
        usage(argv[0]);
        return 0;
    }

    /* parse the arguments */
    if (*argv[1] != '-') {
        if (argc == 2) {
            jit = 0;
            file = argv[1];
        }
    } else if (!strcmp(argv[1], "-j")) {
        if (argc == 3) {
            jit = 1;
            file = argv[2];
        }
    }

    /* check if the command line options are valid */
    if (!file || jit == -1) {
        fprintf(stderr, "* error: invalid command line options.\n");
        usage(argv[0]);
        return 1;
    }

    /* open the source file */
    FILE *fp = fopen(file, "r");
    long size;
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
    int ret = bf_execute(buf, jit);
    free(buf);
    fclose(fp);
    return ret;
}

