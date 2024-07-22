#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

#include "schaf.h"
#include "utils.h"

[[gnu::noreturn]]
static void usage(FILE *out)
{
    fprintf(out, "Usage: schaf [-e <source>] [-pPTMh] <file>\n");
    fprintf(out, "  -e <source>\tevaluate <source> directly instead of <file>\n");
    fprintf(out, "  -H <MiB>\tspecify initial heap size\n");
    fprintf(out, "  -M\t\tprint memory usage (VmHWM) at exit\n");
    fprintf(out, "  -p\t\tprint last expression in the input\n");
    fprintf(out, "  -P\t\tonly parse then exit before evaluation. implies -p\n");
    fprintf(out, "  -S\t\tput stress on GC\n");
    fprintf(out, "  -T\t\tprint consumed CPU time at exit\n");
    fprintf(out, "  -h\t\tprint this help\n");
    exit(out == stdout ? 0 : 2);
}

[[gnu::format(printf, 1, 2)]]
static void opt_error(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    fprintf(stderr, "error: ");
    vfprintf(stderr, fmt, ap);
    fprintf(stderr, "\n");
    va_end(ap);
    usage(stderr);
}

typedef struct {
    const char *path;
    const char *script;
    bool print;
    bool parse_only;
    bool cputime;
    bool memory;
    double init_heap_size_mib;
    bool stress_gc;
} Option;

static double parse_posnum(const char *s)
{
    char *ep;
    double val = strtod(s, &ep);
    if (val <= 0 || ep[0] != '\0')
        error("invalid positive number '%s'", s);
    return val;
}

static Option parse_opt(int argc, char *const *argv)
{
    Option o = {
        .path = NULL,
        .script = NULL,
        .print = false,
        .parse_only = false,
        .cputime = false,
        .memory = false,
        .init_heap_size_mib = 0.0,
        .stress_gc = false,
    };
    int opt;
    while ((opt = getopt(argc, argv, "e:H:MPpSTh")) != -1) {
        switch (opt) {
        case 'e':
            o.script = optarg;
            break;
        case 'H':
            o.init_heap_size_mib = parse_posnum(optarg);
            break;
        case 'M':
            o.memory = true;
            break;
        case 'P':
            o.parse_only = o.print = true;
            break;
        case 'p':
            o.print = true;
            break;
        case 'S':
            o.stress_gc = true;
            break;
        case 'T':
            o.cputime = true;
            break;
        case 'h':
            usage(stdout);
        case '?':
        default:
            usage(stderr);
        }
    }
    o.path = argv[optind];
    if (o.path == NULL && o.script == NULL)
        opt_error("no program provided");
    if (o.path != NULL && o.script != NULL)
        opt_error("filename %s given while option '-e' passed", o.path);
    return o;
}

static double cputime_ms(void)
{
    struct timespec ts;
    clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &ts);
    return ts.tv_sec * 1000.0 + ts.tv_nsec / (1000.0*1000.0);
}

static void print_cputime(void)
{
    fprintf(stderr, "CPU: %.3lf ms\n", cputime_ms());
}

static void print_vmhwm(void)
{
    static const char *const path = "/proc/self/status",
        *const pat = "VmHWM";
    FILE *status = fopen(path, "r");
    if (status == NULL)
        error("cannot open file %s", path);
    char buf[BUFSIZ];
    bool printed = false;
    while (fgets(buf, sizeof(buf), status) != NULL) {
        if (strncmp(buf, pat, strlen(pat)) == 0) {
            fprintf(stderr, "%s", buf);
            printed = true;
            break;
        }
    }
    fclose(status);
    if (!printed)
        error("memory usage not printed");
}

int main(int argc, char **argv)
{
    Option o = parse_opt(argc, argv);
    sch_set_gc_stress(o.stress_gc);
    if (o.init_heap_size_mib > 0.0)
        sch_set_gc_init_size(o.init_heap_size_mib);

    SCH_INIT();
    Value v;
    if (o.parse_only)
        v = o.script ? parse_string(o.script) : parse(o.path);
    else
        v = o.script ? eval_string(o.script) : load(o.path);
    if (v == Qundef)
        error("%s", error_message());
    if (o.print) {
        display(v);
        printf("\n");
    }
    if (o.cputime)
        print_cputime();
    if (o.memory)
        print_vmhwm();
    return sch_fin();
}
