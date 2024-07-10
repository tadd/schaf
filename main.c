#include <ctype.h>
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
    fprintf(out, "  -s\t\tprint heap statistics before/after GC\n");
    fprintf(out, "  -T\t\tprint consumed CPU time at exit\n");
    fprintf(out, "  -h\t\tprint this help\n");
    exit(out == stdout ? 0 : 2);
}

#define opt_error(fmt, ...) do { \
        fprintf(stderr, "error: " fmt "\n" __VA_OPT__(,) __VA_ARGS__); \
        usage(stderr); \
    } while (0)

static const char *const OPTION_EXISTS = (void *)1U;
typedef struct {
    const char *t[127]; // non-extended ASCII table
    int index;
} GetOption;

static GetOption getoption(int argc, char *const *argv, const char *optstr)
{
    GetOption o;
    memset(&o, 0, sizeof(GetOption));
    for (int opt; (opt = getopt(argc, argv, optstr)) != -1; )
        o.t[opt] = optarg != NULL ? optarg : OPTION_EXISTS;
    o.index = optind;
    return o;
}

typedef struct {
    const char *path;
    const char *script;
    bool print;
    bool parse_only;
    bool cputime;
    bool memory;
    bool heap_stat;
    double init_heap_size_mib;
    bool stress_gc;
    bool interacitve;
} Option;

static double parse_posnum(const char *s)
{
    char *ep;
    double val = strtod(s, &ep);
    if (val <= 0 || ep[0] != '\0')
        opt_error("invalid positive number '%s'", s);
    return val;
}

static Option parse_opt(int argc, char *const *argv)
{
    Option o = {
        .init_heap_size_mib = -1,
    };
    GetOption go = getoption(argc, argv, "e:H:MPpSsTh");
    if (go.t['?'])
        usage(stderr);
    if (go.t['h'])
        usage(stdout);
    o.script = go.t['e'];
    if (go.t['H'])
        o.init_heap_size_mib = parse_posnum(go.t['H']);
    o.memory = go.t['M'];
    o.parse_only = o.print = go.t['P'];
    o.print = go.t['p'];
    o.stress_gc = go.t['S'];
    o.heap_stat = go.t['s'];
    o.cputime = go.t['T'];
    o.path = argv[go.index];
    if (o.path == NULL && o.script == NULL)
        o.interacitve = true;
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

static bool to_be_continued(SchValue ret)
{
    return ret == SCH_UNDEF &&
        strstr(sch_error_message(), "expected ')' but got EOF") != NULL;
}

static bool is_empty(const char *line)
{
    const char *p = line;
    while (isspace(*p))
        p++;
    return *p == '\0';
}

static int repl(void)
{
    char buf[BUFSIZ] = { '\0' }, line[BUFSIZ];
    for (;;) {
        printf(buf[0] ? ".....> " : "schaf$ ");
        if (fgets(line, sizeof(line), stdin) == NULL) // read,
            break;
        if (is_empty(line))
            continue;// ignore
        size_t nextlen = strlen(buf) + strlen(line);
        if (nextlen >= sizeof(buf)) {
            printf("error: input too large\n");
            buf[0] = '\0';// shrink
            continue;
        }
        strcat(buf, line);
        SchValue v = sch_eval_string(buf); // eval,
        if (to_be_continued(v))
            continue;
        buf[0] = '\0';
        if (v == SCH_UNDEF) {
            printf("error: %s\n", sch_error_message());
            continue;
        }
        sch_display(v); // print!
        printf("\n");
    }
    printf("\n");
    fflush(stdout);
    return 0;
}

int main(int argc, char **argv)
{
    Option o = parse_opt(argc, argv);
    sch_set_gc_stress(o.stress_gc);
    sch_set_gc_print_stat(o.heap_stat);
    if (o.init_heap_size_mib > 0.0)
        sch_set_gc_init_size(o.init_heap_size_mib);

    SCH_INIT();
    if (o.interacitve)
        return repl();
    SchValue v;
    if (o.parse_only)
        v = o.script ? sch_parse_string(o.script) : sch_parse(o.path);
    else
        v = o.script ? sch_eval_string(o.script) : sch_load(o.path);
    if (v == SCH_UNDEF)
        error("%s", sch_error_message()); // runtime error occurred
    if (o.print) {
        sch_display(v);
        printf("\n");
    }
    if (o.cputime)
        print_cputime();
    if (o.memory)
        print_vmhwm();
    return sch_fin();
}
