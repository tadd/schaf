#include <ctype.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

#include "schaf.h"
#include "utils.h"

typedef struct {
    const char *name;
    int ch; // declare an alias of short option
    bool has_arg;
} OptLonger;

typedef struct {
    const char *name;
    const char *value;
} OptLongerData;

#define eprintf(fmt, ...) fprintf(stderr, "error: " fmt "\n" __VA_OPT__(,) __VA_ARGS__)

static int getopt_longer(int argc, char *const *argv, const char *optstrorig,
                         const OptLonger longopts[], OptLongerData *val)
{
    if (longopts == NULL)
        return getopt(argc, argv, optstrorig);
    static char optstring[0x100] = { '\0', };
    if (optstring[0] == '\0') {
        if (strlen(optstrorig) >= sizeof(optstring) - 2 ||
            strchr(optstrorig, '-') != NULL)
            return '?';
        strcpy(optstring, optstrorig);
        strcat(optstring, "-:");
    }
    int opt = getopt(argc, argv, optstring);
    if (opt != '-')
        return opt; // as is
    const char *flag = argv[optind - 1];
    if (strncmp(flag, "--", 2) != 0)
        goto invalid;
    const char *name = flag + 2;
    for (size_t i = 0; longopts[i].name != NULL; i++) {
        const OptLonger *e = &longopts[i];
        size_t len = strlen(e->name);
        if (!(strncmp(e->name, name, len) == 0 && (name[len] == '\0' || name[len] == '=')))
            continue;
        const char *value = NULL;
        if (!e->has_arg && name[len] == '\0')
            ; // accepted
        else if (e->has_arg && name[len] == '=' && name[len + 1] != '\0')
            value = name + len + 1;
        else {
            eprintf("option requires %s argument '%s'",
                    e->has_arg ? "an" : "no", argv[optind - 1]);
            return '?';
        }
        if (val != NULL)
            *val = (OptLongerData) { .name = e->name, .value = value };
        return e->ch;
    }
 invalid:
    eprintf("invalid option '%s'", argv[optind - 1]);
    return '?';
}

static void usage_opt(FILE *out, const char *opt, const char *desc)
{
    fprintf(out, "  %-18s%s\n", opt, desc);
}

[[noreturn]]
static void usage(FILE *out)
{
    fprintf(out, "Usage: schaf [-HMpPsSTh] [-e <source>] <file>\n");
    usage_opt(out, "<file>", "Path or - (stdin).");
    usage_opt(out, "-e <source>", "Evaluate <source> string directly instead of <file>.");
    usage_opt(out, "-H <MiB>", "Specify initial heap size.");
    usage_opt(out, "-M", "Print memory usage (VmHWM) at exit.");
    usage_opt(out, "-p", "Print the last expression before exit.");
    usage_opt(out, "-P", "Only parse and print syntax list without evaluation.");
    usage_opt(out, "-s", "Print heap statistics before/after GC.");
    usage_opt(out, "--gc=<algorithm>", "Specify GC algorithm: mark-sweep, mark-sweep+bitmap, epsilon.");
    usage_opt(out, "-S, --gc-stress", "Put stress on GC.");
    usage_opt(out, "-T", "Print consumed CPU time at exit.");
    usage_opt(out, "-h, --help", "Print this help.");
    exit(out == stdout ? 0 : 2);
}

#define opt_error(fmt, ...) do { \
        eprintf(fmt __VA_OPT__(,) __VA_ARGS__); \
        usage(stderr); \
    } while (0)

typedef struct {
    const char *path;
    const char *script;
    double init_heap_size_mib;
    int gc_algorithm;
    bool print;
    bool parse_only;
    bool cputime;
    bool memory;
    bool heap_stat;
    bool stress_gc;
    bool interacitve;
} SchOption;

enum {
    SCH_GC_ALGORITHM_INVALID = 0xFF,
};

static int get_gc_algorithm(const char *s)
{
    static const struct {
        const char *name;
        SchGCAlgorithm algorithm;
    } algos[] = {
        { "mark-sweep", SCH_GC_ALGORITHM_MARK_SWEEP },
        { "mark-sweep+bitmap", SCH_GC_ALGORITHM_MARK_SWEEP_BITMAP },
        { "epsilon", SCH_GC_ALGORITHM_EPSILON },
    };
    for (size_t i = 0; i < sizeof(algos) / sizeof(*algos); i++) {
        if (strcmp(s, algos[i].name) == 0)
            return algos[i].algorithm;
    }
    return SCH_GC_ALGORITHM_INVALID;
}

static void parse_opt_longer(SchOption *o, const char *name, const char *value)
{
    if (strcmp(name, "gc") == 0) {
        int s = get_gc_algorithm(value);
        if (s == SCH_GC_ALGORITHM_INVALID)
            opt_error("invalid value for --gc: %s", value);
        o->gc_algorithm = s;
    }
}

static double parse_posnum(const char *s)
{
    char *ep;
    double val = strtod(s, &ep);
    if (val <= 0 || ep[0] != '\0')
        opt_error("invalid positive number '%s'", s);
    return val;
}

static SchOption parse_opt(int argc, char *const *argv)
{
    const OptLonger opts[] = {
        { "help", 'h', false },
        { "gc", 0, true },
        { "gc-stress", 'S', false },
        {}
    };
    SchOption o = {
        .init_heap_size_mib = 0.0,
        .gc_algorithm = SCH_GC_ALGORITHM_INVALID,
    };
    OptLongerData data = { 0 };
    int opt;
    while ((opt = getopt_longer(argc, argv, "e:H:MPpSsTh", opts, &data)) != -1) {
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
        case 's':
            o.heap_stat = true;
            break;
        case 'T':
            o.cputime = true;
            break;
        case 'h':
            usage(stdout);
        case 0:
            parse_opt_longer(&o, data.name, data.value);
            break;
        case '?':
            usage(stderr);
        }
    }
    o.path = argv[optind];
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
    SchOption o = parse_opt(argc, argv);
    sch_set_gc_stress(o.stress_gc);
    sch_set_gc_print_stat(o.heap_stat);
    if (o.init_heap_size_mib > 0.0)
        sch_set_gc_init_size(o.init_heap_size_mib);
    if (o.gc_algorithm != SCH_GC_ALGORITHM_INVALID)
        sch_set_gc_algorithm(o.gc_algorithm);

    SCH_INIT();
    if (o.interacitve)
        return repl();
    SchValue v;
    if (o.script)
        v = o.parse_only ? sch_parse_string(o.script) : sch_eval_string(o.script);
    else {
        if (strcmp(o.path, "-") == 0)
            v = o.parse_only ?
                sch_parse_file(stdin, "<stdin>") : sch_load_file(stdin, "<stdin>");
        else
            v = o.parse_only ? sch_parse(o.path) : sch_load(o.path);
    }
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
