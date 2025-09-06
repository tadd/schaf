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
    const char *flag = argv[optind-1];
    if (strncmp(flag, "--", 2) != 0) {
        eprintf("invalid option -- '%s'", flag);
        return '?';
    }
    const char *name = flag + 2;
    for (size_t i = 0; longopts[i].name != NULL; i++) {
        const OptLonger *e = &longopts[i];
        if (!e->has_arg && strcmp(e->name, name) == 0) {
            *val = (OptLongerData) { .name = name, .value = NULL };
            return 0;
        } else if (e->has_arg) {
            size_t len = strlen(e->name);
            if (strncmp(e->name, name, len) != 0 || (name[len] != '\0' && name[len] != '='))
                continue;
            if (name[len] == '\0' || strcmp(name + len, "=") == 0) {
                eprintf("option requires an argument '%s'", argv[optind - 1]);
                return '?';
            }
            const char *value = name + len + 1;
            *val = (OptLongerData) { .name = e->name, .value = value };
            return 0;
        }
    }
    eprintf("invalid option '%s'", argv[optind - 1]);
    return '?';
}

static void usage_opt(FILE* out, const char *opt, const char *desc)
{
    fprintf(out, "  %-16s%s\n", opt, desc);
}

[[gnu::noreturn]]
static void usage(FILE *out)
{
    fprintf(out, "Usage: schaf [-e <source>] [-pPTMh] <file>\n");
    usage_opt(out, "-e <source>", "evaluate <source> string directly instead of <file>");
    usage_opt(out, "-H <MiB>", "specify initial heap size");
    usage_opt(out, "-M", "print memory usage (VmHWM) at exit");
    usage_opt(out, "-p", "print last expression in the input");
    usage_opt(out, "-P", "only parse then exit before evaluation. implies -p");
    usage_opt(out, "-S", "put stress on GC");
    usage_opt(out, "-s", "print heap statistics before/after GC");
    usage_opt(out, "-T", "print consumed CPU time at exit");
    usage_opt(out, "-h", "print this help");
    exit(out == stdout ? 0 : 2);
}

#define opt_error(fmt, ...) do { \
        fprintf(stderr, "error: " fmt "\n" __VA_OPT__(,) __VA_ARGS__); \
        usage(stderr); \
    } while (0)

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
} SchOption;

static double parse_posnum(const char *s)
{
    char *ep;
    double val = strtod(s, &ep);
    if (val <= 0 || ep[0] != '\0')
        opt_error("invalid positive number '%s'", s);
    return val;
}

static void option_init(SchOption *o)
{
    memset(o, 0, sizeof(SchOption));
    o->init_heap_size_mib = 0.0;
}

static SchOption parse_opt(int argc, char *const *argv)
{
    SchOption o;
    option_init(&o);
    int opt;
    while ((opt = getopt_longer(argc, argv, "e:H:MPpSsTh", NULL, NULL)) != -1) {
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
        case '?':
        default:
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
