CC = gcc
OPTFLAGS ?= -O0 -ggdb3 -DDEBUG
#OPTFLAGS ?= -O3 -flto=auto
warnflags = -Wcast-align -Wfloat-equal -Wpointer-arith -Wshadow -Wstrict-prototypes \
	    -Wswitch-enum -Wundef -Wunreachable-code -Wwrite-strings -Wformat=2
CFLAGS = -std=gnu2x -Wall -Wextra $(warnflags) -I. $(OPTFLAGS) $(XCFLAGS)
LIBS = -lm
ANALYZER = -fanalyzer
SANITIZER = -fsanitize=undefined #,address

OBJ_COMMON = gc.o libscary.o parse.o schaf.o utils.o
OBJ = $(OBJ_COMMON) main.o

all: schaf

schaf: $(OBJ)
	$(CC) $(CFLAGS) -o $@ $^ $(LIBS)

clean:
	rm -f schaf schaf-san test/basic-test test/basic-test-san *.o test/*.o

analyze: $(OBJ:.o=.analyzer)
sanitize: schaf-san test-san

schaf-san: $(OBJ:.o=.san.o)
	$(CC) $(CFLAGS) $(SANITIZER) -o $@ $^ $(LIBS)

microbench: schaf
	@$(MAKE) -C $@

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

%.human.s: %.c
	$(CC) -S -fverbose-asm $(CFLAGS) -c $< -o $@

%.s: %.c # for diff
	$(CC) -S -fno-asynchronous-unwind-tables $(CFLAGS) -c $<

%.analyzer: %.c
	$(CC) $(CFLAGS) $(ANALYZER) -c $< -o /dev/null

%.san.o: %.c
	$(CC) $(CFLAGS) $(SANITIZER) -c $< -o $@

gc.o gc.san.o: intern.h schaf.h utils.h libscary.h
libscary.o libscary.san.o: libscary.h
main.o main.san.o: schaf.h utils.h
parse.o parse.san.o: intern.h schaf.h utils.h libscary.h
schaf.o schaf.san.o: intern.h schaf.h utils.h libscary.h
utils.o util.san.o: utils.h

.PHONY: all clean analyze sanitize microbench

test test-all test-c test-c-san test-error test-error-san test-san test-scheme \
test-scheme-san test-scheme-stress test-scheme-stress-san test-stress gauche-test:
	$(MAKE) -C test $@

.PHONY: test test-all test-c test-c-san test-error test-error-san test-san test-scheme \
	test-scheme-san test-scheme-stress test-scheme-stress-san test-stress gauche-test
