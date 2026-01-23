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

gc.o: intern.h schaf.h utils.h libscary.h
gc.%.o: intern.h schaf.h utils.h libscary.h
libscary.o: libscary.h
libscary.%.o: libscary.h
main.o: schaf.h utils.h
main.%.o: schaf.h utils.h
parse.o: intern.h schaf.h utils.h libscary.h
parse.%.o: intern.h schaf.h utils.h libscary.h
schaf.o: intern.h schaf.h utils.h libscary.h
schaf.%.o: intern.h schaf.h utils.h libscary.h
utils.o: utils.h
utils.%.o: utils.h

.PHONY: all clean analyze sanitize microbench

#
# Test
#
OBJ_TEST = $(OBJ_COMMON) test/basic-test.o
TIMEOUT_SEC ?= 20
TIMEOUT_SEC_LONGER ?= 60
RUNNER = timeout $(TIMEOUT_SEC)
v0 = $(V:0=)
VERBOSE = $(v0:1=-v)

test: test-c test-scheme test-error
test-san: test-c-san test-scheme-san test-error-san
test-stress: test-scheme-stress test-scheme-stress-san
test-all: test test-san test-stress

test-scheme-stress test-scheme-stress-san: TIMEOUT_SEC := $(TIMEOUT_SEC_LONGER)
test-c-san test-scheme-san test-scheme-stress-san: export ASAN_OPTIONS = detect_stack_use_after_return=0

test-c: test/basic-test
	$(RUNNER) ./$<
test-c-san: test/basic-test-san
	$(RUNNER) ./$<

test-scheme: schaf
	$(RUNNER) ./$< test/test.scm
test-scheme-san: schaf-san
	$(RUNNER) ./$< test/test.scm
test-scheme-stress: schaf
	$(RUNNER) ./$< -S test/test.scm
test-scheme-stress-san: schaf-san
	$(RUNNER) ./$< -S test/test.scm
test-error: schaf
	$(RUNNER) test/error.bash $(VERBOSE)
test-error-san: schaf-san
	SCHAF=$< $(RUNNER) test/error.bash $(VERBOSE)

test/basic-test: $(OBJ_TEST)
	$(CC) $(CFLAGS) -o $@ $^ $(LIBS) -lcriterion
test/basic-test-san: $(OBJ_TEST:.o=.san.o)
	$(CC) $(CFLAGS) $(SANITIZER) -o $@ $^ $(LIBS) -lcriterion

test/basic-test.o: schaf.h utils.h
test/basic-test.%.o: schaf.h utils.h

# Keep portability
gauche-test: # Do not forget to make `local?` #f
	gosh -Itest test.scm

.PHONY: test test-all test-san test-c test-c-san test-scheme test-scheme-san \
	test-scheme-stress test-scheme-stress-san gauche-test
