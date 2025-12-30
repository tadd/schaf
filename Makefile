CC = gcc
OPTFLAGS ?= -O0 -ggdb3 -DDEBUG
#OPTFLAGS ?= -O3 -flto=auto
warnflags = -Wcast-align -Wfloat-equal -Wpointer-arith -Wshadow -Wstrict-prototypes \
	    -Wswitch-enum -Wundef -Wunreachable-code -Wwrite-strings -Wformat=2
CFLAGS = -std=gnu2x -Wall -Wextra $(warnflags) -I. $(OPTFLAGS) $(XCFLAGS)
LIBS = -lm
ANALYZER = -fanalyzer
UBSAN = -fsanitize=undefined
ASAN = -fsanitize=address

OBJ_COMMON = gc.o libscary.o parse.o schaf.o utils.o
OBJ = $(OBJ_COMMON) main.o

all: schaf

schaf schaf-ubsan schaf-asan:
	$(CC) $(CFLAGS) -o $@ $^ $(LIBS)

schaf: $(OBJ)
schaf-ubsan: CFLAGS := $(CFLAGS) $(UBSAN)
schaf-ubsan: $(OBJ:.o=.ubsan.o)
schaf-asan: CFLAGS := $(CFLAGS) $(ASAN)
schaf-asan: $(OBJ:.o=.asan.o)

clean:
	rm -f schaf schaf-*san test/basic-test test/basic-test-*san *.o test/*.o

analyze: $(OBJ:.o=.analyzer)
sanitize: ubsan

ubsan: schaf-ubsan test-ubsan
asan: schaf-asan test-asan

microbench: schaf
	@$(MAKE) -C $@

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

%.s: %.c
	$(CC) -S -fverbose-asm $(CFLAGS) -c $<

%.analyzer: %.c
	$(CC) $(CFLAGS) $(ANALYZER) -c $< -o /dev/null

%.ubsan.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

%.asan.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

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

.PHONY: all clean analyze ubsan asan microbench

#
# Test
#
OBJ_TEST = $(OBJ_COMMON) test/basic-test.o
TIMEOUT_SEC = 2
TIMEOUT_SEC_LONGER = 40
RUNNER = timeout $(TIMEOUT_SEC)

test: test-c test-scheme
test-ubsan: test-c-ubsan test-scheme-ubsan
test-asan: test-c-asan test-scheme-asan
test-stress: test-scheme-stress test-scheme-stress-ubsan
test-all: test test-ubsan test-stress

test-c-asan test-scheme-asan: RUNNER := ASAN_OPTIONS=detect_stack_use_after_return=0 $(RUNNER)
test-scheme-stress test-scheme-stress-ubsan: TIMEOUT_SEC := $(TIMEOUT_SEC_LONGER)

test-c: test/basic-test
test-c-ubsan: test/basic-test-ubsan
test-c-asan: test/basic-test-asan

test-c test-c-ubsan test-c-asan:
	$(RUNNER) ./$<

test-scheme: schaf
test-scheme-ubsan: schaf-ubsan
test-scheme-asan: schaf-asan

test-scheme-stress: schaf
test-scheme-stress-ubsan: schaf-ubsan

test-scheme test-scheme-ubsan test-scheme-asan:
	$(RUNNER) ./$< test/test.scm

test-scheme-stress test-scheme-stress-ubsan:
	$(RUNNER) ./$< -S test/test.scm

test/basic-test: $(OBJ_TEST)

test/basic-test-ubsan: CFLAGS := $(CFLAGS) $(UBSAN)
test/basic-test-ubsan: $(OBJ_TEST:.o=.ubsan.o)
test/basic-test-asan: CFLAGS := $(CFLAGS) $(ASAN)
test/basic-test-asan: $(OBJ_TEST:.o=.asan.o)

test/basic-test test/basic-test-ubsan test/basic-test-asan:
	$(CC) $(CFLAGS) -o $@ $^ $(LIBS) -lcriterion

test/basic-test.o: schaf.h utils.h
test/basic-test.%.o: schaf.h utils.h

.PHONY: test test-all test-ubsan test-c test-c-ubsan test-scheme test-scheme-ubsan \
	test-asan test-c-asan test-scheme-asan \
	test-scheme-stress test-scheme-stress-ubsan
