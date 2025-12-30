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
TIMEOUT = timeout 2
TIMEOUT_LONGER = timeout 40

OBJ_COMMON = gc.o libscary.o parse.o schaf.o utils.o
OBJ = $(OBJ_COMMON) main.o

all: schaf

schaf: $(OBJ)
	$(CC) $(CFLAGS) -o $@ $^ $(LIBS)

clean:
	rm -f schaf schaf-*san test/basic-test test/basic-test-*san *.o test/*.o

analyze: $(OBJ:.o=.analyzer)

ubsan: schaf-ubsan test-ubsan
asan: schaf-asan test-asan

schaf-ubsan: $(OBJ:.o=.ubsan.o)
	$(CC) $(CFLAGS) $(UBSAN) -o $@ $^ $(LIBS)

schaf-asan: $(OBJ:.o=.asan.o)
	$(CC) $(CFLAGS) $(ASAN) -o $@ $^ $(LIBS)

microbench: schaf
	@$(MAKE) -C $@

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

%.s: %.c
	$(CC) -S -fverbose-asm $(CFLAGS) -c $<

%.analyzer: %.c
	$(CC) $(CFLAGS) $(ANALYZER) -c $< -o /dev/null

%.ubsan.o: %.c
	$(CC) $(CFLAGS) $(UBSAN) -c $< -o $@

%.asan.o: %.c
	$(CC) $(CFLAGS) $(ASAN) -c $< -o $@

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

test: test-c test-scheme
test-ubsan: test-c-ubsan test-scheme-ubsan
test-asan: test-c-asan test-scheme-asan
test-stress: test-scheme-stress test-scheme-stress-ubsan
test-all: test test-ubsan test-stress

test-c: test/basic-test
	$(TIMEOUT) ./$<
test-c-ubsan: test/basic-test-ubsan
	$(TIMEOUT) ./$<
test-c-asan: test/basic-test-asan
	$(TIMEOUT) ./$<

test-scheme: schaf
	$(TIMEOUT) ./$< test/test.scm
test-scheme-ubsan: schaf-ubsan
	$(TIMEOUT) ./$< test/test.scm
test-scheme-asan: schaf-asan
	$(TIMEOUT) ./$< test/test.scm
test-scheme-stress: schaf
	$(TIMEOUT_LONGER) ./$< -S test/test.scm
test-scheme-stress-ubsan: schaf-ubsan
	$(TIMEOUT_LONGER) ./$< -S test/test.scm

test/basic-test: $(OBJ_TEST)
	$(CC) $(CFLAGS) -o $@ $^ $(LIBS) -lcriterion
test/basic-test-ubsan: $(OBJ_TEST:.o=.ubsan.o)
	$(CC) $(CFLAGS) $(UBSAN) -o $@ $^ $(LIBS) -lcriterion
test/basic-test-asan: $(OBJ_TEST:.o=.asan.o)
	$(CC) $(CFLAGS) $(ASAN) -o $@ $^ $(LIBS) -lcriterion

test/basic-test.o: schaf.h utils.h
test/basic-test.%.o: schaf.h utils.h

.PHONY: test test-all test-ubsan test-c test-c-ubsan test-scheme test-scheme-ubsan \
	test-asan test-c-asan test-scheme-asan \
	test-scheme-stress test-scheme-stress-ubsan
