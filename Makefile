CC = gcc
OPTFLAGS ?= -O0 -ggdb3 -DDEBUG
#OPTFLAGS ?= -O3 -flto=auto
warnflags = -Wcast-align -Wfloat-equal -Wpointer-arith -Wshadow -Wstrict-prototypes \
	    -Wswitch-enum -Wundef -Wunreachable-code -Wwrite-strings -Wformat=2
CFLAGS = -std=gnu2x -Wall -Wextra $(warnflags) -I. $(OPTFLAGS) $(XCFLAGS)
LIBS = -lm
ANALYZER = -fanalyzer
SANITIZER = -fsanitize=undefined #,address
TIMEOUT = timeout 2
TIMEOUT_LONGER = timeout 40

OBJ_COMMON = gc.o libscary.o parse.o schaf.o utils.o
OBJ = $(OBJ_COMMON) main.o

all: schaf

schaf: $(OBJ)
	$(CC) $(CFLAGS) -o $@ $^ $(LIBS)

clean:
	rm -f schaf test/basic-test *-san test/*-san *.o test/*.o

analyze: $(OBJ:.o=.analyzer)

sanitize: schaf-san test-san

schaf-san: $(OBJ:.o=.san.o)
	$(CC) $(CFLAGS) $(SANITIZER) -o $@ $^ $(LIBS)

microbench: schaf
	@$(MAKE) -C $@

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

%.s: %.c
	$(CC) -S -fverbose-asm $(CFLAGS) -c $<

%.analyzer: %.c
	$(CC) $(CFLAGS) $(ANALYZER) -c $< -o /dev/null

%.san.o: %.c
	$(CC) $(CFLAGS) $(SANITIZER) -c $< -o $@

gc.o gc.san.o: intern.h schaf.h utils.h libscary.h
libscary.o libscary.san.o: libscary.h
main.o main.san.o: schaf.h utils.h
parse.o parse.san.o: intern.h schaf.h utils.h libscary.h
schaf.o schaf.san.o: intern.h schaf.h utils.h libscary.h
utils.o utils.san.o: utils.h

.PHONY: all clean analyze sanitize microbench

#
# Test
#
OBJ_TEST = $(OBJ_COMMON) test/basic-test.o

test: test-c test-scheme
test-san: test-c-san test-scheme-san
test-stress: test-scheme-stress test-scheme-stress-san
test-all: test test-san test-stress

test-c: test/basic-test
	$(TIMEOUT) ./$<
test-c-san: test/basic-test-san
	$(TIMEOUT) ./$<

test-scheme: schaf
	$(TIMEOUT) ./$< test/test.scm
test-scheme-san: schaf-san
	$(TIMEOUT) ./$< test/test.scm
test-scheme-stress: schaf
	$(TIMEOUT_LONGER) ./$< -S test/test.scm
test-scheme-stress-san: schaf-san
	$(TIMEOUT_LONGER) ./$< -S test/test.scm

test/basic-test: $(OBJ_TEST)
	$(CC) $(CFLAGS) -o $@ $^ $(LIBS) -lcriterion
test/basic-test-san: $(OBJ_TEST:.o=.san.o)
	$(CC) $(CFLAGS) $(SANITIZER) -o $@ $^ $(LIBS) -lcriterion

test/basic-test.o test/basic-test.san.o: schaf.h utils.h

.PHONY: test test-all test-san test-c test-c-san test-scheme test-scheme-san \
	test-scheme-stress test-scheme-stress-san
