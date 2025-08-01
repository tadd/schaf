CC = gcc
OPTFLAGS = -O0 -ggdb3 -DDEBUG
CFLAGS = -std=gnu2x -Wall -Wextra -I. $(OPTFLAGS) $(XCFLAGS)
LIBS = -lm
ANALYZER = -fanalyzer
SANITIZER = -fsanitize=undefined #,address
TIMEOUT = timeout 2

OBJ_COMMON = gc.o parse.o schaf.o utils.o
OBJ = $(OBJ_COMMON) main.o
OBJ_TEST = $(OBJ_COMMON) test/basic-test.o

all: schaf test

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

gc.o: intern.h schaf.h utils.h
main.o: schaf.h utils.h
parse.o: intern.h schaf.h utils.h
schaf.o: intern.h schaf.h utils.h
utils.o: utils.h

.PHONY: all clean analyze sanitize microbench

# Test

test: test-c test-scheme
test-san: test-c-san test-scheme-san

test-c: test/basic-test
	$(TIMEOUT) ./$<
test-c-san: test/basic-test-san
	$(TIMEOUT) ./$<

test-scheme: schaf
	$(TIMEOUT) ./$< test/test.scm
test-scheme-san: schaf-san
	$(TIMEOUT) ./$< test/test.scm
test-scheme-stress: schaf
	./$< -S test/test.scm

test/basic-test: $(OBJ_TEST)
	$(CC) $(CFLAGS) -o $@ $^ $(LIBS) -lcriterion
test/basic-test-san: $(OBJ_TEST:.o=.san.o)
	$(CC) $(CFLAGS) $(SANITIZER) -o $@ $^ $(LIBS) -lcriterion

test/basic-test.o: schaf.h utils.h

.PHONY: test test-san test-c test-c-san test-scheme test-scheme-san test-scheme-stress
