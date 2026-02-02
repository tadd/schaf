include ../Makefile

OBJ_TEST = $(OBJ_COMMON) test/basic-test.o
TIMEOUT_SEC ?= 5
TIMEOUT_SEC_LONGER ?= 30
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
	$(RUNNER) test/error/runtest.bash $(VERBOSE)
test-error-san: schaf-san
	SCHAF=$< $(RUNNER) test/error/runtest.bash $(VERBOSE)

test/basic-test: $(OBJ_TEST)
	$(CC) $(CFLAGS) -o $@ $^ $(LIBS) -lcriterion
test/basic-test-san: $(OBJ_TEST:.o=.san.o)
	$(CC) $(CFLAGS) $(SANITIZER) -o $@ $^ $(LIBS) -lcriterion

test/basic-test.o: schaf.h utils.h
test/basic-test.%.o: schaf.h utils.h

# Keep portability
gauche-test: # Do not forget to make (define local? #f)
	gosh -Itest test.scm

.PHONY: test test-all test-san test-c test-c-san test-scheme test-scheme-san \
	test-scheme-stress test-scheme-stress-san gauche-test
