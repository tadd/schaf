#ifndef SCARY_H
#define SCARY_H

#include <stddef.h>
#include <stdint.h>

void *scary_new(size_t size);
void scary_free(void *ary);
size_t scary_length(const void *ary);
void scary_pop(void *ary);

#ifdef __APPLE__
#define scary_push_archdep_pattern() unsigned long: scary_push_uint64,
#else
#define scary_push_archdep_pattern() //empty
#endif
#define scary_push(pary, elem) \
    _Generic(elem, \
        scary_push_archdep_pattern() \
        char: scary_push_char, \
        int8_t: scary_push_int8, \
        int16_t: scary_push_int16, \
        int32_t: scary_push_int32, \
        int64_t: scary_push_int64, \
        uint8_t: scary_push_uint8, \
        uint16_t: scary_push_uint16, \
        uint32_t: scary_push_uint32, \
        uint64_t: scary_push_uint64, \
        char *: scary_push_charp, \
        const char *: scary_push_ccharp, \
        int8_t *: scary_push_int8p, \
        int16_t *: scary_push_int16p, \
        int32_t *: scary_push_int32, \
        int64_t *: scary_push_int64p, \
        uint8_t *: scary_push_uint8p, \
        uint16_t *: scary_push_uint16p, \
        uint32_t *: scary_push_uint32p, \
        uint64_t *: scary_push_uint64p, \
        void *: scary_push_voidp)(pary, elem)

void scary_push_char(char **, char);
void scary_push_int8(int8_t **, int8_t);
void scary_push_int16(int16_t **, int16_t);
void scary_push_int32(int32_t **, int32_t);
void scary_push_int64(int64_t **, int64_t);
void scary_push_uint8(uint8_t **, uint8_t);
void scary_push_uint16(uint16_t **, uint16_t);
void scary_push_uint32(uint32_t **, uint32_t);
void scary_push_uint64(uint64_t **, uint64_t);
void scary_push_ccharp(const char ***, const char *);
void scary_push_charp(char ***, const char *);
void scary_push_voidp(void ***, const void *);
void scary_push_int8p(int8_t ***, const int8_t *);
void scary_push_int16p(int16_t ***, const int16_t *);
void scary_push_int32p(int32_t ***, const int32_t *);
void scary_push_int64p(int64_t ***, const int64_t *);
void scary_push_uint8p(uint8_t ***, const uint8_t *);
void scary_push_uint16p(uint16_t ***, const uint16_t *);
void scary_push_uint32p(uint32_t ***, const uint32_t *);
void scary_push_uint64p(uint64_t ***, const uint64_t *);

#define scary_dup(p) \
    _Generic(p, \
        char *: scary_dup_char, \
        int8_t *: scary_dup_int8, \
        int16_t *: scary_dup_int16, \
        int32_t *: scary_dup_int32, \
        int64_t *: scary_dup_int64, \
        uint8_t *: scary_dup_uint8, \
        uint16_t *: scary_dup_uint16, \
        uint32_t *: scary_dup_uint32, \
        uint64_t *: scary_dup_uint64, \
        void *: scary_dup_void)(p)

char *scary_dup_char(const char *);
void *scary_dup_void(const void *);
int8_t *scary_dup_int8(const int8_t *);
int16_t *scary_dup_int16(const int16_t *);
int32_t *scary_dup_int32(const int32_t *);
int64_t *scary_dup_int64(const int64_t *);
uint8_t *scary_dup_uint8(const uint8_t *);
uint16_t *scary_dup_uint16(const uint16_t *);
uint32_t *scary_dup_uint32(const uint32_t *);
uint64_t *scary_dup_uint64(const uint64_t *);

#endif
