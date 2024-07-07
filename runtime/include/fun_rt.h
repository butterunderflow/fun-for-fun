#ifndef FUN4FUN_RUNTIME_FUN_RT_H
#define FUN4FUN_RUNTIME_FUN_RT_H
#include <stdint.h>

void test_rt();

typedef struct {
    int64_t tag;
    const void* data;
} ff_obj_t;

typedef ff_obj_t* ff_fvs_t;

typedef ff_obj_t (*ff_cfunc_t)(ff_fvs_t, ff_obj_t);

typedef struct {
    ff_fvs_t fvs;
    ff_obj_t (*cfn)(ff_fvs_t, ff_obj_t);
} ff_closure_t;

typedef struct ff_obj_member_t {
    const char* name;
    ff_obj_t val;
    struct ff_obj_member_t* next;
} ff_obj_member_t;

ff_obj_t ff_make_int(int64_t val);

ff_obj_t ff_make_str(const char* val);

ff_obj_t ff_make_mod_obj(const int64_t size,
                         const char** fields,
                         const ff_obj_t* values);

ff_obj_t ff_make_closure(const ff_obj_t* fvs, int64_t fvs_n, ff_cfunc_t cfn);

ff_obj_t ff_make_placeholder();
#endif
