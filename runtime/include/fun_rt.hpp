#ifndef FUN4FUN_RUNTIME_FUN_RT_H
#define FUN4FUN_RUNTIME_FUN_RT_H
#include <assert.h>
#include <stdint.h>

extern const int64_t FF_CLOSURE_TAG;

void test_rt();

struct ff_obj_t {
    int64_t tag;
    const void* data;
};

using ff_fvs_t = ff_obj_t*;

using ff_erased_fptr = void (*)(void);

struct ff_closure_t {
    ff_fvs_t fvs;
    ff_erased_fptr cfn;
};

struct ff_obj_member_t {
    const char* name;
    ff_obj_t val;
    ff_obj_member_t* next;
};

ff_obj_t ff_make_int(int64_t val);

ff_obj_t ff_make_str(const char* val);

ff_obj_t ff_make_mod_obj(const int64_t size,
                         const char** fields,
                         const ff_obj_t* values);

ff_obj_t
ff_make_closure(const ff_obj_t* fvs, int64_t fvs_n, ff_erased_fptr cfn);

ff_obj_t ff_make_placeholder();

template <typename ReturnType, typename... Args> struct FunctionPointerType {
    using Type = ReturnType (*)(Args...);
};

template <typename... Types>
ff_obj_t ff_apply_generic(ff_obj_t op, Types... args) {
    assert(op.tag == FF_CLOSURE_TAG);
    ff_closure_t* clos = (ff_closure_t*)op.data;
    return reinterpret_cast<
        typename FunctionPointerType<ff_obj_t, ff_fvs_t, Types...>::Type>(
        clos->cfn)(clos->fvs, args...);
}

#endif
