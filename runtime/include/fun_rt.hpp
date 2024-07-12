#ifndef FUN4FUN_RUNTIME_FUN_RT_H
#define FUN4FUN_RUNTIME_FUN_RT_H
#include <assert.h>
#include <cstdint>
#include <initializer_list>
#include <stdint.h>
#include <vector>

extern const int64_t FF_PLACEHOLDER_TAG;
extern const int64_t FF_INT_TAG;
extern const int64_t FF_STR_TAG;
extern const int64_t FF_TUPLE_TAG;
extern const int64_t FF_CLOSURE_TAG;
extern const int64_t FF_MODOBJ_TAG;
extern const int64_t FF_CONSTR_BARRIER;

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

ff_obj_t ff_make_constr_no_payload(int64_t id);

ff_obj_t ff_make_constr_payload(int64_t id);

ff_obj_t ff_make_tuple(std::vector<ff_obj_t> objs, int64_t size);

ff_obj_t ff_make_mod_obj(const int64_t size,
                         std::vector<const char*>&& fields,
                         std::vector<ff_obj_t> values);

ff_obj_t
ff_make_closure(const ff_obj_t* fvs, int64_t fvs_n, ff_erased_fptr cfn);

ff_obj_t ff_make_closure(std::initializer_list<ff_obj_t>&& fvs,
                         int64_t fvs_n,
                         ff_erased_fptr cfn);

ff_obj_t ff_make_placeholder();

int64_t ff_get_int(ff_obj_t obj);

const char* ff_get_str(ff_obj_t obj);

int64_t ff_get_tag(ff_obj_t obj);

const void* ff_get_data(ff_obj_t obj);

const char* ff_get_str(ff_obj_t obj);

ff_obj_t ff_get_member(ff_obj_t base, const char* name);

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

ff_obj_t ff_add_int(ff_obj_t x, ff_obj_t y);

extern const ff_obj_t ff_builtin_add;

extern const ff_obj_t ff_builtin_print_int;

extern const ff_obj_t ff_builtin_print_str;

bool ff_match_constr(int64_t id, ff_obj_t cond);

bool ff_match_constr(int64_t id, ff_obj_t cond, ff_obj_t* payload);

bool ff_match_tuple(ff_obj_t cond, std::vector<ff_obj_t*> payloads);

#endif
