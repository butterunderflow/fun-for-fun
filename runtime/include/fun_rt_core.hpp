#ifndef FUN4FUN_RUNTIME_FUN_RT_CORE_HPP
#define FUN4FUN_RUNTIME_FUN_RT_CORE_HPP

#include <array>
#include <assert.h>
#include <cstddef>
#include <cstdint>
#include <initializer_list>
#include <stdint.h>

extern const int64_t FF_PLACEHOLDER_TAG;
extern const int64_t FF_INT_TAG;
extern const int64_t FF_STR_TAG;
extern const int64_t FF_TUPLE_TAG;
extern const int64_t FF_CLOSURE_TAG;
extern const int64_t FF_MODOBJ_TAG;
extern const int64_t FF_CONSTR_BARRIER;

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

struct ff_tuple_t {
    int64_t size;
    ff_obj_t payloads[];
};

ff_obj_t ff_make_int(int64_t val);

ff_obj_t ff_make_str(const char* val);

ff_obj_t ff_make_constr_no_payload(int64_t id);

ff_obj_t ff_make_constr_payload(int64_t id);

template <std::size_t N>
ff_obj_t ff_make_tuple(ff_obj_t const (&objs)[N], int64_t size) {
    ff_tuple_t* GC_alloc_tuple(int64_t size);
    assert(N == size);
    ff_obj_t ret = {.tag = FF_TUPLE_TAG};
    ff_tuple_t* tu = GC_alloc_tuple(size);
    for (std::size_t i = 0; i < size; i++) {
        tu->payloads[i] = objs[i];
    }
    ret.data = tu;
    return ret;
}

inline ff_obj_t ff_make_mod_obj(const int64_t size,
                                const char* const (&fields)[0],
                                ff_obj_t const (&values)[0]) {
    return {.tag = FF_MODOBJ_TAG, .data = nullptr};
}

template <std::size_t N>
ff_obj_t ff_make_mod_obj(const int64_t size,
                         const char* const (&fields)[N],
                         ff_obj_t const (&values)[N]) {
    ff_obj_member_t* GC_alloc_member(void);

    assert(N == size);
    ff_obj_member_t* members = NULL;
    for (int64_t i = 0; i < N; i++) {
        ff_obj_member_t* new_member = GC_alloc_member();
        new_member->name = fields[i];
        new_member->val = values[i];
        new_member->next = members;
        members = new_member;
    }
    ff_obj_t ret = {.tag = FF_MODOBJ_TAG, .data = members};
    return ret;
}

ff_obj_t
ff_make_closure(const ff_obj_t* fvs, int64_t fvs_n, ff_erased_fptr cfn);

ff_obj_t ff_make_closure(std::initializer_list<ff_obj_t>&& fvs,
                         int64_t fvs_n,
                         ff_erased_fptr cfn);

ff_obj_t ff_make_placeholder();

template <std::size_t N2>
ff_fvs_t _ff_alloc_closure_and_fill(ff_erased_fptr const (&cfns)[N2],
                                    ff_obj_t* const (&binds)[N2],
                                    std::size_t N1) {
    ff_obj_t* GC_alloc_n_objs(int64_t n);
    ff_closure_t* GC_alloc_empty_closure(void);

    ff_fvs_t fvs_all = GC_alloc_n_objs(N1 + N2);
    for (int64_t i = 0; i < N2; i++) {
        ff_closure_t* self_i = GC_alloc_empty_closure();
        self_i->fvs = fvs_all;
        self_i->cfn = cfns[i];
        fvs_all[i].tag = FF_CLOSURE_TAG;
        fvs_all[i].data = self_i;
        *binds[i] = fvs_all[i];
    }
    return fvs_all;
}

template <std::size_t N2>
void ff_fill_letrec_closure(ff_obj_t const (&fvs)[0],
                            int64_t fvs_n,
                            ff_erased_fptr const (&cfns)[N2],
                            int64_t binds_n,
                            ff_obj_t* const (&binds)[N2]) {
    _ff_alloc_closure_and_fill(cfns, binds, 0);
}

template <std::size_t N1, std::size_t N2>
void ff_fill_letrec_closure(ff_obj_t const (&fvs)[N1],
                            int64_t fvs_n,
                            ff_erased_fptr const (&cfns)[N2],
                            int64_t binds_n,
                            ff_obj_t* const (&binds)[N2]) {
    assert(N1 == fvs_n);
    assert(N2 == binds_n);
    ff_fvs_t fvs_all = _ff_alloc_closure_and_fill(cfns, binds, N1);
    std::copy(fvs, fvs + N1, fvs_all + N2);
}

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

bool ff_match_constr(int64_t id, ff_obj_t cond);

bool ff_match_constr(int64_t id, ff_obj_t cond, ff_obj_t* payload);

template <std::size_t N>
bool ff_match_tuple(ff_obj_t cond, ff_obj_t* const (&payloads)[N]) {
    if (cond.tag != FF_TUPLE_TAG) {
        return false;
    }
    auto tu_ptr = reinterpret_cast<const ff_tuple_t*>(cond.data);
    for (size_t i = 0; i < N; i++) {
        *payloads[i] = tu_ptr->payloads[i];
    }
    return true;
}

ff_obj_t ff_is_equal(ff_obj_t x, ff_obj_t y);

bool ff_is_equal_aux(const ff_obj_t& x, const ff_obj_t& y);

ff_obj_t ff_is_not_equal(ff_obj_t x, ff_obj_t y);

bool ff_is_zero(ff_obj_t x);

#endif
