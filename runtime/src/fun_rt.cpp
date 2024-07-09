#include "fun_rt.hpp"
#include <assert.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void test_rt() {
    printf("Hello Runtime\n");
}

const int64_t FF_PLACEHOLDER_TAG = 0;
const int64_t FF_INT_TAG = 1;
const int64_t FF_STR_TAG = 2;
const int64_t FF_TUPLE_TAG = 3;
const int64_t FF_CLOSURE_TAG = 4;
const int64_t FF_MODOBJ_TAG = 5;
const int64_t FF_CONSTR_BARRIER = 10;

ff_obj_t* GC_alloc_n_objs(int64_t n) {
    return (ff_obj_t*)malloc(n * sizeof(ff_obj_t));
}

ff_closure_t* GC_alloc_empty_closure() {
    ff_closure_t* closure_ptr = (ff_closure_t*)malloc(sizeof(ff_closure_t));
    closure_ptr->fvs = NULL;
    return closure_ptr;
}

ff_closure_t* GC_alloc_closure(int64_t fvs_n) {
    ff_closure_t* closure_ptr = GC_alloc_empty_closure();
    closure_ptr->fvs = GC_alloc_n_objs(fvs_n);
    return closure_ptr;
}

ff_obj_member_t* GC_alloc_member() {
    ff_obj_member_t* member = (ff_obj_member_t*)malloc(sizeof(ff_obj_member_t));
    return member;
}

ff_obj_t ff_make_int(int64_t val) {
    ff_obj_t ret = {.tag = FF_INT_TAG};
    ret.data = (void*)val;
    return ret;
}

ff_obj_t ff_make_str(const char* val) {
    ff_obj_t ret = {.tag = FF_STR_TAG};
    ret.data = val;
    return ret;
}

ff_obj_t ff_make_tuple(const ff_obj_t* objs, int64_t size) {
    ff_obj_t ret = {.tag = FF_TUPLE_TAG};
    ff_obj_t* arr = GC_alloc_n_objs(size);
    for (size_t i = 0; i < size; i++) {
        arr[i] = objs[i];
    }
    ret.data = arr;
    return ret;
}

ff_obj_t ff_make_constr_no_payload(int64_t id) {
    ff_obj_t ret = {.tag = id + FF_CONSTR_BARRIER, .data = NULL};
    return ret;
}

ff_obj_t ff_constructor(ff_fvs_t fvs, ff_obj_t x) {
    int64_t id = (int64_t)fvs[0].data;
    ff_obj_t* payload = GC_alloc_n_objs(1);
    payload[0] = x;
    ff_obj_t ret = {.tag = id + FF_CONSTR_BARRIER, .data = payload};
    return ret;
}

ff_obj_t ff_make_constr_payload(int64_t id) {
    ff_obj_t* id_obj = GC_alloc_n_objs(1);
    id_obj[0] = ff_make_int(id);
    ff_obj_t constructor =
        ff_make_closure(id_obj, 1, (ff_erased_fptr)ff_constructor);
    return constructor;
}

ff_obj_t
ff_make_closure(const ff_obj_t* fvs, int64_t fvs_n, ff_erased_fptr cfn) {
    ff_closure_t* closure = GC_alloc_closure(fvs_n);
    closure->cfn = cfn;
    memcpy(closure->fvs, fvs, fvs_n * (sizeof(ff_obj_t)));
    ff_obj_t ret = {.tag = FF_CLOSURE_TAG, .data = closure};
    return ret;
}

void ff_fill_letrec_closure(ff_obj_t* fvs,
                            int64_t fvs_n,
                            ff_erased_fptr* cfns,
                            int64_t self_n,
                            ff_obj_t** binds) {

    ff_fvs_t fvs_all = GC_alloc_n_objs(fvs_n + self_n);

    for (int64_t i = 0; i < self_n; i++) {
        ff_closure_t* self_i = GC_alloc_empty_closure();
        self_i->fvs = fvs_all;
        self_i->cfn = cfns[i];
        fvs_all[i].tag = FF_CLOSURE_TAG;
        fvs_all[i].data = self_i;
        *binds[i] = fvs_all[i];
    }
    memcpy(fvs_all + self_n, fvs, fvs_n);
}

ff_obj_t ff_make_mod_obj(const int64_t size,
                         const char** fields,
                         const ff_obj_t* values) {
    /* todo: use sequenced memory for module members */
    ff_obj_member_t* members = NULL;
    for (int64_t i = 0; i < size; i++) {
        ff_obj_member_t* new_member = GC_alloc_member();
        new_member->name = fields[i];
        new_member->val = values[i];
        new_member->next = members;
        members = new_member;
    }
    ff_obj_t ret = {.tag = FF_MODOBJ_TAG, .data = members};
    return ret;
}

int64_t ff_get_tag(ff_obj_t obj) {
    return obj.tag;
}

const void* ff_get_data(ff_obj_t obj) {
    return obj.data;
}

int64_t ff_get_int(ff_obj_t obj) {
    return reinterpret_cast<int64_t>(obj.data);
}

bool ff_get_bool(ff_obj_t obj) {
    return bool(reinterpret_cast<int64_t>(obj.data));
}

const char* ff_get_str(ff_obj_t obj) {
    return reinterpret_cast<const char*>(obj.data);
}

ff_obj_t ff_get_member(ff_obj_t base, const char* name) {
    assert(base.tag == FF_MODOBJ_TAG);
    ff_obj_member_t* member = (ff_obj_member_t*)base.data;
    while (member != NULL) {
        if (strcmp(member->name, name) == 0) {
            return member->val;
        }
        member = member->next;
    }
    assert(0 && "Member not exists, type system may unsound");
}

ff_obj_t ff_make_placeholder() {
    ff_obj_t ret = {.tag = FF_PLACEHOLDER_TAG, .data = NULL};
    return ret;
}

ff_obj_t ff_add_int(ff_obj_t x, ff_obj_t y) {
    assert(x.tag == FF_INT_TAG);
    assert(y.tag == FF_INT_TAG);
    return ff_make_int(ff_get_int(x) + ff_get_int(y));
}
