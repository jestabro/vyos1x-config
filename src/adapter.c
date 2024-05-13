/*
 * cstore adapter
 */
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <string.h>
#include <assert.h>

#include <vyatta-cfg/vy_adapter.h>

static value val_of_voidptr(void * p)
{
  assert (((uintptr_t) p & 1) == 0);
  return (value) p | 1;
}

static void * voidptr_of_val(value v)
{
  return (void *) (v & ~1);
}

CAMLprim value cstore_handle_init( value unit )
{
    CAMLparam1( unit );
    CAMLlocal1( ml_val );

    void *h = vy_cstore_init();
    ml_val = val_of_voidptr(h);

    CAMLreturn ( ml_val );
}

CAMLprim value cstore_handle_free( value handle )
{
    CAMLparam1( handle );

    void * h = voidptr_of_val(handle);
    vy_cstore_free(h);

    CAMLreturn( Val_unit );
}

CAMLprim value cpaths_handle_init( value unit )
{
    CAMLparam1( unit );
    CAMLlocal1( ml_val );

    void *h = vy_cpaths_init();
    ml_val = val_of_voidptr(h);

    CAMLreturn ( ml_val );
}

CAMLprim value cpaths_handle_free( value handle )
{
    CAMLparam1( handle );

    void * h = voidptr_of_val(handle);
    vy_cpaths_free(h);

    CAMLreturn( Val_unit );
}

CAMLprim value in_config_session_handle( value handle )
{
    CAMLparam1( handle );
    CAMLlocal1( ml_val );

    void *h = voidptr_of_val(handle);
    int v = vy_in_session(h);
    ml_val = Val_bool(v);

    CAMLreturn( ml_val );
}

CAMLprim value in_config_session( value unit )
{
    CAMLparam1( unit );
    CAMLlocal1( ml_val );

    void *h = vy_cstore_init();
    int v = vy_in_session(h);
    vy_cstore_free(h);
    ml_val = Val_bool(v);

    CAMLreturn( ml_val );
}

CAMLprim value set_path( value handle, value ml_list, value len )
{
    void *h = voidptr_of_val( handle );
    int length = Int_val(len);
    const char *path[length];
    int ii = 0;

    while ( ml_list != Val_emptylist )
    {
        path[ii++] = String_val(Field(ml_list, 0));
        ml_list = Field(ml_list, 1);
    }
    vy_add_set_path(h, path, length);
}

CAMLprim value set_path_reversed( value handle, value ml_list, value len )
{
    void *h = voidptr_of_val( handle );
    int length = Int_val(len);
    const char *path[length];
    int ii = 0;

    while ( ml_list != Val_emptylist )
    {
        path[length-1-ii++] = String_val(Field(ml_list, 0));
        ml_list = Field(ml_list, 1);
    }
    vy_add_set_path(h, path, length);
}

CAMLprim value delete_path( value handle, value ml_list, value len )
{
    void *h = voidptr_of_val( handle );
    int length = Int_val(len);
    const char *path[length];
    int ii = 0;

    while ( ml_list != Val_emptylist )
    {
        path[ii++] = String_val(Field(ml_list, 0));
        ml_list = Field(ml_list, 1);
    }
    vy_add_del_path(h, path, length);
}

CAMLprim value delete_path_reversed( value handle, value ml_list, value len )
{
    void *h = voidptr_of_val( handle );
    int length = Int_val(len);
    const char *path[length];
    int ii = 0;

    while ( ml_list != Val_emptylist )
    {
        path[length-1-ii++] = String_val(Field(ml_list, 0));
        ml_list = Field(ml_list, 1);
    }
    vy_add_del_path(h, path, length);
}

CAMLprim value load_paths( value cstore_handle, value cpaths_handle, value legacy )
{
    CAMLparam3( cstore_handle, cpaths_handle, legacy );
    CAMLlocal1( ml_data );
    int leg = Int_val(legacy);
    void *h = voidptr_of_val( cstore_handle );
    void *p = voidptr_of_val( cpaths_handle );
    char *raw_data;
    size_t data_len;

    out_data_t *out_data =  vy_load_paths(h, p, leg);
    if (out_data != NULL && out_data->length > 0) {
        data_len = out_data->length;
        raw_data = out_data->data;
        ml_data = caml_alloc_initialized_string(data_len, raw_data);
    }
    else {
        ml_data = caml_copy_string("");
    }
    out_data_free(out_data);

    CAMLreturn( ml_data );
}
