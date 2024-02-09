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
//  assert (((uintptr_t) p & 1) == 0);
  return (value) p | 1;
}

static void * voidptr_of_val(value v)
{
  return (void *) (v & ~1);
}

CAMLprim value handle_init( value unit )
{
    CAMLparam1( unit );
    CAMLlocal1( ml_val );

    void *h = vy_cstore_init();
    ml_val = val_of_voidptr(h);

    CAMLreturn ( ml_val );
}

CAMLprim value handle_free( value handle )
{
    CAMLparam1( handle );

    void * h = voidptr_of_val(handle);
    vy_cstore_free(h);

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

CAMLprim value set_path( value handle, value ml_array, value len )
{
    CAMLparam3( handle, ml_array, len );
    int length = Int_val(len);
    const char *path[length];
    int err;
    int i;
    for (i=0; i < length; i++)
    {
        path[i++] = String_val(Field(ml_array, i));
    }

    void *h = voidptr_of_val( handle );
    err = vy_set_path(h, path, length);
    return Val_int(err);
}

CAMLprim value set_path_reversed( value handle, value ml_array, value len )
{
    CAMLparam3( handle, ml_array, len );
    int length = Int_val(len);
    const char *path[length];
    int err;
    int i;
    for (i=0; i < length; i++)
    {
        path[length-1-i++] = String_val(Field(ml_array, i));
    }

    void *h = voidptr_of_val( handle );
    err = vy_set_path(h, path, length);
    return Val_int(err);
}

CAMLprim value delete_path( value handle, value ml_array, value len )
{
    CAMLparam3( handle, ml_array, len );
    int length = Int_val(len);
    const char *path[length];
    int err;
    int i;
    for (i=0; i < length; i++)
    {
        path[i++] = String_val(Field(ml_array, i));
    }

    void *h = voidptr_of_val( handle );
    err = vy_delete_path(h, path, length);
    return Val_int(err);
}

CAMLprim value delete_path_reversed( value handle, value ml_array, value len )
{
    CAMLparam3( handle, ml_array, len );
    int length = Int_val(len);
    const char *path[length];
    int err;
    int i;
    for (i=0; i < length; i++)
    {
        path[length-1-i++] = String_val(Field(ml_array, i));
    }

    void *h = voidptr_of_val( handle );
    err = vy_delete_path(h, path, length);
    return Val_int(err);
}
