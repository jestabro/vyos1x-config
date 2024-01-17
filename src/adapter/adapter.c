/*
 * Testing things.
 */
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <string.h>
#include <assert.h>

#include <vyatta-cfg/vy_adapter.h>

// https://v2.ocaml.org/releases/4.14/htmlman/intfc.html#ss:c-outside-head
/* Create an OCaml value encapsulating the pointer p */
static value val_of_voidptr(void * p)
{
  assert (((uintptr_t) p & 1) == 0);  /* check correct alignment */
  return (value) p | 1;
}

/* Extract the pointer encapsulated in the given OCaml value */
static void * voidptr_of_val(value v)
{
  return (void *) (v & ~1);
}

CAMLprim value handle_init( value unit )
{
    CAMLparam1( unit );
    CAMLlocal1( ml_val );

    void *h = vy_cstore_init();
    ml_val = val_of_voidptr( h );

    CAMLreturn ( ml_val );
}

CAMLprim value handle_free ( value handle )
{
    CAMLparam1( handle );

    void * h = voidptr_of_val( handle );
    vy_cstore_free(h);

    CAMLreturn( Val_unit );
}

CAMLprim value set_result( value handle, value ml_list, value len )
{
    CAMLparam3( handle, ml_list, len );
    CAMLlocal2( ml_data, head );
    int length = Int_val(len);
    char * raw_data;
    size_t data_len;
    const char *path[length];
    int ii = 0;
    while ( ml_list != Val_emptylist )
    {
        head = Field(ml_list, 0);
        path[ii++] = String_val(head);
        ml_list = Field(ml_list, 1);
    }

    void *h = voidptr_of_val( handle );
    out_data_t *out_data = vy_set_path(h, path, length);
    if (out_data->length > 0) {
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
