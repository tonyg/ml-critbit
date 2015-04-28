#include <string.h>
#include <stdint.h>
#include <caml/mlvalues.h>

static int integer_length(unsigned int i) {
  int n = 0;
  while (i != 0) {
    i = i >> 1;
    n++;
  }
  return n;
}

static int all_zero(char const *buf, mlsize_t len)
{
  while (len--) {
    if (*buf++) return 0;
  }
  return 1;
}

static int first_nonzero_bit(char const *buf, mlsize_t len, mlsize_t offset_byte)
{
  mlsize_t i;
  for (i = 0; i < len; i++, buf++) {
    unsigned int v = * (unsigned char const *) buf;
    if (v) {
      int bit = 8 - integer_length(v);
      return (offset_byte + i) * 8 + bit;
    }
  }
  return -1;
}

CAMLprim value faststr_infinite_bytes_eq(value s1, value s2)
{
  mlsize_t len1, len2;
  int res;

  if (s1 == s2) return Val_bool(1);
  len1 = caml_string_length(s1);
  len2 = caml_string_length(s2);
  res = memcmp(String_val(s1), String_val(s2), len1 <= len2 ? len1 : len2);
  if (res != 0) return Val_bool(0);
  if (len1 < len2) return Val_bool(all_zero(String_val(s2) + len1, len2 - len1));
  if (len1 > len2) return Val_bool(all_zero(String_val(s1) + len2, len1 - len2));
  return Val_bool(1);
}

CAMLprim value faststr_infinite_bytes_diffbitpos(value s1, value s2)
{
  mlsize_t len1, len2, commonlen, i;
  unsigned char const *p1,  *p2;

  if (s1 == s2) return Val_int(-1);
  len1 = caml_string_length(s1);
  len2 = caml_string_length(s2);
  p1 = (unsigned char const *) String_val(s1);
  p2 = (unsigned char const *) String_val(s2);

  commonlen = len1 <= len2 ? len1 : len2;

  /* /\* NB: assumes 4-byte-aligned p1 and p2! *\/ */
  /* i = 0; */
  /* while ((i < (commonlen - 3)) && ((* (uint32_t *) p1) == (* (uint32_t *) p2))) { */
  /*   p1 += 4; */
  /*   p2 += 4; */
  /*   i += 4; */
  /* } */

  /* NB: assumes 8-byte-aligned p1 and p2! */
  i = 0;
  while ((i < (commonlen - 7)) && ((* (uint64_t *) p1) == (* (uint64_t *) p2))) {
    p1 += 8;
    p2 += 8;
    i += 8;
  }

  while (i < commonlen) {
    int delta = (*p1++) ^ (*p2++);
    if (delta) {
      int bit = 8 - integer_length(delta); /* 0th bit is high */
      return Val_int(i * 8 + bit);
    }
    i++;
  }

  if (len1 < len2) return Val_int(first_nonzero_bit(String_val(s2) + len1, len2 - len1, len1));
  if (len1 > len2) return Val_int(first_nonzero_bit(String_val(s1) + len2, len1 - len2, len2));
  return Val_int(-1);
}
