#include <stdio.h>
#include <assert.h>
#include "runtime.h"

char* next_stack_pos(char* stack) {
  return stack - wordsize;
}


char* next_heap_pos(char* heap) {
  return heap + wordsize;
}

void fill_vec_with_fixnum(int *v, int len) {
  int i = 0;
  for (i = 0; i < len; i++) {
    v[i] = to_fixnum_rep(v[i]);
  }
}

char* vector_alloc(memory* mem, char* stack, unsigned int len, int *v) {
  char* heap = heap_alloc(mem, stack, align_heap((len+1)*wordsize));
  char* pv = heap;
  set_word_value(heap, to_fixnum_rep(len));
  heap = next_heap_pos(heap);
  int i = 0;
  for(i = 0; i < len; i++) {
    set_word_value(heap, v[i]);
    heap = next_heap_pos(heap);
  }
  return add_vectag(pv);
}

char* pair_alloc(memory* mem, char* stack, pair a_pair) {
  char* heap = heap_alloc(mem, stack, 2 * wordsize);
  char* pv = heap;
  set_word(heap, &(a_pair.car));
  heap = next_heap_pos(heap);
  set_word(heap, &(a_pair.cdr));
  return add_pairtag(pv);
}

char* closure_alloc(memory* mem, char* stack, closure a_clj) {
  char* heap = heap_alloc(mem, stack, 2 * wordsize);
  char* pv = heap;
  set_word(heap, &(a_clj.f_addr));
  heap = next_heap_pos(heap);
  set_word(heap, &(a_clj.env));
  return add_cljtag(pv);
}

// for vector
void test_gc1() {
  memory mem;
  int stack_size=16 * 4096,
      heap_size=64,
      global_size=16 * 4096,
      temp_size=stack_size;
  allocate_memory(&mem, stack_size, heap_size, global_size, temp_size);
  char* stack = mem.stack_top; // stack on top
  int v1[2] = {1, 2};
  fill_vec_with_fixnum(v1, 2);
  char* pv1 = vector_alloc(&mem, stack, 2, v1); // 16 bytes
  stack = next_stack_pos(stack);
  set_word(stack, &pv1); // stack point to pv1;
  int v2[2] = {3, 4};
  fill_vec_with_fixnum(v2, 2);
  vector_alloc(&mem, stack, 2, v2); // 16 bytes garbage
  vector_alloc(&mem, stack, 1, v2); // 8 bytes, trigger gc
  assert(mem.heap - mem.heap_base == 24);
  delete_memory(&mem);
}

// for vector, with cyclic reference
void test_gc2() {
  memory mem;
  int stack_size = 16,
      heap_size = 112,
      global_size = 16,
      temp_size = stack_size;
  allocate_memory(&mem, stack_size, heap_size, global_size,
      temp_size);
  char* stack = mem.stack_top;
  int v[1] = {0};
  fill_vec_with_fixnum(v, 1);
  char* pva = vector_alloc(&mem, stack, 1, v);
  /*vector_rep_set((ptr)pva, 0, to_fixnum_rep(2));
  printf("%d\n", to_fixnum(vector_rep_ref((ptr)pva, 0)));*/
  stack = next_stack_pos(stack);
  set_word(stack, &pva);
  char* pvb = vector_alloc(&mem, stack, 1, v);
  char* pvc = vector_alloc(&mem, stack, 1, v);
  vector_rep_set((ptr)pva, 0, (int)pvc);
  char* pvd = vector_alloc(&mem, stack, 1, v);
  vector_rep_set((ptr)pvb, 0, (int)pvd);
  char* pve = vector_alloc(&mem, stack, 1, v);
  char* pvf = vector_alloc(&mem, stack, 1, v);
  vector_rep_set((ptr)pvc, 0, (int)pvf);
  vector_rep_set((ptr)pvf, 0, (int)pva);
  /*print_ptr((ptr)pva);*/

  // pva->pvc->pvf->pva; recursive. pva can't be printed
  vector_alloc(&mem, stack, 1, v); // trigger gc
  assert((mem.heap - mem.heap_base) == 56);
  pva = (char*)get_word(stack);
  delete_memory(&mem);
}

// for vector, with cyclic reference
void test_gc3() {
  int stack_size = 16,
      heap_size = 64,
      global_size = 16,
      temp_size = stack_size;
  memory mem;
  allocate_memory(&mem, stack_size, heap_size, global_size,
      temp_size);
  char* stack = mem.stack_top;
  int v1[1] = {1};
  fill_vec_with_fixnum(v1, 1);
  int v2[1] = {2};
  fill_vec_with_fixnum(v2, 1);
  char* pva = vector_alloc(&mem, stack, 1, v1); // 8 bytes
  stack = next_stack_pos(stack);
  set_word(stack, &pva);
  char* pvb = vector_alloc(&mem, stack, 1, v2); // 8 bytes
  vector_rep_set((ptr)pva, 0, (int)pvb);
  vector_rep_set((ptr)pvb, 0, (int)pva);
  int v3[3] = {1,2,3};
  fill_vec_with_fixnum(v3, 3);
  vector_alloc(&mem, stack, 3, v3); // 16bytes garbage
  vector_alloc(&mem, stack, 1, v3); // 8bytes, trigger gc
  assert((mem.heap - mem.heap_base) == 24);
  delete_memory(&mem);
}


// for vector,pair and closure
void test_gc4() {
  int stack_size = 16,
      heap_size = 80,
      global_size = 16,
      temp_size = heap_size / 2;
  memory mem;
  allocate_memory(&mem, stack_size, heap_size, global_size, temp_size);
  char* stack = mem.stack_top;

  pair p1;
  p1.car = to_fixnum_rep(0);
  p1.cdr = to_fixnum_rep(0);
  char* pp1 = pair_alloc(&mem, stack, p1); // 8bytes, pair

  int v1[1] = {2};
  fill_vec_with_fixnum(v1, 1);
  char* pv1 = vector_alloc(&mem, stack, 1, v1); // 8bytes, vector

  vector_alloc(&mem, stack, 1, v1); // 8bytes, vector, garbage

  // pclj1:(addr,pv1)
  closure clj1;
  clj1.f_addr = 0;
  clj1.env = (ptr)pv1;
  char* pclj1 = closure_alloc(&mem, stack, clj1); // 8bytes, closure
  // pv3: (len, pp1)
  int v2[1] = {(int)pp1};
  char* pv3 = vector_alloc(&mem, stack, 1, v2); // 8bytes, vector


  // push pp1 to stack
  stack = next_stack_pos(stack);
  set_word_value(stack, (int)pp1);

  // create cyclic reference
  // pp1: (pv3, pclj1)
  set_car_from_rep((ptr)pp1, (int)pv3);
  set_cdr_from_rep((ptr)pp1, (int)pclj1);
  /*pair* temp = to_pair((ptr)pp1);
  printf("%d\n", is_vector(temp->car));
  return;*/


  vector_alloc(&mem, stack, 1, v1); // 8bytes, vector, togger gc
  assert(mem.heap - mem.heap_base == 40);
}


int main(int argc, const char *argv[])
{
  test_gc1();
  test_gc2();
  test_gc3();
  test_gc4();
  return 0;
}
