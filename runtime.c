#include <assert.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <unistd.h>
#include <string.h>
#include <assert.h>
#include "runtime.h"

int is_fixnum(ptr x) {
  return (x & fxmask) == fx_tag;
}

int is_char(ptr x) {
  return (x & charmask) == char_tag;
}

int to_fixnum(ptr x) {
  /*printf("%u\n", x);*/
  return ((int)x) >> fxshift;
}

int to_fixnum_rep(int v) {
  return (v << fxshift);
}

char to_char(ptr x) {
  return (char)((int)x >> charshift);
}

char beautify_temp[10];
char* beautify(char c) {
  if (c == '\t') {
    return "#\\tab";
  } else if (c == '\n') {
    return "#\\newline";
  } else if (c == '\r'){
    return "#\\return";
  } else if (c == ' ') {
    return "#\\space";
  } else {
    sprintf(beautify_temp, "#\\%c", c);
    return beautify_temp;
  }
}

int is_null(ptr x) {
  return x == null_v;
}

int is_void(ptr x) {
  return x == void_v;
}

int is_pair(ptr x) {
  return (x & pairmask) == pair_tag;
}

int is_list(ptr x) {
  return (is_pair(x) | is_null(x));
}

pair* to_pair(ptr x) {
  return (pair*)(x - pair_tag);
}

int is_closure(ptr x) {
  return (x & cljmask) == clj_tag;
}

closure* to_closure(ptr x) {
  return (closure*)(x - clj_tag);
}

int is_vector(ptr x) {
  return (x & vecmask) == vec_tag;
}

int is_string(ptr x) {
  return (x & strmask) == str_tag;
}

int is_heap_ptr(ptr x) {
  return is_pair(x) || is_closure(x) || is_vector(x) || is_string(x);
}

void print_null() {
  printf("()");
}

void print_void() {
  // ignore
}

void print_ptr_rec(ptr x);
void print_pair(ptr x) {
  pair* p = to_pair(x);
  print_ptr_rec(p->car);
  if (is_pair(p->cdr)) {
    printf(" ");
    print_pair(p->cdr);
  } else if (is_null(p->cdr)) {
    /*pass*/
  } else {
    printf(" . ");
    print_ptr_rec(p->cdr);
  }
}

void print_vector(int* x) {
  if (is_point_to_forward_ptr((char*)x)) {
    printf("-->%u", (ptr)x);
  } else {
    int len = vector_length(x);
    printf("#(");
    int i = 0;
    for(i = 0; i < len; i++) {
      print_ptr_rec((ptr)(vector_ref(x, i)));
      assert(get_word(vector_pi(x, i)) == vector_ref(x,i));
      if (i < len-1) {
        printf(" ");
      }
    }
    printf(")");
  }
}

void print_string(char* s) {
  unsigned int len = string_length(s);
  s += wordsize; // move to point to first char
  int i = 0;
  printf("\"");
  for (i = 0; i < len; i++) {
    char c = s[i];
    if (c == '"' )  {
      printf("\\\"");
    } else if (c == '\\') {
      printf("\\\\");
    } else {
      putchar(c);
    }
  }
  printf("\"");
}

void print_ptr_rec(ptr x) {
  /*printf("%u\n", x);*/
  if (is_fixnum(x)) {
    printf("%d", to_fixnum(x));
  } else if (x == bool_f) {
    printf("#f");
  } else if (x == bool_t) {
    printf("#t");
  } else if (is_null(x)) {
    print_null();
  } else if (is_void(x)) {
    print_void();
  } else if (is_char(x)) {
    printf("%s", beautify(to_char(x)));
  } else if (is_pair(x)) {
    printf("(");
    print_pair(x);
    printf(")");
  } else if (is_vector(x)) {
    print_vector(to_vector(x));
  } else if (is_string(x)) {
    print_string(to_string(x));
  } else {
    printf("#<unknown 0x%08x>", x);
  }
}

void print_ptr(ptr x) {
  print_ptr_rec(x);
  printf("\n");
}

int get_word(char *p) {
  int v;
  memcpy(&v, p, wordsize);
  return v;
}

// set the first word pointed by p with value pointed by vp
void set_word(void* p, void* vp) {
  memcpy(p, vp, wordsize);
}

// set word value
void set_word_value(void* p, int v) {
  set_word(p, &v);
}

// we use double word to store a forward_ptr
// since the heap alignment is double word
/*int is_forward_ptr(ptr x) {*/
int is_point_to_forward_ptr(char* p) {
  int tag = get_word(p);
  return (tag & gc_forward_mask) == gc_forward_tag;
}

// get forward ptr from a pointer that point to the forward ptr
char* get_forward_ptr(char* p) {
  return (char*)get_word(p + wordsize);
}

void mark_forward(char *new_p, char *p) {
  unsigned int tag = gc_forward_tag;
  set_word(p, &tag);
  set_word(p + wordsize, &new_p);
}

int* to_vector(ptr p) {
  return (int*)(p - vec_tag);
}
char* to_string(ptr p) {
  return (char*)(p - str_tag);
}

// get length of the vector stored in p
int vector_length(int* v) {
  return to_fixnum(v[0]);
}

// get ith pointer of vector
char* vector_pi(int* v, int i) {
  return (char*)&v[i+1];
}
// get ith value of vector
int vector_ref(int* v, int i) {
  return v[i+1];
}

int vector_rep_ref(ptr vp, int i) {
  return vector_ref(to_vector(vp), i);
}

void vector_set(int* vec, int i, int val) {
  set_word_value(vector_pi(vec, i), val);
}

void vector_rep_set(ptr vrep, int i, int val) {
  vector_set(to_vector(vrep), i, val);
}

void set_car(pair* pair_ptr, int val) {
  pair_ptr->car = (ptr)val;
}
void set_cdr(pair* pair_ptr, int val) {
  pair_ptr->cdr = (ptr)val;
}

void set_car_from_rep(ptr pair_rep, int val) {
  set_car(to_pair(pair_rep), val);
}

void set_cdr_from_rep(ptr pair_rep, int val) {
  set_cdr(to_pair(pair_rep), val);
}

unsigned int string_length(char* s) {
  return to_fixnum((ptr)get_word(s)); // get string length
}


char* add_heapptr_tag(char* p, unsigned int tag) {
  return (p + tag);
}

char* add_vectag(char* p) {
  return (p + vec_tag);
}

char* add_cljtag(char* p) {
  return (p + clj_tag);
}

char* add_pairtag(char* p) {
  return (p + pair_tag);
}

char* copy_to_newspace(memory* mem, char* pv, unsigned int size, unsigned int tag) {
  char* new_p = mem->heap;
  mem->heap += size;
  memcpy(new_p, pv, size);
  new_p = add_heapptr_tag(new_p, tag);
  mark_forward(new_p, pv);
  return new_p;
}

char* copy_as_vector(memory* mem, char* pv, unsigned int size) {
  return copy_to_newspace(mem, pv, size, vec_tag);
}

char* copy_as_pair(memory* mem, char* pv, unsigned int size) {
  return copy_to_newspace(mem, pv, size, pair_tag);
}

char* copy_as_clojure(memory* mem, char* pv, unsigned int size) {
  return copy_to_newspace(mem, pv, size, clj_tag);
}

char* copy_as_string(memory* mem, char* pv, unsigned int size) {
  return copy_to_newspace(mem, pv, size, str_tag);
}

// copy/forward p to the newspace
char* gc_forward(char* p, memory* mem,
    // return values:
    int* is_forward_ptr) {
  *is_forward_ptr = 0;
  if (!is_heap_ptr((ptr)p)) {
    // not a pointer to heap
    // should be immediate value
    return p;
  } else if (is_vector((ptr)p)) {
    // is_point_to_forward_ptr should be included in is_vector 
    // test!!! Think about it!
    int* vec = to_vector((ptr)p);
    if (is_point_to_forward_ptr((char*)vec)) {
      *is_forward_ptr = 1;
      return get_forward_ptr((char*)vec);
    } else {
      unsigned int vec_size = align_heap((vector_length(vec) + 1) * wordsize);
      return copy_as_vector(mem, (char*)vec, vec_size);
    } 
  } else if (is_pair((ptr)p)) {
    pair* pair_ptr = to_pair((ptr)p);
    if (is_point_to_forward_ptr((char*)pair_ptr)) {
      *is_forward_ptr = 1;
      return get_forward_ptr((char*)pair_ptr);
    } else {
      return copy_as_pair(mem, (char*)pair_ptr, 2 * wordsize);
    }
  } else if (is_closure((ptr)p)) {
    closure* clj_ptr = to_closure((ptr)p);
    if (is_point_to_forward_ptr((char*)clj_ptr)) {
      *is_forward_ptr = 1;
      return get_forward_ptr((char*)clj_ptr);
    } else {
      return copy_as_clojure(mem, (char*)clj_ptr, 2 * wordsize);
    }
  } else if (is_string((ptr)p)) {
    char* str_ptr = to_string((ptr)p);
    if (is_point_to_forward_ptr(str_ptr)) {
      *is_forward_ptr = 1;
      return get_forward_ptr((char*)str_ptr);
    } else {
      unsigned int str_size = align_heap(string_length(str_ptr) + wordsize);
      return copy_as_string(mem, str_ptr, str_size);
    }
  } else {
    print_ptr((ptr)p);
    exit(20);
  }
}

// forward the pointer and update it accordingly.
// pp is the address of p
void forward_and_update(char* p, char* pp, memory* mem, queue* pq) {
  int is_forward_ptr = 0;
  char* new_p = gc_forward(p, mem, &is_forward_ptr);
  assert(new_p != NULL);
  if (new_p != p) {
    set_word(pp, &new_p);
    if (!is_forward_ptr) {
      enqueue(pq, &new_p);
    }
  }
}

// swap heap_base,heap_top and heap_base1,heap_top1
void swap_mem_heap(memory* mem) {
  char* temp = mem->heap_base1;
  mem->heap_base1 = mem->heap_base;
  mem->heap_base = temp;
  temp = mem->heap_top1;
  mem->heap_top1 = mem->heap_top;
  mem->heap_top = temp;
}

// reset a block of memory
void reset_memory(char* start, char* end) {
  memset(start, 0, end-start);
}


// pmem: base of the memory that queue lies on
// usize: the size of cell in queue
// align: the alignment that the base, usize and maxsize should follow
// maxsize: max size of the queue
void queue_init(queue* pq, void* pmem, unsigned int usize,
    unsigned int align, unsigned int maxsize) {
  assert(
      is_align(usize, align) &&
      is_align(maxsize, align) &&
      is_align((int)pmem, align));
  pq->usize = usize;
  pq->maxsize = maxsize;
  pq->base = pq->front = pq->tail = pmem;
  pq->top = pmem + maxsize;
}

int is_queue_empty(queue* pq) {
  return pq->front == pq->tail;
}

int is_queue_full(queue* pq) {
  if ((pq->front - pq->tail) == pq->usize) {
    // we assume if there is only one cell available, then the
    // queue is full!
    return 1;
  } else {
    return 0;
  }
}

void _enqueue(queue* pq, void* pv) {
  memcpy(pq->tail, pv, pq->usize);
  pq->tail += pq->usize;
}
void enqueue(queue* pq, void* pv) {
  assert(!is_queue_full(pq));
  if (pq->tail + pq->usize <= pq->top) {
    _enqueue(pq, pv);
  } else {
    assert(pq->tail == pq->top); // alignment
    pq->tail = pq->base;
    _enqueue(pq, pv);
  }
}

// return pointer to the value
void* dequeue(queue* pq) {
  assert(!is_queue_empty(pq));
  void* temp = pq->front;
  pq->front += pq->usize;
  if (pq->front >= pq->top) {
    pq->front = pq->base;
  }
  return temp;
}

void queue_front(queue* pq, void* ret) {
  memcpy(ret, pq->front, pq->usize);
}

unsigned int queue_size(queue* pq) {
  if (pq->tail >= pq->front) {
    return (pq->tail - pq->front) / pq->usize;
  } else {
    return ((pq->top - pq->base) - (pq->front - pq->tail)) / pq->usize;
  }
}


void traverse_roots(char* base, char* top, queue* pq, memory* pmem) {
  while (base < top) {
    char* p = (char*)get_word(base); // p is pointer lies on stack but points to the heap 
    forward_and_update(p, base, pmem, pq);
    base += wordsize; // step base pointer
  }
}

// stop and copy garbage collection.
// BFS
void gc(memory *mem, char* stack) {
  /*printf("gc\n");*/
  reset_memory(mem->heap_base1, mem->heap_top1);
  mem->heap = mem->heap_base1; // set new heap pointer

  queue ptrq;
  queue_init(&ptrq, mem->temp_base, wordsize, wordsize,
      mem->temp_top - mem->temp_base);


  // traverse stack
  traverse_roots(stack, mem->stack_top, &ptrq, mem);

  // traverse global
  traverse_roots(mem->global_base, mem->global, &ptrq, mem);

  /*printf("queue size:%u\n", queue_size(&ptrq));*/
  while (!is_queue_empty(&ptrq)) {
    char* p;
    queue_front(&ptrq, &p);
    dequeue(&ptrq);
    if (is_vector((ptr)p)) {
      int* vec = to_vector((ptr)p);
      int vec_len = vector_length(vec);
      int i = 0;
      for(i = 0; i < vec_len; i+=1) {
        char* pvi = vector_pi(vec, i); // pointer to ith element of vector
        char* vi = (char*)get_word(pvi); // get ith element
        forward_and_update(vi, pvi, mem, &ptrq);
      }
    } else if (is_pair((ptr)p)) {
      pair* pair_ptr = to_pair((ptr)p);
      forward_and_update((char*)(pair_ptr->car), (char*)(&(pair_ptr->car)),
          mem, &ptrq);
      forward_and_update((char*)pair_ptr->cdr, (char*)(&(pair_ptr->cdr)),
          mem, &ptrq);
    } else if (is_closure((ptr)p)) {
      closure* pclj = to_closure((ptr)p);
      // for clojure, there's only a pointer(env)
      forward_and_update((char*)pclj->env,(char*)(&(pclj->env)), mem, &ptrq);
    } else if (is_string((ptr)p)) {
      // ignore string since it doesn't contain sub pointers
    } else {
      // just ignore
    }
  }
  swap_mem_heap(mem); // swap old space and new space
  /*reset_memory(mem->heap_base1, mem->heap_top1);*/
  // mem->heap now points to the address of the first free position in heap
}

int is_align(unsigned int v, unsigned int align) {
  return align_formula(v, align) == v;
}
unsigned int align_formula(unsigned int size, unsigned int align) {
  return (size + align - 1) & -align;
}
unsigned int align_heap(unsigned int size) {
  return align_formula(size, heap_align);
}

char* heap_alloc(memory *mem, char* stack, unsigned int size) {
  // [heap_base, heap_top)
  if (mem->heap + size > mem->heap_top) {
    gc(mem, stack);
  }
  assert((mem->heap + size) <= mem->heap_top);
  char* heap_tmp = mem->heap;
  mem->heap += size;
  return heap_tmp;
}


char* allocate_protected_space(int size) {
  int page = getpagesize();
  int status;
  int aligned_size = ((size + page - 1) / page) * page;
  char* p = mmap(0, aligned_size + 2 * page,
      PROT_READ | PROT_WRITE,
      MAP_ANONYMOUS | MAP_PRIVATE,
      0, 0);
  if (p == MAP_FAILED) { perror("map"); exit(1); }
  status = mprotect(p, page, PROT_NONE);
  if (status != 0) { perror("mprotect"); exit(status); }
  status = mprotect(p + page + aligned_size, page, PROT_NONE);
  if (status != 0) { perror("mprotect"); exit(status); }
  return (p + page);
}

void deallocate_protected_space(char* p, int size) {
  int page = getpagesize();
  int status;
  int aligned_size = ((size + page - 1) / page) * page;
  status = munmap(p - page, aligned_size + 2 * page);
  if (status != 0) { perror("munmap"); exit(status); }
}


void allocate_memory(memory* mem, unsigned int stack_size,
    unsigned int heap_size, unsigned int global_size,
    unsigned int temp_size) {
  char* mem_stack_base = allocate_protected_space(stack_size);
  char* mem_heap_base = allocate_protected_space(heap_size);
  char* mem_global_base = allocate_protected_space(global_size);
  char* temp_base = allocate_protected_space(temp_size);
  /*memset(mem_stack_base, 0, stack_size);
  memset(mem_heap_base, 0, heap_size);
  memset(mem_global_base, 0, global_size);*/

  mem->heap = mem_heap_base;
  mem->heap_base = mem_heap_base;
  mem->heap_top = mem->heap_base + heap_size/2;
  mem->heap_base1 = mem->heap_top;
  mem->heap_top1 = mem->heap_base + heap_size;
  mem->stack_base = mem_stack_base;
  mem->stack_top = mem->stack_base + stack_size;
  mem->global = mem_global_base;
  mem->global_base = mem_global_base;
  mem->global_top = mem->global_base + global_size;
  mem->heap_perm_base = mem_heap_base;
  mem->heap_perm_top = mem_heap_base + heap_size;
  mem->temp_base = temp_base;
  mem->temp_top = temp_base + temp_size;
}

void delete_memory(memory* mem) {
  deallocate_protected_space(mem->stack_base,
      mem->stack_top - mem->stack_base);
  deallocate_protected_space(mem->heap_perm_base,
      mem->heap_perm_top - mem->heap_perm_base);
  deallocate_protected_space(mem->global_base,
      mem->global_top - mem->global_base);
  deallocate_protected_space(mem->temp_base,
      mem->temp_top - mem->temp_base);
}
