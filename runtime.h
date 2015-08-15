
#define fxshift 2
#define fxmask 0x03
#define bool_f 0x2F
#define bool_t 0x3F
#define null_v 0x4F
#define void_v 0x6F
#define wordsize 4
#define fx_tag 0x00
#define objshift 3
#define objmask 0x07
#define char_tag 0x0F
#define charmask 0xFF
#define charshift 8
#define pair_tag 0x01
#define pairmask objmask
#define symmask objmask
#define symtag 0x03
#define symshift 3
#define cljmask objmask
#define clj_tag 0x02
#define cljshift 3
#define vecmask objmask
#define vec_tag 0x05
#define vecshift 3
#define strmask objmask
#define str_tag 0x06
#define gc_forward_tag 0x47
#define gc_forward_mask 0xFF
#define gc_forward_shift 8
#define obj_shift 3  // object shift
#define heap_align (1 << obj_shift) // double word

#define debug_gc 0
#define test_gc 1


typedef unsigned int ptr;

// used to store temporary register value
typedef struct {
  void* eax; /* 0 scratch */
  void* ebx; /* 4 preserve */
  void* ecx; /* 8 scratch */
  void* edx; /* 12 scratch */
  void* esi; /* 16 preserve */
  void* edi; /*20 preserve*/
  void* ebp; /*24 preserve*/
  void* esp; /*28 preserve*/
} context;

typedef struct {
  char* heap; // heap pointer
  char* global; // global pointer
  char* heap_base;
  char* heap_top;
  char* heap_base1;
  char* heap_top1;
  char* stack_top;
  char* stack_base;
  char* global_base;
  char* global_top;
  char* heap_perm_base;
  char* heap_perm_top;
  char* temp_base;  // memories for temporary usage
  char* temp_top;
} memory;

typedef struct {
  ptr car;
  ptr cdr;
} pair;

typedef struct {
  ptr f_addr;
  ptr env;  // fvs env
} closure;

// a simple implementation of circular queue
typedef struct {
  void* front;
  void* tail;
  void* base;
  void* top;
  unsigned int usize; // number of bytes for each cell in queue
  unsigned int maxsize; // max number of bytes the queue can store
} queue;

//typedef int* vector;

// ptr points to heap
int is_heap_ptr(ptr p);
  
int to_fixnum(ptr x) ;
int to_fixnum_rep(int v);
void print_ptr(ptr x);
int is_vector(ptr p);
void print_ptr_rec(ptr x) ;
int vector_length(int* v);
int* to_vector(ptr p) ;
char* to_string(ptr p);
pair* to_pair(ptr p);
closure* to_closure(ptr p);
int is_closure(ptr x) ;
int is_pair(ptr x);
int is_fixnum(ptr x);
char* vector_pi(int* v, int i);
int vector_ref(int* v, int i);
int vector_rep_ref(ptr vp, int i);
void vector_set(int* vec, int i, int val);
void vector_rep_set(ptr vrep, int i, int val);
unsigned int string_length(char* s);
void set_car(pair* pair_ptr, int val);
void set_cdr(pair* pair_ptr, int val);
void set_car_from_rep(ptr pair_rep, int val);
void set_cdr_from_rep(ptr pair_rep, int val);
int get_word(char *p);
void set_word(void* p, void* vp);
void set_word_value(void* p, int val);
unsigned int align_heap(unsigned int size) ;
unsigned int align_formula(unsigned int size, unsigned align);
int is_align(unsigned int v, unsigned int align);
char* add_vectag(char* p) ;
char* add_cljtag(char* p) ;
char* add_pairtag(char* p) ;
char* heap_alloc(memory *mem, char* stack, unsigned int size);
void allocate_memory(memory* mem, unsigned int stack_size,
                     unsigned int heap_size,
                     unsigned int global_size,
                     unsigned int temp_size);
void delete_memory(memory *mem);

int is_point_to_forward_ptr(char* p);
char* get_forward_ptr(char* p);
void queue_init(queue* pq, void* pmem, unsigned int usize,
    unsigned int align, unsigned int maxsize) ;

int is_queue_empty(queue* pq);
int is_queue_full(queue* pq);
void enqueue(queue* pq, void* pv);
void* dequeue(queue* pq) ;
void queue_front(queue* pq, void* ret);
