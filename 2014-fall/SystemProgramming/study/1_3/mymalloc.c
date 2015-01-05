#ifdef COMPILETIME
#include <stdio.h>

void* mymalloc(size_t size, char* file, int line) {
    void *ptr = malloc(size);
    printf("%s:%d: malloc(%d)=%p\n", file, line, (int) size, ptr);
    return ptr;
}

void* myfree(void* ptr, char* file, int line) {
    free(ptr);
    printf("%s:%d: free(%p)\n", file, line, ptr);
}
#endif

#ifdef LINTERTIME
#include <stdio.h>

void* __real_malloc(size_t size);
void* __real_free(void* ptr);

void* mymalloc(size_t size, char* file, int line) {
    void *ptr = __real_malloc(size);
    printf("%s:%d: malloc(%d)=%p\n", file, line, (int) size, ptr);
    return ptr;
}

void* myfree(void* ptr, char* file, int line) {
    __real_free(ptr);
    printf("%s:%d: free(%p)\n", file, line, ptr);
}
#endif
