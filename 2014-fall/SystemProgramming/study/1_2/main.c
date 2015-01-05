#include<stdio.h>
#include<dlfcn.h>

//strong
int x[2] = {1,2};
int y[2] = {3,4};

//weak
//bss
int z[2];

//global : define at module m, used at other module
int main() {
    void *handle;
    void (*addvec)(int*, int*, int*, int);
    char *error;

    handle = dlopen("./libvector.so", RTLD_LAZY);
    if(!handle) {
        fprintf(stderr, "%s\n", dlerror());
        exit(1);
    }

    addvec = dlsym(handle, "addvec");
    if((error=dlerror()) != NULL) {
        fprintf(stderr, "%s\n", error);
        exit(1);
    }

    addvec(x, y, z, 2);
    printf("(%d, %d)\n", z[0], z[1]);
}
