#include<stdio.h>

/*
int *BinheapDelete(int **binheap, int* size) {
    int* packet;
    packet = binheap[0];
    binheap[0] = binheap[*size - 1];
    *size--;
    Heapify(binheap, 
}
*/
int main() {
    int b =7;
    int *a = &b;
    printf("%p\n",a);
    *(a--);
    printf("%p\n",a);


}
