#include "csapp.h"
#include <stdio.h>

#define READER_THREADS 5
#define WRITER_THREADS 3

int readcnt;
sem_t mutex, w, r;

void* reader(void* vargp);
void* writer(void* vargp);

int main () {
    int i = 0;
    pthread_t reader_thread_tids[READER_THREADS];
    pthread_t writer_thread_tids[WRITER_THREADS];
    Sem_init(&mutex, 2, 1);
    Sem_init(&w, 2, 1);
    Sem_init(&r, 2, 1);

    for(i=0; i<READER_THREADS; i++) {
        Pthread_create(&reader_thread_tids[i], NULL, reader, NULL);
    }

    for(i=0; i<WRITER_THREADS; i++) {
        Pthread_create(&writer_thread_tids[i], NULL, writer, NULL);
    }

    for(i=0; i<READER_THREADS; i++) {
        Pthread_join(reader_thread_tids[i], NULL);
    }

    for(i=0; i<READER_THREADS; i++) {
        Pthread_join(writer_thread_tids[i], NULL);
    }
    return 10;
}

void* reader(void* vargp) {
    while(1) {
        P(&r);
        P(&mutex); readcnt++;
        if(readcnt == 1)
            P(&w);
        V(&mutex);
        V(&r);

        //READING
        printf("reading\n");

        P(&mutex);
        readcnt--;
        if(readcnt == 0)
            V(&w);
        V(&mutex);
    }
}

void* writer(void* vargp) {
    while(1) {
        P(&r);
        P(&w);
        printf("writing\n");
        V(&w);
        V(&r);
    }
}

