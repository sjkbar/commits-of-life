#include <stdio.h>
#include <stdlib.h>

typedef struct {
    char type;
    short arg1;
    short arg2;
} Command;

unsigned short START_INDEX;
unsigned short ROOT_INDEX;
unsigned short END_INDEX;
short* ROOT;
short* END;

int WORD_SIZE = 2;

int NODE_SIZE_OFFSET = 0;
int NODE_NEXT_OFFSET = 2;
int NODE_PREV_OFFSET = 4;

int MIN_FREE_BLOC_SIZE = 2 * 4;

char memorry[65536] = {0};
short memorrySize = 0;

short ids[1000] = {0};

int readTotalMemmorySize(FILE* file);
char readMomorryManagePolicy(FILE* file);
int readOneCommand(FILE* file, Command* command);
int memorryManage(unsigned short memorrySize, char memorryPolicy, FILE* file);
int readInt(FILE* file);
int firstFit(int memorrySize, FILE* file);
int BestFit(int memorrySize, FILE* file);
int WorstFit(int memorrySize, FILE* file);
int allocation(int position, int size);
unsigned short firstFitSearch(unsigned short position, short size);

short readFrontSize(short position);
short readEndSize(short position);
short readNext(short position);
short readPrev(short position);

void writeFrontSize(unsigned short position, unsigned short size);
void writeEndSize(unsigned short position, unsigned short size);
void writeNext(unsigned short position, unsigned short next);
void writePrev(unsigned short position, unsigned short prev);

void printArray(char array[], int size);

void addBlock(unsigned short position, unsigned short size);
void freeBlock(unsigned short position);

void printMem();
