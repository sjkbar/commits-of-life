#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include "hw3.h"

int main(int argc, char* argv[]) {

    char* outputFilePath = "result.out";
    FILE* outputFile;

    if (argc != 2) {
        printf("need 2 args but %d\n", argc);
        return -1;
    }

    char* inputFilePath = argv[1];
    FILE* inputFile = fopen(inputFilePath, "r");
    if (inputFile == NULL) {
        printf("open input file fail\n");
        return -1;
    }

    memorrySize = readTotalMemmorySize(inputFile);
    if (memorrySize < 64 || memorrySize > 64000) {
        printf("wrong memorry size\n");
        return -1;
    }

    char memorryPolicy = readMomorryManagePolicy(inputFile);
    if (!(memorryPolicy == 'F' || memorryPolicy == 'B' || memorryPolicy == 'W')) {
        printf("wrong memorry policy\n");
        return -1;
    }

    int result = memorryManage(memorrySize, memorryPolicy, inputFile);
    if(result == -1) {
        printf("memorry manage fail\n");
        return -1;
    }

    printf("Done!");
    return 0;
}

int readTotalMemmorySize(FILE* file) {
    return readInt(file);
}

char readMomorryManagePolicy(FILE* file) {
    char ch = 0;
    char result = fgetc(file);

    //read until new line
    for(ch = fgetc(file); !(ch == EOF || ch == '\n'); ch = fgetc(file));

    return result;
}

int readOneCommand(FILE* file, Command* command) {
    command->type = fgetc(file);

    //read blank
    fgetc(file);

    if(command->type == EOF) {
        return EOF;
    }else if(command->type == 'A') {
        command->arg1 = readInt(file);
        command->arg2 = readInt(file);
        return 0;
    } else if(command->type == 'F') {
        command->arg1 = readInt(file);
        command->arg2 = -1;
        return 0;
    } else if(command->type == 'S') {
        command->arg1 = -1;
        command->arg2 = -1;
        return 0;
    } else {
        printf("parseOneCommand fail %c", command->type);
        exit(-1);
    }
}

int readInt(FILE* file) {
    char ch = 0;
    int result = 0;
    for(ch = fgetc(file); !(ch == EOF || ch == ' ' || ch == '\n'); ch = fgetc(file)) {
        result = (result * 10) + (ch - '0');
    }

    return result;
}

void initBlock(short memorrySize) {
    ROOT = (short*) memorry;
    END = (short*) (memorry + WORD_SIZE);

    ROOT_INDEX = (char*) ROOT - memorry;
    END_INDEX = (char*) END - memorry;

    START_INDEX = WORD_SIZE * 2;
    *ROOT = START_INDEX;
    *END = START_INDEX;

    writeFrontSize(START_INDEX, memorrySize - START_INDEX);
    writeEndSize(START_INDEX, memorrySize - START_INDEX);
    writeNext(START_INDEX, (char*) END - memorry);
    writePrev(START_INDEX, (char*) ROOT - memorry);

    //printf("front Size %u | next %u | prev %u\n", readFrontSize(0), readNext(0), readPrev(0));
}

int memorryManage(unsigned short memorrySize, char memorryPolicy, FILE* file) {
    if(memorryPolicy == 'F')
        firstFit(memorrySize, file);
    else if(memorryPolicy == 'B')
        BestFit(memorrySize, file);
    else if(memorryPolicy == 'W')
        WorstFit(memorrySize, file);
    else {
        printf("Wrong memorryPolicy %c", memorryPolicy);
        exit(-1);
    }

    return 0;
}

int firstFit(int memorrySize, FILE* file) {
    initBlock(memorrySize);

    Command command;
    while (readOneCommand(file, &command) == 0) {
        printf("\n************\n");
        printMem();
        printf("Command (%c |  %d | %d)\n", command.type, command.arg1, command.arg2);


        if(command.type == 'A') {
            short size = command.arg2 + WORD_SIZE * 2;
            unsigned short position = firstFitSearch(*ROOT, size);
            printf("position %u\n", position);
            if(position == END_INDEX) {
                printf("Allocation Fail\n");
                continue;
            } else {
                addBlock(position, size);
                ids[command.arg1] = position;
            }
        } else if (command.type == 'F') {
            unsigned short position = ids[command.arg1];
            if(position == 0) {
                printf("Free Fail\n");
                continue;
            } else {
                freeBlock(position);
                ids[command.arg1] = 0;
            }

        } else {
            printf("Not implemeted protocol\n");
        }

        printMem();
    }

    return 0;
}

int BestFit(int memorrySize, FILE* file) {
    Command command;
    while (readOneCommand(file, &command) == 0) {
        printf("type : %c | arg1 = %d | arg2 = %d\n", command.type, command.arg1, command.arg2);
    }

    return 0;
}

int WorstFit(int memorrySize, FILE* file) {
    Command command;
    while (readOneCommand(file, &command) == 0) {
        printf("type : %c | arg1 = %d | arg2 = %d\n", command.type, command.arg1, command.arg2);
    }

    return 0;
}

short readFrontSize(short position) {
    return *((short*) (memorry + position + NODE_SIZE_OFFSET));
}

short readNext(short position) {
    return *((short*) (memorry + position + NODE_NEXT_OFFSET));
}

short readPrev(short position) {
    return *((short*) (memorry + position + NODE_PREV_OFFSET));
}

void writeFrontSize(unsigned short position, unsigned short size) {
    *((short*) (memorry + position + NODE_SIZE_OFFSET)) = size;
}       

void writeEndSize(unsigned short position, unsigned short size) {
    *((short*) (memorry + position + (size & -2) - WORD_SIZE)) = size;
}

void writeNext(unsigned short position, unsigned short next) {
    *((short*) (memorry + position + NODE_NEXT_OFFSET)) = next;
}

void writePrev(unsigned short position, unsigned short prev) {
    *((short*) (memorry + position + NODE_PREV_OFFSET)) = prev;
}

short readEndSize(short position) {
    unsigned short size = readFrontSize(position) & -2;
    if(size == 0) {
        printf("\n\nreadEndSize error\n");
        exit(-1);
    }
    return *((short*) (memorry + position + size - WORD_SIZE));
}

void printArray(char array[], int size) {
    int i;
    for(i =0; i < size ; i++) {
        printf("%u ", array[i]);
    }
    printf("\n");
}

void printMem() {
    printArray(memorry, memorrySize);
    printf("Current Memmory Allocation State\n");
    printf("--------------------------------\n");
    printf("ROOT %u\n", *ROOT);
    printf("END %u\n", *END);
    printf("ROOT_INDEX %u\n", ROOT_INDEX);
    printf("END_INDEX %u\n", END_INDEX);


    int count = 0;
    printf("Available Memorry\n");
    unsigned short position = *ROOT & -2;
    while(position != END_INDEX) {
        if(count++ == 10)
            return;
        unsigned short next = readNext(position) & -2;
        if(next == END_INDEX)
            printf("\t[%u~%u] = [%u | %u | %u | %u)\n", position, position + readFrontSize(position), readFrontSize(position), next, readPrev(position), readEndSize(position));
        else
            printf("\t[%u~%u] = [%u | %u | %u | %u)\n", position, next, readFrontSize(position), next, readPrev(position), readEndSize(position));

        position = next;
    }

    printf("Allocated Memorry\n");
    position = START_INDEX & -2;
    while(position < memorrySize) {
        unsigned short size = readFrontSize(position);
        unsigned short next = (position + size) & -2;

        if(size & 1)
            printf("\t[%u~%u]\n", position, next);
        position = next;
    }

    
    printf("\n\n");
}

// -1 : fail
// else : proper position
// size should be added FRONT_SIZE + END_SIZE
unsigned short firstFitSearch(unsigned short position, short size) {
    printf("firstFitSearch( position = %u, size = %u)\n", position, size);
    if (position == END_INDEX) {
        printf("firstFitSearch fail [position(%u) > memorrySize(%u)]\n", position, memorrySize);
        return END_INDEX;
    }

    short blockSize = readFrontSize(position);
    if (blockSize >= size)
        return position;
    else {
        short next = readNext(position);
        return firstFitSearch(next, size);
    }
}

//size should be added FRONT_SIZE + END_SIZE
void addBlock(unsigned short position, unsigned short size) {
    printf("addBlock( position = %u, size = %u)\n", position, size);
    short newSize = ((size + 1) >> 1) << 1; // evenify
    short oldSize = readFrontSize(position) & -2;

    unsigned short next = readNext(position);
    unsigned short prev = readPrev(position);
    if((oldSize - newSize) >= MIN_FREE_BLOC_SIZE) {

        // using bit setting with bit wise or
        memset(memorry+position, 0, newSize);
        writeFrontSize(position, (newSize | 1)); 
        writeEndSize(position, (newSize | 1));

        // build free block with linked list
        short newBlockPosition = position + newSize; 
        writeFrontSize(newBlockPosition, oldSize - newSize);
        writeEndSize(newBlockPosition, oldSize - newSize);
        writeNext(newBlockPosition, next);
        writePrev(newBlockPosition, prev);

        if (prev == ROOT_INDEX)
            *ROOT = newBlockPosition;
        else
            writeNext(prev, newBlockPosition);

        writePrev(newBlockPosition, prev);

        if (next == END_INDEX)
            *END = newBlockPosition;
        else
            writePrev(next, newBlockPosition);

        writeNext(newBlockPosition, next);
    } else {
        memset(memorry+position, 0, oldSize);
        writeFrontSize(position, (oldSize | 1)); 
        writeEndSize(position, (oldSize | 1));

        //goto prev and set prev's next to current
        //goto next and set next's prev to current
        if (prev == ROOT_INDEX)
            *ROOT = next;
        else
            writeNext(prev, next);

        if (next == END_INDEX)
            *END = prev;
        else
            writePrev(next, prev);
    }

    return;
}

void freeBlock(unsigned short position) {
    printf("freeBlock( position = %u)\n", position);
    unsigned short size = readFrontSize(position) & -2;

    //check next block is not using
    unsigned short nextPosition = position + size;
    unsigned short nextBlockSize = 0;
    bool isNextBlockUsing = 1;
    printf("nextPosition %u | memorrySize %u\n", nextPosition, memorrySize);
    if(nextPosition < memorrySize) {
        nextBlockSize = readFrontSize(nextPosition);
        isNextBlockUsing = nextBlockSize & 1;
    }

    //check previous block is not using
    short prevEndSizePosition = position - WORD_SIZE;
    unsigned short prevBlockSize = 0;
    bool isPrevBlockUsing = 1;
    if(prevEndSizePosition >= START_INDEX) {
        prevBlockSize = prevBlockSize = readFrontSize(prevEndSizePosition);
        isPrevBlockUsing = prevBlockSize & 1;
    }

    printf("size = %u | nextBlockSize = %u | prevBlockSize = %u\n", size, nextBlockSize, prevBlockSize);
    if(isNextBlockUsing && isPrevBlockUsing) {
        printf("Case A\n");
        writeFrontSize(position, size & -2);
        writeEndSize(position, size & -2);
        memset(memorry + position + WORD_SIZE, 0, size - MIN_FREE_BLOC_SIZE);

        unsigned short nextPosition = *ROOT;
        writeNext(position, nextPosition);
        if(nextPosition == END_INDEX)
            *END = position;
        else
            writePrev(nextPosition, position);

        *ROOT = position;

    } else if(isNextBlockUsing && !isPrevBlockUsing) {
        printf("Case B\n");

        unsigned short prevPosition = position - prevBlockSize;
        unsigned short prevFreeBlockPosition = readPrev(prevPosition);
        unsigned short nextFreeBlockPosition = readNext(prevPosition);

        writeFrontSize(prevPosition, (size + prevBlockSize) & -2);
        writeEndSize(prevPosition, (size + prevBlockSize) & -2);
        //printf("prevPosition = %u | size = %u | prevBlockSize = %u\n", prevPosition, size, prevBlockSize);
        memset(memorry + prevPosition + WORD_SIZE, 0, size + prevBlockSize - WORD_SIZE*2);

        //printf("prevFreeBlockPosition = %u | nextFreeBlockPosition = %u\n", prevFreeBlockPosition, nextFreeBlockPosition);
        if(prevFreeBlockPosition == ROOT_INDEX)
            *ROOT = nextFreeBlockPosition;
        else
            writeNext(prevFreeBlockPosition, nextFreeBlockPosition);

        if(nextFreeBlockPosition == END_INDEX)
            *END = prevFreeBlockPosition;
        else
            writePrev(nextFreeBlockPosition, prevFreeBlockPosition);


        nextFreeBlockPosition = *ROOT;
        writeNext(prevPosition, nextFreeBlockPosition);
        if(nextFreeBlockPosition == END_INDEX)
            *END = prevPosition;
        else
            writePrev(nextFreeBlockPosition, prevPosition);

        *ROOT = prevPosition;

    } else if(!isNextBlockUsing && isPrevBlockUsing) {
        printf("Case C\n");

        unsigned short prevFreeBlockPosition = readPrev(nextPosition);
        unsigned short nextFreeBlockPosition = readNext(nextPosition);

        writeFrontSize(position, (size + nextBlockSize) & -2);
        writeEndSize(position, (size + nextBlockSize) & -2);
        printf("nextPosition %u | nextBlockSize %u\n", nextPosition, nextBlockSize);
        memset(memorry + position + WORD_SIZE, 0, size + nextBlockSize - WORD_SIZE*2);

        printf("prevFreeBlockPosition %u | nextFreeBlockPosition %u\n", prevFreeBlockPosition, nextFreeBlockPosition); 
        if(prevFreeBlockPosition == ROOT_INDEX)
            *ROOT = nextFreeBlockPosition;
        else
            writeNext(prevFreeBlockPosition, nextFreeBlockPosition);

        if(nextFreeBlockPosition == END_INDEX)
            *END = prevFreeBlockPosition;
        else
            writePrev(nextFreeBlockPosition, prevFreeBlockPosition);

        nextFreeBlockPosition = *ROOT;
        writeNext(position, nextFreeBlockPosition);
        if(nextFreeBlockPosition == END_INDEX)
            *END = position;
        else
            writePrev(nextFreeBlockPosition, position);

        *ROOT = position;
    } else {
        printf("Case D\n");

        unsigned short prevPosition = position - prevBlockSize;
        unsigned short prevFreeBlockPosition = readPrev(nextPosition);
        unsigned short nextFreeBlockPosition = readNext(nextPosition);

        writeFrontSize(prevPosition, (size + prevBlockSize + nextBlockSize) & -2);
        writeEndSize(prevPosition, (size + prevBlockSize + nextBlockSize) & -2);
        memset(memorry + prevPosition + (WORD_SIZE*3), 0, size + nextBlockSize + prevBlockSize- MIN_FREE_BLOC_SIZE);
    }

}
