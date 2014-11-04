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

// -1 : fail
// else : proper position
// size should be added FRONT_SIZE + END_SIZE
unsigned short firstFitSearch(short position, short size) {
    printf("firstFitSearch( %u, %u)\n", position, size);
    if (position == END) {
        return -1;
    }

    short blockSize = readFrontSize(position);
    bool isUsing = blockSize & 1;
    if (blockSize >= size && isUsing == false)
        return position;
    else {
        short next = readNext(position);
        return firstFitSearch(next, size);
    }
}

//size should be added FRONT_SIZE + END_SIZE
void addBlock(unsigned short position, unsigned short size) {
    printf("addBlock( %u, %u)\n", position, size);
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

        //goto prev and set prev's next to current
        //goto next and set next's prev to current
        printf("next = %u ||| prev = %u ||| ROOT = %u \n", next, prev, ROOT);
        if (prev == ROOT) {
            ROOT = newBlockPosition;
            writePrev(newBlockPosition, ROOT);
        }
        else
            writeNext(prev, newBlockPosition);

        if (next != END)
            writePrev(next, newBlockPosition);
    } else {
        memset(memorry+position, 0, oldSize);
        writeFrontSize(position, (oldSize | 1)); 
        writeEndSize(position, (oldSize | 1));

        //goto prev and set prev's next to current
        //goto next and set next's prev to current
        if (prev == ROOT)
            ROOT = next;
        else
            writeNext(prev, next);

        if (next != END)
            writePrev(next, prev);
    }

    return;
}

void initBlock(short memorrySize) {
    ROOT = 0;

    writeFrontSize(0, memorrySize);
    writeEndSize(0, memorrySize);
    writeNext(0, END);
    writePrev(0, ROOT);

    //printf("front Size %u | next %u | prev %u\n", readFrontSize(0), readNext(0), readPrev(0));
    printArray(memorry, memorrySize);

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
        printf("************\n");
        printf("type : %c | arg1 = %d | arg2 = %d\n", command.type, command.arg1, command.arg2);
        if(command.type == 'A') {
            short size = command.arg2 + WORD_SIZE * 2;
            unsigned short position = firstFitSearch(ROOT, size);
            if(position == END) {
                printf("Allocation Fail\n");
                continue;
            } else {
                addBlock(position, size);
                ids[command.arg1] = position;
                printArray(memorry, memorrySize);
            }
        } else if (command.type == 'F') {
            short id = ids[command.arg1];
        } else {
            printf("Not implemeted protocol\n");
        }


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
    unsigned short allignedSize = size & -2;
    *((short*) (memorry + position + allignedSize - WORD_SIZE)) = size;
}

void writeNext(unsigned short position, unsigned short next) {
    *((short*) (memorry + position + NODE_NEXT_OFFSET)) = next;
}

void writePrev(unsigned short position, unsigned short prev) {
    *((short*) (memorry + position + NODE_PREV_OFFSET)) = prev;
}

void printArray(char array[], int size) {
    printf("ROOT %u\n", ROOT);
    int i;
    for(i =0; i < size ; i++) {
        printf("%u ", array[i]);
    }
    printf("\n");
}
