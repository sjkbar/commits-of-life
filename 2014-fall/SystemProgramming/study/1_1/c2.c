#include <stdio.h>
#include "global.h"

int main() {
    if (!init) {
        g = 37;
    }

    int t = f();
    printf("%d\n", t);
    return 0;
}
