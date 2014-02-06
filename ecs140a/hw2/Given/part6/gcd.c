#include <stdio.h>
#include <math.h>

int mySquare(int n) {
    return n * n;
}

int mySqrt(int n) {
    if (n < 0) {
        return 0;
    } else {
        return (int) sqrt(n);
    }
}

int main (void) {
    int x_x = -12345, x_y = -12345;
    x_x = 12;
    x_y = 21;
    doLabel0:
    if (x_x < x_y) {
        do {
            printf("%d\n", x_y);
            x_y = x_y - x_x;
        } while (x_x < x_y);
        goto doLabel0;
    } else if (x_x > x_y) {
        do {
            printf("%d\n", x_x);
            x_x = x_x - x_y;
        } while (x_x > x_y);
        goto doLabel0;
    }
    printf("%d\n", x_x);
    printf("%d\n", x_y);
    return 0;
}
