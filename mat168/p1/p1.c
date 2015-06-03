#include <stdio.h>
#include <math.h>

int nint(double x)
{
    return (int)(x + 0.5);
}

int main(void)
{
    double x[] = {
        38.24,
        39.57,
        40.56,
        36.26,
        33.48,
        37.56,
        38.42,
        37.52,
        41.23,
        41.17,
        36.08,
        38.47,
        38.15,
        37.51,
        35.49,
        39.36,
        38.09,
        36.09,
        40.44,
        40.33,
        40.37,
        37.57
    };
    double y[] = {
        20.42,
        26.15,
        25.32,
        23.12,
        10.54,
        12.19,
        13.11,
        20.44,
        9.10,
        13.05,
        -5.21,
        15.13,
        15.35,
        15.17,
        14.32,
        19.56,
        24.36,
        23.00,
        13.57,
        14.15,
        14.23,
        22.56
    };
    double latitude[2];
    double longitude[2];

    double PI = 3.141592;

    for (int i = 0; i < 2; ++i)
    {
        int deg = nint(x[i]);
        double min = x[i] - deg;
        latitude[i] = PI * (deg + 5.0 * min / 3.0) / 180.0;
        deg = nint(y[i]);
        min = y[i] - deg;
        longitude[i] = PI * (deg + 5.0 * min / 3.0) / 180.0;
    }

    double RRR = 6378.388;
    double q1 = cos(longitude[0] - longitude[1]);
    double q2 = cos(latitude[0] - latitude[1]);
    double q3 = cos(latitude[0] + latitude[1]);

    double dij =
        (int) (RRR * acos(0.5 * ((1.0 + q1) * q2 - (1.0 - q1) * q3)) + 1.0);

    printf("%d\n", dij);
    // lat[0]: 0.670206293333333480
    // lat[1]: 0.685623365185185185
    // long[0]: 0.361283080000000090
    // long[1]: 0.458148833333333283
    // q1: 0.995312180110287104
    // q2: 0.999881159301683975
    // q3: 0.213314863875116334
    return 0;
}
