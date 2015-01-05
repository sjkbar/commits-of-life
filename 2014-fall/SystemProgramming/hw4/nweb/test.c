
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

char *tokenstring = "first?25.5,second,15";
char *tokenstring1 = "GET /index.html?msg=dmake HTTP/1.0 ";
int result, i;
double fp;
char o[10], f[10], s[10], t[10], z[10], a[10];

void main()
{
        result = sscanf(tokenstring1, "%s %s %s", o, f, s);
        printf("%s\n", f);
        result = sscanf(f, "%[^'?']?%[^'=']=%s", t,z,a);
        printf("%s\n %s\n %s\n", t, z, a);
}

