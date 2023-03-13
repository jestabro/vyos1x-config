
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>

int node_name_compare(const char* s1, const char* s2) {
    const char *p1 = s1;
    const char *p2 = s2;
    int pos = 0;
    int len1 = strlen(s1);
    int len2 = strlen(s2);
    int len = len1 <= len2 ? len1 : len2;

    do {
        while ((pos < len) && (!isdigit(*s1) || !isdigit(*s2))) {
            s1++;
            s2++;
            pos++;
        }
        if (pos) {
            int r = memcmp(p1, p2, pos);
            if (r) {
                int res = r < 1 ? -1 : 1;
                return res;
            }
            else {
                if (pos == len) {
                    if (len1 < len2) return -1;
                    if (len2 < len1) return 1;
                    return 0;
                }
            }
        }
        p1 = s1;
        p2 = s2;
        len = len - pos;
        len1 = len1 - pos;
        len2 = len2 - pos;
        pos = 0;
        int pos1 = pos;
        int pos2 = pos;
        int n1 = 0;
        int n2 = 0;
        while ((pos1 < len1) && isdigit(*s1)) {
            n1 = n1 * 10 + *s1 - '0';
            s1++;
            pos1++;
        }
        while ((pos2 < len2) && isdigit(*s2)) {
            n2 = n2 * 10 + *s2 - '0';
            s2++;
            pos2++;
        }
        if (n1 < n2) return -1;
        if (n2 < n1) return 1;
        if ((pos1 == len1) || (pos2 == len2)) {
            if (len1 < len2) return -1;
            if (len2 < len1) return 1;
            return 0;

        }
    } while (*s1 && *s2);
}

int main(int argc, char* argv[]) {
    if (argc < 3) {
        printf("Requires two strings to compare\n");
        exit(-1);
    }

    char *str1 = strdup(argv[1]);
    char *str2 = strdup(argv[2]);

    int res = node_name_compare(str1, str2);

    printf("result: %d\n", res);

}
