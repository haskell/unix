#include <dirent.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>

void check_error(const char *msg) {
    if (errno) {
        perror(msg);
        exit(1);
    }
}

int main() {
    printf("Testing struct dirent d_type in C\n");

    DIR *dir = opendir(".");
    check_error("opendir");

    struct dirent *de = NULL;

    do {
        de = readdir(dir);
        check_error("readdir");
    } while (de && strcmp(de->d_name, ".") != 0);
    // We found the . dir or encountered end of dir stream

    int status = 0;

    if (!de) {
        printf("Read the whole . dir without encountering \".\"!\n");
        status = 1;
    } else if (de->d_type == DT_DIR) {
        printf("Got DT_DIR for d_type for \".\"\n");
    } else if (de->d_type == DT_UNKNOWN) {
        printf("Got DT_UNKNOWN for d_type for \".\"\n");
        // Signal that we should skip test for non-zero d_type
        status = 2;
    } else {
        printf("Got %d for d_type for \".\"!\n", (int)de->d_type);
        status = 1;
    }

    closedir(dir);
    check_error("closedir");

    exit(status);
}
