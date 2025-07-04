/*
 * The purpose of this C program is to do one of 4 things.  Which one it does,
 * indicates to the DirEnt test suite something it ought to do.
 *
 * fail to compile
 *   This means d_type is undefined.  It should be enough to test for
 *   HAVE_STRUCT_DIRENT_D_TYPE, but this test suite came about because of
 *   https://github.com/haskell/unix/issues/347 which was caused by a
 *   misspelling of that macro, so we're testing it another way.
 *   Absence of d_type means the test suite ought to skip testing for non-
 *   DT_UNKNOWN values for d_type, since dirEntType returns UnknownType in this
 *   case.
 * exit with status 0
 *   This means the "." entry in the DIR stream opened at ".", guaranteed to be
 *   a directory, has a d_type of DT_DIR.  We should proceed with a test for it
 *   in Haskell.
 * exit with status 1
 *   This means something unexpected went wrong.  Fail the Haskell test also.
 * exit with stauts 2
 *   This means the "." entry has a d_type of DT_UNKNOWN.  This is valid; no
 *   filesystem or operating system is required to yield a useful d_type.
 *   We should skip testing for non-DT_UNKNOWN values in Haskell.
 */

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
