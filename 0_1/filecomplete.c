#include <sys/types.h>
#include <dirent.h> // necessary for directory analysis
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

/* Check if string starts with substring */
bool contains(const char *a, const char *b) {
   if(strncmp(a, b, strlen(b)) == 0) return 1;
   return 0;
}

int main() {
  char folderName[100];
  char fileStart[100];
  const char *fileArray[100];
  int idx = 0;
  printf("Enter a folder name: \t");
  scanf("%[^\n]s", folderName);
  fseek(stdin, 0, SEEK_END);
  printf("Enter the beginning of a filename: ");
  scanf("%[^\n]s", fileStart);
  fseek(stdin, 0, SEEK_END);
  DIR *dir;
  struct dirent *files;
  if ((dir = opendir(folderName)) != NULL) {     // if pointer to directory exists
    while ((files = readdir (dir)) != NULL) {    // if there are still files to go through in directory
      if (contains(files->d_name, fileStart)) {  // if files start with substring
        fileArray[idx] = files->d_name;        // add file to array
        idx++;
      }
    }
    closedir(dir);
    for (int i = 0; i < idx; i++) printf("%s\n", fileArray[i]);
  } else { // problem opening directory
      perror("");
      return EXIT_FAILURE;
  }
  return 0;
}
