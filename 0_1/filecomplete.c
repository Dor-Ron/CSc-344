#include <sys/types.h>
#include <dirent.h> // necessary for directory analysis
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main() {
	char folder_name[100];
	printf("Enter file directory: \t");
	scanf("%[^\n]s", folder_name);
	printf("%s", folder_name);
	DIR *dir;
	struct dirent *files;
	if ((dir = opendir(folder_name)) != NULL) { // if pointer to directory exists
  	while ((files = readdir (dir)) != NULL) { // print each file in directory while pointer exists to file
    		printf("%s\n", files->d_name);
  		} closedir(dir);
		} else { // problem opening directory
  		perror("");
	  	return EXIT_FAILURE;
	}
	return 0;
}
