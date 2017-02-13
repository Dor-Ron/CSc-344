/*
   Course: CSc 344 - Programming Languages
   Assignment #: 1
   Instructor: Professor Daniel Schlegel
   Authors: [
      Dor Rondel,
      Nirender
   ]
*/

#include <sys/types.h>
#include <dirent.h> // necessary for directory analysis
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <stdbool.h>

//////////////////////////////////
// Linked List Implementation ////
//////////////////////////////////

struct node {
   const char *data;
   struct node *next;
};

/* Add nodes to linked list */
void addNode(char *data, struct node **node_ptr) {
   struct node *tmp_ptr;
   if (*node_ptr == NULL) {
      *node_ptr = (struct node *) malloc(sizeof(struct node)); // first link
      (*node_ptr)->data = data;
      (*node_ptr)->next = NULL;
   }
   else {    // any other link
     for (struct node *i = *node_ptr; i->next != NULL; i = i->next) node_ptr = &(i->next);
     (*node_ptr)->next = (struct node *) malloc(sizeof(struct node));
     tmp_ptr = (*node_ptr)->next;
     tmp_ptr->data = data;
     tmp_ptr->next = NULL;
   }
}

/* traverse linked list and print the corresponding file names */
void printFiles(struct node *head_ptr, char *dirName, char *startString) {
   printf("\nFiles beginning with %s in %s:\n", startString, dirName);
   for (struct node *i = head_ptr; i != NULL ; i = i->next) printf("%s\n", i->data);
}

/* recursively clear linked list and avoid dangling pointers */
void clearList(struct node **head_ptr) {
  for (struct node *i = *head_ptr; i != NULL ; i = i->next) {
    if (i->next != NULL) {
      clearList(&(i->next));
      i->next = NULL;
    }
    free(i);
  }
}

/* clear array entries */
void clearArray(struct node *arr[26]) {
  for (int i = 0; i < 25; i++) {
    if (arr[i] != NULL) clearList(&arr[i]);
  }
}

/* Make array of length 26 with each index representing a letter set to NULL */
void makeLetterArray(struct node **arr[26]) {
  for (int i = 0; i < 25; i++) (*arr)[i] = NULL;
}

/* reset data for when user enters new directory */
void resetData(struct node *arr[26]) {
  makeLetterArray(&arr);
  clearArray(arr);
}

//////////////////////////////////
//// Bash Tab Simulation /////////
//////////////////////////////////

/* Return index by subtracting ASCII value of first char in d_name string */
int letterIndex(const char *file) {
  return ((int)file[0]) - 97;
}

/* Check if string starts with substring */
bool contains(const char *full, const char *start) {
   if(strncmp(full, start, strlen(start)) == 0) return true;
   return false;
}

int main() {
  struct node *linkedListArray[26];
  struct node *tmp_head;
  char folderName[100];
  char fileStart[100];
  int letterIdx = 0;
  while (true) {
    resetData(linkedListArray);
    printf("\nEnter a folder name: ");
    scanf("%[^\n]s", folderName);
    fseek(stdin, 0, SEEK_END);
    printf("Enter the beginning of a filename: ");
    scanf("%[^\n]s", fileStart);
    fseek(stdin, 0, SEEK_END);
    if (!folderName[0]) break;
    DIR *dir;
    struct dirent *files;
    letterIdx = letterIndex(fileStart);
    if ((dir = opendir(folderName)) != NULL) {     // if pointer to directory exists
      while ((files = readdir(dir)) != NULL) {    // if there are still files to go through in directory
        if (letterIndex(files->d_name) > 0) addNode(files->d_name, &linkedListArray[letterIndex(files->d_name)]);
      }
      closedir(dir);
      tmp_head = linkedListArray[letterIdx];
      printFiles(tmp_head, folderName, fileStart);
    } else perror("No such directory exists, please enter a valid path:\n"); // problem opening directory
    memset(folderName, 0, strlen(folderName));
    memset(fileStart, 0, strlen(fileStart));
  }
  return 0;
}
