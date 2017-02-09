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
struct node* addNode(char *data, struct node *head, struct node *current) {
   struct node *newNode = (struct node*) malloc(sizeof(struct node));
   newNode->data = data;
   newNode->next = NULL;
   if (head == NULL) head = current = newNode; // first link
   else {    // any other link
     current->next = newNode;
     current = newNode;
   }
   return newNode;
}

/* traverse linked list and print the corresponding file names */
void printFiles(struct node *head_ptr, char *dirName, char *startString) {
   printf("\nFiles beginning with %s in %s:\n", startString, dirName);
   for (struct node *i = head_ptr; i != NULL ; i = i->next) printf("%s\n", i->data);
}

/* recursively clear linked list and avoid dangling pointers */
void clearList(struct node *head_ptr) {
  for (struct node *i = head_ptr; i != NULL ; i = i->next) {
    if (i->next != NULL) {
      clearList(i->next);
      i->next = NULL;
    }
    free(i);
  }
}

/* clear array entries */
void clearArray(struct node *arr[26]) {
  for (int i = 0; i < 25; i++) {
    if (arr[i] != NULL) clearList(arr[i]);
  }
}

/* Make array of length 26 with each index representing a letter set to NULL */
void makeLetterArray(struct node *arr[26]) {
  for (int i = 0; i < 25; i++) arr[i] = NULL;
}

/* reset data for when user enters new directory */
void resetData(struct node *arr[26]) {
  makeLetterArray(arr);
  clearArray(arr);
}

//////////////////////////////////
//// Bash Tab Simulation /////////
//////////////////////////////////

/* Return index by subtracting ASCII value of first char in d_name string */
int letterIndex(const char *file) {
  return ((int)toupper(file[0])) - 65;
}

/* Check if string starts with substring */
bool contains(const char *full, const char *start) {
   if(strncmp(full, start, strlen(start)) == 0) return true;
   return false;
}

int main() {
  struct node *linkedListArray[26];
  char folderName[100];
  char fileStart[100];
  bool end = false;
  int letterIdx = 0;
  while (!end) {
    resetData(linkedListArray);
    printf("\nEnter a folder name: ");
    scanf("%[^\n]s", folderName);
    fseek(stdin, 0, SEEK_END);
    printf("Enter the beginning of a filename: ");
    scanf("%[^\n]s", fileStart);
    fseek(stdin, 0, SEEK_END);
    if (!folderName[0]) end = true;
    struct node *head = linkedListArray[letterIdx];
    struct node *curIdx = head;
    DIR *dir;
    struct dirent *files;
    letterIdx = letterIndex(folderName);
    if ((dir = opendir(folderName)) != NULL) {     // if pointer to directory exists
      while ((files = readdir (dir)) != NULL) {    // if there are still files to go through in directory
        if (contains(files->d_name, fileStart)) {  // if files start with substring
          if (head == NULL) head = curIdx = addNode(files->d_name, head, curIdx);
          else curIdx = addNode(files->d_name, head, curIdx);
        }
      }
      closedir(dir);
      printFiles(head, folderName, fileStart);
    } else perror("No such directory exists, please enter a valid path:\n"); // problem opening directory
    memset(folderName, 0, strlen(folderName));
    memset(fileStart, 0, strlen(fileStart));
  }
  return 0;
}
