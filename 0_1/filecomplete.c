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
void addNode(char *data, struct node *head, struct node *current) {
   struct node *newNode = (struct node*) malloc(sizeof(struct node));
   newNode->data = data;
   newNode->next = NULL;
   if (head == NULL) { // first node
     head = newNode; // make head the new node
     printf("hmmm\n]\n");
     current = head; // make curIdx == head
   }
   current->next = newNode;  // else just add new node to current list after curIdx
}

/* traverse linked list and print the corresponding file names */
void printFiles(struct node *head_ptr) {
   struct node *cur_ptr = head_ptr;
   while(cur_ptr != NULL) {  // while not last node
     printf("%s\n", cur_ptr->data);
     cur_ptr = cur_ptr->next;
   }
}

/* clear array entries */
void clearArray(struct node *arr[26]) {
  for (int i = 0; i < 25; i++) {
    if (arr[i] != NULL) arr[i] = NULL; // reset head effectively resetting list
  }
}

/* Make array of length 26 with each index representing a letter set to NULL */
void makeLetterArray(struct node *arr[26]) {
  for (int i = 0; i < 25; i++) arr[i] = NULL;
}

/* reset data for when user enters new directory */
void resetData(struct node *head_ptr, struct node *cur_ptr, struct node *arr[26]) {
  head_ptr = NULL;
  cur_ptr = NULL;
  clearArray(arr);
  makeLetterArray(arr);
}

//////////////////////////////////
//// Bash Tab Simulation /////////
//////////////////////////////////

/* Return index by subtracting ASCII value of first char in d_name string */
int letterIndex(const char *file) {
  return ((int)toupper(file[0])) - 65;
}

/* Check if string starts with substring */
bool contains(const char *a, const char *b) {
   if(strncmp(a, b, strlen(b)) == 0) return 1;
   return 0;
}

int main() {
  struct node *head = NULL;
  struct node *curIdx = NULL;
  struct node *linkedListArray[26];
  char folderName[100];
  char fileStart[100];
  bool end = false;
  int letterIdx = 0;
  while (!end) {
    resetData(head, curIdx, linkedListArray);
    printf("Enter a folder name: \t");
    scanf("%[^\n]s", folderName);
    fseek(stdin, 0, SEEK_END);
    printf("Enter the beginning of a filename: ");
    scanf("%[^\n]s", fileStart);
    fseek(stdin, 0, SEEK_END);
    DIR *dir;
    struct dirent *files;
    letterIdx = letterIndex(folderName);
    head = linkedListArray[letterIdx];
    curIdx = head;
    if ((dir = opendir(folderName)) != NULL) {     // if pointer to directory exists
      while ((files = readdir (dir)) != NULL) {    // if there are still files to go through in directory
        if (contains(files->d_name, fileStart)) {  // if files start with substring
          addNode(files->d_name, head, curIdx);
          if (head == NULL) printf("orale wey\n\n\n");
        }
      }
      closedir(dir);
      for (struct node *i = linkedListArray[letterIdx]; i != NULL ; i = i->next) printf("%suytre\n", i->data);
    } else perror("No such directory exists, please enter a valid path:\n"); // problem opening directory
  }
  return 0;
}
