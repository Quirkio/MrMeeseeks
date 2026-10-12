#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <unistd.h>

#define MAX_REQUEST_LENGTH 1024

typedef enum {
    TASK_TEXT = 1,
    TASK_CALCULATION,
    TASK_EXTERNAL_PROGRAM
} TaskType;

typedef struct {
    TaskType type;
    char request[MAX_REQUEST_LENGTH];
} TaskMessage;

typedef struct {
    unsigned int level;
    unsigned int instance;
} MeeseeksInfo;

void introduce_meeseeks(const MeeseeksInfo *info);
void say_goodbye(const char *message);

int write_message(int fd, const TaskMessage *message); //Uses IPC
int read_message(int fd, TaskMessage *message); //Uses IPC
void handle_request(const TaskMessage *message);
int create_meeseeks(const TaskMessage *message, MeeseeksInfo info);
int read_line(const char *prompt, char *buffer, size_t capacity);
int submit_request(TaskType type, const char *prompt);

void introduce_meeseeks(const MeeseeksInfo *info)
{
    printf("Hi, I'm Mr. Meeseeks! Look at Meeeee. "
           "(pid=%ld, ppid=%ld, N=%u, i=%u)\n",
           (long)getpid(), (long)getppid(), info->level, info->instance);
    fflush(stdout);
}

void say_goodbye(const char *message)
{
    printf("Mr. Meeseeks (pid=%ld, ppid=%ld): %s\n",
           (long)getpid(), (long)getppid(), message);
    fflush(stdout);
}

int main(void)
{
    char choice[32];

    puts("=== Mr. Meeseeks Box ===");

    for (;;) {
        puts("\nChoose a request type:");
        puts("  1) Text task (simulation)");
        puts("  2) Arithmetic/logical calculation");
        puts("  3) Run an external program");
        puts("  4) Quit");
        puts("  q) Quit anytime");
        printf("Box> ");
        fflush(stdout);

        if (fgets(choice, sizeof(choice), stdin) == NULL) {
            break;
        }

        choice[strcspn(choice, "\n")] = '\0'; //Trimming, copilot said so :nod:

        if (strcmp(choice, "1") == 0) {
            puts("Meeseeks texto.");
        } else if (strcmp(choice, "2") == 0) {
            puts("67.");
        } else if (strcmp(choice, "3") == 0) {
            puts("Meeseeks otros.");
        } else if (strcmp(choice, "4") == 0 ||
                   strcmp(choice, "q") == 0 || strcmp(choice, "Q") == 0) {
            break;
        } else {
            puts("Box: Please choose 1, 2, 3, 4, or q.");
        }
    }

    puts("Box: Goodbye. [resumen de la sesión]");
    return EXIT_SUCCESS;
}
