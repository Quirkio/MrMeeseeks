#include <ctype.h>
#include <errno.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

#define MAX_REQUEST_LENGTH 1024
#define MAX_OPERANDS 256

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

typedef struct {
    double operands[MAX_OPERANDS];
    char operators[MAX_OPERANDS - 1];
    size_t count;
} ParsedExpression;

void introduce_meeseeks(const MeeseeksInfo *info);
void say_goodbye(const char *message);
int write_message(int fd, const TaskMessage *message);
int read_message(int fd, TaskMessage *message);
int handle_request(const TaskMessage *message);
int create_meeseeks(const TaskMessage *message, MeeseeksInfo info);
int read_line(const char *prompt, char *buffer, size_t capacity);

int parse_arithmetic(const char *text, ParsedExpression *parsed,
                     const char **error);
int parse_logical(const char *text, ParsedExpression *parsed,
                  const char **error);
int evaluate_arithmetic(const ParsedExpression *parsed, double *result,
                        const char **error);
int evaluate_logical(const ParsedExpression *parsed, int *result,
                     const char **error);

// Reads alternating numeric operands and arithmetic operators into parsed.
int parse_arithmetic(const char *text, ParsedExpression *parsed,
                     const char **error)
{
    const char *cursor = text;
    int expecting_operand = 1;

    parsed->count = 0;
    *error = NULL;
    while (1) {
        while (isspace((unsigned char)*cursor)) cursor++;
        if (*cursor == '\0') break;

        if (expecting_operand) {
            char *end;
            if (parsed->count == MAX_OPERANDS) {
                *error = "expression has too many operands";
                return -1;
            }
            errno = 0;
            double value = strtod(cursor, &end);
            if (end == cursor || errno == ERANGE || !isfinite(value)) {
                *error = "expected a finite number";
                return -1;
            }
            parsed->operands[parsed->count++] = value;
            cursor = end;
            expecting_operand = 0;
        } else {
            if (*cursor != '+' && *cursor != '-' &&
                *cursor != '*' && *cursor != '/') {
                *error = "expected +, -, *, or /";
                return -1;
            }
            parsed->operators[parsed->count - 1] = *cursor++;
            expecting_operand = 1;
        }
    }

    if (parsed->count == 0 || expecting_operand) {
        *error = "expression is incomplete";
        return -1;
    }
    return 0;
}

// Reads true/false operands and logical operators, applying unary ! to operands.
int parse_logical(const char *text, ParsedExpression *parsed,
                  const char **error)
{
    const char *cursor = text;
    int expecting_operand = 1;

    parsed->count = 0;
    *error = NULL;
    while (1) {
        while (isspace((unsigned char)*cursor)) cursor++;
        if (*cursor == '\0') break;

        if (expecting_operand) {
            int negate = 0;
            while (*cursor == '!') {
                negate = !negate;
                cursor++;
                while (isspace((unsigned char)*cursor)) cursor++;
            }
            int value;
            if (strncmp(cursor, "true", 4) == 0 &&
                !isalnum((unsigned char)cursor[4]) && cursor[4] != '_') {
                value = 1;
                cursor += 4;
            } else if (strncmp(cursor, "false", 5) == 0 &&
                       !isalnum((unsigned char)cursor[5]) && cursor[5] != '_') {
                value = 0;
                cursor += 5;
            } else {
                *error = "expected true or false";
                return -1;
            }
            if (parsed->count == MAX_OPERANDS) {
                *error = "expression has too many operands";
                return -1;
            }
            parsed->operands[parsed->count++] = negate ? !value : value;
            expecting_operand = 0;
        } else {
            if (strncmp(cursor, "&&", 2) == 0) {
                parsed->operators[parsed->count - 1] = '&';
                cursor += 2;
            } else if (strncmp(cursor, "||", 2) == 0) {
                parsed->operators[parsed->count - 1] = '|';
                cursor += 2;
            } else {
                *error = "expected && or ||";
                return -1;
            }
            expecting_operand = 1;
        }
    }

    if (parsed->count == 0 || expecting_operand) {
        *error = "expression is incomplete";
        return -1;
    }
    return 0;
}

// Evaluates parsed arithmetic with multiplication and division precedence.
int evaluate_arithmetic(const ParsedExpression *parsed, double *result,
                        const char **error)
{
    double total = 0.0;
    double term;

    *error = NULL;
    if (parsed->count == 0) {
        *error = "empty expression";
        return -1;
    }
    term = parsed->operands[0];
    for (size_t i = 0; i + 1 < parsed->count; i++) {
        double next = parsed->operands[i + 1];
        switch (parsed->operators[i]) {
        case '*':
            term *= next;
            break;
        case '/':
            if (next == 0.0) {
                *error = "division by zero";
                return -1;
            }
            term /= next;
            break;
        case '+':
        case '-':
            total += term;
            term = parsed->operators[i] == '+' ? next : -next;
            break;
        default:
            *error = "invalid arithmetic operator";
            return -1;
        }
        if (!isfinite(term) || !isfinite(total)) {
            *error = "arithmetic result is out of range";
            return -1;
        }
    }
    *result = total + term;
    if (!isfinite(*result)) {
        *error = "arithmetic result is out of range";
        return -1;
    }
    return 0;
}

// Evaluates parsed logic with && precedence over ||.
int evaluate_logical(const ParsedExpression *parsed, int *result,
                     const char **error)
{
    int group;
    int total;

    *error = NULL;
    if (parsed->count == 0) {
        *error = "empty expression";
        return -1;
    }
    group = parsed->operands[0] != 0.0;
    total = 0;
    for (size_t i = 0; i + 1 < parsed->count; i++) {
        int next = parsed->operands[i + 1] != 0.0;
        if (parsed->operators[i] == '&') {
            group = group && next;
        } else if (parsed->operators[i] == '|') {
            total = total || group;
            group = next;
        } else {
            *error = "invalid logical operator";
            return -1;
        }
    }
    *result = total || group;
    return 0;
}

// Announces this process identity and its level/instance to the user.
void introduce_meeseeks(const MeeseeksInfo *info)
{
    printf("Hi, I'm Mr. Meeseeks! Look at Meeeee. "
           "(pid=%ld, ppid=%ld, N=%u, i=%u)\n",
           (long)getpid(), (long)getppid(), info->level, info->instance);
    fflush(stdout);
}

// Prints this process's farewell, including its PID and PPID.
void say_goodbye(const char *message)
{
    printf("Mr. Meeseeks (pid=%ld, ppid=%ld): %s\n",
           (long)getpid(), (long)getppid(), message);
    fflush(stdout);
}

// Writes the complete request struct to the pipe, retrying interrupted writes.
int write_message(int fd, const TaskMessage *message)
{
    const char *data = (const char *)message;
    size_t remaining = sizeof(*message);
    while (remaining > 0) {
        ssize_t written = write(fd, data, remaining);
        if (written < 0) {
            if (errno == EINTR) continue;
            return -1;
        }
        data += written;
        remaining -= (size_t)written;
    }
    return 0;
}

// Reads one complete request struct from the pipe, retrying interrupted reads.
int read_message(int fd, TaskMessage *message)
{
    char *data = (char *)message;
    size_t remaining = sizeof(*message);
    while (remaining > 0) {
        ssize_t received = read(fd, data, remaining);
        if (received == 0) {
            errno = EPIPE;
            return -1;
        }
        if (received < 0) {
            if (errno == EINTR) continue;
            return -1;
        }
        data += received;
        remaining -= (size_t)received;
    }
    return 0;
}

// Runs in a Meeseeks process: prompts for expression type and text, then calls
// the matching parser/evaluator and reports either the result or an error.
int handle_request(const TaskMessage *message)
{
    char kind[32] = { 0 };
    char expression[MAX_REQUEST_LENGTH] = { 0 };
    ParsedExpression parsed;
    const char *error = NULL;
    int input_result;

    if (message->type != TASK_CALCULATION) {
        printf("Mr. Meeseeks (pid=%ld, ppid=%ld): Meeseeks not yet ready.\n",
               (long)getpid(), (long)getppid());
        return EXIT_FAILURE;
    }

    printf("Mr. Meeseeks (pid=%ld, ppid=%ld): "
           "Which expression should I evaluate: arithmetic or logical? (a/l): ",
           (long)getpid(), (long)getppid());
    input_result = read_line("", kind, sizeof(kind));
    if (input_result <= 0) {
        printf("Mr. Meeseeks (pid=%ld, ppid=%ld): "
               "I didn't receive an expression type.\n",
               (long)getpid(), (long)getppid());
        return EXIT_FAILURE;
    }
    if (strcmp(kind, "a") != 0 && strcmp(kind, "A") != 0 &&
        strcmp(kind, "l") != 0 && strcmp(kind, "L") != 0) {
        printf("Mr. Meeseeks (pid=%ld, ppid=%ld): "
               "Please choose 'a' for arithmetic or 'l' for logical.\n",
               (long)getpid(), (long)getppid());
        return EXIT_FAILURE;
    }

    const char *prompt = (kind[0] == 'a' || kind[0] == 'A')
        ? "Enter arithmetic (numbers, +, -, *, /; no parentheses): "
        : "Enter logical (true/false, !, &&, ||; no parentheses): ";
    printf("Mr. Meeseeks (pid=%ld, ppid=%ld): %s",
           (long)getpid(), (long)getppid(), prompt);
    input_result = read_line("", expression, sizeof(expression));
    if (input_result <= 0 || expression[0] == '\0') {
        printf("Mr. Meeseeks (pid=%ld, ppid=%ld): "
               "Please provide a non-empty expression within the length limit.\n",
               (long)getpid(), (long)getppid());
        return EXIT_FAILURE;
    }

    if (strcmp(kind, "a") == 0 || strcmp(kind, "A") == 0) {
        double result;
        if (parse_arithmetic(expression, &parsed, &error) < 0 ||
            evaluate_arithmetic(&parsed, &result, &error) < 0) {
            printf("Mr. Meeseeks (pid=%ld, ppid=%ld): "
                   "I couldn't evaluate \"%s\": %s.\n",
                   (long)getpid(), (long)getppid(), expression, error);
            return EXIT_FAILURE;
        }
        printf("Mr. Meeseeks (pid=%ld, ppid=%ld): "
               "The result of \"%s\" is %.15g.\n",
               (long)getpid(), (long)getppid(), expression, result);
    } else if (strcmp(kind, "l") == 0 || strcmp(kind, "L") == 0) {
        int result;
        if (parse_logical(expression, &parsed, &error) < 0 ||
            evaluate_logical(&parsed, &result, &error) < 0) {
            printf("Mr. Meeseeks (pid=%ld, ppid=%ld): "
                   "I couldn't evaluate \"%s\": %s.\n",
                   (long)getpid(), (long)getppid(), expression, error);
            return EXIT_FAILURE;
        }
        printf("Mr. Meeseeks (pid=%ld, ppid=%ld): "
               "The result of \"%s\" is %s.\n",
               (long)getpid(), (long)getppid(), expression,
               result ? "true" : "false");
    }
    return EXIT_SUCCESS;
}

// Creates a private request pipe and forks one Meeseeks. The parent sends the
// request and waits; the child reads it, handles it, says goodbye, and exits.
int create_meeseeks(const TaskMessage *message, MeeseeksInfo info)
{
    int request_pipe[2];
    pid_t child_pid;
    int status;

    if (pipe(request_pipe) == -1) {
        perror("Box: pipe");
        return -1;
    }
    child_pid = fork();
    if (child_pid == -1) {
        perror("Box: fork");
        close(request_pipe[0]);
        close(request_pipe[1]);
        return -1;
    }
    if (child_pid == 0) {
        TaskMessage child_message;
        close(request_pipe[1]);
        introduce_meeseeks(&info);
        if (read_message(request_pipe[0], &child_message) == -1) {
            fprintf(stderr, "Mr. Meeseeks (pid=%ld, ppid=%ld): "
                    "Couldn't receive the request: %s.\n",
                    (long)getpid(), (long)getppid(), strerror(errno));
            close(request_pipe[0]);
            say_goodbye("I couldn't complete the request. Goodbye!");
            _exit(EXIT_FAILURE);
        }
        close(request_pipe[0]);
        int result = handle_request(&child_message);
        say_goodbye(result == EXIT_SUCCESS
                    ? "Thanks for the task. I hope I helped!"
                    : "I couldn't complete the task. Goodbye!");
        _exit(result);
    }

    close(request_pipe[0]);
    if (write_message(request_pipe[1], message) == -1) {
        perror("Box: writing request to pipe");
        close(request_pipe[1]);
        (void)waitpid(child_pid, NULL, 0);
        return -1;
    }
    close(request_pipe[1]);
    while (waitpid(child_pid, &status, 0) == -1) {
        if (errno != EINTR) {
            perror("Box: waitpid");
            return -1;
        }
    }
    if (!WIFEXITED(status) || WEXITSTATUS(status) != EXIT_SUCCESS) {
        puts("Box: The request was not completed successfully.");
        return -1;
    }
    return 0;
}

// Prints a prompt and reads one bounded line; returns 0 at EOF and -1 if too long.
int read_line(const char *prompt, char *buffer, size_t capacity)
{
    size_t length;
    printf("%s", prompt);
    fflush(stdout);
    if (fgets(buffer, (int)capacity, stdin) == NULL) return 0;

    length = strlen(buffer);
    if (length > 0 && buffer[length - 1] == '\n') {
        buffer[length - 1] = '\0';
        return 1;
    }
    if (!feof(stdin)) {
        int character;
        while ((character = getchar()) != '\n' && character != EOF) {}
        return -1;
    }
    return 1;
}

// Displays the CLI menu and starts a Meeseeks for calculation requests.
int main(void)
{
    char choice[32];

    setvbuf(stdin, NULL, _IONBF, 0);
    puts("=== Mr. Meeseeks Box ===");

    for (;;) {
        puts("\nChoose a request type:");
        puts("  1) Text task");
        puts("  2) Calculation");
        puts("  3) External program");
        puts("  4) Quit");
        puts("  q) Quit anytime");

        if (read_line("Box> ", choice, sizeof(choice)) <= 0) break;
        if (strcmp(choice, "1") == 0 || strcmp(choice, "3") == 0) {
            puts("Meeseeks not yet ready.");
        } else if (strcmp(choice, "2") == 0) {
            TaskMessage message = { .type = TASK_CALCULATION };
            (void)create_meeseeks(&message,
                                  (MeeseeksInfo){ .level = 1, .instance = 1 });
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
