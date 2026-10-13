/*
Instituto Tecnológico de Costa Rica
Escuela de Ingeniería en Computación

IC6600 Sistemas Operativos - Proferos Esteban Arias
Tarea #3

José Ignacio Madriz Ortiz
Carnet: 2024089190
*/

#define _DEFAULT_SOURCE
#define _POSIX_C_SOURCE 200809L

#include <ctype.h>
#include <errno.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <semaphore.h>
#include <time.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

#define MAX_REQUEST_LENGTH 1024
#define MAX_OPERANDS 256
#define TEXT_TASK_TIMEOUT_SECONDS 300
#define TEXT_TASK_PROCESS_LIMIT 128
#define TEXT_TASK_MAX_DEPTH 8
#define DEFAULT_SIMULATION_MIN_SECONDS 0.5
#define DEFAULT_SIMULATION_MAX_SECONDS 5.0

typedef enum {
    TASK_TEXT = 1,
    TASK_CALCULATION,
    TASK_EXTERNAL_PROGRAM
} TaskType;

typedef struct {
    TaskType type;
    char request[MAX_REQUEST_LENGTH];
    double difficulty;
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

typedef struct {
    sem_t mutex;
    struct timespec deadline;
    unsigned int created_count;
    int stop_reason;
    double progress_percent;
    pid_t winner_pid;
    pid_t winner_ppid;
    unsigned int winner_level;
    unsigned int winner_instance;
} SharedTaskState;

enum {
    TASK_RUNNING,
    TASK_SOLVED,
    TASK_TIMED_OUT,
    TASK_PROCESS_LIMIT,
    TASK_IMPOSSIBLE
};

static double simulation_min_seconds = DEFAULT_SIMULATION_MIN_SECONDS;
static double simulation_max_seconds = DEFAULT_SIMULATION_MAX_SECONDS;
static struct timespec box_deadline;
static volatile sig_atomic_t box_process_group = -1;

void introduce_meeseeks(const MeeseeksInfo *info);
void say_goodbye(const char *message);
int write_message(int fd, const TaskMessage *message);
int read_message(int fd, TaskMessage *message);
int handle_request(const TaskMessage *message);
int create_meeseeks(const TaskMessage *message, MeeseeksInfo info,
                    SharedTaskState *shared);
int read_line(const char *prompt, char *buffer, size_t capacity);

int parse_arithmetic(const char *text, ParsedExpression *parsed,
                     const char **error);
int parse_logical(const char *text, ParsedExpression *parsed,
                  const char **error);
int evaluate_arithmetic(const ParsedExpression *parsed, double *result,
                        const char **error);
int evaluate_logical(const ParsedExpression *parsed, int *result,
                     const char **error);
int run_text_task(const TaskMessage *message, MeeseeksInfo info,
                  SharedTaskState *shared);

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

// Acquires the process-shared lock, retrying if a signal interrupts sem_wait.
static int lock_task_state(SharedTaskState *shared)
{
    while (sem_wait(&shared->mutex) == -1) {
        if (errno != EINTR) {
            perror("Meeseeks: sem_wait");
            return -1;
        }
    }
    return 0;
}

// Releases the lock protecting counters, deadline state, and the winner record.
static int unlock_task_state(SharedTaskState *shared)
{
    if (sem_post(&shared->mutex) == -1) {
        perror("Meeseeks: sem_post");
        return -1;
    }
    return 0;
}

// Checks shared cancellation state and marks the request timed out at its deadline.
static int task_stop_reason(SharedTaskState *shared)
{
    struct timespec now;
    int reason;

    if (clock_gettime(CLOCK_MONOTONIC, &now) == -1) {
        perror("Meeseeks: clock_gettime");
        if (lock_task_state(shared) == 0) {
            if (shared->stop_reason == TASK_RUNNING)
                shared->stop_reason = TASK_TIMED_OUT;
            (void)unlock_task_state(shared);
        }
        return TASK_TIMED_OUT;
    }
    if (lock_task_state(shared) == -1) return TASK_TIMED_OUT;
    if (shared->stop_reason == TASK_RUNNING &&
        (now.tv_sec > shared->deadline.tv_sec ||
         (now.tv_sec == shared->deadline.tv_sec &&
          now.tv_nsec >= shared->deadline.tv_nsec))) {
        shared->stop_reason = TASK_TIMED_OUT;
    }
    reason = shared->stop_reason;
    (void)unlock_task_state(shared);
    return reason;
}

// Reserves a slot for a fork or stops the request when the process cap is reached.
static int reserve_process_slot(SharedTaskState *shared)
{
    int reserved = 0;

    if (lock_task_state(shared) == -1) return -1;
    if (shared->stop_reason == TASK_RUNNING &&
        shared->created_count < TEXT_TASK_PROCESS_LIMIT) {
        shared->created_count++;
        reserved = 1;
    } else if (shared->stop_reason == TASK_RUNNING) {
        shared->stop_reason = TASK_PROCESS_LIMIT;
    }
    (void)unlock_task_state(shared);
    return reserved;
}

// Publishes the first successful worker as the result for the whole process tree.
static void publish_solution(SharedTaskState *shared, const MeeseeksInfo *info)
{
    struct timespec now;
    int clock_ok = clock_gettime(CLOCK_MONOTONIC, &now) == 0;

    if (!clock_ok) perror("Meeseeks: clock_gettime");
    if (lock_task_state(shared) == -1) return;
    if (shared->stop_reason == TASK_RUNNING && clock_ok &&
        (now.tv_sec < shared->deadline.tv_sec ||
         (now.tv_sec == shared->deadline.tv_sec &&
          now.tv_nsec < shared->deadline.tv_nsec))) {
        shared->stop_reason = TASK_SOLVED;
        shared->winner_pid = getpid();
        shared->winner_ppid = getppid();
        shared->winner_level = info->level;
        shared->winner_instance = info->instance;
    } else if (shared->stop_reason == TASK_RUNNING) {
        shared->stop_reason = TASK_TIMED_OUT;
    }
    (void)unlock_task_state(shared);
}

// Adds this worker's contribution and marks the task complete if shared progress reaches 100%.
static int record_task_progress(SharedTaskState *shared, const MeeseeksInfo *info,
                               double contribution, double *total_progress)
{
    int completed = 0;

    if (lock_task_state(shared) == -1) return -1;
    if (shared->stop_reason == TASK_RUNNING) {
        shared->progress_percent += contribution;
        if (shared->progress_percent > 100.0) shared->progress_percent = 100.0;
        *total_progress = shared->progress_percent;
        if (shared->progress_percent >= 100.0) {
            shared->stop_reason = TASK_SOLVED;
            shared->winner_pid = getpid();
            shared->winner_ppid = getppid();
            shared->winner_level = info->level;
            shared->winner_instance = info->instance;
            completed = 1;
        }
    } else {
        *total_progress = shared->progress_percent;
    }
    (void)unlock_task_state(shared);
    return completed;
}

// Distinguishes the solver's farewell from helpers cancelled after a sibling wins.
static const char *text_task_farewell(SharedTaskState *shared, int result)
{
    int another_worker_won = 0;
    if (lock_task_state(shared) == 0) {
        another_worker_won = shared->stop_reason == TASK_SOLVED &&
                             shared->winner_pid != getpid();
        (void)unlock_task_state(shared);
    }
    if (another_worker_won) return "Another Meeseeks completed the task. Goodbye!";
    return result == EXIT_SUCCESS
        ? "Thanks for the task. I'm glad I could help!"
        : "I couldn't complete the task. Goodbye!";
}

// Returns a uniformly distributed random percentage in [0, 100).
static double random_percentage(void)
{
    return 100.0 * (double)rand() / ((double)RAND_MAX + 1.0);
}

// Terminates the isolated Box process group when its universal countdown expires.
static void planetary_chaos_handler(int signal_number)
{
    static const char message[] =
        "\nBox: Planetary chaos! The five-minute limit expired; terminating Box and all Meeseeks.\n";
    (void)signal_number;
    (void)write(STDERR_FILENO, message, sizeof(message) - 1);
    if (box_process_group > 0) (void)kill(-box_process_group, SIGKILL);
    _exit(128 + SIGALRM);
}

// Prints the monotonic-time countdown remaining before planetary chaos.
static void print_time_remaining(void)
{
    struct timespec now;
    if (clock_gettime(CLOCK_MONOTONIC, &now) == -1) {
        perror("Box: clock_gettime");
        return;
    }

    double remaining = (double)(box_deadline.tv_sec - now.tv_sec) +
        (double)(box_deadline.tv_nsec - now.tv_nsec) / 1000000000.0;
    if (remaining < 0.0) remaining = 0.0;
    unsigned long seconds = (unsigned long)remaining;
    if ((double)seconds < remaining) seconds++;
    printf("Time until planetary chaos: %lu:%02lu remaining.\n",
           seconds / 60, seconds % 60);
}

// Seeds each process independently so forked workers do not share random outcomes.
static void seed_random_generator(void)
{
    struct timespec now;
    unsigned int seed = (unsigned int)getpid();
    if (clock_gettime(CLOCK_MONOTONIC, &now) == 0)
        seed ^= (unsigned int)now.tv_nsec;
    srand(seed);
}

// Waits for a configured simulated duration and reports actual elapsed wall time.
static int simulate_task_time(SharedTaskState *shared, double *elapsed_seconds)
{
    struct timespec now;
    struct timespec start;
    struct timespec finish;
    double delay = simulation_min_seconds +
        random_percentage() / 100.0 *
        (simulation_max_seconds - simulation_min_seconds);

    if (clock_gettime(CLOCK_MONOTONIC, &now) == -1) {
        perror("Meeseeks: clock_gettime");
        return -1;
    }
    start = now;
    finish.tv_sec = now.tv_sec + (time_t)delay;
    finish.tv_nsec = now.tv_nsec +
        (long)((delay - (double)(time_t)delay) * 1000000000.0);
    if (finish.tv_nsec >= 1000000000L) {
        finish.tv_sec++;
        finish.tv_nsec -= 1000000000L;
    }

    while (task_stop_reason(shared) == TASK_RUNNING) {
        struct timespec remaining;
        if (clock_gettime(CLOCK_MONOTONIC, &now) == -1) {
            perror("Meeseeks: clock_gettime");
            return -1;
        }
        if (now.tv_sec > finish.tv_sec ||
            (now.tv_sec == finish.tv_sec && now.tv_nsec >= finish.tv_nsec)) {
            *elapsed_seconds = (double)(now.tv_sec - start.tv_sec) +
                (double)(now.tv_nsec - start.tv_nsec) / 1000000000.0;
            return 0;
        }
        remaining.tv_sec = finish.tv_sec - now.tv_sec;
        remaining.tv_nsec = finish.tv_nsec - now.tv_nsec;
        if (remaining.tv_nsec < 0) {
            remaining.tv_sec--;
            remaining.tv_nsec += 1000000000L;
        }
        if (remaining.tv_sec > 0 || remaining.tv_nsec > 100000000L) {
            remaining.tv_sec = 0;
            remaining.tv_nsec = 100000000L;
        }
        while (nanosleep(&remaining, &remaining) == -1 && errno == EINTR) {
        }
    }
    if (clock_gettime(CLOCK_MONOTONIC, &now) == -1) {
        perror("Meeseeks: clock_gettime");
        return -1;
    }
    *elapsed_seconds = (double)(now.tv_sec - start.tv_sec) +
        (double)(now.tv_nsec - start.tv_nsec) / 1000000000.0;
    return 1;
}

// Forks a child, gives it the task through a private pipe, and returns its PID.
static pid_t spawn_text_child(const TaskMessage *message, MeeseeksInfo info,
                              SharedTaskState *shared)
{
    int request_pipe[2];
    pid_t child_pid;
    int reserved = reserve_process_slot(shared);

    if (reserved <= 0) return -1;
    if (pipe(request_pipe) == -1) {
        perror("Meeseeks: pipe");
        if (lock_task_state(shared) == 0) {
            if (shared->created_count > 0) shared->created_count--;
            if (shared->stop_reason == TASK_RUNNING)
                shared->stop_reason = TASK_PROCESS_LIMIT;
            (void)unlock_task_state(shared);
        }
        return -1;
    }
    child_pid = fork();
    if (child_pid == -1) {
        perror("Meeseeks: fork");
        close(request_pipe[0]);
        close(request_pipe[1]);
        if (lock_task_state(shared) == 0) {
            if (shared->created_count > 0) shared->created_count--;
            if (shared->stop_reason == TASK_RUNNING)
                shared->stop_reason = TASK_PROCESS_LIMIT;
            (void)unlock_task_state(shared);
        }
        return -1;
    }
    if (child_pid == 0) {
        TaskMessage child_message;
        int result;

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
        result = run_text_task(&child_message, info, shared);
        say_goodbye(text_task_farewell(shared, result));
        _exit(result);
    }

    close(request_pipe[0]);
    if (write_message(request_pipe[1], message) == -1) {
        perror("Meeseeks: writing child request");
        close(request_pipe[1]);
        (void)waitpid(child_pid, NULL, 0);
        if (lock_task_state(shared) == 0) {
            if (shared->stop_reason == TASK_RUNNING)
                shared->stop_reason = TASK_PROCESS_LIMIT;
            (void)unlock_task_state(shared);
        }
        return -1;
    }
    close(request_pipe[1]);
    return child_pid;
}

// Attempts the task, forks difficulty-based helpers, and propagates any solution upward.
int run_text_task(const TaskMessage *message, MeeseeksInfo info,
                  SharedTaskState *shared)
{
    pid_t children[5];
    size_t child_count = 0;
    int reason = task_stop_reason(shared);

    if (reason != TASK_RUNNING) return reason == TASK_SOLVED ? EXIT_SUCCESS : EXIT_FAILURE;
    seed_random_generator();
    if (message->difficulty <= 0.0) {
        if (lock_task_state(shared) == 0) {
            if (shared->stop_reason == TASK_RUNNING)
                shared->stop_reason = TASK_IMPOSSIBLE;
            (void)unlock_task_state(shared);
        }
        printf("Mr. Meeseeks (pid=%ld, ppid=%ld): A difficulty of 0 is impossible.\n",
               (long)getpid(), (long)getppid());
        return EXIT_FAILURE;
    }

    if (message->difficulty <= 85.0) {
        child_count = message->difficulty <= 45.0 ? 5 : 2;
        if (info.level >= TEXT_TASK_MAX_DEPTH) {
            if (lock_task_state(shared) == 0) {
                if (shared->stop_reason == TASK_RUNNING)
                    shared->stop_reason = TASK_PROCESS_LIMIT;
                (void)unlock_task_state(shared);
            }
            return EXIT_FAILURE;
        }
    }

    for (;;) {
        size_t spawned_count = 0;

        reason = task_stop_reason(shared);
        if (reason != TASK_RUNNING)
            return reason == TASK_SOLVED ? EXIT_SUCCESS : EXIT_FAILURE;

        if (child_count > 0) {
            printf("Mr. Meeseeks (pid=%ld, ppid=%ld): "
                   "Difficulty %.2f requires %zu helpers; creating this wave now.\n",
                   (long)getpid(), (long)getppid(), message->difficulty, child_count);
            for (size_t i = 0; i < child_count; i++) {
                TaskMessage child_message = *message;
                MeeseeksInfo child_info = {
                    .level = info.level + 1,
                    .instance = (unsigned int)i + 1
                };
                if (task_stop_reason(shared) != TASK_RUNNING) break;

                child_message.difficulty = message->difficulty +
                    (100.0 - message->difficulty) * 0.35;
                pid_t child_pid = spawn_text_child(&child_message, child_info, shared);
                if (child_pid < 0) break;
                children[spawned_count++] = child_pid;
            }

            for (size_t i = 0; i < spawned_count; i++) {
                int status;
                while (waitpid(children[i], &status, 0) == -1) {
                    if (errno != EINTR) {
                        perror("Meeseeks: waitpid");
                        break;
                    }
                }
            }

            reason = task_stop_reason(shared);
            if (reason != TASK_RUNNING)
                return reason == TASK_SOLVED ? EXIT_SUCCESS : EXIT_FAILURE;
            if (spawned_count < child_count) return EXIT_FAILURE;

            if (lock_task_state(shared) == 0) {
                printf("Mr. Meeseeks (pid=%ld, ppid=%ld): "
                       "The wave is reaped; total progress is %.2f%% and the task "
                       "remains. I will try next; if I cannot finish it, I will "
                       "create more Meeseeks.\n",
                       (long)getpid(), (long)getppid(), shared->progress_percent);
                (void)unlock_task_state(shared);
            }
        }

        printf("Mr. Meeseeks (pid=%ld, ppid=%ld): Working on \"%s\" "
               "(difficulty %.4f).\n",
               (long)getpid(), (long)getppid(), message->request, message->difficulty);
        double attempt_seconds = 0.0;
        int wait_result = simulate_task_time(shared, &attempt_seconds);
        if (wait_result != 0) {
            printf("Mr. Meeseeks (pid=%ld, ppid=%ld): "
                   "This attempt took %.3f seconds.\n",
                   (long)getpid(), (long)getppid(), attempt_seconds);
            reason = task_stop_reason(shared);
            return reason == TASK_SOLVED ? EXIT_SUCCESS : EXIT_FAILURE;
        }
        if (random_percentage() < message->difficulty) {
            printf("Mr. Meeseeks (pid=%ld, ppid=%ld): "
                   "I completed my attempt in %.3f seconds.\n",
                   (long)getpid(), (long)getppid(), attempt_seconds);
            publish_solution(shared, &info);
            reason = task_stop_reason(shared);
            return reason == TASK_SOLVED ? EXIT_SUCCESS : EXIT_FAILURE;
        }

        double contribution = 1.0 + message->difficulty * 0.15 *
            random_percentage() / 100.0;
        double total_progress = 0.0;
        int completed = record_task_progress(shared, &info, contribution,
                                             &total_progress);
        if (completed < 0) return EXIT_FAILURE;
        printf("Mr. Meeseeks (pid=%ld, ppid=%ld): I couldn't finish it, "
               "but I completed %.2f%% of it (total progress: %.2f%%) "
               "in %.3f seconds.\n",
               (long)getpid(), (long)getppid(), contribution, total_progress,
               attempt_seconds);
        if (completed > 0) return EXIT_SUCCESS;

        reason = task_stop_reason(shared);
        if (reason != TASK_RUNNING)
            return reason == TASK_SOLVED ? EXIT_SUCCESS : EXIT_FAILURE;
        if (child_count == 0) return EXIT_FAILURE;
        printf("Mr. Meeseeks (pid=%ld, ppid=%ld): I couldn't finish it either; "
               "I'll ask for another wave of %zu Meeseeks.\n",
               (long)getpid(), (long)getppid(), child_count);
    }
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

// Runs in a Meeseeks process prompting for expression type and text, then calls
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

// Creates the root Meeseeks over a private pipe; text-task descendants share
// semaphore-protected stop/winner state, while Box waits and prints the result.
int create_meeseeks(const TaskMessage *message, MeeseeksInfo info,
                    SharedTaskState *shared)
{
    int request_pipe[2];
    pid_t child_pid;
    int status;

    if (shared != NULL && reserve_process_slot(shared) <= 0) {
        puts("Box: The request could not start within the process limit.");
        return -1;
    }
    if (pipe(request_pipe) == -1) {
        perror("Box: pipe");
        if (shared != NULL && lock_task_state(shared) == 0) {
            if (shared->created_count > 0) shared->created_count--;
            (void)unlock_task_state(shared);
        }
        return -1;
    }
    child_pid = fork();
    if (child_pid == -1) {
        perror("Box: fork");
        close(request_pipe[0]);
        close(request_pipe[1]);
        if (shared != NULL && lock_task_state(shared) == 0) {
            if (shared->created_count > 0) shared->created_count--;
            (void)unlock_task_state(shared);
        }
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
        int result = child_message.type == TASK_TEXT
            ? run_text_task(&child_message, info, shared)
            : handle_request(&child_message);
        if (child_message.type == TASK_TEXT) {
            say_goodbye(text_task_farewell(shared, result));
        } else {
            say_goodbye(result == EXIT_SUCCESS
                        ? "Thanks for the calculation. I hope I helped!"
                        : "I couldn't complete the calculation. Goodbye!");
        }
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
    if (message->type == TASK_TEXT && shared != NULL) {
        int reason = task_stop_reason(shared);
        if (reason == TASK_SOLVED) {
            printf("Box: Task completed by Meeseeks (pid=%ld, ppid=%ld, N=%u, i=%u).\n",
                   (long)shared->winner_pid, (long)shared->winner_ppid,
                   shared->winner_level, shared->winner_instance);
        } else if (reason == TASK_IMPOSSIBLE) {
            puts("Box: Difficulty 0 is impossible; no helper processes were created.");
        } else if (reason == TASK_TIMED_OUT) {
            printf("Box: Planetary chaos avoided. The %d-second limit expired.\n",
                   TEXT_TASK_TIMEOUT_SECONDS);
        } else if (reason == TASK_PROCESS_LIMIT) {
            puts("Box: The safety limit stopped further Meeseeks creation.");
        } else {
            puts("Box: The task could not be completed.");
        }
        printf("Box: Meeseeks created for this task: %u (maximum %d).\n",
               shared->created_count, TEXT_TASK_PROCESS_LIMIT);
        return reason == TASK_SOLVED ? 0 : -1;
    }
    if (!WIFEXITED(status) || WEXITSTATUS(status) != EXIT_SUCCESS) {
        puts("Box: The request was not completed successfully.");
        return -1;
    }
    return 0;
}

// Prints a prompt and reads one bounded line. Returns 0 at EOF and -1 if too long.
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

// Reads and validates one nonnegative simulation duration in seconds.
static int read_simulation_seconds(const char *prompt, double *seconds)
{
    char input[64];
    char *end;
    if (read_line(prompt, input, sizeof(input)) <= 0) return -1;

    errno = 0;
    *seconds = strtod(input, &end);
    while (isspace((unsigned char)*end)) end++;
    if (end == input || *end != '\0' || errno == ERANGE ||
        !isfinite(*seconds) || *seconds < 0.0 ||
        *seconds > TEXT_TASK_TIMEOUT_SECONDS) {
        puts("Box: Enter a finite duration between 0 and 300 seconds.");
        return -1;
    }
    return 0;
}

// Lets the user set simulation bounds or restore the default 0.5-to-5-second range.
static void configure_simulation_time(void)
{
    char choice[32];
    for (;;) {
        puts("\nSimulation duration settings:");
        printf("Current range: %.3f to %.3f seconds.\n",
               simulation_min_seconds, simulation_max_seconds);
        print_time_remaining();
        puts("  1) Set minimum and maximum");
        puts("  2) Restore defaults (0.5 to 5 seconds)");
        puts("  3) Return to main menu");
        if (read_line("Box settings> ", choice, sizeof(choice)) <= 0) return;

        if (strcmp(choice, "1") == 0) {
            double minimum;
            double maximum;
            if (read_simulation_seconds("Minimum seconds: ", &minimum) < 0 ||
                read_simulation_seconds("Maximum seconds: ", &maximum) < 0) {
                continue;
            }
            if (minimum > maximum) {
                puts("Box: Minimum duration cannot exceed maximum duration.");
                continue;
            }
            simulation_min_seconds = minimum;
            simulation_max_seconds = maximum;
            printf("Box: Simulation duration set to %.3f-%.3f seconds.\n",
                   simulation_min_seconds, simulation_max_seconds);
        } else if (strcmp(choice, "2") == 0) {
            simulation_min_seconds = DEFAULT_SIMULATION_MIN_SECONDS;
            simulation_max_seconds = DEFAULT_SIMULATION_MAX_SECONDS;
            puts("Box: Simulation duration restored to 0.5-5 seconds.");
        } else if (strcmp(choice, "3") == 0 ||
                   strcmp(choice, "q") == 0 || strcmp(choice, "Q") == 0) {
            return;
        } else {
            puts("Box: Choose 1, 2, or 3.");
        }
    }
}

// Collects a text task and difficulty, initializes shared controls, and waits for its tree.
static void start_text_request(void)
{
    TaskMessage message = { .type = TASK_TEXT };
    char difficulty_mode[32];
    char difficulty_input[64];
    char *end;
    int input_result;

    input_result = read_line("Box: What should the Meeseeks do? ",
                             message.request, sizeof(message.request));
    if (input_result <= 0) {
        puts("Box: Couldn't read the task text.");
        return;
    }
    input_result = read_line("Set difficulty: 1) enter 0-100  2) random uniform: ",
                             difficulty_mode, sizeof(difficulty_mode));
    if (input_result <= 0) {
        puts("Box: Couldn't read the difficulty choice.");
        return;
    }
    if (strcmp(difficulty_mode, "1") == 0) {
        input_result = read_line("Box: Difficulty (0 = impossible, 100 = easiest): ",
                                 difficulty_input, sizeof(difficulty_input));
        if (input_result <= 0) {
            puts("Box: Couldn't read the difficulty value.");
            return;
        }
        errno = 0;
        message.difficulty = strtod(difficulty_input, &end);
        while (isspace((unsigned char)*end)) end++;
        if (end == difficulty_input || *end != '\0' || errno == ERANGE ||
            !isfinite(message.difficulty) || message.difficulty < 0.0 ||
            message.difficulty > 100.0) {
            puts("Box: Difficulty must be a number from 0 to 100.");
            return;
        }
    } else if (strcmp(difficulty_mode, "2") == 0) {
        message.difficulty = random_percentage();
        printf("Box: Uniformly selected difficulty %.4f.\n", message.difficulty);
    } else {
        puts("Box: Choose 1 for an entered difficulty or 2 for random.");
        return;
    }

    SharedTaskState *shared = mmap(NULL, sizeof(*shared),
                                   PROT_READ | PROT_WRITE,
                                   MAP_SHARED | MAP_ANONYMOUS, -1, 0);
    if (shared == MAP_FAILED) {
        perror("Box: mmap");
        return;
    }
    memset(shared, 0, sizeof(*shared));
    if (sem_init(&shared->mutex, 1, 1) == -1) {
        perror("Box: sem_init");
        (void)munmap(shared, sizeof(*shared));
        return;
    }
    shared->deadline = box_deadline;
    shared->stop_reason = TASK_RUNNING;
    printf("Box: Simulation duration is %.3f-%.3f seconds; this request shares "
           "the Box five-minute deadline and has a %d-process safety cap.\n",
           simulation_min_seconds, simulation_max_seconds,
           TEXT_TASK_PROCESS_LIMIT);

    (void)create_meeseeks(&message,
                          (MeeseeksInfo){ .level = 1, .instance = 1 }, shared);
    if (sem_destroy(&shared->mutex) == -1) perror("Box: sem_destroy");
    if (munmap(shared, sizeof(*shared)) == -1) perror("Box: munmap");
}

// Displays the CLI menu and starts Meeseeks for text-task and calculation requests.
int main(void)
{
    char choice[32];
    struct sigaction chaos_action = { 0 };

    setvbuf(stdin, NULL, _IONBF, 0);
    setvbuf(stdout, NULL, _IONBF, 0);
    if (setpgid(0, 0) == -1 && getpgrp() != getpid()) {
        perror("Box: couldn't create an isolated process group");
        return EXIT_FAILURE;
    }
    box_process_group = (sig_atomic_t)getpgrp();
    if (clock_gettime(CLOCK_MONOTONIC, &box_deadline) == -1) {
        perror("Box: clock_gettime");
        return EXIT_FAILURE;
    }
    box_deadline.tv_sec += TEXT_TASK_TIMEOUT_SECONDS;
    chaos_action.sa_handler = planetary_chaos_handler;
    sigemptyset(&chaos_action.sa_mask);
    if (sigaction(SIGALRM, &chaos_action, NULL) == -1) {
        perror("Box: sigaction");
        return EXIT_FAILURE;
    }
    alarm(TEXT_TASK_TIMEOUT_SECONDS);
    seed_random_generator();
    puts("=== Mr. Meeseeks Box ===");

    for (;;) {
        puts("\nChoose a request type:");
        puts("  1) Text task");
        puts("  2) Calculation");
        puts("  3) External program");
        puts("  4) Quit");
        puts("  5) Configure simulation duration");
        puts("  q) Quit anytime");
        print_time_remaining();

        if (read_line("Box> ", choice, sizeof(choice)) <= 0) break;
        if (strcmp(choice, "1") == 0) {
            start_text_request();
        } else if (strcmp(choice, "3") == 0) {
            puts("Meeseeks not yet ready.");
        } else if (strcmp(choice, "2") == 0) {
            TaskMessage message = { .type = TASK_CALCULATION };
            (void)create_meeseeks(&message,
                                  (MeeseeksInfo){ .level = 1, .instance = 1 },
                                  NULL);
        } else if (strcmp(choice, "5") == 0) {
            configure_simulation_time();
        } else if (strcmp(choice, "4") == 0 ||
                   strcmp(choice, "q") == 0 || strcmp(choice, "Q") == 0) {
            break;
        } else {
            puts("Box: Please choose 1, 2, 3, 4, 5, or q.");
        }
    }

    puts("Box: Goodbye.");
    return EXIT_SUCCESS;
}
