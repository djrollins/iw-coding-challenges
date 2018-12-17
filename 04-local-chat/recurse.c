#include <stdlib.h> /* exit */
#include <stdio.h> /* printf */
#include <string.h> /* strlen */
#include <unistd.h> /* fork, execv */
#include <sys/wait.h> /* waitpid */

/* Counts down by recursively forking and re-running the program */
int main(int argc, char **argv) {

	if (argc == 1) {
		exit(-1);
	}

	if (strlen(argv[1]) > 5) {
		fputs("argument to long", stderr);
		exit(-1);
	}

	printf("%s: %s\n", argv[0], argv[1]);

	int arg = atoi(argv[1]);

	if (arg == 0) {
		return 0;
	}

	pid_t pid = fork();

	arg--;

	if (pid == 0) {
		char buffer[5];
		snprintf(buffer, sizeof(buffer), "%d", arg);
		argv[1] = buffer;

		execv(argv[0], argv);
		exit(-1); /* execv failed */
	} else {
		waitpid(pid, 0, 0);
	}

	return 0;
}
