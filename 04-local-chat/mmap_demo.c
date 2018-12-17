#include <fcntl.h>
#include <stdio.h>
#include <sys/mman.h>

int main() {
	int fd = open("./file.txt", O_RDWR);

	char *file = mmap(
		NULL,
		12,
		PROT_READ | PROT_WRITE,
		MAP_SHARED,
		fd,
		0);

	printf("%s\n", file);

	file[0] = 'h';

	pause();

	munmap(file, 12);
}
