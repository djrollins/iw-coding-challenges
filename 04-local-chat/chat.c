#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>

#include <sys/select.h>
#include <sys/socket.h>
#include <sys/un.h>

void die(const char *msg) {
	perror(msg);
	exit(-1);
}

bool server_is_up() {
	const int fd = open("/tmp/server_status.lock", O_RDWR | O_CREAT, 0666);

	if (fd == -1) {
		die("unable to open server_status.lock");
	}
	
	struct flock file_lock = {
		.l_type = F_RDLCK,
		.l_whence = SEEK_SET,
		.l_start = 0,
		.l_len = 1
	};

	if (fcntl(fd, F_SETLKW, &file_lock) == 0) {
		char val;
		int err = read(fd, &val, 1);

		file_lock.l_type = F_UNLCK;

		fcntl(fd, F_SETLKW, &file_lock);
		close(fd);

		return !err && val == '1';
	}

	die("unable to read server status file");
}

void do_run_server() {

}

void run_server() {
	const int fd = open("/tmp/server_status.lock", O_RDWR | O_CREAT, 0666);

	if (fd == -1) {
		die("unable to open server_status.lock");
	}
	
	struct flock file_lock = {
		.l_type = F_WRLCK,
		.l_whence = SEEK_SET,
		.l_start = 0,
		.l_len = 1
	};

	if (fcntl(fd, F_SETLKW, &file_lock) == 0) {
		char val;
		int err = read(fd, &val, 1);

		file_lock.l_type = F_UNLCK;

		if (err < 0) {
			fcntl(fd, F_SETLKW, &file_lock);
			close(fd);
			die("unable to read server status file");
		}

		if (val == '1') {
			puts("server running");
			fcntl(fd, F_SETLKW, &file_lock);
			close(fd);
			return;
		}
 
		puts("starting server...");
		write(fd, "1", 1);

		fcntl(fd, F_SETLKW, &file_lock);
		close(fd);
	}
}

int main(int argc, char **argv) {
	if (!server_is_up()) {
		run_server();
	} else {
	}

	die("foo");

	const int client_fd = socket(AF_UNIX, SOCK_STREAM, 0);

	if (client_fd == -1) {
		die("unable to create socket");
	}

	struct sockaddr_un addr = {
		.sun_family = AF_UNIX,
		.sun_path = "/tmp/local-chat.sock"
	};

	struct sockaddr *addr_ptr = (struct sockaddr*)&addr;

	if (connect(client_fd, addr_ptr, sizeof(addr)) == -1) {
		die("unable to connect to socket");
	}

	fd_set fds;
	FD_ZERO(&fds);
	FD_SET(0, &fds);
	FD_SET(client_fd, &fds);

	int result;

	while ((result = select(2, &fds, NULL, NULL, NULL)) > 0) {
		if(FD_ISSET(0, &fds)) {
			puts("read from stdin");
		} else if (FD_ISSET(client_fd, &fds)) {
			puts("read from socket");
		}
	}
	
	if (result) {
		die("error reading from socket or stdin");
	} else {
		puts("we're donezo bois");
	}
} 
