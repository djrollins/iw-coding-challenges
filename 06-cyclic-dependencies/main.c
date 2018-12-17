#include <stdint.h>

struct tree_node
{
	const char *name;
	struct tree_node *children
};

const char *example1 = "a,b\nb,c\nc,d\n";

size_t pos_of(const char *str, const char ch) {
	size_t pos;

	for (pos = 0; *str; ++str, ++pos) {
		if (*str == ch) {
			return pos;
		}
	}

	return pos;
}

int main()
{
	const char *input = example1;

	while (*input) {
		size_t line_end = pos_of(input, '\n');
		const char *first, *second;

		sscanf(input, 
	}
}
