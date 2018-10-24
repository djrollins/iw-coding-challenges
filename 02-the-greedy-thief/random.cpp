#include <random>
#include <iostream>

int main()
{
	std::random_device device{};
	std::mt19937 gen{device()};
	std::uniform_int_distribution<> dis{10, 50};

	for (int i = 0; i < 3000; ++i) {
		std::cout << " " << dis(gen);
	}
}
