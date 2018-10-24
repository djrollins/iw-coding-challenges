#include <chrono>
#include <iostream>
#include <vector>
#include <utility>
#include <algorithm>
#include <string>
#include <sstream>

namespace greedy_thief {

using id_t = int;
using weight_t = int;
using value_t = int;

struct item
{
    id_t id;
    weight_t weight;
    value_t value;
};

struct fit_result
{
    value_t total_value;
    std::vector<item> items;
};

// Precondition: items are ordered by weight, heaviest first
template<typename BeginIter, typename EndIter>
static fit_result fit_items_in_weight(BeginIter begin, EndIter end, int weight) {

	// Drop any that won't fit in remaining weight
	while (begin != end && begin->weight > weight) {
		++begin;
	}

	// No items fit in remaining weight.
    if (begin == end) {
        return {0, {}};
    }

	// If this is the last item in the list, just return as we know it fits.
    if (std::next(begin) == end) {
	    return {begin->value, {*begin}};
    }

	// Recursively find best fit for remaining weight
    auto rest = fit_items_in_weight(std::next(begin), end, weight - begin->weight);

	// Recursively find best result if we ignore the current heaviest item
    auto exluding_heaviest = fit_items_in_weight(std::next(begin), end, weight);

    const value_t total_including_heaviest = begin->value + rest.total_value;

	// Favour returning excluding heaviest so we don't have to do any copies
	// Also smaller/lighter items are easier to sell secretly!
    if (exluding_heaviest.total_value >= total_including_heaviest) {
        return std::move(exluding_heaviest);
    }

	// Add the heaviest to the rest and return it!
	rest.total_value = total_including_heaviest;
	rest.items.push_back(*begin);

	return std::move(rest);
}

std::vector<item> steal(std::vector<item> items) {
    // Sort by heaviest first
    std::sort(std::begin(items), std::end(items), [](const item &lhs, const item &rhs) {
        return lhs.weight > rhs.weight;
    });

    return fit_items_in_weight(std::begin(items), std::end(items), 50).items;
}

}

using greedy_thief::item;

void run(const std::vector<item> &items) {
	const auto start = std::chrono::high_resolution_clock::now();
	auto stolen_items = steal(items);
	const auto end = std::chrono::high_resolution_clock::now();

	std::cout << "Sifted through "
	          << items.size()
			  << " items in "
			  << std::chrono::duration_cast<std::chrono::microseconds>(end - start).count()
			  << "us\n";

	std::sort(std::begin(stolen_items), std::end(stolen_items),
		[](const auto &lhs, const auto &rhs) { return lhs.id < rhs.id; });

	for (const auto &item : stolen_items) {
		std::cout << "id: " << item.id << ", weight: " << item.weight << ", value: " << item.value << "\n";
	}
	
	std::cout << std::flush;
}

int main() {
	for (std::string line; std::getline(std::cin, line);) {
		const auto start = std::chrono::high_resolution_clock::now();
		std::stringstream ss{line};

		std::vector<item> items;

		while (ss.good()) {
			item &item = items.emplace_back();
			ss >> item.id >> item.weight >> item.value;
		}

		run(items);

		const auto end = std::chrono::high_resolution_clock::now();
		std::cout << "Total: " << std::chrono::duration_cast<std::chrono::microseconds>(end - start).count() << "us\n";
	}
}
