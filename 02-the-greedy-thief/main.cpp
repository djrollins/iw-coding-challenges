#include <chrono>
#include <iostream>
#include <vector>
#include <utility>
#include <algorithm>
#include <string>
#include <sstream>
#include <map>

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

template<typename Item>
struct fit_result
{
	value_t total_value;
	weight_t total_weight;
	std::vector<Item> items;
};

template<typename BeginIter>
using memoization_table = std::map<BeginIter, std::vector<fit_result<BeginIter>>>;

// Precondition: items are ordered by weight, heaviest first
template<bool Memoize, typename BeginIter, typename EndIter>
static fit_result<BeginIter> fit_items_in_weight(BeginIter begin, EndIter end, int weight) {

	static memoization_table<BeginIter> cache{};
	auto iter = begin;

	if constexpr (Memoize) {
		auto &results = cache[begin];

		std::sort(std::begin(results), std::end(results),
			[](const auto &lhs, const auto &rhs) {
				return lhs.total_value > rhs.total_value;
			}
		);

		const auto found = std::find_if(
			std::begin(results), std::end(results),
			[weight](const auto &result) {
				return result.total_weight <= weight;
			});

		if (found != std::end(results)) return *found;
	}

	// Drop any that won't fit in remaining weight
	while (iter != end && iter->weight > weight) {
		++iter;
	}

	// No items fit in remaining weight.
	if (iter == end) {
		return {0, 0, {}};
	}

	// If this is the last item in the list, just return as we know it fits.
	if (std::next(iter) == end) {
		return {iter->value, iter->weight, {iter}};
	}

	// Recursively find best result if we ignore the current heaviest item
	auto exluding_heaviest = fit_items_in_weight<Memoize>(std::next(iter), end, weight);

	// Recursively find best fit for remaining weight
	auto rest = fit_items_in_weight<Memoize>(std::next(iter), end, weight - iter->weight);

	// Add the heaviest to the rest.
	rest.total_value = rest.total_value + iter->value;
	rest.total_weight = rest.total_weight + iter->weight;
	rest.items.push_back(iter);

	if (exluding_heaviest.total_value > rest.total_value) {
		if constexpr (Memoize) {
			cache[begin].push_back(exluding_heaviest);
		}
		return std::move(exluding_heaviest);
	} else if (exluding_heaviest.total_value < rest.total_value) {
		if constexpr (Memoize) {
			cache[begin].push_back(rest);
		}
		return std::move(rest);
	}

	if (exluding_heaviest.total_weight < rest.total_weight) {
		if constexpr (Memoize) {
			cache[begin].push_back(exluding_heaviest);
		}
		return std::move(exluding_heaviest);
	} else {
		if constexpr (Memoize) {
			cache[begin].push_back(rest);
		}
		return std::move(rest);
	}
}

std::vector<item> steal(std::vector<item> items, int weight_limit) {
	std::vector<item> stolen_items;

	std::sort(std::begin(items), std::end(items), [](const item &lhs, const item &rhs) {
		return lhs.value > rhs.value;
	});
	// Sort by heaviest first
	std::stable_sort(std::begin(items), std::end(items), [](const item &lhs, const item &rhs) {
		return lhs.weight > rhs.weight;
	});


	const auto haul = fit_items_in_weight<true>(std::begin(items), std::end(items), weight_limit);

	std::transform(
		std::begin(haul.items), std::end(haul.items),
		std::back_inserter(stolen_items),
		[](const auto iter) { return *iter; });

	std::cout << "weight: " << haul.total_weight << ", total value: " << haul.total_value << "\n";

	return stolen_items;
}

}

using greedy_thief::item;

void run(const std::vector<item> &items, int weight_limit) {
	const auto start = std::chrono::high_resolution_clock::now();
	auto stolen_items = steal(items, weight_limit);
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
	int weight_limit;
	std::cin >> weight_limit;

	for (std::string line; std::getline(std::cin, line);) {
		const auto start = std::chrono::high_resolution_clock::now();
		std::stringstream ss{line};

		std::vector<item> items;

		while (ss.good()) {
			item &item = items.emplace_back();
			ss >> item.id >> item.weight >> item.value;
		}

		run(items, weight_limit);

		const auto end = std::chrono::high_resolution_clock::now();
		std::cout << "Total: " << std::chrono::duration_cast<std::chrono::microseconds>(end - start).count() << "us\n";
	}
}
