#include <fstream>
#include <iostream>

constexpr int getFirst(const std::string_view &s) {
    auto idx = s.find_first_of("0123456789");
    return s.at(idx) - '0';
}

constexpr int getLast(const std::string_view &s) {
    auto idx = s.find_last_of("0123456789");
    return s.at(idx) - '0';
}

int main() {
    std::ifstream input;
    std::string line;
    int sum{};

    input.open("input");
    while (std::getline(input, line)) {
        sum += getFirst(line)*10 + getLast(line);
    }
    input.close();

    std::cout << sum << "\n";
}