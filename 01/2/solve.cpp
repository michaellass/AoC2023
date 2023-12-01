#include <array>
#include <fstream>
#include <iostream>
#include <string>

constexpr int getFirst(const std::string_view &s) {
    auto idx = s.find_first_of("0123456789");
    return s.at(idx) - '0';
}

constexpr int getLast(const std::string_view &s) {
    auto idx = s.find_last_of("0123456789");
    return s.at(idx) - '0';
}

void replaceWords(std::string &s) {
    constexpr std::array<std::pair<std::string_view, char>, 9> mapping {{
        {"one", '1'},
        {"two", '2'},
        {"three", '3'},
        {"four", '4'},
        {"five", '5'},
        {"six", '6'},
        {"seven", '7'},
        {"eight", '8'},
        {"nine", '9'}
    }};

    for (auto it = s.begin(); it < s.end(); ++it) {
        const auto window = std::string_view(it, s.end());
        for (const auto& [search, replace] : mapping) {
            if (window.starts_with(search)) {
                *it = replace;
            }
        }
    }
}

int main() {
    std::ifstream input;
    std::string line;
    int sum{};

    input.open("input");
    while (std::getline(input, line)) {
        replaceWords(line);
        sum += getFirst(line)*10 + getLast(line);
    }
    input.close();

    std::cout << sum << "\n";
}