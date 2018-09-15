#include <iostream>

#include "simplerng.h"

int main()
{
    SimpleRNG rng(42);

    for (size_t n = 10; n > 0; --n)
        std::cout << static_cast<unsigned long>(rng.rand()) << '\n';
}

