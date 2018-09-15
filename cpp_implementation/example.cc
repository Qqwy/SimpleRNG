#include <iostream>

#include "simplerng.h"

int main()
{
    SimpleRNG rng(42);

    size_t n = 10;
    while (n--)
        std::cout << static_cast<unsigned long>(rng.SimpleRNG_rand()) << '\n';

    // Modifying a random number in place
    uint32_t thisWillBeModifiedInPlace = 43;
    rng.SimpleRNG_rand(thisWillBeModifiedInPlace);
    std::cout << '\n' << "43 modified in place: "
        << static_cast<unsigned long>(thisWillBeModifiedInPlace) << std::endl;
}

