#include <iostream>
#include <string>
#include <vector>
#include <math.h>

// 1. A function called positive_negative_ratio which accepts a std::vector of floats and returns a float. It should calculate and return the magnitude of the ratio of [the sum of all the positive entries in the input] to [the sum of all the negative entries in the input].
//

float positive_negative_ratio(std::vector<float> numbers)
{   // function receives a vector of floats called numbers, returns a float value
    float pos = 0; float neg = 0;  // initialise variables to store positive and negative sums
    for (auto x : numbers)  // for each value in numbers
    { if (x >= 0)       // if the value is positive or equal to zero, 
        { pos += x; }  // add it to the positive variable 
        else
        { neg += x; }  // all other values, those which are negative, add to the negative variable
    }
    return (pos / (-neg));  // return calculated ratio of positive values to negative values (convert negative to positive value for division) as a float
}

// 2. A function called as_negative which accepts a float and returns a float. It should turn whatever the input is, into a negative number and return it. Inputs which are already negative should remain so.

float as_negative(float i)
{                 // function receives float value, returns float value
    if (i == 0)   // if the value is equal to zero, which cannot be negative 
    {throw std::invalid_argument("Zero is neither a positive nor negative value. Please remove from vector.") ; }  // terminate and throw invalid argument error
    else if (i > 0)  // if the value is postitive 
    { i = i * (-1); return i; }   // turn it into a negative number and return
    else   // all numbers less than zero, stay negative
    return i;  // return the negative float value
}

// 3. A function called n_multiples_of which accepts two integers and returns a std::vector of integers containing 1..n multiples of its argument. The first argument defines how many multiples to return and the second gives the number for which the multiples are calculated.For example, calling n_multiples_of(4, 3) should return {3, 6, 9, 12} (the first 4 multiples of 3) and n_multiples_of(3, 1) should return {1, 2, 3} (the first 3 multiples of 1).
//

std::vector<int> n_multiples_of(int n, int x)  // returns vector output, receives integer values
{
   if (n < 0)  // if the number of multiples to make is negative 
   { throw std::invalid_argument("Cannot have negative multiples. Please input positive integer.") ; }  // terminate and throw error for invalid argument
   std::vector<int> multiples;  // initialise the vector inside function
   for (int z=1; z!=n+1; z++)  // create variable z starting at 1, as long as z is less than n+1 (it will loop through n times), increment z by one each time
   {
       multiples.push_back(z * x);  // append the vector with each multiple of x 
   }
    return multiples;  // return the vector of multiple values
}


int main() {
    // The code in this function is provided as an example of how the
    // functions you write may be called. It is non-exhaustive and
    // does not check correctness.
    
    // Positive/negative ratio
    std::vector<float> numbers = {4.0, 7.0, -6.6, 3.65, 76.0, -7.0, -83.0, 1.0, 0.0};
    auto ratio = positive_negative_ratio(numbers);
    std::cout << "Ratio magnitude is " << ratio << std::endl;

    // As negative
    std::cout << std::endl;  // Just a blank line
    for(auto i : {-5.4, 4.0, 3.67}) {
        std::cout << i << " as negative is " << as_negative(i) << std::endl;
    }

   // N multiples of
     std::cout << std::endl;  // Just a blank line
     int n = 5;
     int x = 4;
     std::vector<int> multiples = n_multiples_of(n, x);
     std::cout << "The first " << n << " multiples of " << x << " are:" << std::endl;
     for (auto i : multiples) {
        std::cout << "  " << i << std::endl;
    }
}
