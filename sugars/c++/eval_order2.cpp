#include <iostream>
#include <type_traits>

using namespace std;

template<typename A>
struct inner {
  static_assert(sizeof(A*) == 7, "inner");
  int a;
};

template<typename B>
struct outer {
  static_assert(sizeof(B*) == 7, "outer1");
  inner<B> a;
  static_assert(sizeof(B*) == 7, "outer2");
};

// The order of the errors suggests that
// 'outer' is expanded before 'inner':

int main() {
  outer<int> x;
  return 0;
}
