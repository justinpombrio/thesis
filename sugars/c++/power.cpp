#include <iostream>

using namespace std;


template<typename A>
//template<typename B> syntax error: templates cannot define templates
int add(A a, A b) {
  return a + b;
}

int main() {
  std::cout << add<int>(1, 2) << std::endl;
  return 0;
}
