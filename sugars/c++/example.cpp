#include <iostream>

template<class T>
T circular_area(T r) {
  return T(3.141592653589793238) * r * r;
}

int main() {
  float area = circular_area<float>(1);
  std::cout << area << std::endl;
  return 0;
}
