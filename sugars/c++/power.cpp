#include <iostream>

using namespace std;

template<class A>
struct Foo {
  template<class B> struct Bar {
    A fieldA;
    B fieldB;
  };
};


int main() {
  using IntFoo = Foo<int>;
  using IntFooFloatBar = IntFoo::Bar<float>;
  using IntFooCharBar = IntFoo::Bar<char>;
  IntFooFloatBar bar1 = {6,6.6};
  IntFooCharBar bar2 = {6, '6'};
  std::cout << bar1.fieldA + bar1.fieldB << std::endl;
  std::cout << bar2.fieldA + bar2.fieldB << std::endl;
  return 0;
}
