#include <stdio.h>

// Not syntax safe
#define SUB1(a, b) a - b
#define SUB2(a, b) (a) - (b)
#define SUB3(a, b) ((a) - (b))

// No error: not scope safe.
#define ILL_SCOPED x

// No type error: not type safe.
#define ILL_TYPED int* ptr = 2.3

// No hygienic
#define UNHYGIENIC x = x + 1

int main() {
  //ILL_TYPED;
  int x = 0;
  UNHYGIENIC;
  printf("Unhygienic: %i\n", x);
  // Not syntax safe!
  printf("'#define SUB(a, b) a - b'       in SUB(0, 2-1): %i\n", SUB1(0, 2-1));
  printf("'#define SUB(a, b) (a) - (b)'   in SUB(5, 3)*2: %i\n", SUB2(5, 3)*2);
  printf("'#define SUB(a, b) ((a) - (b))' in SUB(0, 2-1): %i\n", SUB3(0, 2-1));
  printf("'#define SUB(a, b) ((a) - (b))' in SUB(5, 3)*2: %i\n", SUB3(5, 3)*2);
  return 0;
}
