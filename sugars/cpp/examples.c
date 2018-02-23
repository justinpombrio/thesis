#include <stdio.h>

#define SUB1(a, b) a - b
#define SUB2(a, b) (a) - (b)
#define SUB3(a, b) ((a) - (b))

int main() {
  printf("'#define SUB(a, b) a - b'       in SUB(0, 2-1): %i\n", SUB1(0, 2-1));
  printf("'#define SUB(a, b) (a) - (b)'   in SUB(5, 3)*2: %i\n", SUB2(5, 3)*2);
  printf("'#define SUB(a, b) ((a) - (b))' in SUB(0, 2-1): %i\n", SUB3(0, 2-1));
  printf("'#define SUB(a, b) ((a) - (b))' in SUB(5, 3)*2: %i\n", SUB3(5, 3)*2);
  return 0;
}
