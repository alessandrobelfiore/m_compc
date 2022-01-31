#include <stdio.h>
#include <stdlib.h>

int Prelude_getint(){
    char buffer[32];
    if(fgets(buffer, 32, stdin) == NULL)
      return 0;
    return atoi(buffer);
}

void Prelude_print(int n){
  printf("%d\n", n);
}