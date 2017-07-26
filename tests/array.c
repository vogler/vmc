int[5] a;

int main(){
  a[0] = 2;
  a[1] = *a + 3;
  a[2] = *(a+1) + 4;
  return a[2];
}
