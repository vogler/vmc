int main(){
  int[5] a;
  a[0] = 2;
  a[1] = *a + 3;
  a[2] = *(a+1) + 4;
  return a[2];
}
