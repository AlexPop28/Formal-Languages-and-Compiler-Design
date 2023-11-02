int n;
n = read_int();
int prime;
prime = 1;
int d;
d = 2;
while (d < n) {
  if (n % d == 0) {
    prime = 0;
  }
  d = d + 1;
}
if (prime == 0) {
  print_str("not prime")
} else {
  print_str("prime")
}
