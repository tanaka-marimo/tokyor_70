data {
  int N;
  int R;
  real Y[N];
  real X1[N];
  real X2[N];
  real X3[N];
  real X4[N];
  int<lower=1, upper=R> RID[N];
}

parameters {
  real a0;
  real b;
  real c;
  real d;
  real e;
  real a[R];
  real<lower=0> s_a;
  real<lower=0> s_Y[R];
}

model {
  for (r in 1:R) {
    a[r] ~ normal(a0, s_a);
  }

  for (n in 1:N)
    Y[n] ~ normal(a[RID[n]]*X1[n] + b*X2[n] + c*X3[n] +  d*X4[n] + e, 
                  s_Y[RID[n]]);
}
