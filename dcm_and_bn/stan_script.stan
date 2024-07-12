data {
  int<lower=1> J; // # of respondents/participants/students
  int<lower=1> I; // # of items
  int<lower=1> K; // # of attributes/skills/knowledge (latent variables)
  int<lower=1> C; // # of attribute profiles/classes (latent classes)
  matrix[J, I] Y; // response matrix
}
transformed data {

}
parameters {
  simplex[C] tau[C];
  simplex[C] Vc[C];

  // intercepts for each item
  real l1_int;
  real l2_int;
  real l3_int;
  real l4_int;
  real l5_int;
  real l6_int;
  real l7_int;
  real l8_int;
  real l9_int;
  real l10_int;
  real l11_int;
  real l12_int;
  real l13_int;
  real l14_int;
  real l15_int;
  real l16_int;
  real l17_int;
  real l18_int;
  real l19_int;
  real l20_int;
  real l21_int;
  real l22_int;
  real l23_int;
  real l24_int;
  real l25_int;
  real l26_int;
  real l27_int;
  real l28_int;

  // main effects for each item
  real<lower=0> l1_11;
  real<lower=0> l1_12;
  real<lower=0> l2_12;
  real<lower=0> l3_11;
  real<lower=0> l3_13;
  real<lower=0> l4_13;
  real<lower=0> l5_13;
  real<lower=0> l5_13;
  real<lower=0> l6_13;
  real<lower=0> l7_11;
  real<lower=0> l7_13;
  real<lower=0> l8_12;
  real<lower=0> l9_13;
  real<lower=0> l10_11;
  real<lower=0> l11_11;
  real<lower=0> l11_13;
  real<lower=0> l12_11;
  real<lower=0> l12_13;
  real<lower=0> l13_11;
  real<lower=0> l14_11;
  real<lower=0> l15_13;
  real<lower=0> l16_11;
  real<lower=0> l16_13;
  real<lower=0> l17_12;
  real<lower=0> l17_13;
  real<lower=0> l18_13;
  real<lower=0> l19_13;
  real<lower=0> l20_11;
  real<lower=0> l20_13;
  real<lower=0> l21_11;
  real<lower=0> l21_13;
  real<lower=0> l22_13;
  real<lower=0> l23_12;
  real<lower=0> l24_12;
  real<lower=0> l25_11;
  real<lower=0> l26_13;
  real<lower=0> l27_11;
  real<lower=0> l28_13;

  //interactions
  real<lower=-1 * fmin(l1_11, l1_12)> l1_212;

  real<lower=-1 * fmin(l4_11, l4_14)> l4_214;
  real<lower=-1 * fmin(l5_11, l5_14)> l5_214;
  real<lower=-1 * fmin(l6_11, l6_13)> l6_213;
  real<lower=-1 * fmin(l7_11, l7_13)> l7_213;
  real<lower=-1 * fmin(l11_12, l11_15)> l11_225;
  real<lower=-1 * fmin(l12_12, l12_14)> l12_224;
  real<lower=-1 * fmin(l13_12, l13_14)> l13_224;
  real<lower=-1 * fmin(l14_12, l14_13)> l14_223;
  real<lower=-1 * fmin(l18_13, l18_14)> l18_234;
  real<lower=-1 * fmin(l19_12, l19_13)> l19_223;
  real<lower=-1 * fmin(l20_11, l21_13)> l20_213;
  real<lower=-1 * fmin(l21_11, l21_13)> l21_213;
  real<lower=-1 * fmin(l25_13, l25_14)> l25_234;
  real<lower=-1 * fmin(l26_11, l26_14)> l26_214;
  real<lower=-1 * fmin(l27_13, l27_14)> l27_234;
  real<lower=-1 * fmin(l28_11, l28_14)> l28_214;
  real<lower=-1 * fmin(l32_11, l32_15)> l32_215;
  real<lower=-1 * fmin(l33_14, l33_15)> l33_245;
  real<lower=-1 * fmin(l34_14, l34_15)> l34_245;
  real<lower=-1 * fmin(l35_13, l35_15)> l35_235;

}
transformed parameters {
  matrix[I,C] pi;

  pi[1,1] = inv_logit(l1_int);
  pi[2,1] = inv_logit(l2_int);
  pi[3,1] = inv_logit(l3_int);
  pi[4,1] = inv_logit(l4_int);
  pi[5,1] = inv_logit(l5_int);
  pi[6,1] = inv_logit(l6_int);
  pi[7,1] = inv_logit(l7_int);
  pi[8,1] = inv_logit(l8_int);
  pi[9,1] = inv_logit(l9_int);
  pi[10,1] = inv_logit(l10_int);
  pi[11,1] = inv_logit(l11_int);
  pi[12,1] = inv_logit(l12_int);
  pi[13,1] = inv_logit(l13_int);
  pi[14,1] = inv_logit(l14_int);
  pi[15,1] = inv_logit(l15_int);
  pi[16,1] = inv_logit(l16_int);
  pi[17,1] = inv_logit(l17_int);
  pi[18,1] = inv_logit(l18_int);
  pi[19,1] = inv_logit(l19_int);
  pi[20,1] = inv_logit(l20_int);
  pi[21,1] = inv_logit(l21_int);
  pi[22,1] = inv_logit(l22_int);
  pi[23,1] = inv_logit(l23_int);
  pi[24,1] = inv_logit(l24_int);
  pi[25,1] = inv_logit(l25_int);
  pi[26,1] = inv_logit(l26_int);
  pi[27,1] = inv_logit(l27_int);
  pi[28,1] = inv_logit(l28_int);
  pi[29,1] = inv_logit(l29_int);
  pi[30,1] = inv_logit(l30_int)
  pi[31,1] = inv_logit(l31_int)
  pi[32,1] = inv_logit(l32_int)
  pi[33,1] = inv_logit(l33_int)
  pi[34,1] = inv_logit(l34_int);
  pi[35,1] = inv_logit(l35_int);

 
  pi[1,2] = inv_logit(l1_int+l1_11);
  pi[2,2] = inv_logit(l2_int+l2_11);
  pi[3,2] = inv_logit(l3_int+l3_11);
  pi[4,2] = inv_logit(l4_int+l4_11);
  pi[5,2] = inv_logit(l5_int+l5_11);
  pi[6,2] = inv_logit(l6_int+l6_11);
  pi[7,2] = inv_logit(l7_int+l7_11);
  pi[8,2] = inv_logit(l8_int);
  pi[9,2] = inv_logit(l9_int);
  pi[10,2] = inv_logit(l10_int);
  pi[11,2] = inv_logit(l11_int);
  pi[12,2] = inv_logit(l12_int);
  pi[13,2] = inv_logit(l13_int);
  pi[14,2] = inv_logit(l14_int);
  pi[15,2] = inv_logit(l15_int);
  pi[16,2] = inv_logit(l16_int);
  pi[17,2] = inv_logit(l17_int);
  pi[18,2] = inv_logit(l18_int);
  pi[19,2] = inv_logit(l19_int);
  pi[20,2] = inv_logit(l20_int+l20_11);
  pi[21,2] = inv_logit(l21_int+l21_11);
  pi[22,2] = inv_logit(l22_int);
  pi[23,2] = inv_logit(l23_int);
  pi[24,2] = inv_logit(l24_int);
  pi[25,2] = inv_logit(l25_int);
  pi[26,2] = inv_logit(l26_int+l26_11);
  pi[27,2] = inv_logit(l27_int);
  pi[28,2] = inv_logit(l28_int+l28_11);
  pi[29,2] = inv_logit(l29_int);
  pi[30,2] = inv_logit(l30_int);
  pi[31,2] = inv_logit(l31_int);
  pi[32,2] = inv_logit(l32_int+l32_11);
  pi[33,2] = inv_logit(l33_int);
  pi[34,2] = inv_logit(l34_int);
  pi[35,2] = inv_logit(l35_int);

  pi[1,3] = inv_logit(l1_int);
  pi[2,3] = inv_logit(l2_int);
  pi[3,3] = inv_logit(l3_int);
  pi[4,3] = inv_logit(l4_int);
  pi[5,3] = inv_logit(l5_int);
  pi[6,3] = inv_logit(l6_int);
  pi[7,3] = inv_logit(l7_int);
  pi[8,3] = inv_logit(l8_int+l8_12);
  pi[9,3] = inv_logit(l9_int+l9_12);
  pi[10,3] = inv_logit(l10_int+l10_12);
  pi[11,3] = inv_logit(l11_int+l11_12);
  pi[12,3] = inv_logit(l12_int+l12_12);
  pi[13,3] = inv_logit(l13_int+l13_12);
  pi[14,3] = inv_logit(l14_int+l14_12);
  pi[15,3] = inv_logit(l15_int);
  pi[16,3] = inv_logit(l16_int);
  pi[17,3] = inv_logit(l17_int);
  pi[18,3] = inv_logit(l18_int);
  pi[19,3] = inv_logit(l19_int+l19_12);
  pi[20,3] = inv_logit(l20_int);
  pi[21,3] = inv_logit(l21_int);
  pi[22,3] = inv_logit(l22_int);
  pi[23,3] = inv_logit(l23_int);
  pi[24,3] = inv_logit(l24_int);
  pi[25,3] = inv_logit(l25_int);
  pi[26,3] = inv_logit(l26_int);
  pi[27,3] = inv_logit(l27_int);
  pi[28,3] = inv_logit(l28_int);
  pi[29,3] = inv_logit(l29_int);
  pi[30,3] = inv_logit(l30_int);
  pi[31,3] = inv_logit(l31_int);
  pi[32,3] = inv_logit(l32_int);
  pi[33,3] = inv_logit(l33_int);
  pi[34,3] = inv_logit(l34_int);
  pi[35,3] = inv_logit(l35_int);

  pi[1,4] = inv_logit(l1_int);
  pi[2,4] = inv_logit(l2_int);
  pi[3,4] = inv_logit(l3_int);
  pi[4,4] = inv_logit(l4_int);
  pi[5,4] = inv_logit(l5_int);
  pi[6,4] = inv_logit(l6_int+l6_13);
  pi[7,4] = inv_logit(l7_int+l7_13);
  pi[8,4] = inv_logit(l8_int);
  pi[9,4] = inv_logit(l9_int);
  pi[10,4] = inv_logit(l10_int);
  pi[11,4] = inv_logit(l11_int);
  pi[12,4] = inv_logit(l12_int);
  pi[13,4] = inv_logit(l13_int);
  pi[14,4] = inv_logit(l14_int+l14_13);
  pi[15,4] = inv_logit(l15_int+l15_13);
  pi[16,4] = inv_logit(l16_int+l16_13);
  pi[17,4] = inv_logit(l17_int+l17_13);
  pi[18,4] = inv_logit(l18_int+l18_13);
  pi[19,4] = inv_logit(l19_int+l19_13);
  pi[20,4] = inv_logit(l20_int+l20_13);
  pi[21,4] = inv_logit(l21_int+l21_13);
  pi[22,4] = inv_logit(l22_int);
  pi[23,4] = inv_logit(l23_int);
  pi[24,4] = inv_logit(l24_int);
  pi[25,4] = inv_logit(l25_int+l25_13);
  pi[26,4] = inv_logit(l26_int);
  pi[27,4] = inv_logit(l27_int+l27_13);
  pi[28,4] = inv_logit(l28_int);
  pi[29,4] = inv_logit(l29_int);
  pi[30,4] = inv_logit(l30_int);
  pi[31,4] = inv_logit(l31_int);
  pi[32,4] = inv_logit(l32_int);
  pi[33,4] = inv_logit(l33_int);
  pi[34,4] = inv_logit(l34_int);
  pi[35,4] = inv_logit(l35_int+l35_13);

  pi[1,5] = inv_logit(l1_int);
  pi[2,5] = inv_logit(l2_int);
  pi[3,5] = inv_logit(l3_int);
  pi[4,5] = inv_logit(l4_int+l4_14);
  pi[5,5] = inv_logit(l5_int+l5_14);
  pi[6,5] = inv_logit(l6_int);
  pi[7,5] = inv_logit(l7_int);
  pi[8,5] = inv_logit(l8_int);
  pi[9,5] = inv_logit(l9_int);
  pi[10,5] = inv_logit(l10_int);
  pi[11,5] = inv_logit(l11_int);
  pi[12,5] = inv_logit(l12_int+l12_14);
  pi[13,5] = inv_logit(l13_int+l13_14);
  pi[14,5] = inv_logit(l14_int);
  pi[15,5] = inv_logit(l15_int);
  pi[16,5] = inv_logit(l16_int);
  pi[17,5] = inv_logit(l17_int);
  pi[18,5] = inv_logit(l18_int+l18_14);
  pi[19,5] = inv_logit(l19_int);
  pi[20,5] = inv_logit(l20_int);
  pi[21,5] = inv_logit(l21_int);
  pi[22,5] = inv_logit(l22_int+l22_14);
  pi[23,5] = inv_logit(l23_int+l23_14);
  pi[24,5] = inv_logit(l24_int+l24_14);
  pi[25,5] = inv_logit(l25_int+l25_14);
  pi[26,5] = inv_logit(l26_int+l26_14);
  pi[27,5] = inv_logit(l27_int+l27_14);
  pi[28,5] = inv_logit(l28_int+l28_14);
  pi[29,5] = inv_logit(l29_int);
  pi[30,5] = inv_logit(l30_int);
  pi[31,5] = inv_logit(l31_int);
  pi[32,5] = inv_logit(l32_int);
  pi[33,5] = inv_logit(l33_int+l33_14);
  pi[34,5] = inv_logit(l34_int+l34_14);
  pi[35,5] = inv_logit(l35_int);

  pi[1,6] = inv_logit(l1_int);
  pi[2,6] = inv_logit(l2_int);
  pi[3,6] = inv_logit(l3_int);
  pi[4,6] = inv_logit(l4_int);
  pi[5,6] = inv_logit(l5_int);
  pi[6,6] = inv_logit(l6_int);
  pi[7,6] = inv_logit(l7_int);
  pi[8,6] = inv_logit(l8_int);
  pi[9,6] = inv_logit(l9_int);
  pi[10,6] = inv_logit(l10_int);
  pi[11,6] = inv_logit(l11_int+l11_15);
  pi[12,6] = inv_logit(l12_int);
  pi[13,6] = inv_logit(l13_int);
  pi[14,6] = inv_logit(l14_int);
  pi[15,6] = inv_logit(l15_int);
  pi[16,6] = inv_logit(l16_int);
  pi[17,6] = inv_logit(l17_int);
  pi[18,6] = inv_logit(l18_int);
  pi[19,6] = inv_logit(l19_int);
  pi[20,6] = inv_logit(l20_int);
  pi[21,6] = inv_logit(l21_int);
  pi[22,6] = inv_logit(l22_int);
  pi[23,6] = inv_logit(l23_int);
  pi[24,6] = inv_logit(l24_int);
  pi[25,6] = inv_logit(l25_int);
  pi[26,6] = inv_logit(l26_int);
  pi[27,6] = inv_logit(l27_int);
  pi[28,6] = inv_logit(l28_int);
  pi[29,6] = inv_logit(l29_int+l29_15);
  pi[30,6] = inv_logit(l30_int+l30_15);
  pi[31,6] = inv_logit(l31_int+l31_15);
  pi[32,6] = inv_logit(l32_int+l32_15);
  pi[33,6] = inv_logit(l33_int+l33_15);
  pi[34,6] = inv_logit(l34_int+l34_15);
  pi[35,6] = inv_logit(l35_int+l35_15);
  //interactions need to be present in each column they appear in
  pi[1,7] = inv_logit(l1_int+l1_11);
  pi[2,7] = inv_logit(l2_int+l2_11);
  pi[3,7] = inv_logit(l3_int+l3_11);
  pi[4,7] = inv_logit(l4_int+l4_11+l4_14+l4_214);
  pi[5,7] = inv_logit(l5_int+l5_11+l5_14+l5_214);
}

  model{
    real ps[C];             // temp for log component densities
    real pi;
    real log_items[I];
    slip ~ beta(5,25);
    guess ~ beta(5,25);
    for (j in 1:J){
        for (c in 1:C){
            for (i in 1:I){
                pi = (1 - slip[i])^xi[i,c] * guess[i]^(1 - xi[i,c]);
                log_items[i] = y[j,i] * log(pi)
                        + (1 - y[j,i]) * log(1 - pi);
            }
            ps[c] = log_nu[c] + sum(log_items); 
        }
        target += log_sum_exp(ps);
    }
}