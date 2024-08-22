#==========================================================================================================#
# Purpose : Generate table and plot for Quantile Treatment Effects
# Name    : Saani Rawat
# Created : 08/13/2024
# Log     : 
#           08/13/2024: 
#==========================================================================================================#


#------------------------------------------------------------------------------------------------------;
#------------------------------------------------------------------------------------------------------;
# Estimates extracted from Stata output with help of GPT-4
# Outcome variable: Median Sale Amount
# datasets used: housing_agg_roads_census
# for more details, see road_spending_reg_discontinuity_agg_qte.do
#------------------------------------------------------------------------------------------------------;
#------------------------------------------------------------------------------------------------------;

#------------------------------------------------------------------------------------------------------;
# Running with no covariates
#------------------------------------------------------------------------------------------------------;

# Extracted values for t_minus_3

t_minus_3_q1 <- c(Coef = -6300, Std_Err = 10551.09, Lower_95_CI = -26979.75, Upper_95_CI = 14379.75)
t_minus_3_q2 <- c(Coef = -680.9492, Std_Err = 10807.44, Lower_95_CI = -21863.14, Upper_95_CI = 20501.25)
t_minus_3_q3 <- c(Coef = -11850, Std_Err = 12424.12, Lower_95_CI = -36200.83, Upper_95_CI = 12500.83)
t_minus_3_q4 <- c(Coef = -20790, Std_Err = 12174.19, Lower_95_CI = -44650.98, Upper_95_CI = 3070.98)
t_minus_3_q5 <- c(Coef = -8720.5, Std_Err = 12558.48, Lower_95_CI = -33334.67, Upper_95_CI = 15893.67)
t_minus_3_q6 <- c(Coef = -6900, Std_Err = 11978.24, Lower_95_CI = -30376.92, Upper_95_CI = 16576.92)
t_minus_3_q7 <- c(Coef = -10035, Std_Err = 11158.85, Lower_95_CI = -31905.94, Upper_95_CI = 11835.94)
t_minus_3_q8 <- c(Coef = -9020, Std_Err = 15268.95, Lower_95_CI = -38946.59, Upper_95_CI = 20906.59)
t_minus_3_q9 <- c(Coef = -20057, Std_Err = 20408.12, Lower_95_CI = -60056.18, Upper_95_CI = 19942.18)

# Extracted values for t_minus_2

t_minus_2_q1 <- c(Coef = 14436, Std_Err = 11007.05, Lower_95_CI = -7137.43, Upper_95_CI = 36009.43)
t_minus_2_q2 <- c(Coef = 0, Std_Err = 11224.42, Lower_95_CI = -21999.46, Upper_95_CI = 21999.46)
t_minus_2_q3 <- c(Coef = -5280.5, Std_Err = 10608.87, Lower_95_CI = -26073.5, Upper_95_CI = 15512.5)
t_minus_2_q4 <- c(Coef = -6486, Std_Err = 10556.23, Lower_95_CI = -27175.82, Upper_95_CI = 14203.82)
t_minus_2_q5 <- c(Coef = -11592, Std_Err = 11805.75, Lower_95_CI = -34730.84, Upper_95_CI = 11546.84)
t_minus_2_q6 <- c(Coef = -730, Std_Err = 11465.63, Lower_95_CI = -23202.22, Upper_95_CI = 21742.22)
t_minus_2_q7 <- c(Coef = -15179, Std_Err = 13027.79, Lower_95_CI = -40713, Upper_95_CI = 10355)
t_minus_2_q8 <- c(Coef = -31003, Std_Err = 14631.2, Lower_95_CI = -59679.62, Upper_95_CI = -2326.382)
t_minus_2_q9 <- c(Coef = -34680, Std_Err = 18849.57, Lower_95_CI = -71624.48, Upper_95_CI = 2264.476)

# Extracted values for t_minus_1

t_minus_1_q1 <- c(Coef = 3522, Std_Err = 11311.88, Lower_95_CI = -18648.89, Upper_95_CI = 25692.89)
t_minus_1_q2 <- c(Coef = 0, Std_Err = 14976.88, Lower_95_CI = -29354.14, Upper_95_CI = 29354.14)
t_minus_1_q3 <- c(Coef = 9476.5, Std_Err = 10102.43, Lower_95_CI = -10323.89, Upper_95_CI = 29276.89)
t_minus_1_q4 <- c(Coef = -4605, Std_Err = 9125.166, Lower_95_CI = -22490, Upper_95_CI = 13280)
t_minus_1_q5 <- c(Coef = -9802, Std_Err = 9976.557, Lower_95_CI = -29355.69, Upper_95_CI = 9751.692)
t_minus_1_q6 <- c(Coef = 1300, Std_Err = 11501.4, Lower_95_CI = -21242.33, Upper_95_CI = 23842.33)
t_minus_1_q7 <- c(Coef = -13360, Std_Err = 10871.71, Lower_95_CI = -34668.17, Upper_95_CI = 7948.168)
t_minus_1_q8 <- c(Coef = -17810, Std_Err = 11144.07, Lower_95_CI = -39651.97, Upper_95_CI = 4031.967)
t_minus_1_q9 <- c(Coef = -31377.5, Std_Err = 16539.58, Lower_95_CI = -63794.48, Upper_95_CI = 1039.48)

# Extracted values for t_plus_0

t_plus_0_q1 <- c(Coef = -1333.5, Std_Err = 12704.12, Lower_95_CI = -26233.11, Upper_95_CI = 23566.11)
t_plus_0_q2 <- c(Coef = -9540, Std_Err = 9567.107, Lower_95_CI = -28291.19, Upper_95_CI = 9211.186)
t_plus_0_q3 <- c(Coef = -13450, Std_Err = 9202.609, Lower_95_CI = -31486.78, Upper_95_CI = 4586.782)
t_plus_0_q4 <- c(Coef = -8360, Std_Err = 8842.716, Lower_95_CI = -25691.41, Upper_95_CI = 8971.406)
t_plus_0_q5 <- c(Coef = -22144, Std_Err = 9955.957, Lower_95_CI = -41657.32, Upper_95_CI = -2630.683)
t_plus_0_q6 <- c(Coef = -12943, Std_Err = 11343.04, Lower_95_CI = -35174.95, Upper_95_CI = 9288.949)
t_plus_0_q7 <- c(Coef = -12997.5, Std_Err = 11435.96, Lower_95_CI = -35411.58, Upper_95_CI = 9416.577)
t_plus_0_q8 <- c(Coef = -17970, Std_Err = 12556.81, Lower_95_CI = -42580.9, Upper_95_CI = 6640.902)
t_plus_0_q9 <- c(Coef = -33240, Std_Err = 22661.4, Lower_95_CI = -77655.53, Upper_95_CI = 11175.53)

# Extracted values for t_plus_1

t_plus_1_q1 <- c(Coef = 0, Std_Err = 9330.293, Lower_95_CI = -18287.04, Upper_95_CI = 18287.04)
t_plus_1_q2 <- c(Coef = 0, Std_Err = 9997.316, Lower_95_CI = -19594.38, Upper_95_CI = 19594.38)
t_plus_1_q3 <- c(Coef = -9314, Std_Err = 10183.83, Lower_95_CI = -29273.94, Upper_95_CI = 10645.94)
t_plus_1_q4 <- c(Coef = -5495, Std_Err = 10272.91, Lower_95_CI = -25629.54, Upper_95_CI = 14639.54)
t_plus_1_q5 <- c(Coef = -7045, Std_Err = 11132.97, Lower_95_CI = -28865.23, Upper_95_CI = 14775.23)
t_plus_1_q6 <- c(Coef = -1902.5, Std_Err = 12470.46, Lower_95_CI = -26344.15, Upper_95_CI = 22539.15)
t_plus_1_q7 <- c(Coef = -1312, Std_Err = 12453.31, Lower_95_CI = -25720.03, Upper_95_CI = 23096.03)
t_plus_1_q8 <- c(Coef = -7806, Std_Err = 14390.02, Lower_95_CI = -36009.92, Upper_95_CI = 20397.92)
t_plus_1_q9 <- c(Coef = -19180, Std_Err = 19535.89, Lower_95_CI = -57469.64, Upper_95_CI = 19109.64)

# Extracted values for t_plus_2

t_plus_2_q1 <- c(Coef = -5600, Std_Err = 9680.06, Lower_95_CI = -24572.57, Upper_95_CI = 13372.57)
t_plus_2_q2 <- c(Coef = -4621.5, Std_Err = 9677.051, Lower_95_CI = -23588.17, Upper_95_CI = 14345.17)
t_plus_2_q3 <- c(Coef = -5640, Std_Err = 10623.43, Lower_95_CI = -26461.54, Upper_95_CI = 15181.54)
t_plus_2_q4 <- c(Coef = -12645, Std_Err = 10655.03, Lower_95_CI = -33528.48, Upper_95_CI = 8238.479)
t_plus_2_q5 <- c(Coef = 2000, Std_Err = 10146.66, Lower_95_CI = -17887.08, Upper_95_CI = 21887.08)
t_plus_2_q6 <- c(Coef = 0, Std_Err = 10080.66, Lower_95_CI = -19757.74, Upper_95_CI = 19757.74)
t_plus_2_q7 <- c(Coef = -570, Std_Err = 11338.65, Lower_95_CI = -22793.35, Upper_95_CI = 21653.35)
t_plus_2_q8 <- c(Coef = -6975, Std_Err = 16336.25, Lower_95_CI = -38993.45, Upper_95_CI = 25043.45)
t_plus_2_q9 <- c(Coef = -28580.86, Std_Err = 20797.43, Lower_95_CI = -69343.08, Upper_95_CI = 12181.36)

# Extracted values for t_plus_3

t_plus_3_q1 <- c(Coef = 1280, Std_Err = 8688.793, Lower_95_CI = -15749.72, Upper_95_CI = 18309.72)
t_plus_3_q2 <- c(Coef = -774.8008, Std_Err = 8582.149, Lower_95_CI = -17595.5, Upper_95_CI = 16045.9)
t_plus_3_q3 <- c(Coef = -9820, Std_Err = 8945.45, Lower_95_CI = -27352.76, Upper_95_CI = 7712.76)
t_plus_3_q4 <- c(Coef = -5697.5, Std_Err = 10517.23, Lower_95_CI = -26310.89, Upper_95_CI = 14915.89)
t_plus_3_q5 <- c(Coef = -3355, Std_Err = 13211.56, Lower_95_CI = -29249.18, Upper_95_CI = 22539.18)
t_plus_3_q6 <- c(Coef = -4539.305, Std_Err = 11943.06, Lower_95_CI = -27947.27, Upper_95_CI = 18868.66)
t_plus_3_q7 <- c(Coef = -14580, Std_Err = 11913.81, Lower_95_CI = -37930.64, Upper_95_CI = 8770.64)
t_plus_3_q8 <- c(Coef = -15932, Std_Err = 13635.04, Lower_95_CI = -42656.19, Upper_95_CI = 10792.19)
t_plus_3_q9 <- c(Coef = -19705, Std_Err = 15789.53, Lower_95_CI = -50651.91, Upper_95_CI = 11241.91)

# Extracted values for t_plus_4

t_plus_4_q1 <- c(Coef = -6433, Std_Err = 9363.507, Lower_95_CI = -24785.14, Upper_95_CI = 11919.14)
t_plus_4_q2 <- c(Coef = -5400, Std_Err = 9982.642, Lower_95_CI = -24965.62, Upper_95_CI = 14165.62)
t_plus_4_q3 <- c(Coef = -6625, Std_Err = 10579.9, Lower_95_CI = -27361.22, Upper_95_CI = 14111.22)
t_plus_4_q4 <- c(Coef = -10054, Std_Err = 10634.05, Lower_95_CI = -30896.35, Upper_95_CI = 10788.35)
t_plus_4_q5 <- c(Coef = -10580, Std_Err = 10430.42, Lower_95_CI = -31023.26, Upper_95_CI = 9863.256)
t_plus_4_q6 <- c(Coef = -10425, Std_Err = 10357.41, Lower_95_CI = -30725.15, Upper_95_CI = 9875.149)
t_plus_4_q7 <- c(Coef = -21760, Std_Err = 12333.25, Lower_95_CI = -45932.72, Upper_95_CI = 2412.718)
t_plus_4_q8 <- c(Coef = -28477.5, Std_Err = 13342.75, Lower_95_CI = -54628.81, Upper_95_CI = -2326.19)
t_plus_4_q9 <- c(Coef = -51470.31, Std_Err = 18408.86, Lower_95_CI = -87551.01, Upper_95_CI = -15389.62)

# Extracted values for t_plus_5

t_plus_5_q1 <- c(Coef = -22570, Std_Err = 9065.034, Lower_95_CI = -40337.14, Upper_95_CI = -4802.859)
t_plus_5_q2 <- c(Coef = -15070, Std_Err = 9885.76, Lower_95_CI = -34445.73, Upper_95_CI = 4305.734)
t_plus_5_q3 <- c(Coef = -13178.5, Std_Err = 10118.55, Lower_95_CI = -33010.5, Upper_95_CI = 6653.498)
t_plus_5_q4 <- c(Coef = -9593, Std_Err = 10422.55, Lower_95_CI = -30020.83, Upper_95_CI = 10834.83)
t_plus_5_q5 <- c(Coef = -15763.5, Std_Err = 11463.02, Lower_95_CI = -38230.6, Upper_95_CI = 6703.602)
t_plus_5_q6 <- c(Coef = -8330, Std_Err = 16166.17, Lower_95_CI = -40015.1, Upper_95_CI = 23355.1)
t_plus_5_q7 <- c(Coef = -11171, Std_Err = 11806.4, Lower_95_CI = -34311.11, Upper_95_CI = 11969.11)
t_plus_5_q8 <- c(Coef = -16379, Std_Err = 11404.45, Lower_95_CI = -38731.3, Upper_95_CI = 5973.302)
t_plus_5_q9 <- c(Coef = -34604, Std_Err = 15836.97, Lower_95_CI = -65643.89, Upper_95_CI = -3564.112)

# Extracted values for t_plus_6

t_plus_6_q1 <- c(Coef = -9601.969, Std_Err = 9204.804, Lower_95_CI = -27643.05, Upper_95_CI = 8439.116)
t_plus_6_q2 <- c(Coef = 4014.398, Std_Err = 7443.133, Lower_95_CI = -10573.87, Upper_95_CI = 18602.67)
t_plus_6_q3 <- c(Coef = 0, Std_Err = 7799.36, Lower_95_CI = -15286.46, Upper_95_CI = 15286.46)
t_plus_6_q4 <- c(Coef = -7680, Std_Err = 8097.818, Lower_95_CI = -23551.43, Upper_95_CI = 8191.432)
t_plus_6_q5 <- c(Coef = -12480, Std_Err = 9528.71, Lower_95_CI = -31155.93, Upper_95_CI = 6195.928)
t_plus_6_q6 <- c(Coef = -20110, Std_Err = 11194.97, Lower_95_CI = -42051.73, Upper_95_CI = 1831.734)
t_plus_6_q7 <- c(Coef = -38082, Std_Err = 12834.75, Lower_95_CI = -63237.64, Upper_95_CI = -12926.36)
t_plus_6_q8 <- c(Coef = -38460, Std_Err = 18623.27, Lower_95_CI = -74960.94, Upper_95_CI = -1959.061)
t_plus_6_q9 <- c(Coef = -38510, Std_Err = 22193.83, Lower_95_CI = -82009.11, Upper_95_CI = 4989.107)

# Extracted values for t_plus_7

t_plus_7_q1 <- c(Coef = -12984, Std_Err = 8419.83, Lower_95_CI = -29486.56, Upper_95_CI = 3518.564)
t_plus_7_q2 <- c(Coef = -14682, Std_Err = 8502.486, Lower_95_CI = -31346.57, Upper_95_CI = 1982.566)
t_plus_7_q3 <- c(Coef = -9352.5, Std_Err = 8128.178, Lower_95_CI = -25283.44, Upper_95_CI = 6578.436)
t_plus_7_q4 <- c(Coef = -13739, Std_Err = 9351.889, Lower_95_CI = -32068.37, Upper_95_CI = 4590.366)
t_plus_7_q5 <- c(Coef = -14348, Std_Err = 12007.65, Lower_95_CI = -37882.55, Upper_95_CI = 9186.553)
t_plus_7_q6 <- c(Coef = -20397, Std_Err = 14973.8, Lower_95_CI = -49745.12, Upper_95_CI = 8951.116)
t_plus_7_q7 <- c(Coef = -36685, Std_Err = 12163.38, Lower_95_CI = -60524.78, Upper_95_CI = -12845.22)
t_plus_7_q8 <- c(Coef = -37470, Std_Err = 12168.54, Lower_95_CI = -61319.89, Upper_95_CI = -13620.11)
t_plus_7_q9 <- c(Coef = -27039, Std_Err = 16308.05, Lower_95_CI = -59002.19, Upper_95_CI = 4924.193)

# Extracted values for t_plus_8

t_plus_8_q1 <- c(Coef = -11217, Std_Err = 9135.857, Lower_95_CI = -29122.95, Upper_95_CI = 6688.95)
t_plus_8_q2 <- c(Coef = -15039.5, Std_Err = 8159.683, Lower_95_CI = -31032.18, Upper_95_CI = 953.1839)
t_plus_8_q3 <- c(Coef = -17520, Std_Err = 8959.188, Lower_95_CI = -35079.69, Upper_95_CI = 39.6862)
t_plus_8_q4 <- c(Coef = -5166, Std_Err = 11176.77, Lower_95_CI = -27072.06, Upper_95_CI = 16740.06)
t_plus_8_q5 <- c(Coef = -4962, Std_Err = 10709.67, Lower_95_CI = -25952.57, Upper_95_CI = 16028.57)
t_plus_8_q6 <- c(Coef = -12613.04, Std_Err = 12681.88, Lower_95_CI = -37469.07, Upper_95_CI = 12243)
t_plus_8_q7 <- c(Coef = -21356, Std_Err = 12217.69, Lower_95_CI = -45302.24, Upper_95_CI = 2590.235)
t_plus_8_q8 <- c(Coef = -28950, Std_Err = 12506.67, Lower_95_CI = -53462.63, Upper_95_CI = -4437.374)
t_plus_8_q9 <- c(Coef = -29010, Std_Err = 16640.33, Lower_95_CI = -61624.45, Upper_95_CI = 3604.453)

# Extracted values for t_plus_9

t_plus_9_q1 <- c(Coef = -6568.949, Std_Err = 10809.36, Lower_95_CI = -27754.9, Upper_95_CI = 14617)
t_plus_9_q2 <- c(Coef = -3227.5, Std_Err = 10434.53, Lower_95_CI = -23678.81, Upper_95_CI = 17223.81)
t_plus_9_q3 <- c(Coef = -2214, Std_Err = 9612.324, Lower_95_CI = -21053.81, Upper_95_CI = 16625.81)
t_plus_9_q4 <- c(Coef = -7170.602, Std_Err = 10126.56, Lower_95_CI = -27018.3, Upper_95_CI = 12677.09)
t_plus_9_q5 <- c(Coef = -12582, Std_Err = 11580.95, Lower_95_CI = -35280.24, Upper_95_CI = 10116.24)
t_plus_9_q6 <- c(Coef = -23457.02, Std_Err = 13671.96, Lower_95_CI = -50253.57, Upper_95_CI = 3339.541)
t_plus_9_q7 <- c(Coef = -25605, Std_Err = 13983.6, Lower_95_CI = -53012.36, Upper_95_CI = 1802.359)
t_plus_9_q8 <- c(Coef = -27800, Std_Err = 12421.05, Lower_95_CI = -52144.8, Upper_95_CI = -3455.198)
t_plus_9_q9 <- c(Coef = -49093, Std_Err = 14497.95, Lower_95_CI = -77508.46, Upper_95_CI = -20677.54)

# Extracted values for t_plus_10

t_plus_10_q1 <- c(Coef = -1326, Std_Err = 8792.932, Lower_95_CI = -18559.83, Upper_95_CI = 15907.83)
t_plus_10_q2 <- c(Coef = 624.5, Std_Err = 8509.243, Lower_95_CI = -16053.31, Upper_95_CI = 17302.31)
t_plus_10_q3 <- c(Coef = 3050, Std_Err = 9206.227, Lower_95_CI = -14993.87, Upper_95_CI = 21093.87)
t_plus_10_q4 <- c(Coef = 7635, Std_Err = 10227.14, Lower_95_CI = -12409.82, Upper_95_CI = 27679.82)
t_plus_10_q5 <- c(Coef = -6051.5, Std_Err = 11590.59, Lower_95_CI = -28768.63, Upper_95_CI = 16665.63)
t_plus_10_q6 <- c(Coef = -9100, Std_Err = 10127.21, Lower_95_CI = -28948.96, Upper_95_CI = 10748.96)
t_plus_10_q7 <- c(Coef = -18600, Std_Err = 9872.361, Lower_95_CI = -37949.47, Upper_95_CI = 749.4718)
t_plus_10_q8 <- c(Coef = -18657.95, Std_Err = 11807.61, Lower_95_CI = -41800.44, Upper_95_CI = 4484.532)
t_plus_10_q9 <- c(Coef = -36661.5, Std_Err = 19110.38, Lower_95_CI = -74117.16, Upper_95_CI = 794.1597)

# combine all the values into a comprehensive list

# Creating a list to store all time periods and their respective quantiles

quantile_treatment_effects <- list(
  t_minus_3 = list(
    q1 = t_minus_3_q1,
    q2 = t_minus_3_q2,
    q3 = t_minus_3_q3,
    q4 = t_minus_3_q4,
    q5 = t_minus_3_q5,
    q6 = t_minus_3_q6,
    q7 = t_minus_3_q7,
    q8 = t_minus_3_q8,
    q9 = t_minus_3_q9
  ),
  t_minus_2 = list(
    q1 = t_minus_2_q1,
    q2 = t_minus_2_q2,
    q3 = t_minus_2_q3,
    q4 = t_minus_2_q4,
    q5 = t_minus_2_q5,
    q6 = t_minus_2_q6,
    q7 = t_minus_2_q7,
    q8 = t_minus_2_q8,
    q9 = t_minus_2_q9
  ),
  t_minus_1 = list(
    q1 = t_minus_1_q1,
    q2 = t_minus_1_q2,
    q3 = t_minus_1_q3,
    q4 = t_minus_1_q4,
    q5 = t_minus_1_q5,
    q6 = t_minus_1_q6,
    q7 = t_minus_1_q7,
    q8 = t_minus_1_q8,
    q9 = t_minus_1_q9
  ),
  t_plus_1 = list(
    q1 = t_plus_1_q1,
    q2 = t_plus_1_q2,
    q3 = t_plus_1_q3,
    q4 = t_plus_1_q4,
    q5 = t_plus_1_q5,
    q6 = t_plus_1_q6,
    q7 = t_plus_1_q7,
    q8 = t_plus_1_q8,
    q9 = t_plus_1_q9
  ),
  t_plus_2 = list(
    q1 = t_plus_2_q1,
    q2 = t_plus_2_q2,
    q3 = t_plus_2_q3,
    q4 = t_plus_2_q4,
    q5 = t_plus_2_q5,
    q6 = t_plus_2_q6,
    q7 = t_plus_2_q7,
    q8 = t_plus_2_q8,
    q9 = t_plus_2_q9
  ),
  t_plus_3 = list(
    q1 = t_plus_3_q1,
    q2 = t_plus_3_q2,
    q3 = t_plus_3_q3,
    q4 = t_plus_3_q4,
    q5 = t_plus_3_q5,
    q6 = t_plus_3_q6,
    q7 = t_plus_3_q7,
    q8 = t_plus_3_q8,
    q9 = t_plus_3_q9
  ),
  t_plus_4 = list(
    q1 = t_plus_4_q1,
    q2 = t_plus_4_q2,
    q3 = t_plus_4_q3,
    q4 = t_plus_4_q4,
    q5 = t_plus_4_q5,
    q6 = t_plus_4_q6,
    q7 = t_plus_4_q7,
    q8 = t_plus_4_q8,
    q9 = t_plus_4_q9
  ),
  t_plus_5 = list(
    q1 = t_plus_5_q1,
    q2 = t_plus_5_q2,
    q3 = t_plus_5_q3,
    q4 = t_plus_5_q4,
    q5 = t_plus_5_q5,
    q6 = t_plus_5_q6,
    q7 = t_plus_5_q7,
    q8 = t_plus_5_q8,
    q9 = t_plus_5_q9
  ),
  t_plus_6 = list(
    q1 = t_plus_6_q1,
    q2 = t_plus_6_q2,
    q3 = t_plus_6_q3,
    q4 = t_plus_6_q4,
    q5 = t_plus_6_q5,
    q6 = t_plus_6_q6,
    q7 = t_plus_6_q7,
    q8 = t_plus_6_q8,
    q9 = t_plus_6_q9
  ),
  t_plus_7 = list(
    q1 = t_plus_7_q1,
    q2 = t_plus_7_q2,
    q3 = t_plus_7_q3,
    q4 = t_plus_7_q4,
    q5 = t_plus_7_q5,
    q6 = t_plus_7_q6,
    q7 = t_plus_7_q7,
    q8 = t_plus_7_q8,
    q9 = t_plus_7_q9
  ),
  t_plus_8 = list(
    q1 = t_plus_8_q1,
    q2 = t_plus_8_q2,
    q3 = t_plus_8_q3,
    q4 = t_plus_8_q4,
    q5 = t_plus_8_q5,
    q6 = t_plus_8_q6,
    q7 = t_plus_8_q7,
    q8 = t_plus_8_q8,
    q9 = t_plus_8_q9
  ),
  t_plus_9 = list(
    q1 = t_plus_9_q1,
    q2 = t_plus_9_q2,
    q3 = t_plus_9_q3,
    q4 = t_plus_9_q4,
    q5 = t_plus_9_q5,
    q6 = t_plus_9_q6,
    q7 = t_plus_9_q7,
    q8 = t_plus_9_q8,
    q9 = t_plus_9_q9
  ),
  t_plus_10 = list(
    q1 = t_plus_10_q1,
    q2 = t_plus_10_q2,
    q3 = t_plus_10_q3,
    q4 = t_plus_10_q4,
    q5 = t_plus_10_q5,
    q6 = t_plus_10_q6,
    q7 = t_plus_10_q7,
    q8 = t_plus_10_q8,
    q9 = t_plus_10_q9
  )
)

# To view the list
print(quantile_treatment_effects)

#--------------------------------------------------------------------------------;
# Preparing table for plot
#--------------------------------------------------------------------------------;

# Creating the tibble for q1
q1_results <- tibble(
  robust_coef = c(t_minus_3_q1["Coef"],t_minus_2_q1["Coef"],t_minus_1_q1["Coef"],t_plus_0_q1["Coef"],t_plus_1_q1["Coef"],t_plus_2_q1["Coef"],t_plus_3_q1["Coef"],t_plus_4_q1["Coef"],t_plus_5_q1["Coef"],t_plus_6_q1["Coef"],t_plus_7_q1["Coef"],t_plus_8_q1["Coef"],t_plus_9_q1["Coef"],t_plus_10_q1["Coef"]),
  se = c(t_minus_3_q1["Std_Err"],t_minus_2_q1["Std_Err"],t_minus_1_q1["Std_Err"],t_plus_0_q1["Std_Err"],t_plus_1_q1["Std_Err"],t_plus_2_q1["Std_Err"],t_plus_3_q1["Std_Err"],t_plus_4_q1["Std_Err"],t_plus_5_q1["Std_Err"],t_plus_6_q1["Std_Err"],t_plus_7_q1["Std_Err"],t_plus_8_q1["Std_Err"],t_plus_9_q1["Std_Err"],t_plus_10_q1["Std_Err"]),
  conf_int_low = c(t_minus_3_q1["Lower_95_CI"],t_minus_2_q1["Lower_95_CI"],t_minus_1_q1["Lower_95_CI"],t_plus_0_q1["Lower_95_CI"],t_plus_1_q1["Lower_95_CI"],t_plus_2_q1["Lower_95_CI"],t_plus_3_q1["Lower_95_CI"],t_plus_4_q1["Lower_95_CI"],t_plus_5_q1["Lower_95_CI"],t_plus_6_q1["Lower_95_CI"],t_plus_7_q1["Lower_95_CI"],t_plus_8_q1["Lower_95_CI"],t_plus_9_q1["Lower_95_CI"],t_plus_10_q1["Lower_95_CI"]),
  conf_int_high = c(t_minus_3_q1["Upper_95_CI"],t_minus_2_q1["Upper_95_CI"],t_minus_1_q1["Upper_95_CI"],t_plus_0_q1["Upper_95_CI"],t_plus_1_q1["Upper_95_CI"],t_plus_2_q1["Upper_95_CI"],t_plus_3_q1["Upper_95_CI"],t_plus_4_q1["Upper_95_CI"],t_plus_5_q1["Upper_95_CI"],t_plus_6_q1["Upper_95_CI"],t_plus_7_q1["Upper_95_CI"],t_plus_8_q1["Upper_95_CI"],t_plus_9_q1["Upper_95_CI"],t_plus_10_q1["Upper_95_CI"]),
  year = c(
    "t_minus_3", "t_minus_2", "t_minus_1", "t_plus_0",
    "t_plus_1", "t_plus_2", "t_plus_3", "t_plus_4",
    "t_plus_5", "t_plus_6", "t_plus_7", "t_plus_8",
    "t_plus_9", "t_plus_10"
  ),
  ord = c(
    -3, -2, -1, 0,
    1, 2, 3, 4,
    5, 6, 7, 8,
    9, 10
  ),
  cat = rep("q1", 14)
)

# Creating the tibble for q2
q2_results <- tibble(
  robust_coef = c(t_minus_3_q2["Coef"],t_minus_2_q2["Coef"],t_minus_1_q2["Coef"],t_plus_0_q2["Coef"],t_plus_1_q2["Coef"],t_plus_2_q2["Coef"],t_plus_3_q2["Coef"],t_plus_4_q2["Coef"],t_plus_5_q2["Coef"],t_plus_6_q2["Coef"],t_plus_7_q2["Coef"],t_plus_8_q2["Coef"],t_plus_9_q2["Coef"],t_plus_10_q2["Coef"]),
  se = c(t_minus_3_q2["Std_Err"],t_minus_2_q2["Std_Err"],t_minus_1_q2["Std_Err"],t_plus_0_q2["Std_Err"],t_plus_1_q2["Std_Err"],t_plus_2_q2["Std_Err"],t_plus_3_q2["Std_Err"],t_plus_4_q2["Std_Err"],t_plus_5_q2["Std_Err"],t_plus_6_q2["Std_Err"],t_plus_7_q2["Std_Err"],t_plus_8_q2["Std_Err"],t_plus_9_q2["Std_Err"],t_plus_10_q2["Std_Err"]),
  conf_int_low = c(t_minus_3_q2["Lower_95_CI"],t_minus_2_q2["Lower_95_CI"],t_minus_1_q2["Lower_95_CI"],t_plus_0_q2["Lower_95_CI"],t_plus_1_q2["Lower_95_CI"],t_plus_2_q2["Lower_95_CI"],t_plus_3_q2["Lower_95_CI"],t_plus_4_q2["Lower_95_CI"],t_plus_5_q2["Lower_95_CI"],t_plus_6_q2["Lower_95_CI"],t_plus_7_q2["Lower_95_CI"],t_plus_8_q2["Lower_95_CI"],t_plus_9_q2["Lower_95_CI"],t_plus_10_q2["Lower_95_CI"]),
  conf_int_high = c(t_minus_3_q2["Upper_95_CI"],t_minus_2_q2["Upper_95_CI"],t_minus_1_q2["Upper_95_CI"],t_plus_0_q2["Upper_95_CI"],t_plus_1_q2["Upper_95_CI"],t_plus_2_q2["Upper_95_CI"],t_plus_3_q2["Upper_95_CI"],t_plus_4_q2["Upper_95_CI"],t_plus_5_q2["Upper_95_CI"],t_plus_6_q2["Upper_95_CI"],t_plus_7_q2["Upper_95_CI"],t_plus_8_q2["Upper_95_CI"],t_plus_9_q2["Upper_95_CI"],t_plus_10_q2["Upper_95_CI"]),
  year = c(
    "t_minus_3", "t_minus_2", "t_minus_1", "t_plus_0",
    "t_plus_1", "t_plus_2", "t_plus_3", "t_plus_4",
    "t_plus_5", "t_plus_6", "t_plus_7", "t_plus_8",
    "t_plus_9", "t_plus_10"
  ),
  ord = c(
    -3, -2, -1, 0,
    1, 2, 3, 4,
    5, 6, 7, 8,
    9, 10
  ),
  cat = rep("q2", 14)
)

# Creating the tibble for q3
q3_results <- tibble(
  robust_coef = c(t_minus_3_q3["Coef"],t_minus_2_q3["Coef"],t_minus_1_q3["Coef"],t_plus_0_q3["Coef"],t_plus_1_q3["Coef"],t_plus_2_q3["Coef"],t_plus_3_q3["Coef"],t_plus_4_q3["Coef"],t_plus_5_q3["Coef"],t_plus_6_q3["Coef"],t_plus_7_q3["Coef"],t_plus_8_q3["Coef"],t_plus_9_q3["Coef"],t_plus_10_q3["Coef"]),
  se = c(t_minus_3_q3["Std_Err"],t_minus_2_q3["Std_Err"],t_minus_1_q3["Std_Err"],t_plus_0_q3["Std_Err"],t_plus_1_q3["Std_Err"],t_plus_2_q3["Std_Err"],t_plus_3_q3["Std_Err"],t_plus_4_q3["Std_Err"],t_plus_5_q3["Std_Err"],t_plus_6_q3["Std_Err"],t_plus_7_q3["Std_Err"],t_plus_8_q3["Std_Err"],t_plus_9_q3["Std_Err"],t_plus_10_q3["Std_Err"]),
  conf_int_low = c(t_minus_3_q3["Lower_95_CI"],t_minus_2_q3["Lower_95_CI"],t_minus_1_q3["Lower_95_CI"],t_plus_0_q3["Lower_95_CI"],t_plus_1_q3["Lower_95_CI"],t_plus_2_q3["Lower_95_CI"],t_plus_3_q3["Lower_95_CI"],t_plus_4_q3["Lower_95_CI"],t_plus_5_q3["Lower_95_CI"],t_plus_6_q3["Lower_95_CI"],t_plus_7_q3["Lower_95_CI"],t_plus_8_q3["Lower_95_CI"],t_plus_9_q3["Lower_95_CI"],t_plus_10_q3["Lower_95_CI"]),
  conf_int_high = c(t_minus_3_q3["Upper_95_CI"],t_minus_2_q3["Upper_95_CI"],t_minus_1_q3["Upper_95_CI"],t_plus_0_q3["Upper_95_CI"],t_plus_1_q3["Upper_95_CI"],t_plus_2_q3["Upper_95_CI"],t_plus_3_q3["Upper_95_CI"],t_plus_4_q3["Upper_95_CI"],t_plus_5_q3["Upper_95_CI"],t_plus_6_q3["Upper_95_CI"],t_plus_7_q3["Upper_95_CI"],t_plus_8_q3["Upper_95_CI"],t_plus_9_q3["Upper_95_CI"],t_plus_10_q3["Upper_95_CI"]),
  year = c(
    "t_minus_3", "t_minus_2", "t_minus_1", "t_plus_0",
    "t_plus_1", "t_plus_2", "t_plus_3", "t_plus_4",
    "t_plus_5", "t_plus_6", "t_plus_7", "t_plus_8",
    "t_plus_9", "t_plus_10"
  ),
  ord = c(
    -3, -2, -1, 0,
    1, 2, 3, 4,
    5, 6, 7, 8,
    9, 10
  ),
  cat = rep("q3", 14)
)

# Creating the tibble for q7
q7_results <- tibble(
  robust_coef = c(t_minus_3_q7["Coef"],t_minus_2_q7["Coef"],t_minus_1_q7["Coef"],t_plus_0_q7["Coef"],t_plus_1_q7["Coef"],t_plus_2_q7["Coef"],t_plus_3_q7["Coef"],t_plus_4_q7["Coef"],t_plus_5_q7["Coef"],t_plus_6_q7["Coef"],t_plus_7_q7["Coef"],t_plus_8_q7["Coef"],t_plus_9_q7["Coef"],t_plus_10_q7["Coef"]),
  se = c(t_minus_3_q7["Std_Err"],t_minus_2_q7["Std_Err"],t_minus_1_q7["Std_Err"],t_plus_0_q7["Std_Err"],t_plus_1_q7["Std_Err"],t_plus_2_q7["Std_Err"],t_plus_3_q7["Std_Err"],t_plus_4_q7["Std_Err"],t_plus_5_q7["Std_Err"],t_plus_6_q7["Std_Err"],t_plus_7_q7["Std_Err"],t_plus_8_q7["Std_Err"],t_plus_9_q7["Std_Err"],t_plus_10_q7["Std_Err"]),
  conf_int_low = c(t_minus_3_q7["Lower_95_CI"],t_minus_2_q7["Lower_95_CI"],t_minus_1_q7["Lower_95_CI"],t_plus_0_q7["Lower_95_CI"],t_plus_1_q7["Lower_95_CI"],t_plus_2_q7["Lower_95_CI"],t_plus_3_q7["Lower_95_CI"],t_plus_4_q7["Lower_95_CI"],t_plus_5_q7["Lower_95_CI"],t_plus_6_q7["Lower_95_CI"],t_plus_7_q7["Lower_95_CI"],t_plus_8_q7["Lower_95_CI"],t_plus_9_q7["Lower_95_CI"],t_plus_10_q7["Lower_95_CI"]),
  conf_int_high = c(t_minus_3_q7["Upper_95_CI"],t_minus_2_q7["Upper_95_CI"],t_minus_1_q7["Upper_95_CI"],t_plus_0_q7["Upper_95_CI"],t_plus_1_q7["Upper_95_CI"],t_plus_2_q7["Upper_95_CI"],t_plus_3_q7["Upper_95_CI"],t_plus_4_q7["Upper_95_CI"],t_plus_5_q7["Upper_95_CI"],t_plus_6_q7["Upper_95_CI"],t_plus_7_q7["Upper_95_CI"],t_plus_8_q7["Upper_95_CI"],t_plus_9_q7["Upper_95_CI"],t_plus_10_q7["Upper_95_CI"]),
  year = c(
    "t_minus_3", "t_minus_2", "t_minus_1", "t_plus_0",
    "t_plus_1", "t_plus_2", "t_plus_3", "t_plus_4",
    "t_plus_5", "t_plus_6", "t_plus_7", "t_plus_8",
    "t_plus_9", "t_plus_10"
  ),
  ord = c(
    -3, -2, -1, 0,
    1, 2, 3, 4,
    5, 6, 7, 8,
    9, 10
  ),
  cat = rep("q7", 14)
)

# Creating the tibble for q8
q8_results <- tibble(
  robust_coef = c(t_minus_3_q8["Coef"],t_minus_2_q8["Coef"],t_minus_1_q8["Coef"],t_plus_0_q8["Coef"],t_plus_1_q8["Coef"],t_plus_2_q8["Coef"],t_plus_3_q8["Coef"],t_plus_4_q8["Coef"],t_plus_5_q8["Coef"],t_plus_6_q8["Coef"],t_plus_7_q8["Coef"],t_plus_8_q8["Coef"],t_plus_9_q8["Coef"],t_plus_10_q8["Coef"]),
  se = c(t_minus_3_q8["Std_Err"],t_minus_2_q8["Std_Err"],t_minus_1_q8["Std_Err"],t_plus_0_q8["Std_Err"],t_plus_1_q8["Std_Err"],t_plus_2_q8["Std_Err"],t_plus_3_q8["Std_Err"],t_plus_4_q8["Std_Err"],t_plus_5_q8["Std_Err"],t_plus_6_q8["Std_Err"],t_plus_7_q8["Std_Err"],t_plus_8_q8["Std_Err"],t_plus_9_q8["Std_Err"],t_plus_10_q8["Std_Err"]),
  conf_int_low = c(t_minus_3_q8["Lower_95_CI"],t_minus_2_q8["Lower_95_CI"],t_minus_1_q8["Lower_95_CI"],t_plus_0_q8["Lower_95_CI"],t_plus_1_q8["Lower_95_CI"],t_plus_2_q8["Lower_95_CI"],t_plus_3_q8["Lower_95_CI"],t_plus_4_q8["Lower_95_CI"],t_plus_5_q8["Lower_95_CI"],t_plus_6_q8["Lower_95_CI"],t_plus_7_q8["Lower_95_CI"],t_plus_8_q8["Lower_95_CI"],t_plus_9_q8["Lower_95_CI"],t_plus_10_q8["Lower_95_CI"]),
  conf_int_high = c(t_minus_3_q8["Upper_95_CI"],t_minus_2_q8["Upper_95_CI"],t_minus_1_q8["Upper_95_CI"],t_plus_0_q8["Upper_95_CI"],t_plus_1_q8["Upper_95_CI"],t_plus_2_q8["Upper_95_CI"],t_plus_3_q8["Upper_95_CI"],t_plus_4_q8["Upper_95_CI"],t_plus_5_q8["Upper_95_CI"],t_plus_6_q8["Upper_95_CI"],t_plus_7_q8["Upper_95_CI"],t_plus_8_q8["Upper_95_CI"],t_plus_9_q8["Upper_95_CI"],t_plus_10_q8["Upper_95_CI"]),
  year = c(
    "t_minus_3", "t_minus_2", "t_minus_1", "t_plus_0",
    "t_plus_1", "t_plus_2", "t_plus_3", "t_plus_4",
    "t_plus_5", "t_plus_6", "t_plus_7", "t_plus_8",
    "t_plus_9", "t_plus_10"
  ),
  ord = c(
    -3, -2, -1, 0,
    1, 2, 3, 4,
    5, 6, 7, 8,
    9, 10
  ),
  cat = rep("q8", 14)
)

# Creating the tibble for q9
q9_results <- tibble(
  robust_coef = c(t_minus_3_q9["Coef"],t_minus_2_q9["Coef"],t_minus_1_q9["Coef"],t_plus_0_q9["Coef"],t_plus_1_q9["Coef"],t_plus_2_q9["Coef"],t_plus_3_q9["Coef"],t_plus_4_q9["Coef"],t_plus_5_q9["Coef"],t_plus_6_q9["Coef"],t_plus_7_q9["Coef"],t_plus_8_q9["Coef"],t_plus_9_q9["Coef"],t_plus_10_q9["Coef"]),
  se = c(t_minus_3_q9["Std_Err"],t_minus_2_q9["Std_Err"],t_minus_1_q9["Std_Err"],t_plus_0_q9["Std_Err"],t_plus_1_q9["Std_Err"],t_plus_2_q9["Std_Err"],t_plus_3_q9["Std_Err"],t_plus_4_q9["Std_Err"],t_plus_5_q9["Std_Err"],t_plus_6_q9["Std_Err"],t_plus_7_q9["Std_Err"],t_plus_8_q9["Std_Err"],t_plus_9_q9["Std_Err"],t_plus_10_q9["Std_Err"]),
  conf_int_low = c(t_minus_3_q9["Lower_95_CI"],t_minus_2_q9["Lower_95_CI"],t_minus_1_q9["Lower_95_CI"],t_plus_0_q9["Lower_95_CI"],t_plus_1_q9["Lower_95_CI"],t_plus_2_q9["Lower_95_CI"],t_plus_3_q9["Lower_95_CI"],t_plus_4_q9["Lower_95_CI"],t_plus_5_q9["Lower_95_CI"],t_plus_6_q9["Lower_95_CI"],t_plus_7_q9["Lower_95_CI"],t_plus_8_q9["Lower_95_CI"],t_plus_9_q9["Lower_95_CI"],t_plus_10_q9["Lower_95_CI"]),
  conf_int_high = c(t_minus_3_q9["Upper_95_CI"],t_minus_2_q9["Upper_95_CI"],t_minus_1_q9["Upper_95_CI"],t_plus_0_q9["Upper_95_CI"],t_plus_1_q9["Upper_95_CI"],t_plus_2_q9["Upper_95_CI"],t_plus_3_q9["Upper_95_CI"],t_plus_4_q9["Upper_95_CI"],t_plus_5_q9["Upper_95_CI"],t_plus_6_q9["Upper_95_CI"],t_plus_7_q9["Upper_95_CI"],t_plus_8_q9["Upper_95_CI"],t_plus_9_q9["Upper_95_CI"],t_plus_10_q9["Upper_95_CI"]),
  year = c(
    "t_minus_3", "t_minus_2", "t_minus_1", "t_plus_0",
    "t_plus_1", "t_plus_2", "t_plus_3", "t_plus_4",
    "t_plus_5", "t_plus_6", "t_plus_7", "t_plus_8",
    "t_plus_9", "t_plus_10"
  ),
  ord = c(
    -3, -2, -1, 0,
    1, 2, 3, 4,
    5, 6, 7, 8,
    9, 10
  ),
  cat = rep("q9", 14)
)

# combining all the tibbles
q_results <- bind_rows(q2_results, q8_results)

# Apply jitter to the 'ord' column based on the 'cat' category
q_results <- q_results %>%
  mutate(ord = case_when(
    cat == "q7" ~ ord,                 # No jitter for the base category
    cat == "q2" ~ ord - 0.15,
    cat == "q1" ~ ord - 0.30,
    cat == "q8" ~ ord + 0.15,
    cat == "q9" ~ ord + 0.30,
    # cat == "q9" ~ ord + 0.45,
    TRUE ~ ord  # Default case if needed
),
  quantile = case_when(
    cat == "q1" ~ "10%",
    cat == "q2" ~ "20%",
    cat == "q3" ~ "30%",
    cat == "q7" ~ "70%",
    cat == "q8" ~ "80%",
    cat == "q9" ~ "90%",
    TRUE ~ "q1"
  )
)

# Display the jittered table
print(q_results %>% filter(ord >= 4), n = 84)

ggplot(q_results, aes(ord, robust_coef, color = quantile)) +       
  geom_point(size = 3, shape = 19) +
  geom_errorbar(aes(ymin = conf_int_low, ymax = conf_int_high, color = quantile), 
                width = 0.2, color = "grey50", size = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = 1) +
  labs(
    x = "Year",
    y = "Treatment Effect",
    color = "Position",
    title = "Treatment Effects: by quantile of Median Sale Amount"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom",
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank(), 
    panel.grid.minor.y = element_blank(),
    legend.title = element_blank()
  ) + 
  scale_x_continuous(
    breaks = c(-3:10)
)

#--------------------------------------------------------------------------------;
# Running with covariates | Extracting only q2 and q8
#--------------------------------------------------------------------------------;

# Extracted values for each period

# t_minus_3
t_minus_3_q2_covs <- c(Coef = -680.9492, Std_Err = 12456.78, Lower_95_CI = -25095.79, Upper_95_CI = 23733.9)
t_minus_3_q8_covs <- c(Coef = -9020, Std_Err = 15188.72, Lower_95_CI = -38789.34, Upper_95_CI = 20749.34)

# t_minus_2
t_minus_2_q2_covs <- c(Coef = 0, Std_Err = 12628.4, Lower_95_CI = -24751.22, Upper_95_CI = 24751.22)
t_minus_2_q8_covs <- c(Coef = -31003, Std_Err = 16098.54, Lower_95_CI = -62555.57, Upper_95_CI = 549.568)

# t_minus_1
t_minus_1_q2_covs <- c(Coef = 0, Std_Err = 14963.71, Lower_95_CI = -29328.32, Upper_95_CI = 29328.32)
t_minus_1_q8_covs <- c(Coef = -17810, Std_Err = 14925.45, Lower_95_CI = -47063.35, Upper_95_CI = 11443.35)

# t_plus_0
t_plus_0_q2_covs <- c(Coef = -9540, Std_Err = 11374.04, Lower_95_CI = -31832.7, Upper_95_CI = 12752.7)
t_plus_0_q8_covs <- c(Coef = -17970, Std_Err = 14653.97, Lower_95_CI = -46691.24, Upper_95_CI = 10751.24)

# t_plus_1
t_plus_1_q2_covs <- c(Coef = -4464, Std_Err = 9766.189, Lower_95_CI = -23605.38, Upper_95_CI = 14677.38)
t_plus_1_q8_covs <- c(Coef = -7806, Std_Err = 13103.15, Lower_95_CI = -33487.71, Upper_95_CI = 17875.71)

# t_plus_2
t_plus_2_q2_covs <- c(Coef = -4621.5, Std_Err = 12087.79, Lower_95_CI = -28313.14, Upper_95_CI = 19070.14)
t_plus_2_q8_covs <- c(Coef = -6975, Std_Err = 17106.74, Lower_95_CI = -40503.6, Upper_95_CI = 26553.6)

# t_plus_3
t_plus_3_q2_covs <- c(Coef = -774.8008, Std_Err = 9091.019, Lower_95_CI = -18592.87, Upper_95_CI = 17043.27)
t_plus_3_q8_covs <- c(Coef = -15932, Std_Err = 13781.57, Lower_95_CI = -42943.37, Upper_95_CI = 11079.37)

# t_plus_4
t_plus_4_q2_covs <- c(Coef = -5400, Std_Err = 8680.23, Lower_95_CI = -22412.94, Upper_95_CI = 11612.94)
t_plus_4_q8_covs <- c(Coef = -28477.5, Std_Err = 15436.49, Lower_95_CI = -58732.46, Upper_95_CI = 1777.461)

# t_plus_5
t_plus_5_q2_covs <- c(Coef = -17065, Std_Err = 10237.68, Lower_95_CI = -37130.49, Upper_95_CI = 3000.493)
t_plus_5_q8_covs <- c(Coef = -17656, Std_Err = 10120.69, Lower_95_CI = -37492.18, Upper_95_CI = 2180.181)

# t_plus_6
t_plus_6_q2_covs <- c(Coef = 4014.398, Std_Err = 12279.83, Lower_95_CI = -20053.63, Upper_95_CI = 28082.42)
t_plus_6_q8_covs <- c(Coef = -38460, Std_Err = 16850.34, Lower_95_CI = -71486.07, Upper_95_CI = -5433.931)

# t_plus_7
t_plus_7_q2_covs <- c(Coef = -14682, Std_Err = 8006.845, Lower_95_CI = -30375.13, Upper_95_CI = 1011.128)
t_plus_7_q8_covs <- c(Coef = -37470, Std_Err = 12707.57, Lower_95_CI = -62376.38, Upper_95_CI = -12563.62)

# t_plus_8
t_plus_8_q2_covs <- c(Coef = -15039.5, Std_Err = 6041.702, Lower_95_CI = -26881.02, Upper_95_CI = -3197.981)
t_plus_8_q8_covs <- c(Coef = -28950, Std_Err = 12204.37, Lower_95_CI = -52870.13, Upper_95_CI = -5029.866)

# t_plus_9
t_plus_9_q2_covs <- c(Coef = -2191.5, Std_Err = 11031.8, Lower_95_CI = -23813.43, Upper_95_CI = 19430.43)
t_plus_9_q8_covs <- c(Coef = -27800, Std_Err = 12157.61, Lower_95_CI = -51628.47, Upper_95_CI = -3971.53)

# t_plus_10
t_plus_10_q2_covs <- c(Coef = 1746.25, Std_Err = 9815.47, Lower_95_CI = -17491.72, Upper_95_CI = 20984.22)
t_plus_10_q8_covs <- c(Coef = -18657.95, Std_Err = 12400.89, Lower_95_CI = -42963.26, Upper_95_CI = 5647.35)


# Creating the tibble for q2
q2_results_covs <- tibble(
  robust_coef = c(t_minus_3_q2_covs["Coef"],t_minus_2_q2_covs["Coef"],t_minus_1_q2_covs["Coef"],t_plus_0_q2_covs["Coef"],t_plus_1_q2_covs["Coef"],t_plus_2_q2_covs["Coef"],t_plus_3_q2_covs["Coef"],t_plus_4_q2_covs["Coef"],t_plus_5_q2_covs["Coef"],t_plus_6_q2_covs["Coef"],t_plus_7_q2_covs["Coef"],t_plus_8_q2_covs["Coef"],t_plus_9_q2_covs["Coef"],t_plus_10_q2_covs["Coef"]),
  se = c(t_minus_3_q2_covs["Std_Err"],t_minus_2_q2_covs["Std_Err"],t_minus_1_q2_covs["Std_Err"],t_plus_0_q2_covs["Std_Err"],t_plus_1_q2_covs["Std_Err"],t_plus_2_q2_covs["Std_Err"],t_plus_3_q2_covs["Std_Err"],t_plus_4_q2_covs["Std_Err"],t_plus_5_q2_covs["Std_Err"],t_plus_6_q2_covs["Std_Err"],t_plus_7_q2_covs["Std_Err"],t_plus_8_q2_covs["Std_Err"],t_plus_9_q2_covs["Std_Err"],t_plus_10_q2_covs["Std_Err"]),
  conf_int_low = c(t_minus_3_q2_covs["Lower_95_CI"],t_minus_2_q2_covs["Lower_95_CI"],t_minus_1_q2_covs["Lower_95_CI"],t_plus_0_q2_covs["Lower_95_CI"],t_plus_1_q2_covs["Lower_95_CI"],t_plus_2_q2_covs["Lower_95_CI"],t_plus_3_q2_covs["Lower_95_CI"],t_plus_4_q2_covs["Lower_95_CI"],t_plus_5_q2_covs["Lower_95_CI"],t_plus_6_q2_covs["Lower_95_CI"],t_plus_7_q2_covs["Lower_95_CI"],t_plus_8_q2_covs["Lower_95_CI"],t_plus_9_q2_covs["Lower_95_CI"],t_plus_10_q2_covs["Lower_95_CI"]),
  conf_int_high = c(t_minus_3_q2_covs["Upper_95_CI"],t_minus_2_q2_covs["Upper_95_CI"],t_minus_1_q2_covs["Upper_95_CI"],t_plus_0_q2_covs["Upper_95_CI"],t_plus_1_q2_covs["Upper_95_CI"],t_plus_2_q2_covs["Upper_95_CI"],t_plus_3_q2_covs["Upper_95_CI"],t_plus_4_q2_covs["Upper_95_CI"],t_plus_5_q2_covs["Upper_95_CI"],t_plus_6_q2_covs["Upper_95_CI"],t_plus_7_q2_covs["Upper_95_CI"],t_plus_8_q2_covs["Upper_95_CI"],t_plus_9_q2_covs["Upper_95_CI"],t_plus_10_q2_covs["Upper_95_CI"]),
  year = c(
    "t_minus_3", "t_minus_2", "t_minus_1", "t_plus_0",
    "t_plus_1", "t_plus_2", "t_plus_3", "t_plus_4",
    "t_plus_5", "t_plus_6", "t_plus_7", "t_plus_8",
    "t_plus_9", "t_plus_10"
  ),
  ord = c(
    -3, -2, -1, 0,
    1, 2, 3, 4,
    5, 6, 7, 8,
    9, 10
  ),
  cat = rep("q2", 14)
)

# Creating the tibble for q8
q8_results_covs <- tibble(
  robust_coef = c(t_minus_3_q8_covs["Coef"],t_minus_2_q8_covs["Coef"],t_minus_1_q8_covs["Coef"],t_plus_0_q8_covs["Coef"],t_plus_1_q8_covs["Coef"],t_plus_2_q8_covs["Coef"],t_plus_3_q8_covs["Coef"],t_plus_4_q8_covs["Coef"],t_plus_5_q8_covs["Coef"],t_plus_6_q8_covs["Coef"],t_plus_7_q8_covs["Coef"],t_plus_8_q8_covs["Coef"],t_plus_9_q8_covs["Coef"],t_plus_10_q8_covs["Coef"]),
  se = c(t_minus_3_q8_covs["Std_Err"],t_minus_2_q8_covs["Std_Err"],t_minus_1_q8_covs["Std_Err"],t_plus_0_q8_covs["Std_Err"],t_plus_1_q8_covs["Std_Err"],t_plus_2_q8_covs["Std_Err"],t_plus_3_q8_covs["Std_Err"],t_plus_4_q8_covs["Std_Err"],t_plus_5_q8_covs["Std_Err"],t_plus_6_q8_covs["Std_Err"],t_plus_7_q8_covs["Std_Err"],t_plus_8_q8_covs["Std_Err"],t_plus_9_q8_covs["Std_Err"],t_plus_10_q8_covs["Std_Err"]),
  conf_int_low = c(t_minus_3_q8_covs["Lower_95_CI"],t_minus_2_q8_covs["Lower_95_CI"],t_minus_1_q8_covs["Lower_95_CI"],t_plus_0_q8_covs["Lower_95_CI"],t_plus_1_q8_covs["Lower_95_CI"],t_plus_2_q8_covs["Lower_95_CI"],t_plus_3_q8_covs["Lower_95_CI"],t_plus_4_q8_covs["Lower_95_CI"],t_plus_5_q8_covs["Lower_95_CI"],t_plus_6_q8_covs["Lower_95_CI"],t_plus_7_q8_covs["Lower_95_CI"],t_plus_8_q8_covs["Lower_95_CI"],t_plus_9_q8_covs["Lower_95_CI"],t_plus_10_q8_covs["Lower_95_CI"]),
  conf_int_high = c(t_minus_3_q8_covs["Upper_95_CI"],t_minus_2_q8_covs["Upper_95_CI"],t_minus_1_q8_covs["Upper_95_CI"],t_plus_0_q8_covs["Upper_95_CI"],t_plus_1_q8_covs["Upper_95_CI"],t_plus_2_q8_covs["Upper_95_CI"],t_plus_3_q8_covs["Upper_95_CI"],t_plus_4_q8_covs["Upper_95_CI"],t_plus_5_q8_covs["Upper_95_CI"],t_plus_6_q8_covs["Upper_95_CI"],t_plus_7_q8_covs["Upper_95_CI"],t_plus_8_q8_covs["Upper_95_CI"],t_plus_9_q8_covs["Upper_95_CI"],t_plus_10_q8_covs["Upper_95_CI"]),
  year = c(
    "t_minus_3", "t_minus_2", "t_minus_1", "t_plus_0",
    "t_plus_1", "t_plus_2", "t_plus_3", "t_plus_4",
    "t_plus_5", "t_plus_6", "t_plus_7", "t_plus_8",
    "t_plus_9", "t_plus_10"
  ),
  ord = c(
    -3, -2, -1, 0,
    1, 2, 3, 4,
    5, 6, 7, 8,
    9, 10
  ),
  cat = rep("q8", 14)
)

# combining all the tibbles
q_results_covs <- bind_rows(q2_results_covs, q8_results_covs)

print(q_results_covs, n = 28)


# Apply jitter to the 'ord' column based on the 'cat' category
q_results_covs <- q_results_covs %>%
  mutate(ord = case_when(
    cat == "q7" ~ ord,                 # No jitter for the base category
    cat == "q2" ~ ord - 0.15,
    cat == "q1" ~ ord - 0.30,
    cat == "q8" ~ ord + 0.15,
    cat == "q9" ~ ord + 0.30,
    # cat == "q9" ~ ord + 0.45,
    TRUE ~ ord  # Default case if needed
  ),
  quantile = case_when(
    cat == "q1" ~ "10%",
    cat == "q2" ~ "20%",
    cat == "q3" ~ "30%",
    cat == "q7" ~ "70%",
    cat == "q8" ~ "80%",
    cat == "q9" ~ "90%",
    TRUE ~ "q1"
  )
  )

# Display the jittered table
print(q_results_covs %>% filter(ord >= 4), n = 84)

ggplot(q_results_covs, aes(ord, robust_coef, color = quantile)) +       
  geom_point(size = 3, shape = 19) +
  geom_errorbar(aes(ymin = conf_int_low, ymax = conf_int_high, color = quantile), 
                width = 0.2, color = "grey50", size = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = 1) +
  labs(
    x = "Year",
    y = "Treatment Effect",
    color = "Position",
    title = "Treatment Effects: by quantile of Median Sale Amount",
    subtitle = "with covariates"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom",
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank(), 
    panel.grid.minor.y = element_blank(),
    legend.title = element_blank()
  ) + 
  scale_x_continuous(
    breaks = c(-3:10)
  )
