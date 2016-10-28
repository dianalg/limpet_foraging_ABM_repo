# plot_counts.R

# Diana LaScala-Gruenewald
# 8/24/16
# R 3.3.0

plot_counts <- function(count_data, xdata, scenario) {
  
  ### Takes count_data (like cBR_vulgata1_viable) and plots how many viable instances are in
  ### count data for various combinations of parameters.  sightangle1 and sightangle2 are
  ### 1.047198 and 6.283185 respectively, but need to put in a reference to count_data, because
  ### floats are not exact...
  
  if (scenario <= 1) {
    
    # Calculate alpha counts:
    
    viable1 = count_data < 1
    
    counts_map4_alpha1 = sum(viable1[4, c(1, 11, 21, 31, 41, 51, 61, 71, 81, 91, 101, 111)] == T)
    counts_map5_alpha1 = sum(viable1[5, c(1, 11, 21, 31, 41, 51, 61, 71, 81, 91, 101, 111)] == T)
    counts_map11_alpha1 = sum(viable1[11, c(1, 11, 21, 31, 41, 51, 61, 71, 81, 91, 101, 111)] == T)
    counts_map12_alpha1 = sum(viable1[12, c(1, 11, 21, 31, 41, 51, 61, 71, 81, 91, 101, 111)] == T)
    counts_map19_alpha1 = sum(viable1[19, c(1, 11, 21, 31, 41, 51, 61, 71, 81, 91, 101, 111)] == T)
    counts_map20_alpha1 = sum(viable1[20, c(1, 11, 21, 31, 41, 51, 61, 71, 81, 91, 101, 111)] == T)
    counts_map21_alpha1 = sum(viable1[21, c(1, 11, 21, 31, 41, 51, 61, 71, 81, 91, 101, 111)] == T)
    counts_map4_alpha1.5 = sum(viable1[4, c(3, 13, 23, 33, 43, 53, 63, 73, 83, 93, 103, 113)] == T)
    counts_map5_alpha1.5 = sum(viable1[5, c(3, 13, 23, 33, 43, 53, 63, 73, 83, 93, 103, 113)] == T)
    counts_map11_alpha1.5 = sum(viable1[11, c(3, 13, 23, 33, 43, 53, 63, 73, 83, 93, 103, 113)] == T)
    counts_map12_alpha1.5 = sum(viable1[12, c(3, 13, 23, 33, 43, 53, 63, 73, 83, 93, 103, 113)] == T)
    counts_map19_alpha1.5 = sum(viable1[19, c(3, 13, 23, 33, 43, 53, 63, 73, 83, 93, 103, 113)] == T)
    counts_map20_alpha1.5 = sum(viable1[20, c(3, 13, 23, 33, 43, 53, 63, 73, 83, 93, 103, 113)] == T)
    counts_map21_alpha1.5 = sum(viable1[21, c(3, 13, 23, 33, 43, 53, 63, 73, 83, 93, 103, 113)] == T)
    counts_map4_alpha2 = sum(viable1[4, c(5, 15, 25, 35, 45, 55, 65, 75, 85, 95, 105, 115)] == T)
    counts_map5_alpha2 = sum(viable1[5, c(5, 15, 25, 35, 45, 55, 65, 75, 85, 95, 105, 115)] == T)
    counts_map11_alpha2 = sum(viable1[11, c(5, 15, 25, 35, 45, 55, 65, 75, 85, 95, 105, 115)] == T)
    counts_map12_alpha2 = sum(viable1[12, c(5, 15, 25, 35, 45, 55, 65, 75, 85, 95, 105, 115)] == T)
    counts_map19_alpha2 = sum(viable1[19, c(5, 15, 25, 35, 45, 55, 65, 75, 85, 95, 105, 115)] == T)
    counts_map20_alpha2 = sum(viable1[20, c(5, 15, 25, 35, 45, 55, 65, 75, 85, 95, 105, 115)] == T)
    counts_map21_alpha2 = sum(viable1[21, c(5, 15, 25, 35, 45, 55, 65, 75, 85, 95, 105, 115)] == T)
    counts_map4_alpha2.5 = sum(viable1[4, c(7, 17, 27, 37, 47, 57, 67, 77, 87, 97, 107, 117)] == T)
    counts_map5_alpha2.5 = sum(viable1[5, c(7, 17, 27, 37, 47, 57, 67, 77, 87, 97, 107, 117)] == T)
    counts_map11_alpha2.5 = sum(viable1[11, c(7, 17, 27, 37, 47, 57, 67, 77, 87, 97, 107, 117)] == T)
    counts_map12_alpha2.5 = sum(viable1[12, c(7, 17, 27, 37, 47, 57, 67, 77, 87, 97, 107, 117)] == T)
    counts_map19_alpha2.5 = sum(viable1[19, c(7, 17, 27, 37, 47, 57, 67, 77, 87, 97, 107, 117)] == T)
    counts_map20_alpha2.5 = sum(viable1[20, c(7, 17, 27, 37, 47, 57, 67, 77, 87, 97, 107, 117)] == T)
    counts_map21_alpha2.5 = sum(viable1[21, c(7, 17, 27, 37, 47, 57, 67, 77, 87, 97, 107, 117)] == T)
    counts_map4_alpha3 = sum(viable1[4, c(9, 19, 29, 39, 49, 59, 69, 79, 89, 99, 109, 119)] == T)
    counts_map5_alpha3 = sum(viable1[5, c(9, 19, 29, 39, 49, 59, 69, 79, 89, 99, 109, 119)] == T)
    counts_map11_alpha3 = sum(viable1[11, c(9, 19, 29, 39, 49, 59, 69, 79, 89, 99, 109, 119)] == T)
    counts_map12_alpha3 = sum(viable1[12, c(9, 19, 29, 39, 49, 59, 69, 79, 89, 99, 109, 119)] == T)
    counts_map19_alpha3 = sum(viable1[19, c(9, 19, 29, 39, 49, 59, 69, 79, 89, 99, 109, 119)] == T)
    counts_map20_alpha3 = sum(viable1[20, c(9, 19, 29, 39, 49, 59, 69, 79, 89, 99, 109, 119)] == T)
    counts_map21_alpha3 = sum(viable1[21, c(9, 19, 29, 39, 49, 59, 69, 79, 89, 99, 109, 119)] == T)
    
    alpha1_counts = c(counts_map11_alpha1, counts_map19_alpha1, counts_map20_alpha1, counts_map4_alpha1,
                      counts_map5_alpha1, counts_map21_alpha1, counts_map12_alpha1)
    alpha1.5_counts = c(counts_map11_alpha1.5, counts_map19_alpha1.5, counts_map20_alpha1.5, counts_map4_alpha1.5,
                        counts_map5_alpha1.5, counts_map21_alpha1.5, counts_map12_alpha1.5)
    alpha2_counts = c(counts_map11_alpha2, counts_map19_alpha2, counts_map20_alpha2, counts_map4_alpha2,
                      counts_map5_alpha2, counts_map21_alpha2, counts_map12_alpha2)
    alpha2.5_counts = c(counts_map11_alpha2.5, counts_map19_alpha2.5, counts_map20_alpha2.5, counts_map4_alpha2.5,
                        counts_map5_alpha2.5, counts_map21_alpha2.5, counts_map12_alpha2.5)
    alpha3_counts = c(counts_map11_alpha3, counts_map19_alpha3, counts_map20_alpha3, counts_map4_alpha3,
                      counts_map5_alpha3, counts_map21_alpha3, counts_map12_alpha3)
    
    # Calculate sight counts:
    
    counts_map4_sight0 = sum(viable1[4, c(1, 3, 5, 7, 9, 61, 63, 65, 67, 69)] == T)
    counts_map5_sight0 = sum(viable1[5, c(1, 3, 5, 7, 9, 61, 63, 65, 67, 69)] == T)
    counts_map11_sight0 = sum(viable1[11, c(1, 3, 5, 7, 9, 61, 63, 65, 67, 69)] == T)
    counts_map12_sight0 = sum(viable1[12, c(1, 3, 5, 7, 9, 61, 63, 65, 67, 69)] == T)
    counts_map19_sight0 = sum(viable1[19, c(1, 3, 5, 7, 9, 61, 63, 65, 67, 69)] == T)
    counts_map20_sight0 = sum(viable1[20, c(1, 3, 5, 7, 9, 61, 63, 65, 67, 69)] == T)
    counts_map21_sight0 = sum(viable1[21, c(1, 3, 5, 7, 9, 61, 63, 65, 67, 69)] == T)
    counts_map4_sight1 = sum(viable1[4, c(11, 13, 15, 17, 19, 71, 73, 75, 77, 79)] == T)
    counts_map5_sight1 = sum(viable1[5, c(11, 13, 15, 17, 19, 71, 73, 75, 77, 79)] == T)
    counts_map11_sight1 = sum(viable1[11, c(11, 13, 15, 17, 19, 71, 73, 75, 77, 79)] == T)
    counts_map12_sight1 = sum(viable1[12, c(11, 13, 15, 17, 19, 71, 73, 75, 77, 79)] == T)
    counts_map19_sight1 = sum(viable1[19, c(11, 13, 15, 17, 19, 71, 73, 75, 77, 79)] == T)
    counts_map20_sight1 = sum(viable1[20, c(11, 13, 15, 17, 19, 71, 73, 75, 77, 79)] == T)
    counts_map21_sight1 = sum(viable1[21, c(11, 13, 15, 17, 19, 71, 73, 75, 77, 79)] == T)
    counts_map4_sight2 = sum(viable1[4, c(21, 23, 25, 27, 29, 81, 83, 85, 87, 89)] == T)
    counts_map5_sight2 = sum(viable1[5, c(21, 23, 25, 27, 29, 81, 83, 85, 87, 89)] == T)
    counts_map11_sight2 = sum(viable1[11, c(21, 23, 25, 27, 29, 81, 83, 85, 87, 89)] == T)
    counts_map12_sight2 = sum(viable1[12, c(21, 23, 25, 27, 29, 81, 83, 85, 87, 89)] == T)
    counts_map19_sight2 = sum(viable1[19, c(21, 23, 25, 27, 29, 81, 83, 85, 87, 89)] == T)
    counts_map20_sight2 = sum(viable1[20, c(21, 23, 25, 27, 29, 81, 83, 85, 87, 89)] == T)
    counts_map21_sight2 = sum(viable1[21, c(21, 23, 25, 27, 29, 81, 83, 85, 87, 89)] == T)
    counts_map4_sight5 = sum(viable1[4, c(31, 33, 35, 37, 39, 91, 93, 95, 97, 99)] == T)
    counts_map5_sight5 = sum(viable1[5, c(31, 33, 35, 37, 39, 91, 93, 95, 97, 99)] == T)
    counts_map11_sight5 = sum(viable1[11, c(31, 33, 35, 37, 39, 91, 93, 95, 97, 99)] == T)
    counts_map12_sight5 = sum(viable1[12, c(31, 33, 35, 37, 39, 91, 93, 95, 97, 99)] == T)
    counts_map19_sight5 = sum(viable1[19, c(31, 33, 35, 37, 39, 91, 93, 95, 97, 99)] == T)
    counts_map20_sight5 = sum(viable1[20, c(31, 33, 35, 37, 39, 91, 93, 95, 97, 99)] == T)
    counts_map21_sight5 = sum(viable1[21, c(31, 33, 35, 37, 39, 91, 93, 95, 97, 99)] == T)
    counts_map4_sight10 = sum(viable1[4, c(41, 43, 45, 47, 49, 101, 103, 105, 107, 109)] == T)
    counts_map5_sight10 = sum(viable1[5, c(41, 43, 45, 47, 49, 101, 103, 105, 107, 109)] == T)
    counts_map11_sight10 = sum(viable1[11, c(41, 43, 45, 47, 49, 101, 103, 105, 107, 109)] == T)
    counts_map12_sight10 = sum(viable1[12, c(41, 43, 45, 47, 49, 101, 103, 105, 107, 109)] == T)
    counts_map19_sight10 = sum(viable1[19, c(41, 43, 45, 47, 49, 101, 103, 105, 107, 109)] == T)
    counts_map20_sight10 = sum(viable1[20, c(41, 43, 45, 47, 49, 101, 103, 105, 107, 109)] == T)
    counts_map21_sight10 = sum(viable1[21, c(41, 43, 45, 47, 49, 101, 103, 105, 107, 109)] == T)
    counts_map4_sight20 = sum(viable1[4, c(51, 53, 55, 57, 59, 111, 113, 115, 117, 119)] == T)
    counts_map5_sight20 = sum(viable1[5, c(51, 53, 55, 57, 59, 111, 113, 115, 117, 119)] == T)
    counts_map11_sight20 = sum(viable1[11, c(51, 53, 55, 57, 59, 111, 113, 115, 117, 119)] == T)
    counts_map12_sight20 = sum(viable1[12, c(51, 53, 55, 57, 59, 111, 113, 115, 117, 119)] == T)
    counts_map19_sight20 = sum(viable1[19, c(51, 53, 55, 57, 59, 111, 113, 115, 117, 119)] == T)
    counts_map20_sight20 = sum(viable1[20, c(51, 53, 55, 57, 59, 111, 113, 115, 117, 119)] == T)
    counts_map21_sight20 = sum(viable1[21, c(51, 53, 55, 57, 59, 111, 113, 115, 117, 119)] == T)
    
    sight0_counts = c(counts_map11_sight0, counts_map19_sight0, counts_map20_sight0, counts_map4_sight0,
                          counts_map5_sight0, counts_map21_sight0, counts_map12_sight0)
    sight1_counts = c(counts_map11_sight1, counts_map19_sight1, counts_map20_sight1, counts_map4_sight1,
                          counts_map5_sight1, counts_map21_sight1, counts_map12_sight1)
    sight2_counts = c(counts_map11_sight2, counts_map19_sight2, counts_map20_sight2, counts_map4_sight2,
                          counts_map5_sight2, counts_map21_sight2, counts_map12_sight2)
    sight5_counts = c(counts_map11_sight5, counts_map19_sight5, counts_map20_sight5, counts_map4_sight5,
                          counts_map5_sight5, counts_map21_sight5, counts_map12_sight5)
    sight10_counts = c(counts_map11_sight10, counts_map19_sight10, counts_map20_sight10, counts_map4_sight10,
                           counts_map5_sight10, counts_map21_sight10, counts_map12_sight10)
    sight20_counts = c(counts_map11_sight20, counts_map19_sight20, counts_map20_sight20, counts_map4_sight20,
                           counts_map5_sight20, counts_map21_sight20, counts_map12_sight20)
    
    
    # Calculate sight counts w/ sightangle = 60:
    
    counts_map4_sight0 = sum(viable1[4, c(1, 3, 5, 7, 9)] == T)
    counts_map5_sight0 = sum(viable1[5, c(1, 3, 5, 7, 9)] == T)
    counts_map11_sight0 = sum(viable1[11, c(1, 3, 5, 7, 9)] == T)
    counts_map12_sight0 = sum(viable1[12, c(1, 3, 5, 7, 9)] == T)
    counts_map19_sight0 = sum(viable1[19, c(1, 3, 5, 7, 9)] == T)
    counts_map20_sight0 = sum(viable1[20, c(1, 3, 5, 7, 9)] == T)
    counts_map21_sight0 = sum(viable1[21, c(1, 3, 5, 7, 9)] == T)
    counts_map4_sight1 = sum(viable1[4, c(11, 13, 15, 17, 19)] == T)
    counts_map5_sight1 = sum(viable1[5, c(11, 13, 15, 17, 19)] == T)
    counts_map11_sight1 = sum(viable1[11, c(11, 13, 15, 17, 19)] == T)
    counts_map12_sight1 = sum(viable1[12, c(11, 13, 15, 17, 19)] == T)
    counts_map19_sight1 = sum(viable1[19, c(11, 13, 15, 17, 19)] == T)
    counts_map20_sight1 = sum(viable1[20, c(11, 13, 15, 17, 19)] == T)
    counts_map21_sight1 = sum(viable1[21, c(11, 13, 15, 17, 19)] == T)
    counts_map4_sight2 = sum(viable1[4, c(21, 23, 25, 27, 29)] == T)
    counts_map5_sight2 = sum(viable1[5, c(21, 23, 25, 27, 29)] == T)
    counts_map11_sight2 = sum(viable1[11, c(21, 23, 25, 27, 29)] == T)
    counts_map12_sight2 = sum(viable1[12, c(21, 23, 25, 27, 29)] == T)
    counts_map19_sight2 = sum(viable1[19, c(21, 23, 25, 27, 29)] == T)
    counts_map20_sight2 = sum(viable1[20, c(21, 23, 25, 27, 29)] == T)
    counts_map21_sight2 = sum(viable1[21, c(21, 23, 25, 27, 29)] == T)
    counts_map4_sight5 = sum(viable1[4, c(31, 33, 35, 37, 39)] == T)
    counts_map5_sight5 = sum(viable1[5, c(31, 33, 35, 37, 39)] == T)
    counts_map11_sight5 = sum(viable1[11, c(31, 33, 35, 37, 39)] == T)
    counts_map12_sight5 = sum(viable1[12, c(31, 33, 35, 37, 39)] == T)
    counts_map19_sight5 = sum(viable1[19, c(31, 33, 35, 37, 39)] == T)
    counts_map20_sight5 = sum(viable1[20, c(31, 33, 35, 37, 39)] == T)
    counts_map21_sight5 = sum(viable1[21, c(31, 33, 35, 37, 39)] == T)
    counts_map4_sight10 = sum(viable1[4, c(41, 43, 45, 47, 49)] == T)
    counts_map5_sight10 = sum(viable1[5, c(41, 43, 45, 47, 49)] == T)
    counts_map11_sight10 = sum(viable1[11, c(41, 43, 45, 47, 49)] == T)
    counts_map12_sight10 = sum(viable1[12, c(41, 43, 45, 47, 49)] == T)
    counts_map19_sight10 = sum(viable1[19, c(41, 43, 45, 47, 49)] == T)
    counts_map20_sight10 = sum(viable1[20, c(41, 43, 45, 47, 49)] == T)
    counts_map21_sight10 = sum(viable1[21, c(41, 43, 45, 47, 49)] == T)
    counts_map4_sight20 = sum(viable1[4, c(51, 53, 55, 57, 59)] == T)
    counts_map5_sight20 = sum(viable1[5, c(51, 53, 55, 57, 59)] == T)
    counts_map11_sight20 = sum(viable1[11, c(51, 53, 55, 57, 59)] == T)
    counts_map12_sight20 = sum(viable1[12, c(51, 53, 55, 57, 59)] == T)
    counts_map19_sight20 = sum(viable1[19, c(51, 53, 55, 57, 59)] == T)
    counts_map20_sight20 = sum(viable1[20, c(51, 53, 55, 57, 59)] == T)
    counts_map21_sight20 = sum(viable1[21, c(51, 53, 55, 57, 59)] == T)
    
    sight0_60_counts = c(counts_map11_sight0, counts_map19_sight0, counts_map20_sight0, counts_map4_sight0,
                          counts_map5_sight0, counts_map21_sight0, counts_map12_sight0)
    sight1_60_counts = c(counts_map11_sight1, counts_map19_sight1, counts_map20_sight1, counts_map4_sight1,
                          counts_map5_sight1, counts_map21_sight1, counts_map12_sight1)
    sight2_60_counts = c(counts_map11_sight2, counts_map19_sight2, counts_map20_sight2, counts_map4_sight2,
                          counts_map5_sight2, counts_map21_sight2, counts_map12_sight2)
    sight5_60_counts = c(counts_map11_sight5, counts_map19_sight5, counts_map20_sight5, counts_map4_sight5,
                          counts_map5_sight5, counts_map21_sight5, counts_map12_sight5)
    sight10_60_counts = c(counts_map11_sight10, counts_map19_sight10, counts_map20_sight10, counts_map4_sight10,
                           counts_map5_sight10, counts_map21_sight10, counts_map12_sight10)
    sight20_60_counts = c(counts_map11_sight20, counts_map19_sight20, counts_map20_sight20, counts_map4_sight20,
                           counts_map5_sight20, counts_map21_sight20, counts_map12_sight20)
    
    # Calculate sight counts w/ sightangle = 360:
    
    counts_map4_sight0 = sum(viable1[4, c(61, 63, 65, 67, 69)] == T)
    counts_map5_sight0 = sum(viable1[5, c(61, 63, 65, 67, 69)] == T)
    counts_map11_sight0 = sum(viable1[11, c(61, 63, 65, 67, 69)] == T)
    counts_map12_sight0 = sum(viable1[12, c(61, 63, 65, 67, 69)] == T)
    counts_map19_sight0 = sum(viable1[19, c(61, 63, 65, 67, 69)] == T)
    counts_map20_sight0 = sum(viable1[20, c(61, 63, 65, 67, 69)] == T)
    counts_map21_sight0 = sum(viable1[21, c(61, 63, 65, 67, 69)] == T)
    counts_map4_sight1 = sum(viable1[4, c(71, 73, 75, 77, 79)] == T)
    counts_map5_sight1 = sum(viable1[5, c(71, 73, 75, 77, 79)] == T)
    counts_map11_sight1 = sum(viable1[11, c(71, 73, 75, 77, 79)] == T)
    counts_map12_sight1 = sum(viable1[12, c(71, 73, 75, 77, 79)] == T)
    counts_map19_sight1 = sum(viable1[19, c(71, 73, 75, 77, 79)] == T)
    counts_map20_sight1 = sum(viable1[20, c(71, 73, 75, 77, 79)] == T)
    counts_map21_sight1 = sum(viable1[21, c(71, 73, 75, 77, 79)] == T)
    counts_map4_sight2 = sum(viable1[4, c(81, 83, 85, 87, 89)] == T)
    counts_map5_sight2 = sum(viable1[5, c(81, 83, 85, 87, 89)] == T)
    counts_map11_sight2 = sum(viable1[11, c(81, 83, 85, 87, 89)] == T)
    counts_map12_sight2 = sum(viable1[12, c(81, 83, 85, 87, 89)] == T)
    counts_map19_sight2 = sum(viable1[19, c(81, 83, 85, 87, 89)] == T)
    counts_map20_sight2 = sum(viable1[20, c(81, 83, 85, 87, 89)] == T)
    counts_map21_sight2 = sum(viable1[21, c(81, 83, 85, 87, 89)] == T)
    counts_map4_sight5 = sum(viable1[4, c(91, 93, 95, 97, 99)] == T)
    counts_map5_sight5 = sum(viable1[5, c(91, 93, 95, 97, 99)] == T)
    counts_map11_sight5 = sum(viable1[11, c(91, 93, 95, 97, 99)] == T)
    counts_map12_sight5 = sum(viable1[12, c(91, 93, 95, 97, 99)] == T)
    counts_map19_sight5 = sum(viable1[19, c(91, 93, 95, 97, 99)] == T)
    counts_map20_sight5 = sum(viable1[20, c(91, 93, 95, 97, 99)] == T)
    counts_map21_sight5 = sum(viable1[21, c(91, 93, 95, 97, 99)] == T)
    counts_map4_sight10 = sum(viable1[4, c(101, 103, 105, 107, 109)] == T)
    counts_map5_sight10 = sum(viable1[5, c(101, 103, 105, 107, 109)] == T)
    counts_map11_sight10 = sum(viable1[11, c(101, 103, 105, 107, 109)] == T)
    counts_map12_sight10 = sum(viable1[12, c(101, 103, 105, 107, 109)] == T)
    counts_map19_sight10 = sum(viable1[19, c(101, 103, 105, 107, 109)] == T)
    counts_map20_sight10 = sum(viable1[20, c(101, 103, 105, 107, 109)] == T)
    counts_map21_sight10 = sum(viable1[21, c(101, 103, 105, 107, 109)] == T)
    counts_map4_sight20 = sum(viable1[4, c(111, 113, 115, 117, 119)] == T)
    counts_map5_sight20 = sum(viable1[5, c(111, 113, 115, 117, 119)] == T)
    counts_map11_sight20 = sum(viable1[11, c(111, 113, 115, 117, 119)] == T)
    counts_map12_sight20 = sum(viable1[12, c(111, 113, 115, 117, 119)] == T)
    counts_map19_sight20 = sum(viable1[19, c(111, 113, 115, 117, 119)] == T)
    counts_map20_sight20 = sum(viable1[20, c(111, 113, 115, 117, 119)] == T)
    counts_map21_sight20 = sum(viable1[21, c(111, 113, 115, 117, 119)] == T)
    
    sight0_360_counts = c(counts_map11_sight0, counts_map19_sight0, counts_map20_sight0, counts_map4_sight0,
                      counts_map5_sight0, counts_map21_sight0, counts_map12_sight0)
    sight1_360_counts = c(counts_map11_sight1, counts_map19_sight1, counts_map20_sight1, counts_map4_sight1,
                          counts_map5_sight1, counts_map21_sight1, counts_map12_sight1)
    sight2_360_counts = c(counts_map11_sight2, counts_map19_sight2, counts_map20_sight2, counts_map4_sight2,
                          counts_map5_sight2, counts_map21_sight2, counts_map12_sight2)
    sight5_360_counts = c(counts_map11_sight5, counts_map19_sight5, counts_map20_sight5, counts_map4_sight5,
                          counts_map5_sight5, counts_map21_sight5, counts_map12_sight5)
    sight10_360_counts = c(counts_map11_sight10, counts_map19_sight10, counts_map20_sight10, counts_map4_sight10,
                           counts_map5_sight10, counts_map21_sight10, counts_map12_sight10)
    sight20_360_counts = c(counts_map11_sight20, counts_map19_sight20, counts_map20_sight20, counts_map4_sight20,
                           counts_map5_sight20, counts_map21_sight20, counts_map12_sight20)
    
    # Plot
    
    par(mfrow=c(2,2))
    xs = (sort(unique(xdata))/(500*500))*100
    xs = xs[15:21]
    plot(xs, alpha1_counts, type="o", pch=19, col="navyblue", xlab="Percent Cover", 
         ylab="Viable Instances", ylim=c(0,5), main="Alpha")
    lines(xs, alpha1.5_counts, type="o", pch=19, col="blue")
    lines(xs, alpha2_counts, type="o", pch=19, col="dodgerblue")
    lines(xs, alpha2.5_counts, type="o", pch=19, col="deepskyblue")
    lines(xs, alpha3_counts, type="o", pch=19, col="lightblue")
    legend("topright", c("Alpha=1", "Alpha=1.5", "Alpha=2", "Alpha=2.5", "Alpha=3"),
           col=c("navyblue", "blue", "dodgerblue", "deepskyblue", "lightblue"), pch=19, 
           cex=0.5)
    
    plot(xs, sight0_counts, type="o", pch=19, col="black", xlab="Percent Cover", 
         ylab="Viable Instances", ylim=c(0,5), main="Sight")
    lines(xs, sight1_counts, type="o", pch=19, col="gray17")
    lines(xs, sight2_counts, type="o", pch=19, col="gray27")
    lines(xs, sight5_counts, type="o", pch=19, col="gray37")
    lines(xs, sight10_counts, type="o", pch=19, col="gray47")
    lines(xs, sight20_counts, type="o", pch=19, col="gray57")
    legend("topright", c("Sight=0", "Sight=1", "Sight=2", "Sight=5", "Sight=10", "Sight=20"),
           col=c("black", "gray17", "gray27", "gray37", "gray47", "gray57"), pch=19,
           cex=0.5)
    
    plot(xs, sight0_60_counts, type="o", pch=19, col="#a63603", xlab="Percent Cover", 
         ylab="Viable Instances", ylim=c(0,5), main="Sight and SightAngle=60")
    lines(xs, sight1_60_counts, type="o", pch=19, col="#e6550d")
    lines(xs, sight2_60_counts, type="o", pch=19, col="#fd8d3c")
    lines(xs, sight5_60_counts, type="o", pch=19, col="#fdae6b")
    lines(xs, sight10_60_counts, type="o", pch=19, col="#fdd0a2")
    lines(xs, sight20_60_counts, type="o", pch=19, col="#feedde")
    legend("topright", c("Sight=0", "Sight=1", "Sight=2", "Sight=5", "Sight=10", "Sight=20"),
           col=c("#a63603", "#e6550d", "#fd8d3c", "#fdae6b", "#fdd0a2", "#feedde"), pch=19,
           cex=0.5)
    
    plot(xs, sight0_360_counts, type="o", pch=19, col="#54278f", xlab="Percent Cover", 
         ylab="Viable Instances", ylim=c(0,5), main="Sight and SightAngle=360")
    lines(xs, sight1_360_counts, type="o", pch=19, col="#756bb1")
    lines(xs, sight2_360_counts, type="o", pch=19, col="#9e9ac8")
    lines(xs, sight5_360_counts, type="o", pch=19, col="#bcbddc")
    lines(xs, sight10_360_counts, type="o", pch=19, col="#dadaeb")
    lines(xs, sight20_360_counts, type="o", pch=19, col="#f2f0f7")
    legend("topright", c("Sight=0", "Sight=1", "Sight=2", "Sight=5", "Sight=10", "Sight=20"),
           col=c("#54278f", "#756bb1", "#9e9ac8", "#bcbddc", "#dadaeb", "#f2f0f7"), pch=19,
           cex=0.5)
    
  }
  
  if (scenario == 2) {
    
    # Calculate alpha counts:
    
    viable1 = count_data < 1
    
    counts_map2_alpha1 = sum(viable1[2, c(1, 11, 21, 31, 41, 51, 61, 71, 81, 91, 101, 111)] == T)
    counts_map3_alpha1 = sum(viable1[3, c(1, 11, 21, 31, 41, 51, 61, 71, 81, 91, 101, 111)] == T)
    counts_map4_alpha1 = sum(viable1[4, c(1, 11, 21, 31, 41, 51, 61, 71, 81, 91, 101, 111)] == T)
    counts_map5_alpha1 = sum(viable1[5, c(1, 11, 21, 31, 41, 51, 61, 71, 81, 91, 101, 111)] == T)
    counts_map7_alpha1 = sum(viable1[7, c(1, 11, 21, 31, 41, 51, 61, 71, 81, 91, 101, 111)] == T)
    counts_map8_alpha1 = sum(viable1[8, c(1, 11, 21, 31, 41, 51, 61, 71, 81, 91, 101, 111)] == T)
    counts_map9_alpha1 = sum(viable1[9, c(1, 11, 21, 31, 41, 51, 61, 71, 81, 91, 101, 111)] == T)
    counts_map10_alpha1 = sum(viable1[10, c(1, 11, 21, 31, 41, 51, 61, 71, 81, 91, 101, 111)] == T)
    counts_map11_alpha1 = sum(viable1[11, c(1, 11, 21, 31, 41, 51, 61, 71, 81, 91, 101, 111)] == T)
    counts_map12_alpha1 = sum(viable1[12, c(1, 11, 21, 31, 41, 51, 61, 71, 81, 91, 101, 111)] == T)
    counts_map14_alpha1 = sum(viable1[14, c(1, 11, 21, 31, 41, 51, 61, 71, 81, 91, 101, 111)] == T)
    counts_map15_alpha1 = sum(viable1[15, c(1, 11, 21, 31, 41, 51, 61, 71, 81, 91, 101, 111)] == T)
    counts_map16_alpha1 = sum(viable1[16, c(1, 11, 21, 31, 41, 51, 61, 71, 81, 91, 101, 111)] == T)
    counts_map17_alpha1 = sum(viable1[17, c(1, 11, 21, 31, 41, 51, 61, 71, 81, 91, 101, 111)] == T)
    counts_map18_alpha1 = sum(viable1[18, c(1, 11, 21, 31, 41, 51, 61, 71, 81, 91, 101, 111)] == T)
    counts_map19_alpha1 = sum(viable1[19, c(1, 11, 21, 31, 41, 51, 61, 71, 81, 91, 101, 111)] == T)
    counts_map20_alpha1 = sum(viable1[20, c(1, 11, 21, 31, 41, 51, 61, 71, 81, 91, 101, 111)] == T)
    counts_map21_alpha1 = sum(viable1[21, c(1, 11, 21, 31, 41, 51, 61, 71, 81, 91, 101, 111)] == T)
    counts_map2_alpha1.5 = sum(viable1[2, c(3, 13, 23, 33, 43, 53, 63, 73, 83, 93, 103, 113)] == T)
    counts_map3_alpha1.5 = sum(viable1[3, c(3, 13, 23, 33, 43, 53, 63, 73, 83, 93, 103, 113)] == T)
    counts_map4_alpha1.5 = sum(viable1[4, c(3, 13, 23, 33, 43, 53, 63, 73, 83, 93, 103, 113)] == T)
    counts_map5_alpha1.5 = sum(viable1[5, c(3, 13, 23, 33, 43, 53, 63, 73, 83, 93, 103, 113)] == T)
    counts_map7_alpha1.5 = sum(viable1[7, c(3, 13, 23, 33, 43, 53, 63, 73, 83, 93, 103, 113)] == T)
    counts_map8_alpha1.5 = sum(viable1[8, c(3, 13, 23, 33, 43, 53, 63, 73, 83, 93, 103, 113)] == T)
    counts_map9_alpha1.5 = sum(viable1[9, c(3, 13, 23, 33, 43, 53, 63, 73, 83, 93, 103, 113)] == T)
    counts_map10_alpha1.5 = sum(viable1[10, c(3, 13, 23, 33, 43, 53, 63, 73, 83, 93, 103, 113)] == T)
    counts_map11_alpha1.5 = sum(viable1[11, c(3, 13, 23, 33, 43, 53, 63, 73, 83, 93, 103, 113)] == T)
    counts_map12_alpha1.5 = sum(viable1[12, c(3, 13, 23, 33, 43, 53, 63, 73, 83, 93, 103, 113)] == T)
    counts_map14_alpha1.5 = sum(viable1[14, c(3, 13, 23, 33, 43, 53, 63, 73, 83, 93, 103, 113)] == T)
    counts_map15_alpha1.5 = sum(viable1[15, c(3, 13, 23, 33, 43, 53, 63, 73, 83, 93, 103, 113)] == T)
    counts_map16_alpha1.5 = sum(viable1[16, c(3, 13, 23, 33, 43, 53, 63, 73, 83, 93, 103, 113)] == T)
    counts_map17_alpha1.5 = sum(viable1[17, c(3, 13, 23, 33, 43, 53, 63, 73, 83, 93, 103, 113)] == T)
    counts_map18_alpha1.5 = sum(viable1[18, c(3, 13, 23, 33, 43, 53, 63, 73, 83, 93, 103, 113)] == T)
    counts_map19_alpha1.5 = sum(viable1[19, c(3, 13, 23, 33, 43, 53, 63, 73, 83, 93, 103, 113)] == T)
    counts_map20_alpha1.5 = sum(viable1[20, c(3, 13, 23, 33, 43, 53, 63, 73, 83, 93, 103, 113)] == T)
    counts_map21_alpha1.5 = sum(viable1[21, c(3, 13, 23, 33, 43, 53, 63, 73, 83, 93, 103, 113)] == T)
    counts_map2_alpha2 = sum(viable1[2, c(5, 15, 25, 35, 45, 55, 65, 75, 85, 95, 105, 115)] == T)
    counts_map3_alpha2 = sum(viable1[3, c(5, 15, 25, 35, 45, 55, 65, 75, 85, 95, 105, 115)] == T)
    counts_map4_alpha2 = sum(viable1[4, c(5, 15, 25, 35, 45, 55, 65, 75, 85, 95, 105, 115)] == T)
    counts_map5_alpha2 = sum(viable1[5, c(5, 15, 25, 35, 45, 55, 65, 75, 85, 95, 105, 115)] == T)
    counts_map7_alpha2 = sum(viable1[7, c(5, 15, 25, 35, 45, 55, 65, 75, 85, 95, 105, 115)] == T)
    counts_map8_alpha2 = sum(viable1[8, c(5, 15, 25, 35, 45, 55, 65, 75, 85, 95, 105, 115)] == T)
    counts_map9_alpha2 = sum(viable1[9, c(5, 15, 25, 35, 45, 55, 65, 75, 85, 95, 105, 115)] == T)
    counts_map10_alpha2 = sum(viable1[10, c(5, 15, 25, 35, 45, 55, 65, 75, 85, 95, 105, 115)] == T)
    counts_map11_alpha2 = sum(viable1[11, c(5, 15, 25, 35, 45, 55, 65, 75, 85, 95, 105, 115)] == T)
    counts_map12_alpha2 = sum(viable1[12, c(5, 15, 25, 35, 45, 55, 65, 75, 85, 95, 105, 115)] == T)
    counts_map14_alpha2 = sum(viable1[14, c(5, 15, 25, 35, 45, 55, 65, 75, 85, 95, 105, 115)] == T)
    counts_map15_alpha2 = sum(viable1[15, c(5, 15, 25, 35, 45, 55, 65, 75, 85, 95, 105, 115)] == T)
    counts_map16_alpha2 = sum(viable1[16, c(5, 15, 25, 35, 45, 55, 65, 75, 85, 95, 105, 115)] == T)
    counts_map17_alpha2 = sum(viable1[17, c(5, 15, 25, 35, 45, 55, 65, 75, 85, 95, 105, 115)] == T)
    counts_map18_alpha2 = sum(viable1[18, c(5, 15, 25, 35, 45, 55, 65, 75, 85, 95, 105, 115)] == T)
    counts_map19_alpha2 = sum(viable1[19, c(5, 15, 25, 35, 45, 55, 65, 75, 85, 95, 105, 115)] == T)
    counts_map20_alpha2 = sum(viable1[20, c(5, 15, 25, 35, 45, 55, 65, 75, 85, 95, 105, 115)] == T)
    counts_map21_alpha2 = sum(viable1[21, c(5, 15, 25, 35, 45, 55, 65, 75, 85, 95, 105, 115)] == T)
    counts_map2_alpha2.5 = sum(viable1[2, c(7, 17, 27, 37, 47, 57, 67, 77, 87, 97, 107, 117)] == T)
    counts_map3_alpha2.5 = sum(viable1[3, c(7, 17, 27, 37, 47, 57, 67, 77, 87, 97, 107, 117)] == T)
    counts_map4_alpha2.5 = sum(viable1[4, c(7, 17, 27, 37, 47, 57, 67, 77, 87, 97, 107, 117)] == T)
    counts_map5_alpha2.5 = sum(viable1[5, c(7, 17, 27, 37, 47, 57, 67, 77, 87, 97, 107, 117)] == T)
    counts_map7_alpha2.5 = sum(viable1[7, c(7, 17, 27, 37, 47, 57, 67, 77, 87, 97, 107, 117)] == T)
    counts_map8_alpha2.5 = sum(viable1[8, c(7, 17, 27, 37, 47, 57, 67, 77, 87, 97, 107, 117)] == T)
    counts_map9_alpha2.5 = sum(viable1[9, c(7, 17, 27, 37, 47, 57, 67, 77, 87, 97, 107, 117)] == T)
    counts_map10_alpha2.5 = sum(viable1[10, c(7, 17, 27, 37, 47, 57, 67, 77, 87, 97, 107, 117)] == T)
    counts_map11_alpha2.5 = sum(viable1[11, c(7, 17, 27, 37, 47, 57, 67, 77, 87, 97, 107, 117)] == T)
    counts_map12_alpha2.5 = sum(viable1[12, c(7, 17, 27, 37, 47, 57, 67, 77, 87, 97, 107, 117)] == T)
    counts_map14_alpha2.5 = sum(viable1[14, c(7, 17, 27, 37, 47, 57, 67, 77, 87, 97, 107, 117)] == T)
    counts_map15_alpha2.5 = sum(viable1[15, c(7, 17, 27, 37, 47, 57, 67, 77, 87, 97, 107, 117)] == T)
    counts_map16_alpha2.5 = sum(viable1[16, c(7, 17, 27, 37, 47, 57, 67, 77, 87, 97, 107, 117)] == T)
    counts_map17_alpha2.5 = sum(viable1[17, c(7, 17, 27, 37, 47, 57, 67, 77, 87, 97, 107, 117)] == T)
    counts_map18_alpha2.5 = sum(viable1[18, c(7, 17, 27, 37, 47, 57, 67, 77, 87, 97, 107, 117)] == T)
    counts_map19_alpha2.5 = sum(viable1[19, c(7, 17, 27, 37, 47, 57, 67, 77, 87, 97, 107, 117)] == T)
    counts_map20_alpha2.5 = sum(viable1[20, c(7, 17, 27, 37, 47, 57, 67, 77, 87, 97, 107, 117)] == T)
    counts_map21_alpha2.5 = sum(viable1[21, c(7, 17, 27, 37, 47, 57, 67, 77, 87, 97, 107, 117)] == T)
    counts_map2_alpha3 = sum(viable1[2, c(9, 19, 29, 39, 49, 59, 69, 79, 89, 99, 109, 119)] == T)
    counts_map3_alpha3 = sum(viable1[3, c(9, 19, 29, 39, 49, 59, 69, 79, 89, 99, 109, 119)] == T)
    counts_map4_alpha3 = sum(viable1[4, c(9, 19, 29, 39, 49, 59, 69, 79, 89, 99, 109, 119)] == T)
    counts_map5_alpha3 = sum(viable1[5, c(9, 19, 29, 39, 49, 59, 69, 79, 89, 99, 109, 119)] == T)
    counts_map7_alpha3 = sum(viable1[7, c(9, 19, 29, 39, 49, 59, 69, 79, 89, 99, 109, 119)] == T)
    counts_map8_alpha3 = sum(viable1[8, c(9, 19, 29, 39, 49, 59, 69, 79, 89, 99, 109, 119)] == T)
    counts_map9_alpha3 = sum(viable1[9, c(9, 19, 29, 39, 49, 59, 69, 79, 89, 99, 109, 119)] == T)
    counts_map10_alpha3 = sum(viable1[10, c(9, 19, 29, 39, 49, 59, 69, 79, 89, 99, 109, 119)] == T)
    counts_map11_alpha3 = sum(viable1[11, c(9, 19, 29, 39, 49, 59, 69, 79, 89, 99, 109, 119)] == T)
    counts_map12_alpha3 = sum(viable1[12, c(9, 19, 29, 39, 49, 59, 69, 79, 89, 99, 109, 119)] == T)
    counts_map14_alpha3 = sum(viable1[14, c(9, 19, 29, 39, 49, 59, 69, 79, 89, 99, 109, 119)] == T)
    counts_map15_alpha3 = sum(viable1[15, c(9, 19, 29, 39, 49, 59, 69, 79, 89, 99, 109, 119)] == T)
    counts_map16_alpha3 = sum(viable1[16, c(9, 19, 29, 39, 49, 59, 69, 79, 89, 99, 109, 119)] == T)
    counts_map17_alpha3 = sum(viable1[17, c(9, 19, 29, 39, 49, 59, 69, 79, 89, 99, 109, 119)] == T)
    counts_map18_alpha3 = sum(viable1[18, c(9, 19, 29, 39, 49, 59, 69, 79, 89, 99, 109, 119)] == T)
    counts_map19_alpha3 = sum(viable1[19, c(9, 19, 29, 39, 49, 59, 69, 79, 89, 99, 109, 119)] == T)
    counts_map20_alpha3 = sum(viable1[20, c(9, 19, 29, 39, 49, 59, 69, 79, 89, 99, 109, 119)] == T)
    counts_map21_alpha3 = sum(viable1[21, c(9, 19, 29, 39, 49, 59, 69, 79, 89, 99, 109, 119)] == T)
    
    alpha1_counts = c(counts_map14_alpha1, counts_map15_alpha1, counts_map7_alpha1, counts_map8_alpha1,
                      counts_map2_alpha1, counts_map3_alpha1, counts_map16_alpha1, counts_map17_alpha1,
                      counts_map18_alpha1, counts_map9_alpha1, counts_map10_alpha1, 
                      counts_map11_alpha1, counts_map19_alpha1, counts_map20_alpha1,
                      counts_map4_alpha1, counts_map5_alpha1, counts_map21_alpha1,
                      counts_map12_alpha1)
    alpha1.5_counts = c(counts_map14_alpha1.5, counts_map15_alpha1.5, counts_map7_alpha1.5, counts_map8_alpha1.5,
                        counts_map2_alpha1.5, counts_map3_alpha1.5, counts_map16_alpha1.5, counts_map17_alpha1.5,
                        counts_map18_alpha1.5, counts_map9_alpha1.5, counts_map10_alpha1.5, 
                        counts_map11_alpha1.5, counts_map19_alpha1.5, counts_map20_alpha1.5,
                        counts_map4_alpha1.5, counts_map5_alpha1.5, counts_map21_alpha1.5,
                        counts_map12_alpha1.5)
    alpha2_counts = c(counts_map14_alpha2, counts_map15_alpha2, counts_map7_alpha2, counts_map8_alpha2,
                      counts_map2_alpha2, counts_map3_alpha2, counts_map16_alpha2, counts_map17_alpha2,
                      counts_map18_alpha2, counts_map9_alpha2, counts_map10_alpha2, 
                      counts_map11_alpha2, counts_map19_alpha2, counts_map20_alpha2,
                      counts_map4_alpha2, counts_map5_alpha2, counts_map21_alpha2,
                      counts_map12_alpha2)
    alpha2.5_counts = c(counts_map14_alpha2.5, counts_map15_alpha2.5, counts_map7_alpha2.5, counts_map8_alpha2.5,
                        counts_map2_alpha2.5, counts_map3_alpha2.5, counts_map16_alpha2.5, counts_map17_alpha2.5,
                        counts_map18_alpha2.5, counts_map9_alpha2.5, counts_map10_alpha2.5, 
                        counts_map11_alpha2.5, counts_map19_alpha2.5, counts_map20_alpha2.5,
                        counts_map4_alpha2.5, counts_map5_alpha2.5, counts_map21_alpha2.5,
                        counts_map12_alpha2.5)
    alpha3_counts = c(counts_map14_alpha3, counts_map15_alpha3, counts_map7_alpha3, counts_map8_alpha3,
                      counts_map2_alpha3, counts_map3_alpha3, counts_map16_alpha3, counts_map17_alpha3,
                      counts_map18_alpha3, counts_map9_alpha3, counts_map10_alpha3, 
                      counts_map11_alpha3, counts_map19_alpha3, counts_map20_alpha3,
                      counts_map4_alpha3, counts_map5_alpha3, counts_map21_alpha3,
                      counts_map12_alpha3)
    
    # Calculate sight counts:
    
    counts_map2_sight0 = sum(viable1[2, c(1, 3, 5, 7, 9, 61, 63, 65, 67, 69)] == T)
    counts_map3_sight0 = sum(viable1[3, c(1, 3, 5, 7, 9, 61, 63, 65, 67, 69)] == T)
    counts_map4_sight0 = sum(viable1[4, c(1, 3, 5, 7, 9, 61, 63, 65, 67, 69)] == T)
    counts_map5_sight0 = sum(viable1[5, c(1, 3, 5, 7, 9, 61, 63, 65, 67, 69)] == T)
    counts_map7_sight0 = sum(viable1[7, c(1, 3, 5, 7, 9, 61, 63, 65, 67, 69)] == T)
    counts_map8_sight0 = sum(viable1[8, c(1, 3, 5, 7, 9, 61, 63, 65, 67, 69)] == T)
    counts_map9_sight0 = sum(viable1[9, c(1, 3, 5, 7, 9, 61, 63, 65, 67, 69)] == T)
    counts_map10_sight0 = sum(viable1[10, c(1, 3, 5, 7, 9, 61, 63, 65, 67, 69)] == T)
    counts_map11_sight0 = sum(viable1[11, c(1, 3, 5, 7, 9, 61, 63, 65, 67, 69)] == T)
    counts_map12_sight0 = sum(viable1[12, c(1, 3, 5, 7, 9, 61, 63, 65, 67, 69)] == T)
    counts_map14_sight0 = sum(viable1[14, c(1, 3, 5, 7, 9, 61, 63, 65, 67, 69)] == T)
    counts_map15_sight0 = sum(viable1[15, c(1, 3, 5, 7, 9, 61, 63, 65, 67, 69)] == T)
    counts_map16_sight0 = sum(viable1[16, c(1, 3, 5, 7, 9, 61, 63, 65, 67, 69)] == T)
    counts_map17_sight0 = sum(viable1[17, c(1, 3, 5, 7, 9, 61, 63, 65, 67, 69)] == T)
    counts_map18_sight0 = sum(viable1[18, c(1, 3, 5, 7, 9, 61, 63, 65, 67, 69)] == T)
    counts_map19_sight0 = sum(viable1[19, c(1, 3, 5, 7, 9, 61, 63, 65, 67, 69)] == T)
    counts_map20_sight0 = sum(viable1[20, c(1, 3, 5, 7, 9, 61, 63, 65, 67, 69)] == T)
    counts_map21_sight0 = sum(viable1[21, c(1, 3, 5, 7, 9, 61, 63, 65, 67, 69)] == T)
    counts_map2_sight1 = sum(viable1[2, c(11, 13, 15, 17, 19, 71, 73, 75, 77, 79)] == T)
    counts_map3_sight1 = sum(viable1[3, c(11, 13, 15, 17, 19, 71, 73, 75, 77, 79)] == T)
    counts_map4_sight1 = sum(viable1[4, c(11, 13, 15, 17, 19, 71, 73, 75, 77, 79)] == T)
    counts_map5_sight1 = sum(viable1[5, c(11, 13, 15, 17, 19, 71, 73, 75, 77, 79)] == T)
    counts_map7_sight1 = sum(viable1[7, c(11, 13, 15, 17, 19, 71, 73, 75, 77, 79)] == T)
    counts_map8_sight1 = sum(viable1[8, c(11, 13, 15, 17, 19, 71, 73, 75, 77, 79)] == T)
    counts_map9_sight1 = sum(viable1[9, c(11, 13, 15, 17, 19, 71, 73, 75, 77, 79)] == T)
    counts_map10_sight1 = sum(viable1[10, c(11, 13, 15, 17, 19, 71, 73, 75, 77, 79)] == T)
    counts_map11_sight1 = sum(viable1[11, c(11, 13, 15, 17, 19, 71, 73, 75, 77, 79)] == T)
    counts_map12_sight1 = sum(viable1[12, c(11, 13, 15, 17, 19, 71, 73, 75, 77, 79)] == T)
    counts_map14_sight1 = sum(viable1[14, c(11, 13, 15, 17, 19, 71, 73, 75, 77, 79)] == T)
    counts_map15_sight1 = sum(viable1[15, c(11, 13, 15, 17, 19, 71, 73, 75, 77, 79)] == T)
    counts_map16_sight1 = sum(viable1[16, c(11, 13, 15, 17, 19, 71, 73, 75, 77, 79)] == T)
    counts_map17_sight1 = sum(viable1[17, c(11, 13, 15, 17, 19, 71, 73, 75, 77, 79)] == T)
    counts_map18_sight1 = sum(viable1[18, c(11, 13, 15, 17, 19, 71, 73, 75, 77, 79)] == T)
    counts_map19_sight1 = sum(viable1[19, c(11, 13, 15, 17, 19, 71, 73, 75, 77, 79)] == T)
    counts_map20_sight1 = sum(viable1[20, c(11, 13, 15, 17, 19, 71, 73, 75, 77, 79)] == T)
    counts_map21_sight1 = sum(viable1[21, c(11, 13, 15, 17, 19, 71, 73, 75, 77, 79)] == T)
    counts_map2_sight2 = sum(viable1[2, c(21, 23, 25, 27, 29, 81, 83, 85, 87, 89)] == T)
    counts_map3_sight2 = sum(viable1[3, c(21, 23, 25, 27, 29, 81, 83, 85, 87, 89)] == T)
    counts_map4_sight2 = sum(viable1[4, c(21, 23, 25, 27, 29, 81, 83, 85, 87, 89)] == T)
    counts_map5_sight2 = sum(viable1[5, c(21, 23, 25, 27, 29, 81, 83, 85, 87, 89)] == T)
    counts_map7_sight2 = sum(viable1[7, c(21, 23, 25, 27, 29, 81, 83, 85, 87, 89)] == T)
    counts_map8_sight2 = sum(viable1[8, c(21, 23, 25, 27, 29, 81, 83, 85, 87, 89)] == T)
    counts_map9_sight2 = sum(viable1[9, c(21, 23, 25, 27, 29, 81, 83, 85, 87, 89)] == T)
    counts_map10_sight2 = sum(viable1[10, c(21, 23, 25, 27, 29, 81, 83, 85, 87, 89)] == T)
    counts_map11_sight2 = sum(viable1[11, c(21, 23, 25, 27, 29, 81, 83, 85, 87, 89)] == T)
    counts_map12_sight2 = sum(viable1[12, c(21, 23, 25, 27, 29, 81, 83, 85, 87, 89)] == T)
    counts_map14_sight2 = sum(viable1[14, c(21, 23, 25, 27, 29, 81, 83, 85, 87, 89)] == T)
    counts_map15_sight2 = sum(viable1[15, c(21, 23, 25, 27, 29, 81, 83, 85, 87, 89)] == T)
    counts_map16_sight2 = sum(viable1[16, c(21, 23, 25, 27, 29, 81, 83, 85, 87, 89)] == T)
    counts_map17_sight2 = sum(viable1[17, c(21, 23, 25, 27, 29, 81, 83, 85, 87, 89)] == T)
    counts_map18_sight2 = sum(viable1[18, c(21, 23, 25, 27, 29, 81, 83, 85, 87, 89)] == T)
    counts_map19_sight2 = sum(viable1[19, c(21, 23, 25, 27, 29, 81, 83, 85, 87, 89)] == T)
    counts_map20_sight2 = sum(viable1[20, c(21, 23, 25, 27, 29, 81, 83, 85, 87, 89)] == T)
    counts_map21_sight2 = sum(viable1[21, c(21, 23, 25, 27, 29, 81, 83, 85, 87, 89)] == T)
    counts_map2_sight5 = sum(viable1[2, c(31, 33, 35, 37, 39, 91, 93, 95, 97, 99)] == T)
    counts_map3_sight5 = sum(viable1[3, c(31, 33, 35, 37, 39, 91, 93, 95, 97, 99)] == T)
    counts_map4_sight5 = sum(viable1[4, c(31, 33, 35, 37, 39, 91, 93, 95, 97, 99)] == T)
    counts_map5_sight5 = sum(viable1[5, c(31, 33, 35, 37, 39, 91, 93, 95, 97, 99)] == T)
    counts_map7_sight5 = sum(viable1[7, c(31, 33, 35, 37, 39, 91, 93, 95, 97, 99)] == T)
    counts_map8_sight5 = sum(viable1[8, c(31, 33, 35, 37, 39, 91, 93, 95, 97, 99)] == T)
    counts_map9_sight5 = sum(viable1[9, c(31, 33, 35, 37, 39, 91, 93, 95, 97, 99)] == T)
    counts_map10_sight5 = sum(viable1[10, c(31, 33, 35, 37, 39, 91, 93, 95, 97, 99)] == T)
    counts_map11_sight5 = sum(viable1[11, c(31, 33, 35, 37, 39, 91, 93, 95, 97, 99)] == T)
    counts_map12_sight5 = sum(viable1[12, c(31, 33, 35, 37, 39, 91, 93, 95, 97, 99)] == T)
    counts_map14_sight5 = sum(viable1[14, c(31, 33, 35, 37, 39, 91, 93, 95, 97, 99)] == T)
    counts_map15_sight5 = sum(viable1[15, c(31, 33, 35, 37, 39, 91, 93, 95, 97, 99)] == T)
    counts_map16_sight5 = sum(viable1[16, c(31, 33, 35, 37, 39, 91, 93, 95, 97, 99)] == T)
    counts_map17_sight5 = sum(viable1[17, c(31, 33, 35, 37, 39, 91, 93, 95, 97, 99)] == T)
    counts_map18_sight5 = sum(viable1[18, c(31, 33, 35, 37, 39, 91, 93, 95, 97, 99)] == T)
    counts_map19_sight5 = sum(viable1[19, c(31, 33, 35, 37, 39, 91, 93, 95, 97, 99)] == T)
    counts_map20_sight5 = sum(viable1[20, c(31, 33, 35, 37, 39, 91, 93, 95, 97, 99)] == T)
    counts_map21_sight5 = sum(viable1[21, c(31, 33, 35, 37, 39, 91, 93, 95, 97, 99)] == T)
    counts_map2_sight10 = sum(viable1[2, c(41, 43, 45, 47, 49, 101, 103, 105, 107, 109)] == T)
    counts_map3_sight10 = sum(viable1[3, c(41, 43, 45, 47, 49, 101, 103, 105, 107, 109)] == T)
    counts_map4_sight10 = sum(viable1[4, c(41, 43, 45, 47, 49, 101, 103, 105, 107, 109)] == T)
    counts_map5_sight10 = sum(viable1[5, c(41, 43, 45, 47, 49, 101, 103, 105, 107, 109)] == T)
    counts_map7_sight10 = sum(viable1[7, c(41, 43, 45, 47, 49, 101, 103, 105, 107, 109)] == T)
    counts_map8_sight10 = sum(viable1[8, c(41, 43, 45, 47, 49, 101, 103, 105, 107, 109)] == T)
    counts_map9_sight10 = sum(viable1[9, c(41, 43, 45, 47, 49, 101, 103, 105, 107, 109)] == T)
    counts_map10_sight10 = sum(viable1[10, c(41, 43, 45, 47, 49, 101, 103, 105, 107, 109)] == T)
    counts_map11_sight10 = sum(viable1[11, c(41, 43, 45, 47, 49, 101, 103, 105, 107, 109)] == T)
    counts_map12_sight10 = sum(viable1[12, c(41, 43, 45, 47, 49, 101, 103, 105, 107, 109)] == T)
    counts_map14_sight10 = sum(viable1[14, c(41, 43, 45, 47, 49, 101, 103, 105, 107, 109)] == T)
    counts_map15_sight10 = sum(viable1[15, c(41, 43, 45, 47, 49, 101, 103, 105, 107, 109)] == T)
    counts_map16_sight10 = sum(viable1[16, c(41, 43, 45, 47, 49, 101, 103, 105, 107, 109)] == T)
    counts_map17_sight10 = sum(viable1[17, c(41, 43, 45, 47, 49, 101, 103, 105, 107, 109)] == T)
    counts_map18_sight10 = sum(viable1[18, c(41, 43, 45, 47, 49, 101, 103, 105, 107, 109)] == T)
    counts_map19_sight10 = sum(viable1[19, c(41, 43, 45, 47, 49, 101, 103, 105, 107, 109)] == T)
    counts_map20_sight10 = sum(viable1[20, c(41, 43, 45, 47, 49, 101, 103, 105, 107, 109)] == T)
    counts_map21_sight10 = sum(viable1[21, c(41, 43, 45, 47, 49, 101, 103, 105, 107, 109)] == T)
    counts_map2_sight20 = sum(viable1[2, c(51, 53, 55, 57, 59, 111, 113, 115, 117, 119)] == T)
    counts_map3_sight20 = sum(viable1[3, c(51, 53, 55, 57, 59, 111, 113, 115, 117, 119)] == T)
    counts_map4_sight20 = sum(viable1[4, c(51, 53, 55, 57, 59, 111, 113, 115, 117, 119)] == T)
    counts_map5_sight20 = sum(viable1[5, c(51, 53, 55, 57, 59, 111, 113, 115, 117, 119)] == T)
    counts_map7_sight20 = sum(viable1[7, c(51, 53, 55, 57, 59, 111, 113, 115, 117, 119)] == T)
    counts_map8_sight20 = sum(viable1[8, c(51, 53, 55, 57, 59, 111, 113, 115, 117, 119)] == T)
    counts_map9_sight20 = sum(viable1[9, c(51, 53, 55, 57, 59, 111, 113, 115, 117, 119)] == T)
    counts_map10_sight20 = sum(viable1[10, c(51, 53, 55, 57, 59, 111, 113, 115, 117, 119)] == T)
    counts_map11_sight20 = sum(viable1[11, c(51, 53, 55, 57, 59, 111, 113, 115, 117, 119)] == T)
    counts_map12_sight20 = sum(viable1[12, c(51, 53, 55, 57, 59, 111, 113, 115, 117, 119)] == T)
    counts_map14_sight20 = sum(viable1[14, c(51, 53, 55, 57, 59, 111, 113, 115, 117, 119)] == T)
    counts_map15_sight20 = sum(viable1[15, c(51, 53, 55, 57, 59, 111, 113, 115, 117, 119)] == T)
    counts_map16_sight20 = sum(viable1[16, c(51, 53, 55, 57, 59, 111, 113, 115, 117, 119)] == T)
    counts_map17_sight20 = sum(viable1[17, c(51, 53, 55, 57, 59, 111, 113, 115, 117, 119)] == T)
    counts_map18_sight20 = sum(viable1[18, c(51, 53, 55, 57, 59, 111, 113, 115, 117, 119)] == T)
    counts_map19_sight20 = sum(viable1[19, c(51, 53, 55, 57, 59, 111, 113, 115, 117, 119)] == T)
    counts_map20_sight20 = sum(viable1[20, c(51, 53, 55, 57, 59, 111, 113, 115, 117, 119)] == T)
    counts_map21_sight20 = sum(viable1[21, c(51, 53, 55, 57, 59, 111, 113, 115, 117, 119)] == T)
    
    sight0_counts = c(counts_map14_sight0, counts_map15_sight0, counts_map7_sight0, counts_map8_sight0,
                          counts_map2_sight0, counts_map3_sight0, counts_map16_sight0,
                          counts_map17_sight0, counts_map18_sight0, counts_map9_sight0,
                          counts_map10_sight0, counts_map11_sight0, counts_map19_sight0,
                          counts_map20_sight0, counts_map4_sight0, counts_map5_sight0,
                          counts_map21_sight0, counts_map12_sight0)
    sight1_counts = c(counts_map14_sight1, counts_map15_sight1, counts_map7_sight1, counts_map8_sight1,
                          counts_map2_sight1, counts_map3_sight1, counts_map16_sight1,
                          counts_map17_sight1, counts_map18_sight1, counts_map9_sight1,
                          counts_map10_sight1, counts_map11_sight1, counts_map19_sight1,
                          counts_map20_sight1, counts_map4_sight1, counts_map5_sight1,
                          counts_map21_sight1, counts_map12_sight1)
    sight2_counts = c(counts_map14_sight2, counts_map15_sight2, counts_map7_sight2, counts_map8_sight2,
                          counts_map2_sight2, counts_map3_sight2, counts_map16_sight2,
                          counts_map17_sight2, counts_map18_sight2, counts_map9_sight2,
                          counts_map10_sight2, counts_map11_sight2, counts_map19_sight2,
                          counts_map20_sight2, counts_map4_sight2, counts_map5_sight2,
                          counts_map21_sight2, counts_map12_sight2)
    sight5_counts = c(counts_map14_sight5, counts_map15_sight5, counts_map7_sight5, counts_map8_sight5,
                          counts_map2_sight5, counts_map3_sight5, counts_map16_sight5,
                          counts_map17_sight5, counts_map18_sight5, counts_map9_sight5,
                          counts_map10_sight5, counts_map11_sight5, counts_map19_sight5,
                          counts_map20_sight5, counts_map4_sight5, counts_map5_sight5,
                          counts_map21_sight5, counts_map12_sight5)
    sight10_counts = c(counts_map14_sight10, counts_map15_sight10, counts_map7_sight10, counts_map8_sight10,
                           counts_map2_sight10, counts_map3_sight10, counts_map16_sight10,
                           counts_map17_sight10, counts_map18_sight10, counts_map9_sight10,
                           counts_map10_sight10, counts_map11_sight10, counts_map19_sight10,
                           counts_map20_sight10, counts_map4_sight10, counts_map5_sight10,
                           counts_map21_sight10, counts_map12_sight10)
    sight20_counts = c(counts_map14_sight20, counts_map15_sight20, counts_map7_sight20, counts_map8_sight20,
                           counts_map2_sight20, counts_map3_sight20, counts_map16_sight20,
                           counts_map17_sight20, counts_map18_sight20, counts_map9_sight20,
                           counts_map10_sight20, counts_map11_sight20, counts_map19_sight20,
                           counts_map20_sight20, counts_map4_sight20, counts_map5_sight20,
                           counts_map21_sight20, counts_map12_sight20)
    
    
    # Calculate sight counts w/ sightangle = 60:
    
    counts_map2_sight0 = sum(viable1[2, c(1, 3, 5, 7, 9)] == T)
    counts_map3_sight0 = sum(viable1[3, c(1, 3, 5, 7, 9)] == T)
    counts_map4_sight0 = sum(viable1[4, c(1, 3, 5, 7, 9)] == T)
    counts_map5_sight0 = sum(viable1[5, c(1, 3, 5, 7, 9)] == T)
    counts_map7_sight0 = sum(viable1[7, c(1, 3, 5, 7, 9)] == T)
    counts_map8_sight0 = sum(viable1[8, c(1, 3, 5, 7, 9)] == T)
    counts_map9_sight0 = sum(viable1[9, c(1, 3, 5, 7, 9)] == T)
    counts_map10_sight0 = sum(viable1[10, c(1, 3, 5, 7, 9)] == T)
    counts_map11_sight0 = sum(viable1[11, c(1, 3, 5, 7, 9)] == T)
    counts_map12_sight0 = sum(viable1[12, c(1, 3, 5, 7, 9)] == T)
    counts_map14_sight0 = sum(viable1[14, c(1, 3, 5, 7, 9)] == T)
    counts_map15_sight0 = sum(viable1[15, c(1, 3, 5, 7, 9)] == T)
    counts_map16_sight0 = sum(viable1[16, c(1, 3, 5, 7, 9)] == T)
    counts_map17_sight0 = sum(viable1[17, c(1, 3, 5, 7, 9)] == T)
    counts_map18_sight0 = sum(viable1[18, c(1, 3, 5, 7, 9)] == T)
    counts_map19_sight0 = sum(viable1[19, c(1, 3, 5, 7, 9)] == T)
    counts_map20_sight0 = sum(viable1[20, c(1, 3, 5, 7, 9)] == T)
    counts_map21_sight0 = sum(viable1[21, c(1, 3, 5, 7, 9)] == T)
    counts_map2_sight1 = sum(viable1[2, c(11, 13, 15, 17, 19)] == T)
    counts_map3_sight1 = sum(viable1[3, c(11, 13, 15, 17, 19)] == T)
    counts_map4_sight1 = sum(viable1[4, c(11, 13, 15, 17, 19)] == T)
    counts_map5_sight1 = sum(viable1[5, c(11, 13, 15, 17, 19)] == T)
    counts_map7_sight1 = sum(viable1[7, c(11, 13, 15, 17, 19)] == T)
    counts_map8_sight1 = sum(viable1[8, c(11, 13, 15, 17, 19)] == T)
    counts_map9_sight1 = sum(viable1[9, c(11, 13, 15, 17, 19)] == T)
    counts_map10_sight1 = sum(viable1[10, c(11, 13, 15, 17, 19)] == T)
    counts_map11_sight1 = sum(viable1[11, c(11, 13, 15, 17, 19)] == T)
    counts_map12_sight1 = sum(viable1[12, c(11, 13, 15, 17, 19)] == T)
    counts_map14_sight1 = sum(viable1[14, c(11, 13, 15, 17, 19)] == T)
    counts_map15_sight1 = sum(viable1[15, c(11, 13, 15, 17, 19)] == T)
    counts_map16_sight1 = sum(viable1[16, c(11, 13, 15, 17, 19)] == T)
    counts_map17_sight1 = sum(viable1[17, c(11, 13, 15, 17, 19)] == T)
    counts_map18_sight1 = sum(viable1[18, c(11, 13, 15, 17, 19)] == T)
    counts_map19_sight1 = sum(viable1[19, c(11, 13, 15, 17, 19)] == T)
    counts_map20_sight1 = sum(viable1[20, c(11, 13, 15, 17, 19)] == T)
    counts_map21_sight1 = sum(viable1[21, c(11, 13, 15, 17, 19)] == T)
    counts_map2_sight2 = sum(viable1[2, c(21, 23, 25, 27, 29)] == T)
    counts_map3_sight2 = sum(viable1[3, c(21, 23, 25, 27, 29)] == T)
    counts_map4_sight2 = sum(viable1[4, c(21, 23, 25, 27, 29)] == T)
    counts_map5_sight2 = sum(viable1[5, c(21, 23, 25, 27, 29)] == T)
    counts_map7_sight2 = sum(viable1[7, c(21, 23, 25, 27, 29)] == T)
    counts_map8_sight2 = sum(viable1[8, c(21, 23, 25, 27, 29)] == T)
    counts_map9_sight2 = sum(viable1[9, c(21, 23, 25, 27, 29)] == T)
    counts_map10_sight2 = sum(viable1[10, c(21, 23, 25, 27, 29)] == T)
    counts_map11_sight2 = sum(viable1[11, c(21, 23, 25, 27, 29)] == T)
    counts_map12_sight2 = sum(viable1[12, c(21, 23, 25, 27, 29)] == T)
    counts_map14_sight2 = sum(viable1[14, c(21, 23, 25, 27, 29)] == T)
    counts_map15_sight2 = sum(viable1[15, c(21, 23, 25, 27, 29)] == T)
    counts_map16_sight2 = sum(viable1[16, c(21, 23, 25, 27, 29)] == T)
    counts_map17_sight2 = sum(viable1[17, c(21, 23, 25, 27, 29)] == T)
    counts_map18_sight2 = sum(viable1[18, c(21, 23, 25, 27, 29)] == T)
    counts_map19_sight2 = sum(viable1[19, c(21, 23, 25, 27, 29)] == T)
    counts_map20_sight2 = sum(viable1[20, c(21, 23, 25, 27, 29)] == T)
    counts_map21_sight2 = sum(viable1[21, c(21, 23, 25, 27, 29)] == T)
    counts_map2_sight5 = sum(viable1[2, c(31, 33, 35, 37, 39)] == T)
    counts_map3_sight5 = sum(viable1[3, c(31, 33, 35, 37, 39)] == T)
    counts_map4_sight5 = sum(viable1[4, c(31, 33, 35, 37, 39)] == T)
    counts_map5_sight5 = sum(viable1[5, c(31, 33, 35, 37, 39)] == T)
    counts_map7_sight5 = sum(viable1[7, c(31, 33, 35, 37, 39)] == T)
    counts_map8_sight5 = sum(viable1[8, c(31, 33, 35, 37, 39)] == T)
    counts_map9_sight5 = sum(viable1[9, c(31, 33, 35, 37, 39)] == T)
    counts_map10_sight5 = sum(viable1[10, c(31, 33, 35, 37, 39)] == T)
    counts_map11_sight5 = sum(viable1[11, c(31, 33, 35, 37, 39)] == T)
    counts_map12_sight5 = sum(viable1[12, c(31, 33, 35, 37, 39)] == T)
    counts_map14_sight5 = sum(viable1[14, c(31, 33, 35, 37, 39)] == T)
    counts_map15_sight5 = sum(viable1[15, c(31, 33, 35, 37, 39)] == T)
    counts_map16_sight5 = sum(viable1[16, c(31, 33, 35, 37, 39)] == T)
    counts_map17_sight5 = sum(viable1[17, c(31, 33, 35, 37, 39)] == T)
    counts_map18_sight5 = sum(viable1[18, c(31, 33, 35, 37, 39)] == T)
    counts_map19_sight5 = sum(viable1[19, c(31, 33, 35, 37, 39)] == T)
    counts_map20_sight5 = sum(viable1[20, c(31, 33, 35, 37, 39)] == T)
    counts_map21_sight5 = sum(viable1[21, c(31, 33, 35, 37, 39)] == T)
    counts_map2_sight10 = sum(viable1[2, c(41, 43, 45, 47, 49)] == T)
    counts_map3_sight10 = sum(viable1[3, c(41, 43, 45, 47, 49)] == T)
    counts_map4_sight10 = sum(viable1[4, c(41, 43, 45, 47, 49)] == T)
    counts_map5_sight10 = sum(viable1[5, c(41, 43, 45, 47, 49)] == T)
    counts_map7_sight10 = sum(viable1[7, c(41, 43, 45, 47, 49)] == T)
    counts_map8_sight10 = sum(viable1[8, c(41, 43, 45, 47, 49)] == T)
    counts_map9_sight10 = sum(viable1[9, c(41, 43, 45, 47, 49)] == T)
    counts_map10_sight10 = sum(viable1[10, c(41, 43, 45, 47, 49)] == T)
    counts_map11_sight10 = sum(viable1[11, c(41, 43, 45, 47, 49)] == T)
    counts_map12_sight10 = sum(viable1[12, c(41, 43, 45, 47, 49)] == T)
    counts_map14_sight10 = sum(viable1[14, c(41, 43, 45, 47, 49)] == T)
    counts_map15_sight10 = sum(viable1[15, c(41, 43, 45, 47, 49)] == T)
    counts_map16_sight10 = sum(viable1[16, c(41, 43, 45, 47, 49)] == T)
    counts_map17_sight10 = sum(viable1[17, c(41, 43, 45, 47, 49)] == T)
    counts_map18_sight10 = sum(viable1[18, c(41, 43, 45, 47, 49)] == T)
    counts_map19_sight10 = sum(viable1[19, c(41, 43, 45, 47, 49)] == T)
    counts_map20_sight10 = sum(viable1[20, c(41, 43, 45, 47, 49)] == T)
    counts_map21_sight10 = sum(viable1[21, c(41, 43, 45, 47, 49)] == T)
    counts_map2_sight20 = sum(viable1[2, c(51, 53, 55, 57, 59)] == T)
    counts_map3_sight20 = sum(viable1[3, c(51, 53, 55, 57, 59)] == T)
    counts_map4_sight20 = sum(viable1[4, c(51, 53, 55, 57, 59)] == T)
    counts_map5_sight20 = sum(viable1[5, c(51, 53, 55, 57, 59)] == T)
    counts_map7_sight20 = sum(viable1[7, c(51, 53, 55, 57, 59)] == T)
    counts_map8_sight20 = sum(viable1[8, c(51, 53, 55, 57, 59)] == T)
    counts_map9_sight20 = sum(viable1[9, c(51, 53, 55, 57, 59)] == T)
    counts_map10_sight20 = sum(viable1[10, c(51, 53, 55, 57, 59)] == T)
    counts_map11_sight20 = sum(viable1[11, c(51, 53, 55, 57, 59)] == T)
    counts_map12_sight20 = sum(viable1[12, c(51, 53, 55, 57, 59)] == T)
    counts_map14_sight20 = sum(viable1[14, c(51, 53, 55, 57, 59)] == T)
    counts_map15_sight20 = sum(viable1[15, c(51, 53, 55, 57, 59)] == T)
    counts_map16_sight20 = sum(viable1[16, c(51, 53, 55, 57, 59)] == T)
    counts_map17_sight20 = sum(viable1[17, c(51, 53, 55, 57, 59)] == T)
    counts_map18_sight20 = sum(viable1[18, c(51, 53, 55, 57, 59)] == T)
    counts_map19_sight20 = sum(viable1[19, c(51, 53, 55, 57, 59)] == T)
    counts_map20_sight20 = sum(viable1[20, c(51, 53, 55, 57, 59)] == T)
    counts_map21_sight20 = sum(viable1[21, c(51, 53, 55, 57, 59)] == T)
    
    sight0_60_counts = c(counts_map14_sight0, counts_map15_sight0, counts_map7_sight0, counts_map8_sight0,
                          counts_map2_sight0, counts_map3_sight0, counts_map16_sight0,
                          counts_map17_sight0, counts_map18_sight0, counts_map9_sight0,
                          counts_map10_sight0, counts_map11_sight0, counts_map19_sight0,
                          counts_map20_sight0, counts_map4_sight0, counts_map5_sight0,
                          counts_map21_sight0, counts_map12_sight0)
    sight1_60_counts = c(counts_map14_sight1, counts_map15_sight1, counts_map7_sight1, counts_map8_sight1,
                          counts_map2_sight1, counts_map3_sight1, counts_map16_sight1,
                          counts_map17_sight1, counts_map18_sight1, counts_map9_sight1,
                          counts_map10_sight1, counts_map11_sight1, counts_map19_sight1,
                          counts_map20_sight1, counts_map4_sight1, counts_map5_sight1,
                          counts_map21_sight1, counts_map12_sight1)
    sight2_60_counts = c(counts_map14_sight2, counts_map15_sight2, counts_map7_sight2, counts_map8_sight2,
                          counts_map2_sight2, counts_map3_sight2, counts_map16_sight2,
                          counts_map17_sight2, counts_map18_sight2, counts_map9_sight2,
                          counts_map10_sight2, counts_map11_sight2, counts_map19_sight2,
                          counts_map20_sight2, counts_map4_sight2, counts_map5_sight2,
                          counts_map21_sight2, counts_map12_sight2)
    sight5_60_counts = c(counts_map14_sight5, counts_map15_sight5, counts_map7_sight5, counts_map8_sight5,
                          counts_map2_sight5, counts_map3_sight5, counts_map16_sight5,
                          counts_map17_sight5, counts_map18_sight5, counts_map9_sight5,
                          counts_map10_sight5, counts_map11_sight5, counts_map19_sight5,
                          counts_map20_sight5, counts_map4_sight5, counts_map5_sight5,
                          counts_map21_sight5, counts_map12_sight5)
    sight10_60_counts = c(counts_map14_sight10, counts_map15_sight10, counts_map7_sight10, counts_map8_sight10,
                           counts_map2_sight10, counts_map3_sight10, counts_map16_sight10,
                           counts_map17_sight10, counts_map18_sight10, counts_map9_sight10,
                           counts_map10_sight10, counts_map11_sight10, counts_map19_sight10,
                           counts_map20_sight10, counts_map4_sight10, counts_map5_sight10,
                           counts_map21_sight10, counts_map12_sight10)
    sight20_60_counts = c(counts_map14_sight20, counts_map15_sight20, counts_map7_sight20, counts_map8_sight20,
                           counts_map2_sight20, counts_map3_sight20, counts_map16_sight20,
                           counts_map17_sight20, counts_map18_sight20, counts_map9_sight20,
                           counts_map10_sight20, counts_map11_sight20, counts_map19_sight20,
                           counts_map20_sight20, counts_map4_sight20, counts_map5_sight20,
                           counts_map21_sight20, counts_map12_sight20)
    
    # Calculate sight counts w/ sightangle = 360:
    
    counts_map2_sight0 = sum(viable1[2, c(61, 63, 65, 67, 69)] == T)
    counts_map3_sight0 = sum(viable1[3, c(61, 63, 65, 67, 69)] == T)
    counts_map4_sight0 = sum(viable1[4, c(61, 63, 65, 67, 69)] == T)
    counts_map5_sight0 = sum(viable1[5, c(61, 63, 65, 67, 69)] == T)
    counts_map7_sight0 = sum(viable1[7, c(61, 63, 65, 67, 69)] == T)
    counts_map8_sight0 = sum(viable1[8, c(61, 63, 65, 67, 69)] == T)
    counts_map9_sight0 = sum(viable1[9, c(61, 63, 65, 67, 69)] == T)
    counts_map20_sight0 = sum(viable1[10, c(61, 63, 65, 67, 69)] == T)
    counts_map11_sight0 = sum(viable1[11, c(61, 63, 65, 67, 69)] == T)
    counts_map12_sight0 = sum(viable1[12, c(61, 63, 65, 67, 69)] == T)
    counts_map14_sight0 = sum(viable1[14, c(61, 63, 65, 67, 69)] == T)
    counts_map15_sight0 = sum(viable1[15, c(61, 63, 65, 67, 69)] == T)
    counts_map16_sight0 = sum(viable1[16, c(61, 63, 65, 67, 69)] == T)
    counts_map17_sight0 = sum(viable1[17, c(61, 63, 65, 67, 69)] == T)
    counts_map18_sight0 = sum(viable1[18, c(61, 63, 65, 67, 69)] == T)
    counts_map19_sight0 = sum(viable1[19, c(61, 63, 65, 67, 69)] == T)
    counts_map20_sight0 = sum(viable1[20, c(61, 63, 65, 67, 69)] == T)
    counts_map21_sight0 = sum(viable1[21, c(61, 63, 65, 67, 69)] == T)
    counts_map2_sight1 = sum(viable1[2, c(71, 73, 75, 77, 79)] == T)
    counts_map3_sight1 = sum(viable1[3, c(71, 73, 75, 77, 79)] == T)
    counts_map4_sight1 = sum(viable1[4, c(71, 73, 75, 77, 79)] == T)
    counts_map5_sight1 = sum(viable1[5, c(71, 73, 75, 77, 79)] == T)
    counts_map7_sight1 = sum(viable1[7, c(71, 73, 75, 77, 79)] == T)
    counts_map8_sight1 = sum(viable1[8, c(71, 73, 75, 77, 79)] == T)
    counts_map9_sight1 = sum(viable1[9, c(71, 73, 75, 77, 79)] == T)
    counts_map10_sight1 = sum(viable1[10, c(71, 73, 75, 77, 79)] == T)
    counts_map11_sight1 = sum(viable1[11, c(71, 73, 75, 77, 79)] == T)
    counts_map12_sight1 = sum(viable1[12, c(71, 73, 75, 77, 79)] == T)
    counts_map14_sight1 = sum(viable1[14, c(71, 73, 75, 77, 79)] == T)
    counts_map15_sight1 = sum(viable1[15, c(71, 73, 75, 77, 79)] == T)
    counts_map16_sight1 = sum(viable1[16, c(71, 73, 75, 77, 79)] == T)
    counts_map17_sight1 = sum(viable1[17, c(71, 73, 75, 77, 79)] == T)
    counts_map18_sight1 = sum(viable1[18, c(71, 73, 75, 77, 79)] == T)
    counts_map19_sight1 = sum(viable1[19, c(71, 73, 75, 77, 79)] == T)
    counts_map20_sight1 = sum(viable1[20, c(71, 73, 75, 77, 79)] == T)
    counts_map21_sight1 = sum(viable1[21, c(71, 73, 75, 77, 79)] == T)
    counts_map2_sight2 = sum(viable1[2, c(81, 83, 85, 87, 89)] == T)
    counts_map3_sight2 = sum(viable1[3, c(81, 83, 85, 87, 89)] == T)
    counts_map4_sight2 = sum(viable1[4, c(81, 83, 85, 87, 89)] == T)
    counts_map5_sight2 = sum(viable1[5, c(81, 83, 85, 87, 89)] == T)
    counts_map7_sight2 = sum(viable1[7, c(81, 83, 85, 87, 89)] == T)
    counts_map8_sight2 = sum(viable1[8, c(81, 83, 85, 87, 89)] == T)
    counts_map9_sight2 = sum(viable1[9, c(81, 83, 85, 87, 89)] == T)
    counts_map10_sight2 = sum(viable1[10, c(81, 83, 85, 87, 89)] == T)
    counts_map11_sight2 = sum(viable1[11, c(81, 83, 85, 87, 89)] == T)
    counts_map12_sight2 = sum(viable1[12, c(81, 83, 85, 87, 89)] == T)
    counts_map14_sight2 = sum(viable1[14, c(81, 83, 85, 87, 89)] == T)
    counts_map15_sight2 = sum(viable1[15, c(81, 83, 85, 87, 89)] == T)
    counts_map16_sight2 = sum(viable1[16, c(81, 83, 85, 87, 89)] == T)
    counts_map17_sight2 = sum(viable1[17, c(81, 83, 85, 87, 89)] == T)
    counts_map18_sight2 = sum(viable1[18, c(81, 83, 85, 87, 89)] == T)
    counts_map19_sight2 = sum(viable1[19, c(81, 83, 85, 87, 89)] == T)
    counts_map20_sight2 = sum(viable1[20, c(81, 83, 85, 87, 89)] == T)
    counts_map21_sight2 = sum(viable1[21, c(81, 83, 85, 87, 89)] == T)
    counts_map2_sight5 = sum(viable1[2, c(91, 93, 95, 97, 99)] == T)
    counts_map3_sight5 = sum(viable1[3, c(91, 93, 95, 97, 99)] == T)
    counts_map4_sight5 = sum(viable1[4, c(91, 93, 95, 97, 99)] == T)
    counts_map5_sight5 = sum(viable1[5, c(91, 93, 95, 97, 99)] == T)
    counts_map7_sight5 = sum(viable1[7, c(91, 93, 95, 97, 99)] == T)
    counts_map8_sight5 = sum(viable1[8, c(91, 93, 95, 97, 99)] == T)
    counts_map9_sight5 = sum(viable1[9, c(91, 93, 95, 97, 99)] == T)
    counts_map10_sight5 = sum(viable1[10, c(91, 93, 95, 97, 99)] == T)
    counts_map11_sight5 = sum(viable1[11, c(91, 93, 95, 97, 99)] == T)
    counts_map12_sight5 = sum(viable1[12, c(91, 93, 95, 97, 99)] == T)
    counts_map14_sight5 = sum(viable1[14, c(91, 93, 95, 97, 99)] == T)
    counts_map15_sight5 = sum(viable1[15, c(91, 93, 95, 97, 99)] == T)
    counts_map16_sight5 = sum(viable1[16, c(91, 93, 95, 97, 99)] == T)
    counts_map17_sight5 = sum(viable1[17, c(91, 93, 95, 97, 99)] == T)
    counts_map18_sight5 = sum(viable1[18, c(91, 93, 95, 97, 99)] == T)
    counts_map19_sight5 = sum(viable1[19, c(91, 93, 95, 97, 99)] == T)
    counts_map20_sight5 = sum(viable1[20, c(91, 93, 95, 97, 99)] == T)
    counts_map21_sight5 = sum(viable1[21, c(91, 93, 95, 97, 99)] == T)
    counts_map2_sight10 = sum(viable1[2, c(101, 103, 105, 107, 109)] == T)
    counts_map3_sight10 = sum(viable1[3, c(101, 103, 105, 107, 109)] == T)
    counts_map4_sight10 = sum(viable1[4, c(101, 103, 105, 107, 109)] == T)
    counts_map5_sight10 = sum(viable1[5, c(101, 103, 105, 107, 109)] == T)
    counts_map7_sight10 = sum(viable1[7, c(101, 103, 105, 107, 109)] == T)
    counts_map8_sight10 = sum(viable1[8, c(101, 103, 105, 107, 109)] == T)
    counts_map9_sight10 = sum(viable1[9, c(101, 103, 105, 107, 109)] == T)
    counts_map10_sight10 = sum(viable1[10, c(101, 103, 105, 107, 109)] == T)
    counts_map11_sight10 = sum(viable1[11, c(101, 103, 105, 107, 109)] == T)
    counts_map12_sight10 = sum(viable1[12, c(101, 103, 105, 107, 109)] == T)
    counts_map14_sight10 = sum(viable1[14, c(101, 103, 105, 107, 109)] == T)
    counts_map15_sight10 = sum(viable1[15, c(101, 103, 105, 107, 109)] == T)
    counts_map16_sight10 = sum(viable1[16, c(101, 103, 105, 107, 109)] == T)
    counts_map17_sight10 = sum(viable1[17, c(101, 103, 105, 107, 109)] == T)
    counts_map18_sight10 = sum(viable1[18, c(101, 103, 105, 107, 109)] == T)
    counts_map19_sight10 = sum(viable1[19, c(101, 103, 105, 107, 109)] == T)
    counts_map20_sight10 = sum(viable1[20, c(101, 103, 105, 107, 109)] == T)
    counts_map21_sight10 = sum(viable1[21, c(101, 103, 105, 107, 109)] == T)
    counts_map2_sight20 = sum(viable1[2, c(111, 113, 115, 117, 119)] == T)
    counts_map3_sight20 = sum(viable1[3, c(111, 113, 115, 117, 119)] == T)
    counts_map4_sight20 = sum(viable1[4, c(111, 113, 115, 117, 119)] == T)
    counts_map5_sight20 = sum(viable1[5, c(111, 113, 115, 117, 119)] == T)
    counts_map7_sight20 = sum(viable1[7, c(111, 113, 115, 117, 119)] == T)
    counts_map8_sight20 = sum(viable1[8, c(111, 113, 115, 117, 119)] == T)
    counts_map9_sight20 = sum(viable1[9, c(111, 113, 115, 117, 119)] == T)
    counts_map10_sight20 = sum(viable1[10, c(111, 113, 115, 117, 119)] == T)
    counts_map11_sight20 = sum(viable1[11, c(111, 113, 115, 117, 119)] == T)
    counts_map12_sight20 = sum(viable1[12, c(111, 113, 115, 117, 119)] == T)
    counts_map14_sight20 = sum(viable1[14, c(111, 113, 115, 117, 119)] == T)
    counts_map15_sight20 = sum(viable1[15, c(111, 113, 115, 117, 119)] == T)
    counts_map16_sight20 = sum(viable1[16, c(111, 113, 115, 117, 119)] == T)
    counts_map17_sight20 = sum(viable1[17, c(111, 113, 115, 117, 119)] == T)
    counts_map18_sight20 = sum(viable1[18, c(111, 113, 115, 117, 119)] == T)
    counts_map19_sight20 = sum(viable1[19, c(111, 113, 115, 117, 119)] == T)
    counts_map20_sight20 = sum(viable1[20, c(111, 113, 115, 117, 119)] == T)
    counts_map21_sight20 = sum(viable1[21, c(111, 113, 115, 117, 119)] == T)
    
    sight0_360_counts = c(counts_map14_sight0, counts_map15_sight0, counts_map7_sight0, counts_map8_sight0,
                         counts_map2_sight0, counts_map3_sight0, counts_map16_sight0,
                         counts_map17_sight0, counts_map18_sight0, counts_map9_sight0,
                         counts_map10_sight0, counts_map11_sight0, counts_map19_sight0,
                         counts_map20_sight0, counts_map4_sight0, counts_map5_sight0,
                         counts_map21_sight0, counts_map12_sight0)
    sight1_360_counts = c(counts_map14_sight1, counts_map15_sight1, counts_map7_sight1, counts_map8_sight1,
                          counts_map2_sight1, counts_map3_sight1, counts_map16_sight1,
                          counts_map17_sight1, counts_map18_sight1, counts_map9_sight1,
                          counts_map10_sight1, counts_map11_sight1, counts_map19_sight1,
                          counts_map20_sight1, counts_map4_sight1, counts_map5_sight1,
                          counts_map21_sight1, counts_map12_sight1)
    sight2_360_counts = c(counts_map14_sight2, counts_map15_sight2, counts_map7_sight2, counts_map8_sight2,
                          counts_map2_sight2, counts_map3_sight2, counts_map16_sight2,
                          counts_map17_sight2, counts_map18_sight2, counts_map9_sight2,
                          counts_map10_sight2, counts_map11_sight2, counts_map19_sight2,
                          counts_map20_sight2, counts_map4_sight2, counts_map5_sight2,
                          counts_map21_sight2, counts_map12_sight2)
    sight5_360_counts = c(counts_map14_sight5, counts_map15_sight5, counts_map7_sight5, counts_map8_sight5,
                          counts_map2_sight5, counts_map3_sight5, counts_map16_sight5,
                          counts_map17_sight5, counts_map18_sight5, counts_map9_sight5,
                          counts_map10_sight5, counts_map11_sight5, counts_map19_sight5,
                          counts_map20_sight5, counts_map4_sight5, counts_map5_sight5,
                          counts_map21_sight5, counts_map12_sight5)
    sight10_360_counts = c(counts_map14_sight10, counts_map15_sight10, counts_map7_sight10, counts_map8_sight10,
                           counts_map2_sight10, counts_map3_sight10, counts_map16_sight10,
                           counts_map17_sight10, counts_map18_sight10, counts_map9_sight10,
                           counts_map10_sight10, counts_map11_sight10, counts_map19_sight10,
                           counts_map20_sight10, counts_map4_sight10, counts_map5_sight10,
                           counts_map21_sight10, counts_map12_sight10)
    sight20_360_counts = c(counts_map14_sight20, counts_map15_sight20, counts_map7_sight20, counts_map8_sight20,
                           counts_map2_sight20, counts_map3_sight20, counts_map16_sight20,
                           counts_map17_sight20, counts_map18_sight20, counts_map9_sight20,
                           counts_map10_sight20, counts_map11_sight20, counts_map19_sight20,
                           counts_map20_sight20, counts_map4_sight20, counts_map5_sight20,
                           counts_map21_sight20, counts_map12_sight20)
    
    # Plot
    
    par(mfrow=c(2,2))
    xs = (sort(unique(xdata))/(500*500))*100
    xs = xs[4:21]
    plot(xs, alpha1_counts, type="o", pch=19, col="navyblue", xlab="Percent Cover", 
         ylab="Viable Instances", ylim=c(0,20), main="Alpha")
    lines(xs, alpha1.5_counts, type="o", pch=19, col="blue")
    lines(xs, alpha2_counts, type="o", pch=19, col="dodgerblue")
    lines(xs, alpha2.5_counts, type="o", pch=19, col="deepskyblue")
    lines(xs, alpha3_counts, type="o", pch=19, col="lightblue")
    legend("topright", c("Alpha=1", "Alpha=1.5", "Alpha=2", "Alpha=2.5", "Alpha=3"),
           col=c("navyblue", "blue", "dodgerblue", "deepskyblue", "lightblue"), pch=19, 
           cex=0.5)
    
    plot(xs, sight0_counts, type="o", pch=19, col="black", xlab="Percent Cover", 
         ylab="Viable Instances", ylim=c(0,20), main="Sight")
    lines(xs, sight1_counts, type="o", pch=19, col="gray17")
    lines(xs, sight2_counts, type="o", pch=19, col="gray27")
    lines(xs, sight5_counts, type="o", pch=19, col="gray37")
    lines(xs, sight10_counts, type="o", pch=19, col="gray47")
    lines(xs, sight20_counts, type="o", pch=19, col="gray57")
    legend("topright", c("Sight=0", "Sight=1", "Sight=2", "Sight=5", "Sight=10", "Sight=20"),
           col=c("black", "gray17", "gray27", "gray37", "gray47", "gray57"), pch=19,
           cex=0.5)
    
    plot(xs, sight0_60_counts, type="o", pch=19, col="#a63603", xlab="Percent Cover", 
         ylab="Viable Instances", ylim=c(0,20), main="Sight and SightAngle=60")
    lines(xs, sight1_60_counts, type="o", pch=19, col="#e6550d")
    lines(xs, sight2_60_counts, type="o", pch=19, col="#fd8d3c")
    lines(xs, sight5_60_counts, type="o", pch=19, col="#fdae6b")
    lines(xs, sight10_60_counts, type="o", pch=19, col="#fdd0a2")
    lines(xs, sight20_60_counts, type="o", pch=19, col="#feedde")
    legend("topright", c("Sight=0", "Sight=1", "Sight=2", "Sight=5", "Sight=10", "Sight=20"),
           col=c("#a63603", "#e6550d", "#fd8d3c", "#fdae6b", "#fdd0a2", "#feedde"), pch=19,
           cex=0.5)
    
    plot(xs, sight0_360_counts, type="o", pch=19, col="#54278f", xlab="Percent Cover", 
         ylab="Viable Instances", ylim=c(0,20), main="Sight and SightAngle=360")
    lines(xs, sight1_360_counts, type="o", pch=19, col="#756bb1")
    lines(xs, sight2_360_counts, type="o", pch=19, col="#9e9ac8")
    lines(xs, sight5_360_counts, type="o", pch=19, col="#bcbddc")
    lines(xs, sight10_360_counts, type="o", pch=19, col="#dadaeb")
    lines(xs, sight20_360_counts, type="o", pch=19, col="#f2f0f7")
    legend("topright", c("Sight=0", "Sight=1", "Sight=2", "Sight=5", "Sight=10", "Sight=20"),
           col=c("#54278f", "#756bb1", "#9e9ac8", "#bcbddc", "#dadaeb", "#f2f0f7"), pch=19,
           cex=0.5)
    
    # Plot differentiating by alpha
    
    par(mfrow=c(2,3))
    xs = (sort(unique(xdata))/(500*500))*100
    xs = xs[4:21]
    
    plot(xs, c(viable1[14, 71], viable1[15, 71], viable1[7, 71], viable1[8, 71], 
               viable1[2, 71], viable1[3, 71], viable1[16, 71], viable1[17, 71], 
               viable1[18, 71], viable1[9, 71], viable1[10, 71], viable1[11, 71], 
               viable1[19, 71], viable1[20, 71], viable1[4, 71], viable1[5, 71], 
               viable1[21, 71], viable1[12, 71]), xlab="Percent Cover", ylab="Count",
         type="o", pch=19, main="Sight = 1")
    lines(xs, c(viable1[14, 79], viable1[15, 79], viable1[7, 79], viable1[8, 79], 
                viable1[2, 79], viable1[3, 79], viable1[16, 79], viable1[17, 79], 
                viable1[18, 79], viable1[9, 79], viable1[10, 79], viable1[11, 79], 
                viable1[19, 79], viable1[20, 79], viable1[4, 79], viable1[5, 79], 
                viable1[21, 79], viable1[12, 79]), xlab="Percent Cover", ylab="Count",
          type="o", col="red", pch=19)
    lines(xs, c(viable1[14, 11], viable1[15, 11], viable1[7, 11], viable1[8, 11], 
                viable1[2, 11], viable1[3, 11], viable1[16, 11], viable1[17, 11], 
                viable1[18, 11], viable1[9, 11], viable1[10, 11], viable1[11, 11], 
                viable1[19, 11], viable1[20, 11], viable1[4, 11], viable1[5, 11], 
                viable1[21, 11], viable1[12, 11]), xlab="Percent Cover", ylab="Count",
          type="o", col="green", pch=19)
    lines(xs, c(viable1[14, 19], viable1[15, 19], viable1[7, 19], viable1[8, 19], 
                viable1[2, 19], viable1[3, 19], viable1[16, 19], viable1[17, 19], 
                viable1[18, 19], viable1[9, 19], viable1[10, 19], viable1[11, 19], 
                viable1[19, 19], viable1[20, 19], viable1[4, 19], viable1[5, 19], 
                viable1[21, 19], viable1[12, 19]), xlab="Percent Cover", ylab="Count",
          type="o", col="blue", pch=19)
    
    plot(xs, c(viable1[14, 81], viable1[15, 81], viable1[7, 81], viable1[8, 81], 
               viable1[2, 81], viable1[3, 81], viable1[16, 81], viable1[17, 81], 
               viable1[18, 81], viable1[9, 81], viable1[10, 81], viable1[11, 81], 
               viable1[19, 81], viable1[20, 81], viable1[4, 81], viable1[5, 81], 
               viable1[21, 81], viable1[12, 81]), xlab="Percent Cover", ylab="Count",
         type="o", pch=19, main="Sight = 2")
    lines(xs, c(viable1[14, 89], viable1[15, 89], viable1[7, 89], viable1[8, 89], 
                viable1[2, 89], viable1[3, 89], viable1[16, 89], viable1[17, 89], 
                viable1[18, 89], viable1[9, 89], viable1[10, 89], viable1[11, 89], 
                viable1[19, 89], viable1[20, 89], viable1[4, 89], viable1[5, 89], 
                viable1[21, 89], viable1[12, 89]), xlab="Percent Cover", ylab="Count",
          type="o", col="red", pch=19)
    lines(xs, c(viable1[14, 21], viable1[15, 21], viable1[7, 21], viable1[8, 21], 
                viable1[2, 21], viable1[3, 21], viable1[16, 21], viable1[17, 21], 
                viable1[18, 21], viable1[9, 21], viable1[10, 21], viable1[11, 21], 
                viable1[19, 21], viable1[20, 21], viable1[4, 21], viable1[5, 21], 
                viable1[21, 21], viable1[12, 21]), xlab="Percent Cover", ylab="Count",
          type="o", col="green", pch=19)
    lines(xs, c(viable1[14, 29], viable1[15, 29], viable1[7, 29], viable1[8, 29], 
                viable1[2, 29], viable1[3, 29], viable1[16, 29], viable1[17, 29], 
                viable1[18, 29], viable1[9, 29], viable1[10, 29], viable1[11, 29], 
                viable1[19, 29], viable1[20, 29], viable1[4, 29], viable1[5, 29], 
                viable1[21, 29], viable1[12, 29]), xlab="Percent Cover", ylab="Count",
          type="o", col="blue", pch=19)
    
    plot(xs, c(viable1[14, 91], viable1[15, 91], viable1[7, 91], viable1[8, 91], 
               viable1[2, 91], viable1[3, 91], viable1[16, 91], viable1[17, 91], 
               viable1[18, 91], viable1[9, 91], viable1[10, 91], viable1[11, 91], 
               viable1[19, 91], viable1[20, 91], viable1[4, 91], viable1[5, 91], 
               viable1[21, 91], viable1[12, 91]), xlab="Percent Cover", ylab="Count",
         type="o", pch=19, main="Sight = 5")
    lines(xs, c(viable1[14, 99], viable1[15, 99], viable1[7, 99], viable1[8, 99], 
                viable1[2, 99], viable1[3, 99], viable1[16, 99], viable1[17, 99], 
                viable1[18, 99], viable1[9, 99], viable1[10, 99], viable1[11, 99], 
                viable1[19, 99], viable1[20, 99], viable1[4, 99], viable1[5, 99], 
                viable1[21, 99], viable1[12, 99]), xlab="Percent Cover", ylab="Count",
          type="o", col="red", pch=19)
    lines(xs, c(viable1[14, 31], viable1[15, 31], viable1[7, 31], viable1[8, 31], 
                viable1[2, 31], viable1[3, 31], viable1[16, 31], viable1[17, 31], 
                viable1[18, 31], viable1[9, 31], viable1[10, 31], viable1[11, 31], 
                viable1[19, 31], viable1[20, 31], viable1[4, 31], viable1[5, 31], 
                viable1[21, 31], viable1[12, 31]), xlab="Percent Cover", ylab="Count",
          type="o", col="green", pch=19)
    lines(xs, c(viable1[14, 39], viable1[15, 39], viable1[7, 39], viable1[8, 39], 
                viable1[2, 39], viable1[3, 39], viable1[16, 39], viable1[17, 39], 
                viable1[18, 39], viable1[9, 39], viable1[10, 39], viable1[11, 39], 
                viable1[21, 39], viable1[20, 39], viable1[4, 39], viable1[5, 39], 
                viable1[21, 39], viable1[12, 39]), xlab="Percent Cover", ylab="Count",
          type="o", col="blue", pch=19)
    
    plot(xs, c(viable1[14, 101], viable1[15, 101], viable1[7, 101], viable1[8, 101], 
               viable1[2, 101], viable1[3, 101], viable1[16, 101], viable1[17, 101], 
               viable1[18, 101], viable1[9, 101], viable1[10, 101], viable1[11, 101], 
               viable1[19, 101], viable1[20, 101], viable1[4, 101], viable1[5, 101], 
               viable1[21, 101], viable1[12, 101]), xlab="Percent Cover", ylab="Count",
         type="o", pch=19, main="Sight = 10")
    lines(xs, c(viable1[14, 109], viable1[15, 109], viable1[7, 109], viable1[8, 109], 
                viable1[2, 109], viable1[3, 109], viable1[16, 109], viable1[17, 109], 
                viable1[18, 109], viable1[9, 109], viable1[10, 109], viable1[11, 109], 
                viable1[19, 109], viable1[20, 109], viable1[4, 109], viable1[5, 109], 
                viable1[21, 109], viable1[12, 109]), xlab="Percent Cover", ylab="Count",
          type="o", col="red", pch=19)
    lines(xs, c(viable1[14, 41], viable1[15, 41], viable1[7, 41], viable1[8, 41], 
                viable1[2, 41], viable1[3, 41], viable1[16, 41], viable1[17, 41], 
                viable1[18, 41], viable1[9, 41], viable1[10, 41], viable1[11, 41], 
                viable1[19, 41], viable1[20, 41], viable1[4, 41], viable1[5, 41], 
                viable1[21, 41], viable1[12, 41]), xlab="Percent Cover", ylab="Count",
          type="o", col="green", pch=19)
    lines(xs, c(viable1[14, 49], viable1[15, 49], viable1[7, 49], viable1[8, 49], 
                viable1[2, 49], viable1[3, 49], viable1[16, 49], viable1[17, 49], 
                viable1[18, 49], viable1[9, 49], viable1[10, 49], viable1[11, 49], 
                viable1[21, 49], viable1[20, 49], viable1[4, 49], viable1[5, 49], 
                viable1[21, 49], viable1[12, 49]), xlab="Percent Cover", ylab="Count",
          type="o", col="blue", pch=19)
    
    plot(xs, c(viable1[14, 111], viable1[15, 111], viable1[7, 111], viable1[8, 111], 
           viable1[2, 111], viable1[3, 111], viable1[16, 111], viable1[17, 111], 
           viable1[18, 111], viable1[9, 111], viable1[10, 111], viable1[11, 111], 
           viable1[19, 111], viable1[20, 111], viable1[4, 111], viable1[5, 111], 
           viable1[21, 111], viable1[12, 111]), xlab="Percent Cover", ylab="Count",
         type="o", pch=19, main="Sight = 20")
    lines(xs, c(viable1[14, 119], viable1[15, 119], viable1[7, 119], viable1[8, 119], 
               viable1[2, 119], viable1[3, 119], viable1[16, 119], viable1[17, 119], 
               viable1[18, 119], viable1[9, 119], viable1[10, 119], viable1[11, 119], 
               viable1[19, 119], viable1[20, 119], viable1[4, 119], viable1[5, 119], 
               viable1[21, 119], viable1[12, 119]), xlab="Percent Cover", ylab="Count",
          type="o", col="red", pch=19)
    lines(xs, c(viable1[14, 51], viable1[15, 51], viable1[7, 51], viable1[8, 51], 
                viable1[2, 51], viable1[3, 51], viable1[16, 51], viable1[17, 51], 
                viable1[18, 51], viable1[9, 51], viable1[10, 51], viable1[11, 51], 
                viable1[19, 51], viable1[20, 51], viable1[4, 51], viable1[5, 51], 
                viable1[21, 51], viable1[12, 51]), xlab="Percent Cover", ylab="Count",
          type="o", col="green", pch=19)
    lines(xs, c(viable1[14, 59], viable1[15, 59], viable1[7, 59], viable1[8, 59], 
                viable1[2, 59], viable1[3, 59], viable1[16, 59], viable1[17, 59], 
                viable1[18, 59], viable1[9, 59], viable1[10, 59], viable1[11, 59], 
                viable1[19, 59], viable1[20, 59], viable1[4, 59], viable1[5, 59], 
                viable1[21, 59], viable1[12, 59]), xlab="Percent Cover", ylab="Count",
          type="o", col="blue", pch=19)
    plot(xs, c(viable1[14, 59], viable1[15, 59], viable1[7, 59], viable1[8, 59], 
            viable1[2, 59], viable1[3, 59], viable1[16, 59], viable1[17, 59], 
            viable1[18, 59], viable1[9, 59], viable1[10, 59], viable1[11, 59], 
            viable1[19, 59], viable1[20, 59], viable1[4, 59], viable1[5, 59], 
            viable1[21, 59], viable1[12, 59]))
           
          
    
  }
  
}
  
  