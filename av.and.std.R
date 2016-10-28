av.and.std <- function(data, response){
  
  ### Takes four-factor ANOVA data from limpet_analysis.R, data. response
  ### indicates the desired response variable.  Finds the mean and standard
  ### deviation across replicates.  Returns a data.frame containing the 
  ### means and standard deviations for each combination of factors
  
  # Calculate means:
  
  averages1_0_60 <- c(mean(response[data$map==1 & data$alpha==1.0 & data$sight==0
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==2 & data$alpha==1.0 & data$sight==0
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==3 & data$alpha==1.0 & data$sight==0
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==4 & data$alpha==1.0 & data$sight==0
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==5 & data$alpha==1.0 & data$sight==0
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==6 & data$alpha==1.0 & data$sight==0
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==7 & data$alpha==1.0 & data$sight==0
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==8 & data$alpha==1.0 & data$sight==0
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==9 & data$alpha==1.0 & data$sight==0
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==10 & data$alpha==1.0 & data$sight==0
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==11 & data$alpha==1.0 & data$sight==0
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==12 & data$alpha==1.0 & data$sight==0
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==13 & data$alpha==1.0 & data$sight==0
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==14 & data$alpha==1.0 & data$sight==0
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==15 & data$alpha==1.0 & data$sight==0
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==16 & data$alpha==1.0 & data$sight==0
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==17 & data$alpha==1.0 & data$sight==0
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==18 & data$alpha==1.0 & data$sight==0
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==19 & data$alpha==1.0 & data$sight==0
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==20 & data$alpha==1.0 & data$sight==0
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==21 & data$alpha==1.0 & data$sight==0
                                          & data$sightangle==data$sightangle[1]]))
  
  averages1.5_0_60 <- c(mean(response[data$map==1 & data$alpha==1.5 & data$sight==0
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==2 & data$alpha==1.5 & data$sight==0
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==3 & data$alpha==1.5 & data$sight==0
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==4 & data$alpha==1.5 & data$sight==0
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==5 & data$alpha==1.5 & data$sight==0
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==6 & data$alpha==1.5 & data$sight==0
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==7 & data$alpha==1.5 & data$sight==0
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==8 & data$alpha==1.5 & data$sight==0
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==9 & data$alpha==1.5 & data$sight==0
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==10 & data$alpha==1.5 & data$sight==0
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==11 & data$alpha==1.5 & data$sight==0
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==12 & data$alpha==1.5 & data$sight==0
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==13 & data$alpha==1.5 & data$sight==0
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==14 & data$alpha==1.5 & data$sight==0
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==15 & data$alpha==1.5 & data$sight==0
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==16 & data$alpha==1.5 & data$sight==0
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==17 & data$alpha==1.5 & data$sight==0
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==18 & data$alpha==1.5 & data$sight==0
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==19 & data$alpha==1.5 & data$sight==0
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==20 & data$alpha==1.5 & data$sight==0
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==21 & data$alpha==1.5 & data$sight==0
                                            & data$sightangle==data$sightangle[1]]))
  
  averages2_0_60 <- c(mean(response[data$map==1 & data$alpha==2.0 & data$sight==0
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==2 & data$alpha==2.0 & data$sight==0
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==3 & data$alpha==2.0 & data$sight==0
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==4 & data$alpha==2.0 & data$sight==0
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==5 & data$alpha==2.0 & data$sight==0
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==6 & data$alpha==2.0 & data$sight==0
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==7 & data$alpha==2.0 & data$sight==0
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==8 & data$alpha==2.0 & data$sight==0
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==9 & data$alpha==2.0 & data$sight==0
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==10 & data$alpha==2.0 & data$sight==0
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==11 & data$alpha==2.0 & data$sight==0
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==12 & data$alpha==2.0 & data$sight==0
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==13 & data$alpha==2.0 & data$sight==0
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==14 & data$alpha==2.0 & data$sight==0
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==15 & data$alpha==2.0 & data$sight==0
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==16 & data$alpha==2.0 & data$sight==0
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==17 & data$alpha==2.0 & data$sight==0
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==18 & data$alpha==2.0 & data$sight==0
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==19 & data$alpha==2.0 & data$sight==0
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==20 & data$alpha==2.0 & data$sight==0
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==21 & data$alpha==2.0 & data$sight==0
                                          & data$sightangle==data$sightangle[1]]))
  
  averages2.5_0_60 <- c(mean(response[data$map==1 & data$alpha==2.5 & data$sight==0
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==2 & data$alpha==2.5 & data$sight==0
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==3 & data$alpha==2.5 & data$sight==0
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==4 & data$alpha==2.5 & data$sight==0
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==5 & data$alpha==2.5 & data$sight==0
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==6 & data$alpha==2.5 & data$sight==0
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==7 & data$alpha==2.5 & data$sight==0
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==8 & data$alpha==2.5 & data$sight==0
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==9 & data$alpha==2.5 & data$sight==0
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==10 & data$alpha==2.5 & data$sight==0
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==11 & data$alpha==2.5 & data$sight==0
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==12 & data$alpha==2.5 & data$sight==0
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==13 & data$alpha==2.5 & data$sight==0
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==14 & data$alpha==2.5 & data$sight==0
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==15 & data$alpha==2.5 & data$sight==0
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==16 & data$alpha==2.5 & data$sight==0
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==17 & data$alpha==2.5 & data$sight==0
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==18 & data$alpha==2.5 & data$sight==0
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==19 & data$alpha==2.5 & data$sight==0
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==20 & data$alpha==2.5 & data$sight==0
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==21 & data$alpha==2.5 & data$sight==0
                                            & data$sightangle==data$sightangle[1]]))
  
  averages3_0_60 <- c(mean(response[data$map==1 & data$alpha==3.0 & data$sight==0
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==2 & data$alpha==3.0 & data$sight==0
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==3 & data$alpha==3.0 & data$sight==0
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==4 & data$alpha==3.0 & data$sight==0
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==5 & data$alpha==3.0 & data$sight==0
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==6 & data$alpha==3.0 & data$sight==0
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==7 & data$alpha==3.0 & data$sight==0
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==8 & data$alpha==3.0 & data$sight==0
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==9 & data$alpha==3.0 & data$sight==0
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==10 & data$alpha==3.0 & data$sight==0
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==11 & data$alpha==3.0 & data$sight==0
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==12 & data$alpha==3.0 & data$sight==0
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==13 & data$alpha==3.0 & data$sight==0
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==14 & data$alpha==3.0 & data$sight==0
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==15 & data$alpha==3.0 & data$sight==0
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==16 & data$alpha==3.0 & data$sight==0
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==17 & data$alpha==3.0 & data$sight==0
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==18 & data$alpha==3.0 & data$sight==0
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==19 & data$alpha==3.0 & data$sight==0
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==20 & data$alpha==3.0 & data$sight==0
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==21 & data$alpha==3.0 & data$sight==0
                                          & data$sightangle==data$sightangle[1]]))
  
  averages1_1_60 <- c(mean(response[data$map==1 & data$alpha==1.0 & data$sight==1
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==2 & data$alpha==1.0 & data$sight==1
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==3 & data$alpha==1.0 & data$sight==1
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==4 & data$alpha==1.0 & data$sight==1
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==5 & data$alpha==1.0 & data$sight==1
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==6 & data$alpha==1.0 & data$sight==1
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==7 & data$alpha==1.0 & data$sight==1
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==8 & data$alpha==1.0 & data$sight==1
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==9 & data$alpha==1.0 & data$sight==1
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==10 & data$alpha==1.0 & data$sight==1
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==11 & data$alpha==1.0 & data$sight==1
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==12 & data$alpha==1.0 & data$sight==1
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==13 & data$alpha==1.0 & data$sight==1
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==14 & data$alpha==1.0 & data$sight==1
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==15 & data$alpha==1.0 & data$sight==1
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==16 & data$alpha==1.0 & data$sight==1
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==17 & data$alpha==1.0 & data$sight==1
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==18 & data$alpha==1.0 & data$sight==1
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==19 & data$alpha==1.0 & data$sight==1
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==20 & data$alpha==1.0 & data$sight==1
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==21 & data$alpha==1.0 & data$sight==1
                                          & data$sightangle==data$sightangle[1]]))
  
  averages1.5_1_60 <- c(mean(response[data$map==1 & data$alpha==1.5 & data$sight==1
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==2 & data$alpha==1.5 & data$sight==1
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==3 & data$alpha==1.5 & data$sight==1
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==4 & data$alpha==1.5 & data$sight==1
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==5 & data$alpha==1.5 & data$sight==1
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==6 & data$alpha==1.5 & data$sight==1
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==7 & data$alpha==1.5 & data$sight==1
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==8 & data$alpha==1.5 & data$sight==1
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==9 & data$alpha==1.5 & data$sight==1
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==10 & data$alpha==1.5 & data$sight==1
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==11 & data$alpha==1.5 & data$sight==1
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==12 & data$alpha==1.5 & data$sight==1
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==13 & data$alpha==1.5 & data$sight==1
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==14 & data$alpha==1.5 & data$sight==1
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==15 & data$alpha==1.5 & data$sight==1
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==16 & data$alpha==1.5 & data$sight==1
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==17 & data$alpha==1.5 & data$sight==1
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==18 & data$alpha==1.5 & data$sight==1
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==19 & data$alpha==1.5 & data$sight==1
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==20 & data$alpha==1.5 & data$sight==1
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==21 & data$alpha==1.5 & data$sight==1
                                            & data$sightangle==data$sightangle[1]]))
  
  averages2_1_60 <- c(mean(response[data$map==1 & data$alpha==2.0 & data$sight==1
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==2 & data$alpha==2.0 & data$sight==1
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==3 & data$alpha==2.0 & data$sight==1
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==4 & data$alpha==2.0 & data$sight==1
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==5 & data$alpha==2.0 & data$sight==1
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==6 & data$alpha==2.0 & data$sight==1
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==7 & data$alpha==2.0 & data$sight==1
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==8 & data$alpha==2.0 & data$sight==1
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==9 & data$alpha==2.0 & data$sight==1
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==10 & data$alpha==2.0 & data$sight==1
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==11 & data$alpha==2.0 & data$sight==1
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==12 & data$alpha==2.0 & data$sight==1
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==13 & data$alpha==2.0 & data$sight==1
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==14 & data$alpha==2.0 & data$sight==1
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==15 & data$alpha==2.0 & data$sight==1
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==16 & data$alpha==2.0 & data$sight==1
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==17 & data$alpha==2.0 & data$sight==1
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==18 & data$alpha==2.0 & data$sight==1
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==19 & data$alpha==2.0 & data$sight==1
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==20 & data$alpha==2.0 & data$sight==1
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==21 & data$alpha==2.0 & data$sight==1
                                          & data$sightangle==data$sightangle[1]]))
  
  averages2.5_1_60 <- c(mean(response[data$map==1 & data$alpha==2.5 & data$sight==1
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==2 & data$alpha==2.5 & data$sight==1
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==3 & data$alpha==2.5 & data$sight==1
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==4 & data$alpha==2.5 & data$sight==1
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==5 & data$alpha==2.5 & data$sight==1
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==6 & data$alpha==2.5 & data$sight==1
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==7 & data$alpha==2.5 & data$sight==1
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==8 & data$alpha==2.5 & data$sight==1
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==9 & data$alpha==2.5 & data$sight==1
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==10 & data$alpha==2.5 & data$sight==1
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==11 & data$alpha==2.5 & data$sight==1
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==12 & data$alpha==2.5 & data$sight==1
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==13 & data$alpha==2.5 & data$sight==1
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==14 & data$alpha==2.5 & data$sight==1
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==15 & data$alpha==2.5 & data$sight==1
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==16 & data$alpha==2.5 & data$sight==1
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==17 & data$alpha==2.5 & data$sight==1
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==18 & data$alpha==2.5 & data$sight==1
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==19 & data$alpha==2.5 & data$sight==1
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==20 & data$alpha==2.5 & data$sight==1
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==21 & data$alpha==2.5 & data$sight==1
                                            & data$sightangle==data$sightangle[1]]))
  
  averages3_1_60 <- c(mean(response[data$map==1 & data$alpha==3.0 & data$sight==1
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==2 & data$alpha==3.0 & data$sight==1
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==3 & data$alpha==3.0 & data$sight==1
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==4 & data$alpha==3.0 & data$sight==1
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==5 & data$alpha==3.0 & data$sight==1
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==6 & data$alpha==3.0 & data$sight==1
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==7 & data$alpha==3.0 & data$sight==1
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==8 & data$alpha==3.0 & data$sight==1
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==9 & data$alpha==3.0 & data$sight==1
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==10 & data$alpha==3.0 & data$sight==1
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==11 & data$alpha==3.0 & data$sight==1
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==12 & data$alpha==3.0 & data$sight==1
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==13 & data$alpha==3.0 & data$sight==1
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==14 & data$alpha==3.0 & data$sight==1
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==15 & data$alpha==3.0 & data$sight==1
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==16 & data$alpha==3.0 & data$sight==1
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==17 & data$alpha==3.0 & data$sight==1
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==18 & data$alpha==3.0 & data$sight==1
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==19 & data$alpha==3.0 & data$sight==1
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==20 & data$alpha==3.0 & data$sight==1
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==21 & data$alpha==3.0 & data$sight==1
                                          & data$sightangle==data$sightangle[1]]))
  
  averages1_2_60 <- c(mean(response[data$map==1 & data$alpha==1.0 & data$sight==2
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==2 & data$alpha==1.0 & data$sight==2
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==3 & data$alpha==1.0 & data$sight==2
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==4 & data$alpha==1.0 & data$sight==2
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==5 & data$alpha==1.0 & data$sight==2
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==6 & data$alpha==1.0 & data$sight==2
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==7 & data$alpha==1.0 & data$sight==2
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==8 & data$alpha==1.0 & data$sight==2
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==9 & data$alpha==1.0 & data$sight==2
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==10 & data$alpha==1.0 & data$sight==2
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==11 & data$alpha==1.0 & data$sight==2
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==12 & data$alpha==1.0 & data$sight==2
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==13 & data$alpha==1.0 & data$sight==2
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==14 & data$alpha==1.0 & data$sight==2
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==15 & data$alpha==1.0 & data$sight==2
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==16 & data$alpha==1.0 & data$sight==2
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==17 & data$alpha==1.0 & data$sight==2
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==18 & data$alpha==1.0 & data$sight==2
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==19 & data$alpha==1.0 & data$sight==2
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==20 & data$alpha==1.0 & data$sight==2
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==21 & data$alpha==1.0 & data$sight==2
                                          & data$sightangle==data$sightangle[1]]))
  
  averages1.5_2_60 <- c(mean(response[data$map==1 & data$alpha==1.5 & data$sight==2
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==2 & data$alpha==1.5 & data$sight==2
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==3 & data$alpha==1.5 & data$sight==2
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==4 & data$alpha==1.5 & data$sight==2
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==5 & data$alpha==1.5 & data$sight==2
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==6 & data$alpha==1.5 & data$sight==2
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==7 & data$alpha==1.5 & data$sight==2
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==8 & data$alpha==1.5 & data$sight==2
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==9 & data$alpha==1.5 & data$sight==2
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==10 & data$alpha==1.5 & data$sight==2
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==11 & data$alpha==1.5 & data$sight==2
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==12 & data$alpha==1.5 & data$sight==2
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==13 & data$alpha==1.5 & data$sight==2
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==14 & data$alpha==1.5 & data$sight==2
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==15 & data$alpha==1.5 & data$sight==2
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==16 & data$alpha==1.5 & data$sight==2
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==17 & data$alpha==1.5 & data$sight==2
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==18 & data$alpha==1.5 & data$sight==2
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==19 & data$alpha==1.5 & data$sight==2
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==20 & data$alpha==1.5 & data$sight==2
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==21 & data$alpha==1.5 & data$sight==2
                                            & data$sightangle==data$sightangle[1]]))
  
  averages2_2_60 <- c(mean(response[data$map==1 & data$alpha==2.0 & data$sight==2
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==2 & data$alpha==2.0 & data$sight==2
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==3 & data$alpha==2.0 & data$sight==2
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==4 & data$alpha==2.0 & data$sight==2
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==5 & data$alpha==2.0 & data$sight==2
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==6 & data$alpha==2.0 & data$sight==2
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==7 & data$alpha==2.0 & data$sight==2
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==8 & data$alpha==2.0 & data$sight==2
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==9 & data$alpha==2.0 & data$sight==2
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==10 & data$alpha==2.0 & data$sight==2
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==11 & data$alpha==2.0 & data$sight==2
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==12 & data$alpha==2.0 & data$sight==2
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==13 & data$alpha==2.0 & data$sight==2
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==14 & data$alpha==2.0 & data$sight==2
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==15 & data$alpha==2.0 & data$sight==2
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==16 & data$alpha==2.0 & data$sight==2
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==17 & data$alpha==2.0 & data$sight==2
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==18 & data$alpha==2.0 & data$sight==2
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==19 & data$alpha==2.0 & data$sight==2
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==20 & data$alpha==2.0 & data$sight==2
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==21 & data$alpha==2.0 & data$sight==2
                                          & data$sightangle==data$sightangle[1]]))
  
  averages2.5_2_60 <- c(mean(response[data$map==1 & data$alpha==2.5 & data$sight==2
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==2 & data$alpha==2.5 & data$sight==2
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==3 & data$alpha==2.5 & data$sight==2
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==4 & data$alpha==2.5 & data$sight==2
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==5 & data$alpha==2.5 & data$sight==2
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==6 & data$alpha==2.5 & data$sight==2
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==7 & data$alpha==2.5 & data$sight==2
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==8 & data$alpha==2.5 & data$sight==2
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==9 & data$alpha==2.5 & data$sight==2
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==10 & data$alpha==2.5 & data$sight==2
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==11 & data$alpha==2.5 & data$sight==2
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==12 & data$alpha==2.5 & data$sight==2
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==13 & data$alpha==2.5 & data$sight==2
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==14 & data$alpha==2.5 & data$sight==2
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==15 & data$alpha==2.5 & data$sight==2
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==16 & data$alpha==2.5 & data$sight==2
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==17 & data$alpha==2.5 & data$sight==2
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==18 & data$alpha==2.5 & data$sight==2
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==19 & data$alpha==2.5 & data$sight==2
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==20 & data$alpha==2.5 & data$sight==2
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==21 & data$alpha==2.5 & data$sight==2
                                            & data$sightangle==data$sightangle[1]]))
  
  averages3_2_60 <- c(mean(response[data$map==1 & data$alpha==3.0 & data$sight==2
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==2 & data$alpha==3.0 & data$sight==2
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==3 & data$alpha==3.0 & data$sight==2
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==4 & data$alpha==3.0 & data$sight==2
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==5 & data$alpha==3.0 & data$sight==2
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==6 & data$alpha==3.0 & data$sight==2
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==7 & data$alpha==3.0 & data$sight==2
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==8 & data$alpha==3.0 & data$sight==2
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==9 & data$alpha==3.0 & data$sight==2
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==10 & data$alpha==3.0 & data$sight==2
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==11 & data$alpha==3.0 & data$sight==2
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==12 & data$alpha==3.0 & data$sight==2
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==13 & data$alpha==3.0 & data$sight==2
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==14 & data$alpha==3.0 & data$sight==2
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==15 & data$alpha==3.0 & data$sight==2
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==16 & data$alpha==3.0 & data$sight==2
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==17 & data$alpha==3.0 & data$sight==2
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==18 & data$alpha==3.0 & data$sight==2
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==19 & data$alpha==3.0 & data$sight==2
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==20 & data$alpha==3.0 & data$sight==2
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==21 & data$alpha==3.0 & data$sight==2
                                          & data$sightangle==data$sightangle[1]]))
  
  averages1_5_60 <- c(mean(response[data$map==1 & data$alpha==1.0 & data$sight==5
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==2 & data$alpha==1.0 & data$sight==5
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==3 & data$alpha==1.0 & data$sight==5
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==4 & data$alpha==1.0 & data$sight==5
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==5 & data$alpha==1.0 & data$sight==5
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==6 & data$alpha==1.0 & data$sight==5
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==7 & data$alpha==1.0 & data$sight==5
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==8 & data$alpha==1.0 & data$sight==5
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==9 & data$alpha==1.0 & data$sight==5
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==10 & data$alpha==1.0 & data$sight==5
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==11 & data$alpha==1.0 & data$sight==5
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==12 & data$alpha==1.0 & data$sight==5
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==13 & data$alpha==1.0 & data$sight==5
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==14 & data$alpha==1.0 & data$sight==5
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==15 & data$alpha==1.0 & data$sight==5
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==16 & data$alpha==1.0 & data$sight==5
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==17 & data$alpha==1.0 & data$sight==5
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==18 & data$alpha==1.0 & data$sight==5
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==19 & data$alpha==1.0 & data$sight==5
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==20 & data$alpha==1.0 & data$sight==5
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==21 & data$alpha==1.0 & data$sight==5
                                          & data$sightangle==data$sightangle[1]]))
  
  averages1.5_5_60 <- c(mean(response[data$map==1 & data$alpha==1.5 & data$sight==5
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==2 & data$alpha==1.5 & data$sight==5
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==3 & data$alpha==1.5 & data$sight==5
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==4 & data$alpha==1.5 & data$sight==5
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==5 & data$alpha==1.5 & data$sight==5
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==6 & data$alpha==1.5 & data$sight==5
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==7 & data$alpha==1.5 & data$sight==5
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==8 & data$alpha==1.5 & data$sight==5
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==9 & data$alpha==1.5 & data$sight==5
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==10 & data$alpha==1.5 & data$sight==5
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==11 & data$alpha==1.5 & data$sight==5
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==12 & data$alpha==1.5 & data$sight==5
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==13 & data$alpha==1.5 & data$sight==5
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==14 & data$alpha==1.5 & data$sight==5
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==15 & data$alpha==1.5 & data$sight==5
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==16 & data$alpha==1.5 & data$sight==5
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==17 & data$alpha==1.5 & data$sight==5
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==18 & data$alpha==1.5 & data$sight==5
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==19 & data$alpha==1.5 & data$sight==5
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==20 & data$alpha==1.5 & data$sight==5
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==21 & data$alpha==1.5 & data$sight==5
                                            & data$sightangle==data$sightangle[1]]))
  
  averages2_5_60 <- c(mean(response[data$map==1 & data$alpha==2.0 & data$sight==5
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==2 & data$alpha==2.0 & data$sight==5
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==3 & data$alpha==2.0 & data$sight==5
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==4 & data$alpha==2.0 & data$sight==5
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==5 & data$alpha==2.0 & data$sight==5
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==6 & data$alpha==2.0 & data$sight==5
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==7 & data$alpha==2.0 & data$sight==5
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==8 & data$alpha==2.0 & data$sight==5
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==9 & data$alpha==2.0 & data$sight==5
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==10 & data$alpha==2.0 & data$sight==5
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==11 & data$alpha==2.0 & data$sight==5
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==12 & data$alpha==2.0 & data$sight==5
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==13 & data$alpha==2.0 & data$sight==5
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==14 & data$alpha==2.0 & data$sight==5
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==15 & data$alpha==2.0 & data$sight==5
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==16 & data$alpha==2.0 & data$sight==5
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==17 & data$alpha==2.0 & data$sight==5
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==18 & data$alpha==2.0 & data$sight==5
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==19 & data$alpha==2.0 & data$sight==5
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==20 & data$alpha==2.0 & data$sight==5
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==21 & data$alpha==2.0 & data$sight==5
                                          & data$sightangle==data$sightangle[1]]))
  
  averages2.5_5_60 <- c(mean(response[data$map==1 & data$alpha==2.5 & data$sight==5
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==2 & data$alpha==2.5 & data$sight==5
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==3 & data$alpha==2.5 & data$sight==5
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==4 & data$alpha==2.5 & data$sight==5
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==5 & data$alpha==2.5 & data$sight==5
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==6 & data$alpha==2.5 & data$sight==5
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==7 & data$alpha==2.5 & data$sight==5
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==8 & data$alpha==2.5 & data$sight==5
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==9 & data$alpha==2.5 & data$sight==5
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==10 & data$alpha==2.5 & data$sight==5
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==11 & data$alpha==2.5 & data$sight==5
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==12 & data$alpha==2.5 & data$sight==5
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==13 & data$alpha==2.5 & data$sight==5
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==14 & data$alpha==2.5 & data$sight==5
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==15 & data$alpha==2.5 & data$sight==5
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==16 & data$alpha==2.5 & data$sight==5
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==17 & data$alpha==2.5 & data$sight==5
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==18 & data$alpha==2.5 & data$sight==5
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==19 & data$alpha==2.5 & data$sight==5
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==20 & data$alpha==2.5 & data$sight==5
                                            & data$sightangle==data$sightangle[1]]),
                        mean(response[data$map==21 & data$alpha==2.5 & data$sight==5
                                            & data$sightangle==data$sightangle[1]]))
  
  averages3_5_60 <- c(mean(response[data$map==1 & data$alpha==3.0 & data$sight==5
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==2 & data$alpha==3.0 & data$sight==5
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==3 & data$alpha==3.0 & data$sight==5
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==4 & data$alpha==3.0 & data$sight==5
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==5 & data$alpha==3.0 & data$sight==5
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==6 & data$alpha==3.0 & data$sight==5
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==7 & data$alpha==3.0 & data$sight==5
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==8 & data$alpha==3.0 & data$sight==5
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==9 & data$alpha==3.0 & data$sight==5
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==10 & data$alpha==3.0 & data$sight==5
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==11 & data$alpha==3.0 & data$sight==5
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==12 & data$alpha==3.0 & data$sight==5
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==13 & data$alpha==3.0 & data$sight==5
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==14 & data$alpha==3.0 & data$sight==5
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==15 & data$alpha==3.0 & data$sight==5
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==16 & data$alpha==3.0 & data$sight==5
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==17 & data$alpha==3.0 & data$sight==5
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==18 & data$alpha==3.0 & data$sight==5
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==19 & data$alpha==3.0 & data$sight==5
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==20 & data$alpha==3.0 & data$sight==5
                                          & data$sightangle==data$sightangle[1]]),
                      mean(response[data$map==21 & data$alpha==3.0 & data$sight==5
                                          & data$sightangle==data$sightangle[1]]))
  
  averages1_10_60 <- c(mean(response[data$map==1 & data$alpha==1.0 & data$sight==10
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==2 & data$alpha==1.0 & data$sight==10
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==3 & data$alpha==1.0 & data$sight==10
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==4 & data$alpha==1.0 & data$sight==10
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==5 & data$alpha==1.0 & data$sight==10
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==6 & data$alpha==1.0 & data$sight==10
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==7 & data$alpha==1.0 & data$sight==10
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==8 & data$alpha==1.0 & data$sight==10
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==9 & data$alpha==1.0 & data$sight==10
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==10 & data$alpha==1.0 & data$sight==10
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==11 & data$alpha==1.0 & data$sight==10
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==12 & data$alpha==1.0 & data$sight==10
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==13 & data$alpha==1.0 & data$sight==10
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==14 & data$alpha==1.0 & data$sight==10
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==15 & data$alpha==1.0 & data$sight==10
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==16 & data$alpha==1.0 & data$sight==10
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==17 & data$alpha==1.0 & data$sight==10
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==18 & data$alpha==1.0 & data$sight==10
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==19 & data$alpha==1.0 & data$sight==10
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==20 & data$alpha==1.0 & data$sight==10
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==21 & data$alpha==1.0 & data$sight==10
                                           & data$sightangle==data$sightangle[1]]))
  
  averages1.5_10_60 <- c(mean(response[data$map==1 & data$alpha==1.5 & data$sight==10
                                             & data$sightangle==data$sightangle[1]]),
                         mean(response[data$map==2 & data$alpha==1.5 & data$sight==10
                                             & data$sightangle==data$sightangle[1]]),
                         mean(response[data$map==3 & data$alpha==1.5 & data$sight==10
                                             & data$sightangle==data$sightangle[1]]),
                         mean(response[data$map==4 & data$alpha==1.5 & data$sight==10
                                             & data$sightangle==data$sightangle[1]]),
                         mean(response[data$map==5 & data$alpha==1.5 & data$sight==10
                                             & data$sightangle==data$sightangle[1]]),
                         mean(response[data$map==6 & data$alpha==1.5 & data$sight==10
                                             & data$sightangle==data$sightangle[1]]),
                         mean(response[data$map==7 & data$alpha==1.5 & data$sight==10
                                             & data$sightangle==data$sightangle[1]]),
                         mean(response[data$map==8 & data$alpha==1.5 & data$sight==10
                                             & data$sightangle==data$sightangle[1]]),
                         mean(response[data$map==9 & data$alpha==1.5 & data$sight==10
                                             & data$sightangle==data$sightangle[1]]),
                         mean(response[data$map==10 & data$alpha==1.5 & data$sight==10
                                             & data$sightangle==data$sightangle[1]]),
                         mean(response[data$map==11 & data$alpha==1.5 & data$sight==10
                                             & data$sightangle==data$sightangle[1]]),
                         mean(response[data$map==12 & data$alpha==1.5 & data$sight==10
                                             & data$sightangle==data$sightangle[1]]),
                         mean(response[data$map==13 & data$alpha==1.5 & data$sight==10
                                             & data$sightangle==data$sightangle[1]]),
                         mean(response[data$map==14 & data$alpha==1.5 & data$sight==10
                                             & data$sightangle==data$sightangle[1]]),
                         mean(response[data$map==15 & data$alpha==1.5 & data$sight==10
                                             & data$sightangle==data$sightangle[1]]),
                         mean(response[data$map==16 & data$alpha==1.5 & data$sight==10
                                             & data$sightangle==data$sightangle[1]]),
                         mean(response[data$map==17 & data$alpha==1.5 & data$sight==10
                                             & data$sightangle==data$sightangle[1]]),
                         mean(response[data$map==18 & data$alpha==1.5 & data$sight==10
                                             & data$sightangle==data$sightangle[1]]),
                         mean(response[data$map==19 & data$alpha==1.5 & data$sight==10
                                             & data$sightangle==data$sightangle[1]]),
                         mean(response[data$map==20 & data$alpha==1.5 & data$sight==10
                                             & data$sightangle==data$sightangle[1]]),
                         mean(response[data$map==21 & data$alpha==1.5 & data$sight==10
                                             & data$sightangle==data$sightangle[1]]))
  
  averages2_10_60 <- c(mean(response[data$map==1 & data$alpha==2.0 & data$sight==10
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==2 & data$alpha==2.0 & data$sight==10
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==3 & data$alpha==2.0 & data$sight==10
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==4 & data$alpha==2.0 & data$sight==10
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==5 & data$alpha==2.0 & data$sight==10
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==6 & data$alpha==2.0 & data$sight==10
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==7 & data$alpha==2.0 & data$sight==10
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==8 & data$alpha==2.0 & data$sight==10
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==9 & data$alpha==2.0 & data$sight==10
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==10 & data$alpha==2.0 & data$sight==10
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==11 & data$alpha==2.0 & data$sight==10
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==12 & data$alpha==2.0 & data$sight==10
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==13 & data$alpha==2.0 & data$sight==10
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==14 & data$alpha==2.0 & data$sight==10
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==15 & data$alpha==2.0 & data$sight==10
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==16 & data$alpha==2.0 & data$sight==10
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==17 & data$alpha==2.0 & data$sight==10
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==18 & data$alpha==2.0 & data$sight==10
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==19 & data$alpha==2.0 & data$sight==10
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==20 & data$alpha==2.0 & data$sight==10
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==21 & data$alpha==2.0 & data$sight==10
                                           & data$sightangle==data$sightangle[1]]))
  
  averages2.5_10_60 <- c(mean(response[data$map==1 & data$alpha==2.5 & data$sight==10
                                             & data$sightangle==data$sightangle[1]]),
                         mean(response[data$map==2 & data$alpha==2.5 & data$sight==10
                                             & data$sightangle==data$sightangle[1]]),
                         mean(response[data$map==3 & data$alpha==2.5 & data$sight==10
                                             & data$sightangle==data$sightangle[1]]),
                         mean(response[data$map==4 & data$alpha==2.5 & data$sight==10
                                             & data$sightangle==data$sightangle[1]]),
                         mean(response[data$map==5 & data$alpha==2.5 & data$sight==10
                                             & data$sightangle==data$sightangle[1]]),
                         mean(response[data$map==6 & data$alpha==2.5 & data$sight==10
                                             & data$sightangle==data$sightangle[1]]),
                         mean(response[data$map==7 & data$alpha==2.5 & data$sight==10
                                             & data$sightangle==data$sightangle[1]]),
                         mean(response[data$map==8 & data$alpha==2.5 & data$sight==10
                                             & data$sightangle==data$sightangle[1]]),
                         mean(response[data$map==9 & data$alpha==2.5 & data$sight==10
                                             & data$sightangle==data$sightangle[1]]),
                         mean(response[data$map==10 & data$alpha==2.5 & data$sight==10
                                             & data$sightangle==data$sightangle[1]]),
                         mean(response[data$map==11 & data$alpha==2.5 & data$sight==10
                                             & data$sightangle==data$sightangle[1]]),
                         mean(response[data$map==12 & data$alpha==2.5 & data$sight==10
                                             & data$sightangle==data$sightangle[1]]),
                         mean(response[data$map==13 & data$alpha==2.5 & data$sight==10
                                             & data$sightangle==data$sightangle[1]]),
                         mean(response[data$map==14 & data$alpha==2.5 & data$sight==10
                                             & data$sightangle==data$sightangle[1]]),
                         mean(response[data$map==15 & data$alpha==2.5 & data$sight==10
                                             & data$sightangle==data$sightangle[1]]),
                         mean(response[data$map==16 & data$alpha==2.5 & data$sight==10
                                             & data$sightangle==data$sightangle[1]]),
                         mean(response[data$map==17 & data$alpha==2.5 & data$sight==10
                                             & data$sightangle==data$sightangle[1]]),
                         mean(response[data$map==18 & data$alpha==2.5 & data$sight==10
                                             & data$sightangle==data$sightangle[1]]),
                         mean(response[data$map==19 & data$alpha==2.5 & data$sight==10
                                             & data$sightangle==data$sightangle[1]]),
                         mean(response[data$map==20 & data$alpha==2.5 & data$sight==10
                                             & data$sightangle==data$sightangle[1]]),
                         mean(response[data$map==21 & data$alpha==2.5 & data$sight==10
                                             & data$sightangle==data$sightangle[1]]))
  
  averages3_10_60 <- c(mean(response[data$map==1 & data$alpha==3.0 & data$sight==10
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==2 & data$alpha==3.0 & data$sight==10
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==3 & data$alpha==3.0 & data$sight==10
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==4 & data$alpha==3.0 & data$sight==10
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==5 & data$alpha==3.0 & data$sight==10
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==6 & data$alpha==3.0 & data$sight==10
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==7 & data$alpha==3.0 & data$sight==10
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==8 & data$alpha==3.0 & data$sight==10
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==9 & data$alpha==3.0 & data$sight==10
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==10 & data$alpha==3.0 & data$sight==10
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==11 & data$alpha==3.0 & data$sight==10
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==12 & data$alpha==3.0 & data$sight==10
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==13 & data$alpha==3.0 & data$sight==10
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==14 & data$alpha==3.0 & data$sight==10
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==15 & data$alpha==3.0 & data$sight==10
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==16 & data$alpha==3.0 & data$sight==10
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==17 & data$alpha==3.0 & data$sight==10
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==18 & data$alpha==3.0 & data$sight==10
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==19 & data$alpha==3.0 & data$sight==10
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==20 & data$alpha==3.0 & data$sight==10
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==21 & data$alpha==3.0 & data$sight==10
                                           & data$sightangle==data$sightangle[1]]))
  
  averages1_20_60 <- c(mean(response[data$map==1 & data$alpha==1.0 & data$sight==20
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==2 & data$alpha==1.0 & data$sight==20
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==3 & data$alpha==1.0 & data$sight==20
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==4 & data$alpha==1.0 & data$sight==20
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==5 & data$alpha==1.0 & data$sight==20
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==6 & data$alpha==1.0 & data$sight==20
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==7 & data$alpha==1.0 & data$sight==20
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==8 & data$alpha==1.0 & data$sight==20
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==9 & data$alpha==1.0 & data$sight==20
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==10 & data$alpha==1.0 & data$sight==20
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==11 & data$alpha==1.0 & data$sight==20
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==12 & data$alpha==1.0 & data$sight==20
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==13 & data$alpha==1.0 & data$sight==20
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==14 & data$alpha==1.0 & data$sight==20
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==15 & data$alpha==1.0 & data$sight==20
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==16 & data$alpha==1.0 & data$sight==20
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==17 & data$alpha==1.0 & data$sight==20
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==18 & data$alpha==1.0 & data$sight==20
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==19 & data$alpha==1.0 & data$sight==20
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==20 & data$alpha==1.0 & data$sight==20
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==21 & data$alpha==1.0 & data$sight==20
                                           & data$sightangle==data$sightangle[1]]))
  
  averages1.5_20_60 <- c(mean(response[data$map==1 & data$alpha==1.5 & data$sight==20
                                             & data$sightangle==data$sightangle[1]]),
                         mean(response[data$map==2 & data$alpha==1.5 & data$sight==20
                                             & data$sightangle==data$sightangle[1]]),
                         mean(response[data$map==3 & data$alpha==1.5 & data$sight==20
                                             & data$sightangle==data$sightangle[1]]),
                         mean(response[data$map==4 & data$alpha==1.5 & data$sight==20
                                             & data$sightangle==data$sightangle[1]]),
                         mean(response[data$map==5 & data$alpha==1.5 & data$sight==20
                                             & data$sightangle==data$sightangle[1]]),
                         mean(response[data$map==6 & data$alpha==1.5 & data$sight==20
                                             & data$sightangle==data$sightangle[1]]),
                         mean(response[data$map==7 & data$alpha==1.5 & data$sight==20
                                             & data$sightangle==data$sightangle[1]]),
                         mean(response[data$map==8 & data$alpha==1.5 & data$sight==20
                                             & data$sightangle==data$sightangle[1]]),
                         mean(response[data$map==9 & data$alpha==1.5 & data$sight==20
                                             & data$sightangle==data$sightangle[1]]),
                         mean(response[data$map==10 & data$alpha==1.5 & data$sight==20
                                             & data$sightangle==data$sightangle[1]]),
                         mean(response[data$map==11 & data$alpha==1.5 & data$sight==20
                                             & data$sightangle==data$sightangle[1]]),
                         mean(response[data$map==12 & data$alpha==1.5 & data$sight==20
                                             & data$sightangle==data$sightangle[1]]),
                         mean(response[data$map==13 & data$alpha==1.5 & data$sight==20
                                             & data$sightangle==data$sightangle[1]]),
                         mean(response[data$map==14 & data$alpha==1.5 & data$sight==20
                                             & data$sightangle==data$sightangle[1]]),
                         mean(response[data$map==15 & data$alpha==1.5 & data$sight==20
                                             & data$sightangle==data$sightangle[1]]),
                         mean(response[data$map==16 & data$alpha==1.5 & data$sight==20
                                             & data$sightangle==data$sightangle[1]]),
                         mean(response[data$map==17 & data$alpha==1.5 & data$sight==20
                                             & data$sightangle==data$sightangle[1]]),
                         mean(response[data$map==18 & data$alpha==1.5 & data$sight==20
                                             & data$sightangle==data$sightangle[1]]),
                         mean(response[data$map==19 & data$alpha==1.5 & data$sight==20
                                             & data$sightangle==data$sightangle[1]]),
                         mean(response[data$map==20 & data$alpha==1.5 & data$sight==20
                                             & data$sightangle==data$sightangle[1]]),
                         mean(response[data$map==21 & data$alpha==1.5 & data$sight==20
                                             & data$sightangle==data$sightangle[1]]))
  
  averages2_20_60 <- c(mean(response[data$map==1 & data$alpha==2.0 & data$sight==20
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==2 & data$alpha==2.0 & data$sight==20
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==3 & data$alpha==2.0 & data$sight==20
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==4 & data$alpha==2.0 & data$sight==20
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==5 & data$alpha==2.0 & data$sight==20
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==6 & data$alpha==2.0 & data$sight==20
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==7 & data$alpha==2.0 & data$sight==20
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==8 & data$alpha==2.0 & data$sight==20
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==9 & data$alpha==2.0 & data$sight==20
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==10 & data$alpha==2.0 & data$sight==20
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==11 & data$alpha==2.0 & data$sight==20
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==12 & data$alpha==2.0 & data$sight==20
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==13 & data$alpha==2.0 & data$sight==20
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==14 & data$alpha==2.0 & data$sight==20
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==15 & data$alpha==2.0 & data$sight==20
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==16 & data$alpha==2.0 & data$sight==20
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==17 & data$alpha==2.0 & data$sight==20
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==18 & data$alpha==2.0 & data$sight==20
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==19 & data$alpha==2.0 & data$sight==20
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==20 & data$alpha==2.0 & data$sight==20
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==21 & data$alpha==2.0 & data$sight==20
                                           & data$sightangle==data$sightangle[1]]))
  
  averages2.5_20_60 <- c(mean(response[data$map==1 & data$alpha==2.5 & data$sight==20
                                             & data$sightangle==data$sightangle[1]]),
                         mean(response[data$map==2 & data$alpha==2.5 & data$sight==20
                                             & data$sightangle==data$sightangle[1]]),
                         mean(response[data$map==3 & data$alpha==2.5 & data$sight==20
                                             & data$sightangle==data$sightangle[1]]),
                         mean(response[data$map==4 & data$alpha==2.5 & data$sight==20
                                             & data$sightangle==data$sightangle[1]]),
                         mean(response[data$map==5 & data$alpha==2.5 & data$sight==20
                                             & data$sightangle==data$sightangle[1]]),
                         mean(response[data$map==6 & data$alpha==2.5 & data$sight==20
                                             & data$sightangle==data$sightangle[1]]),
                         mean(response[data$map==7 & data$alpha==2.5 & data$sight==20
                                             & data$sightangle==data$sightangle[1]]),
                         mean(response[data$map==8 & data$alpha==2.5 & data$sight==20
                                             & data$sightangle==data$sightangle[1]]),
                         mean(response[data$map==9 & data$alpha==2.5 & data$sight==20
                                             & data$sightangle==data$sightangle[1]]),
                         mean(response[data$map==10 & data$alpha==2.5 & data$sight==20
                                             & data$sightangle==data$sightangle[1]]),
                         mean(response[data$map==11 & data$alpha==2.5 & data$sight==20
                                             & data$sightangle==data$sightangle[1]]),
                         mean(response[data$map==12 & data$alpha==2.5 & data$sight==20
                                             & data$sightangle==data$sightangle[1]]),
                         mean(response[data$map==13 & data$alpha==2.5 & data$sight==20
                                             & data$sightangle==data$sightangle[1]]),
                         mean(response[data$map==14 & data$alpha==2.5 & data$sight==20
                                             & data$sightangle==data$sightangle[1]]),
                         mean(response[data$map==15 & data$alpha==2.5 & data$sight==20
                                             & data$sightangle==data$sightangle[1]]),
                         mean(response[data$map==16 & data$alpha==2.5 & data$sight==20
                                             & data$sightangle==data$sightangle[1]]),
                         mean(response[data$map==17 & data$alpha==2.5 & data$sight==20
                                             & data$sightangle==data$sightangle[1]]),
                         mean(response[data$map==18 & data$alpha==2.5 & data$sight==20
                                             & data$sightangle==data$sightangle[1]]),
                         mean(response[data$map==19 & data$alpha==2.5 & data$sight==20
                                             & data$sightangle==data$sightangle[1]]),
                         mean(response[data$map==20 & data$alpha==2.5 & data$sight==20
                                             & data$sightangle==data$sightangle[1]]),
                         mean(response[data$map==21 & data$alpha==2.5 & data$sight==20
                                             & data$sightangle==data$sightangle[1]]))
  
  averages3_20_60 <- c(mean(response[data$map==1 & data$alpha==3.0 & data$sight==20
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==2 & data$alpha==3.0 & data$sight==20
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==3 & data$alpha==3.0 & data$sight==20
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==4 & data$alpha==3.0 & data$sight==20
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==5 & data$alpha==3.0 & data$sight==20
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==6 & data$alpha==3.0 & data$sight==20
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==7 & data$alpha==3.0 & data$sight==20
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==8 & data$alpha==3.0 & data$sight==20
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==9 & data$alpha==3.0 & data$sight==20
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==10 & data$alpha==3.0 & data$sight==20
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==11 & data$alpha==3.0 & data$sight==20
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==12 & data$alpha==3.0 & data$sight==20
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==13 & data$alpha==3.0 & data$sight==20
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==14 & data$alpha==3.0 & data$sight==20
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==15 & data$alpha==3.0 & data$sight==20
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==16 & data$alpha==3.0 & data$sight==20
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==17 & data$alpha==3.0 & data$sight==20
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==18 & data$alpha==3.0 & data$sight==20
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==19 & data$alpha==3.0 & data$sight==20
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==20 & data$alpha==3.0 & data$sight==20
                                           & data$sightangle==data$sightangle[1]]),
                       mean(response[data$map==21 & data$alpha==3.0 & data$sight==20
                                           & data$sightangle==data$sightangle[1]]))
  
  averages1_0_360 <- c(mean(response[data$map==1 & data$alpha==1.0 & data$sight==0
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==2 & data$alpha==1.0 & data$sight==0
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==3 & data$alpha==1.0 & data$sight==0
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==4 & data$alpha==1.0 & data$sight==0
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==5 & data$alpha==1.0 & data$sight==0
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==6 & data$alpha==1.0 & data$sight==0
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==7 & data$alpha==1.0 & data$sight==0
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==8 & data$alpha==1.0 & data$sight==0
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==9 & data$alpha==1.0 & data$sight==0
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==10 & data$alpha==1.0 & data$sight==0
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==11 & data$alpha==1.0 & data$sight==0
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==12 & data$alpha==1.0 & data$sight==0
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==13 & data$alpha==1.0 & data$sight==0
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==14 & data$alpha==1.0 & data$sight==0
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==15 & data$alpha==1.0 & data$sight==0
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==16 & data$alpha==1.0 & data$sight==0
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==17 & data$alpha==1.0 & data$sight==0
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==18 & data$alpha==1.0 & data$sight==0
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==19 & data$alpha==1.0 & data$sight==0
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==20 & data$alpha==1.0 & data$sight==0
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==21 & data$alpha==1.0 & data$sight==0
                                           & data$sightangle==data$sightangle[11]]))
  
  averages1.5_0_360 <- c(mean(response[data$map==1 & data$alpha==1.5 & data$sight==0
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==2 & data$alpha==1.5 & data$sight==0
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==3 & data$alpha==1.5 & data$sight==0
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==4 & data$alpha==1.5 & data$sight==0
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==5 & data$alpha==1.5 & data$sight==0
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==6 & data$alpha==1.5 & data$sight==0
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==7 & data$alpha==1.5 & data$sight==0
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==8 & data$alpha==1.5 & data$sight==0
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==9 & data$alpha==1.5 & data$sight==0
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==10 & data$alpha==1.5 & data$sight==0
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==11 & data$alpha==1.5 & data$sight==0
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==12 & data$alpha==1.5 & data$sight==0
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==13 & data$alpha==1.5 & data$sight==0
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==14 & data$alpha==1.5 & data$sight==0
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==15 & data$alpha==1.5 & data$sight==0
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==16 & data$alpha==1.5 & data$sight==0
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==17 & data$alpha==1.5 & data$sight==0
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==18 & data$alpha==1.5 & data$sight==0
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==19 & data$alpha==1.5 & data$sight==0
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==20 & data$alpha==1.5 & data$sight==0
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==21 & data$alpha==1.5 & data$sight==0
                                             & data$sightangle==data$sightangle[11]]))
  
  averages2_0_360 <- c(mean(response[data$map==1 & data$alpha==2.0 & data$sight==0
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==2 & data$alpha==2.0 & data$sight==0
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==3 & data$alpha==2.0 & data$sight==0
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==4 & data$alpha==2.0 & data$sight==0
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==5 & data$alpha==2.0 & data$sight==0
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==6 & data$alpha==2.0 & data$sight==0
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==7 & data$alpha==2.0 & data$sight==0
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==8 & data$alpha==2.0 & data$sight==0
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==9 & data$alpha==2.0 & data$sight==0
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==10 & data$alpha==2.0 & data$sight==0
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==11 & data$alpha==2.0 & data$sight==0
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==12 & data$alpha==2.0 & data$sight==0
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==13 & data$alpha==2.0 & data$sight==0
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==14 & data$alpha==2.0 & data$sight==0
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==15 & data$alpha==2.0 & data$sight==0
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==16 & data$alpha==2.0 & data$sight==0
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==17 & data$alpha==2.0 & data$sight==0
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==18 & data$alpha==2.0 & data$sight==0
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==19 & data$alpha==2.0 & data$sight==0
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==20 & data$alpha==2.0 & data$sight==0
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==21 & data$alpha==2.0 & data$sight==0
                                           & data$sightangle==data$sightangle[11]]))
  
  averages2.5_0_360 <- c(mean(response[data$map==1 & data$alpha==2.5 & data$sight==0
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==2 & data$alpha==2.5 & data$sight==0
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==3 & data$alpha==2.5 & data$sight==0
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==4 & data$alpha==2.5 & data$sight==0
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==5 & data$alpha==2.5 & data$sight==0
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==6 & data$alpha==2.5 & data$sight==0
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==7 & data$alpha==2.5 & data$sight==0
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==8 & data$alpha==2.5 & data$sight==0
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==9 & data$alpha==2.5 & data$sight==0
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==10 & data$alpha==2.5 & data$sight==0
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==11 & data$alpha==2.5 & data$sight==0
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==12 & data$alpha==2.5 & data$sight==0
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==13 & data$alpha==2.5 & data$sight==0
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==14 & data$alpha==2.5 & data$sight==0
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==15 & data$alpha==2.5 & data$sight==0
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==16 & data$alpha==2.5 & data$sight==0
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==17 & data$alpha==2.5 & data$sight==0
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==18 & data$alpha==2.5 & data$sight==0
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==19 & data$alpha==2.5 & data$sight==0
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==20 & data$alpha==2.5 & data$sight==0
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==21 & data$alpha==2.5 & data$sight==0
                                             & data$sightangle==data$sightangle[11]]))
  
  averages3_0_360 <- c(mean(response[data$map==1 & data$alpha==3.0 & data$sight==0
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==2 & data$alpha==3.0 & data$sight==0
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==3 & data$alpha==3.0 & data$sight==0
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==4 & data$alpha==3.0 & data$sight==0
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==5 & data$alpha==3.0 & data$sight==0
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==6 & data$alpha==3.0 & data$sight==0
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==7 & data$alpha==3.0 & data$sight==0
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==8 & data$alpha==3.0 & data$sight==0
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==9 & data$alpha==3.0 & data$sight==0
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==10 & data$alpha==3.0 & data$sight==0
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==11 & data$alpha==3.0 & data$sight==0
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==12 & data$alpha==3.0 & data$sight==0
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==13 & data$alpha==3.0 & data$sight==0
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==14 & data$alpha==3.0 & data$sight==0
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==15 & data$alpha==3.0 & data$sight==0
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==16 & data$alpha==3.0 & data$sight==0
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==17 & data$alpha==3.0 & data$sight==0
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==18 & data$alpha==3.0 & data$sight==0
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==19 & data$alpha==3.0 & data$sight==0
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==20 & data$alpha==3.0 & data$sight==0
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==21 & data$alpha==3.0 & data$sight==0
                                           & data$sightangle==data$sightangle[11]]))
  
  averages1_1_360 <- c(mean(response[data$map==1 & data$alpha==1.0 & data$sight==1
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==2 & data$alpha==1.0 & data$sight==1
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==3 & data$alpha==1.0 & data$sight==1
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==4 & data$alpha==1.0 & data$sight==1
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==5 & data$alpha==1.0 & data$sight==1
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==6 & data$alpha==1.0 & data$sight==1
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==7 & data$alpha==1.0 & data$sight==1
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==8 & data$alpha==1.0 & data$sight==1
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==9 & data$alpha==1.0 & data$sight==1
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==10 & data$alpha==1.0 & data$sight==1
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==11 & data$alpha==1.0 & data$sight==1
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==12 & data$alpha==1.0 & data$sight==1
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==13 & data$alpha==1.0 & data$sight==1
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==14 & data$alpha==1.0 & data$sight==1
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==15 & data$alpha==1.0 & data$sight==1
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==16 & data$alpha==1.0 & data$sight==1
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==17 & data$alpha==1.0 & data$sight==1
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==18 & data$alpha==1.0 & data$sight==1
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==19 & data$alpha==1.0 & data$sight==1
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==20 & data$alpha==1.0 & data$sight==1
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==21 & data$alpha==1.0 & data$sight==1
                                           & data$sightangle==data$sightangle[11]]))
  
  averages1.5_1_360 <- c(mean(response[data$map==1 & data$alpha==1.5 & data$sight==1
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==2 & data$alpha==1.5 & data$sight==1
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==3 & data$alpha==1.5 & data$sight==1
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==4 & data$alpha==1.5 & data$sight==1
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==5 & data$alpha==1.5 & data$sight==1
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==6 & data$alpha==1.5 & data$sight==1
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==7 & data$alpha==1.5 & data$sight==1
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==8 & data$alpha==1.5 & data$sight==1
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==9 & data$alpha==1.5 & data$sight==1
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==10 & data$alpha==1.5 & data$sight==1
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==11 & data$alpha==1.5 & data$sight==1
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==12 & data$alpha==1.5 & data$sight==1
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==13 & data$alpha==1.5 & data$sight==1
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==14 & data$alpha==1.5 & data$sight==1
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==15 & data$alpha==1.5 & data$sight==1
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==16 & data$alpha==1.5 & data$sight==1
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==17 & data$alpha==1.5 & data$sight==1
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==18 & data$alpha==1.5 & data$sight==1
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==19 & data$alpha==1.5 & data$sight==1
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==20 & data$alpha==1.5 & data$sight==1
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==21 & data$alpha==1.5 & data$sight==1
                                             & data$sightangle==data$sightangle[11]]))
  
  averages2_1_360 <- c(mean(response[data$map==1 & data$alpha==2.0 & data$sight==1
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==2 & data$alpha==2.0 & data$sight==1
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==3 & data$alpha==2.0 & data$sight==1
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==4 & data$alpha==2.0 & data$sight==1
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==5 & data$alpha==2.0 & data$sight==1
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==6 & data$alpha==2.0 & data$sight==1
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==7 & data$alpha==2.0 & data$sight==1
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==8 & data$alpha==2.0 & data$sight==1
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==9 & data$alpha==2.0 & data$sight==1
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==10 & data$alpha==2.0 & data$sight==1
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==11 & data$alpha==2.0 & data$sight==1
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==12 & data$alpha==2.0 & data$sight==1
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==13 & data$alpha==2.0 & data$sight==1
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==14 & data$alpha==2.0 & data$sight==1
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==15 & data$alpha==2.0 & data$sight==1
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==16 & data$alpha==2.0 & data$sight==1
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==17 & data$alpha==2.0 & data$sight==1
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==18 & data$alpha==2.0 & data$sight==1
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==19 & data$alpha==2.0 & data$sight==1
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==20 & data$alpha==2.0 & data$sight==1
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==21 & data$alpha==2.0 & data$sight==1
                                           & data$sightangle==data$sightangle[11]]))
  
  averages2.5_1_360 <- c(mean(response[data$map==1 & data$alpha==2.5 & data$sight==1
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==2 & data$alpha==2.5 & data$sight==1
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==3 & data$alpha==2.5 & data$sight==1
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==4 & data$alpha==2.5 & data$sight==1
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==5 & data$alpha==2.5 & data$sight==1
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==6 & data$alpha==2.5 & data$sight==1
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==7 & data$alpha==2.5 & data$sight==1
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==8 & data$alpha==2.5 & data$sight==1
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==9 & data$alpha==2.5 & data$sight==1
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==10 & data$alpha==2.5 & data$sight==1
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==11 & data$alpha==2.5 & data$sight==1
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==12 & data$alpha==2.5 & data$sight==1
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==13 & data$alpha==2.5 & data$sight==1
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==14 & data$alpha==2.5 & data$sight==1
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==15 & data$alpha==2.5 & data$sight==1
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==16 & data$alpha==2.5 & data$sight==1
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==17 & data$alpha==2.5 & data$sight==1
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==18 & data$alpha==2.5 & data$sight==1
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==19 & data$alpha==2.5 & data$sight==1
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==20 & data$alpha==2.5 & data$sight==1
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==21 & data$alpha==2.5 & data$sight==1
                                             & data$sightangle==data$sightangle[11]]))
  
  averages3_1_360 <- c(mean(response[data$map==1 & data$alpha==3.0 & data$sight==1
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==2 & data$alpha==3.0 & data$sight==1
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==3 & data$alpha==3.0 & data$sight==1
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==4 & data$alpha==3.0 & data$sight==1
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==5 & data$alpha==3.0 & data$sight==1
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==6 & data$alpha==3.0 & data$sight==1
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==7 & data$alpha==3.0 & data$sight==1
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==8 & data$alpha==3.0 & data$sight==1
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==9 & data$alpha==3.0 & data$sight==1
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==10 & data$alpha==3.0 & data$sight==1
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==11 & data$alpha==3.0 & data$sight==1
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==12 & data$alpha==3.0 & data$sight==1
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==13 & data$alpha==3.0 & data$sight==1
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==14 & data$alpha==3.0 & data$sight==1
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==15 & data$alpha==3.0 & data$sight==1
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==16 & data$alpha==3.0 & data$sight==1
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==17 & data$alpha==3.0 & data$sight==1
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==18 & data$alpha==3.0 & data$sight==1
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==19 & data$alpha==3.0 & data$sight==1
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==20 & data$alpha==3.0 & data$sight==1
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==21 & data$alpha==3.0 & data$sight==1
                                           & data$sightangle==data$sightangle[11]]))
  
  averages1_2_360 <- c(mean(response[data$map==1 & data$alpha==1.0 & data$sight==2
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==2 & data$alpha==1.0 & data$sight==2
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==3 & data$alpha==1.0 & data$sight==2
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==4 & data$alpha==1.0 & data$sight==2
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==5 & data$alpha==1.0 & data$sight==2
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==6 & data$alpha==1.0 & data$sight==2
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==7 & data$alpha==1.0 & data$sight==2
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==8 & data$alpha==1.0 & data$sight==2
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==9 & data$alpha==1.0 & data$sight==2
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==10 & data$alpha==1.0 & data$sight==2
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==11 & data$alpha==1.0 & data$sight==2
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==12 & data$alpha==1.0 & data$sight==2
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==13 & data$alpha==1.0 & data$sight==2
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==14 & data$alpha==1.0 & data$sight==2
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==15 & data$alpha==1.0 & data$sight==2
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==16 & data$alpha==1.0 & data$sight==2
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==17 & data$alpha==1.0 & data$sight==2
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==18 & data$alpha==1.0 & data$sight==2
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==19 & data$alpha==1.0 & data$sight==2
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==20 & data$alpha==1.0 & data$sight==2
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==21 & data$alpha==1.0 & data$sight==2
                                           & data$sightangle==data$sightangle[11]]))
  
  averages1.5_2_360 <- c(mean(response[data$map==1 & data$alpha==1.5 & data$sight==2
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==2 & data$alpha==1.5 & data$sight==2
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==3 & data$alpha==1.5 & data$sight==2
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==4 & data$alpha==1.5 & data$sight==2
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==5 & data$alpha==1.5 & data$sight==2
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==6 & data$alpha==1.5 & data$sight==2
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==7 & data$alpha==1.5 & data$sight==2
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==8 & data$alpha==1.5 & data$sight==2
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==9 & data$alpha==1.5 & data$sight==2
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==10 & data$alpha==1.5 & data$sight==2
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==11 & data$alpha==1.5 & data$sight==2
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==12 & data$alpha==1.5 & data$sight==2
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==13 & data$alpha==1.5 & data$sight==2
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==14 & data$alpha==1.5 & data$sight==2
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==15 & data$alpha==1.5 & data$sight==2
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==16 & data$alpha==1.5 & data$sight==2
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==17 & data$alpha==1.5 & data$sight==2
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==18 & data$alpha==1.5 & data$sight==2
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==19 & data$alpha==1.5 & data$sight==2
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==20 & data$alpha==1.5 & data$sight==2
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==21 & data$alpha==1.5 & data$sight==2
                                             & data$sightangle==data$sightangle[11]]))
  
  averages2_2_360 <- c(mean(response[data$map==1 & data$alpha==2.0 & data$sight==2
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==2 & data$alpha==2.0 & data$sight==2
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==3 & data$alpha==2.0 & data$sight==2
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==4 & data$alpha==2.0 & data$sight==2
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==5 & data$alpha==2.0 & data$sight==2
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==6 & data$alpha==2.0 & data$sight==2
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==7 & data$alpha==2.0 & data$sight==2
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==8 & data$alpha==2.0 & data$sight==2
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==9 & data$alpha==2.0 & data$sight==2
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==10 & data$alpha==2.0 & data$sight==2
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==11 & data$alpha==2.0 & data$sight==2
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==12 & data$alpha==2.0 & data$sight==2
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==13 & data$alpha==2.0 & data$sight==2
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==14 & data$alpha==2.0 & data$sight==2
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==15 & data$alpha==2.0 & data$sight==2
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==16 & data$alpha==2.0 & data$sight==2
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==17 & data$alpha==2.0 & data$sight==2
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==18 & data$alpha==2.0 & data$sight==2
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==19 & data$alpha==2.0 & data$sight==2
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==20 & data$alpha==2.0 & data$sight==2
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==21 & data$alpha==2.0 & data$sight==2
                                           & data$sightangle==data$sightangle[11]]))
  
  averages2.5_2_360 <- c(mean(response[data$map==1 & data$alpha==2.5 & data$sight==2
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==2 & data$alpha==2.5 & data$sight==2
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==3 & data$alpha==2.5 & data$sight==2
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==4 & data$alpha==2.5 & data$sight==2
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==5 & data$alpha==2.5 & data$sight==2
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==6 & data$alpha==2.5 & data$sight==2
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==7 & data$alpha==2.5 & data$sight==2
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==8 & data$alpha==2.5 & data$sight==2
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==9 & data$alpha==2.5 & data$sight==2
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==10 & data$alpha==2.5 & data$sight==2
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==11 & data$alpha==2.5 & data$sight==2
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==12 & data$alpha==2.5 & data$sight==2
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==13 & data$alpha==2.5 & data$sight==2
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==14 & data$alpha==2.5 & data$sight==2
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==15 & data$alpha==2.5 & data$sight==2
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==16 & data$alpha==2.5 & data$sight==2
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==17 & data$alpha==2.5 & data$sight==2
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==18 & data$alpha==2.5 & data$sight==2
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==19 & data$alpha==2.5 & data$sight==2
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==20 & data$alpha==2.5 & data$sight==2
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==21 & data$alpha==2.5 & data$sight==2
                                             & data$sightangle==data$sightangle[11]]))
  
  averages3_2_360 <- c(mean(response[data$map==1 & data$alpha==3.0 & data$sight==2
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==2 & data$alpha==3.0 & data$sight==2
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==3 & data$alpha==3.0 & data$sight==2
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==4 & data$alpha==3.0 & data$sight==2
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==5 & data$alpha==3.0 & data$sight==2
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==6 & data$alpha==3.0 & data$sight==2
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==7 & data$alpha==3.0 & data$sight==2
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==8 & data$alpha==3.0 & data$sight==2
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==9 & data$alpha==3.0 & data$sight==2
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==10 & data$alpha==3.0 & data$sight==2
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==11 & data$alpha==3.0 & data$sight==2
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==12 & data$alpha==3.0 & data$sight==2
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==13 & data$alpha==3.0 & data$sight==2
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==14 & data$alpha==3.0 & data$sight==2
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==15 & data$alpha==3.0 & data$sight==2
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==16 & data$alpha==3.0 & data$sight==2
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==17 & data$alpha==3.0 & data$sight==2
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==18 & data$alpha==3.0 & data$sight==2
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==19 & data$alpha==3.0 & data$sight==2
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==20 & data$alpha==3.0 & data$sight==2
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==21 & data$alpha==3.0 & data$sight==2
                                           & data$sightangle==data$sightangle[11]]))
  
  averages1_5_360 <- c(mean(response[data$map==1 & data$alpha==1.0 & data$sight==5
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==2 & data$alpha==1.0 & data$sight==5
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==3 & data$alpha==1.0 & data$sight==5
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==4 & data$alpha==1.0 & data$sight==5
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==5 & data$alpha==1.0 & data$sight==5
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==6 & data$alpha==1.0 & data$sight==5
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==7 & data$alpha==1.0 & data$sight==5
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==8 & data$alpha==1.0 & data$sight==5
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==9 & data$alpha==1.0 & data$sight==5
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==10 & data$alpha==1.0 & data$sight==5
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==11 & data$alpha==1.0 & data$sight==5
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==12 & data$alpha==1.0 & data$sight==5
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==13 & data$alpha==1.0 & data$sight==5
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==14 & data$alpha==1.0 & data$sight==5
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==15 & data$alpha==1.0 & data$sight==5
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==16 & data$alpha==1.0 & data$sight==5
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==17 & data$alpha==1.0 & data$sight==5
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==18 & data$alpha==1.0 & data$sight==5
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==19 & data$alpha==1.0 & data$sight==5
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==20 & data$alpha==1.0 & data$sight==5
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==21 & data$alpha==1.0 & data$sight==5
                                           & data$sightangle==data$sightangle[11]]))
  
  averages1.5_5_360 <- c(mean(response[data$map==1 & data$alpha==1.5 & data$sight==5
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==2 & data$alpha==1.5 & data$sight==5
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==3 & data$alpha==1.5 & data$sight==5
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==4 & data$alpha==1.5 & data$sight==5
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==5 & data$alpha==1.5 & data$sight==5
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==6 & data$alpha==1.5 & data$sight==5
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==7 & data$alpha==1.5 & data$sight==5
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==8 & data$alpha==1.5 & data$sight==5
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==9 & data$alpha==1.5 & data$sight==5
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==10 & data$alpha==1.5 & data$sight==5
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==11 & data$alpha==1.5 & data$sight==5
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==12 & data$alpha==1.5 & data$sight==5
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==13 & data$alpha==1.5 & data$sight==5
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==14 & data$alpha==1.5 & data$sight==5
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==15 & data$alpha==1.5 & data$sight==5
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==16 & data$alpha==1.5 & data$sight==5
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==17 & data$alpha==1.5 & data$sight==5
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==18 & data$alpha==1.5 & data$sight==5
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==19 & data$alpha==1.5 & data$sight==5
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==20 & data$alpha==1.5 & data$sight==5
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==21 & data$alpha==1.5 & data$sight==5
                                             & data$sightangle==data$sightangle[11]]))
  
  averages2_5_360 <- c(mean(response[data$map==1 & data$alpha==2.0 & data$sight==5
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==2 & data$alpha==2.0 & data$sight==5
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==3 & data$alpha==2.0 & data$sight==5
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==4 & data$alpha==2.0 & data$sight==5
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==5 & data$alpha==2.0 & data$sight==5
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==6 & data$alpha==2.0 & data$sight==5
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==7 & data$alpha==2.0 & data$sight==5
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==8 & data$alpha==2.0 & data$sight==5
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==9 & data$alpha==2.0 & data$sight==5
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==10 & data$alpha==2.0 & data$sight==5
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==11 & data$alpha==2.0 & data$sight==5
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==12 & data$alpha==2.0 & data$sight==5
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==13 & data$alpha==2.0 & data$sight==5
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==14 & data$alpha==2.0 & data$sight==5
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==15 & data$alpha==2.0 & data$sight==5
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==16 & data$alpha==2.0 & data$sight==5
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==17 & data$alpha==2.0 & data$sight==5
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==18 & data$alpha==2.0 & data$sight==5
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==19 & data$alpha==2.0 & data$sight==5
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==20 & data$alpha==2.0 & data$sight==5
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==21 & data$alpha==2.0 & data$sight==5
                                           & data$sightangle==data$sightangle[11]]))
  
  averages2.5_5_360 <- c(mean(response[data$map==1 & data$alpha==2.5 & data$sight==5
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==2 & data$alpha==2.5 & data$sight==5
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==3 & data$alpha==2.5 & data$sight==5
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==4 & data$alpha==2.5 & data$sight==5
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==5 & data$alpha==2.5 & data$sight==5
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==6 & data$alpha==2.5 & data$sight==5
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==7 & data$alpha==2.5 & data$sight==5
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==8 & data$alpha==2.5 & data$sight==5
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==9 & data$alpha==2.5 & data$sight==5
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==10 & data$alpha==2.5 & data$sight==5
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==11 & data$alpha==2.5 & data$sight==5
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==12 & data$alpha==2.5 & data$sight==5
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==13 & data$alpha==2.5 & data$sight==5
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==14 & data$alpha==2.5 & data$sight==5
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==15 & data$alpha==2.5 & data$sight==5
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==16 & data$alpha==2.5 & data$sight==5
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==17 & data$alpha==2.5 & data$sight==5
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==18 & data$alpha==2.5 & data$sight==5
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==19 & data$alpha==2.5 & data$sight==5
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==20 & data$alpha==2.5 & data$sight==5
                                             & data$sightangle==data$sightangle[11]]),
                         mean(response[data$map==21 & data$alpha==2.5 & data$sight==5
                                             & data$sightangle==data$sightangle[11]]))
  
  averages3_5_360 <- c(mean(response[data$map==1 & data$alpha==3.0 & data$sight==5
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==2 & data$alpha==3.0 & data$sight==5
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==3 & data$alpha==3.0 & data$sight==5
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==4 & data$alpha==3.0 & data$sight==5
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==5 & data$alpha==3.0 & data$sight==5
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==6 & data$alpha==3.0 & data$sight==5
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==7 & data$alpha==3.0 & data$sight==5
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==8 & data$alpha==3.0 & data$sight==5
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==9 & data$alpha==3.0 & data$sight==5
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==10 & data$alpha==3.0 & data$sight==5
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==11 & data$alpha==3.0 & data$sight==5
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==12 & data$alpha==3.0 & data$sight==5
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==13 & data$alpha==3.0 & data$sight==5
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==14 & data$alpha==3.0 & data$sight==5
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==15 & data$alpha==3.0 & data$sight==5
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==16 & data$alpha==3.0 & data$sight==5
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==17 & data$alpha==3.0 & data$sight==5
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==18 & data$alpha==3.0 & data$sight==5
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==19 & data$alpha==3.0 & data$sight==5
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==20 & data$alpha==3.0 & data$sight==5
                                           & data$sightangle==data$sightangle[11]]),
                       mean(response[data$map==21 & data$alpha==3.0 & data$sight==5
                                           & data$sightangle==data$sightangle[11]]))
  
  averages1_10_360 <- c(mean(response[data$map==1 & data$alpha==1.0 & data$sight==10
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==2 & data$alpha==1.0 & data$sight==10
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==3 & data$alpha==1.0 & data$sight==10
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==4 & data$alpha==1.0 & data$sight==10
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==5 & data$alpha==1.0 & data$sight==10
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==6 & data$alpha==1.0 & data$sight==10
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==7 & data$alpha==1.0 & data$sight==10
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==8 & data$alpha==1.0 & data$sight==10
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==9 & data$alpha==1.0 & data$sight==10
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==10 & data$alpha==1.0 & data$sight==10
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==11 & data$alpha==1.0 & data$sight==10
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==12 & data$alpha==1.0 & data$sight==10
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==13 & data$alpha==1.0 & data$sight==10
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==14 & data$alpha==1.0 & data$sight==10
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==15 & data$alpha==1.0 & data$sight==10
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==16 & data$alpha==1.0 & data$sight==10
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==17 & data$alpha==1.0 & data$sight==10
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==18 & data$alpha==1.0 & data$sight==10
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==19 & data$alpha==1.0 & data$sight==10
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==20 & data$alpha==1.0 & data$sight==10
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==21 & data$alpha==1.0 & data$sight==10
                                            & data$sightangle==data$sightangle[11]]))
  
  averages1.5_10_360 <- c(mean(response[data$map==1 & data$alpha==1.5 & data$sight==10
                                              & data$sightangle==data$sightangle[11]]),
                          mean(response[data$map==2 & data$alpha==1.5 & data$sight==10
                                              & data$sightangle==data$sightangle[11]]),
                          mean(response[data$map==3 & data$alpha==1.5 & data$sight==10
                                              & data$sightangle==data$sightangle[11]]),
                          mean(response[data$map==4 & data$alpha==1.5 & data$sight==10
                                              & data$sightangle==data$sightangle[11]]),
                          mean(response[data$map==5 & data$alpha==1.5 & data$sight==10
                                              & data$sightangle==data$sightangle[11]]),
                          mean(response[data$map==6 & data$alpha==1.5 & data$sight==10
                                              & data$sightangle==data$sightangle[11]]),
                          mean(response[data$map==7 & data$alpha==1.5 & data$sight==10
                                              & data$sightangle==data$sightangle[11]]),
                          mean(response[data$map==8 & data$alpha==1.5 & data$sight==10
                                              & data$sightangle==data$sightangle[11]]),
                          mean(response[data$map==9 & data$alpha==1.5 & data$sight==10
                                              & data$sightangle==data$sightangle[11]]),
                          mean(response[data$map==10 & data$alpha==1.5 & data$sight==10
                                              & data$sightangle==data$sightangle[11]]),
                          mean(response[data$map==11 & data$alpha==1.5 & data$sight==10
                                              & data$sightangle==data$sightangle[11]]),
                          mean(response[data$map==12 & data$alpha==1.5 & data$sight==10
                                              & data$sightangle==data$sightangle[11]]),
                          mean(response[data$map==13 & data$alpha==1.5 & data$sight==10
                                              & data$sightangle==data$sightangle[11]]),
                          mean(response[data$map==14 & data$alpha==1.5 & data$sight==10
                                              & data$sightangle==data$sightangle[11]]),
                          mean(response[data$map==15 & data$alpha==1.5 & data$sight==10
                                              & data$sightangle==data$sightangle[11]]),
                          mean(response[data$map==16 & data$alpha==1.5 & data$sight==10
                                              & data$sightangle==data$sightangle[11]]),
                          mean(response[data$map==17 & data$alpha==1.5 & data$sight==10
                                              & data$sightangle==data$sightangle[11]]),
                          mean(response[data$map==18 & data$alpha==1.5 & data$sight==10
                                              & data$sightangle==data$sightangle[11]]),
                          mean(response[data$map==19 & data$alpha==1.5 & data$sight==10
                                              & data$sightangle==data$sightangle[11]]),
                          mean(response[data$map==20 & data$alpha==1.5 & data$sight==10
                                              & data$sightangle==data$sightangle[11]]),
                          mean(response[data$map==21 & data$alpha==1.5 & data$sight==10
                                              & data$sightangle==data$sightangle[11]]))
  
  averages2_10_360 <- c(mean(response[data$map==1 & data$alpha==2.0 & data$sight==10
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==2 & data$alpha==2.0 & data$sight==10
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==3 & data$alpha==2.0 & data$sight==10
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==4 & data$alpha==2.0 & data$sight==10
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==5 & data$alpha==2.0 & data$sight==10
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==6 & data$alpha==2.0 & data$sight==10
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==7 & data$alpha==2.0 & data$sight==10
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==8 & data$alpha==2.0 & data$sight==10
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==9 & data$alpha==2.0 & data$sight==10
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==10 & data$alpha==2.0 & data$sight==10
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==11 & data$alpha==2.0 & data$sight==10
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==12 & data$alpha==2.0 & data$sight==10
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==13 & data$alpha==2.0 & data$sight==10
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==14 & data$alpha==2.0 & data$sight==10
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==15 & data$alpha==2.0 & data$sight==10
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==16 & data$alpha==2.0 & data$sight==10
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==17 & data$alpha==2.0 & data$sight==10
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==18 & data$alpha==2.0 & data$sight==10
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==19 & data$alpha==2.0 & data$sight==10
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==20 & data$alpha==2.0 & data$sight==10
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==21 & data$alpha==2.0 & data$sight==10
                                            & data$sightangle==data$sightangle[11]]))
  
  averages2.5_10_360 <- c(mean(response[data$map==1 & data$alpha==2.5 & data$sight==10
                                              & data$sightangle==data$sightangle[11]]),
                          mean(response[data$map==2 & data$alpha==2.5 & data$sight==10
                                              & data$sightangle==data$sightangle[11]]),
                          mean(response[data$map==3 & data$alpha==2.5 & data$sight==10
                                              & data$sightangle==data$sightangle[11]]),
                          mean(response[data$map==4 & data$alpha==2.5 & data$sight==10
                                              & data$sightangle==data$sightangle[11]]),
                          mean(response[data$map==5 & data$alpha==2.5 & data$sight==10
                                              & data$sightangle==data$sightangle[11]]),
                          mean(response[data$map==6 & data$alpha==2.5 & data$sight==10
                                              & data$sightangle==data$sightangle[11]]),
                          mean(response[data$map==7 & data$alpha==2.5 & data$sight==10
                                              & data$sightangle==data$sightangle[11]]),
                          mean(response[data$map==8 & data$alpha==2.5 & data$sight==10
                                              & data$sightangle==data$sightangle[11]]),
                          mean(response[data$map==9 & data$alpha==2.5 & data$sight==10
                                              & data$sightangle==data$sightangle[11]]),
                          mean(response[data$map==10 & data$alpha==2.5 & data$sight==10
                                              & data$sightangle==data$sightangle[11]]),
                          mean(response[data$map==11 & data$alpha==2.5 & data$sight==10
                                              & data$sightangle==data$sightangle[11]]),
                          mean(response[data$map==12 & data$alpha==2.5 & data$sight==10
                                              & data$sightangle==data$sightangle[11]]),
                          mean(response[data$map==13 & data$alpha==2.5 & data$sight==10
                                              & data$sightangle==data$sightangle[11]]),
                          mean(response[data$map==14 & data$alpha==2.5 & data$sight==10
                                              & data$sightangle==data$sightangle[11]]),
                          mean(response[data$map==15 & data$alpha==2.5 & data$sight==10
                                              & data$sightangle==data$sightangle[11]]),
                          mean(response[data$map==16 & data$alpha==2.5 & data$sight==10
                                              & data$sightangle==data$sightangle[11]]),
                          mean(response[data$map==17 & data$alpha==2.5 & data$sight==10
                                              & data$sightangle==data$sightangle[11]]),
                          mean(response[data$map==18 & data$alpha==2.5 & data$sight==10
                                              & data$sightangle==data$sightangle[11]]),
                          mean(response[data$map==19 & data$alpha==2.5 & data$sight==10
                                              & data$sightangle==data$sightangle[11]]),
                          mean(response[data$map==20 & data$alpha==2.5 & data$sight==10
                                              & data$sightangle==data$sightangle[11]]),
                          mean(response[data$map==21 & data$alpha==2.5 & data$sight==10
                                              & data$sightangle==data$sightangle[11]]))
  
  averages3_10_360 <- c(mean(response[data$map==1 & data$alpha==3.0 & data$sight==10
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==2 & data$alpha==3.0 & data$sight==10
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==3 & data$alpha==3.0 & data$sight==10
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==4 & data$alpha==3.0 & data$sight==10
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==5 & data$alpha==3.0 & data$sight==10
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==6 & data$alpha==3.0 & data$sight==10
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==7 & data$alpha==3.0 & data$sight==10
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==8 & data$alpha==3.0 & data$sight==10
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==9 & data$alpha==3.0 & data$sight==10
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==10 & data$alpha==3.0 & data$sight==10
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==11 & data$alpha==3.0 & data$sight==10
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==12 & data$alpha==3.0 & data$sight==10
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==13 & data$alpha==3.0 & data$sight==10
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==14 & data$alpha==3.0 & data$sight==10
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==15 & data$alpha==3.0 & data$sight==10
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==16 & data$alpha==3.0 & data$sight==10
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==17 & data$alpha==3.0 & data$sight==10
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==18 & data$alpha==3.0 & data$sight==10
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==19 & data$alpha==3.0 & data$sight==10
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==20 & data$alpha==3.0 & data$sight==10
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==21 & data$alpha==3.0 & data$sight==10
                                            & data$sightangle==data$sightangle[11]]))
  
  averages1_20_360 <- c(mean(response[data$map==1 & data$alpha==1.0 & data$sight==20
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==2 & data$alpha==1.0 & data$sight==20
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==3 & data$alpha==1.0 & data$sight==20
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==4 & data$alpha==1.0 & data$sight==20
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==5 & data$alpha==1.0 & data$sight==20
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==6 & data$alpha==1.0 & data$sight==20
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==7 & data$alpha==1.0 & data$sight==20
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==8 & data$alpha==1.0 & data$sight==20
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==9 & data$alpha==1.0 & data$sight==20
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==10 & data$alpha==1.0 & data$sight==20
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==11 & data$alpha==1.0 & data$sight==20
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==12 & data$alpha==1.0 & data$sight==20
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==13 & data$alpha==1.0 & data$sight==20
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==14 & data$alpha==1.0 & data$sight==20
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==15 & data$alpha==1.0 & data$sight==20
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==16 & data$alpha==1.0 & data$sight==20
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==17 & data$alpha==1.0 & data$sight==20
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==18 & data$alpha==1.0 & data$sight==20
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==19 & data$alpha==1.0 & data$sight==20
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==20 & data$alpha==1.0 & data$sight==20
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==21 & data$alpha==1.0 & data$sight==20
                                            & data$sightangle==data$sightangle[11]]))
  
  averages1.5_20_360 <- c(mean(response[data$map==1 & data$alpha==1.5 & data$sight==20
                                              & data$sightangle==data$sightangle[11]]),
                          mean(response[data$map==2 & data$alpha==1.5 & data$sight==20
                                              & data$sightangle==data$sightangle[11]]),
                          mean(response[data$map==3 & data$alpha==1.5 & data$sight==20
                                              & data$sightangle==data$sightangle[11]]),
                          mean(response[data$map==4 & data$alpha==1.5 & data$sight==20
                                              & data$sightangle==data$sightangle[11]]),
                          mean(response[data$map==5 & data$alpha==1.5 & data$sight==20
                                              & data$sightangle==data$sightangle[11]]),
                          mean(response[data$map==6 & data$alpha==1.5 & data$sight==20
                                              & data$sightangle==data$sightangle[11]]),
                          mean(response[data$map==7 & data$alpha==1.5 & data$sight==20
                                              & data$sightangle==data$sightangle[11]]),
                          mean(response[data$map==8 & data$alpha==1.5 & data$sight==20
                                              & data$sightangle==data$sightangle[11]]),
                          mean(response[data$map==9 & data$alpha==1.5 & data$sight==20
                                              & data$sightangle==data$sightangle[11]]),
                          mean(response[data$map==10 & data$alpha==1.5 & data$sight==20
                                              & data$sightangle==data$sightangle[11]]),
                          mean(response[data$map==11 & data$alpha==1.5 & data$sight==20
                                              & data$sightangle==data$sightangle[11]]),
                          mean(response[data$map==12 & data$alpha==1.5 & data$sight==20
                                              & data$sightangle==data$sightangle[11]]),
                          mean(response[data$map==13 & data$alpha==1.5 & data$sight==20
                                              & data$sightangle==data$sightangle[11]]),
                          mean(response[data$map==14 & data$alpha==1.5 & data$sight==20
                                              & data$sightangle==data$sightangle[11]]),
                          mean(response[data$map==15 & data$alpha==1.5 & data$sight==20
                                              & data$sightangle==data$sightangle[11]]),
                          mean(response[data$map==16 & data$alpha==1.5 & data$sight==20
                                              & data$sightangle==data$sightangle[11]]),
                          mean(response[data$map==17 & data$alpha==1.5 & data$sight==20
                                              & data$sightangle==data$sightangle[11]]),
                          mean(response[data$map==18 & data$alpha==1.5 & data$sight==20
                                              & data$sightangle==data$sightangle[11]]),
                          mean(response[data$map==19 & data$alpha==1.5 & data$sight==20
                                              & data$sightangle==data$sightangle[11]]),
                          mean(response[data$map==20 & data$alpha==1.5 & data$sight==20
                                              & data$sightangle==data$sightangle[11]]),
                          mean(response[data$map==21 & data$alpha==1.5 & data$sight==20
                                              & data$sightangle==data$sightangle[11]]))
  
  averages2_20_360 <- c(mean(response[data$map==1 & data$alpha==2.0 & data$sight==20
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==2 & data$alpha==2.0 & data$sight==20
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==3 & data$alpha==2.0 & data$sight==20
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==4 & data$alpha==2.0 & data$sight==20
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==5 & data$alpha==2.0 & data$sight==20
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==6 & data$alpha==2.0 & data$sight==20
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==7 & data$alpha==2.0 & data$sight==20
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==8 & data$alpha==2.0 & data$sight==20
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==9 & data$alpha==2.0 & data$sight==20
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==10 & data$alpha==2.0 & data$sight==20
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==11 & data$alpha==2.0 & data$sight==20
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==12 & data$alpha==2.0 & data$sight==20
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==13 & data$alpha==2.0 & data$sight==20
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==14 & data$alpha==2.0 & data$sight==20
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==15 & data$alpha==2.0 & data$sight==20
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==16 & data$alpha==2.0 & data$sight==20
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==17 & data$alpha==2.0 & data$sight==20
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==18 & data$alpha==2.0 & data$sight==20
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==19 & data$alpha==2.0 & data$sight==20
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==20 & data$alpha==2.0 & data$sight==20
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==21 & data$alpha==2.0 & data$sight==20
                                            & data$sightangle==data$sightangle[11]]))
  
  averages2.5_20_360 <- c(mean(response[data$map==1 & data$alpha==2.5 & data$sight==20
                                              & data$sightangle==data$sightangle[11]]),
                          mean(response[data$map==2 & data$alpha==2.5 & data$sight==20
                                              & data$sightangle==data$sightangle[11]]),
                          mean(response[data$map==3 & data$alpha==2.5 & data$sight==20
                                              & data$sightangle==data$sightangle[11]]),
                          mean(response[data$map==4 & data$alpha==2.5 & data$sight==20
                                              & data$sightangle==data$sightangle[11]]),
                          mean(response[data$map==5 & data$alpha==2.5 & data$sight==20
                                              & data$sightangle==data$sightangle[11]]),
                          mean(response[data$map==6 & data$alpha==2.5 & data$sight==20
                                              & data$sightangle==data$sightangle[11]]),
                          mean(response[data$map==7 & data$alpha==2.5 & data$sight==20
                                              & data$sightangle==data$sightangle[11]]),
                          mean(response[data$map==8 & data$alpha==2.5 & data$sight==20
                                              & data$sightangle==data$sightangle[11]]),
                          mean(response[data$map==9 & data$alpha==2.5 & data$sight==20
                                              & data$sightangle==data$sightangle[11]]),
                          mean(response[data$map==10 & data$alpha==2.5 & data$sight==20
                                              & data$sightangle==data$sightangle[11]]),
                          mean(response[data$map==11 & data$alpha==2.5 & data$sight==20
                                              & data$sightangle==data$sightangle[11]]),
                          mean(response[data$map==12 & data$alpha==2.5 & data$sight==20
                                              & data$sightangle==data$sightangle[11]]),
                          mean(response[data$map==13 & data$alpha==2.5 & data$sight==20
                                              & data$sightangle==data$sightangle[11]]),
                          mean(response[data$map==14 & data$alpha==2.5 & data$sight==20
                                              & data$sightangle==data$sightangle[11]]),
                          mean(response[data$map==15 & data$alpha==2.5 & data$sight==20
                                              & data$sightangle==data$sightangle[11]]),
                          mean(response[data$map==16 & data$alpha==2.5 & data$sight==20
                                              & data$sightangle==data$sightangle[11]]),
                          mean(response[data$map==17 & data$alpha==2.5 & data$sight==20
                                              & data$sightangle==data$sightangle[11]]),
                          mean(response[data$map==18 & data$alpha==2.5 & data$sight==20
                                              & data$sightangle==data$sightangle[11]]),
                          mean(response[data$map==19 & data$alpha==2.5 & data$sight==20
                                              & data$sightangle==data$sightangle[11]]),
                          mean(response[data$map==20 & data$alpha==2.5 & data$sight==20
                                              & data$sightangle==data$sightangle[11]]),
                          mean(response[data$map==21 & data$alpha==2.5 & data$sight==20
                                              & data$sightangle==data$sightangle[11]]))
  
  averages3_20_360 <- c(mean(response[data$map==1 & data$alpha==3.0 & data$sight==20
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==2 & data$alpha==3.0 & data$sight==20
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==3 & data$alpha==3.0 & data$sight==20
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==4 & data$alpha==3.0 & data$sight==20
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==5 & data$alpha==3.0 & data$sight==20
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==6 & data$alpha==3.0 & data$sight==20
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==7 & data$alpha==3.0 & data$sight==20
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==8 & data$alpha==3.0 & data$sight==20
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==9 & data$alpha==3.0 & data$sight==20
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==10 & data$alpha==3.0 & data$sight==20
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==11 & data$alpha==3.0 & data$sight==20
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==12 & data$alpha==3.0 & data$sight==20
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==13 & data$alpha==3.0 & data$sight==20
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==14 & data$alpha==3.0 & data$sight==20
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==15 & data$alpha==3.0 & data$sight==20
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==16 & data$alpha==3.0 & data$sight==20
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==17 & data$alpha==3.0 & data$sight==20
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==18 & data$alpha==3.0 & data$sight==20
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==19 & data$alpha==3.0 & data$sight==20
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==20 & data$alpha==3.0 & data$sight==20
                                            & data$sightangle==data$sightangle[11]]),
                        mean(response[data$map==21 & data$alpha==3.0 & data$sight==20
                                            & data$sightangle==data$sightangle[11]]))
  
  # Calculate standard deviations:
  
  stds1_0_60 <- c(sd(response[data$map==1 & data$alpha==1.0 & data$sight==0
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==2 & data$alpha==1.0 & data$sight==0
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==3 & data$alpha==1.0 & data$sight==0
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==4 & data$alpha==1.0 & data$sight==0
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==5 & data$alpha==1.0 & data$sight==0
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==6 & data$alpha==1.0 & data$sight==0
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==7 & data$alpha==1.0 & data$sight==0
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==8 & data$alpha==1.0 & data$sight==0
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==9 & data$alpha==1.0 & data$sight==0
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==10 & data$alpha==1.0 & data$sight==0
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==11 & data$alpha==1.0 & data$sight==0
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==12 & data$alpha==1.0 & data$sight==0
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==13 & data$alpha==1.0 & data$sight==0
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==14 & data$alpha==1.0 & data$sight==0
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==15 & data$alpha==1.0 & data$sight==0
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==16 & data$alpha==1.0 & data$sight==0
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==17 & data$alpha==1.0 & data$sight==0
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==18 & data$alpha==1.0 & data$sight==0
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==19 & data$alpha==1.0 & data$sight==0
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==20 & data$alpha==1.0 & data$sight==0
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==21 & data$alpha==1.0 & data$sight==0
                                    & data$sightangle==data$sightangle[1]]))
  
  stds1.5_0_60 <- c(sd(response[data$map==1 & data$alpha==1.5 & data$sight==0
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==2 & data$alpha==1.5 & data$sight==0
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==3 & data$alpha==1.5 & data$sight==0
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==4 & data$alpha==1.5 & data$sight==0
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==5 & data$alpha==1.5 & data$sight==0
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==6 & data$alpha==1.5 & data$sight==0
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==7 & data$alpha==1.5 & data$sight==0
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==8 & data$alpha==1.5 & data$sight==0
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==9 & data$alpha==1.5 & data$sight==0
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==10 & data$alpha==1.5 & data$sight==0
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==11 & data$alpha==1.5 & data$sight==0
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==12 & data$alpha==1.5 & data$sight==0
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==13 & data$alpha==1.5 & data$sight==0
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==14 & data$alpha==1.5 & data$sight==0
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==15 & data$alpha==1.5 & data$sight==0
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==16 & data$alpha==1.5 & data$sight==0
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==17 & data$alpha==1.5 & data$sight==0
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==18 & data$alpha==1.5 & data$sight==0
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==19 & data$alpha==1.5 & data$sight==0
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==20 & data$alpha==1.5 & data$sight==0
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==21 & data$alpha==1.5 & data$sight==0
                                      & data$sightangle==data$sightangle[1]]))
  
  stds2_0_60 <- c(sd(response[data$map==1 & data$alpha==2.0 & data$sight==0
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==2 & data$alpha==2.0 & data$sight==0
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==3 & data$alpha==2.0 & data$sight==0
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==4 & data$alpha==2.0 & data$sight==0
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==5 & data$alpha==2.0 & data$sight==0
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==6 & data$alpha==2.0 & data$sight==0
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==7 & data$alpha==2.0 & data$sight==0
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==8 & data$alpha==2.0 & data$sight==0
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==9 & data$alpha==2.0 & data$sight==0
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==10 & data$alpha==2.0 & data$sight==0
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==11 & data$alpha==2.0 & data$sight==0
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==12 & data$alpha==2.0 & data$sight==0
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==13 & data$alpha==2.0 & data$sight==0
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==14 & data$alpha==2.0 & data$sight==0
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==15 & data$alpha==2.0 & data$sight==0
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==16 & data$alpha==2.0 & data$sight==0
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==17 & data$alpha==2.0 & data$sight==0
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==18 & data$alpha==2.0 & data$sight==0
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==19 & data$alpha==2.0 & data$sight==0
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==20 & data$alpha==2.0 & data$sight==0
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==21 & data$alpha==2.0 & data$sight==0
                                    & data$sightangle==data$sightangle[1]]))
  
  stds2.5_0_60 <- c(sd(response[data$map==1 & data$alpha==2.5 & data$sight==0
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==2 & data$alpha==2.5 & data$sight==0
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==3 & data$alpha==2.5 & data$sight==0
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==4 & data$alpha==2.5 & data$sight==0
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==5 & data$alpha==2.5 & data$sight==0
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==6 & data$alpha==2.5 & data$sight==0
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==7 & data$alpha==2.5 & data$sight==0
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==8 & data$alpha==2.5 & data$sight==0
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==9 & data$alpha==2.5 & data$sight==0
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==10 & data$alpha==2.5 & data$sight==0
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==11 & data$alpha==2.5 & data$sight==0
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==12 & data$alpha==2.5 & data$sight==0
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==13 & data$alpha==2.5 & data$sight==0
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==14 & data$alpha==2.5 & data$sight==0
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==15 & data$alpha==2.5 & data$sight==0
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==16 & data$alpha==2.5 & data$sight==0
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==17 & data$alpha==2.5 & data$sight==0
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==18 & data$alpha==2.5 & data$sight==0
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==19 & data$alpha==2.5 & data$sight==0
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==20 & data$alpha==2.5 & data$sight==0
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==21 & data$alpha==2.5 & data$sight==0
                                      & data$sightangle==data$sightangle[1]]))
  
  stds3_0_60 <- c(sd(response[data$map==1 & data$alpha==3.0 & data$sight==0
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==2 & data$alpha==3.0 & data$sight==0
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==3 & data$alpha==3.0 & data$sight==0
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==4 & data$alpha==3.0 & data$sight==0
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==5 & data$alpha==3.0 & data$sight==0
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==6 & data$alpha==3.0 & data$sight==0
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==7 & data$alpha==3.0 & data$sight==0
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==8 & data$alpha==3.0 & data$sight==0
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==9 & data$alpha==3.0 & data$sight==0
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==10 & data$alpha==3.0 & data$sight==0
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==11 & data$alpha==3.0 & data$sight==0
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==12 & data$alpha==3.0 & data$sight==0
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==13 & data$alpha==3.0 & data$sight==0
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==14 & data$alpha==3.0 & data$sight==0
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==15 & data$alpha==3.0 & data$sight==0
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==16 & data$alpha==3.0 & data$sight==0
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==17 & data$alpha==3.0 & data$sight==0
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==18 & data$alpha==3.0 & data$sight==0
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==19 & data$alpha==3.0 & data$sight==0
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==20 & data$alpha==3.0 & data$sight==0
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==21 & data$alpha==3.0 & data$sight==0
                                    & data$sightangle==data$sightangle[1]]))
  
  stds1_1_60 <- c(sd(response[data$map==1 & data$alpha==1.0 & data$sight==1
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==2 & data$alpha==1.0 & data$sight==1
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==3 & data$alpha==1.0 & data$sight==1
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==4 & data$alpha==1.0 & data$sight==1
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==5 & data$alpha==1.0 & data$sight==1
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==6 & data$alpha==1.0 & data$sight==1
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==7 & data$alpha==1.0 & data$sight==1
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==8 & data$alpha==1.0 & data$sight==1
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==9 & data$alpha==1.0 & data$sight==1
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==10 & data$alpha==1.0 & data$sight==1
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==11 & data$alpha==1.0 & data$sight==1
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==12 & data$alpha==1.0 & data$sight==1
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==13 & data$alpha==1.0 & data$sight==1
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==14 & data$alpha==1.0 & data$sight==1
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==15 & data$alpha==1.0 & data$sight==1
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==16 & data$alpha==1.0 & data$sight==1
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==17 & data$alpha==1.0 & data$sight==1
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==18 & data$alpha==1.0 & data$sight==1
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==19 & data$alpha==1.0 & data$sight==1
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==20 & data$alpha==1.0 & data$sight==1
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==21 & data$alpha==1.0 & data$sight==1
                                    & data$sightangle==data$sightangle[1]]))
  
  stds1.5_1_60 <- c(sd(response[data$map==1 & data$alpha==1.5 & data$sight==1
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==2 & data$alpha==1.5 & data$sight==1
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==3 & data$alpha==1.5 & data$sight==1
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==4 & data$alpha==1.5 & data$sight==1
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==5 & data$alpha==1.5 & data$sight==1
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==6 & data$alpha==1.5 & data$sight==1
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==7 & data$alpha==1.5 & data$sight==1
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==8 & data$alpha==1.5 & data$sight==1
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==9 & data$alpha==1.5 & data$sight==1
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==10 & data$alpha==1.5 & data$sight==1
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==11 & data$alpha==1.5 & data$sight==1
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==12 & data$alpha==1.5 & data$sight==1
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==13 & data$alpha==1.5 & data$sight==1
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==14 & data$alpha==1.5 & data$sight==1
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==15 & data$alpha==1.5 & data$sight==1
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==16 & data$alpha==1.5 & data$sight==1
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==17 & data$alpha==1.5 & data$sight==1
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==18 & data$alpha==1.5 & data$sight==1
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==19 & data$alpha==1.5 & data$sight==1
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==20 & data$alpha==1.5 & data$sight==1
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==21 & data$alpha==1.5 & data$sight==1
                                      & data$sightangle==data$sightangle[1]]))
  
  stds2_1_60 <- c(sd(response[data$map==1 & data$alpha==2.0 & data$sight==1
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==2 & data$alpha==2.0 & data$sight==1
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==3 & data$alpha==2.0 & data$sight==1
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==4 & data$alpha==2.0 & data$sight==1
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==5 & data$alpha==2.0 & data$sight==1
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==6 & data$alpha==2.0 & data$sight==1
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==7 & data$alpha==2.0 & data$sight==1
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==8 & data$alpha==2.0 & data$sight==1
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==9 & data$alpha==2.0 & data$sight==1
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==10 & data$alpha==2.0 & data$sight==1
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==11 & data$alpha==2.0 & data$sight==1
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==12 & data$alpha==2.0 & data$sight==1
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==13 & data$alpha==2.0 & data$sight==1
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==14 & data$alpha==2.0 & data$sight==1
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==15 & data$alpha==2.0 & data$sight==1
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==16 & data$alpha==2.0 & data$sight==1
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==17 & data$alpha==2.0 & data$sight==1
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==18 & data$alpha==2.0 & data$sight==1
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==19 & data$alpha==2.0 & data$sight==1
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==20 & data$alpha==2.0 & data$sight==1
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==21 & data$alpha==2.0 & data$sight==1
                                    & data$sightangle==data$sightangle[1]]))
  
  stds2.5_1_60 <- c(sd(response[data$map==1 & data$alpha==2.5 & data$sight==1
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==2 & data$alpha==2.5 & data$sight==1
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==3 & data$alpha==2.5 & data$sight==1
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==4 & data$alpha==2.5 & data$sight==1
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==5 & data$alpha==2.5 & data$sight==1
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==6 & data$alpha==2.5 & data$sight==1
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==7 & data$alpha==2.5 & data$sight==1
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==8 & data$alpha==2.5 & data$sight==1
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==9 & data$alpha==2.5 & data$sight==1
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==10 & data$alpha==2.5 & data$sight==1
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==11 & data$alpha==2.5 & data$sight==1
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==12 & data$alpha==2.5 & data$sight==1
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==13 & data$alpha==2.5 & data$sight==1
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==14 & data$alpha==2.5 & data$sight==1
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==15 & data$alpha==2.5 & data$sight==1
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==16 & data$alpha==2.5 & data$sight==1
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==17 & data$alpha==2.5 & data$sight==1
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==18 & data$alpha==2.5 & data$sight==1
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==19 & data$alpha==2.5 & data$sight==1
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==20 & data$alpha==2.5 & data$sight==1
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==21 & data$alpha==2.5 & data$sight==1
                                      & data$sightangle==data$sightangle[1]]))
  
  stds3_1_60 <- c(sd(response[data$map==1 & data$alpha==3.0 & data$sight==1
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==2 & data$alpha==3.0 & data$sight==1
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==3 & data$alpha==3.0 & data$sight==1
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==4 & data$alpha==3.0 & data$sight==1
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==5 & data$alpha==3.0 & data$sight==1
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==6 & data$alpha==3.0 & data$sight==1
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==7 & data$alpha==3.0 & data$sight==1
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==8 & data$alpha==3.0 & data$sight==1
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==9 & data$alpha==3.0 & data$sight==1
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==10 & data$alpha==3.0 & data$sight==1
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==11 & data$alpha==3.0 & data$sight==1
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==12 & data$alpha==3.0 & data$sight==1
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==13 & data$alpha==3.0 & data$sight==1
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==14 & data$alpha==3.0 & data$sight==1
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==15 & data$alpha==3.0 & data$sight==1
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==16 & data$alpha==3.0 & data$sight==1
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==17 & data$alpha==3.0 & data$sight==1
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==18 & data$alpha==3.0 & data$sight==1
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==19 & data$alpha==3.0 & data$sight==1
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==20 & data$alpha==3.0 & data$sight==1
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==21 & data$alpha==3.0 & data$sight==1
                                    & data$sightangle==data$sightangle[1]]))
  
  stds1_2_60 <- c(sd(response[data$map==1 & data$alpha==1.0 & data$sight==2
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==2 & data$alpha==1.0 & data$sight==2
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==3 & data$alpha==1.0 & data$sight==2
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==4 & data$alpha==1.0 & data$sight==2
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==5 & data$alpha==1.0 & data$sight==2
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==6 & data$alpha==1.0 & data$sight==2
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==7 & data$alpha==1.0 & data$sight==2
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==8 & data$alpha==1.0 & data$sight==2
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==9 & data$alpha==1.0 & data$sight==2
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==10 & data$alpha==1.0 & data$sight==2
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==11 & data$alpha==1.0 & data$sight==2
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==12 & data$alpha==1.0 & data$sight==2
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==13 & data$alpha==1.0 & data$sight==2
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==14 & data$alpha==1.0 & data$sight==2
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==15 & data$alpha==1.0 & data$sight==2
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==16 & data$alpha==1.0 & data$sight==2
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==17 & data$alpha==1.0 & data$sight==2
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==18 & data$alpha==1.0 & data$sight==2
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==19 & data$alpha==1.0 & data$sight==2
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==20 & data$alpha==1.0 & data$sight==2
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==21 & data$alpha==1.0 & data$sight==2
                                    & data$sightangle==data$sightangle[1]]))
  
  stds1.5_2_60 <- c(sd(response[data$map==1 & data$alpha==1.5 & data$sight==2
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==2 & data$alpha==1.5 & data$sight==2
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==3 & data$alpha==1.5 & data$sight==2
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==4 & data$alpha==1.5 & data$sight==2
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==5 & data$alpha==1.5 & data$sight==2
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==6 & data$alpha==1.5 & data$sight==2
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==7 & data$alpha==1.5 & data$sight==2
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==8 & data$alpha==1.5 & data$sight==2
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==9 & data$alpha==1.5 & data$sight==2
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==10 & data$alpha==1.5 & data$sight==2
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==11 & data$alpha==1.5 & data$sight==2
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==12 & data$alpha==1.5 & data$sight==2
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==13 & data$alpha==1.5 & data$sight==2
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==14 & data$alpha==1.5 & data$sight==2
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==15 & data$alpha==1.5 & data$sight==2
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==16 & data$alpha==1.5 & data$sight==2
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==17 & data$alpha==1.5 & data$sight==2
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==18 & data$alpha==1.5 & data$sight==2
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==19 & data$alpha==1.5 & data$sight==2
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==20 & data$alpha==1.5 & data$sight==2
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==21 & data$alpha==1.5 & data$sight==2
                                      & data$sightangle==data$sightangle[1]]))
  
  stds2_2_60 <- c(sd(response[data$map==1 & data$alpha==2.0 & data$sight==2
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==2 & data$alpha==2.0 & data$sight==2
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==3 & data$alpha==2.0 & data$sight==2
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==4 & data$alpha==2.0 & data$sight==2
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==5 & data$alpha==2.0 & data$sight==2
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==6 & data$alpha==2.0 & data$sight==2
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==7 & data$alpha==2.0 & data$sight==2
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==8 & data$alpha==2.0 & data$sight==2
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==9 & data$alpha==2.0 & data$sight==2
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==10 & data$alpha==2.0 & data$sight==2
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==11 & data$alpha==2.0 & data$sight==2
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==12 & data$alpha==2.0 & data$sight==2
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==13 & data$alpha==2.0 & data$sight==2
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==14 & data$alpha==2.0 & data$sight==2
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==15 & data$alpha==2.0 & data$sight==2
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==16 & data$alpha==2.0 & data$sight==2
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==17 & data$alpha==2.0 & data$sight==2
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==18 & data$alpha==2.0 & data$sight==2
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==19 & data$alpha==2.0 & data$sight==2
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==20 & data$alpha==2.0 & data$sight==2
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==21 & data$alpha==2.0 & data$sight==2
                                    & data$sightangle==data$sightangle[1]]))
  
  stds2.5_2_60 <- c(sd(response[data$map==1 & data$alpha==2.5 & data$sight==2
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==2 & data$alpha==2.5 & data$sight==2
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==3 & data$alpha==2.5 & data$sight==2
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==4 & data$alpha==2.5 & data$sight==2
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==5 & data$alpha==2.5 & data$sight==2
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==6 & data$alpha==2.5 & data$sight==2
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==7 & data$alpha==2.5 & data$sight==2
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==8 & data$alpha==2.5 & data$sight==2
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==9 & data$alpha==2.5 & data$sight==2
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==10 & data$alpha==2.5 & data$sight==2
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==11 & data$alpha==2.5 & data$sight==2
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==12 & data$alpha==2.5 & data$sight==2
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==13 & data$alpha==2.5 & data$sight==2
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==14 & data$alpha==2.5 & data$sight==2
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==15 & data$alpha==2.5 & data$sight==2
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==16 & data$alpha==2.5 & data$sight==2
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==17 & data$alpha==2.5 & data$sight==2
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==18 & data$alpha==2.5 & data$sight==2
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==19 & data$alpha==2.5 & data$sight==2
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==20 & data$alpha==2.5 & data$sight==2
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==21 & data$alpha==2.5 & data$sight==2
                                      & data$sightangle==data$sightangle[1]]))
  
  stds3_2_60 <- c(sd(response[data$map==1 & data$alpha==3.0 & data$sight==2
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==2 & data$alpha==3.0 & data$sight==2
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==3 & data$alpha==3.0 & data$sight==2
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==4 & data$alpha==3.0 & data$sight==2
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==5 & data$alpha==3.0 & data$sight==2
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==6 & data$alpha==3.0 & data$sight==2
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==7 & data$alpha==3.0 & data$sight==2
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==8 & data$alpha==3.0 & data$sight==2
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==9 & data$alpha==3.0 & data$sight==2
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==10 & data$alpha==3.0 & data$sight==2
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==11 & data$alpha==3.0 & data$sight==2
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==12 & data$alpha==3.0 & data$sight==2
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==13 & data$alpha==3.0 & data$sight==2
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==14 & data$alpha==3.0 & data$sight==2
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==15 & data$alpha==3.0 & data$sight==2
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==16 & data$alpha==3.0 & data$sight==2
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==17 & data$alpha==3.0 & data$sight==2
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==18 & data$alpha==3.0 & data$sight==2
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==19 & data$alpha==3.0 & data$sight==2
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==20 & data$alpha==3.0 & data$sight==2
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==21 & data$alpha==3.0 & data$sight==2
                                    & data$sightangle==data$sightangle[1]]))
  
  stds1_5_60 <- c(sd(response[data$map==1 & data$alpha==1.0 & data$sight==5
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==2 & data$alpha==1.0 & data$sight==5
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==3 & data$alpha==1.0 & data$sight==5
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==4 & data$alpha==1.0 & data$sight==5
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==5 & data$alpha==1.0 & data$sight==5
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==6 & data$alpha==1.0 & data$sight==5
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==7 & data$alpha==1.0 & data$sight==5
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==8 & data$alpha==1.0 & data$sight==5
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==9 & data$alpha==1.0 & data$sight==5
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==10 & data$alpha==1.0 & data$sight==5
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==11 & data$alpha==1.0 & data$sight==5
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==12 & data$alpha==1.0 & data$sight==5
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==13 & data$alpha==1.0 & data$sight==5
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==14 & data$alpha==1.0 & data$sight==5
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==15 & data$alpha==1.0 & data$sight==5
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==16 & data$alpha==1.0 & data$sight==5
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==17 & data$alpha==1.0 & data$sight==5
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==18 & data$alpha==1.0 & data$sight==5
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==19 & data$alpha==1.0 & data$sight==5
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==20 & data$alpha==1.0 & data$sight==5
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==21 & data$alpha==1.0 & data$sight==5
                                    & data$sightangle==data$sightangle[1]]))
  
  stds1.5_5_60 <- c(sd(response[data$map==1 & data$alpha==1.5 & data$sight==5
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==2 & data$alpha==1.5 & data$sight==5
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==3 & data$alpha==1.5 & data$sight==5
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==4 & data$alpha==1.5 & data$sight==5
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==5 & data$alpha==1.5 & data$sight==5
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==6 & data$alpha==1.5 & data$sight==5
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==7 & data$alpha==1.5 & data$sight==5
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==8 & data$alpha==1.5 & data$sight==5
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==9 & data$alpha==1.5 & data$sight==5
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==10 & data$alpha==1.5 & data$sight==5
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==11 & data$alpha==1.5 & data$sight==5
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==12 & data$alpha==1.5 & data$sight==5
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==13 & data$alpha==1.5 & data$sight==5
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==14 & data$alpha==1.5 & data$sight==5
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==15 & data$alpha==1.5 & data$sight==5
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==16 & data$alpha==1.5 & data$sight==5
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==17 & data$alpha==1.5 & data$sight==5
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==18 & data$alpha==1.5 & data$sight==5
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==19 & data$alpha==1.5 & data$sight==5
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==20 & data$alpha==1.5 & data$sight==5
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==21 & data$alpha==1.5 & data$sight==5
                                      & data$sightangle==data$sightangle[1]]))
  
  stds2_5_60 <- c(sd(response[data$map==1 & data$alpha==2.0 & data$sight==5
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==2 & data$alpha==2.0 & data$sight==5
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==3 & data$alpha==2.0 & data$sight==5
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==4 & data$alpha==2.0 & data$sight==5
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==5 & data$alpha==2.0 & data$sight==5
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==6 & data$alpha==2.0 & data$sight==5
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==7 & data$alpha==2.0 & data$sight==5
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==8 & data$alpha==2.0 & data$sight==5
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==9 & data$alpha==2.0 & data$sight==5
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==10 & data$alpha==2.0 & data$sight==5
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==11 & data$alpha==2.0 & data$sight==5
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==12 & data$alpha==2.0 & data$sight==5
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==13 & data$alpha==2.0 & data$sight==5
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==14 & data$alpha==2.0 & data$sight==5
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==15 & data$alpha==2.0 & data$sight==5
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==16 & data$alpha==2.0 & data$sight==5
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==17 & data$alpha==2.0 & data$sight==5
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==18 & data$alpha==2.0 & data$sight==5
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==19 & data$alpha==2.0 & data$sight==5
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==20 & data$alpha==2.0 & data$sight==5
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==21 & data$alpha==2.0 & data$sight==5
                                    & data$sightangle==data$sightangle[1]]))
  
  stds2.5_5_60 <- c(sd(response[data$map==1 & data$alpha==2.5 & data$sight==5
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==2 & data$alpha==2.5 & data$sight==5
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==3 & data$alpha==2.5 & data$sight==5
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==4 & data$alpha==2.5 & data$sight==5
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==5 & data$alpha==2.5 & data$sight==5
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==6 & data$alpha==2.5 & data$sight==5
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==7 & data$alpha==2.5 & data$sight==5
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==8 & data$alpha==2.5 & data$sight==5
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==9 & data$alpha==2.5 & data$sight==5
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==10 & data$alpha==2.5 & data$sight==5
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==11 & data$alpha==2.5 & data$sight==5
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==12 & data$alpha==2.5 & data$sight==5
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==13 & data$alpha==2.5 & data$sight==5
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==14 & data$alpha==2.5 & data$sight==5
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==15 & data$alpha==2.5 & data$sight==5
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==16 & data$alpha==2.5 & data$sight==5
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==17 & data$alpha==2.5 & data$sight==5
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==18 & data$alpha==2.5 & data$sight==5
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==19 & data$alpha==2.5 & data$sight==5
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==20 & data$alpha==2.5 & data$sight==5
                                      & data$sightangle==data$sightangle[1]]),
                        sd(response[data$map==21 & data$alpha==2.5 & data$sight==5
                                      & data$sightangle==data$sightangle[1]]))
  
  stds3_5_60 <- c(sd(response[data$map==1 & data$alpha==3.0 & data$sight==5
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==2 & data$alpha==3.0 & data$sight==5
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==3 & data$alpha==3.0 & data$sight==5
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==4 & data$alpha==3.0 & data$sight==5
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==5 & data$alpha==3.0 & data$sight==5
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==6 & data$alpha==3.0 & data$sight==5
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==7 & data$alpha==3.0 & data$sight==5
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==8 & data$alpha==3.0 & data$sight==5
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==9 & data$alpha==3.0 & data$sight==5
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==10 & data$alpha==3.0 & data$sight==5
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==11 & data$alpha==3.0 & data$sight==5
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==12 & data$alpha==3.0 & data$sight==5
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==13 & data$alpha==3.0 & data$sight==5
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==14 & data$alpha==3.0 & data$sight==5
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==15 & data$alpha==3.0 & data$sight==5
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==16 & data$alpha==3.0 & data$sight==5
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==17 & data$alpha==3.0 & data$sight==5
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==18 & data$alpha==3.0 & data$sight==5
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==19 & data$alpha==3.0 & data$sight==5
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==20 & data$alpha==3.0 & data$sight==5
                                    & data$sightangle==data$sightangle[1]]),
                      sd(response[data$map==21 & data$alpha==3.0 & data$sight==5
                                    & data$sightangle==data$sightangle[1]]))
  
  stds1_10_60 <- c(sd(response[data$map==1 & data$alpha==1.0 & data$sight==10
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==2 & data$alpha==1.0 & data$sight==10
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==3 & data$alpha==1.0 & data$sight==10
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==4 & data$alpha==1.0 & data$sight==10
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==5 & data$alpha==1.0 & data$sight==10
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==6 & data$alpha==1.0 & data$sight==10
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==7 & data$alpha==1.0 & data$sight==10
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==8 & data$alpha==1.0 & data$sight==10
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==9 & data$alpha==1.0 & data$sight==10
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==10 & data$alpha==1.0 & data$sight==10
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==11 & data$alpha==1.0 & data$sight==10
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==12 & data$alpha==1.0 & data$sight==10
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==13 & data$alpha==1.0 & data$sight==10
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==14 & data$alpha==1.0 & data$sight==10
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==15 & data$alpha==1.0 & data$sight==10
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==16 & data$alpha==1.0 & data$sight==10
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==17 & data$alpha==1.0 & data$sight==10
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==18 & data$alpha==1.0 & data$sight==10
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==19 & data$alpha==1.0 & data$sight==10
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==20 & data$alpha==1.0 & data$sight==10
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==21 & data$alpha==1.0 & data$sight==10
                                     & data$sightangle==data$sightangle[1]]))
  
  stds1.5_10_60 <- c(sd(response[data$map==1 & data$alpha==1.5 & data$sight==10
                                       & data$sightangle==data$sightangle[1]]),
                         sd(response[data$map==2 & data$alpha==1.5 & data$sight==10
                                       & data$sightangle==data$sightangle[1]]),
                         sd(response[data$map==3 & data$alpha==1.5 & data$sight==10
                                       & data$sightangle==data$sightangle[1]]),
                         sd(response[data$map==4 & data$alpha==1.5 & data$sight==10
                                       & data$sightangle==data$sightangle[1]]),
                         sd(response[data$map==5 & data$alpha==1.5 & data$sight==10
                                       & data$sightangle==data$sightangle[1]]),
                         sd(response[data$map==6 & data$alpha==1.5 & data$sight==10
                                       & data$sightangle==data$sightangle[1]]),
                         sd(response[data$map==7 & data$alpha==1.5 & data$sight==10
                                       & data$sightangle==data$sightangle[1]]),
                         sd(response[data$map==8 & data$alpha==1.5 & data$sight==10
                                       & data$sightangle==data$sightangle[1]]),
                         sd(response[data$map==9 & data$alpha==1.5 & data$sight==10
                                       & data$sightangle==data$sightangle[1]]),
                         sd(response[data$map==10 & data$alpha==1.5 & data$sight==10
                                       & data$sightangle==data$sightangle[1]]),
                         sd(response[data$map==11 & data$alpha==1.5 & data$sight==10
                                       & data$sightangle==data$sightangle[1]]),
                         sd(response[data$map==12 & data$alpha==1.5 & data$sight==10
                                       & data$sightangle==data$sightangle[1]]),
                         sd(response[data$map==13 & data$alpha==1.5 & data$sight==10
                                       & data$sightangle==data$sightangle[1]]),
                         sd(response[data$map==14 & data$alpha==1.5 & data$sight==10
                                       & data$sightangle==data$sightangle[1]]),
                         sd(response[data$map==15 & data$alpha==1.5 & data$sight==10
                                       & data$sightangle==data$sightangle[1]]),
                         sd(response[data$map==16 & data$alpha==1.5 & data$sight==10
                                       & data$sightangle==data$sightangle[1]]),
                         sd(response[data$map==17 & data$alpha==1.5 & data$sight==10
                                       & data$sightangle==data$sightangle[1]]),
                         sd(response[data$map==18 & data$alpha==1.5 & data$sight==10
                                       & data$sightangle==data$sightangle[1]]),
                         sd(response[data$map==19 & data$alpha==1.5 & data$sight==10
                                       & data$sightangle==data$sightangle[1]]),
                         sd(response[data$map==20 & data$alpha==1.5 & data$sight==10
                                       & data$sightangle==data$sightangle[1]]),
                         sd(response[data$map==21 & data$alpha==1.5 & data$sight==10
                                       & data$sightangle==data$sightangle[1]]))
  
  stds2_10_60 <- c(sd(response[data$map==1 & data$alpha==2.0 & data$sight==10
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==2 & data$alpha==2.0 & data$sight==10
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==3 & data$alpha==2.0 & data$sight==10
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==4 & data$alpha==2.0 & data$sight==10
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==5 & data$alpha==2.0 & data$sight==10
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==6 & data$alpha==2.0 & data$sight==10
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==7 & data$alpha==2.0 & data$sight==10
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==8 & data$alpha==2.0 & data$sight==10
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==9 & data$alpha==2.0 & data$sight==10
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==10 & data$alpha==2.0 & data$sight==10
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==11 & data$alpha==2.0 & data$sight==10
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==12 & data$alpha==2.0 & data$sight==10
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==13 & data$alpha==2.0 & data$sight==10
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==14 & data$alpha==2.0 & data$sight==10
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==15 & data$alpha==2.0 & data$sight==10
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==16 & data$alpha==2.0 & data$sight==10
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==17 & data$alpha==2.0 & data$sight==10
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==18 & data$alpha==2.0 & data$sight==10
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==19 & data$alpha==2.0 & data$sight==10
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==20 & data$alpha==2.0 & data$sight==10
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==21 & data$alpha==2.0 & data$sight==10
                                     & data$sightangle==data$sightangle[1]]))
  
  stds2.5_10_60 <- c(sd(response[data$map==1 & data$alpha==2.5 & data$sight==10
                                       & data$sightangle==data$sightangle[1]]),
                         sd(response[data$map==2 & data$alpha==2.5 & data$sight==10
                                       & data$sightangle==data$sightangle[1]]),
                         sd(response[data$map==3 & data$alpha==2.5 & data$sight==10
                                       & data$sightangle==data$sightangle[1]]),
                         sd(response[data$map==4 & data$alpha==2.5 & data$sight==10
                                       & data$sightangle==data$sightangle[1]]),
                         sd(response[data$map==5 & data$alpha==2.5 & data$sight==10
                                       & data$sightangle==data$sightangle[1]]),
                         sd(response[data$map==6 & data$alpha==2.5 & data$sight==10
                                       & data$sightangle==data$sightangle[1]]),
                         sd(response[data$map==7 & data$alpha==2.5 & data$sight==10
                                       & data$sightangle==data$sightangle[1]]),
                         sd(response[data$map==8 & data$alpha==2.5 & data$sight==10
                                       & data$sightangle==data$sightangle[1]]),
                         sd(response[data$map==9 & data$alpha==2.5 & data$sight==10
                                       & data$sightangle==data$sightangle[1]]),
                         sd(response[data$map==10 & data$alpha==2.5 & data$sight==10
                                       & data$sightangle==data$sightangle[1]]),
                         sd(response[data$map==11 & data$alpha==2.5 & data$sight==10
                                       & data$sightangle==data$sightangle[1]]),
                         sd(response[data$map==12 & data$alpha==2.5 & data$sight==10
                                       & data$sightangle==data$sightangle[1]]),
                         sd(response[data$map==13 & data$alpha==2.5 & data$sight==10
                                       & data$sightangle==data$sightangle[1]]),
                         sd(response[data$map==14 & data$alpha==2.5 & data$sight==10
                                       & data$sightangle==data$sightangle[1]]),
                         sd(response[data$map==15 & data$alpha==2.5 & data$sight==10
                                       & data$sightangle==data$sightangle[1]]),
                         sd(response[data$map==16 & data$alpha==2.5 & data$sight==10
                                       & data$sightangle==data$sightangle[1]]),
                         sd(response[data$map==17 & data$alpha==2.5 & data$sight==10
                                       & data$sightangle==data$sightangle[1]]),
                         sd(response[data$map==18 & data$alpha==2.5 & data$sight==10
                                       & data$sightangle==data$sightangle[1]]),
                         sd(response[data$map==19 & data$alpha==2.5 & data$sight==10
                                       & data$sightangle==data$sightangle[1]]),
                         sd(response[data$map==20 & data$alpha==2.5 & data$sight==10
                                       & data$sightangle==data$sightangle[1]]),
                         sd(response[data$map==21 & data$alpha==2.5 & data$sight==10
                                       & data$sightangle==data$sightangle[1]]))
  
  stds3_10_60 <- c(sd(response[data$map==1 & data$alpha==3.0 & data$sight==10
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==2 & data$alpha==3.0 & data$sight==10
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==3 & data$alpha==3.0 & data$sight==10
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==4 & data$alpha==3.0 & data$sight==10
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==5 & data$alpha==3.0 & data$sight==10
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==6 & data$alpha==3.0 & data$sight==10
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==7 & data$alpha==3.0 & data$sight==10
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==8 & data$alpha==3.0 & data$sight==10
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==9 & data$alpha==3.0 & data$sight==10
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==10 & data$alpha==3.0 & data$sight==10
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==11 & data$alpha==3.0 & data$sight==10
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==12 & data$alpha==3.0 & data$sight==10
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==13 & data$alpha==3.0 & data$sight==10
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==14 & data$alpha==3.0 & data$sight==10
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==15 & data$alpha==3.0 & data$sight==10
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==16 & data$alpha==3.0 & data$sight==10
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==17 & data$alpha==3.0 & data$sight==10
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==18 & data$alpha==3.0 & data$sight==10
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==19 & data$alpha==3.0 & data$sight==10
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==20 & data$alpha==3.0 & data$sight==10
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==21 & data$alpha==3.0 & data$sight==10
                                     & data$sightangle==data$sightangle[1]]))
  
  stds1_20_60 <- c(sd(response[data$map==1 & data$alpha==1.0 & data$sight==20
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==2 & data$alpha==1.0 & data$sight==20
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==3 & data$alpha==1.0 & data$sight==20
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==4 & data$alpha==1.0 & data$sight==20
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==5 & data$alpha==1.0 & data$sight==20
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==6 & data$alpha==1.0 & data$sight==20
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==7 & data$alpha==1.0 & data$sight==20
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==8 & data$alpha==1.0 & data$sight==20
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==9 & data$alpha==1.0 & data$sight==20
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==10 & data$alpha==1.0 & data$sight==20
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==11 & data$alpha==1.0 & data$sight==20
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==12 & data$alpha==1.0 & data$sight==20
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==13 & data$alpha==1.0 & data$sight==20
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==14 & data$alpha==1.0 & data$sight==20
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==15 & data$alpha==1.0 & data$sight==20
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==16 & data$alpha==1.0 & data$sight==20
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==17 & data$alpha==1.0 & data$sight==20
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==18 & data$alpha==1.0 & data$sight==20
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==19 & data$alpha==1.0 & data$sight==20
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==20 & data$alpha==1.0 & data$sight==20
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==21 & data$alpha==1.0 & data$sight==20
                                     & data$sightangle==data$sightangle[1]]))
  
  stds1.5_20_60 <- c(sd(response[data$map==1 & data$alpha==1.5 & data$sight==20
                                       & data$sightangle==data$sightangle[1]]),
                         sd(response[data$map==2 & data$alpha==1.5 & data$sight==20
                                       & data$sightangle==data$sightangle[1]]),
                         sd(response[data$map==3 & data$alpha==1.5 & data$sight==20
                                       & data$sightangle==data$sightangle[1]]),
                         sd(response[data$map==4 & data$alpha==1.5 & data$sight==20
                                       & data$sightangle==data$sightangle[1]]),
                         sd(response[data$map==5 & data$alpha==1.5 & data$sight==20
                                       & data$sightangle==data$sightangle[1]]),
                         sd(response[data$map==6 & data$alpha==1.5 & data$sight==20
                                       & data$sightangle==data$sightangle[1]]),
                         sd(response[data$map==7 & data$alpha==1.5 & data$sight==20
                                       & data$sightangle==data$sightangle[1]]),
                         sd(response[data$map==8 & data$alpha==1.5 & data$sight==20
                                       & data$sightangle==data$sightangle[1]]),
                         sd(response[data$map==9 & data$alpha==1.5 & data$sight==20
                                       & data$sightangle==data$sightangle[1]]),
                         sd(response[data$map==10 & data$alpha==1.5 & data$sight==20
                                       & data$sightangle==data$sightangle[1]]),
                         sd(response[data$map==11 & data$alpha==1.5 & data$sight==20
                                       & data$sightangle==data$sightangle[1]]),
                         sd(response[data$map==12 & data$alpha==1.5 & data$sight==20
                                       & data$sightangle==data$sightangle[1]]),
                         sd(response[data$map==13 & data$alpha==1.5 & data$sight==20
                                       & data$sightangle==data$sightangle[1]]),
                         sd(response[data$map==14 & data$alpha==1.5 & data$sight==20
                                       & data$sightangle==data$sightangle[1]]),
                         sd(response[data$map==15 & data$alpha==1.5 & data$sight==20
                                       & data$sightangle==data$sightangle[1]]),
                         sd(response[data$map==16 & data$alpha==1.5 & data$sight==20
                                       & data$sightangle==data$sightangle[1]]),
                         sd(response[data$map==17 & data$alpha==1.5 & data$sight==20
                                       & data$sightangle==data$sightangle[1]]),
                         sd(response[data$map==18 & data$alpha==1.5 & data$sight==20
                                       & data$sightangle==data$sightangle[1]]),
                         sd(response[data$map==19 & data$alpha==1.5 & data$sight==20
                                       & data$sightangle==data$sightangle[1]]),
                         sd(response[data$map==20 & data$alpha==1.5 & data$sight==20
                                       & data$sightangle==data$sightangle[1]]),
                         sd(response[data$map==21 & data$alpha==1.5 & data$sight==20
                                       & data$sightangle==data$sightangle[1]]))
  
  stds2_20_60 <- c(sd(response[data$map==1 & data$alpha==2.0 & data$sight==20
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==2 & data$alpha==2.0 & data$sight==20
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==3 & data$alpha==2.0 & data$sight==20
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==4 & data$alpha==2.0 & data$sight==20
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==5 & data$alpha==2.0 & data$sight==20
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==6 & data$alpha==2.0 & data$sight==20
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==7 & data$alpha==2.0 & data$sight==20
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==8 & data$alpha==2.0 & data$sight==20
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==9 & data$alpha==2.0 & data$sight==20
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==10 & data$alpha==2.0 & data$sight==20
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==11 & data$alpha==2.0 & data$sight==20
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==12 & data$alpha==2.0 & data$sight==20
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==13 & data$alpha==2.0 & data$sight==20
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==14 & data$alpha==2.0 & data$sight==20
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==15 & data$alpha==2.0 & data$sight==20
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==16 & data$alpha==2.0 & data$sight==20
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==17 & data$alpha==2.0 & data$sight==20
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==18 & data$alpha==2.0 & data$sight==20
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==19 & data$alpha==2.0 & data$sight==20
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==20 & data$alpha==2.0 & data$sight==20
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==21 & data$alpha==2.0 & data$sight==20
                                     & data$sightangle==data$sightangle[1]]))
  
  stds2.5_20_60 <- c(sd(response[data$map==1 & data$alpha==2.5 & data$sight==20
                                       & data$sightangle==data$sightangle[1]]),
                         sd(response[data$map==2 & data$alpha==2.5 & data$sight==20
                                       & data$sightangle==data$sightangle[1]]),
                         sd(response[data$map==3 & data$alpha==2.5 & data$sight==20
                                       & data$sightangle==data$sightangle[1]]),
                         sd(response[data$map==4 & data$alpha==2.5 & data$sight==20
                                       & data$sightangle==data$sightangle[1]]),
                         sd(response[data$map==5 & data$alpha==2.5 & data$sight==20
                                       & data$sightangle==data$sightangle[1]]),
                         sd(response[data$map==6 & data$alpha==2.5 & data$sight==20
                                       & data$sightangle==data$sightangle[1]]),
                         sd(response[data$map==7 & data$alpha==2.5 & data$sight==20
                                       & data$sightangle==data$sightangle[1]]),
                         sd(response[data$map==8 & data$alpha==2.5 & data$sight==20
                                       & data$sightangle==data$sightangle[1]]),
                         sd(response[data$map==9 & data$alpha==2.5 & data$sight==20
                                       & data$sightangle==data$sightangle[1]]),
                         sd(response[data$map==10 & data$alpha==2.5 & data$sight==20
                                       & data$sightangle==data$sightangle[1]]),
                         sd(response[data$map==11 & data$alpha==2.5 & data$sight==20
                                       & data$sightangle==data$sightangle[1]]),
                         sd(response[data$map==12 & data$alpha==2.5 & data$sight==20
                                       & data$sightangle==data$sightangle[1]]),
                         sd(response[data$map==13 & data$alpha==2.5 & data$sight==20
                                       & data$sightangle==data$sightangle[1]]),
                         sd(response[data$map==14 & data$alpha==2.5 & data$sight==20
                                       & data$sightangle==data$sightangle[1]]),
                         sd(response[data$map==15 & data$alpha==2.5 & data$sight==20
                                       & data$sightangle==data$sightangle[1]]),
                         sd(response[data$map==16 & data$alpha==2.5 & data$sight==20
                                       & data$sightangle==data$sightangle[1]]),
                         sd(response[data$map==17 & data$alpha==2.5 & data$sight==20
                                       & data$sightangle==data$sightangle[1]]),
                         sd(response[data$map==18 & data$alpha==2.5 & data$sight==20
                                       & data$sightangle==data$sightangle[1]]),
                         sd(response[data$map==19 & data$alpha==2.5 & data$sight==20
                                       & data$sightangle==data$sightangle[1]]),
                         sd(response[data$map==20 & data$alpha==2.5 & data$sight==20
                                       & data$sightangle==data$sightangle[1]]),
                         sd(response[data$map==21 & data$alpha==2.5 & data$sight==20
                                       & data$sightangle==data$sightangle[1]]))
  
  stds3_20_60 <- c(sd(response[data$map==1 & data$alpha==3.0 & data$sight==20
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==2 & data$alpha==3.0 & data$sight==20
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==3 & data$alpha==3.0 & data$sight==20
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==4 & data$alpha==3.0 & data$sight==20
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==5 & data$alpha==3.0 & data$sight==20
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==6 & data$alpha==3.0 & data$sight==20
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==7 & data$alpha==3.0 & data$sight==20
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==8 & data$alpha==3.0 & data$sight==20
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==9 & data$alpha==3.0 & data$sight==20
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==10 & data$alpha==3.0 & data$sight==20
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==11 & data$alpha==3.0 & data$sight==20
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==12 & data$alpha==3.0 & data$sight==20
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==13 & data$alpha==3.0 & data$sight==20
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==14 & data$alpha==3.0 & data$sight==20
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==15 & data$alpha==3.0 & data$sight==20
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==16 & data$alpha==3.0 & data$sight==20
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==17 & data$alpha==3.0 & data$sight==20
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==18 & data$alpha==3.0 & data$sight==20
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==19 & data$alpha==3.0 & data$sight==20
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==20 & data$alpha==3.0 & data$sight==20
                                     & data$sightangle==data$sightangle[1]]),
                       sd(response[data$map==21 & data$alpha==3.0 & data$sight==20
                                     & data$sightangle==data$sightangle[1]]))
  
  stds1_0_360 <- c(sd(response[data$map==1 & data$alpha==1.0 & data$sight==0
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==2 & data$alpha==1.0 & data$sight==0
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==3 & data$alpha==1.0 & data$sight==0
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==4 & data$alpha==1.0 & data$sight==0
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==5 & data$alpha==1.0 & data$sight==0
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==6 & data$alpha==1.0 & data$sight==0
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==7 & data$alpha==1.0 & data$sight==0
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==8 & data$alpha==1.0 & data$sight==0
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==9 & data$alpha==1.0 & data$sight==0
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==10 & data$alpha==1.0 & data$sight==0
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==11 & data$alpha==1.0 & data$sight==0
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==12 & data$alpha==1.0 & data$sight==0
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==13 & data$alpha==1.0 & data$sight==0
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==14 & data$alpha==1.0 & data$sight==0
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==15 & data$alpha==1.0 & data$sight==0
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==16 & data$alpha==1.0 & data$sight==0
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==17 & data$alpha==1.0 & data$sight==0
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==18 & data$alpha==1.0 & data$sight==0
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==19 & data$alpha==1.0 & data$sight==0
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==20 & data$alpha==1.0 & data$sight==0
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==21 & data$alpha==1.0 & data$sight==0
                                     & data$sightangle==data$sightangle[11]]))
  
  stds1.5_0_360 <- c(sd(response[data$map==1 & data$alpha==1.5 & data$sight==0
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==2 & data$alpha==1.5 & data$sight==0
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==3 & data$alpha==1.5 & data$sight==0
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==4 & data$alpha==1.5 & data$sight==0
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==5 & data$alpha==1.5 & data$sight==0
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==6 & data$alpha==1.5 & data$sight==0
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==7 & data$alpha==1.5 & data$sight==0
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==8 & data$alpha==1.5 & data$sight==0
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==9 & data$alpha==1.5 & data$sight==0
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==10 & data$alpha==1.5 & data$sight==0
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==11 & data$alpha==1.5 & data$sight==0
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==12 & data$alpha==1.5 & data$sight==0
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==13 & data$alpha==1.5 & data$sight==0
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==14 & data$alpha==1.5 & data$sight==0
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==15 & data$alpha==1.5 & data$sight==0
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==16 & data$alpha==1.5 & data$sight==0
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==17 & data$alpha==1.5 & data$sight==0
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==18 & data$alpha==1.5 & data$sight==0
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==19 & data$alpha==1.5 & data$sight==0
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==20 & data$alpha==1.5 & data$sight==0
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==21 & data$alpha==1.5 & data$sight==0
                                       & data$sightangle==data$sightangle[11]]))
  
  stds2_0_360 <- c(sd(response[data$map==1 & data$alpha==2.0 & data$sight==0
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==2 & data$alpha==2.0 & data$sight==0
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==3 & data$alpha==2.0 & data$sight==0
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==4 & data$alpha==2.0 & data$sight==0
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==5 & data$alpha==2.0 & data$sight==0
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==6 & data$alpha==2.0 & data$sight==0
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==7 & data$alpha==2.0 & data$sight==0
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==8 & data$alpha==2.0 & data$sight==0
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==9 & data$alpha==2.0 & data$sight==0
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==10 & data$alpha==2.0 & data$sight==0
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==11 & data$alpha==2.0 & data$sight==0
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==12 & data$alpha==2.0 & data$sight==0
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==13 & data$alpha==2.0 & data$sight==0
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==14 & data$alpha==2.0 & data$sight==0
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==15 & data$alpha==2.0 & data$sight==0
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==16 & data$alpha==2.0 & data$sight==0
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==17 & data$alpha==2.0 & data$sight==0
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==18 & data$alpha==2.0 & data$sight==0
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==19 & data$alpha==2.0 & data$sight==0
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==20 & data$alpha==2.0 & data$sight==0
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==21 & data$alpha==2.0 & data$sight==0
                                     & data$sightangle==data$sightangle[11]]))
  
  stds2.5_0_360 <- c(sd(response[data$map==1 & data$alpha==2.5 & data$sight==0
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==2 & data$alpha==2.5 & data$sight==0
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==3 & data$alpha==2.5 & data$sight==0
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==4 & data$alpha==2.5 & data$sight==0
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==5 & data$alpha==2.5 & data$sight==0
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==6 & data$alpha==2.5 & data$sight==0
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==7 & data$alpha==2.5 & data$sight==0
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==8 & data$alpha==2.5 & data$sight==0
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==9 & data$alpha==2.5 & data$sight==0
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==10 & data$alpha==2.5 & data$sight==0
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==11 & data$alpha==2.5 & data$sight==0
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==12 & data$alpha==2.5 & data$sight==0
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==13 & data$alpha==2.5 & data$sight==0
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==14 & data$alpha==2.5 & data$sight==0
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==15 & data$alpha==2.5 & data$sight==0
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==16 & data$alpha==2.5 & data$sight==0
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==17 & data$alpha==2.5 & data$sight==0
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==18 & data$alpha==2.5 & data$sight==0
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==19 & data$alpha==2.5 & data$sight==0
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==20 & data$alpha==2.5 & data$sight==0
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==21 & data$alpha==2.5 & data$sight==0
                                       & data$sightangle==data$sightangle[11]]))
  
  stds3_0_360 <- c(sd(response[data$map==1 & data$alpha==3.0 & data$sight==0
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==2 & data$alpha==3.0 & data$sight==0
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==3 & data$alpha==3.0 & data$sight==0
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==4 & data$alpha==3.0 & data$sight==0
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==5 & data$alpha==3.0 & data$sight==0
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==6 & data$alpha==3.0 & data$sight==0
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==7 & data$alpha==3.0 & data$sight==0
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==8 & data$alpha==3.0 & data$sight==0
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==9 & data$alpha==3.0 & data$sight==0
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==10 & data$alpha==3.0 & data$sight==0
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==11 & data$alpha==3.0 & data$sight==0
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==12 & data$alpha==3.0 & data$sight==0
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==13 & data$alpha==3.0 & data$sight==0
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==14 & data$alpha==3.0 & data$sight==0
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==15 & data$alpha==3.0 & data$sight==0
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==16 & data$alpha==3.0 & data$sight==0
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==17 & data$alpha==3.0 & data$sight==0
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==18 & data$alpha==3.0 & data$sight==0
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==19 & data$alpha==3.0 & data$sight==0
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==20 & data$alpha==3.0 & data$sight==0
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==21 & data$alpha==3.0 & data$sight==0
                                     & data$sightangle==data$sightangle[11]]))
  
  stds1_1_360 <- c(sd(response[data$map==1 & data$alpha==1.0 & data$sight==1
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==2 & data$alpha==1.0 & data$sight==1
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==3 & data$alpha==1.0 & data$sight==1
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==4 & data$alpha==1.0 & data$sight==1
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==5 & data$alpha==1.0 & data$sight==1
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==6 & data$alpha==1.0 & data$sight==1
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==7 & data$alpha==1.0 & data$sight==1
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==8 & data$alpha==1.0 & data$sight==1
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==9 & data$alpha==1.0 & data$sight==1
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==10 & data$alpha==1.0 & data$sight==1
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==11 & data$alpha==1.0 & data$sight==1
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==12 & data$alpha==1.0 & data$sight==1
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==13 & data$alpha==1.0 & data$sight==1
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==14 & data$alpha==1.0 & data$sight==1
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==15 & data$alpha==1.0 & data$sight==1
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==16 & data$alpha==1.0 & data$sight==1
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==17 & data$alpha==1.0 & data$sight==1
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==18 & data$alpha==1.0 & data$sight==1
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==19 & data$alpha==1.0 & data$sight==1
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==20 & data$alpha==1.0 & data$sight==1
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==21 & data$alpha==1.0 & data$sight==1
                                     & data$sightangle==data$sightangle[11]]))
  
  stds1.5_1_360 <- c(sd(response[data$map==1 & data$alpha==1.5 & data$sight==1
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==2 & data$alpha==1.5 & data$sight==1
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==3 & data$alpha==1.5 & data$sight==1
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==4 & data$alpha==1.5 & data$sight==1
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==5 & data$alpha==1.5 & data$sight==1
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==6 & data$alpha==1.5 & data$sight==1
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==7 & data$alpha==1.5 & data$sight==1
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==8 & data$alpha==1.5 & data$sight==1
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==9 & data$alpha==1.5 & data$sight==1
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==10 & data$alpha==1.5 & data$sight==1
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==11 & data$alpha==1.5 & data$sight==1
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==12 & data$alpha==1.5 & data$sight==1
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==13 & data$alpha==1.5 & data$sight==1
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==14 & data$alpha==1.5 & data$sight==1
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==15 & data$alpha==1.5 & data$sight==1
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==16 & data$alpha==1.5 & data$sight==1
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==17 & data$alpha==1.5 & data$sight==1
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==18 & data$alpha==1.5 & data$sight==1
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==19 & data$alpha==1.5 & data$sight==1
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==20 & data$alpha==1.5 & data$sight==1
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==21 & data$alpha==1.5 & data$sight==1
                                       & data$sightangle==data$sightangle[11]]))
  
  stds2_1_360 <- c(sd(response[data$map==1 & data$alpha==2.0 & data$sight==1
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==2 & data$alpha==2.0 & data$sight==1
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==3 & data$alpha==2.0 & data$sight==1
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==4 & data$alpha==2.0 & data$sight==1
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==5 & data$alpha==2.0 & data$sight==1
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==6 & data$alpha==2.0 & data$sight==1
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==7 & data$alpha==2.0 & data$sight==1
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==8 & data$alpha==2.0 & data$sight==1
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==9 & data$alpha==2.0 & data$sight==1
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==10 & data$alpha==2.0 & data$sight==1
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==11 & data$alpha==2.0 & data$sight==1
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==12 & data$alpha==2.0 & data$sight==1
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==13 & data$alpha==2.0 & data$sight==1
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==14 & data$alpha==2.0 & data$sight==1
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==15 & data$alpha==2.0 & data$sight==1
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==16 & data$alpha==2.0 & data$sight==1
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==17 & data$alpha==2.0 & data$sight==1
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==18 & data$alpha==2.0 & data$sight==1
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==19 & data$alpha==2.0 & data$sight==1
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==20 & data$alpha==2.0 & data$sight==1
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==21 & data$alpha==2.0 & data$sight==1
                                     & data$sightangle==data$sightangle[11]]))
  
  stds2.5_1_360 <- c(sd(response[data$map==1 & data$alpha==2.5 & data$sight==1
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==2 & data$alpha==2.5 & data$sight==1
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==3 & data$alpha==2.5 & data$sight==1
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==4 & data$alpha==2.5 & data$sight==1
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==5 & data$alpha==2.5 & data$sight==1
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==6 & data$alpha==2.5 & data$sight==1
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==7 & data$alpha==2.5 & data$sight==1
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==8 & data$alpha==2.5 & data$sight==1
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==9 & data$alpha==2.5 & data$sight==1
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==10 & data$alpha==2.5 & data$sight==1
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==11 & data$alpha==2.5 & data$sight==1
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==12 & data$alpha==2.5 & data$sight==1
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==13 & data$alpha==2.5 & data$sight==1
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==14 & data$alpha==2.5 & data$sight==1
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==15 & data$alpha==2.5 & data$sight==1
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==16 & data$alpha==2.5 & data$sight==1
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==17 & data$alpha==2.5 & data$sight==1
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==18 & data$alpha==2.5 & data$sight==1
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==19 & data$alpha==2.5 & data$sight==1
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==20 & data$alpha==2.5 & data$sight==1
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==21 & data$alpha==2.5 & data$sight==1
                                       & data$sightangle==data$sightangle[11]]))
  
  stds3_1_360 <- c(sd(response[data$map==1 & data$alpha==3.0 & data$sight==1
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==2 & data$alpha==3.0 & data$sight==1
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==3 & data$alpha==3.0 & data$sight==1
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==4 & data$alpha==3.0 & data$sight==1
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==5 & data$alpha==3.0 & data$sight==1
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==6 & data$alpha==3.0 & data$sight==1
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==7 & data$alpha==3.0 & data$sight==1
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==8 & data$alpha==3.0 & data$sight==1
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==9 & data$alpha==3.0 & data$sight==1
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==10 & data$alpha==3.0 & data$sight==1
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==11 & data$alpha==3.0 & data$sight==1
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==12 & data$alpha==3.0 & data$sight==1
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==13 & data$alpha==3.0 & data$sight==1
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==14 & data$alpha==3.0 & data$sight==1
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==15 & data$alpha==3.0 & data$sight==1
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==16 & data$alpha==3.0 & data$sight==1
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==17 & data$alpha==3.0 & data$sight==1
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==18 & data$alpha==3.0 & data$sight==1
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==19 & data$alpha==3.0 & data$sight==1
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==20 & data$alpha==3.0 & data$sight==1
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==21 & data$alpha==3.0 & data$sight==1
                                     & data$sightangle==data$sightangle[11]]))
  
  stds1_2_360 <- c(sd(response[data$map==1 & data$alpha==1.0 & data$sight==2
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==2 & data$alpha==1.0 & data$sight==2
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==3 & data$alpha==1.0 & data$sight==2
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==4 & data$alpha==1.0 & data$sight==2
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==5 & data$alpha==1.0 & data$sight==2
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==6 & data$alpha==1.0 & data$sight==2
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==7 & data$alpha==1.0 & data$sight==2
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==8 & data$alpha==1.0 & data$sight==2
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==9 & data$alpha==1.0 & data$sight==2
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==10 & data$alpha==1.0 & data$sight==2
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==11 & data$alpha==1.0 & data$sight==2
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==12 & data$alpha==1.0 & data$sight==2
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==13 & data$alpha==1.0 & data$sight==2
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==14 & data$alpha==1.0 & data$sight==2
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==15 & data$alpha==1.0 & data$sight==2
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==16 & data$alpha==1.0 & data$sight==2
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==17 & data$alpha==1.0 & data$sight==2
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==18 & data$alpha==1.0 & data$sight==2
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==19 & data$alpha==1.0 & data$sight==2
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==20 & data$alpha==1.0 & data$sight==2
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==21 & data$alpha==1.0 & data$sight==2
                                     & data$sightangle==data$sightangle[11]]))
  
  stds1.5_2_360 <- c(sd(response[data$map==1 & data$alpha==1.5 & data$sight==2
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==2 & data$alpha==1.5 & data$sight==2
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==3 & data$alpha==1.5 & data$sight==2
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==4 & data$alpha==1.5 & data$sight==2
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==5 & data$alpha==1.5 & data$sight==2
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==6 & data$alpha==1.5 & data$sight==2
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==7 & data$alpha==1.5 & data$sight==2
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==8 & data$alpha==1.5 & data$sight==2
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==9 & data$alpha==1.5 & data$sight==2
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==10 & data$alpha==1.5 & data$sight==2
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==11 & data$alpha==1.5 & data$sight==2
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==12 & data$alpha==1.5 & data$sight==2
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==13 & data$alpha==1.5 & data$sight==2
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==14 & data$alpha==1.5 & data$sight==2
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==15 & data$alpha==1.5 & data$sight==2
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==16 & data$alpha==1.5 & data$sight==2
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==17 & data$alpha==1.5 & data$sight==2
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==18 & data$alpha==1.5 & data$sight==2
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==19 & data$alpha==1.5 & data$sight==2
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==20 & data$alpha==1.5 & data$sight==2
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==21 & data$alpha==1.5 & data$sight==2
                                       & data$sightangle==data$sightangle[11]]))
  
  stds2_2_360 <- c(sd(response[data$map==1 & data$alpha==2.0 & data$sight==2
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==2 & data$alpha==2.0 & data$sight==2
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==3 & data$alpha==2.0 & data$sight==2
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==4 & data$alpha==2.0 & data$sight==2
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==5 & data$alpha==2.0 & data$sight==2
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==6 & data$alpha==2.0 & data$sight==2
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==7 & data$alpha==2.0 & data$sight==2
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==8 & data$alpha==2.0 & data$sight==2
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==9 & data$alpha==2.0 & data$sight==2
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==10 & data$alpha==2.0 & data$sight==2
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==11 & data$alpha==2.0 & data$sight==2
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==12 & data$alpha==2.0 & data$sight==2
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==13 & data$alpha==2.0 & data$sight==2
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==14 & data$alpha==2.0 & data$sight==2
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==15 & data$alpha==2.0 & data$sight==2
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==16 & data$alpha==2.0 & data$sight==2
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==17 & data$alpha==2.0 & data$sight==2
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==18 & data$alpha==2.0 & data$sight==2
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==19 & data$alpha==2.0 & data$sight==2
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==20 & data$alpha==2.0 & data$sight==2
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==21 & data$alpha==2.0 & data$sight==2
                                     & data$sightangle==data$sightangle[11]]))
  
  stds2.5_2_360 <- c(sd(response[data$map==1 & data$alpha==2.5 & data$sight==2
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==2 & data$alpha==2.5 & data$sight==2
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==3 & data$alpha==2.5 & data$sight==2
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==4 & data$alpha==2.5 & data$sight==2
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==5 & data$alpha==2.5 & data$sight==2
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==6 & data$alpha==2.5 & data$sight==2
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==7 & data$alpha==2.5 & data$sight==2
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==8 & data$alpha==2.5 & data$sight==2
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==9 & data$alpha==2.5 & data$sight==2
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==10 & data$alpha==2.5 & data$sight==2
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==11 & data$alpha==2.5 & data$sight==2
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==12 & data$alpha==2.5 & data$sight==2
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==13 & data$alpha==2.5 & data$sight==2
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==14 & data$alpha==2.5 & data$sight==2
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==15 & data$alpha==2.5 & data$sight==2
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==16 & data$alpha==2.5 & data$sight==2
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==17 & data$alpha==2.5 & data$sight==2
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==18 & data$alpha==2.5 & data$sight==2
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==19 & data$alpha==2.5 & data$sight==2
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==20 & data$alpha==2.5 & data$sight==2
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==21 & data$alpha==2.5 & data$sight==2
                                       & data$sightangle==data$sightangle[11]]))
  
  stds3_2_360 <- c(sd(response[data$map==1 & data$alpha==3.0 & data$sight==2
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==2 & data$alpha==3.0 & data$sight==2
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==3 & data$alpha==3.0 & data$sight==2
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==4 & data$alpha==3.0 & data$sight==2
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==5 & data$alpha==3.0 & data$sight==2
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==6 & data$alpha==3.0 & data$sight==2
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==7 & data$alpha==3.0 & data$sight==2
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==8 & data$alpha==3.0 & data$sight==2
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==9 & data$alpha==3.0 & data$sight==2
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==10 & data$alpha==3.0 & data$sight==2
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==11 & data$alpha==3.0 & data$sight==2
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==12 & data$alpha==3.0 & data$sight==2
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==13 & data$alpha==3.0 & data$sight==2
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==14 & data$alpha==3.0 & data$sight==2
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==15 & data$alpha==3.0 & data$sight==2
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==16 & data$alpha==3.0 & data$sight==2
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==17 & data$alpha==3.0 & data$sight==2
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==18 & data$alpha==3.0 & data$sight==2
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==19 & data$alpha==3.0 & data$sight==2
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==20 & data$alpha==3.0 & data$sight==2
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==21 & data$alpha==3.0 & data$sight==2
                                     & data$sightangle==data$sightangle[11]]))
  
  stds1_5_360 <- c(sd(response[data$map==1 & data$alpha==1.0 & data$sight==5
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==2 & data$alpha==1.0 & data$sight==5
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==3 & data$alpha==1.0 & data$sight==5
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==4 & data$alpha==1.0 & data$sight==5
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==5 & data$alpha==1.0 & data$sight==5
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==6 & data$alpha==1.0 & data$sight==5
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==7 & data$alpha==1.0 & data$sight==5
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==8 & data$alpha==1.0 & data$sight==5
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==9 & data$alpha==1.0 & data$sight==5
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==10 & data$alpha==1.0 & data$sight==5
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==11 & data$alpha==1.0 & data$sight==5
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==12 & data$alpha==1.0 & data$sight==5
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==13 & data$alpha==1.0 & data$sight==5
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==14 & data$alpha==1.0 & data$sight==5
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==15 & data$alpha==1.0 & data$sight==5
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==16 & data$alpha==1.0 & data$sight==5
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==17 & data$alpha==1.0 & data$sight==5
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==18 & data$alpha==1.0 & data$sight==5
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==19 & data$alpha==1.0 & data$sight==5
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==20 & data$alpha==1.0 & data$sight==5
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==21 & data$alpha==1.0 & data$sight==5
                                     & data$sightangle==data$sightangle[11]]))
  
  stds1.5_5_360 <- c(sd(response[data$map==1 & data$alpha==1.5 & data$sight==5
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==2 & data$alpha==1.5 & data$sight==5
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==3 & data$alpha==1.5 & data$sight==5
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==4 & data$alpha==1.5 & data$sight==5
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==5 & data$alpha==1.5 & data$sight==5
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==6 & data$alpha==1.5 & data$sight==5
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==7 & data$alpha==1.5 & data$sight==5
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==8 & data$alpha==1.5 & data$sight==5
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==9 & data$alpha==1.5 & data$sight==5
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==10 & data$alpha==1.5 & data$sight==5
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==11 & data$alpha==1.5 & data$sight==5
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==12 & data$alpha==1.5 & data$sight==5
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==13 & data$alpha==1.5 & data$sight==5
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==14 & data$alpha==1.5 & data$sight==5
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==15 & data$alpha==1.5 & data$sight==5
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==16 & data$alpha==1.5 & data$sight==5
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==17 & data$alpha==1.5 & data$sight==5
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==18 & data$alpha==1.5 & data$sight==5
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==19 & data$alpha==1.5 & data$sight==5
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==20 & data$alpha==1.5 & data$sight==5
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==21 & data$alpha==1.5 & data$sight==5
                                       & data$sightangle==data$sightangle[11]]))
  
  stds2_5_360 <- c(sd(response[data$map==1 & data$alpha==2.0 & data$sight==5
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==2 & data$alpha==2.0 & data$sight==5
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==3 & data$alpha==2.0 & data$sight==5
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==4 & data$alpha==2.0 & data$sight==5
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==5 & data$alpha==2.0 & data$sight==5
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==6 & data$alpha==2.0 & data$sight==5
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==7 & data$alpha==2.0 & data$sight==5
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==8 & data$alpha==2.0 & data$sight==5
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==9 & data$alpha==2.0 & data$sight==5
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==10 & data$alpha==2.0 & data$sight==5
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==11 & data$alpha==2.0 & data$sight==5
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==12 & data$alpha==2.0 & data$sight==5
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==13 & data$alpha==2.0 & data$sight==5
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==14 & data$alpha==2.0 & data$sight==5
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==15 & data$alpha==2.0 & data$sight==5
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==16 & data$alpha==2.0 & data$sight==5
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==17 & data$alpha==2.0 & data$sight==5
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==18 & data$alpha==2.0 & data$sight==5
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==19 & data$alpha==2.0 & data$sight==5
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==20 & data$alpha==2.0 & data$sight==5
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==21 & data$alpha==2.0 & data$sight==5
                                     & data$sightangle==data$sightangle[11]]))
  
  stds2.5_5_360 <- c(sd(response[data$map==1 & data$alpha==2.5 & data$sight==5
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==2 & data$alpha==2.5 & data$sight==5
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==3 & data$alpha==2.5 & data$sight==5
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==4 & data$alpha==2.5 & data$sight==5
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==5 & data$alpha==2.5 & data$sight==5
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==6 & data$alpha==2.5 & data$sight==5
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==7 & data$alpha==2.5 & data$sight==5
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==8 & data$alpha==2.5 & data$sight==5
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==9 & data$alpha==2.5 & data$sight==5
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==10 & data$alpha==2.5 & data$sight==5
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==11 & data$alpha==2.5 & data$sight==5
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==12 & data$alpha==2.5 & data$sight==5
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==13 & data$alpha==2.5 & data$sight==5
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==14 & data$alpha==2.5 & data$sight==5
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==15 & data$alpha==2.5 & data$sight==5
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==16 & data$alpha==2.5 & data$sight==5
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==17 & data$alpha==2.5 & data$sight==5
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==18 & data$alpha==2.5 & data$sight==5
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==19 & data$alpha==2.5 & data$sight==5
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==20 & data$alpha==2.5 & data$sight==5
                                       & data$sightangle==data$sightangle[11]]),
                         sd(response[data$map==21 & data$alpha==2.5 & data$sight==5
                                       & data$sightangle==data$sightangle[11]]))
  
  stds3_5_360 <- c(sd(response[data$map==1 & data$alpha==3.0 & data$sight==5
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==2 & data$alpha==3.0 & data$sight==5
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==3 & data$alpha==3.0 & data$sight==5
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==4 & data$alpha==3.0 & data$sight==5
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==5 & data$alpha==3.0 & data$sight==5
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==6 & data$alpha==3.0 & data$sight==5
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==7 & data$alpha==3.0 & data$sight==5
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==8 & data$alpha==3.0 & data$sight==5
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==9 & data$alpha==3.0 & data$sight==5
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==10 & data$alpha==3.0 & data$sight==5
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==11 & data$alpha==3.0 & data$sight==5
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==12 & data$alpha==3.0 & data$sight==5
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==13 & data$alpha==3.0 & data$sight==5
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==14 & data$alpha==3.0 & data$sight==5
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==15 & data$alpha==3.0 & data$sight==5
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==16 & data$alpha==3.0 & data$sight==5
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==17 & data$alpha==3.0 & data$sight==5
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==18 & data$alpha==3.0 & data$sight==5
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==19 & data$alpha==3.0 & data$sight==5
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==20 & data$alpha==3.0 & data$sight==5
                                     & data$sightangle==data$sightangle[11]]),
                       sd(response[data$map==21 & data$alpha==3.0 & data$sight==5
                                     & data$sightangle==data$sightangle[11]]))
  
  stds1_10_360 <- c(sd(response[data$map==1 & data$alpha==1.0 & data$sight==10
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==2 & data$alpha==1.0 & data$sight==10
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==3 & data$alpha==1.0 & data$sight==10
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==4 & data$alpha==1.0 & data$sight==10
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==5 & data$alpha==1.0 & data$sight==10
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==6 & data$alpha==1.0 & data$sight==10
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==7 & data$alpha==1.0 & data$sight==10
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==8 & data$alpha==1.0 & data$sight==10
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==9 & data$alpha==1.0 & data$sight==10
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==10 & data$alpha==1.0 & data$sight==10
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==11 & data$alpha==1.0 & data$sight==10
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==12 & data$alpha==1.0 & data$sight==10
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==13 & data$alpha==1.0 & data$sight==10
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==14 & data$alpha==1.0 & data$sight==10
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==15 & data$alpha==1.0 & data$sight==10
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==16 & data$alpha==1.0 & data$sight==10
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==17 & data$alpha==1.0 & data$sight==10
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==18 & data$alpha==1.0 & data$sight==10
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==19 & data$alpha==1.0 & data$sight==10
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==20 & data$alpha==1.0 & data$sight==10
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==21 & data$alpha==1.0 & data$sight==10
                                      & data$sightangle==data$sightangle[11]]))
  
  stds1.5_10_360 <- c(sd(response[data$map==1 & data$alpha==1.5 & data$sight==10
                                        & data$sightangle==data$sightangle[11]]),
                          sd(response[data$map==2 & data$alpha==1.5 & data$sight==10
                                        & data$sightangle==data$sightangle[11]]),
                          sd(response[data$map==3 & data$alpha==1.5 & data$sight==10
                                        & data$sightangle==data$sightangle[11]]),
                          sd(response[data$map==4 & data$alpha==1.5 & data$sight==10
                                        & data$sightangle==data$sightangle[11]]),
                          sd(response[data$map==5 & data$alpha==1.5 & data$sight==10
                                        & data$sightangle==data$sightangle[11]]),
                          sd(response[data$map==6 & data$alpha==1.5 & data$sight==10
                                        & data$sightangle==data$sightangle[11]]),
                          sd(response[data$map==7 & data$alpha==1.5 & data$sight==10
                                        & data$sightangle==data$sightangle[11]]),
                          sd(response[data$map==8 & data$alpha==1.5 & data$sight==10
                                        & data$sightangle==data$sightangle[11]]),
                          sd(response[data$map==9 & data$alpha==1.5 & data$sight==10
                                        & data$sightangle==data$sightangle[11]]),
                          sd(response[data$map==10 & data$alpha==1.5 & data$sight==10
                                        & data$sightangle==data$sightangle[11]]),
                          sd(response[data$map==11 & data$alpha==1.5 & data$sight==10
                                        & data$sightangle==data$sightangle[11]]),
                          sd(response[data$map==12 & data$alpha==1.5 & data$sight==10
                                        & data$sightangle==data$sightangle[11]]),
                          sd(response[data$map==13 & data$alpha==1.5 & data$sight==10
                                        & data$sightangle==data$sightangle[11]]),
                          sd(response[data$map==14 & data$alpha==1.5 & data$sight==10
                                        & data$sightangle==data$sightangle[11]]),
                          sd(response[data$map==15 & data$alpha==1.5 & data$sight==10
                                        & data$sightangle==data$sightangle[11]]),
                          sd(response[data$map==16 & data$alpha==1.5 & data$sight==10
                                        & data$sightangle==data$sightangle[11]]),
                          sd(response[data$map==17 & data$alpha==1.5 & data$sight==10
                                        & data$sightangle==data$sightangle[11]]),
                          sd(response[data$map==18 & data$alpha==1.5 & data$sight==10
                                        & data$sightangle==data$sightangle[11]]),
                          sd(response[data$map==19 & data$alpha==1.5 & data$sight==10
                                        & data$sightangle==data$sightangle[11]]),
                          sd(response[data$map==20 & data$alpha==1.5 & data$sight==10
                                        & data$sightangle==data$sightangle[11]]),
                          sd(response[data$map==21 & data$alpha==1.5 & data$sight==10
                                        & data$sightangle==data$sightangle[11]]))
  
  stds2_10_360 <- c(sd(response[data$map==1 & data$alpha==2.0 & data$sight==10
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==2 & data$alpha==2.0 & data$sight==10
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==3 & data$alpha==2.0 & data$sight==10
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==4 & data$alpha==2.0 & data$sight==10
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==5 & data$alpha==2.0 & data$sight==10
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==6 & data$alpha==2.0 & data$sight==10
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==7 & data$alpha==2.0 & data$sight==10
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==8 & data$alpha==2.0 & data$sight==10
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==9 & data$alpha==2.0 & data$sight==10
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==10 & data$alpha==2.0 & data$sight==10
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==11 & data$alpha==2.0 & data$sight==10
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==12 & data$alpha==2.0 & data$sight==10
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==13 & data$alpha==2.0 & data$sight==10
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==14 & data$alpha==2.0 & data$sight==10
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==15 & data$alpha==2.0 & data$sight==10
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==16 & data$alpha==2.0 & data$sight==10
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==17 & data$alpha==2.0 & data$sight==10
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==18 & data$alpha==2.0 & data$sight==10
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==19 & data$alpha==2.0 & data$sight==10
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==20 & data$alpha==2.0 & data$sight==10
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==21 & data$alpha==2.0 & data$sight==10
                                      & data$sightangle==data$sightangle[11]]))
  
  stds2.5_10_360 <- c(sd(response[data$map==1 & data$alpha==2.5 & data$sight==10
                                        & data$sightangle==data$sightangle[11]]),
                          sd(response[data$map==2 & data$alpha==2.5 & data$sight==10
                                        & data$sightangle==data$sightangle[11]]),
                          sd(response[data$map==3 & data$alpha==2.5 & data$sight==10
                                        & data$sightangle==data$sightangle[11]]),
                          sd(response[data$map==4 & data$alpha==2.5 & data$sight==10
                                        & data$sightangle==data$sightangle[11]]),
                          sd(response[data$map==5 & data$alpha==2.5 & data$sight==10
                                        & data$sightangle==data$sightangle[11]]),
                          sd(response[data$map==6 & data$alpha==2.5 & data$sight==10
                                        & data$sightangle==data$sightangle[11]]),
                          sd(response[data$map==7 & data$alpha==2.5 & data$sight==10
                                        & data$sightangle==data$sightangle[11]]),
                          sd(response[data$map==8 & data$alpha==2.5 & data$sight==10
                                        & data$sightangle==data$sightangle[11]]),
                          sd(response[data$map==9 & data$alpha==2.5 & data$sight==10
                                        & data$sightangle==data$sightangle[11]]),
                          sd(response[data$map==10 & data$alpha==2.5 & data$sight==10
                                        & data$sightangle==data$sightangle[11]]),
                          sd(response[data$map==11 & data$alpha==2.5 & data$sight==10
                                        & data$sightangle==data$sightangle[11]]),
                          sd(response[data$map==12 & data$alpha==2.5 & data$sight==10
                                        & data$sightangle==data$sightangle[11]]),
                          sd(response[data$map==13 & data$alpha==2.5 & data$sight==10
                                        & data$sightangle==data$sightangle[11]]),
                          sd(response[data$map==14 & data$alpha==2.5 & data$sight==10
                                        & data$sightangle==data$sightangle[11]]),
                          sd(response[data$map==15 & data$alpha==2.5 & data$sight==10
                                        & data$sightangle==data$sightangle[11]]),
                          sd(response[data$map==16 & data$alpha==2.5 & data$sight==10
                                        & data$sightangle==data$sightangle[11]]),
                          sd(response[data$map==17 & data$alpha==2.5 & data$sight==10
                                        & data$sightangle==data$sightangle[11]]),
                          sd(response[data$map==18 & data$alpha==2.5 & data$sight==10
                                        & data$sightangle==data$sightangle[11]]),
                          sd(response[data$map==19 & data$alpha==2.5 & data$sight==10
                                        & data$sightangle==data$sightangle[11]]),
                          sd(response[data$map==20 & data$alpha==2.5 & data$sight==10
                                        & data$sightangle==data$sightangle[11]]),
                          sd(response[data$map==21 & data$alpha==2.5 & data$sight==10
                                        & data$sightangle==data$sightangle[11]]))
  
  stds3_10_360 <- c(sd(response[data$map==1 & data$alpha==3.0 & data$sight==10
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==2 & data$alpha==3.0 & data$sight==10
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==3 & data$alpha==3.0 & data$sight==10
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==4 & data$alpha==3.0 & data$sight==10
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==5 & data$alpha==3.0 & data$sight==10
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==6 & data$alpha==3.0 & data$sight==10
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==7 & data$alpha==3.0 & data$sight==10
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==8 & data$alpha==3.0 & data$sight==10
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==9 & data$alpha==3.0 & data$sight==10
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==10 & data$alpha==3.0 & data$sight==10
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==11 & data$alpha==3.0 & data$sight==10
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==12 & data$alpha==3.0 & data$sight==10
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==13 & data$alpha==3.0 & data$sight==10
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==14 & data$alpha==3.0 & data$sight==10
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==15 & data$alpha==3.0 & data$sight==10
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==16 & data$alpha==3.0 & data$sight==10
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==17 & data$alpha==3.0 & data$sight==10
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==18 & data$alpha==3.0 & data$sight==10
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==19 & data$alpha==3.0 & data$sight==10
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==20 & data$alpha==3.0 & data$sight==10
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==21 & data$alpha==3.0 & data$sight==10
                                      & data$sightangle==data$sightangle[11]]))
  
  stds1_20_360 <- c(sd(response[data$map==1 & data$alpha==1.0 & data$sight==20
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==2 & data$alpha==1.0 & data$sight==20
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==3 & data$alpha==1.0 & data$sight==20
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==4 & data$alpha==1.0 & data$sight==20
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==5 & data$alpha==1.0 & data$sight==20
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==6 & data$alpha==1.0 & data$sight==20
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==7 & data$alpha==1.0 & data$sight==20
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==8 & data$alpha==1.0 & data$sight==20
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==9 & data$alpha==1.0 & data$sight==20
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==10 & data$alpha==1.0 & data$sight==20
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==11 & data$alpha==1.0 & data$sight==20
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==12 & data$alpha==1.0 & data$sight==20
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==13 & data$alpha==1.0 & data$sight==20
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==14 & data$alpha==1.0 & data$sight==20
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==15 & data$alpha==1.0 & data$sight==20
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==16 & data$alpha==1.0 & data$sight==20
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==17 & data$alpha==1.0 & data$sight==20
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==18 & data$alpha==1.0 & data$sight==20
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==19 & data$alpha==1.0 & data$sight==20
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==20 & data$alpha==1.0 & data$sight==20
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==21 & data$alpha==1.0 & data$sight==20
                                      & data$sightangle==data$sightangle[11]]))
  
  stds1.5_20_360 <- c(sd(response[data$map==1 & data$alpha==1.5 & data$sight==20
                                        & data$sightangle==data$sightangle[11]]),
                          sd(response[data$map==2 & data$alpha==1.5 & data$sight==20
                                        & data$sightangle==data$sightangle[11]]),
                          sd(response[data$map==3 & data$alpha==1.5 & data$sight==20
                                        & data$sightangle==data$sightangle[11]]),
                          sd(response[data$map==4 & data$alpha==1.5 & data$sight==20
                                        & data$sightangle==data$sightangle[11]]),
                          sd(response[data$map==5 & data$alpha==1.5 & data$sight==20
                                        & data$sightangle==data$sightangle[11]]),
                          sd(response[data$map==6 & data$alpha==1.5 & data$sight==20
                                        & data$sightangle==data$sightangle[11]]),
                          sd(response[data$map==7 & data$alpha==1.5 & data$sight==20
                                        & data$sightangle==data$sightangle[11]]),
                          sd(response[data$map==8 & data$alpha==1.5 & data$sight==20
                                        & data$sightangle==data$sightangle[11]]),
                          sd(response[data$map==9 & data$alpha==1.5 & data$sight==20
                                        & data$sightangle==data$sightangle[11]]),
                          sd(response[data$map==10 & data$alpha==1.5 & data$sight==20
                                        & data$sightangle==data$sightangle[11]]),
                          sd(response[data$map==11 & data$alpha==1.5 & data$sight==20
                                        & data$sightangle==data$sightangle[11]]),
                          sd(response[data$map==12 & data$alpha==1.5 & data$sight==20
                                        & data$sightangle==data$sightangle[11]]),
                          sd(response[data$map==13 & data$alpha==1.5 & data$sight==20
                                        & data$sightangle==data$sightangle[11]]),
                          sd(response[data$map==14 & data$alpha==1.5 & data$sight==20
                                        & data$sightangle==data$sightangle[11]]),
                          sd(response[data$map==15 & data$alpha==1.5 & data$sight==20
                                        & data$sightangle==data$sightangle[11]]),
                          sd(response[data$map==16 & data$alpha==1.5 & data$sight==20
                                        & data$sightangle==data$sightangle[11]]),
                          sd(response[data$map==17 & data$alpha==1.5 & data$sight==20
                                        & data$sightangle==data$sightangle[11]]),
                          sd(response[data$map==18 & data$alpha==1.5 & data$sight==20
                                        & data$sightangle==data$sightangle[11]]),
                          sd(response[data$map==19 & data$alpha==1.5 & data$sight==20
                                        & data$sightangle==data$sightangle[11]]),
                          sd(response[data$map==20 & data$alpha==1.5 & data$sight==20
                                        & data$sightangle==data$sightangle[11]]),
                          sd(response[data$map==21 & data$alpha==1.5 & data$sight==20
                                        & data$sightangle==data$sightangle[11]]))
  
  stds2_20_360 <- c(sd(response[data$map==1 & data$alpha==2.0 & data$sight==20
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==2 & data$alpha==2.0 & data$sight==20
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==3 & data$alpha==2.0 & data$sight==20
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==4 & data$alpha==2.0 & data$sight==20
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==5 & data$alpha==2.0 & data$sight==20
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==6 & data$alpha==2.0 & data$sight==20
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==7 & data$alpha==2.0 & data$sight==20
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==8 & data$alpha==2.0 & data$sight==20
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==9 & data$alpha==2.0 & data$sight==20
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==10 & data$alpha==2.0 & data$sight==20
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==11 & data$alpha==2.0 & data$sight==20
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==12 & data$alpha==2.0 & data$sight==20
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==13 & data$alpha==2.0 & data$sight==20
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==14 & data$alpha==2.0 & data$sight==20
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==15 & data$alpha==2.0 & data$sight==20
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==16 & data$alpha==2.0 & data$sight==20
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==17 & data$alpha==2.0 & data$sight==20
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==18 & data$alpha==2.0 & data$sight==20
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==19 & data$alpha==2.0 & data$sight==20
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==20 & data$alpha==2.0 & data$sight==20
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==21 & data$alpha==2.0 & data$sight==20
                                      & data$sightangle==data$sightangle[11]]))
  
  stds2.5_20_360 <- c(sd(response[data$map==1 & data$alpha==2.5 & data$sight==20
                                        & data$sightangle==data$sightangle[11]]),
                          sd(response[data$map==2 & data$alpha==2.5 & data$sight==20
                                        & data$sightangle==data$sightangle[11]]),
                          sd(response[data$map==3 & data$alpha==2.5 & data$sight==20
                                        & data$sightangle==data$sightangle[11]]),
                          sd(response[data$map==4 & data$alpha==2.5 & data$sight==20
                                        & data$sightangle==data$sightangle[11]]),
                          sd(response[data$map==5 & data$alpha==2.5 & data$sight==20
                                        & data$sightangle==data$sightangle[11]]),
                          sd(response[data$map==6 & data$alpha==2.5 & data$sight==20
                                        & data$sightangle==data$sightangle[11]]),
                          sd(response[data$map==7 & data$alpha==2.5 & data$sight==20
                                        & data$sightangle==data$sightangle[11]]),
                          sd(response[data$map==8 & data$alpha==2.5 & data$sight==20
                                        & data$sightangle==data$sightangle[11]]),
                          sd(response[data$map==9 & data$alpha==2.5 & data$sight==20
                                        & data$sightangle==data$sightangle[11]]),
                          sd(response[data$map==10 & data$alpha==2.5 & data$sight==20
                                        & data$sightangle==data$sightangle[11]]),
                          sd(response[data$map==11 & data$alpha==2.5 & data$sight==20
                                        & data$sightangle==data$sightangle[11]]),
                          sd(response[data$map==12 & data$alpha==2.5 & data$sight==20
                                        & data$sightangle==data$sightangle[11]]),
                          sd(response[data$map==13 & data$alpha==2.5 & data$sight==20
                                        & data$sightangle==data$sightangle[11]]),
                          sd(response[data$map==14 & data$alpha==2.5 & data$sight==20
                                        & data$sightangle==data$sightangle[11]]),
                          sd(response[data$map==15 & data$alpha==2.5 & data$sight==20
                                        & data$sightangle==data$sightangle[11]]),
                          sd(response[data$map==16 & data$alpha==2.5 & data$sight==20
                                        & data$sightangle==data$sightangle[11]]),
                          sd(response[data$map==17 & data$alpha==2.5 & data$sight==20
                                        & data$sightangle==data$sightangle[11]]),
                          sd(response[data$map==18 & data$alpha==2.5 & data$sight==20
                                        & data$sightangle==data$sightangle[11]]),
                          sd(response[data$map==19 & data$alpha==2.5 & data$sight==20
                                        & data$sightangle==data$sightangle[11]]),
                          sd(response[data$map==20 & data$alpha==2.5 & data$sight==20
                                        & data$sightangle==data$sightangle[11]]),
                          sd(response[data$map==21 & data$alpha==2.5 & data$sight==20
                                        & data$sightangle==data$sightangle[11]]))
  
  stds3_20_360 <- c(sd(response[data$map==1 & data$alpha==3.0 & data$sight==20
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==2 & data$alpha==3.0 & data$sight==20
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==3 & data$alpha==3.0 & data$sight==20
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==4 & data$alpha==3.0 & data$sight==20
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==5 & data$alpha==3.0 & data$sight==20
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==6 & data$alpha==3.0 & data$sight==20
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==7 & data$alpha==3.0 & data$sight==20
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==8 & data$alpha==3.0 & data$sight==20
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==9 & data$alpha==3.0 & data$sight==20
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==10 & data$alpha==3.0 & data$sight==20
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==11 & data$alpha==3.0 & data$sight==20
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==12 & data$alpha==3.0 & data$sight==20
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==13 & data$alpha==3.0 & data$sight==20
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==14 & data$alpha==3.0 & data$sight==20
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==15 & data$alpha==3.0 & data$sight==20
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==16 & data$alpha==3.0 & data$sight==20
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==17 & data$alpha==3.0 & data$sight==20
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==18 & data$alpha==3.0 & data$sight==20
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==19 & data$alpha==3.0 & data$sight==20
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==20 & data$alpha==3.0 & data$sight==20
                                      & data$sightangle==data$sightangle[11]]),
                        sd(response[data$map==21 & data$alpha==3.0 & data$sight==20
                                      & data$sightangle==data$sightangle[11]]))
  
  # Assemble output data frame
  
  df <- data.frame(averages1_0_60, stds1_0_60, averages1.5_0_60, stds1.5_0_60, averages2_0_60, 
                   stds2_0_60, averages2.5_0_60, stds2.5_0_60, averages3_0_60, stds3_0_60,
                   averages1_1_60, stds1_1_60, averages1.5_1_60, stds1.5_1_60, averages2_1_60,
                   stds2_1_60, averages2.5_1_60, stds2.5_1_60, averages3_1_60, stds3_1_60, 
                   averages1_2_60, stds1_2_60, averages1.5_2_60, stds1.5_2_60, averages2_2_60,
                   stds2_2_60, averages2.5_2_60, stds2.5_2_60, averages3_2_60, stds3_2_60,
                   averages1_5_60, stds1_5_60, averages1.5_5_60, stds1.5_5_60, averages2_5_60,
                   stds2_5_60, averages2.5_5_60, stds2.5_5_60, averages3_5_60, stds3_5_60,
                   averages1_10_60, stds1_10_60, averages1.5_10_60, stds1.5_10_60,
                   averages2_10_60, stds2_10_60, averages2.5_10_60, stds2.5_10_60,
                   averages3_10_60, stds3_10_60, averages1_20_60, stds1_20_60, averages1.5_20_60,
                   stds1.5_20_60, averages2_20_60, stds2_20_60, averages2.5_20_60,
                   stds2.5_20_60, averages3_20_60, stds3_20_60, 
                   averages1_0_360, stds1_0_360, averages1.5_0_360, stds1.5_0_360, averages2_0_360, 
                   stds2_0_360, averages2.5_0_360, stds2.5_0_360, averages3_0_360, stds3_0_360,
                   averages1_1_360, stds1_1_360, averages1.5_1_360, stds1.5_1_360, averages2_1_360,
                   stds2_1_360, averages2.5_1_360, stds2.5_1_360, averages3_1_360, stds3_1_360, 
                   averages1_2_360, stds1_2_360, averages1.5_2_360, stds1.5_2_360, averages2_2_360,
                   stds2_2_360, averages2.5_2_360, stds2.5_2_360, averages3_2_360, stds3_2_360,
                   averages1_5_360, stds1_5_360, averages1.5_5_360, stds1.5_5_360, averages2_5_360,
                   stds2_5_360, averages2.5_5_360, stds2.5_5_360, averages3_5_360, stds3_5_360,
                   averages1_10_360, stds1_10_360, averages1.5_10_360, stds1.5_10_360,
                   averages2_10_360, stds2_10_360, averages2.5_10_360, stds2.5_10_360,
                   averages3_10_360, stds3_10_360, averages1_20_360, stds1_20_360, averages1.5_20_360,
                   stds1.5_20_360, averages2_20_360, stds2_20_360, averages2.5_20_360,
                   stds2.5_20_360, averages3_20_360, stds3_20_360)
  
  return(df)
}