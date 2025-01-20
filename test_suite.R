#Set up.
#Include package.
library(asmodeus)
library(gifski)

#Set seed - ensures same outcome from randomised functions.
set.seed(1)

#Define main save location.
main <- "C:/Users/lxp609/OneDrive - University of Birmingham/Desktop/Stockholm Research Trip/Package/Test Suite/"

#Simulating data.
#Simulate a data set with small circular clusters.
small_cluster_data <- simulatecircularclusters(ROI = 1000,
                                         number_of_clusters = 10,
                                         cluster_radius = 20,
                                         points_per_cluster = 15,
                                         background_to_cluster_ratio = 0.33)

#Simulate a data set with large circular clusters.
big_cluster_data <- simulatecircularclusters(ROI = 1000,
                                               number_of_clusters = 5,
                                               cluster_radius = 40,
                                               points_per_cluster = 30,
                                               background_to_cluster_ratio = 0.33)

#Simulate a data set with very large circular clusters.
very_big_cluster_data <- simulatecircularclusters(ROI = 1000,
                                             number_of_clusters = 3,
                                             cluster_radius = 80,
                                             points_per_cluster = 50,
                                             background_to_cluster_ratio = 0.33)

#Simulate a data set with complete spatially random (CSR) data. This will have the same number of points as the cluster data.
csr_data <- simulatecsr(ROI = 1000, number_of_points = nrow(small_cluster_data))

#We can plot these point clouds individually.
plotpoints(small_cluster_data, xlimits = c(0, 1000), ylimits = c(0, 1000))
plotpoints(big_cluster_data, xlimits = c(0, 1000), ylimits = c(0, 1000))
plotpoints(very_big_cluster_data, xlimits = c(0, 1000), ylimits = c(0, 1000))
plotpoints(csr_data, xlimits = c(0, 1000), ylimits = c(0, 1000))

#Get Ripley's K functions.
small_cluster_K <- ripleyk(points = small_cluster_data, ROI = 1000)
big_cluster_K <- ripleyk(points = big_cluster_data, ROI = 1000)
very_big_cluster_K <- ripleyk(points = very_big_cluster_data, ROI = 1000)
csr_k <- ripleyk(points = csr_data, ROI = 1000)

#Plot Ripley's K functions.
plotlines(data = cbind(small_cluster_K,
                       big_cluster_K[,2],
                       very_big_cluster_K[,2],
                       csr_k[,2]),
          labels = c("r", "K(r)"))

#Global K simulations.
#Create a simulation with default parameters which tries to fit an initially CSR point cloud to the small cluster data set.
sim1 <- globalripleysim(point_cloud = small_cluster_data)

#Animate point distribution.
animate_points(simulation = sim1)

#Save the animation using gganimate's anim_save function.
gganimate::anim_save(filename = paste(main, "1_Small_Clusters_Simulation.gif", sep = ""))

#Animate change in Ripley's K function.
animate_target(simulation = sim1, ROI = 1000, target = small_cluster_K)

#We can remove the legend by setting legendposition = "none". The target here is always static anyway.
animate_target(simulation = sim1, ROI = 1000, target = small_cluster_K, legendposition = "none")
gganimate::anim_save(filename = paste(main, "2_Small_Clusters_K.gif", sep = ""))

#Animate K function directly from point cloud (without manually calculating K function).
animate_K_from_points(simulation = sim1, ROI = 1000, point_clouds = small_cluster_data, legendposition = "none")

#Animate H function directly from point cloud (without manually calculating H function).
animate_H_from_points(simulation = sim1, ROI = 1000, point_clouds = small_cluster_data, legendposition = "none")
gganimate::anim_save(filename = paste(main, "3_Small_Clusters_H.gif", sep = ""))

#Define simulation parameters manually, use help("globalripleysim") for details. In particular, we can specify an initial distribution to start the simulation from. In this case, we have set the initial distribution as the big clusters data.
sim2 <- globalripleysim(D_min = 0,
                       D_max = 63,
                       ROI = 1000,
                       times = 1200,
                       point_cloud = small_cluster_data,
                       rmax = 200,
                       nrval = 20,
                       initial_distribution = big_cluster_data)

#The simulation now starts from large clusters.
animate_points(simulation = sim2)
gganimate::anim_save(filename = paste(main, "4_Big_Clusters_to_Small_Clusters_Simulation.gif", sep = ""))

#This is reflected in the H function.
animate_H_from_points(simulation = sim2, ROI = 1000, point_clouds = small_cluster_data, legendposition = "none")
gganimate::anim_save(filename = paste(main, "5_Big_Clusters_to_Small_Clusters_H.gif", sep = ""))

#In some cases, we can define our own target K function too, but we must specify how many points should be used in the simulation.
sim3 <- globalripleysim(D_min = 0,
                        D_max = 63,
                        ROI = 1000,
                        times = 1200,
                        target = small_cluster_K,
                        number_of_points = 982,
                        rmax = 200,
                        nrval = 20,
                        initial_distribution = big_cluster_data)

#In this case, we still try to fit to the small clusters K function, so there is no difference. 
animate_points(simulation = sim3)
gganimate::anim_save(filename = paste(main, "6_Big_Clusters_to_Small_Clusters_Simulation_from_Target.gif", sep = ""))

#The target here is defined manually.
animate_H_from_points(simulation = sim3, ROI = 1000, point_clouds = small_cluster_data, legendposition = "none")
gganimate::anim_save(filename = paste(main, "7_Big_Clusters_to_Small_Clusters_H_from_Target.gif", sep = ""))

#Multiple targets.
#We can define multiple targets by inputting a list of point clouds or targets.
target_clouds <- list(big_cluster_data, small_cluster_data)

#We can force the simulator to switch from one target to another at given time points. This can be done for as many targets as we like.
sim4 <- distripleysim(point_clouds = target_clouds, time_points = c(500, 1000))

#The target is big clusters up until time point 500, then switches to small clusters until the end of the simulation (time point 1000).
animate_points(simulation = sim4)
gganimate::anim_save(filename = paste(main, "8_Discrete_Time_Points.gif", sep = ""))

#We can observe the shift in target in the H function. Make sure to specify the time points in the animation function.
animate_H_from_points(simulation = sim4, ROI = 1000, point_clouds = target_clouds, time_points = c(500, 1000), legendposition = "none")
gganimate::anim_save(filename = paste(main, "9_Discrete_Time_Points_H.gif", sep = ""))

#We can also provide both targets simultaneously. Each point will move towards which ever target is closest.
sim5 <- simripleysim(point_clouds = target_clouds, times = 1000, perturbation_times = 500, perturbation_strength = 100)

#Points may drift towards either target. We can introduce perturbations which offest each point with distance given by perturbation_strength. Here we use a 100nm offset at time point 500.
animate_points(simulation = sim5)
gganimate::anim_save(filename = paste(main, "10_Simultaneous_Targets.gif", sep = ""))

#The H function may assume one target over another, or fall somewhere inbetween.
animate_H_from_points(simulation = sim5, ROI = 1000, point_clouds = target_clouds, legendposition = "none")
gganimate::anim_save(filename = paste(main, "11_Simultaneous_Targets_H.gif", sep = ""))

#Instead of offsetting points, we can cross-link points together. This gives the effect of "squeezing" points into clusters.
sim6 <- squeezeripleysim(point_clouds = target_clouds, times = 1000, perturbation_time = 500, number_cross_linked_clusters = 5, number_cross_linked = 30, cross_linked_cluster_radius = 40)

#We form 5 cross-linked clusters at time point 500. Each cluster contains 30 points and is of radius 40.
animate_points(simulation = sim6)
gganimate::anim_save(filename = paste(main, "12_Simultaneous_Targets_with_Squeeze.gif", sep = ""))

#A shift in the H function is shown at time point 500, when the squeeze perturbation is introduced.
animate_H_from_points(simulation = sim6, ROI = 1000, point_clouds = target_clouds, legendposition = "none")
gganimate::anim_save(filename = paste(main, "13_Simultaneous_Targets_with_Squeeze_H.gif", sep = ""))

#If we don't wish to immobilise points after cross-linking, we can release them.
sim7 <- squeezereleaseripleysim(point_clouds = target_clouds, times = 1000, perturbation_time = 500, number_cross_linked_clusters = 5, number_cross_linked = 30, cross_linked_cluster_radius = 40)
animate_points(simulation = sim7)
gganimate::anim_save(filename = paste(main, "14_Simultaneous_Targets_with_Squeeze_and_Release.gif", sep = ""))
animate_H_from_points(simulation = sim7, ROI = 1000, point_clouds = target_clouds, legendposition = "none")
gganimate::anim_save(filename = paste(main, "15_Simultaneous_Targets_with_Squeeze_and_Release_H.gif", sep = ""))

#We can manually change the step size (speed) of points during the simulation by inputting a vector of D_min and D_max values. We can also provide a perturbation to offset points.
sim8 <- speedripleysim(D_min = c(0, 10), D_max = c(10, 100), point_clouds = target_clouds, times = 1000, perturbation_times = 500, perturbation_strength = 100)
animate_points(simulation = sim7)
gganimate::anim_save(filename = paste(main, "16_Simultaneous_Targets_with_Speed_Change.gif", sep = ""))
animate_H_from_points(simulation = sim7, ROI = 1000, point_clouds = target_clouds, legendposition = "none")
gganimate::anim_save(filename = paste(main, "17_Simultaneous_Targets_with_Speed_Change_H.gif", sep = ""))

#Multiple Populations.
#We can simulate multiple populations representing different molecular species. Each population can be given its own target function.
target_clouds <- list(small_cluster_data, big_cluster_data, very_big_cluster_data)

#Here we simulate them independently of each other.
sim9 <- popripleysimisol(point_clouds = target_clouds)

#When animating, we can specify labels for each population.
animate_populations(simulation = sim9, labels = c("Small Clusters", "Big Clusters", "Very Big Clusters"))
gganimate::anim_save(filename = paste(main, "18_Multiple_Populations_Isolated.gif", sep = ""))

#We can also plot this without labels.
animate_populations(simulation = sim9, legendposition = "none")
gganimate::anim_save(filename = paste(main, "19_Multiple_Populations_Isolated_without_Legend.gif", sep = ""))

#We can also allow different populations to interact. To do this, we must create a point cloud with three columns: x coordinate, y coordinate and population number.
all_points <- rbind(cbind(small_cluster_data, 1), cbind(big_cluster_data, 2), cbind(very_big_cluster_data, 3))
sim10 <- popripleysim(point_cloud = all_points)
animate_populations(simulation = sim10, legendposition = "none")
gganimate::anim_save(filename = paste(main, "20_Multiple_Populations.gif", sep = ""))

#We can induce molecular interactions after the simulation is complete. Here we assume the first point cloud is made of activators, the second is made of inhibitors, and the third is made of agents, which may be acted on by either activators or inhibitors.
sim11 <- population_changer(simulation = sim10, A = c(3, 4), B = c(4, 3), C = c(1, 2), D = c(5, 5))

#When an inactive agent (population 3) comes within 5nm of an activator (population 1), it becomes an active agent. Conversely, when an active agent (population 4) comes within 5nm of an inhibitor (population 2), it becomes an inactive agent.
animate_populations(simulation = sim11, labels = c("Activator", "Inhibitor", "Inactive Agent", "Active Agent"))
gganimate::anim_save(filename = paste(main, "21_Multiple_Populations_with_Interactions.gif", sep = ""))

#We can track changes in a population over time.
changes <- track_changes(simulation = sim11, A = 4)
plotlines(changes, labels = c("time", "number active"))