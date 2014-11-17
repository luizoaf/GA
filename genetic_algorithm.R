# i =1
old_data = data
new_data = data.frame()
better_fitness_vector = c()
for(i in 1:iteracoes){
  # 1 - SELECTION
  if(type_selection == "roulette"){
    new_data = selection_roulette(old_data,3)
  }
  if (type_selection == "elitism"){
    new_data = selection_elitism(old_data,elitismo*nrow(data))
  }
  
  # 2 - CROSSING
  if(type_crossing == "one"){
    new_data =  rbind(new_data,crossing_one_point(data=new_data,type=type_function,new_pairs_of_individuals=14))
#     new_data[order(new_data$fitnes,decreasing=T),]
  }
  
  if (type_crossing == "two"){
    new_data =   rbind(new_data,crossing_two_points(data=new_data,type=type_function,new_pairs_of_individuals=14))
  }
  
  # 3 - MUTATION
  if(type_mutation == "uniform" || type_mutation == "gaussian"){
    new_data =  (mutation(new_data,type_function,type_mutation,taxa_mutacao))
  }
  #  4- OFFSPRING
  if(type_offspring == "tournament"){
    new_data = tournament_offspring(data=new_data,iterations=30)
  }
  
  if (type_offspring == "random"){
    new_data = random_offspring(data=new_data,iterations=30)
  }
  
  better_fitness_vector[i] = new_data$fitness[which.min(new_data$fitness)]
  row.names(new_data) = 1:nrow(new_data)
  old_data = new_data
  plot(main=paste("Iteration: ",i,"Fitness: ",better_fitness_vector[length(better_fitness_vector)]),better_fitness_vector,type="l",ylab="Fitness",xlab="Iteration")
  print(new_data)
  #   print(new_data[which.min(new_data$fitness)])
  if(better_fitness_vector[i]==0){
    break
  }
}

# configurations = paste("SELECTION:",type_selection,
#                        "CROSSING:",type_crossing,
#                        "MUTATION:",type_mutation ,
#                        "OFFSPRING:",type_offspring)

result = data.frame(experiment = experiment,fitness = better_fitness_vector[which.min(better_fitness_vector)])
# colnames(result) = experiment

# write.table(result,file=paste("configurations",".csv",sep=""))