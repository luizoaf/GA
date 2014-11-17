start.time <- Sys.time()
require("foreach")
source("functions.R")
# sphere_min = -100
# sphere_max = 100
# sphere_by = 1
# rastrigin_min= -5.12
# rastrigin_max = 5.12
# rastrigin_by = 0.01

# PARAMETERS
total_indivíduos = 30
dimensions = 30
# individuos_esfera = define_dimensions(total_indivíduos,dimensions,sphere_min,sphere_max,sphere_by)
# individuos_rastring = define_dimensions(total_indivíduos,dimensions,rastrigin_min,rastrigin_max,rastrigin_by)

iteracoes = 10000
taxa_mutacao= 0.05
taxa_cruzamento = 0.9
elitismo = 0.1

# DATA
# type_function ="sphere"
# data = read.csv("sphere.csv",sep=";")


type_function ="rastrigin"
data = read.csv("rastrigin.csv",sep=";")

execute_algorithm = function(){
  fitness_df = data.frame()
  foreach(i=1:15) %dopar% {
    #   i=1
    print(paste(experiment,"Iteracao: ",i))
    print(fitness_df)
    source("genetic_algorithm.R")
    fitness_df = rbind(fitness_df,result)
    #       experiments[i] = rep(paste(experiment,1),each=10)
  }
  return(fitness_df)
}

# 1 - SELECTION
# type_selection = "roulette"
# type_selection = "elitism"

# 2 - CROSSING
# type_crossing = "one"
# type_crossing = "two"

# 3 - MUTATION
#   type_mutation="uniform" 
# type_mutation ="gaussian"

#  4- OFFSPRING
# type_offspring = "random"
# type_offspring = "tournament"

# Experiment I
experiment = "Experiment I"
type_selection = "elitism" 
type_crossing = "one"
type_mutation="gaussian" 
type_offspring = "tournament"
experiment = paste(experiment,type_selection,type_crossing,type_mutation,type_offspring)
write.table(execute_algorithm(),file="Experimento_1.csv",sep=";",row.names=F)


# Experiment II
experiment = "Experiment II"
type_selection = "roulette" 
type_crossing = "one"
type_mutation="gaussian" 
type_offspring = "tournament"
experiment = paste(experiment,type_selection,type_crossing,type_mutation,type_offspring)
write.table(execute_algorithm(),file="Experimento_2.csv",sep=";",row.names=F)



# Experiment III
experiment = "Experiment III"
type_selection = "elitism" 
type_crossing = "two"
type_mutation="gaussian" 
type_offspring = "tournament"
experiment = paste(experiment,type_selection,type_crossing,type_mutation,type_offspring)
write.table(execute_algorithm(),file="Experimento_3.csv",sep=";",row.names=F)



# Experiment IV
experiment = "Experiment IV"
type_selection = "elitism" 
type_crossing = "one"
type_mutation="uniform" 
type_offspring = "tournament"
experiment = paste(experiment,type_selection,type_crossing,type_mutation,type_offspring)
write.table(execute_algorithm(),file="Experimento_4.csv",sep=";",row.names=F)



# Experiment V
experiment = "Experiment V"
type_selection = "elitism" 
type_crossing = "one"
type_mutation="gaussian" 
type_offspring = "random"
experiment = paste(experiment,type_selection,type_crossing,type_mutation,type_offspring)
write.table(execute_algorithm(),file="Experimento_5.csv",sep=";",row.names=F)

# # 
# # all_fitness = all_fitness[,-1]
# # plot(all_fitness[,1],main="Experiments",type="l",ylab="Fitness",xlab="Iteration",ylim=c(0,max(all_fitness)))
# # lines(all_fitness[,2],col="red")
# # lines(all_fitness[,3],col="blue")
# # lines(all_fitness[,4],col="green")

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken