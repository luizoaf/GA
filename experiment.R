source("functions.R")
sphere_min = -100
sphere_max = 100
sphere_by = 1
rastrigin_min= -5.12
rastrigin_max = 5.12
rastrigin_by = 0.01

# PARAMETERS
total_indivíduos = 30
dimensions = 30
individuos_esfera = define_dimensions(total_indivíduos,dimensions,sphere_min,sphere_max,sphere_by)
individuos_rastring = define_dimensions(total_indivíduos,dimensions,rastrigin_min,rastrigin_max,rastrigin_by)

iteracoes = 5
taxa_mutacao= 0.05
taxa_cruzamento = 0.9
elitismo = 0.1


# DATA
# data = individuos_esfera
# type_function ="sphere"
# data$fitness = fitness(data,type=type_function)
# # write.table(data,"sphere.csv",sep=";",row.names=F)

data = read.csv("sphere.csv",sep=";")
# data = individuos_rastring

type_function ="sphere"
# type_function ="rastrigin"

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
all_fitness = data.frame()
execute_algorithm = function(){
  fitness_df = data.frame()
  for(i in 1:2){
    source("genetic_algorithm.R")
    fitness_df = rbind(fitness_df,result)
  }
  return(fitness_df)
  
}

# Experiment I
experiment = "Experiment I"
type_selection = "elitism"
type_crossing = "one"
type_mutation="gaussian" 
type_offspring = "tournament"
all_fitness = rbind(all_fitness,execute_algorithm())

# Experiment II
experiment = "Experiment II"

type_selection = "roulette"
type_crossing = "one"
type_mutation="gaussian" 
type_offspring = "tournament"
all_fitness = rbind(all_fitness,execute_algorithm())


# Experiment III
experiment = "Experiment III"
type_selection = "elitism"
type_crossing = "two"
type_mutation="gaussian" 
type_offspring = "tournament"


all_fitness = rbind(all_fitness,execute_algorithm())

# Experiment IV
experiment = "Experiment IV"
type_selection = "elitism"
type_crossing = "one"
type_mutation="uniform" 
type_offspring = "tournament"

all_fitness = rbind(all_fitness,execute_algorithm())

# Experiment IV
experiment = "Experiment V"
type_selection = "elitism"
type_crossing = "one"
type_mutation="gaussian" 
type_offspring = "random"
all_fitness = rbind(all_fitness,execute_algorithm())

# 
# all_fitness = all_fitness[,-1]
# plot(all_fitness[,1],main="Experiments",type="l",ylab="Fitness",xlab="Iteration",ylim=c(0,max(all_fitness)))
# lines(all_fitness[,2],col="red")
# lines(all_fitness[,3],col="blue")
# lines(all_fitness[,4],col="green")

