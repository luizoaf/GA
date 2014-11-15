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

iteracoes = 1000
taxa_mutacao= 0.05
taxa_cruzamento = 0.9
elitismo = 0.1


# DATA
data = individuos_esfera
type_function ="sphere"
data$fitness = fitness(data,type=type_function)
# data = individuos_rastring

# type_function ="sphere"
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
all_fitness = data.frame(1)



# Experiment I
experiment = "Experiment I"
type_selection = "elitism"
type_crossing = "one"
type_mutation="gaussian" 
type_offspring = "tournament"

source("genetic_algorithm.R")

all_fitness = cbind(all_fitness,result)

# Experiment II
experiment = "Experiment II"
type_selection = "elitism"
type_crossing = "one"
type_mutation="uniform" 
type_offspring = "random"

source("genetic_algorithm.R")
all_fitness = cbind(all_fitness,result)



# Experiment III
experiment = "Experiment III"
type_selection = "roulette"
type_crossing = "two"
type_mutation="gaussian" 
type_offspring = "tournament"

source("genetic_algorithm.R")

all_fitness = cbind(all_fitness,result)

# Experiment IV
experiment = "Experiment IV"
type_selection = "elitism"
type_crossing = "two"
type_mutation="gaussian" 
type_offspring = "tournament"

source("genetic_algorithm.R")
all_fitness = cbind(all_fitness,result)


all_fitness = all_fitness[,-1]
plot(all_fitness[,1],main="Experiments",type="l",ylab="Fitness",xlab="Iteration",ylim=c(0,max(all_fitness)))
lines(all_fitness[,2],col="red")
lines(all_fitness[,3],col="blue")
lines(all_fitness[,4],col="green")

