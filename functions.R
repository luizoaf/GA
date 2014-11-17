require(compare)

elements_type = function(type,number_elements){
  if(type == "sphere"){
    min = -100
    max = 100
    by = 1
  }
  if (type == "rastrigin"){
    min = -5.12
    max = 5.12
    by = 0.01
  }
  return(sample(seq(min,max,by=by),size=number_elements,replace=T))
}

# valores que nao estao dentro do espaco de busca eh colocado no maximo ou minimo
corrects_elements_type_gaussian = function(type,value){
  if(type == "sphere"){
    min = -100
    max = 100
  }
  if (type == "rastrigin"){
    min = -5.12
    max = 5.12
  }
  if(value > max){
    value = max
  }
  if(value < min){
    value = min
  }
  return(value)
}


define_dimensions = function(total_indivíduos,dimensions,limite_inferior,limite_superior,incremento){
  individuos = data.frame(1)
  for(i in 1:dimensions){
    gera_valores_aleatorios_entre_o_intervalo = as.data.frame(sample(seq(limite_inferior,limite_superior,by=incremento),dimensions))
    individuos = cbind(individuos,gera_valores_aleatorios_entre_o_intervalo)
  }
  individuos = individuos[,-1]
  colnames(individuos) = c(paste("x_",1:dimensions,sep=""))
  return(individuos)
}

fitness = function(data,type){
  if(type == "sphere"){
    type_function = function(vector){
      return(sum(vector^2))
    }
  }
  if (type == "rastrigin"){
    type_function = function(vector){
      elements_number <- length(vector)
      sum <- sum(vector^2 - 10*cos(2*pi*vector))
      result <- 10*elements_number + sum
      return(result)
    }
  }
  return(apply(data,MARGIN=1,FUN=type_function))
}

selection_roulette = function(data,elements_number_for_next_iteration){
  data_roulette = data.frame()
  sum_fitness = sum(data$fitness)
  probabilities = (data$fitness/sum_fitness)
  probabilities_inverse = sum(1/probabilities)
  probabilities = (1/probabilities)/probabilities_inverse 
  probabilities_cum_sum = cumsum(probabilities)
  
  indexs = sample(length(probabilities),prob=probabilities,size=3,replace=F)
  data_roulette = rbind(data_roulette,data[indexs,])
  
  return(data_roulette)
}


selection_elitism = function(data,elements_number_for_next_iteration){
  data_elitism = data[order(data$fitness,decreasing=F)[1:elements_number_for_next_iteration],]
  return(data_elitism)
}

crossing_one_point = function(data,type,new_pairs_of_individuals){
  one_point_crossing_sons = data.frame()
  i = 1
  while(i<=new_pairs_of_individuals){
    random_number = runif(1,min = 0, max = 1)
    if(random_number <= taxa_cruzamento){
      
      point_crossing = sample(1:dimensions,size=2,replace=F)
      point_crossing = point_crossing[order(point_crossing,decreasing=F)]
      elements_for_crossing = sample(1:nrow(data),size=2,replace=F)
      vector_1 = 1:point_crossing[1]
      vector_2 = (point_crossing[1]+1):(ncol(data)-1) # -1 = remove col fitness
      
      parent_1_part_1 = as.vector(data[elements_for_crossing[1],][vector_1])
      parent_2_part_1 = as.vector(data[elements_for_crossing[2],][vector_2])
      
      parent_1_part_2 = as.vector(data[elements_for_crossing[1],][vector_2])
      parent_2_part_2 = as.vector(data[elements_for_crossing[2],][vector_1])
      
      son_1 = cbind(parent_1_part_1,parent_2_part_1)
      son_1$fitness = fitness(son_1,type)
      son_2 =  cbind(parent_1_part_2,parent_2_part_2)
      son_2$fitness = fitness(son_2,type)
      one_point_crossing_sons = rbind(one_point_crossing_sons,rbind(son_1,son_2))
      i = i + 1
    }
  }
  return(one_point_crossing_sons)
}

crossing_two_points = function(data,type,new_pairs_of_individuals){
  one_point_crossing_sons = data.frame()
  i = 1
  while(i<=new_pairs_of_individuals){
    random_number = runif(1,min = 0, max = 1)
    
    if(random_number <= taxa_cruzamento){
      
      point_crossing = sample(1:dimensions,size=3,replace=F)
      point_crossing = point_crossing[order(point_crossing,decreasing=F)]
      elements_for_crossing = sample(1:nrow(data),size=2,replace=F)
      
      vector_1 = 1:point_crossing[1]
      vector_2 = (point_crossing[1]+1):point_crossing[2]
      vector_3 = (point_crossing[2]+1):(ncol(data)-1) # -1 = remove col fitness 
      
      parent_1_part_1 = as.vector(data[elements_for_crossing[1],][vector_1])
      parent_2_part_1 = as.vector(data[elements_for_crossing[2],][vector_2])
      parent_3_part_1 = as.vector(data[elements_for_crossing[1],][vector_3])
      
      parent_1_part_2 = as.vector(data[elements_for_crossing[2],][vector_1])
      parent_2_part_2 = as.vector(data[elements_for_crossing[1],][vector_2])
      parent_3_part_2 = as.vector(data[elements_for_crossing[2],][vector_3])
      
      son_1 = cbind(parent_1_part_1,parent_2_part_1,parent_3_part_1)
      son_1$fitness = fitness(son_1,type)
      son_2 =  cbind(parent_1_part_2,parent_2_part_2,parent_3_part_2)
      son_2$fitness = fitness(son_2,type)
      one_point_crossing_sons = rbind(one_point_crossing_sons,rbind(son_1,son_2))
      i = i + 1
    }
  }
  return(one_point_crossing_sons)
}

mutation = function(data,type_function,type_mutation,taxa_mutacao){
  new_individual_mutation = data.frame()
  for(i_individual in 1: nrow(data)){
    random_number = runif(1,min = 0, max = 1)
    
    if(random_number <= taxa_mutacao){
      
      individual = data[i_individual,1:dimensions] 
      changed_dimensions = as.integer(runif(1,min = 1, max = dimensions))
      indexes = sample(1:dimensions,changed_dimensions,replace=F) 
      
      #uniform
      if(type_mutation == "uniform"){
        new_elements = elements_type(type_function,changed_dimensions)
      }
      
      # gaussian
      if(type_mutation == "gaussian"){
        new_elementos_norm = rnorm(changed_dimensions,as.vector(apply(individual,MARGIN=1,FUN=mean)),as.vector(apply(individual,MARGIN=1,FUN=sd)))
        if(type_function=="sphere"){
          new_elements = as.numeric(format(round(new_elementos_norm, 0)))
        }
        if(type_function=="rastrigin"){
          new_elements = as.numeric(format(round(new_elementos_norm, 2)))
        }
      }
      for(i in 1:length(new_elements)){
        new_elements[i] = corrects_elements_type_gaussian(type=type_function,value=new_elements[i])
      }
      new_individual = individual
      new_individual[indexes] = new_elements
      new_individual = cbind(new_individual,data.frame(fitness=fitness(as.data.frame(new_individual),type_function)))
      new_individual_mutation = rbind(new_individual_mutation,new_individual)
    }
  }
  if(nrow(as.data.frame(new_individual_mutation))!=0){
    new_individual_mutation = rbind(data,new_individual_mutation)
    return(new_individual_mutation)
  }else{
    return(data)
  }
}


tournament_offspring = function(data, iterations){
#   better_individuals = data.frame()
#   for(i in 1:iterations){
#     two_individuals_random = sample(seq(1,nrow(data),by=1),size=2,replace=F)
#     index_better_individuals = two_individuals_random[which.min(data$fitness[two_individuals_random])]
#     better_individuals = rbind(better_individuals,data[index_better_individuals,])
#     data = data[-index_better_individuals,]
#   }
  better_individuals = data.frame()
  indexs = sample(seq(1,nrow(data),by=1),size=iterations,replace=F)
  fitness = data.frame(matrix(indexs,nrow=iterations/2,ncol=2))
  fitness$col1 = data$fitness[fitness[,1]]
  fitness$col2 = data$fitness[fitness[,2]]
  better_individuals = data[ifelse((fitness$col1 - fitness$col2) > 0,fitness[,2],fitness[,1]),]
  return(better_individuals)
}

random_offspring = function(data, iterations){
  index_one_individual_random = sample(seq(1,nrow(data),by=1),size=iterations,replace=F)
  random_individuals = rbind(random_individuals,data[index_one_individual_random,])
  return(data[index_one_individual_random,])
}
