#install.packages("neuralnet")
#library(neuralnet)
########################################################################################
normalize = function(x){
  return((x-min(x))/(max(x)-min(x)))
}

vector_hidden = function(gen){
  nodosxcapa = c()
  for (i in 1:gen[1]){
    nodosxcapa[i] = gen[2]
  }
  nodosxcapa
}

# Mayores parámetros tomarían mucho tiempo
min_capas = 1
max_capas = 3
min_nodos = 1
max_nodos = 5

generar = function(){
  gen = c()
  num_capas = sample(min_capas:max_capas,1)
  num_nodos = sample(min_nodos:max_nodos,1)
  gen = c(num_capas,num_nodos,runif(1))
  #for (i in 1:num_capas){
  #  gen[i] = sample(min_nodos:max_nodos,1)
  #}
  #gen[(num_capas+1)] = runif(1)
  gen
}

fitness = function(gen){
  print("Calculando fitness")
  neural.net = neuralnet(Strength~Cement+Blast+FlyAsh+Water+Superplastic+CoarseAgg+
                          FineAgg+Age,data=concrete_train, hidden=vector_hidden(gen),learningrate = gen[3])
  #plot(neural.net)
  prediction = compute(neural.net,concrete_test[1:8])$net.result
  prediction
  
  ftns = cor(prediction,concrete_test$Strength)
  ftns
}

reproducir = function(pareja){
  padre = lista_reproduccion[[pareja[1]]]
  madre = lista_reproduccion[[pareja[2]]]
  capas = c(padre[1],madre[1])
  nodos = c(padre[2],madre[2])
  alfas = c(padre[3],madre[3])
  hijo = c(round(mean(capas)),
           round(mean(nodos)),
           mean(alfas))
  hijo
}

mutar = function(gen){
  k = sample(1:3,1)
  if (k == 1){
    gen[k] = sample(min_capas:max_capas,1)
  } else if (k == 2){
    gen[k] = sample(min_nodos:max_nodos,1)
  } else if (k == 3){
    gen[k] = runif(1)
  }
  gen
}
########################################################################################
########################################################################################
# Mayores parámetros tomarían mucho tiempo
cant_poblacion = 3
iteraciones = 2
porc_mutacion = 0.1

# Carga de datos
concrete = read.csv("Concrete_Data.csv")
concrete_norm = as.data.frame(lapply(concrete,normalize))
concrete_train = concrete_norm[1:824,]
concrete_test = concrete_norm[825:1030,]

# Creación de población inicial y cálculo de fitness
poblacion = list()
fitness_results = c()
total_fitness = 0
for (i in 1:cant_poblacion){
  poblacion[[i]] = generar()
  ftns = fitness(poblacion[[i]])
  total_fitness = total_fitness + ftns
  fitness_results[i] = ftns
}
for (g in 1:iteraciones){
  lista_reproduccion = list()
  # Creación de lista de reproducción
  for (i in 1:cant_poblacion){
    porc = fitness_results[i]/total_fitness
    n = round(porc*cant_poblacion)
    for (j in 1:n){
      lista_reproduccion[[length(lista_reproduccion)+1]] = poblacion[[i]]
    }
  }
  # Reproducción
  total_fitness = 0
  for (i in 1:cant_poblacion){
    pareja = sample(1:length(lista_reproduccion),2)
    hijo = reproducir(pareja)
    # Mutación
    if (runif(1) <= porc_mutacion){
      hijo = mutar(hijo)
    }
    poblacion[[i]] = hijo
    # Cálculo de fitness
    ftns = fitness(hijo)
    total_fitness = total_fitness + ftns
    fitness_results[i] = ftns
  }
}
