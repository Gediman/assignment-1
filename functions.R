
# Sum values in a column of a data frame.
#
# ARGUMENTS:
# d: a data frame or tibble
# var: the name of a column of d, provided as a string
#
# RETURN VALUE:
# if the specified column exists and contains numbers, returns the sum of
# all values in the column; otherwise, returns NULL
sum_column <- function(d, var) {
  # Set a default value to return
  result <- NULL
  x <- d[[var]] # Remember, as we showed in class: there are two ways
  # to access a column, and this is one; you should try
  # and figure out why the other way won't work here,
  # but that's not part of the homework
  if (!is.null(x)) { # This tests to see whether the column exists in
    # d; again, you should try and find a way to test
    # this out for yourself to verify that it's true,
    # but that's not part of the homework
    # YOUR CODE HERE: if x contains numbers, set the variable
    # result to be the sum of the values in x
    
    if(is.numeric(x)){
      result <- sum(x)
    }
  }
    # You will need to do a bit of research in order to figure out how
    # to test whether a vector contains numbers.
    
  

  # YOUR CODE HERE: return the result
  return(result)
}

# Sum values in a vector.
#
# ARGUMENTS:
# x: a vector
#
# RETURN VALUE:
# if the vector contains numbers, returns the sum of
# all values; otherwise, returns NULL
#
# [YOUR FUNCTION HERE]
my_sum <- function(x) {
  #Initialisation de ma variable à 0 (et non à null car l'on ne pourrait pas additionner après)
  result <- 0
  #Initialisation d'un booléen nous permettant de savoir si l'on a effectué une addition, 
  #nous permettant de renvoyer NULL si ce n'est pas le cas.
  not_null <- F
  #On vérifie si le paramètre n'est pas null, sinon, on aurait une erreur sur la boucle
  if (!is.null(x)) {
    #On boucle sur les valeurs contenu dans le paramètre
    for(number in x){
      #On vérifie si la valeur courante est un nombre
      if(is.numeric(number)){
        #Si c'est le cas, on l'additionne au résultat final.
        result <- result + number
        not_null <- T
        #Si c'est la première fois qu'on a additionné quelque chose
        if (not_null == F){
          #On change la valeur du résultat pour ne pas renvoyer null
          not_null <- T
        }
      }
    }
  }
  #Si on a effectué une addition
  if(not_null == T){
    #On renvoie le résultat de celle-ci
    return(result)
  #Sinon, on renvoie NULL
  }else{
    #On aurait pu initialisé la variable result à 0 et la renvoyer dans tous les cas, 
    #mais on aurait alors pas différencié si la somme du vecteur vallait 0 (si il ne contenait que des
    #0 ou bien si les positifs et les négatifs s'annulaient) d'un vecteur vide ou ne contenant pas de
    #valeurs numériques.
    return(NULL)
  }
}

# Sum values in a vector and divided it by a number.
#
# ARGUMENTS:
# x: a vector
# k: a number (a vector of length one)  not equal to 0.
#
# RETURN VALUE:
# if the vector contains numbers and k is a number, returns the sum of
# all values x, then divided by k; otherwise, returns NULL
#
# [YOUR FUNCTION HERE]
sum_divided_by <- function(x, k) {
  #On applique la fonction my_sum() sur le vecteur x. (La fonction gère déjà les cas limites à savoir,
  #si l'argument est nul ou ne contient pas d'entiers.)
  result <- my_sum(x)
  #Si ni le diviseur, ni le dividende ne sont nuls.
  if(!is.null(k) && !is.null(result)){
    #Si le diviseur est un nombre, qui n'est pas zéro. 
    #Puisque le dividende n'est pas nul, il est forcément numérique - d'après la fonction my_sum.
    if(is.numeric(k) && k != 0){
      #On renvoie la somme du vecteur divisé par k.
      return(result/k)
    }
  }
  #Si toutes les conditions ne sont pas remplies, on renvoie NULL.
  return(NULL)
}


# Calculed the mean of a vector : sum values in a vector and divided it by its length.
#
# ARGUMENTS:
# x: a vector
#
# RETURN VALUE:
# if the vector contains numbers, returns its mean; otherwise, returns NULL
#
# [YOUR FUNCTION HERE]
my_mean <- function(x) {
  #Renvoie la somme des éléments du vecteurs divisés par sa taille. 
  #Les éventuels cas limites sont gérés par les fonctions en amont.
  return(sum_divided_by(x, length(x)))
}

# Return a violin plot.
#
# ARGUMENTS:
# d: a data frame or tibble
# var: the name of a column of d containing the dependent variable, provided as a
# string
# grouping_var: the name of a column of d containing a grouping variable,
# provided as a string
#
# RETURN VALUE:
# A ggplot plot object containing a violin plot, grouped by the values
# of the grouping variable.
#
grouped_violin_plot <- function(d, var, grouping_var) {
  # Create the base ggplot object
  p <- ggplot2::ggplot(d, ggplot2::aes_string(y=var,
                                              x=grouping_var,
                                              fill=grouping_var))
  # YOUR CODE HERE: Create a violin plot
  p <- p + ggplot2::geom_violin()
  return(p)
}

# Difference in the medians between two groups.
#
# ARGUMENTS:
# d: a data frame or tibble
# var: the name of a column of d containing the dependent variable, provided as a string
# grouping_var: the name of a column of d containing a grouping variable, provided as a string
# group1: the value of grouping_var that corresponds to the first group
# group2: the value of grouping_var that corresponds to the second group
#
# RETURN VALUE:
# The median value of var for the first group, minus the median value of var for the second
# group.
#
difference_in_medians <- function(d, var, grouping_var, group1, group2) {
  d_1 <- dplyr::filter(d, get(grouping_var) == group1)
  d_2 <- dplyr::filter(d, get(grouping_var) == group2)
  # YOUR CODE HERE: assign the difference in the medians to to the variable 'result'
  result <- median(d_1[[var]])-median(d_2[[var]])
  return(result)
}

# Randomize the order of a column.
#
# ARGUMENTS:
# d: a data frame or tibble
# var: the name of a column of d containing the variable to randomize,
# provided as a string
#
# RETURN VALUE:
# A data frame or tibble exactly the same as d, except with the order of
# var permuted randomly.
#
randomize <- function(d, var) {
  d[[var]] <- sample(d[[var]], length(d[[var]]))# YOUR CODE HERE: generate a shuffled version of d[[var]]
    return(d)
}


# Perform a permutation test for two groups.
#
# ARGUMENTS:
# d: a data frame or tibble
# var: the name of the column in d on which the test statistic will be calculated,
# provided as a string
# grouping_var: the name of the column in d which gives the grouping
# group1: the value of grouping_var corresponding to the first group
# group2: the value of grouping_var corresponding to the second group
# statistic: a function yielding a test statistic, which takes as input
# a data frame, the name of a variable on which to calculate the
# test statistic, the name of a grouping variable, the value of
# the grouping variable corresponding to the first group, and
# the value of the grouping variable corresponding to the second
# group
# n_samples: the number of permutation samples to draw (default: 9999)
#
# RETURN VALUE:
#
# A list containing two elements:
#
# - observed: the value of statistic() in d
# - permuted: a vector containing the values of statistic() under n_samples
# permutations
#
permutation_twogroups <- function(d, var, grouping_var, group1, group2, statistic,
                                  n_samples=9999) {
  observed_statistic <- statistic(d, var, grouping_var, group1, group2)
  permutation_statistics <- rep(0, n_samples)
  for (i in 1:n_samples) {
    # YOUR CODE HERE: use randomize(...) to create a permutation and then
    # fill in the vector permutation_statistics with the
    # value of statistic(...) for this new permutation
    random_data <- randomize(d,var)
    permutation_statistics[i] <- statistic(random_data, var, grouping_var, group1, group2)
  }
  result <- list(observed=observed_statistic,
                 permuted=permutation_statistics)
  return(result)
}

#####Same fonction as permutation_tow groups but permutated on grouping_var
# Perform a permutation test for two groups.
#
# ARGUMENTS:
# d: a data frame or tibble
# var: the name of the column in d on which the test statistic will be calculated,
# provided as a string
# grouping_var: the name of the column in d which gives the grouping
# group1: the value of grouping_var corresponding to the first group
# group2: the value of grouping_var corresponding to the second group
# statistic: a function yielding a test statistic, which takes as input
# a data frame, the name of a variable on which to calculate the
# test statistic, the name of a grouping variable, the value of
# the grouping variable corresponding to the first group, and
# the value of the grouping variable corresponding to the second
# group
# n_samples: the number of permutation samples to draw (default: 9999)
#
# RETURN VALUE:
#
# A list containing two elements:
#
# - observed: the value of statistic() in d
# - permuted: a vector containing the values of statistic() under n_samples
# permutations
#
permutation_grouping <- function(d, var, grouping_var, group1, group2, statistic,
                                  n_samples=9999) {
  observed_statistic <- statistic(d, var, grouping_var, group1, group2)
  permutation_statistics <- rep(0, n_samples)
  for (i in 1:n_samples) {
    # YOUR CODE HERE: use randomize(...) to create a permutation and then
    # fill in the vector permutation_statistics with the
    # value of statistic(...) for this new permutation
    random_data <- randomize(d, grouping_var)
    permutation_statistics[i] <- statistic(random_data, var, grouping_var, group1, group2)
  }
  result <- list(observed=observed_statistic,
                 permuted=permutation_statistics)
  return(result)
}






