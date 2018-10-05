
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







