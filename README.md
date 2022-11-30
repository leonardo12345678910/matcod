# Algoritmos matemáticos

  #ÁLGEBRA LINEAR/GEOMETRIA ANALÍTICA


    #reflexãoR2

    /*ALGORITMO QUE RECEBE COMO ENTRADAS UM VETOR BIDIMENSIONAL E A IN-
    CLINAÇÃO DE UMA RETA QUE PASSA PELA ORIGEM NO PLANO XY, E RETORNA A 
    REFLEXÃO DO VETOR DE ENTRADA EM TORNO DESSA RETA.*/

    /*Entradas: vetor x, bidimensional e de números reais, e variável 
    float a, que representa a inclinação da reta que passa pela origem*/
    /*Saída: vetor y, bidimensional e de números reais, que é a reflexão 
    de x em torno da reta de inclinação a*/

      INÍCIO
          y <- matrizNula(2,1)
          y(1) <- x(1)*(1-a^2)/(1+a^2) + x(2)*2*a/(1+a^2)
          y(2) <- x(1)*2*a/(1+a^2) - x(2)*(1-a^2)/(1+a^2)

          Se abs(y(1)) < 10^(-6) Então      //correção de erros por arredondamento
              y(1) <- 0
          Se abs(y(2)) < 10^(-6) Então      //correção de erros por arredondamento
              y(2) <- 0
      FIM


    #projeçãoR2

    /*ALGORITMO QUE RECEBE COMO ENTRADAS UM VETOR BIDIMENSIONAL E A IN-
    CLINAÇÃO DE UMA RETA QUE PASSA PELA ORIGEM NO PLANO XY, E RETORNA A 
    PROJEÇÃO DO VETOR DE ENTRADA EM TORNO DESSA RETA.*/

    /*Entradas: vetor x, bidimensional e de números reais, e variável 
    float a, que representa a inclinação da reta que passa pela origem*/
    /*Saída: vetor y, bidimensional e de números reais, que é a projeção 
    de x em torno da reta de inclinação a*/

      INÍCIO
          y <- matrizNula(2,1)
          y(1) <- x(1)/(1+a^2) + x(2)*a/(1+a^2)
          y(2) <- x(1)*a/(1+a^2) + x(2)*a^2/(1+a^2)

          Se abs(y(1)) < 10^(-6) Então      //correção
              y(1) <- 0
          Se abs(y(2)) < 10^(-6) Então      //correção
              y(2) <- 0
      FIM


    #projeçãoR2

    /*ALGORITMO QUE RECEBE COMO ENTRADAS UM VETOR BIDIMENSIONAL E UM ÂN-
    GULO EM RADIANOS, E A ROTAÇÃO DO VETOR DE ENTRADA POR ESSE ÂNGULO.*/

    /*Entradas: vetor x, bidimensional e de números reais, e variável 
    float t, que representa o ângulo de rotação em radianos*/
    /*Saída: vetor y, bidimensional e de números reais, que é a rotação 
    de x em torno do ângulo t*/

      INÍCIO
          y <- matrizNula(2,1)
          y(1) <- x(1)*cos(t) - x(2)*sin(t)
          y(2) <- x(1)*sin(t) + x(2)*cos(t)

          Se abs(y(1)) < 10^(-6) Então      //correção
              y(1) <- 0
          Se abs(y(2)) < 10^(-6) Então      //correção
              y(2) <- 0
      FIM


    #gram_schmidt

    /*ALGORITMO QUE RECEBE UMA MATRIZ QUE POSSUI COMO LINHAS OS VETORES 
    DE UMA BASE DE RN, N NATURAL, E RETORNA OUTRA MATRIZ QUE POSSUI COMO 
    LINHAS OS VETORES DE UMA BASE ORTONORMAL DE RN.*/

    /*Entrada: uma matriz de constantes x, com linhas linearmente inde-
    pendentes*/
    #Saída: uma matriz de constantes y, com linhas ortonormais
    /*Variáveis auxiliares: variáveis inteiras m, que recebe o número de
    linhas da matriz de entrada, e n, que recebe o número de colunas da
    matriz de entrada*/

      INÍCIO
          m <- numeroDeLinhas(x)
          n <- numeroDeColunas(x)

          Se typeof(x(1)) ~= CONSTANT Então
          INÍCIO
              y <- matrizNula(m,n)
          Senão
              y <- x

              De i <- 2 Até m Faça              //cria os vetores dois a dois ortogonais
              INÍCIO
                  y(i,:) <- x(i,:)
                  De j <- 1 Até i-1 Faça
                      y(i,:) <- y(i,:) - ((y(j,:)*x(i,:)')/(y(j,:)*y(j,:)'))*y(j,:)
                  De j <- 1 Até n Faça          //correção
                      Se abs(y(i,j)) < 10^(-6) Então
                          y(i,j) <- 0
              FIM

              De k <- m Até 1 Decremento 1 Faça         //faz a normalização
                  y(k,:) <- y(k,:)/((y(k,:)*y(k,:)')^(1/2))
          FIM

          Retorna(y)
      FIM


    #transforma_eixos

    /*ALGORITMO QUE RECEBE COMO ENTRADA UM PAR ORDENADO x, UM PAR ORDE-
    NADO y, UM ÂNGULO z EM RADIANOS (NÚMERO PURO) E DUAS STRINGS u E v. 
    A SAÍDA É UMA MENSAGEM DE TEXTO, QUE EXIBE OU A COORDENADA DO PONTO 
    x EM RELAÇÃO A UM SISTEMA DE EIXOS ORTOGONAL QUE FOI ROTACIONADO EM 
    TORNO DA ORIGEM PELO ÂNGULO z E TRANSLADADO ATÉ y, OU A COORDENADA 
    DO PONTO x EM RELAÇÃO AO SISTEMA DE EIXOS QUE NÃO SOFREU ROTAÇÃO NEM 
    TRANSLAÇÃO. A STRING u INDICA A ORIENTAÇÃO DO SISTEMA DE EIXOS 
    TRANSFORMADO (MESMA ORIENTAÇÃO QUE O SISTEMA DE EIXOS INICIAL OU NÃO) 
    E A STRING v INDICA SE A COORDENADA DE x EXIBIDA NA TELA SERÁ EM RE-
    LAÇÃO AO SISTEMA DE EIXOS INICIAL (NÃO TRANSFORMADO, v = "invertido") 
    OU AO SISTEMA DE EIXOS FINAL (v = "não_invertido").*/

    /*Entradas: par ordenado x de números reais, par ordenado y de núme-
    ros reais, variável float z que representa o ângulo em radianos, 
    string u e string v*/
    /*Saída: uma mensagem exibindo as coordenadas do ponto x ou em rela-
    ção ao sistema de eixos não transformado (supondo que as coordenadas
    de x sejam dadas em relação ao sistema de eixos transformado), ou 
    em relação ao sistema de eixos transformado (supondo que as coorde-
    nadas de x sejam dadas em relação ao sistema de eixos não transfor-
    mado), ou uma mensagem de erro*/
    #Variáveis auxiliares: w (matriz nula 2x1)

      INÍCIO
          Tente typeof(x(2)) = CONSTANT
          INÍCIO

              Tente typeof(x(3)) = CONSTANT       //só pode ser um par ordenado
              INÍCIO

                  Imprime("Insira um par ordenado como primeiro argumento.")

              Exceção InvalidIndex

                  Se u = "mesma_orientação" Então         //o novo sistema de eixos mantém a mesma orientação do antigo (não transformado)
                  INÍCIO

                      Se v = "invertido" Então            //w(1) e w(2) são calculados em relação ao sistema de referências antigo
                      INÍCIO

                          w <- matrizNula(2,1)
                          w(1) <- x(1)*cos(z)-x(2)*sin(z)+y(1)        //calcula w(1) e w(2)
                          w(2) <- x(1)*sin(z)+x(2)*cos(z)+y(2)
                          Se abs(w(1)) < 10^(-6) Então                //correção de erro por arredondamento
                              w(1) <- 0
                          Se abs(w(2)) < 10^(-6) Então                //correção de erro por arredondamento
                              w(2) <- 0
                          Imprime("Coordenada de x no sistema de eixos inicial: %i %i",w(1),w(2))

                      Senão Se v = "não_invertido" Então          //w(1) e w(2) são calculados em relação ao novo sistema de referências

                          w <- matrizNula(2,1)
                          w(1) <- (x(1)-y(1))*cos(z)+(x(2)-y(2))*sin(z)       //calcula w(1) e w(2)
                          w(2) <- -(x(1)-y(1))*sin(z)+(x(2)-y(2))*cos(z)
                          Se abs(w(1)) < 10^(-6) Então                        //correção
                              w(1) <- 0
                          Se abs(w(2)) < 10^(-6) Então                        //correção
                              w(2) <- 0
                          Imprime("Coordenada de x no sistema de eixos final: %i %i",w(1),w(2))

                      Senão                                       //o quinto argumento só pode ser "invertido" ou "não_invertido"
                          Imprime("Quinto argumento incorreto.")
                      FIM

                  Senão Se u = "orientação_oposta" Então          //o novo sistema de eixos muda de orientação em relação 
                                                                  //ao antigo (como se o antigo fosse girado ao redor do eixo x e depois rotacionado e transladado)

                      Se v = "invertido" Então
                      INÍCIO

                          w <- matrizNula(2,1)
                          w(1) <- x(1)*cos(z)+x(2)*sin(z)+y(1)
                          w(2) <- x(1)*sin(z)-x(2)*cos(z)+y(2)
                          Se abs(w(1)) < 10^(-6) Então
                              w(1) <- 0
                          Se abs(w(2)) < 10^(-6) Então
                              w(2) <- 0
                          Imprime("Coordenada de x no sistema de eixos inicial: %i %i",w(1),w(2))

                      Senão Se v = "não_invertido" Então

                          w <- matrizNula(2,1)
                          w(1) <- (x(1)-y(1))*cos(z)+(x(2)-y(2))*sin(z)
                          w(2) <- (x(1)-y(1))*sin(z)-(x(2)-y(2))*cos(z)
                          Se abs(w(1)) < 10^(-6) Então
                              w(1) <- 0
                          Se abs(w(2)) < 10^(-6) Então
                              w(2) <- 0
                          Imprime("Coordenada de x no sistema de eixos final: %i %i",w(1),w(2))

                      Senão
                          Imprime("Quinto argumento incorreto.")
                      FIM
                  Senão
                      Imprime("Quarto argumento incorreto.")
                  FIM
              FIM
          Exceção InvalidIndex
              Imprime("Insira um par ordenado como primeiro argumento.")
          FIM
      FIM


  #GEOMETRIA ANALÍTICA


    #distânciaPontos

    /*ALGORITMO QUE RECEBE AS COORDENADAS DE DOIS PONTOS DO ESPAÇO, E 
    RETORNA A DISTÂNCIA ENTRE ELES.*/

    #Entradas: vetores tridimensionais x e y de números reais
    #Saída: um float que é a distância entre os pontos de entrada

      INÍCIO
          Tente ((typeof(x(2)) = "constant" Ou typeof(y(2)) = "constant") E (typeof(x(3)) = "constant" Ou typeof(y(3)) = "constant"))
              Retorna(((x(1)-y(1))^2 + (x(2)-y(2))^2 + (x(3)-y(3))^2)^(1/2))
          Exceção "Invalid index"
              Imprime("Insira ternos ordenados de numeros reais apenas.")
      FIM


    #distânciaPontoReta

    /*ALGORITMO QUE RECEBE UM PAR ORDENADO x E DOIS VETORES y E z DE 
    CONSTANTES QUE DEFINEM UMA RETA PARAMETRIZADA (OU SEJA, OS PONTOS DA 
    RETA SÃO DA FORMA (y(1)+z(1)*t,y(2)+z(2)*t,y(3)+z(3)*t), ONDE t É UM 
    PARÂMETRO REAL), E RETORNA A DISTÂNCIA ENTRE O PONTO x E A RETA.*/

    #Entradas: vetores tridimensionais x,y e z de números reais
    /*Saída: uma mensagem que diz a distância entre o ponto e a reta, ou 
    uma mensagem de erro*/
    #Variável auxiliar: variável float w

      INÍCIO
          Tente (typeof(x(2)) = CONSTANT Ou typeof(y(2)) = CONSTANT Ou typeof(z(2)) = CONSTANT) E (typeof(x(3)) = CONSTANT Ou typeof(y(3)) = CONSTANT Ou typeof(z(3)) = CONSTANT)
              Se abs(y(1)-x(1)) < 10^(-6) Ou abs(y(2)-x(2)) < 10^(-6) Ou abs(y(3)-x(3)) < 10^(-6) Então
              INÍCIO

                  v <- zeros(1,3)

                  Se abs(y(1)-x(1)) < 10^(-6) Então
                      v(1) <- 1
                  Senão Se abs(y(2)-x(2)) < 10^(-6) Então
                      v(2) <- 1
                  Senão
                      v(3) <- 1
                  Fim

                  Se v(1) = 0 Então
                      d <- z(1)*(y(1)-x(1))
                  Se v(2) = 0 Então
                      d <- d + z(2)*(y(2)-x(2))
                  Se v(3) = 0 Então
                      d <- d + z(3)*(y(3)-x(3))
                  d <- d/(z(1)^2+z(2)^2+z(3)^2)
              Senão
                  d <- (z(1)*(y(1)-x(1))+z(2)*(y(2)-x(2))+z(3)*(y(3)-x(3)))/(z(1)^2+z(2)^2+z(3)^2)
              FIM

              Se abs(d) < 10^(-6) Então
              INÍCIO
                  w <- ((x(1)-y(1))^2 + (x(2)-y(2))^2 + (x(3)-y(3))^2)^(1/2)
                  Imprime(w)
              Senão
                  w <- ((x(1)-y(1)-z(1)*(-d))^2 + (x(2)-y(2)-z(2)*(-d))^2 + (x(3)-y(3)-z(3)*(-d))^2)^(1/2)
                  Imprime(w)
              FIM
          Exceção InvalidIndex
              Imprime("Insira ternos ordenados de numeros reais apenas.")
          FIM
      FIM
