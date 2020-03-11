#Versão em Português contendo todos os dados de matriz insumo produto do GIC.####
#PORTUGUESE VERSION# There is (will be) a similiar code with a english version.

#Autor: Marcelo R. Tonon
#Data de início: 11/03/2020 (DD/MM/AAAA)
#email: marceloresendetonon@gmail.com

##### A LEITURA DOS 2 PONTOS ABAIXO É OBRIGATÒRIA. ####

#1 -DISCLAIMER / ATENCAO: Os dados aqui não foram feitos por mim. O meu trabalho foi simplesmente o de facilitar organizar estes dados numa base de dados .Rdata. Para acessar o dados originais busque a pasta "data/raw_data" no github do projeto.####

#2 - DISCLAIMER / ATENCAO / IMPORTANTE: É impreterível a citação dos trabalhos originais que estimaram os dados utilizados. A lista "citacao_GIC" retorna a citação bibtex dos dados de cada parte deste código. Dessa forma "citacao_GIC[3]" retornará as entradas bibtex dos dados presentes na parte 3. Por sua vez "citacao_GIC[1]" te dará a referência para o pacote, sendo que esta não pode vir desacompanhada da citação do trabalho original da base de dados utilizada. Mais informações ao longo do código.####



#====PARTE 1: Leitura de pacotes usados no código e estabelecimento de funções=====


#Pacotes requeridos para este processo.
library(purrr)
library(readxl)

#Criando rotulos GIC (vamos chamar de "label" mesmo para deixar mais claro)

label_GIC_91 <- paste0("GIC0", 1:9)
label_GIC_91[10:91] <- paste0("GIC", 10:91)
label_GIC_42 <- label_GIC_91[1:42]

#====PARTE 2: Tabela de Composição do Valor Adicionado por atividades a partir da categorização GIC=====



#====PARTE 3: Matrizes Insumo Produto de: Passoni e Freitas.=====

Usos_Nacional <- read_excel("data/raw_data/MIP_42/MIP_2010_42.xlsx", sheet = "Usos Nacional", range = "C6:AR96", col_names = FALSE) %>% as.data.frame

A_Nacional <- read_excel("data/raw_data/MIP_42/MIP_2010_42.xlsx",  sheet = "An", range = "C6:AR47", col_names = FALSE)  %>%as.data.frame

A_Importado <- read_excel("data/raw_data/MIP_42/MIP_2010_42.xlsx",  sheet = "Am", range = "C6:AR47", col_names = FALSE)  %>%as.data.frame

D_Nacional <- read_excel("data/raw_data/MIP_42/MIP_2010_42.xlsx", sheet = "D", range = "C6:CO47", col_names = FALSE)%>%as.matrix

f_total <- read_excel("data/raw_data/MIP_42/MIP_2010_42.xlsx",                   sheet = "Usos Nacional", range = "AZ6:AZ96", col_names = FALSE)%>%as.matrix


x_total_42 = solve(diag(42)- A_Nacional) %*% (D_Nacional %*% f_total)
matrix_x_total_42 = ((solve(diag(42)- A_Nacional)) %*% diag(as.numeric(D_Nacional %*% f_total))
matrix_imports_42 = as.matrix(A_Importado) %*% diag(as.numeric(D_Nacional %*% f_total))

VA_total_42 = sum((rowSums(diag(42)) - rowSums(t(A_Nacional)) -rowSums(t(A_Importado))) %*% diag(as.numeric(x_total_42)))

sum(t(x_total_42) - rowSums(matrix_x_total_42- diag(as.numeric(D_Nacional %*% f_total))) - rowSums(matrix_imports_42)) - dif)

dif = sum(x_total_42) - 3302840

