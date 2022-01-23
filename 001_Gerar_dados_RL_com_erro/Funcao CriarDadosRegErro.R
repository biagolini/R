CriarDadosRegErro<-function(x,b0,b1, sdErro){
  # Para criar um gráfico com erros, primeiro vamos criar um conjunto de dados x, e estabelecer uma relação linear perfeita com uma variável resposta y.
  y<- b0+b1*x
  
  # Criar uma matriz para receber as respostas
  resposta.completa<-matrix(NA,nrow=length(x),ncol=6)
  colnames(resposta.completa)<-c("x","y","PadronizacaoObservacao","Erros","xReMap","yReMap")
  resposta.completa[,1]<-x
  resposta.completa[,2]<-y
  
  # Como as variáveis podem apresentar escalas diferentes, criamos um eixo padronizado (onde o dado de menor valor observado receberá o numero zero e o maior recebe o valor de 1, os demais são recalculados por regra de 3), para então sortear valores de erro, que sairão da linha de regressão linear e tem distribuição normal ao redor da linha de distribuição normal. 
  # Como aqui os valores de x e y são diretamente proporcionais (porque existe a estrutura nos dados que você estabeleceu no modelo), tanto faz escalonar x ou y. Ao colocar os dados de uma variável em escala 0-1, automaticamente a outra vai apresentar a mesma escala. 
  
  # PadronizacaoObservacao
  xMax<-max(x); xMin<-min(x)
  resposta.completa[,3]<- (x-xMin)/(xMax-xMin) # colocando a variavel x em uma escala padronizada onde o valor minimo possivel é zero, e maximo é 1
  
  # Criar erro. Após aplicar a escala, você pode sortear valores aleatórios de erro (com média igual 0, que indica que os erros vão sair para os dois lados da reta com a mesma probabilidade).
  resposta.completa[,4]<-rnorm(length(x),0,sdErro)

  # Calcular quais sao os valores de x e y correspondentes ao erro
  resposta.completa[,5]= (sqrt(2)/2)*resposta.completa[,4]+resposta.completa[,1] 
  resposta.completa[,6]=resposta.completa[,2] -(sqrt(2)/2)*resposta.completa[,4]
  
  return(resposta.completa[,5:6])}


###### Aplicar funcao para testar
b0=20
b1=0.5
sdErro=0.5
x<-rnorm(100,50,1)
x<-1:100
y<- b0+b1*x
plot(y~x)

dados<-CriarDadosRegErro(x,b0, b1, sdErro=5)
plot(dados[,2]~dados[,1])
abline(lm(y~x))

       