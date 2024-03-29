CriarDadosRegErro<-function(x,b0,b1, sdErro){
  # Para criar um gr�fico com erros, primeiro vamos criar um conjunto de dados x, e estabelecer uma rela��o linear perfeita com uma vari�vel resposta y.
  y<- b0+b1*x
  
  # Criar uma matriz para receber as respostas
  resposta.completa<-matrix(NA,nrow=length(x),ncol=6)
  colnames(resposta.completa)<-c("x","y","PadronizacaoObservacao","Erros","xReMap","yReMap")
  resposta.completa[,1]<-x
  resposta.completa[,2]<-y
  
  # Como as vari�veis podem apresentar escalas diferentes, criamos um eixo padronizado (onde o dado de menor valor observado receber� o numero zero e o maior recebe o valor de 1, os demais s�o recalculados por regra de 3), para ent�o sortear valores de erro, que sair�o da linha de regress�o linear e tem distribui��o normal ao redor da linha de distribui��o normal. 
  # Como aqui os valores de x e y s�o diretamente proporcionais (porque existe a estrutura nos dados que voc� estabeleceu no modelo), tanto faz escalonar x ou y. Ao colocar os dados de uma vari�vel em escala 0-1, automaticamente a outra vai apresentar a mesma escala. 
  
  # PadronizacaoObservacao
  xMax<-max(x); xMin<-min(x)
  resposta.completa[,3]<- (x-xMin)/(xMax-xMin) # colocando a variavel x em uma escala padronizada onde o valor minimo possivel � zero, e maximo � 1
  
  # Criar erro. Ap�s aplicar a escala, voc� pode sortear valores aleat�rios de erro (com m�dia igual 0, que indica que os erros v�o sair para os dois lados da reta com a mesma probabilidade).
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

       