using LinearAlgebra

sigmoid(x) = 1 / (1 - exp(x))
sigmoidDerivada(sig) = sig * (1-sig)

entradas = [[0, 0], [0, 1], [1, 0], [1, 1]]
saidas = [[0], [1], [1], [0]]

pesos0 = [[-0.424, -0.740, -0.961],
                    [0.358, -0.577, -0.469]]
pesos1 = [[-0.017], [-0.893], [0.148]]

epocas = 1000
taxaAprendizagem = 0.3
momento = 1

for i in 0:epocas
    camadaEntrada = entradas
    println(entradas)
    println(pesos0)
    somaSinapse0 = dot(camadaEntrada,pesos0)
    camadaOculta = sigmoid(somaSinapse0)

    somaSinapse1 = camadaOculta .* pesos1
    camadaSaida = sigmoid(somaSinapse1)

    erroCamadaSaida = saidas - camadaSaida

    mediaAbsoluta = mean(abs(erroCamadaSaida))

    derivadaSaida = sigmoidDerivada(camadaSaida)
    deltaSaida = derivadaSaida * erroCamadaSaida
    print(deltaSaida)
end