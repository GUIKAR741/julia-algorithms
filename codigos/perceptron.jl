entrada = [[0, 0], [1, 0], [0, 1], [1, 1]]
saida = [0, 1, 1, 1]
pesos = [0.0, 0.0]
taxaAprendizagem = 0.1
function calculaSaida(x::Array)
    if sum(x.*pesos) >= 1
        return 1
    else
        return 0
    end
end
function treinar()
    erroTotal=1
    while erroTotal!=0
        erroTotal=0
        for i in 1:length(saida)
            saidaCalculada=calculaSaida(entrada[i])
            erro=abs(saida[i]-saidaCalculada)
            erroTotal+=erro
            for j in 1:length(pesos)
                pesos[j]+=(taxaAprendizagem*entrada[i][j]*erro)
            end
        end
    end
end

treinar()

println(calculaSaida(entrada[1]))
println(calculaSaida(entrada[2]))
println(calculaSaida(entrada[3]))
println(calculaSaida(entrada[4]))