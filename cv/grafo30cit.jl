using Random
mutable struct GeradorGrafo
    n::Int64
    grafo::Array
    custos::Dict
    geradorGrafo::Function
    mostraGrafo::Function
    pcvRandom::Function
    pcvGenetico::Function
    distancia::Function
    function GeradorGrafo(n)
        self=new()
        self.n=n
        self.grafo=[[] for i=1:n]
        self.custos=Dict()
        self.distancia=(x1,y1,x2,y2)->(√((x1-x2)^2+(y1-y2)^2))
        self.geradorGrafo=function (Array::Array)
            for i = 1:n
                for j = 1:n
                    if i!=j
                        if !((i,j) in keys(self.custos))
                            # custo=rand(1:100)
                            # println(i," ",j, " ", self.distancia(Array[i][1],Array[i][2],Array[i][1],Array[j][2]))
                            self.custos[(i,j)]=self.distancia(Array[i][1],Array[i][2],Array[i][1],Array[j][2])
                            self.custos[(j,i)]=self.distancia(Array[i][1],Array[i][2],Array[i][1],Array[j][2])
                        end
                        append!(self.grafo[i],[j])
                    end
                end
            end
        end
        self.mostraGrafo=function()
            for i = 1:n
                print("Adjacentes de ",i,":")
                for j in self.grafo[i]
                    print("(Custo ",self.custos[i,j],") -> ",j," -> ")
                end
                println()
            end
        end
        self.pcvRandom=function(iteracoes)
            melhorCircuito=[]
            melhorCusto=0
            function gerarVertices(melhorCircuito, melhorCusto)
                vertices=[i for i = 2:n]
                circuito=[1]
                custo_circuito=0
                while length(vertices)>0
                    shuffle!(vertices)
                    e=popfirst!(vertices)
                    custo_circuito+=self.custos[circuito[end],e]
                    append!(circuito,[e])
                end
                custo_circuito+=self.custos[circuito[end],1]
                #println(circuito,custo_circuito)
                if melhorCircuito == []
                    melhorCircuito=copy(circuito)
                    melhorCusto=custo_circuito
                elseif custo_circuito<melhorCusto
                    melhorCircuito=copy(circuito)
                    melhorCusto=custo_circuito
                end

                return (melhorCircuito, melhorCusto)
            end
            #gerarVertices(melhorCircuito,melhorCusto)
            for i = 1:iteracoes
                melhorCircuito,melhorCusto=gerarVertices(melhorCircuito, melhorCusto)
                # if i==1
                    # println("Melhor circuito: "," - Custo: ",melhorCusto)
                # end
            end
            println("Melhor circuito: - Custo: ",melhorCusto*2)
            # println("Melhor circuito: ",melhorCircuito," - Custo: ",melhorCusto)
        end
        self.pcvGenetico=function(tam_pop::Int64, geracoes::Int64,
            tam_torneio::Int64, prob_cruz::Float64, prob_mut::Float64)
            pop = Array{Int64}[] # população
            function gerar_individuo()::Array{Int64}
                vertices = [i for i = 2:self.n]
    			individuo = [1]
                while length(vertices)>0
                    shuffle!(vertices)
                    e=popfirst!(vertices)
                    append!(individuo,[e])
                end
                individuo
            end
            # função de fitness/objetivo
    		function obter_custo(individuo::Array{Int64})::Int64
    			custo = 0
    			for i = 1:self.n-1
    				custo += self.custos[(individuo[i], individuo[i + 1])]
                end
    			custo += self.custos[(individuo[end], individuo[1])]
    			return custo
            end
            # gerando a população inicial
    		for i = 1:tam_pop
    			append!(pop,[gerar_individuo()])
            end
            # a cada geração
            for i = 1:geracoes
                # seleção por torneio
                for j = 1:tam_torneio
                    if rand()::Float64 <= prob_cruz
    					pai1, pai2 = 0,0
                        while true
    						pai1 = rand(1:tam_pop)::Int64
    						pai2 = rand(1:tam_pop)::Int64
    						if pai1 != pai2
    							break
                            end
                        end
                        gen1_validos = [k for k = 1:self.n]
    					gen2_validos = copy(gen1_validos)
    					filho1, filho2 = Int64[], Int64[]
                        # cruzamento de um ponto
    					while true
    						# selecionando um ponto
    						ponto = rand(1:self.n)::Int64
    						# não seleciona as extremidades
    						if ponto != 1 && ponto != self.n
    							for p = 1:ponto
    								if ~in(pop[pai1][p],filho1)
                                        append!(filho1,pop[pai1][p])
                                        filter!(e->e!=pop[pai1][p],gen1_validos)
    								else
                                        shuffle!(gen1_validos)
                                        e=popfirst!(gen1_validos)
    									append!(filho1,e)
                                        filter!(f->f!=e,gen1_validos)
                                    end
    								if ~in(pop[pai2][p],filho2)
                                        append!(filho2,pop[pai2][p])
                                        filter!(e->e!=pop[pai2][p],gen2_validos)
    								else
                                        shuffle!(gen2_validos)
                                        e=popfirst!(gen2_validos)
    									append!(filho2,e)
                                        filter!(f->f!=e,gen2_validos)
                                    end
                                end
    							for p = ponto+1:self.n
    								if ~in(pop[pai2][p],filho1)
    									append!(filho1,pop[pai2][p])
                                        filter!(e->e!=pop[pai2][p],gen1_validos)
    								else
                                        shuffle!(gen1_validos)
                                        e=popfirst!(gen1_validos)
    									append!(filho1,e)
                                        filter!(f->f!=e,gen1_validos)
                                    end
    								if ~in(pop[pai1][p],filho2)
    									append!(filho2,pop[pai1][p])
                                        filter!(e->e!=pop[pai1][p],gen2_validos)
    								else
                                        shuffle!(gen2_validos)
                                        e=popfirst!(gen2_validos)
    									append!(filho2,e)
                                        filter!(f->f!=e,gen2_validos)
                                    end
                                end
                                break
                            end
    					end
                        # aplica o operador de mutação
    					if rand()::Float64 <= prob_mut
    						gene1, gene2= 0,0
    						while true
    							gene1 = rand(1:self.n)::Int64
    							gene2 = rand(1:self.n)::Int64
    							if gene1 != gene2
    								filho1[gene1], filho1[gene2] = filho1[gene2], filho1[gene1]
    								filho2[gene1], filho2[gene2] = filho2[gene2], filho2[gene1]
    								break
                                end
                            end
                        end
                        # obtém o fitness dos pais e dos filhos
    					fitness_pai1 = obter_custo(pop[pai1])
    					fitness_pai2 = obter_custo(pop[pai2])
    					fitness_filho1 = obter_custo(filho1)
    					fitness_filho2 = obter_custo(filho2)

    					if fitness_filho1 < fitness_pai1 || fitness_filho1 < fitness_pai2
    						if fitness_filho1 < fitness_pai1
    							deleteat!(pop,pai1)
    						else
    							deleteat!(pop,pai2)
                            end
    						append!(pop,[filho1])
    					elseif fitness_filho2 < fitness_pai1 || fitness_filho2 < fitness_pai2
    						if fitness_filho2 < fitness_pai1
    							deleteat!(pop,pai1)
    						else
    							deleteat!(pop,pai2)
                            end
    						append!(pop,[filho2])
                        end
                    end
                end
            end

            # obtém o melhor indivíduo da população
    		melhor_individuo = copy(pop[1])
    		for ind = 1:tam_pop
    			if obter_custo(pop[ind]) < obter_custo(melhor_individuo)
    				melhor_individuo = copy(pop[ind])
                end
            end
    		println("Melhor indivíduo: - Custo: ",obter_custo(melhor_individuo)*2)
    		# print("Melhor indivíduo: ",melhor_individuo," - Custo: ",obter_custo(melhor_individuo))
        end
        return self
    end
end
println()
println("-------------")
println()
arq=open("30CIT.txt", "r")
texto=read(arq, String)
texto=(split(replace(texto, "\r"=>""),"\n"))
arr=map(x->parse.(Int64,x),split.(texto[2:end]))
gg=GeradorGrafo(30)
gg.geradorGrafo(arr)
print("\nGenetico: ")
@time gg.pcvGenetico(500, 20000, 5, 0.75, 0.1)
print("Aleatorio: ")
@time gg.pcvRandom(2000000)
