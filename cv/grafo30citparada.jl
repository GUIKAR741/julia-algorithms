using Random
using Profile
using Printf
Profile.clear()
mutable struct GeradorGrafo
    grafo::Array
    custos::Dict
    geradorGrafo::Function
    mostraGrafo::Function
    pcvGenetico::Function
    distancia::Function
    function GeradorGrafo(n)
        self=new()
        self.grafo=[[] for i=1:n]
        self.custos=Dict()
        self.distancia=(x1::Int64,y1::Int64,x2::Int64,y2::Int64)->(√((x1-x2)^2+(y1-y2)^2))::Float64
        self.geradorGrafo=function (distancias::Array)
            for i = 1:n
                for j = 1:n
                    if i!=j
                        if !((i,j) in keys(self.custos))
                            # custo=rand(1:100)
                            # println(i," ",j, " ", self.distancia(distancias[i][1],distancias[i][2],distancias[i][1],distancias[j][2]))
                            self.custos[(i,j)]=self.distancia(distancias[i][1],distancias[i][2],distancias[i+1][1],distancias[i+1][2])
                            # println(self.distancia(distancias[i][1],distancias[i][2],distancias[i+1][1],distancias[i+1][2]))
                            self.custos[(j,i)]=self.distancia(distancias[i][1],distancias[i][2],distancias[i+1][1],distancias[i+1][2])
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
        self.pcvGenetico=function(tam_pop::Int64, geracoes::Int64,
            tam_torneio::Int64, prob_cruz::Float64, prob_mut::Float64)
            pop = Array{Float64}[] # população
            fit = Array{Float64}[] # fitness
            function gerar_individuo()::Array{Float64}
                vertices = [i for i = 2:n]
    			individuo = [1]
                while length(vertices)>0
                    shuffle!(vertices)
                    e=popfirst!(vertices)
                    append!(individuo,[e])
                end
                individuo
            end
            # função de fitness/objetivo
    		function obter_custo(individuo::Array{Float64})::Float64
    			custo = 0.0
    			for i = 1:n-1
    				custo += self.custos[(individuo[i], individuo[i + 1])]
                end
    			custo += self.custos[(individuo[end], individuo[1])]
    			return custo
            end
            # gerando a população inicial
            pop=[gerar_individuo() for i=1:tam_pop]
    		# for i = 1:tam_pop
    		# 	append!(pop,[gerar_individuo()])
            # end
            fit=obter_custo.(pop)
            melhorSolucao::Float64=0.0
            iter::Int64=0
            iterMelhor::Int64=0
            # a cada geração
            # for i = 1:geracoes
            while (iter-iterMelhor)<=geracoes || geracoes==-1
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
                        gen1_validos = [k for k = 1:n]
    					gen2_validos = copy(gen1_validos)
    					filho1, filho2 = Float64[], Float64[]
                        # cruzamento de um ponto
    					while true
    						# selecionando um ponto
    						ponto = rand(1:n)::Int64
    						# não seleciona as extremidades
    						if ponto != 1 && ponto != n
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
    							for p = ponto+1:n
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
    							gene1 = rand(1:n)::Int64
    							gene2 = rand(1:n)::Int64
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
                iter+=1
                indCusto=obter_custo.(pop)
                if minimum(indCusto)<melhorSolucao || melhorSolucao==0.0
                    melhorSolucao=minimum(indCusto)
                    iterMelhor=iter
                end
                if iter%1000==0 && geracoes==-1
                    # [println(pop[i]) for i = 1:5]
                    @printf("Genetico: Quantidade de Gerações: %d - Custo Minimo: %.4lf - Custo Maximo: %.4lf\n",iter,minimum(indCusto),maximum(indCusto))
                end
            end
            id=[count(f->f==i,pop) for i = pop]
            println([(count(f->f==i,id),i) for i = unique(id)])
            # obtém o melhor indivíduo da população
            melhor_individuo = minimum(obter_custo.(pop))
    		@printf("Genetico: Quantidade de Gerações: %d - Custo: %.4lf\n",iter-geracoes,melhor_individuo)
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
distancias=map(x->parse.(Int64,x),split.(texto[2:end]))
gg=GeradorGrafo(30)
gg.geradorGrafo(distancias)
# print("\nGenetico: ")
# @time [gg.pcvGenetico(150, 500, 5, 0.75, 0.1) for i = 1:10]
@time gg.pcvGenetico(500, 1000, 4, 0.75, 0.1)
#48872
