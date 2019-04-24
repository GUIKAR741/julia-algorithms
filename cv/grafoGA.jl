using Random
using Printf
using Plots
using Base.Filesystem
pyplot()
function plotar(forma, nome,titulo)
    dis=[distancias[i] for i = forma]
    x=UInt32[]
    y=UInt32[]
    [(append!(x,i[1]), append!(y,i[2])) for i = dis]
    append!(x,x[1])
    append!(y,y[1])
    plot(x, y, marker=([:circle]))
    title!(string(titulo))
    savefig(string(nome,".png"))
end
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
        self.pcvGenetico=function(elitismo::Bool;
                                  tam_pop=100::Int64,
                                  geracoes=100::Int64,
                                  sel=2::Int64,
                                  tam_torneio=2::Int64,
                                  prob_cruz=0.7::Float64,
                                  prob_mut=0.05::Float64,
                                  geracoesPlot=1000::Int64
        )
            # função de fitness/objetivo
    		function obter_custo(individuo::Array{Int64})::Float64
    			custo = 0.0
    			for i = 1:n-1
    				custo += self.custos[(individuo[i], individuo[i + 1])]
                end
    			custo += self.custos[(individuo[end], individuo[1])]
    			return custo
            end
            # função que inicia a população
            function gerar_populacao(tam_pop::Int64)::Tuple{Array{Array{Int64,N} where N,1},Array{Float64,1}}
                pop,fit=Array{Int64}[],Float64[]
                for i = 1:tam_pop
                    vertices = [i for i = 2:n]
        			individuo = [1]
                    while length(vertices)>0
                        shuffle!(vertices)
                        e=popfirst!(vertices)
                        append!(individuo,[e])
                    end
                    # individuo
                    append!(pop,[individuo])
                    # fitness do individuo
                    append!(fit,obter_custo(individuo))
                end
                pop,fit
            end
            # função que seleciona a população
            function selecao(pop::Array{Array{Int64}},fit::Array{Float64},
                sel::Int64,tam_torneio::Int64)::Array{Array{Int64}}
                newPop = sel == 1 ? roleta(pop,fit) : torneio(pop,fit,tam_torneio)
            end
            # metodo de seleção por roleta
            function roleta(pop::Array{Array{Int64}},fit::Array{Float64})::Array{Array{Int64}}
                newPop=fill(Int64[],tam_pop)
                a=sum(fit)
                p=-fit/a
                q=cumsum(p)
                r=rand(tam_pop)
                for i = 1:tam_pop
                    if r[i]<q[1]
                        newPop[i]=pop[1]
                    else
                        idx=findall(f->f==true,q.<r[i])[1]
                        newPop[i]=pop[idx]
                    end
                end
                newPop
            end
            # metodo de seleção por torneio
            function torneio(pop::Array{Array{Int64}},fit::Array{Float64},tam_torneio::Int64)::Array{Array{Int64}}
                newPop=fill(Int64[],tam_pop)
                for i=1:tam_pop
                    idx=randperm(tam_pop)[1:tam_torneio]
                    pos=[fit[i] for i = idx]
                    pos=findall(x->x==minimum(pos), pos)
                    newPop[i]=pop[idx[pos[1]]]
                end
                newPop
            end
            # função que faz o crossover na população
            function crossover(pop::Array{Array{Int64}},prob_cruz::Float64)::Array{Array{Int64}}
                newPop=fill(Int64[],tam_pop)
                r=rand(tam_pop)
                popSize=length(pop)
                pop=[pop[i] for i=findall(f->f==true,r.<prob_cruz)]
                tmpPopSize=length(pop)
                for i = 1:2:popSize
                    if tmpPopSize>1
                        pai1,pai2=randperm(tmpPopSize)[1:2]
                        gen1_validos = [k for k = 1:n]
                        gen2_validos = copy(gen1_validos)
                        filho1, filho2 = Int64[], Int64[]
                        # selecionando um ponto
                        ponto = rand(2:n-1)::Int64
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
                        # obtém o fitness dos pais e dos filhos
    					fitness_pai1 = obter_custo(pop[pai1])
    					fitness_pai2 = obter_custo(pop[pai2])
    					fitness_filho1 = obter_custo(filho1)
    					fitness_filho2 = obter_custo(filho2)
                        if fitness_filho1 < fitness_pai1 || fitness_filho1 < fitness_pai2
    						newPop[i]=filho1
                        else
                            newPop[i]=pop[pai1]
                        end
    					if fitness_filho2 < fitness_pai1 || fitness_filho2 < fitness_pai2
    						newPop[i+1]=filho2
                        else
                            newPop[i+1]=pop[pai2]
                        end
                    end
                end
                newPop
            end
            # função que faz a mutação na população
            function mutacao(pop::Array{Array{Int64}},prob_mut::Float64)::Tuple{Array{Array{Int64,N} where N,1},Array{Float64,1}}
                for i = 1:tam_pop
                    # aplica o operador de mutação
                    if rand()::Float64 <= prob_mut
                        gene1, gene2 = randperm(n)[1:2]
                        fitAntes=obter_custo(pop[i])
                        pop[i][gene1], pop[i][gene2] = pop[i][gene2], pop[i][gene1]
                        fitDepois=obter_custo(pop[i])
                        if fitAntes<fitDepois
                            pop[i][gene1], pop[i][gene2] = pop[i][gene2], pop[i][gene1]
                        end
                    end
                end
                pop,obter_custo.(pop)
            end
            # gerando a população inicial
            pop::Array{Array{Int64}} #= população =#,fit::Array{Float64} #= fitness =#=gerar_populacao(tam_pop)
            melhorSolucao::Float64=minimum(fit)
            iter::Int64=1
            iterMelhor::Int64=1
            plotar(pop[findall(f->f==melhorSolucao,fit)[1]],string((sel==1 ? "Roleta" : "Torneio"),"-Gerações",iter),string((sel==1 ? "Roleta" : "Torneio")," Gerações: ",iter," - Fitness: ",melhorSolucao))
            # a cada geração
            while (iter-iterMelhor)<=geracoes || geracoes==-1
                tPop::Array{Array{Int64}}=selecao(pop,fit,sel,tam_torneio)
                tPop=crossover(pop,prob_cruz)
                tPop,tFit::Array{Float64}=mutacao(pop,prob_mut)
                if elitismo
                    bestT::Float64=minimum(tFit)
                    if melhorSolucao<bestT
                        idx::Int64=findall(f->f==minimum(fit),fit)[1]
                        idxWorst::Int64=findall(f->f==maximum(tFit),tFit)[1]
                        tPop[idxWorst]=pop[idx]
                        tFit[idxWorst]=fit[idx]
                    end
                end
                pop=tPop
                fit=tFit
                iter+=1
                if minimum(fit)<melhorSolucao
                    melhorSolucao=minimum(fit)
                    iterMelhor=iter
                end
                if iter%geracoesPlot==0 && (geracoes==-1 || geracoesPlot!=1000)
                    id=[count(f->f==i,pop) for i = pop]
                    println([(count(f->f==i,id),i) for i = unique(id)])
                    @printf(
                    "Seleção: %s \nQuantidade de Gerações: %d - População: %d\nCusto Minimo: %.4lf - Custo Maximo: %.4lf\n\n",
                    (sel==1 ? "Roleta" : "Torneio"),iter,tam_pop,melhorSolucao,maximum(fit))
                    plotar(pop[findall(f->f==melhorSolucao,fit)[1]],string((sel==1 ? "Roleta" : "Torneio"),"-Gerações",iter),string((sel==1 ? "Roleta" : "Torneio")," Gerações: ",iter," - Fitness: ",melhorSolucao))
                end
            end
            id=[count(f->f==i,pop) for i = pop]
            println([(count(f->f==i,id),i) for i = unique(id)])
            # obtém o melhor indivíduo da população
    		@printf("Seleção: %s - Quantidade de Gerações: %d - Custo: %.4lf\n",
            (sel==1 ? "Roleta" : "Torneio"),iter-geracoes,melhorSolucao)
            plotar(pop[findall(f->f==melhorSolucao,fit)[1]],string((sel==1 ? "Roleta" : "Torneio"),"-Gerações",iter),string((sel==1 ? "Roleta" : "Torneio")," Gerações: ",iter," - Fitness: ",melhorSolucao))
        end
        return self
    end
end

println()
println("-------------")
println()
cd("/mnt/8A5492C05492AF07/Programação/Julia")
arq=open("30CIT.txt", "r")
texto=read(arq, String)
texto=(split(replace(texto, "\r"=>""),"\n"))
distancias=map(x->parse.(Int64,x),split.(texto[2:end]))
gg=GeradorGrafo(30)
gg.geradorGrafo(distancias)
# @time [gg.pcvGenetico(150, 500, 5, 0.75, 0.1) for i = 1:10]
cd("plots/")
[occursin("Gerações",i) ? rm(string(pwd(),"/",i)) : 1 for i = readdir(pwd())]
@time [gg.pcvGenetico(true, tam_pop=100, geracoes=500, sel=i, tam_torneio=4,
                      prob_cruz=0.8, prob_mut=0.1, geracoesPlot=100)
       for i = 2:2]
#Melhor Conhecido: 48872
#Melhor Obtido:59442 - 11000 Gerações
