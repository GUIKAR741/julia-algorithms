using Printf
println()
arq=open("30CIT.txt", "r")
texto=read(arq, String)
texto=(split(replace(texto, "\r"=>""),"\n"))
arr=map(x->parse.(Int64,x),split.(texto[2:end]))
dis=(x1,y1,x2,y2)->(√((x1-x2)^2+(y1-y2)^2))
soma=0.0
for i = 1:29
    global soma
    soma+=dis(arr[i][1],arr[i][2],arr[i+1][1],arr[i+1][2])
end
soma+=dis(arr[end][1],arr[end][2],arr[1][1],arr[1][2])
@printf("%.4lf",soma)
# for i = 30:-1:2
#     global soma
#     soma+=dis(arr[i][1],arr[i][2],arr[i-1][1],arr[i-1][2])
# end
# println(soma)
