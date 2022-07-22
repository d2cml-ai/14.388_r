install.packages("dagitty")
install.packages("ggdag")
library(dagitty)
library(ggdag)

#generate a couple of DAGs and plot them

G = dagitty('dag{
Z1 [pos="-2,-1.5"]
X1 [pos="-2,0"]
Z2 [pos="1.5,-1.5"]
X3 [pos="1.5, 0"]
Y [outcome,pos="1.5,1.5"]
D [exposure,pos="-2,1.5"]
M [mediator, pos="0,1.5"]
X2 [pos="0,0"]
Z1 -> X1
X1 -> D
Z1 -> X2
Z2 -> X3
X3 -> Y
Z2 -> X2
D -> Y
X2 -> Y
X2 -> D
M->Y
D->M
}')


ggdag(G)+  theme_dag()

print(parents(G, "X2"))
print(children(G, "X2"))
print(ancestors(G, "X2"))
print(descendants(G, "X2"))

paths(G, "D", "Y")

print( impliedConditionalIndependencies(G) )

print( adjustmentSets( G, "D", "Y" ) )

SWIG = dagitty('dag{
Z1 [pos="-2,-1.5"]
X1 [pos="-2,0"]
Z2 [pos="1.5,-1.5"]
X3 [pos="1.5, 0"]
Yd [outcome,pos="1.5,1.5"]
D [exposure,pos="-2,1.5"]
d [pos="-1, 1.5"]
Md [mediator, pos="0,1.5"]
X2 [pos="0,0"]
Z1 -> X1
X1 -> D
Z1 -> X2
Z2 -> X3
X3 -> Yd
Z2 -> X2
X2 -> Yd
X2 -> D
X3-> Yd
Md-> Yd
d-> Md
}')

ggdag(SWIG)+  theme_dag()


print( impliedConditionalIndependencies(SWIG)[5:8] )


for( n in names(G) ){
    for( m in children(G,n) ){
        a <- adjustmentSets( G, n, m )
        if( length(a) > 0 ){
            cat("The effect ",n,"->",m,
                " is identifiable by controlling for:\n",sep="")
            print( a, prefix=" * " )
        }
    }
}

P=equivalenceClass(G)
plot(P)
#equivalentDAGs(G,10)

G3<- dagitty('dag{
D -> Y
X -> D
X -> Y
}
')

ggdag(G3)+  theme_dag()

print(impliedConditionalIndependencies(G3))



P=equivalenceClass(G3)
plot(P)
equivalentDAGs(G3,10)



set.seed(1)
x <- simulateSEM(G)
head(x)
#cov(x)
localTests(G, data = x, type = c("cis"))



x.R = x
x.R$D = (x$D+ x$Y)/2

localTests(G, data = x.R, type = c("cis"))


