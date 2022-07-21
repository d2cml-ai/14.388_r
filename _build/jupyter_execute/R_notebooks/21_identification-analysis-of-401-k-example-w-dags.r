#install and load package

install.packages("dagitty")
install.packages("ggdag")
library(dagitty)
library(ggdag)


#generate a DAGs and plot them

G1 = dagitty('dag{
Y [outcome,pos="4, 0"]
D [exposure,pos="0, 0"]
X [confounder, pos="2,-2"]
F [uobserved, pos="0, -1"]
D -> Y
X -> D
F -> X
F -> D
X -> Y}')


ggdag(G1)+  theme_dag()

adjustmentSets( G1, "D", "Y",effect="total" ) 

#generate a couple of DAGs and plot them

G2 = dagitty('dag{
Y [outcome,pos="4, 0"]
D [exposure,pos="0, 0"]
X [confounder, pos="2,-2"]
F [uobserved, pos="0, -1"]
D -> Y
X -> D
X -> F
F -> D
X -> Y}')


ggdag(G2)+  theme_dag()

adjustmentSets( G2, "D", "Y", effect="total" )


G3 = dagitty('dag{
Y [outcome,pos="4, 0"]
D [exposure,pos="0, 0"]
X [confounder, pos="2,-2"]
F [unobserved, pos="0, -1"]
A [unobserved, pos="-1, -1"]
D -> Y
X -> D
F -> D
A -> F
A -> X
A -> D
X -> Y}')

adjustmentSets( G3, "D", "Y", effect="total"  ) 

ggdag(G3)+  theme_dag()

G4 = dagitty('dag{
Y [outcome,pos="4, 0"]
D [exposure,pos="0, 0"]
X [confounder, pos="2,-2"]
F [unobserved, pos="0, -1"]
A [unobserved, pos="-1, -1"]
D -> Y
X -> D
F -> D
A -> F
A -> X
A -> D
F -> Y
X -> Y}')


ggdag(G4)+  theme_dag()

adjustmentSets( G4, "D", "Y",effect="total"  )


G5 = dagitty('dag{
Y [outcome,pos="4, 0"]
D [exposure,pos="0, 0"]
X [confounder, pos="2,-2"]
F [unobserved, pos="0, -1"]
A [unobserved, pos="-1, -1"]
M [unobserved, pos="2, -.5"]
D -> Y
X -> D
F -> D
A -> F
A -> X
A -> D
D -> M
M -> Y
X -> M
X -> Y}')

print( adjustmentSets( G5, "D", "Y",effect="total"  ) )

ggdag(G5)+  theme_dag()

G6 = dagitty('dag{
Y [outcome,pos="4, 0"]
D [exposure,pos="0, 0"]
X [confounder, pos="2,-2"]
F [unobserved, pos="0, -1"]
A [unobserved, pos="-1, -1"]
M [uobserved, pos="2, -.5"]
D -> Y
X -> D
F -> D
A -> F
A -> X
D -> M
F -> M
A -> D
M -> Y
X -> M
X -> Y}')

print( adjustmentSets( G6, "D", "Y" ),effect="total"  )

ggdag(G6)+  theme_dag()
