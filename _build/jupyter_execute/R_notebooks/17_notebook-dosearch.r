install.packages("librarian", quiet = T)

librarian::shelf(
  dosearch, 
  quiet = T
)

data <- "p(y,d,x)"  #data structure

query <- "p(y | do(d),x)" #query -- target parameter

graph <- "x -> y
  x -> d 
  d -> y"

dosearch(data, query, graph)

data <- "p(y,d,x)"

query <- "p(y | do(d))"

graph <- "x -> y
  x -> d 
  d -> y"


dosearch(data, query, graph)



data <- "p(y,d)"

query <- "p(y | do(d))"

graph <- "x -> y
  x -> d 
  d -> y"

dosearch(data, query, graph)



data <- "p(y,d,x2)"   #observed only (Y, D, X_2)

query<- "p(y|do(d))"   #target parameter

graph<- "z1 -> x1
z1 -> x2
z2 -> x2
z2 -> x3
x2 -> d
x2 -> y
x3 -> y
x1 -> d
d -> m
m -> y
"

dosearch(data, query, graph)



data <- "p(y,d,x2,x3)"

conditional.query<- "p(y|do(d),x2, x3)"  #can ID conditional average effect?
query<- "p(y|do(d))"  #can ID unconditional effect?

graph<- "z1 -> x1
z1 -> x2
z2 -> x2
z2 -> x3
x2 -> d
x2 -> y
x3 -> y
x1 -> d
d -> m
m -> y
"

print(dosearch(data, conditional.query, graph))
print(dosearch(data, query, graph))


data <- "p(y,d, m)" 

query.dm<- "p(m|do(d))" 
query.md<- "p(y|do(m))" 
query<- "p(y|do(d))"

graph<- "z1 -> x1
z1 -> x2
z2 -> x2
z2 -> x3
x2 -> d
x2 -> y
x3 -> y
x1 -> d
d -> m
m -> y
"
print(dosearch(data, query.dm, graph))
print(dosearch(data, query.md, graph))
print(dosearch(data, query, graph))

data <- "p(y,m)
         p(m,d)" 

query.dm<- "p(m|do(d))" 
query.md<- "p(y|do(m))" 
query<- "p(y|do(d))"

graph<- "z1 -> x1
z1 -> x2
z2 -> x2
z2 -> x3
x2 -> d
x2 -> y
x3 -> y
x1 -> d
d -> m
m -> y
"
print(dosearch(data, query.dm, graph))
print(dosearch(data, query.md, graph))
print(dosearch(data, query, graph))
