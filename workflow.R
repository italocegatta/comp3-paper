library(DiagrammeR)

png(file = "workfow.png")

grViz("
  
  digraph index {
  
  node [shape  = box,  style = filled]
  A [label = 'Índice de competição']
  B [label = 'Indipendente da distância']
  B1 [label = 'Índices']
  C [label = 'Dependente da distância']
  D [label = 'Parcela útil']
  E [label = 'Competidor']
  E1 [label = 'Distância fixa']
  E2 [label = 'Vizinhos mais próximos']
  F [label = 'Índices']
  
  A -> B [label = ' di_*()']
  B-> B1
  A -> C [label = ' dd_*()']
  C -> D [label = ' available_tree()']
  D -> E [label = '']
  E -> E1 [label = 'search=dfixed']
  E -> E2 [label = 'search=nearest']
  {E1 E2} -> F
  
  }
")

dev.off()





