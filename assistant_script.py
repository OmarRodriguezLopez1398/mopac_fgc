#!/usr/bin/env python3
# Import necessary libraries
from collections import defaultdict 
import sys
import numpy

# Define a Graph class to represent molecular structure
class Graph:
    # Method to find the vertex with minimum distance value
    def minDistance(self,dist,queue):
        minimum = float("Inf") 
        min_index = -1
        for i in range(len(dist)): 
            if dist[i] < minimum and i in queue: 
                minimum = dist[i] 
                min_index = i 
        return min_index
    def degree(self,graph):
        deg = [0]*natom
        for iatom in range(natom):
            for ele in graph[iatom]:
                deg[iatom] += ele        
        return deg
    # Implementation of Dijkstra's algorithm for shortest path
    def dijkstra(self, graph, src, atom_j):
        row = len(graph) 
        col = len(graph[0])
        dist = [float("Inf")] * row
        parent = [-1] * row
        dist[src] = 0
        queue = []
        for i in range(row): 
            queue.append(i)
        while queue:
            u = self.minDistance(dist,queue)
            if u == -1:
               break
            else:
               queue.remove(u)
            for i in range(col):
                if graph[u][i] and i in queue: 
                    if dist[u] + graph[u][i] < dist[i]: 
                        dist[i] = dist[u] + graph[u][i] 
                        parent[i] = u 
        return dist[atom_j]
# Dictionary of covalent radii for different elements
g = Graph()
lar_dict = {"h" : 0.31 ,"he" : 0.28 ,"li" : 1.28 ,"be" : 0.96 ,"b" : 0.84 ,"c" : 0.74 , \
"n" : 0.71 ,"o" : 0.66 ,"f" : 0.57 ,"ne" : 0.58 ,"na" : 1.66 ,"mg" : 1.41 ,"al" : 1.21 ,"si" : 1.11 ,"p" : 1.07 ,"s" : 1.05 , \
"cl" : 1.02 ,"ar" : 1.06 ,"k" : 2.03 ,"ca" : 1.76 ,"sc" : 1.70 ,"ti" : 1.60 ,"v" : 1.53 ,"cr" : 1.39 ,"mn" : 1.50 ,"fe" : 1.42 , \
"co" : 1.38 ,"ni" : 1.24 ,"cu" : 1.32 ,"zn" : 1.22 ,"ga" : 1.22 ,"ge" : 1.20 ,"as" : 1.19 ,"se" : 1.20 ,"br" : 1.20 ,"kr" : 1.16 , \
"rb" : 2.20 ,"sr" : 1.95 ,"y" : 1.90 ,"zr" : 1.75 ,"nb" : 1.64 ,"mo" : 1.54 ,"tc" : 1.47 ,"ru" : 1.46 ,"rh" : 1.42 ,"pd" : 1.39 , \
"ag" : 1.45 ,"cd" : 1.44 ,"in" : 1.42 ,"sn" : 1.39 ,"sb" : 1.39 ,"te" : 1.38 ,"i" : 1.39 ,"xe" : 1.40 ,"cs" : 2.44 ,"ba" : 2.15 , \
"la" : 2.07 ,"ce" : 2.04 ,"pr" : 2.03 ,"nd" : 2.01 ,"pm" : 1.99 ,"sm" : 1.98 ,"eu" : 1.98 ,"gd" : 1.96 ,"tb" : 1.94 ,"dy" : 1.92 , \
"ho" : 1.92 ,"er" : 1.89 ,"tm" : 1.90 ,"yb" : 1.87 ,"lu" : 1.87 ,"hf" : 1.75 ,"ta" : 1.70 ,"w" : 1.62 ,"re" : 1.51 ,"os" : 1.44 , \
"ir" : 1.41 ,"pt" : 1.36 ,"au" : 1.36 ,"hg" : 1.32 ,"tl" : 1.45 ,"pb" : 1.46 ,"bi" : 1.48 ,"po" : 1.40 ,"at" : 1.50 ,"rn" : 1.50 , \
"fr" : 2.60 ,"ra" : 2.21 ,"ac" : 2.15 ,"th" : 2.06 ,"pa" : 2.00 ,"u" : 1.96 ,"np" : 1.90 ,"pu" : 1.87 ,"am" : 1.80 ,"cm" : 1.69 }

# Function to create adjacency matrix based on atomic distances        
def create_adjacency(x,rc):
    a = []
    for i in range(natom):
        ai = []
        for j in range(natom):
            if i == j:
               r = float("Inf") 
            else:
               x1 = x[3*i  ]-x[3*j  ]
               x2 = x[3*i+1]-x[3*j+1]
               x3 = x[3*i+2]-x[3*j+2]
               r = numpy.sqrt(x1 * x1 + x2 * x2 + x3 * x3)
            rct =  ( rc[i] + rc[j] ) * 1.3
            if r <= rct:
               ai.append(1)
               #print(f"Conexion entre el atomo {i + 1} y el atomo {j + 1}")
            else:
               ai.append(0)
               #print(f"NO HAY Conexion entre el atomo {i + 1} y el atomo {j + 1}")
        a.append(ai)
    return a


###############################################################################        
# Read XYZ file and return atomic symbols and coordinates       
xyzFile = open(str(sys.argv[1]),'r')
rc = []
x  = []
atomic_symbol = []
natom = 0        
x_coor = []
y_coor = []
z_coor = []
i = 0        
for line in xyzFile:
    i = i + 1
    columns = line.split()
    if len(columns)==4:
       x_coor.append(float(columns[1])),y_coor.append(float(columns[2])),z_coor.append(float(columns[3]))
       natom += 1
       atomic_symbol.append( str(columns[0]).lower())
       rc.append(lar_dict[str(columns[0]).lower()])
       x.extend( (float(columns[1]),float(columns[2]),float(columns[3])) )

# Determine atom types based on connectivity
A = create_adjacency(x,rc)
atype = []
d = []
ind = []
jnd = []
att = []     
attt = []   
modet = []
for atom_i in range(natom):
    neighsi = []
    mode = []
    for i in [i for i, e in enumerate(A[atom_i]) if e==1]:
        if atomic_symbol[atom_i] == "h": 
                neighsi.append(atomic_symbol[i]+str(g.degree(A)[i]))
                mode.append(atomic_symbol[i]+str(g.degree(A)[i]))
        elif atomic_symbol[atom_i] == "c": 
            if g.degree(A)[atom_i] == 4:                
                continue
            elif g.degree(A)[atom_i] == 3 and atomic_symbol[i]=="o" and g.degree(A)[i]==2:
                mode.append(str("o2"))
            elif g.degree(A)[atom_i] == 3 and atomic_symbol[i]=="o" and g.degree(A)[i]==1:
                mode.append(str("o1"))
            else:                
                neighsi.append(atomic_symbol[i]+str(g.degree(A)[i]))
        elif atomic_symbol[atom_i] == "n": 
            if g.degree(A)[atom_i] == 4:
                continue
            elif g.degree(A)[atom_i] == 3:
                continue
            else:
                neighsi.append(atomic_symbol[i]+str(g.degree(A)[i]))
                mode.append(atomic_symbol[i]+str(g.degree(A)[i]))
        elif atomic_symbol[atom_i] == "o":
            if g.degree(A)[atom_i] == 1:
                neighsi.append(atomic_symbol[i]+str(g.degree(A)[i]))
                mode.append(atomic_symbol[i]+str(g.degree(A)[i]))
            elif g.degree(A)[atom_i] == 2 and atomic_symbol[i]=="c":
                neighsi.append(atomic_symbol[i]+str(g.degree(A)[i]))
                mode.append(atomic_symbol[i]+str(g.degree(A)[i]))
            else:
                continue
        else:
            continue
    neighsi.sort()
    attt.append( atomic_symbol[atom_i]+str(g.degree(A)[atom_i])+''.join(neighsi))
    modet.append( atomic_symbol[atom_i]+str(g.degree(A)[atom_i])+''.join(mode))
    att.append( atomic_symbol[atom_i]+str(g.degree(A)[atom_i])+''.join(neighsi))
    for atom_j in range(atom_i+1,natom):
        d.append(g.dijkstra(A,atom_i,atom_j)) 
        ind.append(atom_i+1)       
        jnd.append(atom_j+1)
    
#***************************************************************************
#Starting in Nitrogen
for atom_i in range(natom):
    if modet[atom_i] == "n3":
        modet[atom_i]="NAN"        
        for i in [i for i, e in enumerate(A[atom_i]) if e==1]:                                     
            if modet[i] == "c3o1":                  
                modet[atom_i] = "NAD"
                modet[i] = "CAD"
            else:
                continue
for atom_i in range(natom):
    if modet[atom_i]=="NAN":       
        for i in [i for i, e in enumerate(A[atom_i]) if e==1]:
            if modet[i]=="h1n3":
                modet[i]="HNAN"
            else:
                continue
for atom_i in range(natom):
    if modet[atom_i]=="NAD":       
        for i in [i for i, e in enumerate(A[atom_i]) if e==1]:
            if modet[i]=="h1n3":
                modet[i]="HNAD"
            else:
                continue
for atom_i in range(natom):
    if modet[atom_i]=="CAD":       
        for i in [i for i, e in enumerate(A[atom_i]) if e==1]:
            if modet[i]=="o1c3":
                modet[i]="OAD"
            elif modet[i]=="c4":
                modet[i]="CA"
            else:
                continue        
for atom_i in range(natom):
    if modet[atom_i]=="c3o2o1" or modet[atom_i]=="c3o1o2":
        modet[atom_i]="CCA"
        for i in [i for i, e in enumerate(A[atom_i]) if e==1]:
            if modet[i]=="o2c3":
                modet[i]="OCA2"
            elif modet[i]=="o1c3":
                modet[i]="OCA1"
            elif modet[i]=="c4":
                modet[i]="CA"
            elif modet[i]=="h1c3":
                modet[i]="HCCA"
            else:
                continue
            
for atom_i in range(natom):
    if modet[atom_i]=="OCA2":       
        for i in [i for i, e in enumerate(A[atom_i]) if e==1]:
            if modet[i]=="h1o2":
                modet[i]="HOCA"
            else:
                continue
for atom_i in range(natom):
    if modet[atom_i]=="CA":       
        for i in [i for i, e in enumerate(A[atom_i]) if e==1]:
            if modet[i]=="h1c4":
                modet[i]="HCA"
            elif modet[i]=="c4": 
                modet[i]="CT"
            else:
                continue
for atom_i in range(natom):
    if modet[atom_i]=="CT":        
        for i in [i for i, e in enumerate(A[atom_i]) if e==1]:
            if modet[i]=="c4": 
                modet[i]="CT"
            elif modet[i]=="h1c4": 
                modet[i]="HCT"
            else:
                continue
for atom_i in range(natom):
    if modet[atom_i]=="NAN":        
        for i in [i for i, e in enumerate(A[atom_i]) if e==1]:
            if modet[i]=="c4" and modet[i]!="CT" and modet[i]!="CA": 
                modet[i]="CAN"
            else:
                continue 
for atom_i in range(natom):
    if modet[atom_i]=="NAD":        
        for i in [i for i, e in enumerate(A[atom_i]) if e==1]:
            if modet[i]=="c4" and modet[i]!="CT" and modet[i]!="CA": 
                modet[i]="CAN"
            else:
                continue
for atom_i in range(natom):
    if modet[atom_i]=="CAN":        
        for i in [i for i, e in enumerate(A[atom_i]) if e==1]:
            if modet[i]=="h1c4": 
                modet[i]="HCAN"
            else:
                continue

###############################################################################
#  Process the molecules and generate setup output files
molecules = []
for atom_i in range(natom):
    connected_molecules = []
    for molecule_id, molecule in enumerate(molecules, start=1):
        for atom_j in molecule:
            if A[atom_i][atom_j - 1]==1:
                connected_molecules.append(molecule_id)
                break
            
    if not connected_molecules:
        molecules.append([atom_i+1])
    else:
        new_molecule = [atom_i + 1]
        for molecule_id in connected_molecules:
            new_molecule.extend(molecules[molecule_id - 1])
        molecules = [m for i, m in enumerate(molecules) if i + 1 not in connected_molecules ]
        molecules.append(new_molecule)    

# Write atom types to file
file = open("assistant_atom_type.txt", "w")
sys.stdout = file
for molecule_id, molecule in enumerate(molecules, start=1):    
    molecule=sorted(molecule)
    for atom_id, i in enumerate(molecule, start=1):
        atomic_index = i - 1
        if modet[atomic_index]=="c4":
                modet[atomic_index]="C3"
                   
        elif modet[atomic_index]=="c3":
                   modet[atomic_index]="C2"
                   
        elif modet[atomic_index]=="h1c4":
                   modet[atomic_index]="H3"
                   
        elif modet[atomic_index]=="h1c3":
                   modet[atomic_index]="H2"        
        print(f"{i}\t{atomic_symbol[atomic_index].upper()}\t{molecule_id}\t{modet[atomic_index]}")
sys.stdout = sys.__stdout__
file.close()
 # Write setup information to file
file1 = open("setup.txt", "w")
sys.stdout = file1
###############################################################################
for i in range(len(d)):
      if d[i] == float('inf'):
         d[i] = 0
for i in range(len(d)):  
        #if d[i] >= 4:
        if d[i] == 0:
           a=ind[i]-1
           b=jnd[i]-1
           for atom_i in range(natom):
               if modet[b]=="c4" or modet[a]=="c4":
                   modet[b]="C3"
                   modet[a]="C3"
               elif modet[b]=="c3" or modet[a]=="c3":
                   modet[b]="C2"
                   modet[a]="C2"
               elif modet[b]=="h1c4" or modet[a]=="h1c4":
                   modet[b]="H3"
                   modet[a]="H3"
               elif modet[b]=="h1c3" or modet[a]=="h1c3":
                   modet[b]="H2"
                   modet[a]="H2"
          
           print(" %4s %4s %4s %10s %10s " % (jnd[i],ind[i],d[i],modet[b],modet[a])) 
sys.stdout = sys.__stdout__
file1.close()          
           
           

