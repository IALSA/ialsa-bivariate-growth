This folder contains the files consumed and produced by Mplus during the process of estimation.

Proposed structure of the directories that host output files:

./ # root
./outputs/
./outputs/studies # needed for identifying the location to extract study name (legacy)
./outputs/studies/eas/ 
./outputs/studies/elsa/
./outputs/studies/.../  
./outputs/studies/map/
./outputs/studies/map/phys-phys/  # phys-phys track
./outputs/studies/map/phys-cog/   # phys-cog track 
./outputs/studies/map/phys-cog/gait/
./outputs/studies/map/phys-cog/grip/
./outputs/studies/map/phys-cog/pulmonary/ 
./outputs/studies/map/phys-cog/pulmonary/fev-mmse/
./outputs/studies/map/phys-cog/pulmonary/fev-bnt/
./outputs/studies/map/phys-cog/pulmonary/fev-numcomp/

Each of the folder hosts a collection of models, varying in the following respects:
- model_type : u0,u1,u2, b0,b1,b2
- waves_selected: 1:5, 1:7, c(1,3,5,7)
- subgroup: male, female, unisex