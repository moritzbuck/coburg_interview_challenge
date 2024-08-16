# set up environment

root=.
mkdir -p $root/data
mkdir -p $root/src
mkdir -p $root/img

# Download necessary data 

wget -P $root/data/ https://www.openml.org/data/download/20649148/freMTPL2freq.arff
wget -P $root/data/ https://www.openml.org/data/download/20649149/freMTPL2sev.arff

## slightly dirty downloading of official french sensus data.
echo "code,name,population" > $root/data/census_data.csv
wget -O - -o /dev/null https://www.insee.fr/fr/statistiques/6683015 |
    tr "\n" "%" |
    sed 's#.*<tbody>\(.*\)</tbody>.*#\1#' |
    tr "%" "\n"    |
    sed 's#.*<t.*">##g; s#</t[hd]>##g; s#&nbsp;##g ; s#</tr>#%#g' |
    tr "\n" "," | tr "%" "\n" |
    sed 's#^,,##g ; s#, \+$##g ; s#^, \+$##g ; s#\([0-9]\) \([0-9]\)#\1\2#g ; s#[èé]#e#g ; s#[ô]#o#g; s#Corse-du-Sud#Corse du Sud#g ' |
    tr -d "'"  | sed 's#-dO#-Do# ; s#-dAr#-Dar#'>> $root/data/census_data.csv

# generate conda env

conda create -y -n coburg
conda activate coburg

mamba install -y -c conda-forge  pandas scikit-learn ipython tqdm
mamba install -y -c conda-forge -c r  R r-maps r-ggplot2 r-data.table r-tidyr r-dplyr r-pheatmap r-ggpubr

pip install arff

# 




