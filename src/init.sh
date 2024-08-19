mkdir -p data
mkdir -p docs

wget -P data/ https://www.openml.org/data/download/20649148/freMTPL2freq.arff
wget -P data/ https://www.openml.org/data/download/20649149/freMTPL2sev.arff

echo "code,name,population" > data/census_data.csv
wget -O - -o /dev/null https://www.insee.fr/fr/statistiques/6683015 |     tr "\n" "%" |     sed 's#.*<tbody>\(.*\)</tbody>.*#\1#' |     tr "%" "\n"    |     sed 's#.*<t.*">##g; s#</t[hd]>##g; s#&nbsp;##g ; s#</tr>#%#g' |     tr "\n" "," | tr "%" "\n" |     sed 's#^,,##g ; s#, \+$##g ; s#^, \+$##g ; s#\([0-9]\) \([0-9]\)#\1\2#g ; s#[èé]#e#g ; s#[ô]#o#g; s#Corse-du-Sud#Corse du Sud#g ' |     tr -d "'"  | sed 's#-dO#-Do# ; s#-dAr#-Dar#'>> data/census_data.csv

conda create -y -n coburg
conda activate coburg

mamba install -y -c conda-forge -c R R
mamba install -y -c conda-forge   r-maps r-ggplot2 r-data.table r-tidyr r-dplyr r-ggpubr r-pheatmap r-mass r-rmarkdown r-mapproj


R -e "rmarkdown::render('src/report.Rmd', output_file = '../docs/index.html')"

