
wget -P data/ https://www.openml.org/data/download/20649148/freMTPL2freq.arff
wget -P data/ https://www.openml.org/data/download/20649149/freMTPL2sev.arff

conda create -y -n coburg
conda activate coburg

pip install arff
mamba install -y -c conda-forge liac-arff pandas
