Configuration de la machine:
----------------------------
 
1/ Installer opam, ocaml et ocp-build

apt-get install unzip aspcud
opam init --comp 4.02.1
apt-get install pkg-config
opam install ocp-build

2/ Compiler opam-archive
(version locale, la version publique tourne sur http.new)

git clone git@gitlab.ocamlpro.com:ocamlpro-ocaml/typerex-opam.git
opam install jsonm
eval `opam config env`
./configure
make
cd opam-repository
nohup ../typerex-opam/_obuild/opam-archive/opam-archive.asm &


4/ Installer opam-builder

Un script ./new_switch.sh permet de lancer la création d'un nouveau
switch. Dans chaque switch, un script ./restart.sh permet de le
relancer. Le répertoire ./www est un lien vers le répertoire exporté
en HTML, dans lequel tourne aussi un démon, à lancer avec ./restart.sh.

