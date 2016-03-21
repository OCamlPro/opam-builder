
En fait, il faudrait faire tourner un démon par coeur, chacun pour une
version spécifique de OCaml.

Au lieu d'un commit, on peut sauver avec une date. Pour chaque date,
on dump un fichier marshallé contenant:
* le graphe des packages/versions
* les checksums de chaque version (pour comparaison)
* l'installabilité de chaque version

On peut aussi calculer les versions les plus populaires (apparaissant
dans le plus de demandes d'installation), et tester prioritairement
leur installabilité.

Ensuite, on peut

---------------------------------------------------------------------

opam-platform:

Pour une liste de paquets donnés (la `platform`):
* Pour chaque switch, extraire une liste de versions co-installables
* Compiler tous ces paquets
  * Si ça marche, demander à opam la liste de tous les paquets
      aussi installables (--criteria="+count(new)") avec ces paquets
  * Tenter de les compiler pour découvrir le snapshot le plus grand
* Faire une table: {paquet -> snapshot compilable le contenant}

---------------------------------------------------------------------

opam-builder choisit un paquet à compiler. Pour cela, il calcule une
checksum sur l'ensemble des paquets à installer. Si cette checksum n'a
jamais été compilée, alors il lance `opam install PACKAGE.VERSION`.

Dans OpamAction.build_package:

On enregistre que ce paquet a été construit, et est donc une
dépendance potentielle des paquets suivant.

A-t-on déjà construit ce paquet ? On va dans $OPAMROOT/../cache/, pour y
trouver les informations sur ce paquet.
* PACKAGE/PACKAGE.VERSION/checksum.txt: la checksum courante
* 


Dans OpamAction.install_package

TODO:
* save checksum to which a solution is associated
* save snapshot diff after package removal

---------------------------------------------------
2016-03-07:
 * We create several instances of opam-builder:
   * one instance runs `opam lint` on all the packages
   * one instance per OCaml version
   All these instances monitor a git repository of opam-repository, and
   runs after every commit that is seen one minute after the previous action
   ended. When they have finished, they generate one file and push it
   to the opam-builder monitor.

* Add some more warnings to opam-lint:
  * depexts outside of conf-* package. Can we detect a similar depext in
    a conf-* package ?
  * url file has no checksum, or no archive.
  * not in archive
  
  
