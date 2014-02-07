ChangeLog
================================================================================

31/01/2014
--------------------------------------------------------------------------------
* Début du projet.
* Création des classes nécéssaires à la représentation de l'AST:
    - Term
    - Value
    - Process
* Ajout de methodes retString sur toutes les classes de l'AST pour l'affichage (debug...) 
* Création de la classe de gestion des fichiers (InputFromFile).

02/02/2014
--------------------------------------------------------------------------------
* Ajout du support de TermList au printer

03/02/2014
--------------------------------------------------------------------------------
* Ajouts sur le parser 

04/02/2014
--------------------------------------------------------------------------------
* Clean du parser, ajouts de nouveaux tokens parsables. 
(problème avec le if, il va falloir donner à parsProcess un argument )

07/02/2014
--------------------------------------------------------------------------------
* Modification de la structure du parser: la méthode parseProcess ne cherche plus de "." à la fin des processus pour continuer à parser
* Ajout de ParseProcessSec pour trouver la fin de procecuss et relancer parseProcess le cas échéant.
