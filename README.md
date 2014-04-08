Early Moose
==================

                                                                              *
                                                   \_\_    _/_/               *
                                                       \__/                   *
                                                       (oo)\_______   /       *
                                                       (__)\       )\/        *
                                                           ||-----||          *
                                                           ||     ||          *
Usage
-----

Run _run_ in a shell, for synchrone mode :
```bash
./src/run -sync protocol.prot
```
for asynchrone mode :
```bash
./src/run -async protocol.prot
```

Système Cryptographiques
----

Utilisez dans le dossier src
```bash
make cyphers
```
pour compiler l'ensemble du sous-projet.

Utilisez par exemple dans le dossier src/cyphers
```bash
scala TestCyphers --ElGamal --encrypt 10 coucou | scala TestCyphers --ElGamal --decrypt 10
```
Pour encoder et décoder puis décoder la chaîne "coucou" avec l'algorithme d'ElGamal, et la graine 10
