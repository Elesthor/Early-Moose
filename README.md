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
./src/run -sync protocol.prot cryptosystem
```
for asynchrone mode :
```bash
./src/run -async protocol.prot cryptosystem
```
Where cryptosystem is one of the following :
* -cesar
* -vigenere
* -RSA (you can precise -keysize [int])
* -AES
* -elGamal (you can precise -zpadd [int], -zpmul [int], -ec)


CryptoSystems
----

Use in directory src/
```bash
make cyphers
```
to compile the subproject.

For example, use in directory src/cyphers/
```bash
scala TestCyphers --ElGamal --encrypt 10 hello | scala TestCyphers --ElGamal --decrypt 10
```
to encrypt then decrypt the word "hello" with ElGamal's cryptosystem and the seed 10.
