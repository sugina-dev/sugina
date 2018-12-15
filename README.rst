Sugina
======

*Sugina is the next generation of server for the dual-personal website, built for our bright futures.*

Build and Run
-------------

=================== ==============================================================
**Prerequisites**   * make
                    * `Stack <https://www.haskellstack.org/>`_
**Build**           * Setup ``.secret.json`` file correctly (See section Secret_)
                    * ``make bootstrap``
**Rebuild**         ``stack build``
**Run**             ``stack exec -- sugina``
=================== ==============================================================

APIs
----

============== ====================== ============================================
API Name       Description            Attributes
============== ====================== ============================================
Get User Name  Get the name of the    * **GET** ``/api/username``
               user who is currently  * **Return Value** ``"application/json"``. A
               logged in                nullable string object
                                      * **Defined in** ``src/Handler/UserName.hs``
Get Dictum     Get a random dictum    * **GET** ``/api/dictum``
                                      * **Defined in** ``src/Handler/Dictum.hs``
Get Kunyomi    Get the Kunyomi of a   * **GET** ``/api/kunyomi``
               word                   * **Path Parameter** The query word
                                      * **Return Value** ``"application/json"``.
                                        An array of all the yomikata
                                      * **Defined in** ``src/Handler/Kunyomi.hs``
============== ====================== ============================================

Secret
------

The ``.secret.json`` file is used for the server to set password and other stuff that could not be open to the public. The whole file should be a JSON object with the following contents:

======================== =======================
Key                      Value
======================== =======================
``getHardcodedUsers``    An object:

                         * Key: User Name
                         * Value: User password
======================== =======================
