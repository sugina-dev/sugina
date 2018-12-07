Sugina
======

*Sugina is the next generation of server for the dual-personal website, built for our future with happiness.*

Build and Run
-------------

=================== ===============================================
**Prerequisites**   * make
                    * `Stack <https://www.haskellstack.org/>`_
**Build**           * Create ``.enc``, and set ``ADMIN_PASSWORD``
                      in the following format:
                      ``ADMIN_PASSWORD:xxxxxx``
                    * ``make bootstrap``
**Rebuild**         ``stack build``
**Run**             ``stack exec -- sugina``
=================== ===============================================

APIs
----

============ ====================== ============================================
API Name     Description            Attributes
============ ====================== ============================================
Get Dictum   Get a random dictum    * **GET** ``/api/dictum``
                                    * **Defined in** ``src/Handler/Dictum.hs``
Get Kunyomi  Get the Kunyomi of a   * **GET** ``/api/kunyomi``
             word                   * **Path Parameter** The query word
                                    * **Return Value** ``"application/json"``.
                                      An array of all the yomikata.
                                    * **Defined in** ``src/Handler/Kunyomi.hs``
============ ====================== ============================================
