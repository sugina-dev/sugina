# Sugina

_The main back end for the Sugina Development Group website_

## Design

This project provides the following interfaces for the website:

* **Authorization**: Support GitLab login by OAuth application
* **User Management**: Identify users by a unique number
* **Random Dictum**: Randomly choose a dictum in Old Japanese
* **Kanji**: Find the pronunciation of Chinese characters
* **Message board**: Enable users to post comments and administrators to reply

The program runs on port 3000. It is designed to be used with a reverse proxy.

## Configuration

To support GitLab login, there should be an OAuth application on GitLab, and the callback URL should be https://example.com/api/auth/page/gitlab/callback.

The configuration file, `.secret.json`, should be set up correctly before running. The format should be in accordance with its definition in `lib/Sugina/Secret.hs`.

## Build and Run

* **Prerequisite**: [Haskell Stack](https://www.haskellstack.org/)
* **Configuration** (First time):
    ``` bash
    stack build
    stack runhaskell config/Config.hs
    stack runhaskell config/MakeKunyomi.hs
    stack runhaskell config/MakeHanja.hs
    ```
* **Build**: `stack build`
* **Run**: `stack exec -- sugina`

## Project Structure

* `lib/`: modules that are used by other modules
    - `lib/Sugina/`: server-specific utilities
* `src/`: source codes to compiled into binaries
    - `src/Foundation.hs`: core application (`App`), routes and instances
    - `src/Handler/`: route handlers (i.e. APIs)
    - `src/Main.hs`: application launcher
* `config/`: scripts for configuration

## APIs

All the APIs are designed to be used with the `api` prefix (i.e. `/dictum` becomes https://example.com/api/dictum).

The APIs are defined in `src/Handler/`.

The authorization process of APIs is defined in `src/Foundation.hs`. Unauthorized users would receive status code 401.

### **GET** Methods

| URI | Path Parameters | Return Encoding | Return Values | Authorization |
| :- | :- | :- | :- | :- |
| /isuser | / | text/plain | user name | User |
| /isadmin | / | text/plain | / | Administrator |
| /users | / | application/json | array | Administrator |
| /dictum | / | text/plain | random dictum | / |
| /kunyomi | q | application/json | char, yomikata | / |
| /hanja | q | application/json | char, yomikata | / |
| /board/message | / | application/json | all messages sent by current user | User |
| /board/manage | / | application/json | all previous messages | Administrator |

### **POST** Methods

| URI | Content Encoding | Content Parameters | Return Values | Authorization |
| :- | :- | :- | :- | :- |
| /board/message | application/json | string | / | User |
| /board/manage | application/json | boardId, reply | / | Administrator |

## Contributors

* sgalal

